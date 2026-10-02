// Runs a Bend program, compiled to C by `bend gen-c` and adapted by patch.py, on one thread.
//
// HVM's own IO loop blocks when the program reads stdin. This one returns to its caller
// instead, so a browser can hand the program keys and clock ticks between frames:
//   boot() once, then push_byte(b) for each input byte and resume() to run until the
//   program wants a byte that hasn't arrived (1) or has finished (0).
// Built natively, main() plays in the terminal and sends a 0 byte every 50 ms as a tick.

#include <pthread.h>
#ifndef __wasm__
#include <poll.h>
#include <termios.h>
#include <unistd.h>
#endif

// Runs the evaluator inline instead of on a thread.
#define pthread_create(thread, attr, func, arg) ((func)(arg), 0)
#define pthread_join(thread, result) 0

#define var_slot(net, var) ((var) == get_val(ROOT) ? &(net)->root_var : &(net)->vars_buf[var])

// HVM's `link` would clash with unistd's.
#define link hvm_link
#include PROGRAM
#undef link

#ifdef __wasm__
#define EXPORT(name) __attribute__((export_name(#name)))
#else
#define EXPORT(name)
#endif

static Net* net;
static Book* book;

static u8 inbox[4096];
static u32 inbox_len;
static bool inbox_closed;

// A READ on stdin that is waiting for input, and where its result goes.
static bool waiting;
static Port waiting_argm;
static Port waiting_cont;

EXPORT(push_byte) void push_byte(u32 byte) {
  if (inbox_len < sizeof(inbox)) inbox[inbox_len++] = byte;
}

// Freeing readback
// ----------------
// HVM reads λ-encoded data back without freeing it, leaking a few nodes per byte of IO:
// harmless on its 4GB heap, fatal on ours within a few hundred frames. These readers
// erase what they read, letting HVM's own eraser interactions free every node and wire.

// Follows a wire to whatever it was linked to, freeing its variables on the way.
static Port take_wire(Port port) {
  while (get_tag(port) == VAR) {
    Port next = vars_load(net, get_val(port));
    if (next == NONE || next == 0) break;
    vars_take(net, get_val(port));
    port = next;
  }
  return port;
}

// Reads a constructor λt (((t TAG) arg0) arg1 ...) and erases all of it but the arguments.
static Ctr take_ctr(Port port) {
  Ctr ctr = {.tag = -1, .args_len = 0};
  Port lam = expand(net, book, port);
  if (get_tag(lam) != CON) return ctr;
  Port app = expand(net, book, get_fst(node_load(net, get_val(lam))));
  if (get_tag(app) != CON) return ctr;
  Port tag = expand(net, book, get_fst(node_load(net, get_val(app))));
  if (get_tag(tag) != NUM) return ctr;
  ctr.tag = get_u24(get_val(tag));
  while (true) {
    app = expand(net, book, get_snd(node_load(net, get_val(app))));
    if (get_tag(app) != CON) break;
    Pair node = node_load(net, get_val(app));
    ctr.args_buf[ctr.args_len++] = expand(net, book, take_wire(get_fst(node)));
    node_store(net, get_val(app), new_pair(new_port(ERA, 0), get_snd(node)));
  }
  hvm_link(net, tm[0], new_port(ERA, 0), lam);
  normalize(net, book);
  return ctr;
}

// Replaces HVM's readback_bytes (patch.py removes it), freeing each list cell once read.
Bytes readback_bytes(Net* net, Book* book, Port port) {
  u32 capacity = 256;
  Bytes bytes = {.len = 0, .buf = malloc(capacity)};
  while (true) {
    normalize(net, book);
    Ctr ctr = take_ctr(peek(net, port));
    if (ctr.tag != LIST_CONS || ctr.args_len != 2 || get_tag(ctr.args_buf[0]) != NUM) break;
    if (bytes.len == capacity - 1) bytes.buf = realloc(bytes.buf, capacity *= 2);
    bytes.buf[bytes.len++] = get_u24(get_val(ctr.args_buf[0]));
    boot_redex(net, new_pair(ctr.args_buf[1], ROOT));
    port = ROOT;
  }
  return bytes;
}

// IO
// --

static bool reads_stdin(Port argm) {
  Tup tup = readback_tup(net, book, argm, 2);
  return tup.elem_len == 2 && get_u24(get_val(tup.elem_buf[0])) == 0;
}

// Stdin reads take queued bytes; other files go through HVM's own READ.
static Port host_read(Net* net, Book* book, Port argm) {
  if (!reads_stdin(argm)) return io_read(net, book, argm);
  Tup tup = readback_tup(net, book, argm, 2);
  u32 want = get_u24(get_val(tup.elem_buf[1]));
  Bytes bytes = {.len = want < inbox_len ? want : inbox_len, .buf = (char*)inbox};
  Port ret = inject_ok(net, inject_bytes(net, &bytes));
  inbox_len -= bytes.len;
  memmove(inbox, inbox + bytes.len, inbox_len);
  return ret;
}

static FFn* find_ffn(char* name) {
  for (u32 i = 0; i < book->ffns_len; ++i) {
    if (strcmp(name, book->ffns_buf[i].name) == 0) return &book->ffns_buf[i];
  }
  return NULL;
}

// Checks for, and frees, the magic number pair that marks an IO constructor.
static bool take_magic(Ctr ctr) {
  if (ctr.args_len < 1 || get_tag(ctr.args_buf[0]) != CON) return false;
  Pair magic = node_take(net, get_val(ctr.args_buf[0]));
  return get_val(get_fst(magic)) == new_u24(IO_MAGIC_0) && get_val(get_snd(magic)) == new_u24(IO_MAGIC_1);
}

// Runs an IO call, frees its (a, b) argument, and hands the result to the rest of the program.
static void call(FFn* ffn, Port argm, Port cont) {
  Port ret = ffn ? ffn->func(net, book, argm) : inject_io_err_name(net);
  if (get_tag(argm) == CON) {
    Pair pair = node_take(net, get_val(argm));
    take_wire(get_fst(pair));
    take_wire(get_snd(pair));
  }
  u32 lps = 0;
  u32 loc = node_alloc_1(net, tm[0], &lps);
  node_create(net, loc, new_pair(ret, ROOT));
  boot_redex(net, new_pair(new_port(CON, loc), cont));
}

EXPORT(boot) void boot(void) {
  alloc_static_tms();
  book = malloc(sizeof(Book));
  book_load(book, (u32*)BOOK_BUF);
  book_init(book);
  find_ffn("READ")->func = host_read;
  net = net_new();
  boot_redex(net, new_pair(new_port(REF, 0), ROOT));
}

EXPORT(resume) int resume(void) {
  if (waiting) {
    if (inbox_len == 0 && !inbox_closed) return 1;
    waiting = false;
    call(find_ffn("READ"), waiting_argm, waiting_cont);
  }
  while (true) {
    normalize(net, book);
    Ctr ctr = take_ctr(peek(net, ROOT));
    if (!take_magic(ctr) || ctr.tag != IO_CALL || ctr.args_len != 4) break;
    Str name = readback_str(net, book, ctr.args_buf[1]);
    FFn* ffn = find_ffn(name.buf);
    free(name.buf);
    Port argm = ctr.args_buf[2];
    Port cont = ctr.args_buf[3];
    if (ffn && ffn->func == host_read && inbox_len == 0 && !inbox_closed && reads_stdin(argm)) {
      waiting = true;
      waiting_argm = argm;
      waiting_cont = cont;
      fflush(stdout);
      return 1;
    }
    call(ffn, argm, cont);
  }
  fflush(stdout);
  return 0;
}

#ifndef __wasm__
int main(void) {
  struct termios saved, raw;
  bool tty = tcgetattr(0, &saved) == 0;
  if (tty) {
    raw = saved;
    raw.c_lflag &= ~(ICANON | ECHO);
    tcsetattr(0, TCSANOW, &raw);
  }
  boot();
  u64 next_tick = time64();
  while (resume()) {
    u64 now = time64();
    int wait_ms = next_tick > now ? (next_tick - now) / 1000000 : 0;
    struct pollfd in = {.fd = 0, .events = POLLIN};
    if (poll(&in, 1, wait_ms) > 0) {
      u8 buf[64];
      ssize_t len = read(0, buf, sizeof(buf));
      if (len <= 0) inbox_closed = true;
      for (ssize_t i = 0; i < len; ++i) push_byte(buf[i]);
    } else {
      push_byte(0);
      next_tick = time64() + 50000000;
    }
  }
  if (tty) tcsetattr(0, TCSANOW, &saved);
  return 0;
}
#endif
