// Runs the whole lattice clock by clock against the simulator's state dumps
// (DUMPS=file strands-run ... --chip) and checks every bit of every site after every clock.
//
//   tb_lattice <dumps> [max clocks]      exit status 0 only if everything matched
#include "Vstrands_lattice.h"
#include "verilated.h"
#include <cstdio>
#include <cstdint>
#include <cstring>
#include <cstdlib>
#include <string>
#include <vector>

static const int ENDS = 33, KS = 3, SITE_BYTES = ENDS + 2 * KS + 1, SW = 219;

static void pack(const uint8_t *b, uint32_t w[7]) {
  memset(w, 0, 7 * 4);
  auto put = [&](int at, int n, uint32_t v) { for (int i = 0; i < n; i++) if (v >> i & 1) w[(at + i) / 32] |= 1u << ((at + i) % 32); };
  for (int e = 0; e < ENDS; e++) put(e * 6, 6, b[e] == 255 ? 63 : b[e]);
  for (int k = 0; k < KS; k++) put(198 + 4 * k, 4, b[ENDS + k]);
  for (int k = 0; k < KS; k++) put(210 + k, 1, b[ENDS + KS + k]);
  put(213, 6, b[ENDS + 2 * KS] == 255 ? 63 : b[ENDS + 2 * KS]);
}
static std::string show(const uint32_t w[7]) {
  auto get = [&](int at, int n) { uint32_t v = 0; for (int i = 0; i < n; i++) v |= (w[(at + i) / 32] >> ((at + i) % 32) & 1) << i; return v; };
  std::string r = "mates";
  for (int e = 0; e < ENDS; e++) { uint32_t m = get(e * 6, 6); if (m != 63) r += " " + std::to_string(e) + ":" + std::to_string(m); }
  r += " | tags"; for (int k = 0; k < KS; k++) r += " " + std::to_string(get(198 + 4 * k, 4));
  r += " | want"; for (int k = 0; k < KS; k++) r += " " + std::to_string(get(210 + k, 1));
  r += " | pulse " + std::to_string(get(213, 6));
  return r;
}

int main(int argc, char **argv) {
  if (argc < 2) { fprintf(stderr, "usage: tb_lattice <dumps> [max clocks]\n"); return 2; }
  long maxclk = argc > 2 ? atol(argv[2]) : -1;
  FILE *f = fopen(argv[1], "rb");
  if (!f) { perror(argv[1]); return 2; }
  uint32_t dims[4];
  if (fgetc(f) != 'H' || fread(dims, 4, 4, f) != 4) { fprintf(stderr, "no header\n"); return 2; }
  const uint32_t W = dims[0], H = dims[1], D = dims[2], N = W * H * D;
  Verilated::commandArgs(argc, argv);
  Vstrands_lattice top;
  auto tick = [&]() { top.clk = 0; top.eval(); top.clk = 1; top.eval(); };
  top.seed = dims[3];
  top.rst = 1; tick(); top.rst = 0; tick();
  std::vector<uint8_t> state(N * SITE_BYTES);
  auto read_state = [&](uint32_t &clock) {
    if (fgetc(f) != 'S' || fread(&clock, 4, 1, f) != 1) return false;
    return fread(state.data(), SITE_BYTES, N, f) == N;
  };
  uint32_t clock;
  if (!read_state(clock) || clock != 0) { fprintf(stderr, "no initial state\n"); return 2; }
  for (uint32_t s = 0; s < N; s++) {
    uint32_t w[7]; pack(&state[s * SITE_BYTES], w);
    top.wr_en = 1; top.wr_x = s % W; top.wr_y = s / W % H; top.wr_z = s / (W * H);
    for (int i = 0; i < 7; i++) top.wr_data[i] = w[i];
    tick();
  }
  top.wr_en = 0;
  long clocks = 0, bad = 0; double cycles = 0; long maxcycles = 0;
  while (read_state(clock) && (maxclk < 0 || clocks < maxclk)) {
    top.step = 1; tick(); top.step = 0;
    long n = 0;
    while (top.busy && n < 10000000) { tick(); n++; }
    clocks++; cycles += top.cycles; if ((long)top.cycles > maxcycles) maxcycles = top.cycles;
    if (top.clock != clock) { printf("clock %u, chip says %u\n", clock, top.clock); bad++; break; }
    int wrong = 0;
    for (uint32_t s = 0; s < N; s++) {
      uint32_t want[7], got[7]; pack(&state[s * SITE_BYTES], want);
      top.rd_x = s % W; top.rd_y = s / W % H; top.rd_z = s / (W * H); top.eval();
      for (int i = 0; i < 7; i++) got[i] = top.rd_data[i];
      got[6] &= (1u << (SW - 192)) - 1;
      if (memcmp(want, got, 28)) {
        if (wrong++ < 4) printf("clock %u site %u (%u,%u,%u)\n  want %s\n  got  %s\n", clock, s, s % W, s / W % H, s / (W * H), show(want).c_str(), show(got).c_str());
      }
    }
    if (wrong) { printf("clock %u: %d sites differ\n", clock, wrong); bad++; break; }
  }
  printf("%ld clocks of a %ux%ux%u lattice, %ld mismatched; %.1f cycles per clock on average, %ld at most\n", clocks, W, H, D, bad, clocks ? cycles / clocks : 0, maxcycles);
  return bad ? 1 : 0;
}
