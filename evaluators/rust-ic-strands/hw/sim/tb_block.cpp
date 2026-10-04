// Replays test vectors written by the simulator (VECTORS=file strands-run ... --chip) through the
// block unit and checks every bit of every site after each turn and each block.
//
//   tb_block <vectors> [max records] [--stop]      exit status 0 only if everything matched
#include "Vstrands_block.h"
#include "verilated.h"
#include <cstdio>
#include <cstdint>
#include <cstring>
#include <cstdlib>
#include <map>
#include <string>
#include <vector>

static const int ENDS = 33, KS = 3, SITE_BYTES = ENDS + 2 * KS + 1, SW = 219;
static const char *OPS[] = {"none", "hop", "swap", "fold", "flip", "fire", "collect", "erase unpair", "erase inputs"};

struct Site { uint8_t b[SITE_BYTES]; };

// A site's bytes as the chip's 219-bit word (7 × 32 bits).
static void pack(const Site &s, uint32_t w[7]) {
  memset(w, 0, 7 * 4);
  auto put = [&](int at, int n, uint32_t v) { for (int i = 0; i < n; i++) if (v >> i & 1) w[(at + i) / 32] |= 1u << ((at + i) % 32); };
  for (int e = 0; e < ENDS; e++) put(e * 6, 6, s.b[e] == 255 ? 63 : s.b[e]);
  for (int k = 0; k < KS; k++) put(198 + 4 * k, 4, s.b[ENDS + k]);
  for (int k = 0; k < KS; k++) put(210 + k, 1, s.b[ENDS + KS + k]);
  put(213, 6, s.b[ENDS + 2 * KS] == 255 ? 63 : s.b[ENDS + 2 * KS]);
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
  if (argc < 2) { fprintf(stderr, "usage: tb_block <vectors> [max records] [--stop]\n"); return 2; }
  long maxrec = argc > 2 && argv[2][0] != '-' ? atol(argv[2]) : -1;
  bool stop = false; for (int i = 2; i < argc; i++) if (!strcmp(argv[i], "--stop")) stop = true;
  FILE *f = fopen(argv[1], "rb");
  if (!f) { perror(argv[1]); return 2; }
  Verilated::commandArgs(argc, argv);
  Vstrands_block top;
  auto tick = [&]() { top.clk = 0; top.eval(); top.clk = 1; top.eval(); };
  top.rst = 1; tick(); top.rst = 0; tick();
  uint32_t lw = 0, lh = 0, ld = 0;
  std::map<std::string, std::pair<long, long>> by;  // move: (records, mismatches)
  long records = 0, bad = 0, maxcycles = 0; double cycles = 0;
  int shown = 0;
  for (;;) {
    int tag = fgetc(f);
    if (tag == EOF || (maxrec >= 0 && records >= maxrec)) break;
    if (tag == 'H') { uint32_t d[3]; if (fread(d, 4, 3, f) != 3) break; lw = d[0]; lh = d[1]; ld = d[2]; continue; }
    uint32_t clock; int32_t c[3]; uint8_t valid, pos = 0, taken = 0;
    if (fread(&clock, 4, 1, f) != 1 || fread(c, 4, 3, f) != 3 || fread(&valid, 1, 1, f) != 1) break;
    if (tag == 'T' && (fread(&pos, 1, 1, f) != 1 || fread(&taken, 1, 1, f) != 1)) break;
    Site before[8], after[8];
    if (fread(before, SITE_BYTES, 8, f) != 8 || fread(after, SITE_BYTES, 8, f) != 8) break;
    uint8_t touched = 0, stale = 0, op = 0;
    if (tag == 'T' && (fread(&touched, 1, 1, f) != 1 || fread(&stale, 1, 1, f) != 1 || fread(&op, 1, 1, f) != 1)) break;
    records++;
    // Load the block.
    for (int q = 0; q < 8; q++) {
      uint32_t w[7]; pack(before[q], w);
      top.wr_en = 1; top.wr_pos = q; for (int i = 0; i < 7; i++) top.wr_data[i] = w[i];
      tick();
    }
    top.wr_en = 0;
    top.clock = clock; top.cx1 = c[0] + 1; top.cy1 = c[1] + 1; top.cz1 = c[2] + 1; top.lw = lw; top.lh = lh; top.ld = ld;
    top.turn_pos = pos; top.turn_taken = taken;
    if (tag == 'T') top.go_turn = 1; else top.go_block = 1;
    tick(); top.go_turn = 0; top.go_block = 0;
    long n = 0;
    while (top.busy && n < 1000000) { tick(); n++; }
    cycles += top.cycles; if ((long)top.cycles > maxcycles) maxcycles = top.cycles;
    // Compare.
    bool ok = true; std::string why;
    for (int q = 0; q < 8; q++) {
      uint32_t want[7], got[7]; pack(after[q], want);
      memset(got, 0, sizeof got);
      for (int b = 0; b < SW; b++) if (top.st_out[(q * SW + b) / 32] >> ((q * SW + b) % 32) & 1) got[b / 32] |= 1u << (b % 32);
      if (memcmp(want, got, 28)) { ok = false; why += "  site " + std::to_string(q) + "\n    want " + show(want) + "\n    got  " + show(got) + "\n"; }
    }
    if (tag == 'T') {
      if (top.touched != touched) { ok = false; why += "  touched want " + std::to_string(touched) + " got " + std::to_string(top.touched) + "\n"; }
      if (top.stale != stale) { ok = false; why += "  stale want " + std::to_string(stale) + " got " + std::to_string(top.stale) + "\n"; }
    }
    std::string kind = tag == 'B' ? "block" : op < 9 ? OPS[op] : "?";
    by[kind].first++;
    if (!ok) {
      by[kind].second++; bad++;
      if (shown++ < 5) {
        printf("MISMATCH record %ld (%s) clock %u corner %d,%d,%d pos %d taken %02x\n%s", records, kind.c_str(), clock, c[0], c[1], c[2], pos, taken, why.c_str());
        for (int q = 0; q < 8; q++) { uint32_t w[7]; pack(before[q], w); printf("  before %d: %s\n", q, show(w).c_str()); }
      }
      if (stop) break;
    }
  }
  for (auto &[k, v] : by) printf("%-14s %9ld records %6ld mismatches\n", k.c_str(), v.first, v.second);
  printf("%ld records, %ld mismatches; %.1f cycles per record on average, %ld at most\n", records, bad, records ? cycles / records : 0, maxcycles);
  return bad ? 1 : 0;
}
