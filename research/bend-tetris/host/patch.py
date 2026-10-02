"""Adapts the C that `bend gen-c` emits (HVM 2.0.22) to host.c: one thread, a small heap, no main.

Reads the generated C on stdin and writes the patched C to stdout. Every patch must apply
exactly as many times as expected, so a different Bend or HVM version fails loudly here.
"""
import re
import sys

src = sys.stdin.read()


def swap(old, new, count=1):
    global src
    found = src.count(old)
    if found != count:
        sys.exit(f"patch.py: expected {count} of {old!r}, found {found}")
    src = src.replace(old, new)


# host.c brings its own entry points.
swap("#define WITH_MAIN\n", "")

# Interpret the book instead of compiling each definition to C: for this program it runs
# just as fast (IO dominates) and the WebAssembly is a tenth of the size.
swap("#define COMPILED\n", "")

# One evaluator thread, and it stops as soon as it runs out of redexes instead of
# spinning 256 times first. HVM normalizes once per byte of IO, so the spin dominated.
src, n = re.subn(r"#define TPC_L2 \d+[^\n]*", "#define TPC_L2 0", src)
if n != 1:
    sys.exit("patch.py: TPC_L2 not found")
swap("      sched_yield();\n      // Halt", "      if (TPC > 1) sched_yield();\n      // Halt")
swap("if (tick % 256 == 0) {", "if (TPC == 1 || tick % 256 == 0) {")

# A heap that fits in a browser tab: 4M nodes and vars instead of 512M.
swap("#define RLEN (1ul << 24)", "#define RLEN (1ul << 20)")
swap("#define G_NODE_LEN (1ul << 29)", "#define G_NODE_LEN (1ul << 22)")
swap("#define G_VARS_LEN (1ul << 29)", "#define G_VARS_LEN (1ul << 22)")

# The root variable lives at index 2^29 - 1, past the end of the smaller vars buffer;
# give it its own slot (host.c defines `var_slot`).
swap("  a32 idle; // idle thread counter\n", "  a32 idle; // idle thread counter\n  APort root_var;\n")
swap("&net->vars_buf[var]", "var_slot(net, var)", count=3)
swap("net->vars_buf[get_val(ROOT)] = NONE;", "*var_slot(net, get_val(ROOT)) = NONE;")

# host.c brings a readback_bytes that frees what it reads.
src, n = re.subn(r"Bytes readback_bytes\(Net\* net, Book\* book, Port port\) \{.*?\n  return bytes;\n\}\n",
                 "Bytes readback_bytes(Net* net, Book* book, Port port);\n", src, flags=re.S)
if n != 1:
    sys.exit("patch.py: readback_bytes not found")

# Room for this program's definitions instead of 16k of them (each one is 64KB).
defs = re.search(r"\nstatic const u8 BOOK_BUF\[\] = \{(\d+), (\d+), (\d+), (\d+),", src)
count = int.from_bytes(bytes(int(b) for b in defs.groups()), "little")
swap("Def defs_buf[0x4000];", f"Def defs_buf[{count}];")
swap("FFn ffns_buf[0x4000];", "FFn ffns_buf[0x20];")

sys.stdout.write(src)
