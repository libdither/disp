"""What in rust-ic-strands nothing uses: `Params` fields never read, wasm exports no player page calls,
and every environment-variable switch, with where each is read.

  python evaluators/rust-ic-strands/dead-knobs.py

A field counts as read where `.field` appears other than being assigned (`.field = …`), outside
`Params::set` and `Default`. Fields read only by binaries or tests are listed as such: knobs kept for
experiments. A wasm export counts as called where a player page or script names it.
"""
import os, re, glob

here = os.path.dirname(os.path.abspath(__file__))
crate = os.path.join(here, "crate")
src = open(os.path.join(crate, "src/lattice.rs")).read()
body = re.search(r"pub struct Params \{(.*?)\n\}", src, re.S).group(1)
fields = re.findall(r"^\s*pub (\w+):", body, re.M)

# Where reads may be: the library outside Params::set / Default, then binaries and tests.
def sources(pattern):
    return [f for f in glob.glob(os.path.join(crate, pattern), recursive=True) if f.endswith(".rs")]

lib = sources("src/**/*.rs")
bins = [f for f in lib if "/src/bin/" in f]
lib = [f for f in lib if f not in bins]
tests = sources("tests/*.rs")


def strip_set_and_default(text):
    text = re.sub(r"pub fn set\(&mut self.*?\n    \}\n", "", text, flags=re.S)
    return re.sub(r"impl Default for Params \{.*?\n\}\n", "", text, flags=re.S)


def reads(files, name):
    n = 0
    for f in files:
        text = open(f).read()
        if f.endswith("lattice.rs"):
            text = strip_set_and_default(text)
        n += len(re.findall(r"\." + name + r"\b(?!\s*=[^=])", text))
    return n


print("Params fields never read in the library:")
for name in fields:
    in_lib = reads(lib, name)
    if in_lib:
        continue
    elsewhere = reads(bins, name) + reads(tests, name)
    print(f"  {name}" + (f"  (read {elsewhere}× by binaries or tests only)" if elsewhere else "  (never read)"))

wasm = open(os.path.join(crate, "src/wasm.rs")).read()
exports = re.findall(r'pub extern "C" fn (\w+)', wasm)
player = os.path.join(here, "player")
pages = "".join(open(f).read() for f in glob.glob(os.path.join(player, "*")) if f.endswith((".js", ".html", ".mjs")) and not f.endswith("engine.js"))
unused = [e for e in exports if not re.search(r"\b" + e + r"\b", pages)]
print(f"\nwasm exports no player page names ({len(unused)} of {len(exports)}):")
for e in unused:
    print(f"  {e}")

print("\nEnvironment switches:")
for f in sorted(lib + bins + tests):
    for n, line in enumerate(open(f), 1):
        for v in re.findall(r'env::var(?:_os)?\("(\w+)"\)', line):
            print(f"  {v:12} {os.path.relpath(f, here)}:{n}")
