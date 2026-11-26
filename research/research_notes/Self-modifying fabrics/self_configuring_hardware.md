# Hardware whose configuration is written by the fabric itself

Survey for the question: what would a self-modifying FPGA need, where signals on the wires can
print, read or destroy the fabric's own cells, ideally reversibly? Every quote below was checked
with `scripts/verify-source.sh` (2026-11-11); the link is the fragment it minted. Things not
checked are marked "not verified".

For each system: what a cell is, where its rules/configuration live, who can write them, how
fine the writes are, what a write costs, what is reversible, and how it fails.

---

## 1. Cell Matrix (Macias and Durbeck, 1999–2007)

- **Cell**: a lookup table with D (data) and C (control) lines to each neighbour. 4-sided cells
  hold 128 bits, "organized not only as a 128-bit shift register" and as a 16×8 memory
  ([Durbeck & Macias 2001, p5](https://www.cellmatrix.com/entryway/products/pub/ForesightIOP.pdf#page=5));
  the 6-sided 3D cell has "given 768 bits for the cell's truth table"
  ([p2](https://www.cellmatrix.com/entryway/products/pub/ForesightIOP.pdf#page=2)).
- **Who writes**: only the neighbours. "A cell's current mode is determined not by itself, but by
  its neighbors" and "one cell can modify a neighboring cell's lookup table simply by placing that
  neighbor in C mode"
  ([Macias & Durbeck 2002, p3](https://www.cellmatrix.com/entryway/products/pub/eh2002.pdf#page=3)).
  "There is no pre-existing determination of which cells are in which modes"
  ([p4](https://www.cellmatrix.com/entryway/products/pub/ForesightIOP.pdf#page=4)). A written cell can
  then write the next one: "This interchangability of roles, from object to subject"
  ([BioSystems draft, p16](https://www.cellmatrix.com/entryway/products/pub/BioSystems2004Draft.pdf#page=16)).
- **How a write works**: bit-serial, on a global clock. On each step the new bit goes in and the
  old one comes out: "the configuration bit to be replaced next is presented to the cell's D
  outputs" ([p4](https://www.cellmatrix.com/entryway/products/pub/ForesightIOP.pdf#page=4)); "the
  previous contents of the LUT are shifted out to the D outputs"
  ([Petraglio et al. 2004, p5](https://www-users.york.ac.uk/~gt512/Publications/2004/BioADIT04-Petraglio.pdf#page=5)).
  So reading and writing are one operation, and copying is reading yourself: a cell copies itself by
  "inspecting its own truth table and copying it to another cell"
  ([p4](https://www-users.york.ac.uk/~gt512/Publications/2004/BioADIT04-Petraglio.pdf#page=4)).
- **Cost**: one bit per clock, so a full 4-sided rewrite is about 128 clocks (my reading of the
  128-bit shift register; no paper states a cycle count). The selling point is that all cells
  can configure in parallel, since "serial configuration of trillion trillion switch hardware
  would take years" ([p1](https://www.cellmatrix.com/entryway/products/pub/ForesightIOP.pdf#page=1)).
- **Hardware**: "is a hardware implementation of an 8x8 matrix of Cell Matrix cells"
  ([MOD 88 page](https://www.cellmatrix.com/entryway/products/mod88/mod88.html), no anchor).
- **Failure modes**: no arbitration. If two neighbours write at once, "D inputs on all sides where
  Cin = 1 are ORd" ([p4](https://www.cellmatrix.com/entryway/products/pub/ForesightIOP.pdf#page=4)), so
  their streams are silently mixed. Damage is handled by structure, not hardware: a supercell
  (a group of cells acting as one unit) that sees a fault "will activate a circuit inside itself called a Guard Wall"
  ([p23](https://www.cellmatrix.com/entryway/products/pub/BioSystems2004Draft.pdf#page=23)).
- **Reversible?** Not claimed. But the write is already a swap (new bit in, old bit out). If the
  bits that come out are kept rather than dropped, the write can be undone. That is my inference.

## 2. Embryonics and POEtic (Mange, Sipper, Stauffer, Tempesti; Tyrrell, Sanchez, Thoma)

**Embryonics** ([Mange et al., Proc. IEEE 2000](https://www.cs.virginia.edu/~evans/bio-seminar/Embryonics_ProcIEEE.pdf)):
- Three levels: a *molecule* (a multiplexer cell with a configuration register), a *cell* (a
  small processor built from molecules), an *organism* (a grid of cells). Every cell holds the whole program (the
  "genome") and picks its part by position: "given the proper coordinates, it can execute any one of
  the genes of the operative genome"
  ([p3](https://www.cs.virginia.edu/~evans/bio-seminar/Embryonics_ProcIEEE.pdf#page=3)).
- So the rules stay fixed (the molecule grid is configured once), and the program is data that
  every cell reads. This is the "cheap version": fixed interpreter, program as copied data.
- Repair uses blank spares: "each faulty molecule is deactivated, isolated from the network, and
  replaced by a neighboring molecule"
  ([p6](https://www.cs.virginia.edu/~evans/bio-seminar/Embryonics_ProcIEEE.pdf#page=6)); the scheme
  "requires as many spare columns to the right of the array as there are faulty cells"
  ([p4](https://www.cs.virginia.edu/~evans/bio-seminar/Embryonics_ProcIEEE.pdf#page=4)). A dead
  column is skipped by becoming "transparent with respect to all horizontal signals"
  ([p8](https://www.cs.virginia.edu/~evans/bio-seminar/Embryonics_ProcIEEE.pdf#page=8)).
- Nothing in it is reversible, and it has no garbage: dead cells are simply skipped.

**Tom Thumb loop / von Neumann's constructor** ([Rossier et al., ACRI 2004](https://infoscience.epfl.ch/server/api/core/bitstreams/b79346a7-f056-4027-bc38-eaaee44cdd0c/content)):
in von Neumann's automaton, signals print and erase cells. "Every unused cell of the cellular
array is in the quiescent state" (p2), and a built cell that "receives a special excitation, it is
destructed and becomes quiescent again" (p3). This is the classic "blank cell + destroy signal".
(The tool's fragment link resolves to an expiring CDN URL, so the links here use the stable
repository URL with the page number written out.)

**POEtic** ([Thoma & Sanchez 2004](http://web.cecs.pdx.edu/~mperkows/CAPSTONES/Poetic/thoma04prototyping.pdf)):
- Each molecule (a small LUT plus a flip-flop) has an operating mode set by 3 bits. "In Configure mode, the
  molecule can partially reconfigure a neighbouring molecule"
  ([p3](http://web.cecs.pdx.edu/~mperkows/CAPSTONES/Poetic/thoma04prototyping.pdf#page=3)); "molecules
  can be partially reconfigured without microprocessor intervention" (same page).
- The written cell gets a say: "The molecule can allow a reconfiguration of each of its blocks, and
  chooses the source of the configuration data" (p3). That is a per-block write permission.
- Writes are serial like Cell Matrix: "configuration bits are shifted on every clock cycle" (p3).
- Safety by construction: "no randomly generated bitstream configuration can destroy the chip" (p3).
- An on-chip microprocessor also writes the whole array in parallel; so there are two writers, the
  CPU and the neighbours.

## 3. Movable Feast Machine and robust-first computing (Ackley)

[Ackley & Cannon, HotOS 2011](https://www.usenix.org/legacy/event/hotos11/tech/final_files/Ackley.pdf):
- **Cell**: a site holding one 64-bit *atom*, whose header names its element type.
- **Rules are not matter.** Element behaviour lives in each tile's code store: "Element definitions
  (compiled to non-volatile memory)"
  ([p3](https://www.usenix.org/legacy/event/hotos11/tech/final_files/Ackley.pdf#page=3)). Atoms can
  copy themselves, but they cannot rewrite the physics. The paper does picture LUTs that "might
  cooperate, or compete, to copy their configurations into nearby available real estate"
  ([p2](https://www.usenix.org/legacy/event/hotos11/tech/final_files/Ackley.pdf#page=2)).
- **One visitor per cell**: an event locks a neighbourhood ("event window"); rules are "able to read and write only
  inside the event window" ([p3](https://www.usenix.org/legacy/event/hotos11/tech/final_files/Ackley.pdf#page=3)),
  and "an indefinite number of nonoverlapping windows may occur in parallel"
  ([p2](https://www.usenix.org/legacy/event/hotos11/tech/final_files/Ackley.pdf#page=2)). Across
  tiles, "tiles race against each other to acquire" the windows
  ([Ackley 2016, p2](https://www.cs.unm.edu/~ackley/papers/ackley-aaai16-smpt.pdf#page=2)).
- **Blank cells and garbage at once**: the DReg element "probabilistically may delete an atom in a
  nearby site" or create resource atoms in empty sites
  ([p3](https://www.usenix.org/legacy/event/hotos11/tech/final_files/Ackley.pdf#page=3)); if
  programs are "designed to reproduce only by transmuting existing Res atoms", DReg also caps their
  density ([p4](https://www.usenix.org/legacy/event/hotos11/tech/final_files/Ackley.pdf#page=4)).
  The cost: "Structures sharing space with DReg must thus recover from arbitrary atom deletions"
  ([p4](https://www.usenix.org/legacy/event/hotos11/tech/final_files/Ackley.pdf#page=4)).
- Nothing is reversible: deletion is the whole point.

## 4. FPGAs reconfigured from inside the chip

**Xilinx ICAP / self-reconfiguring platform.** The internal port "is essentially an internal
version of the SelectMAP interface"
([UG909 v2022.1, p8](https://www.xilinx.com/support/documents/sw_manuals/xilinx2022_1/ug909-vivado-partial-reconfiguration.pdf#page=8)).
The [SRP paper (Blodget et al., FPL 2003)](https://eric-keller.github.io/papers/pre/ekeller_srp_fpl03.pdf)
"enables an FPGA to dynamically reconfigure itself under the control of an embedded microprocessor"
([p1](https://eric-keller.github.io/papers/pre/ekeller_srp_fpl03.pdf#page=1)).
- Granularity: "a frame is the smallest unit of reconfiguration"
  ([p3](https://eric-keller.github.io/papers/pre/ekeller_srp_fpl03.pdf#page=3)), a column-high strip,
  not a cell. Changing one LUT is a read, modify, write of whole frames.
- Cost: "the minimal time to reconfigure over ICAP a single XC2V40 frame at 66MHz is 3.2us"
  ([p4](https://eric-keller.github.io/papers/pre/ekeller_srp_fpl03.pdf#page=4)); "Modifying one LUT on
  the XC2V1000 will take 13us for the read"
  ([p9](https://eric-keller.github.io/papers/pre/ekeller_srp_fpl03.pdf#page=9)).
- Rewriting in use: "if a bit stays the same between two configurations no glitch will occur"
  ([p8](https://eric-keller.github.io/papers/pre/ekeller_srp_fpl03.pdf#page=8)), but the region being
  replaced is unsafe while it loads: "RM outputs can be random and all downstream logic should be
  decoupled" ([UG909 p13](https://www.xilinx.com/support/documents/sw_manuals/xilinx2022_1/ug909-vivado-partial-reconfiguration.pdf#page=13)),
  while "The static logic remains functioning and is unaffected by the loading of a partial BIT file"
  ([p7](https://www.xilinx.com/support/documents/sw_manuals/xilinx2022_1/ug909-vivado-partial-reconfiguration.pdf#page=7)).
- The writer is one central port driven by one controller, so it is "self" only at the level of
  the whole chip. The bitstream format is closed and the vendor tools make the partial bitstreams,
  which is general knowledge and not quoted here.

**XC6200 and JBits.** The XC6200 put the configuration in the host's address space: "All user
registers and SRAM control store memory mapped onto processor address space"
([data sheet p2](https://hardwarebee.com/wp-content/uploads/2019/10/Xilinx-XC6000-Datasheet.pdf#page=2)),
random access per cell, plus "The Wildcard Register allows many cell configuration memories within
the same column of cells to be written" at once
([p25](https://hardwarebee.com/wp-content/uploads/2019/10/Xilinx-XC6000-Datasheet.pdf#page=25)).
JBits edits Xilinx bitstreams from Java, including "bitstreams read back from actual hardware"
([Guccione et al., p1](https://users.utcluj.ro/~baruch/resources/JBits/JBitsMAPPLD.pdf#page=1)). In
both, the writer is a host program outside the fabric.

**Self-Reconfigurable Gate Array** ([Sidhu, Wadhwa, Mei, Prasanna, FPL 2000](http://www.cs.ucr.edu/~kmiller/references/sidhu00selfreconfigurable.pdf)):
the closest FPGA proposal to the question. It defines self-reconfiguration as the device that "can
generate configuration bits at runtime and use them to modify its own configuration"
([p1](http://www.cs.ucr.edu/~kmiller/references/sidhu00selfreconfigurable.pdf#page=1)), with "single
cycle context switching and single cycle random access to the unified on-chip configuration/data
memory" (p1). So configuration and data share one memory, and logic in the array can write both.
Per-cell protection: "If the CSMR bit in a PE is set, the PE does not switch contexts"
([p5](http://www.cs.ucr.edu/~kmiller/references/sidhu00selfreconfigurable.pdf#page=5)). Only
simulated or estimated; no chip was reported.

**Garp, PipeRench, CAM-8 (configuration streamed from outside).**
- Garp: "always under the control of a program running on the main processor"
  ([Hauser & Wawrzynek 1997, p1](https://www.jhauser.us/publications/1997_Hauser_Garp.pdf#page=1)).
- PipeRench virtualizes the fabric: "Any virtual stripe can be written into any physical stripe",
  aiming at "configuring a computationally significant pipeline stage in one clock cycle"
  ([Goldstein et al. 1999, p4](https://sites.cc.gatech.edu/computing/nano/documents/Goldstein%20-%20PipeRench%20-%20A%20CoProcessor%20for%20Streaming%20Multime.pdf#page=4)).
  When a stripe is evicted, "the state of the resident virtual stripe, if any, must be stored
  outside of the fabric" ([p5](https://sites.cc.gatech.edu/computing/nano/documents/Goldstein%20-%20PipeRench%20-%20A%20CoProcessor%20for%20Streaming%20Multime.pdf#page=5)),
  so evicted state goes outside the fabric.
- CAM-8: one rule table shared by many cells. Data "is passed through a lookup table, and put back
  exactly where it came from"; the tables are double-buffered so "the front-end workstation can
  send a new table while the cammodules" keep running
  ([Margolus 1993, p4](https://arxiv.org/pdf/comp-gas/9509001#page=4)). The rules can change between
  sweeps, but only the host can write them.
- A design that keeps the problem away: "No changes are made to the local configuration memories
  while logic is operating" ([Beasley 2020, p4](https://arxiv.org/pdf/2003.10472#page=4)).

## 5. Graph-reduction and interaction-net machines

- **Reduceron** ([Naylor & Runciman 2008](https://www.doc.ic.ac.uk/~wl/teachlocal/arch/papers/reduceron08.pdf)):
  an FPGA that "is based on a simple template-instantiation evaluator"
  ([p1](https://www.doc.ic.ac.uk/~wl/teachlocal/arch/papers/reduceron08.pdf#page=1)). The reduction
  rules are in the logic; the program is a separate memory of templates, and "The bytecode for a whole
  program is simply the concatenation of the bytecodes" of its functions
  ([p4](https://www.doc.ic.ac.uk/~wl/teachlocal/arch/papers/reduceron08.pdf#page=4)). Running code
  never writes the program memory. Garbage: "use a simple stop-and-copy two-space garbage collector"
  ([p11](https://www.doc.ic.ac.uk/~wl/teachlocal/arch/papers/reduceron08.pdf#page=11)).
- **HVM2** ([Taelin, paper](https://docs.rs/crate/hvm/latest/source/paper/PAPER.pdf)): the rules are fixed in the
  runtime; programs are a book of named nets: "Each .hvm2 file contains a book, which is executed
  by HVM2" ([p4](https://docs.rs/crate/hvm/latest/source/paper/PAPER.pdf#page=4)); REF nodes "represent
  an immutable net" that is copied in when called ([p5](https://docs.rs/crate/hvm/latest/source/paper/PAPER.pdf#page=5)).
  One visitor per wire: "by representing wires as atomic variables, we're able to perform
  interactions atomically" ([p1](https://docs.rs/crate/hvm/latest/source/paper/PAPER.pdf#page=1)).
  The book is outside the net and read-only.
- **Geometry of Synthesis** ([Ghica, POPL 2007](https://www.irif.fr/~mellies/mpri/mpri-ens/articles/ghica-geometry-of-synthesis.pdf)):
  compiles programs to fixed circuits; its type rule against copying is "useful for managing the thorny
  issue of sharing of physical circuits"
  ([p1](https://www.irif.fr/~mellies/mpri/mpri-ens/articles/ghica-geometry-of-synthesis.pdf#page=1)).
  The interaction abstract machine (IAM) literature says the same: geometry of interaction "is seen as a compilation scheme towards
  circuits" and the IAM "sacrifices time in order to be space-efficient"
  ([Accattoli et al. 2021, p2](https://arxiv.org/pdf/2010.12988#page=2)). Here the graph never
  changes; only a token moves.
- **Dynamic GoI machine** ([Muroya & Ghica](https://arxiv.org/pdf/1803.00427)): a halfway point in
  which "the token holds control over graph rewriting, by visiting redexes and triggering the rewrite
  transitions" ([p3](https://arxiv.org/pdf/1803.00427#page=3)). A single moving visitor rewrites
  the graph it walks on. This is software, not hardware.
- **Interaction nets on FPGAs**: none published (see "Searched, not found").

## 6. Reversible and adiabatic hardware

- **FlatTop** (Frank, Vieri, Ammer, Love, Margolus, Knight 1998): an adiabatic chip running the
  billiard-ball cellular automaton; it "can serve as a general-purpose parallel processor when tiled
  in large arrays" but is "not a very practical architecture for performing arbitrary computations"
  ([p1](https://people.csail.mit.edu/nhm/flattop.pdf#page=1)). Its "program" is the starting state,
  loaded while "the array of cells behaves as a shift register"
  ([p7](https://people.csail.mit.edu/nhm/flattop.pdf#page=7)); the rule is wired in. Frank's slides
  call it the "First Adiabatic FPGA" ([OSTI slides, p12](https://www.osti.gov/servlets/purl/1639682#page=12), OCR text).
  So "reconfiguring" it means rewriting the cell states; the rule itself never changes.
- **Vaire (2025)**: the Ice River test chip "demonstrates energy recovery from" an adiabatic
  resonator ([EE Times via Wayback](https://web.archive.org/web/20261017173744/https://www.eetimes.com/vaire-demos-energy-recovery-with-reversible-computing-test-chip/#main)).
  It cannot be reconfigured.
- **Reversible-logic FPGA papers** (Babu et al., "reversible fault tolerant LUT-based FPGA" and
  similar): these count reversible gates inside the LUT, but the configuration is loaded the normal
  way. Not verified: paywalled (ScienceDirect, academia.edu).

## 7. Asynchronous cellular arrays (Peper, Lee, Isokawa)

[Peper et al., Nanotechnology 2003](http://web.cecs.pdx.edu/~mperkows/CLASS_573/Asynchr_Febr_2007/t30412.pdf)
lay out delay-insensitive circuits (circuits that work whatever the wire delays) on a clockless
cellular automaton. Their answer to two signals wanting the same cell is a primitive in the rules
themselves: "arbitration is necessary to ensure that signals on crossing paths can pass the
crossing", and the sequencer built from it "is used for arbitration between processes that want
access to a shared resource"
([p7](http://web.cecs.pdx.edu/~mperkows/CLASS_573/Asynchr_Febr_2007/t30412.pdf#page=7)). This is
the one-visitor-per-cell rule built into the cell, with no locks. Universal construction on these
automata (Takada et al. 2006) exists but was not verified (see below).

## 8. Instruction as data

- Von Neumann (EDVAC draft): "tempting to treat the entire memory as one organ"
  ([p9](https://web.mit.edu/sts.035/www/PDFs/edvac.pdf#page=9), OCR text), the origin of keeping code in
  the same memory as data.
- LUTstructions (2026) keeps processor instructions as FPGA bitstreams in a cache, with a fabric
  "enabling potential reuse by the softcore to reconfigure itself"
  ([p7](https://arxiv.org/pdf/2602.20802#page=7)). The processor is still the only writer.

---

## What this means for the project

The four missing pieces, and who has solved each one:

| | Rules as matter | One visitor per cell | Blank cells | Garbage |
|---|---|---|---|---|
| Cell Matrix | **yes**: truth table in the cell, written by neighbours | no: concurrent writers are ORed | an inert table, nothing special | none |
| POEtic | **yes**, partial, with per-block permission chosen by the target | the target's permission bits | no | no |
| Embryonics / Tom Thumb / von Neumann | genome as data read by fixed hardware (cheap version); von Neumann cells are built by signals | not addressed (synchronous) | spare columns; the quiescent state | the destroy signal turns a cell back to quiescent |
| MFM | no: physics in tile memory | **yes**: locked event windows | **yes**: Res atoms made by DReg | **yes**: random deletion by DReg |
| ICAP / SRP / SRGA | the chip writes its own bits, but through one central port (SRGA: any logic, one cycle) | decoupling; SRGA mask bits | n/a | n/a |
| Reduceron / HVM2 | no: fixed rules, read-only program (cheap version) | HVM2: atomic wires | free list / heap | Reduceron: copying collector |
| Peper async arrays | no | **yes**: arbitration primitive in the rules | no | no |
| FlatTop | no: rule wired in, the state is the program | synchronous | no | none needed: reversible |

Takeaways:

1. **Rules as matter has been built, in Cell Matrix (8×8 board) and POEtic (chip).** In both, a
   write is bit-serial and takes the shape of a swap (new bit in, old bit out). That swap is the
   natural reversible primitive for "printing" a cell: keep the bits that come out, and the print
   can be undone. Neither design keeps them; doing so is the new step.
2. **None of them has one visitor per cell.** Cell Matrix ORs concurrent writers together. The fixes in the literature are
   MFM's locked windows (costly, needs a lock protocol), Peper's arbitration primitive (built into the
   rules), and POEtic's permission bits (the target cell refuses). For an interaction-net lattice,
   the Peper approach fits best: it is local and clockless.
3. **Blank cells and garbage are solved only by MFM**, and irreversibly (random deletion, a pool
   of resource atoms). Von Neumann's quiescent state plus destroy signal is the deterministic
   version. A reversible version would have to pass erased contents somewhere instead of
   deleting them. FlatTop shows a reversible fabric needs no garbage collector, but it also had no
   way to change its rule.
4. **The cheap version (fixed rules, programs as data) is how every working functional-language
   machine does it** (Reduceron templates, HVM2's book of REF nets, the Embryonics genome). The difference
   in a lattice is that the "book" would have to be cells too: trees printed into the fabric and
   read by visitors, not a side memory. Nothing found does this in hardware; the token-guided
   rewriter (DGoIM) is the nearest model in software.
5. **Conventional FPGA self-reconfiguration (ICAP) is a dead end for this**: one central port,
   frame granularity, microseconds per write, closed bitstreams, and the region being rewritten must
   be cut off while it loads. SRGA (one-cycle access to a configuration memory shared with data)
   is the right shape, but it was only a proposal.

## Searched, not found

- **Interaction nets or interaction combinators on an FPGA or ASIC**: the arXiv API phrase
  "interaction nets" (38 results) and "interaction combinators" returned no hardware papers; web
  searches found only a work-in-progress FPGA target for Vine ("Tendrils Compute"), which is
  announced on Discord and listed in marvinborner's interaction-net-resources, and a ResearchGate page that mixes in
  text from an unnamed thesis describing "a processor for interaction nets". Neither is verified,
  and no paper exists to verify.
- **A reversible FPGA whose configuration is written reversibly**: none found. Reversible-logic FPGA
  papers (Babu et al., Microelectronics J. 2013, and others) are paywalled and not verified; going by
  their abstracts, they use reversible gates inside LUTs and load the configuration the normal way.
- **Not verified (paywalled or blocked)**: Morita & Imai, "Self-reproduction in a reversible
  cellular space" (TCS 1996); Takada, Isokawa, Peper, Matsui, "Construction universality in purely
  asynchronous cellular automata" (JCSS 2006; ScienceDirect 403); Macias & Athanas 2007 and the MOD
  88 IEEE paper; Frank's thesis "Reversibility for efficient computing" (the DSpace landing page
  had no FlatTop text; the PDF was not fetched); the Reduceron JFP 2012 paper (Cambridge gave only
  the abstract page; the 2008 paper was used instead); Garp's configuration-cache timings (only in
  search snippets of the 2000 IEEE Computer article).
- **Interaction Abstract Machine as hardware**: only software and theory papers; the hardware
  version is Ghica's Geometry of Synthesis, which makes fixed circuits.
