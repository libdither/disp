// A first-order cost model of fabrics that run programs as signals on configured cells: energy, time and
// error for each way of carrying a signal, from a handful of device numbers. Every knob is a plain
// number in `P`, so sweep.mjs can trace trade-offs and explorer.html can move them by hand.
//
// The device model: one transistor curve (EKV-style, exponential below threshold and about square-law
// above, `drive`), switching energy C·V², leakage over the cycle, thermal kT/C noise and Pelgrom-style
// mismatch that both shrink as a device (its capacitance) grows. Designs build on it: static CMOS, an
// FPGA (the published FPGA/ASIC ratios), a fine-grained self-configuring cell array, the same with
// low-swing transport between restoring cells, adiabatic reversible cells, analog multiply-accumulate,
// stochastic bitstreams, race (arrival-time) logic, and the strand lattice for reference.

export const K_B = 1.380649e-23

/** Defaults: roughly a 45–28 nm low-power process at room temperature. */
export const P = {
  T: 300, // kelvin
  vdd: 0.8, // supply (V)
  vt: 0.35, // threshold (V)
  n: 1.4, // subthreshold slope factor
  dibl: 0.08, // threshold lowered per volt of drain bias
  c: 1e-15, // switched capacitance of one gate output, its wire and fan-in (F)
  tNom: 15e-12, // one gate delay at 1 V with c = 1 fF (s): sets the drive current
  activity: 0.5, // fraction of outputs that switch per operation
  sigmaVt: 0.025, // threshold mismatch of a 1 fF device (V); scales as 1/√(c)
  kappa: 2, // a gate's noise margin is V/2 less kappa·n·φt (gain runs out near φt)
  // the cell array
  cellGates: 3, // a cell's logic (a 4-input LUT and its muxes) in gate equivalents of energy
  cellDelay: 2, // ... and of delay
  hopC: 0.5e-15, // one hop to a neighbouring cell: wire and the next cell's input (F)
  cfgBits: 32, // configuration bits a cell holds (LUT 16 + routing 16)
  cfgWriteC: 2e-15, // capacitance flipped writing one configuration bit (F)
  // low-swing transport: hops at a small swing, a sense amplifier restoring every `k` hops
  swing: 0.1, // V
  k: 8,
  senseC: 4e-15, // the sense amplifier's switched capacitance (F)
  senseOffset: 0.01, // its input offset before calibration (V)
  calibrated: false, // trimmed by configuration bits written at set-up: offset left `senseResidual`
  senseResidual: 0.001,
  // adiabatic cells
  ramp: 200e-12, // the power clock's ramp time T (s)
  clockLoss: 0.05, // fraction of C·V² the resonant power clock fails to recover per cycle
  // numbers carried as values (dot products)
  bits: 8, // precision wanted
  analogOverhead: 30, // analog energy over its kT floor (bias, drivers, nonlinearity)
  adcFom: 10e-15, // ADC energy per conversion step (J/step): converting a b-bit result costs fom·2^b
  // the FPGA, as ratios to the same design in standard cells
  fpgaEnergy: 14,
  fpgaDelay: 3.5,
  fpgaArea: 35,
  ioBitEnergy: 10e-12, // reading a configuration bit from off-chip memory (J)
  // error handling
  target: 1e-12, // acceptable chance a run's output is wrong
  supplyNoise: 0.015, // supply droop and ripple, as a share of the supply (1σ, correlated along a path)
  timingErr: 1e-15 // chance a run misses its clock: sets the timing guardband (see `guard`)
}

export const kT = (p = P) => K_B * p.T
export const phiT = (p = P) => kT(p) / 1.602176634e-19
const softplus = (x) => (x > 30 ? x : Math.log1p(Math.exp(x)))

/** Drain current at gate drive `vg` and drain bias `vd`, scaled so a 1 fF load at 1 V takes tNom. */
export function drive(vg, vd, p = P) {
  const f = (v, d) => softplus((v - p.vt + p.dibl * d) / (2 * p.n * phiT(p))) ** 2
  const scale = (1e-15 * 1) / p.tNom / f(1, 1)
  return scale * f(vg, vd)
}
/** One gate delay at supply v with load c. */
export const gateDelay = (v, c, p = P) => (c * v) / drive(v, v, p)
/** Off current of one gate at supply v. */
export const leak = (v, p = P) => drive(0, v, p)
/** Standard normal tail. */
export function Q(x) {
  if (x < 0) return 1 - Q(-x)
  // Abramowitz–Stegun 7.1.26 through erfc, fine to a few digits; asymptotic beyond 8σ
  if (x > 8) return Math.exp((-x * x) / 2) / (x * Math.sqrt(2 * Math.PI)) * (1 - 1 / (x * x))
  const z = x / Math.SQRT2
  const t = 1 / (1 + 0.3275911 * z)
  const erfc = t * (0.254829592 + t * (-0.284496736 + t * (1.421413741 + t * (-1.453152027 + t * 1.061405429)))) * Math.exp(-z * z)
  return erfc / 2
}
/** The voltage a gate's input can be off by before its output flips. */
export const margin = (v, p = P) => Math.max(0, v / 2 - p.kappa * p.n * phiT(p))
/** Thermal noise on a node of capacitance c. */
export const thermal = (c, p = P) => Math.sqrt(kT(p) / c)
/** Threshold mismatch of a device whose capacitance is c. */
export const mismatch = (c, p = P) => p.sigmaVt * Math.sqrt(1e-15 / c)
/** Inverse of Q, by bisection. */
export function Qinv(p) {
  let lo = -40, hi = 40
  for (let i = 0; i < 100; i++) { const mid = (lo + hi) / 2; if (Q(mid) > p) lo = mid; else hi = mid }
  return (lo + hi) / 2
}
/** How much longer than its nominal delay a path's clock must be so it is late with chance `allowed`:
 * supply noise moves every gate's delay together, by the slope of log-delay against voltage, which is
 * gentle well above threshold and steep below it. */
export function guard(v, p = P, allowed = p.timingErr) {
  const dv = 1e-3
  const slope = Math.abs(Math.log(gateDelay(v + dv, p.c, p) / gateDelay(v - dv, p.c, p)) / (2 * dv))
  return Math.exp(Math.max(0, Qinv(allowed)) * slope * p.supplyNoise * v)
}
/** A gate's chance of a transient error per operation, and of being built faulty. */
export const gateErr = (v, c, p = P) => Q(margin(v, p) / thermal(c, p))
export const gateFault = (v, c, p = P) => 2 * Q(margin(v, p) / mismatch(c, p))

/** A workload as a netlist summary: gates, logic depth, total hops of wiring between cells, output
 * bits, whether it is a bijection (a two-way program, so adiabatic cells need not erase anything),
 * and for numeric work the number of multiply-accumulates. */
export const WORKLOADS = {
  notw: { name: 'notw, 64 bits', gates: 64, depth: 1, hops: 128, outBits: 64, reversible: true },
  addw: { name: 'add, 32 bits (ripple)', gates: 32 * 5, depth: 64, hops: 32 * 8, outBits: 32, reversible: true },
  mul: { name: 'multiply, 16 × 16 bits', gates: 16 * 16 * 6, depth: 2 * 16 + 4, hops: 16 * 16 * 10, outBits: 32, reversible: false },
  dot: { name: 'dot product, 64 × 8-bit', gates: 64 * 8 * 8 * 6 + 64 * 16 * 5, depth: 16 + 6 * 16, hops: 64 * 8 * 8 * 10, outBits: 22, reversible: false, macs: 64 }
}

// ---- the designs ------------------------------------------------------------------------------------
// Each returns { energy (J per run), time (s per run), err (chance some output bit is wrong), area
// (cells or gate sites), setup: { energy, time } to configure it, faultPerSite } for workload w.

/** Static CMOS standard cells: fixed wiring, every gate restores its output. */
export function asic(w, p = P) {
  const v = p.vdd
  const td = gateDelay(v, p.c, p)
  const time = w.depth * td * guard(v, p)
  const dyn = p.activity * (w.gates + w.hops / 4) * p.c * v * v
  const leakE = w.gates * leak(v, p) * v * time
  const logic = 1 - (1 - gateErr(v, p.c, p)) ** (w.gates)
  return { energy: dyn + leakE, time, err: logic + p.timingErr, errTiming: p.timingErr, area: w.gates, setup: { energy: 0, time: 0 }, faultPerSite: gateFault(v, p.c, p) }
}

/** An FPGA: the same design scaled by measured FPGA/ASIC ratios; its bitstream comes from off chip. */
export function fpga(w, p = P) {
  const a = asic(w, p)
  const bits = a.area * p.fpgaArea * 4 // configuration bits per gate site
  return {
    energy: a.energy * p.fpgaEnergy,
    time: a.time * p.fpgaDelay,
    err: a.err,
    area: a.area * p.fpgaArea,
    setup: { energy: bits * p.ioBitEnergy, time: bits / 1e9 },
    faultPerSite: a.faultPerSite
  }
}

/** A fine-grained array of identical cells, each a small LUT with neighbour-only routing, that writes
 * its own configuration: a configuration stream travels cell to cell (as in the Cell Matrix). Every hop
 * is a full-swing restoring buffer. */
export function cells(w, p = P) {
  const v = p.vdd
  const td = gateDelay(v, p.c, p)
  const hopTd = gateDelay(v, p.hopC + p.c / 2, p)
  const hopsPerLevel = w.hops / Math.max(1, w.gates) // average wiring hops per gate on the critical path
  const time = w.depth * (p.cellDelay * td + hopsPerLevel * hopTd) * guard(v, p)
  const area = w.gates + w.hops / 2 // routing-only cells
  const dyn = p.activity * (w.gates * p.cellGates * p.c + w.hops * (p.hopC + p.c / 2)) * v * v
  const leakE = area * p.cellGates * leak(v, p) * v * time
  const perOp = gateErr(v, p.c, p)
  const err = 1 - (1 - perOp) ** (w.gates + w.hops) + p.timingErr
  const cfgE = area * p.cfgBits * (p.cfgWriteC * v * v + Math.sqrt(area) * (p.hopC * v * v) / 4)
  const cfgT = Math.sqrt(area) * p.cfgBits * hopTd // the wave crosses the region, a bit per hop-time
  return { energy: dyn + leakE, time, err, errTiming: p.timingErr, area, setup: { energy: cfgE, time: cfgT }, faultPerSite: gateFault(v, p.c, p) }
}

/** Transport at a small swing between restoring sense amplifiers every k hops (low-swing interconnect);
 * the amplifiers' offsets can be trimmed by configuration written at set-up. */
export function lowSwing(w, p = P) {
  const base = cells(w, p)
  const v = p.vdd
  const segments = w.hops / p.k
  const hopE = p.hopC * p.swing * v // charge drawn from the supply to swing the hop by `swing`
  const senseE = p.senseC * v * v
  const offset = p.calibrated ? p.senseResidual : p.senseOffset
  const noise = Math.sqrt(p.k * thermal(p.hopC, p) ** 2 + offset ** 2)
  const decision = Q(p.swing / 2 / noise)
  const td = gateDelay(v, p.c, p)
  const hopT = gateDelay(v, p.hopC, p) * (p.swing / v) * 4 // a small swing settles sooner, slower edges
  const time = (w.depth * (p.cellDelay * td + (w.hops / Math.max(1, w.gates)) * hopT) + w.depth * 2 * td / p.k) * guard(v, p)
  const logicE = p.activity * w.gates * p.cellGates * p.c * v * v
  const energy = logicE + p.activity * (w.hops * hopE + segments * senseE) + base.area * p.cellGates * leak(v, p) * v * time
  const err = 1 - (1 - gateErr(v, p.c, p)) ** w.gates * (1 - decision) ** segments + p.timingErr
  const trim = p.calibrated ? segments * 6 * (p.cfgWriteC * v * v) * 20 : 0 // a few trim bits, found by a short search
  return { energy, time, err, errTiming: p.timingErr, area: base.area, setup: { energy: base.setup.energy + trim, time: base.setup.time }, faultPerSite: base.faultPerSite }
}

/** Adiabatic cells: a multi-phase power clock ramps each stage in time T, recovering most of C·V². A
 * bijection loses nothing it must erase; a function that is not one erases a bit per gate it narrows,
 * at C·V²/2 each (or uncomputes it, doubling the time). */
export function adiabatic(w, p = P) {
  const v = p.vdd
  const td = gateDelay(v, p.c, p)
  const T = Math.max(p.ramp, 2 * td * guard(v, p))
  const perSwitch = p.c * v * v * Math.max((2 * td) / T, p.clockLoss) // resistive loss ~ (RC/T)·C·V², floored by the clock
  const stages = w.depth
  const time = stages * T * 4 // four phases a stage
  const switched = p.activity * (w.gates * p.cellGates + w.hops / 2)
  const erase = w.reversible ? 0 : w.gates * 0.5 * p.c * v * v * 0.5
  const leakE = (w.gates + w.hops / 2) * p.cellGates * leak(v, p) * v * time
  const err = 1 - (1 - gateErr(v, p.c, p)) ** (w.gates + w.hops) + p.timingErr
  const base = cells(w, p)
  return { energy: switched * perSwitch + erase + leakE, time, err, errTiming: p.timingErr, area: base.area * 1.5, setup: base.setup, faultPerSite: base.faultPerSite }
}

/** The precision an analog value carries on capacitance c: thermal noise, and mismatch unless trimmed. */
export function analogBits(c, swing, p = P) {
  const sigma = Math.sqrt(thermal(c, p) ** 2 + (p.calibrated ? 0 : mismatch(c, p) * 0.2) ** 2)
  return Math.log2(swing / (6 * sigma))
}
/** A dot product as charge or current summed on a wire: each multiply-accumulate needs enough
 * capacitance for the wanted precision, and the sum is converted once. */
export function analogMac(w, p = P) {
  const macs = w.macs ?? 1
  const swing = p.vdd / 2
  // capacitance for `bits` of precision against thermal noise (6σ per step), and against mismatch untrimmed
  const cTh = kT(p) * (6 * 2 ** p.bits / swing) ** 2
  const cMis = p.calibrated ? 0 : 1e-15 * (p.sigmaVt * 0.2 * 6 * 2 ** p.bits / swing) ** 2
  const cNeed = Math.max(cTh, cMis, 1e-17)
  const macE = p.analogOverhead * cNeed * swing * p.vdd
  const adc = p.adcFom * 2 ** (p.bits + Math.log2(macs) / 2)
  const td = gateDelay(p.vdd, Math.min(cNeed, 1e-12), p)
  const time = 10 * td + (p.bits + 2) * gateDelay(p.vdd, p.c, p) * 4
  const err = Q(3) * 0 + 2 ** -p.bits // a result off by more than one step: set by the 6σ choice
  const trim = p.calibrated ? macs * 8 * p.cfgWriteC * p.vdd ** 2 * 50 : 0
  return { energy: macs * macE + adc, time, err, area: macs * 4, setup: { energy: trim, time: p.calibrated ? macs * 1e-9 : 0 }, faultPerSite: 0, cPerMac: cNeed }
}
/** Stochastic bitstreams: a value is the density of 1s; a multiply is one AND; an n-bit result needs a
 * stream of about 4^n bits, and every input needs a generator (a comparator and a share of an LFSR). */
export function stochastic(w, p = P) {
  const macs = w.macs ?? 1
  const N = 4 ** p.bits
  const v = p.vdd
  const gateE = p.activity * p.c * v * v
  const perBit = macs * (1 + 2 * (p.bits + 2)) * gateE + 2 * macs * p.bits * gateE / 4
  const td = gateDelay(v, p.c, p)
  const err = 2 ** -p.bits
  return { energy: N * perBit, time: N * 3 * td, err, area: macs * (2 + 2 * p.bits), setup: { energy: 0, time: 0 }, faultPerSite: gateFault(v, p.c, p) }
}
/** The digital multiply-accumulate the analog and stochastic ones replace, on the cell array. */
export const digitalMac = (w, p = P) => cells(w, p)

/** Race logic: a value is the time an edge arrives; min and max are an OR and an AND, an add a delay.
 * b bits need 2^b steps of a delay well above the jitter; one edge per wire whatever the value. */
export function race(w, p = P) {
  const v = p.vdd
  const td = gateDelay(v, p.c, p)
  const jitter = (thermal(p.c, p) / v) * td * Math.sqrt(w.depth) // timing noise from voltage noise on each edge
  const step = Math.max(6 * jitter, td)
  const time = 2 ** p.bits * step * Math.max(1, w.depth / 8)
  const energy = (w.gates + w.hops / 4) * p.c * v * v
  return { energy, time, err: 2 ** -p.bits, area: w.gates, setup: { energy: 0, time: 0 }, faultPerSite: gateFault(v, p.c, p) }
}

/** The strand lattice as it runs now (rust-ic-linear-strands): every primitive move is 2–3 rewrites, each
 * taking 3–28 lattice clocks, a clock being a whole site update (switchboard, pairings, a random draw),
 * here taken as 30 gate delays and 200 gate-energies for each of the ~10 sites working near a run. */
export function strands(w, p = P, clocksPerRewrite = 10) {
  const v = p.vdd
  const td = gateDelay(v, p.c, p)
  const moves = w.gates
  const rewrites = moves * 2.5
  const clocks = (rewrites * clocksPerRewrite) / Math.max(1, Math.min(w.gates, 8)) // parallel runs
  const time = clocks * 30 * td
  const energy = rewrites * clocksPerRewrite * 10 * 200 * p.activity * p.c * v * v
  return { energy, time, err: 1 - (1 - gateErr(v, p.c, p)) ** (rewrites * 200), area: w.gates * 40, setup: { energy: 0, time: 0 }, faultPerSite: gateFault(v, p.c, p) }
}

export const DESIGNS = { asic, fpga, cells, lowSwing, adiabatic, strands }
export const NUMERIC = { digitalMac, analogMac, stochastic }

// ---- error handling on top of any design -----------------------------------------------------------

/** Triple the work and vote (von Neumann's majority): an output is wrong when two copies are. */
export function tmr(r, outBits) {
  const pb = 1 - (1 - r.err) ** (1 / Math.max(1, outBits)) // per output bit
  const voted = 3 * pb * pb - 2 * pb ** 3
  return { ...r, energy: r.energy * 3.1, area: r.area * 3.2, err: 1 - (1 - voted) ** outBits }
}
/** Late signals caught by a shadow latch sampled after the clock and the cycle replayed (as Razor
 * does): about 10% more energy, a replay costs a few cycles; only timing errors are caught. */
export function replay(r, depth) {
  const t = r.errTiming ?? 0
  return { ...r, energy: r.energy * 1.1, time: r.time * (1 + t * Math.max(4, depth)), err: r.err - t + t * 1e-6 }
}
/** A bijection checked by running it back (flip) and comparing with the input; a mismatch reruns it.
 * Detects any single error; costs twice the work, more on a retry. */
export function checked(r) {
  const retry = 1 / Math.max(1e-9, 1 - r.err)
  const undetected = r.err * r.err // both the run and its check wrong in a way that cancels: bounded by both failing
  return { ...r, energy: r.energy * 2 * retry, time: r.time * 2 * retry, err: undetected }
}

/** Energy per run once set-up is shared by `uses` runs. */
export const amortized = (r, uses) => r.energy + r.setup.energy / uses
