// Validate lattice_cascade.js against the player's data contract: run after
// dump-cascade, before committing a regenerated bundle (regen.sh does both).
//   node validate_cascade_bundle.mjs
// Checks every trace: schema, result shape, keyframe arrays, no retired fields
// (cables/zips/crosses/sigma/guests), delta applicability, activate-event keys, and
// seed/cursor role names. Prints a per-scenario smoke table (name, frames, ticks,
// result.pass) and fails loudly on anomalies: 0-frame scenarios, missing notes.
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const src = readFileSync(join(here, "lattice_cascade.js"), "utf8");
const window = {};
new Function("window", src)(window);
const TRACES = window.TRACES;
const names = Object.keys(TRACES);
const cellKey = p => `${p[0]},${p[1]},${p[2]}`;
let parked = 0;
let nursery = 0;
const rows = [];
for (const name of names) {
  const t = TRACES[name];
  if (t.schema_version !== 4) throw new Error(`${name}: schema`);
  if (!Array.isArray(t.frames) || !t.frames.length) throw new Error(`${name}: frames`);
  if (typeof t.note !== "string" || !t.note.trim()) throw new Error(`${name}: missing note`);
  if (!Number.isInteger(t.ticks) || t.ticks < 0) throw new Error(`${name}: ticks`);
  if (typeof t.result.pass !== "boolean" || !Number.isFinite(t.result.generations))
    throw new Error(`${name}: result shape ${JSON.stringify(t.result)}`);
  if (t.result.pass === false) parked++;
  rows.push([name, t.frames.length, t.ticks, t.result.pass]);
  const first = t.frames[0].grid;
  for (const field of ["agents", "wires", "chi", "motion", "words"])
    if (!Array.isArray(first[field])) throw new Error(`${name}: keyframe missing ${field}`);
  for (const banned of ["cables", "zips", "crosses", "sigma", "guests"])
    if (banned in first) throw new Error(`${name}: keyframe still has ${banned}`);
  // Replicate the player's inflate: seed a cell map from the keyframe, apply deltas.
  const cells = new Map();
  const rec = p => {
    const k = cellKey(p);
    if (!cells.has(k)) cells.set(k, {});
    return cells.get(k);
  };
  for (const w of first.words) rec(w).word = w[3];
  for (const a of first.agents) {
    if (a[6] !== undefined && a[6] !== "nursery") throw new Error(`${name}: agent marker ${a[6]}`);
    Object.assign(rec(a), { agent: [a[3], a[5]], sid: a[4], nursery: a[6] === "nursery" });
  }
  for (const w of first.wires) rec(w).wire = w[3];
  for (const c of first.chi) rec(c).chi = c[3];
  for (const m of first.motion) {
    const role = String(m[3]);
    if (!["seed", "cursor", "seed+cursor"].includes(role)) throw new Error(`${name}: role ${role}`);
    rec(m).motion = m.slice(3);
  }
  for (let i = 1; i < t.frames.length; i++) {
    const f = t.frames[i];
    if (!Array.isArray(f.delta)) throw new Error(`${name} f${i}: no delta`);
    if (!Number.isFinite(f.wire.length)) throw new Error(`${name} f${i}: wire stats`);
    for (const e of f.events) {
      if (e.t === "activate" && !(Number.isFinite(e.index) && Number.isFinite(e.width) && Number.isFinite(e.gen)))
        throw new Error(`${name} f${i}: activate keys ${JSON.stringify(e)}`);
    }
    for (const ch of f.delta) {
      const k = cellKey(ch);
      if (ch[3] === null) {
        cells.delete(k);
        continue;
      }
      const c = ch[3];
      if ("guest" in c) throw new Error(`${name} f${i}: delta still has guest`);
      if (c.nursery) nursery++;
      cells.set(k, c);
    }
    for (const [k, c] of cells) {
      if (c.agent && !Array.isArray(c.agent[1])) throw new Error(`${name} f${i} ${k}: agent shape`);
      if (c.wire && !Array.isArray(c.wire)) throw new Error(`${name} f${i} ${k}: wire shape`);
    }
  }
}
const w = Math.max(...rows.map(r => r[0].length), "scenario".length);
const line = r => `${r[0].padEnd(w)}  ${String(r[1]).padStart(6)}  ${String(r[2]).padStart(6)}  ${r[3]}`;
console.log(line(["scenario", "frames", "ticks", "pass"]));
for (const r of rows) console.log(line([r[0], r[1], r[2], r[3] ? "pass" : "PARKED"]));
console.log(`${names.length} traces OK; ${parked} parked; ${nursery} nursery delta-cells`);
