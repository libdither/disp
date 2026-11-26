// Holds the player's engine to ../nets/tables.py: the same answer, the same number of rewrites of each family
// (call, open, read, thaw, copy, erase), the same number of agents made, the same list of definitions, and
// nothing left over, on every program in expected.json (python expected.py), in each order of steps; then
// pairs fired by hand and live, seeking back, the page's adds and examples, and the net read as a term at
// every moment. Run: node check.mjs
import { createRequire } from "node:module";
import { readFileSync } from "node:fs";
import { createHash } from "node:crypto";
const require = createRequire(import.meta.url);
// engine.js is a plain script: under a package of ES modules it leaves its names on the global object
const loaded = require("./engine.js"), { Run, PRELUDE, ADDS, EXAMPLES, FAMILIES, show } = loaded.Run ? loaded : globalThis.RLN;
const want = JSON.parse(readFileSync(new URL("./expected.json", import.meta.url), "utf8"));
let bad = 0, runs = 0;
const fail = (name, what) => { if (bad++ < 12) console.log(`BAD ${name}: ${what}`); };
const statusOf = run => run.status === "done" && run.garbage ? `garbage ${run.garbage}` : run.status;
const made = run => run.net.next - run.net.PH;
const sha1 = s => createHash("sha1").update(s).digest("hex");
/// What differs between a run, at its end, and tables.py's run of the same program; "" if nothing.
function differs(run, t, how) {
  const status = statusOf(run);
  if (status !== t.status) return `${how}status ${status}, tables.py ${t.status}`;
  if (run.n !== t.interactions) return `${how}${run.n} rewrites, tables.py ${t.interactions}`;
  if (t.status === "limit") return "";
  const fams = [...run.net.counts];
  if (JSON.stringify(fams) !== JSON.stringify(t.families)) return `${how}rewrites by family ${fams}, tables.py ${t.families} (${FAMILIES})`;
  if (made(run) !== t.made) return `${how}${made(run)} agents made, tables.py ${t.made}`;
  if (t.status === "done" && JSON.stringify(run.answer) !== JSON.stringify(t.answer)) return `${how}answer ${JSON.stringify(run.answer).slice(0, 120)}, tables.py ${JSON.stringify(t.answer).slice(0, 120)}`;
  return "";
}
if (want.prelude.trim() !== PRELUDE.trim()) fail("prelude", "engine.js PRELUDE differs from PROGRAMS in ../nets/tables_check.py");
for (const t of want.tests) {
  for (const order of ["rounds", "newest", "random"]) {
    if (t.interactions > 300000 && order !== "rounds") continue;
    let run;
    try { run = new Run(t.src, { order, seed: 7, limit: t.limit }); } catch (e) { fail(t.name, `${order}: ${e.message}`); continue; }
    runs++;
    if (order === "rounds") {
      const defs = JSON.stringify(run.defs.map(d => [d.binder, d.x, d.body, d.names]));
      if (run.defs.length !== t.ndefs || sha1(defs) !== t.defs) fail(t.name, `${run.defs.length} definitions, not tables.py's ${t.ndefs} (or not the same)`);
    }
    const d = differs(run, t, `${order}: `);
    if (d) fail(t.name, d);
    // going back: the net at a moment is the same whether reached by running or by seeking
    if (order === "rounds" && run.n > 2 && run.n < 20000) {
      const mid = run.n >> 1; run.seek(mid); const a = JSON.stringify(run.net.save()); run.seek(0); run.seek(run.n); run.seek(mid);
      if (JSON.stringify(run.net.save()) !== a) fail(t.name, "seeking back gives a different net");
    }
  }
}
// Pairs fired by hand, in an order no plan would take: every rewrite chosen at random among the pairs
// there are, and then a run where only some are. The answer and the rewrites must not change.
let byHand = 0;
const pick = (() => { let s = 12345; return n => { s = (Math.imul(s, 1103515245) + 12345) >>> 0; return s % n; }; })();
for (const t of want.tests) {
  if (t.interactions > 400 || t.interactions < 2 || t.status === "limit") continue;
  for (const every of [1, 3]) {
    const run = new Run(t.src, { order: "rounds", limit: t.limit });
    run.seek(0);
    for (let k = 0; ; k++) {
      const live = run.net.pairs().filter(p => p[2]);
      if (!live.length) break;
      if (k % every) { run.seek(run.at + 1); continue; }
      const [a, b] = live[pick(live.length)];
      if (!run.fireNow(a, b)) { fail(t.name, "a live pair would not fire"); break; }
    }
    byHand++;
    const d = run.at !== run.n ? `by hand: stopped at ${run.at} of ${run.n}` : differs(run, t, "by hand: ");
    if (d) fail(t.name, d);
    else {
      const end = JSON.stringify(run.net.save()); run.seek(run.n >> 1); run.seek(0); run.seek(run.n);
      if (JSON.stringify(run.net.save()) !== end) fail(t.name, "by hand: seeking back gives a different net");
      if (run.rounds[0] !== 0 || run.rounds[run.rounds.length - 1] !== run.n || run.rounds.some((r, i) => i && r <= run.rounds[i - 1])) fail(t.name, "by hand: the rounds are out of order");
    }
  }
}
// Pairs fired live, with nothing planned after them (as the page does when pairs fire where they meet):
// half the run in a random order, then the rest planned. The same answer and the same rewrites.
let live = 0;
for (const t of want.tests) {
  if (t.interactions > 3000 || t.interactions < 2 || t.status === "limit") continue;
  const run = new Run(t.src, { order: "rounds", limit: t.limit });
  run.seek(0);
  for (let k = 0; k < t.interactions >> 1; k++) {
    const pairs = run.net.pairs().filter(p => p[2]);
    if (!pairs.length) break;
    const [a, b] = pairs[pick(pairs.length)];
    if (!run.fireLive(a, b)) { fail(t.name, "live: a pair would not fire"); break; }
  }
  if ((t.interactions >> 1) && run.status !== "open") fail(t.name, `live: the run is ${run.status} while pairs are still fired`);
  run.replan();
  run.seek(run.n);
  live++;
  const d = differs(run, t, "live: ");
  if (d) fail(t.name, d);
  else {
    const end = JSON.stringify(run.net.save()); run.seek(run.n >> 2); run.seek(0); run.seek(run.n);
    if (JSON.stringify(run.net.save()) !== end) fail(t.name, "live: seeking back gives a different net");
  }
}
// The page's list of adds: each is a program the Python net ran too, and takes the rewrites and rounds it says.
for (const [, rows] of ADDS) for (const [src, , , rewrites, rounds] of rows) {
  if (!want.tests.some(t => t.src === src)) fail(src, "listed among the adds, but not in expected.json");
  const run = new Run(src, { order: "rounds", limit: 5000000 });
  if (run.n !== rewrites || run.rounds.length - 1 !== rounds) fail(src, `listed with ${rewrites} rewrites in ${rounds} rounds, but takes ${run.n} in ${run.rounds.length - 1}`);
}
// The page's examples: each ran in Python too, and a count of rewrites one states is the count it takes.
for (const [src, what] of EXAMPLES) {
  const t = want.tests.find(t => t.src === src);
  if (!t) { fail(src, "an example, but not in expected.json"); continue; }
  const said = what.match(/\(([\d,]+) rewrites\)/);
  if (said && Number(said[1].replace(/,/g, "")) !== t.interactions) fail(src, `said to take ${said[1]} rewrites, but takes ${t.interactions}`);
}
// The net read as a term at every moment of some runs: it reads, whatever is under way.
let read = 0;
for (const t of want.tests) {
  if (t.interactions > 1500 || t.status === "limit") continue;
  const run = new Run(t.src, { order: "rounds", limit: t.limit });
  for (let at = 0; at <= run.n; at++) {
    run.seek(at);
    try { const text = show(run.net.read(run.net.mate(run.net.port(run.net.root, 0)), { budget: 5000 })); if (!text) fail(t.name, `nothing read at ${at}`); read++; }
    catch (e) { fail(t.name, `reading at ${at}: ${e.message}`); break; }
  }
}
console.log(bad ? `${bad} failures` : `ok: ${want.tests.length} programs, ${runs} runs in three orders, ${byHand} with pairs fired by hand and ${live} with pairs fired live, all as tables.py; the net read as a term at ${read} moments`);
process.exit(bad ? 1 : 0);
