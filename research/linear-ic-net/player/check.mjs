// Holds the player's engine to ../nets/net.py: the same answer, the same number of interactions, no
// cancel across two copies and nothing left over, on every program in expected.json (python expected.py),
// in each order of steps. Run: node check.mjs
import { createRequire } from "node:module";
import { readFileSync } from "node:fs";
const require = createRequire(import.meta.url);
// engine.js is a plain script: under a package of ES modules it leaves its names on the global object
const loaded = require("./engine.js"), { Run, PRELUDE, ADDS, show } = loaded.Run ? loaded : globalThis.RLN;
const want = JSON.parse(readFileSync(new URL("./expected.json", import.meta.url), "utf8"));
let bad = 0, runs = 0;
const fail = (name, what) => { if (bad++ < 12) console.log(`BAD ${name}: ${what}`); };
if (want.prelude.trim() !== PRELUDE.trim()) fail("prelude", "engine.js PRELUDE differs from PROGRAMS in ../nets/check.py");
for (const t of want.tests) {
  for (const order of ["rounds", "newest", "random"]) {
    if (t.interactions > 300000 && order !== "rounds") continue;
    let run;
    try { run = new Run(t.src, { order, seed: 7, limit: 5000000 }); } catch (e) { fail(t.name, `${order}: ${e.message}`); continue; }
    runs++;
    const status = run.status === "done" && run.garbage ? `garbage ${run.garbage}` : run.status;
    if (status !== t.status) fail(t.name, `${order}: status ${status}, net.py ${t.status}`);
    else if (run.n !== t.interactions) fail(t.name, `${order}: ${run.n} interactions, net.py ${t.interactions}`);
    else if (run.crossed) fail(t.name, `${order}: ${run.crossed} cancels across two copies`);
    else if (t.status === "done" && JSON.stringify(run.answer) !== JSON.stringify(t.answer)) fail(t.name, `${order}: answer ${JSON.stringify(run.answer).slice(0, 120)}, net.py ${JSON.stringify(t.answer).slice(0, 120)}`);
    // going back: the net at a moment is the same whether reached by running or by seeking
    if (order === "rounds" && run.n > 2 && run.n < 20000) {
      const mid = run.n >> 1; run.seek(mid); const a = JSON.stringify(run.net.save()); run.seek(0); run.seek(run.n); run.seek(mid);
      if (JSON.stringify(run.net.save()) !== a) fail(t.name, "seeking back gives a different net");
    }
  }
}
// Pairs fired by hand, in an order no plan would take: every rewrite chosen at random among the pairs
// there are, and then a run where only some are. The answer and the number of rewrites must not change.
let byHand = 0;
const pick = (() => { let s = 12345; return n => { s = (Math.imul(s, 1103515245) + 12345) >>> 0; return s % n; }; })();
for (const t of want.tests) {
  if (t.interactions > 400 || t.interactions < 2) continue;
  for (const every of [1, 3]) {
    const run = new Run(t.src, { order: "rounds", limit: 5000000 });
    run.seek(0);
    for (let k = 0; ; k++) {
      const live = run.net.pairs().filter(p => p[2]);
      if (!live.length) break;
      if (k % every) { run.seek(run.at + 1); continue; }
      const [a, b] = live[pick(live.length)];
      if (!run.fireNow(a, b)) { fail(t.name, "a live pair would not fire"); break; }
    }
    byHand++;
    const status = run.status === "done" && run.garbage ? `garbage ${run.garbage}` : run.status;
    if (run.at !== run.n) fail(t.name, `by hand: stopped at ${run.at} of ${run.n}`);
    else if (status !== t.status) fail(t.name, `by hand: status ${status}, net.py ${t.status}`);
    else if (run.n !== t.interactions) fail(t.name, `by hand: ${run.n} interactions, net.py ${t.interactions}`);
    else if (run.crossed) fail(t.name, "by hand: a cancel across two copies");
    else if (t.status === "done" && JSON.stringify(run.answer) !== JSON.stringify(t.answer)) fail(t.name, "by hand: a different answer");
    else {
      const end = JSON.stringify(run.net.save()); run.seek(run.n >> 1); run.seek(0); run.seek(run.n);
      if (JSON.stringify(run.net.save()) !== end) fail(t.name, "by hand: seeking back gives a different net");
      if (run.rounds[0] !== 0 || run.rounds[run.rounds.length - 1] !== run.n || run.rounds.some((r, i) => i && r <= run.rounds[i - 1])) fail(t.name, "by hand: the rounds are out of order");
    }
  }
}
// Pairs fired live, with nothing planned after them (as the page does when pairs fire where they meet):
// half the run in a random order, then the rest planned. The same answer and the same number of rewrites.
let live = 0;
for (const t of want.tests) {
  if (t.interactions > 3000 || t.interactions < 2) continue;
  const run = new Run(t.src, { order: "rounds", limit: 5000000 });
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
  const status = run.status === "done" && run.garbage ? `garbage ${run.garbage}` : run.status;
  if (status !== t.status) fail(t.name, `live: status ${status}, net.py ${t.status}`);
  else if (run.n !== t.interactions) fail(t.name, `live: ${run.n} interactions, net.py ${t.interactions}`);
  else if (run.crossed) fail(t.name, "live: a cancel across two copies");
  else if (t.status === "done" && JSON.stringify(run.answer) !== JSON.stringify(t.answer)) fail(t.name, "live: a different answer");
  else {
    const end = JSON.stringify(run.net.save()); run.seek(run.n >> 2); run.seek(0); run.seek(run.n);
    if (JSON.stringify(run.net.save()) !== end) fail(t.name, "live: seeking back gives a different net");
  }
}
// The page's list of adds: each is a program the Python nets ran too, and takes the rewrites and rounds it says.
for (const [, rows] of ADDS) for (const [src, , , rewrites, rounds] of rows) {
  if (!want.tests.some(t => t.src === src)) fail(src, "listed among the adds, but not in expected.json");
  const run = new Run(src, { order: "rounds", limit: 5000000 });
  if (run.n !== rewrites || run.rounds.length - 1 !== rounds) fail(src, `listed with ${rewrites} rewrites in ${rounds} rounds, but takes ${run.n} in ${run.rounds.length - 1}`);
}
// The net read as a term at every moment of some runs: it reads, whatever is under way.
let read = 0;
for (const t of want.tests) {
  if (t.interactions > 1500) continue;
  const run = new Run(t.src, { order: "rounds", limit: 5000000 });
  for (let at = 0; at <= run.n; at++) {
    run.seek(at);
    try { const text = show(run.net.read(run.net.mate(run.net.root * 4), { budget: 5000 })); if (!text) fail(t.name, `nothing read at ${at}`); read++; }
    catch (e) { fail(t.name, `reading at ${at}: ${e.message}`); break; }
  }
}
console.log(bad ? `${bad} failures` : `ok: ${want.tests.length} programs, ${runs} runs in three orders, ${byHand} with pairs fired by hand and ${live} with pairs fired live, all as net.py; the net read as a term at ${read} moments`);
process.exit(bad ? 1 : 0);
