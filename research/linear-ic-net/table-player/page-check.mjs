// Drives index.html in headless Chromium: every example and add is loaded, stepped, played to its end and
// held to engine.js (the answer and the count of rewrites shown), seeked back to the start and the middle,
// and has a pair fired by hand; the rule box is opened for every rule that occurs; the icon chooser and the
// tour are run. Fails on any console error or uncaught exception.
//   node page-check.mjs [url of index.html] [--shots dir]
import { createRequire } from "node:module";
import { readdirSync, mkdirSync } from "node:fs";
import { fileURLToPath, pathToFileURL } from "node:url";
const require = createRequire(new URL("../../../website/package.json", import.meta.url));
const { chromium } = require("playwright-core");
const loaded = createRequire(import.meta.url)("./engine.js"), RLN = loaded.Run ? loaded : globalThis.RLN;

const args = process.argv.slice(2), shotsAt = args.indexOf("--shots"), shots = shotsAt >= 0 ? args.splice(shotsAt, 2)[1] : null;
const here = new URL("./index.html", import.meta.url);
let url = args[0] ?? (process.env.DISP_PAGES_URL ?? "http://localhost:5180") + "/research/linear-ic-net/table-player/index.html";
try { if (!args[0]) await fetch(url, { method: "HEAD", signal: AbortSignal.timeout(3000) }); } catch { url = pathToFileURL(fileURLToPath(here)).href; }

const shells = `${process.env.HOME}/.cache/ms-playwright`;
const shell = process.env.CHROMIUM ?? `${shells}/${readdirSync(shells).filter(d => d.startsWith("chromium_headless_shell")).sort().pop()}/chrome-headless-shell-linux64/chrome-headless-shell`;
const browser = await chromium.launch({ executablePath: shell, args: ["--no-sandbox"] });
const page = await (await browser.newContext({ viewport: { width: 1500, height: 940 } })).newPage();
const errors = [];
page.on("pageerror", e => errors.push(`exception: ${e.message}`));
page.on("console", m => { if (m.type() === "error") errors.push(`console: ${m.text()}`); });
let bad = 0;
const fail = (what) => { bad++; console.log(`BAD ${what}`); };
const drain = (where) => { while (errors.length) fail(`${where}: ${errors.shift()}`); };
const clip = (text, most) => text.length > most ? text.slice(0, most - 1) + "…" : text;
const wait = (fn, arg, ms = 60000) => page.waitForFunction(fn, arg, { timeout: ms, polling: 50 });

await page.goto(url, { waitUntil: "load" });
await wait(() => typeof RUN !== "undefined" && RUN && NET.n > 0);
console.log(`page ${url}`);
drain("load");

/// The page at rest: nothing moving that a click could miss.
const settle = () => page.waitForTimeout(700);
/// Where on screen node `id` is drawn, if it is clear of the dock and the HUD.
const screenOf = id => page.evaluate(id => {
  const p = POS.get(id), r = cv.getBoundingClientRect(), x = r.left + view.ox + p[0] * view.s, y = r.top + view.oy + p[1] * view.s;
  return x > r.left + 10 && x < r.right - 10 && y > r.top + 80 && y < r.bottom - 210 ? [x, y] : null;
}, id);
const strip = await page.locator("#strip").boundingBox();
const rulesSeen = new Set(), rulesShown = new Set();

const programs = await page.$$eval("#examples option", os => os.map(o => o.value).filter(Boolean));
for (const src of programs) {
  const want = new RLN.Run(src.replace(/;/g, "\n"), { order: "rounds", seed: 7, limit: 400000 });
  const head = `${String(want.n).padStart(6)} ${want.status.padEnd(5)} ${clip(src, 48).padEnd(48)}`;
  let problems = 0;
  const check = (ok, what) => { if (!ok) { problems++; fail(`${src}: ${what}`); } };
  await page.selectOption("#examples", src);
  await wait(src => RUN && $("src").value === src && !dirty, src);
  check(await page.evaluate(() => RUN.at === 0 && /^the program, loaded/.test($("status").textContent)), "not at the start after loading");
  // a few steps forward, one back
  for (let i = 0; i < 3; i++) await page.click("#stepFwd");
  await page.click("#stepBack");
  check(await page.evaluate(n => n === 0 || RUN.at > 0, want.n), "the steps went nowhere");
  // to the end: played, or for a run that the page cuts off, a jump there by the bar's right end
  if (want.n >= 50000) await page.mouse.click(strip.x + strip.width - 1, strip.y + strip.height / 2);
  await page.click("#toEnd");
  await wait(() => RUN.at === RUN.n && mode === 0 && !dirty, null, 120000);
  const end = await page.evaluate(() => ({ n: RUN.n, at: RUN.at, answer: $("answer").textContent, status: $("status").textContent, rules: [...new Set(Array.from(RUN.kinds.slice(0, RUN.n), k => RLN.ruleFor(k >> 4, k & 15)[2]))] }));
  end.rules.forEach(r => rulesSeen.add(r));
  check(end.n === want.n, `${end.n} rewrites, the engine ${want.n}`);
  check(end.answer === (want.answer ? clip(want.answerText, 220) : ""), `answer shown "${clip(end.answer, 80)}", the engine "${clip(want.answerText, 80)}"`);
  check(end.status.includes(`${want.n} rewrites`), `status "${end.status}" does not say ${want.n} rewrites`);
  // the rule box, from the bar, for the first rewrite of each rule (a rewrite at a time, so the bar has every rewrite)
  await page.evaluate(() => { $("byRound").checked = false; dirty = true; });
  const firsts = await page.evaluate(() => { const out = new Map(); for (let e = 0; e < RUN.n; e++) { const k = RUN.kinds[e], r = RLN.ruleFor(k >> 4, k & 15)[2]; if (!out.has(r)) out.set(r, e); } return [...out]; });
  for (const [rule, e] of firsts) {
    if (rulesShown.has(rule) && rule !== "run") continue;
    await page.mouse.move(strip.x + (e + 0.5) / want.n * strip.width, strip.y + strip.height / 2);
    const box = await page.evaluate(() => { const e = hover?.event; return e === undefined ? null : { rule: RLN.ruleFor(RUN.kinds[e] >> 4, RUN.kinds[e] & 15)[2], svg: !!$("ruleBox").querySelector("svg path"), text: $("ruleBox").textContent, lit: $("defs").querySelector(".def.on")?.dataset.k ?? null, dk: RUN.dk[e] }; });
    if (!box) { check(false, `the bar shows no rewrite at ${e}`); continue; }
    check(box.svg && box.text.length > 20, `rule box for ${box.rule} is empty`);
    if (box.dk >= 0) check(box.lit === String(box.dk), `rule ${box.rule}: definition ${box.dk} is not lit (${box.lit})`);
    rulesShown.add(box.rule);
  }
  await page.mouse.move(5, 5);
  await page.evaluate(() => { $("byRound").checked = true; dirty = true; });
  // back to the start, and to the middle, by the bar
  await page.mouse.click(strip.x, strip.y + strip.height / 2);
  check(await page.evaluate(() => RUN.at === 0), "the bar's left end is not the start");
  await page.mouse.click(strip.x + strip.width / 2, strip.y + strip.height / 2);
  const mid = await page.evaluate(() => ({ at: RUN.at, n: RUN.n, status: $("status").textContent }));
  check(want.n < 2 || (mid.at > 0 && mid.at < mid.n), `the bar's middle is rewrite ${mid.at} of ${mid.n}`);
  // a pair fired by hand, clicked where it is drawn
  await settle();
  const pairs = await page.evaluate(() => activePairs().map(([i, j]) => [NET.id[i], NET.id[j]]));
  let fired = "no pair in view";
  for (const [a, b] of pairs) {
    const at = await screenOf(a) ?? await screenOf(b);
    if (!at) continue;
    const before = await page.evaluate(() => [RUN.byHand, RUN.at]);
    await page.mouse.click(at[0], at[1]);
    const after = await page.evaluate(() => [RUN.byHand, RUN.at, RUN.n, RUN.status]);
    if (after[0] === before[0] + 1) {
      fired = `fired a pair at rewrite ${before[1]}`;
      check(after[1] === before[1] + 1, "a pair fired by hand did not step the run");
      if (want.status === "done") check(after[2] === want.n && after[3] === "done", `after a pair fired by hand: ${after[2]} rewrites, ${after[3]}`);
    } else fired = "a click on a pair fired nothing";
    break;
  }
  check(!pairs.length || fired.startsWith("fired"), fired);
  drain(src);
  console.log(`${problems ? "BAD" : "ok "} ${head} ${fired}`);
}

// the rules every run had, each shown in the rule box
const missing = [...rulesSeen].filter(r => !rulesShown.has(r));
if (missing.length) fail(`rules never shown in the rule box: ${missing.join(", ")}`);
console.log(`rule box shown for ${rulesShown.size} rules: ${[...rulesShown].join(" ")}`);
// the rule box from the agent box's links, which show a rule with no rewrite of the run behind it
const linked = await page.evaluate(() => {
  const out = [];
  for (let x = 1; x <= K.eraI; x++) for (let y = 1; y <= K.eraI; y++) if (RLN.ruleFor(x, y)) { ruleKey = ""; showRule(x, y, -1); if (!$("ruleBox").querySelector("svg path")) out.push(`${KINDS[x]}-${KINDS[y]}`); }
  return out;
});
if (linked.length) fail(`rule box empty for ${linked.join(", ")}`);
drain("rule links");

// the tour, every step to its end
const tour = await page.$$eval("#tour a", as => as.length);
for (let i = 0; i < tour; i++) {
  await page.locator("#tour a").nth(i).click();
  const src = await page.evaluate(() => $("src").value), want = new RLN.Run(src, { order: "rounds", seed: 7, limit: 400000 });
  const playing = await page.evaluate(() => mode === 1);
  if (playing) await wait(() => RUN.at === RUN.n && mode === 0, null, 120000);
  const got = await page.evaluate(() => ({ at: RUN.at, n: RUN.n, answer: $("answer").textContent }));
  if (got.n !== want.n) fail(`tour ${i + 1}: ${got.n} rewrites, the engine ${want.n}`);
  if (playing && got.answer !== (want.answer ? clip(want.answerText, 220) : "")) fail(`tour ${i + 1}: answer "${got.answer}"`);
  drain(`tour ${i + 1}`);
  console.log(`ok  tour ${i + 1}: ${clip(src, 60)}${playing ? ` played to rewrite ${got.at}` : " loaded"}`);
}

// screenshots: a table being read, with the definitions in view, and a layout step
if (shots) {
  mkdirSync(shots, { recursive: true });
  const shoot = async (hash, pick, name) => {
    await page.evaluate(h => { location.hash = h; start(); }, hash);
    await page.evaluate(pick);
    await page.waitForTimeout(2500);
    await page.evaluate(() => { $("side").scrollTop = $("ruleBox").offsetTop - 34; });
    await page.screenshot({ path: `${shots}/${name}.png` });
    console.log(`shot ${shots}/${name}.png`);
  };
  /// The moment after the first rewrite of a rule (and, for a layout, one that made the most agents).
  const after = rules => new Function(`const rules = ${JSON.stringify(rules)}; let best = -1;
    for (let e = 0; e < RUN.n; e++) { const k = RUN.kinds[e], r = RLN.ruleFor(k >> 4, k & 15)[2];
      if (rules.includes(r) && (best < 0 || (rules.includes("run") && RUN.madeN[e] > RUN.madeN[best]))) { best = e; if (!rules.includes("run")) break; } }
    $("byRound").checked = false; RUN.seek(best + 1); dirty = true; sync(); alpha = 1; relax(400); fit(); lastRule();`);
  await shoot("src=size (\\x y. y x)", after(["readFun"]), "reading");
  await shoot("src=size (\\x y. y x)", after(["read"]), "read-start");
  await shoot("src=interp ((^d. (\\x. x) (\\y. y)) ρ)", after(["run"]), "layout");
  await page.goto(new URL("icons.html", page.url()).href.split("#")[0], { waitUntil: "load" });
  await page.screenshot({ path: `${shots}/icons.png`, fullPage: true });
  console.log(`shot ${shots}/icons.png`);
  drain("shots");
}

// the icon chooser: every kind, a pick kept, and the reset
await page.goto(new URL("icons.html", url).href, { waitUntil: "load" });
const icons = await page.evaluate(() => ({ kinds: document.querySelectorAll("section.kind").length, cards: document.querySelectorAll(".card").length }));
if (icons.kinds !== 13 || icons.cards !== 52) fail(`icons.html shows ${icons.kinds} kinds and ${icons.cards} drawings, not 13 and 52`);
await page.locator('.card[data-kind="tab"][data-i="2"]').click();
const kept = await page.evaluate(() => JSON.parse(localStorage.getItem("rln.tables.icons") ?? "{}").tab);
if (kept !== 2) fail(`a pick on icons.html was not kept (${kept})`);
await page.click("#reset");
await page.goto(url, { waitUntil: "load" });
await wait(() => typeof RUN !== "undefined" && RUN && NET.n > 0);
drain("icons");
console.log(`ok  icons.html: ${icons.kinds} kinds, ${icons.cards} drawings, a pick kept`);

await browser.close();
console.log(bad ? `FAILED: ${bad} problems` : `ok: ${programs.length} programs played, checked and fired by hand, ${rulesShown.size} rules shown, ${tour} tour steps, the icons`);
process.exit(bad ? 1 : 0);
