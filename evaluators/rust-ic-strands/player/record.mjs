// Record rev.html's two-way programs as a video: each scene loads a call, captions it, runs it forward,
// then turns the token around and runs it back. Headless Chromium's screencast frames, timed as they
// came, encoded by ffmpeg into an mp4 a phone plays.
//   node player/record.mjs OUT.mp4 [per|iso]   (needs chromium and ffmpeg on the path; per, the
//   persistent machine, by default)
import { spawn, spawnSync } from "node:child_process";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";
const out = process.argv[2] || "two-way.mp4", engine = process.argv[3] || "per";
const page = "file://" + join(dirname(fileURLToPath(import.meta.url)), "rev.html") + `#engine=${engine}&src=swap%201%202`;
const [W, H] = [1280, 720];
// [call, how to write the carried tree, speed (steps a second), said going forward, said coming back]
const PER_SCENES = [
  ["add 2 3", "nat", 3, "The program is one tree of leaves, stems and forks, the same agents as any data, and it never changes. A token walks it carrying the tree it works on (right) and a stack; what a node does shows only when the token gets there and a rule fires.",
    "Back in time: the same token walks back, each rule firing backward. Nothing was recorded on the way."],
  ["both rev rev [1, 2] [3, 4, 5]", "list", 5, "both: at this node the token splits in two. Its pair waits beside the node (green) with a hole for each half, and a token of its own takes each half: two tokens reversing two lists in the same rev at once.",
    "Each half done went back into its hole, and with both back the fork became one token again. Back in time: the fork opens, both halves set off backward at once, and close into one token at the start."],
  ["both succ rev 3 [1, 2, 3]", "list", 5, "Halves of different lengths: succ's half is back in its hole (a filled dot) after a few moves and waits there while rev's runs. Nothing records how long it waited.",
    "Going back, both halves set off at once, so now the short one waits at its start instead. The way back need not retrace the way there: every move is undone by another, in whatever order, and it ends at the same start."],
  ["tck F(S(F(L,L)),F(L,L)) S(L)", "raw", 3, "The tree calculus as a program on this machine. Its S rule, S c b x = c x (b x): x is copied, and c x and b x are worked out at once by two tokens, c x going on with the trace and b x starting one of its own; then the third application.",
    "Back: the third application undone, the token splits again, both halves walk back at once, and the copy of x is taken back."],
  ["tck add 1 1", "nat", 1500, "disp's add 1 1 on it: S rules split the token again and again, four tokens at most. Worked out eagerly, disp's programs leave little to do at once: 5,541 moves in 3,939 rounds.",
    "Back by rounds, all the way to add, 1 and 1."],
];
const ISO_SCENES = [
  ["swap 1 2", "nat", 1.2, "A two-way program is a tree of steps. This one is one step: take the pair (a, b) apart and put it back together as (b, a).",
    "Turned around, the token walks back, and the step undoes itself: (b, a) becomes (a, b) again."],
  ["succ 3", "nat", 1.2, "succ puts a fork with a leaf on top: n → n + 1 (in disp a number is n forks over a leaf).",
    "Back, the step takes the fork off again. (On 0 there is nothing to take off: succ backward is only defined from 1 up.)"],
  ["add 2 3", "nat", 2.6, "add: (a, b) → (a, a + b). A choice: if a is 0, done; else move one fork from a over to b, and call add on the rest. Each call lays out a copy of add underneath.",
    "Back: the token walks out of each copy, which is taken away again, and the forks move back to a. Nothing was recorded on the way."],
  ["double 2", "nat", 4.5, "double: copy a, then add: a → (a, a + a). Copying loses nothing...",
    "...and coming back, un-copy takes the copy back only after checking that both halves are equal."],
  ["revfast [1, 2, 3]", "list", 3.5, "A loop reverses a list, moving each head onto an accumulator. Its test on the way in (the accumulator is empty) and on the way out (the list is empty) tell the way back where to stop.",
    "Back: the heads move back one at a time until the accumulator is empty again, the loop's way in."],
  ["tc F(L,S(L)) F(L,L)", "raw", 1.4, "The tree calculus, derived. Its K rule: K b x = b throws x away. Here K b is the tree (△, b), and x goes onto a trace (the last part) instead.",
    "Back: x comes off the trace and back beside the program. Keeping the trace is what lets an ordinary (one-way) program run back."],
  ["tc F(S(F(L,L)),F(L,L)) S(L)", "raw", 6, "Its S rule: S c b x = c x (b x). x is copied, then three applications, each a call to tc laid out underneath.",
    "Back: the applications undone one by one, and the two copies of x checked equal and merged."],
];
const SCENES = engine === "per" ? PER_SCENES : ISO_SCENES;
const profile = mkdtempSync(join(tmpdir(), "record-"));
const chrome = spawn("chromium", ["--headless=new", "--remote-debugging-port=0", "--no-first-run", `--user-data-dir=${profile}`,
  "--use-angle=swiftshader", "--enable-unsafe-swiftshader", `--window-size=${W},${H}`, "about:blank"], { stdio: ["ignore", "ignore", "pipe"] });
const frames = [];
const finish = code => { chrome.kill(); setTimeout(() => { rmSync(profile, { recursive: true, force: true }); process.exit(code); }, 300); };
setTimeout(() => { console.log("timeout"); finish(2); }, 15 * 60000);
let ws;
chrome.stderr.on("data", async d => {
  const m = String(d).match(/ws:\/\/\S+/);
  if (!m || ws) return;
  const targets = await (await fetch(m[0].replace("ws://", "http://").replace(/\/devtools\/browser\/.*/, "/json/list"))).json();
  ws = new WebSocket(targets.find(t => t.type === "page").webSocketDebuggerUrl);
  let id = 0; const pending = new Map();
  const call = (method, params = {}) => new Promise(res => { pending.set(++id, res); ws.send(JSON.stringify({ id, method, params })); });
  const ev = async expr => { const r = await call("Runtime.evaluate", { expression: expr, awaitPromise: true, returnByValue: true }); if (r.exceptionDetails) console.log("EXC", JSON.stringify(r.exceptionDetails).slice(0, 400)); return r.result?.value; };
  const sleep = ms => new Promise(r => setTimeout(r, ms));
  ws.onmessage = e => {
    const r = JSON.parse(e.data);
    if (r.method === "Page.screencastFrame") { frames.push({ t: r.params.metadata.timestamp, data: r.params.data }); call("Page.screencastFrameAck", { sessionId: r.params.sessionId }); }
    if (r.method === "Runtime.exceptionThrown") console.log("PAGE EXC", JSON.stringify(r.params.exceptionDetails).slice(0, 400));
    if (r.id && pending.has(r.id)) { pending.get(r.id)(r.result); pending.delete(r.id); }
  };
  ws.onopen = async () => {
    await call("Runtime.enable");
    await call("Emulation.setDeviceMetricsOverride", { width: W, height: H, deviceScaleFactor: 1, mobile: false });
    await call("Page.navigate", { url: page });
    for (let i = 0; i < 300; i++) { await sleep(100); if (await ev(`typeof two !== "undefined" && two()?.st.ok`)) break; }
    // Video mode: no side panel or dock; a caption at the bottom; bigger.
    await ev(`(() => {
      document.getElementById("side").hidden = true; document.getElementById("dock").hidden = true;
      const c = document.createElement("div"); c.id = "caption";
      c.style.cssText = "position:absolute;z-index:9;left:50%;bottom:18px;transform:translateX(-50%);width:88%;padding:12px 20px;border-radius:12px;background:rgba(11,14,20,.88);border:1px solid #2a3a52;color:#e6edf3;font:22px/1.4 system-ui,sans-serif;text-align:center";
      document.getElementById("stage").append(c);
      document.getElementById("answer").hidden = true; document.getElementById("status").style.fontSize = "15px";
      document.getElementById("speed").min = -80; maxScale = 3; fitMargins = [70, 130, 0, innerWidth * 0.38];
      TWO_VIEW.panel = { x: 0.63, y: 0.11, w: 0.35, h: 0.6 };
      dispatchEvent(new Event("resize"));
    })()`);
    const say = async text => ev(`document.getElementById("caption").textContent = ${JSON.stringify(text)}`);
    // steps a second → the page's speed setting (rate() is per frame, at about 60 frames a second)
    const speed = s => 25 * (Math.log10(s / 60) + 1.2);
    const until = async (expr, most) => { for (let i = 0; i < most * 10; i++) { await sleep(100); if (await ev(expr)) return; } };
    await call("Page.startScreencast", { format: "jpeg", quality: 82, maxWidth: W, maxHeight: H, everyNthFrame: 1 });
    for (const [src, read, sps, fwd, back] of SCENES) {
      await ev(`(async () => { TWO_VIEW.read = ${JSON.stringify(read)}; document.getElementById("src").value = ${JSON.stringify(src)}; await load(); follow = true; })()`);
      await say(fwd);
      await ev(`(() => { const s = document.getElementById("speed"); s.value = ${speed(sps)}; s.oninput(); })()`);
      await sleep(2600);
      await ev(`setMode(1)`);
      await until(`mode === 0`, 120);
      await sleep(1800);
      await say(back);
      await sleep(1600);
      await ev(`setMode(-1)`);
      await until(`mode === 0`, 120);
      await sleep(2200);
    }
    await say(engine === "per" ? "One program tree that never changes, tokens that split and come back together, moving in any order, and nothing recorded."
      : "Two-way programs: running backward is the token turning around. Nothing is recorded, except what a program would throw away.");
    await sleep(3500);
    await call("Page.stopScreencast");
    await sleep(300);
    // frames, each shown until the next one came
    const dir = mkdtempSync(join(tmpdir(), "frames-"));
    let list = "ffconcat version 1.0\n";
    frames.forEach((f, i) => {
      const name = join(dir, `f${String(i).padStart(6, "0")}.jpg`);
      writeFileSync(name, Buffer.from(f.data, "base64"));
      const dur = i + 1 < frames.length ? frames[i + 1].t - f.t : 2;
      list += `file '${name}'\nduration ${Math.max(0.001, dur).toFixed(4)}\n`;
    });
    list += `file '${join(dir, `f${String(frames.length - 1).padStart(6, "0")}.jpg`)}'\n`;
    writeFileSync(join(dir, "list.txt"), list);
    const r = spawnSync("ffmpeg", ["-y", "-loglevel", "error", "-f", "concat", "-safe", "0", "-i", join(dir, "list.txt"),
      "-vf", "fps=30,format=yuv420p", "-c:v", "libx264", "-preset", "medium", "-crf", "23", "-movflags", "+faststart", out], { stdio: "inherit" });
    rmSync(dir, { recursive: true, force: true });
    console.log(`${frames.length} frames, ${((frames.at(-1).t - frames[0].t)).toFixed(1)} s → ${out} (ffmpeg ${r.status})`);
    finish(r.status ?? 1);
  };
});
