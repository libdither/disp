// Open the player at URL, play to clock STOP, render every live agent's term as the details drawer does,
// and count what the atoms are: known names, values, ‹code› pieces, and how often a whole term is one ‹…›.
import { spawn } from "node:child_process";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
const [url, stop, out] = process.argv.slice(2);
const profile = mkdtempSync(join(tmpdir(), "drawer-"));
const chrome = spawn("chromium", ["--headless=new", "--remote-debugging-port=0", "--no-first-run", `--user-data-dir=${profile}`,
  "--use-angle=swiftshader", "--enable-unsafe-swiftshader", "--window-size=1280,800", "about:blank"], { stdio: ["ignore", "ignore", "pipe"] });
const finish = code => { chrome.kill(); setTimeout(() => { rmSync(profile, { recursive: true, force: true }); process.exit(code); }, 300); };
setTimeout(() => { console.log("timeout"); finish(2); }, 300000);
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
  ws.onmessage = e => { const r = JSON.parse(e.data); if (r.id && pending.has(r.id)) { pending.get(r.id)(r.result); pending.delete(r.id); } };
  ws.onopen = async () => {
    await call("Page.navigate", { url });
    let set = false;
    for (let i = 0; i < 3000; i++) {
      await sleep(30);
      const c = await ev(`typeof L === "undefined" || !L ? -1 : stats()[1]`);
      if (c >= 0 && !set) { set = true; await ev(`(() => { const sp = $("speed"); sp.value = 60; sp.dispatchEvent(new Event("input")); setPlaying(true); })()`); }
      if (c >= +stop) { await ev("setPlaying(false)"); break; }
    }
    const res = await ev(`(() => {
      nameTable();
      const V = views(), texts = [];
      const n = E.live_len(), live = new Uint32Array(E.memory.buffer, E.live_ptr(), n).slice();
      for (const s of live) for (let k = 0; k < L.ks; k++) {
        const seat = s * L.ks + k; if (!V.tags[seat]) continue;
        const src = E.strands_term(s, k, 4000); if (!src) continue;
        const ast = parseTerm(text(src), seatsOut(E.strands_subtree(s, k)));
        const html = termHTML(ast, { V, block: "m", nodes: [], focus: null, answer: null }, 220);
        const d = document.createElement("div"); d.innerHTML = html;
        // The trees of every value node in the term (code pieces included), as ternary.
        const terns = [];
        const tern = n => { const out = []; const st = [n]; while (st.length) { const m = bare(st.pop()); if (m.k === "L") out.push("0"); else if (m.k === "S") { out.push("1"); st.push(m.x); } else if (m.k === "F") { out.push("2"); st.push(m.y, m.x); } else return null; if (out.length > 60000) return null; } return out.join(""); };
        const walk = (n, depth) => { if (!n || depth > 400) return; const v = val(n); if (v && v.size > SMALL && !NAMED.get(hkey(v)) && dataOf(n) === null) { const t = tern(n); if (t) terns.push(t); return; } for (const c of kidsOf(n)) walk(c, depth + 1); };
        walk(ast, 0);
        // The term itself: @ applications, # shared, values as ternary when they have no name.
        const enc = (n, depth) => { if (!n || depth > 300) return ["…"]; const v = val(n); if (v) { const t = tern(n); return t ? ["tree", t] : ["…"]; } const b = n.k === "#" ? n.body : n; if (b.k === "@") return ["@", enc(b.x, depth + 1), enc(b.y, depth + 1)]; if (b.k === "#") return enc(b.body, depth + 1); return [b.k, ...kidsOf(b).map(c => enc(c, depth + 1))]; };
        const term = enc(ast, 0);
        texts.push({ tag: TAGS[V.tags[seat]], text: d.textContent, nm: d.querySelectorAll(".nm").length, v: d.querySelectorAll(".v").length, ch: d.querySelectorAll(".ch").length, terns, term });
      }
      return texts;
    })()`);
    writeFileSync(out, JSON.stringify(res));
    console.log("agents", res.length, "clock", await ev("stats()[1]"));
    finish(0);
  };
});
