// Run player/gpu-check.html in headless Chromium with WebGPU and print its verdict; exit 0 on "ok".
//   node player/check.mjs 'src=sort:1&temp=2'   (CHROME=path to override the browser, T=ms timeout)
// Headless Chromium usually gets SwiftShader, a CPU-side GPU: slow, but it compiles the shader
// with Chrome's own WGSL compiler (Tint) and runs it under WebGPU's rules.
import { spawn } from "node:child_process";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";
const page = "file://" + join(dirname(fileURLToPath(import.meta.url)), "gpu-check.html") + "#" + (process.argv[2] || "");
const profile = mkdtempSync(join(tmpdir(), "strands-check-"));
const chrome = spawn(process.env.CHROME || "chromium", ["--headless=new", "--remote-debugging-port=0", "--no-first-run", `--user-data-dir=${profile}`,
  "--enable-unsafe-webgpu", "--enable-features=Vulkan", "about:blank"], { stdio: ["ignore", "ignore", "pipe"] });
const finish = code => { chrome.kill(); setTimeout(() => { rmSync(profile, { recursive: true, force: true }); process.exit(code); }, 300); };
let ws;
chrome.stderr.on("data", async d => {
  const m = String(d).match(/ws:\/\/\S+/);
  if (!m || ws) return;
  const targets = await (await fetch(m[0].replace("ws://", "http://").replace(/\/devtools\/browser\/.*/, "/json/list"))).json();
  ws = new WebSocket(targets.find(t => t.type === "page").webSocketDebuggerUrl);
  let id = 0;
  const call = (method, params = {}) => ws.send(JSON.stringify({ id: ++id, method, params }));
  ws.onopen = () => { call("Runtime.enable"); call("Page.navigate", { url: page }); poll(); };
  ws.onmessage = e => {
    const r = JSON.parse(e.data);
    if (r.method === "Runtime.consoleAPICalled") console.log(r.params.args.map(a => a.value ?? a.description).join(" "));
    const v = r.result?.result?.value;
    if (typeof v === "string" && v.startsWith("DONE ")) finish(v.startsWith("DONE ok") ? 0 : 1);
  };
  function poll() { call("Runtime.evaluate", { expression: "document.title" }); setTimeout(poll, 500); }
});
setTimeout(() => { console.log("TIMEOUT"); finish(2); }, +(process.env.T || 600000));
