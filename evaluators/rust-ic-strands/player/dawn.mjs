// gpu-check.js in Node, on this machine's GPU through Dawn (Chrome's WebGPU, from the `webgpu`
// package): far faster than headless Chromium, which only gets a CPU-side GPU. Use dawn.sh.
//   node dawn.mjs check 'src=fib:1'   |   node dawn.mjs bench 'src=fib:1&lazy=0'
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import vm from "node:vm";
const { create, globals } = await import(process.env.WEBGPU);
const here = dirname(fileURLToPath(import.meta.url));
Object.assign(globalThis, globals);
globalThis.window = globalThis;
Object.defineProperty(globalThis, "navigator", { value: { gpu: create([]) }, configurable: true });
for (const f of ["engine.js", "gpu.js", "gpu-check.js"]) vm.runInThisContext(readFileSync(join(here, f), "utf8"));
const [mode = "check", hash = ""] = process.argv.slice(2);
const v = await StrandsCheck[mode](hash, s => console.log(s)).catch(e => "FAIL " + (e.stack || e));
if (mode === "check" || !v.startsWith("ok")) console.log(v);
process.exit(v.startsWith("ok") ? 0 : 1);
