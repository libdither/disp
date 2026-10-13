// The interaction net of raw trees (inet.js) against the two-way machine (iso.rs, through engine.js): the
// same answers, every wire joined both ways after every meeting. Prints how many meetings each takes.
//   node player/inet.test.mjs [CALL…]
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
const here = dirname(fileURLToPath(import.meta.url));
const b64 = readFileSync(join(here, "engine.js"), "utf8").match(/STRANDS_WASM = "([^"]+)"/)[1];
const E = (await WebAssembly.instantiate(Buffer.from(b64, "base64"), {})).instance.exports;
const enc = new TextEncoder(), dec = new TextDecoder();
const call = (f, s) => { const b = enc.encode(s), p = E.alloc(b.length || 1); new Uint8Array(E.memory.buffer, p, b.length).set(b); return Number(E[f](p, b.length)); };
const text = n => dec.decode(new Uint8Array(E.memory.buffer, E.text_ptr(), n));
await import(join(here, "rawprog.js"));
await import(join(here, "inet.js"));
const { fromEngine } = globalThis.RawProg, { Machine } = globalThis.INet;

const calls = process.argv.slice(2).length ? process.argv.slice(2) :
  ["swap 1 2", "succ 3", "add 0 0", "add 2 3", "add 3 1", "add' 2 5", "snoc [1, 2] 3", "rev [1, 2, 3]", "rev' [3, 2, 1]"];
let bad = 0;
for (const src of calls) {
  if (!call("inet_program", src)) { console.log(`${src}: ${text(E.text_len())}`); bad++; continue; }
  let p; try { p = fromEngine(text(E.text_len())); } catch (e) { console.log(`${src}: ${e.message}`); bad++; continue; }
  const n = call("iso_new", src); E.iso_seek(n); const want = text(E.iso_payload());
  const m = new Machine(p.prog, p.input);
  let most = 0, err = "";
  try { while (m.step()) { m.consistent(); most = Math.max(most, m.n.live().length); } } catch (e) { err = `meeting ${m.steps}: ${e.message}`; }
  const got = err || (m.st.mode === "error" ? `error at meeting ${m.steps}: ${m.st.msg}` : m.answer());
  const ok = got === want;
  if (!ok) bad++;
  console.log(`${ok ? "ok " : "BAD"} ${src}: ${m.steps} meetings, ${m.n.made} agents made, ${m.n.gone} taken away, ${most} at most${ok ? "" : `\n    got  ${got}\n    want ${want}\n    last ${m.last?.rule} (${m.st.mode})`}`);
}
process.exit(bad ? 1 : 0);
