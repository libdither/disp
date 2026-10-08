// The player's GPU path (gpu.js) against its CPU engine, and the two's speeds; run in a browser by
// gpu-check.html and in Node by dawn.mjs. A hash such as 'src=fib:1&lazy=0' picks the term; clocks,
// copies and stretch shape the run; every other key changes a setting of the current design, which
// the player runs (lattice.rs `latest`, `Params::set`: depth, seed, lazy, pairs, temp, demand, ...).
window.StrandsCheck = (() => {
  const enc = new TextEncoder(), dec = new TextDecoder(), R = 12;
  async function engine() {
    const bytes = Uint8Array.from(atob(window.STRANDS_WASM), c => c.charCodeAt(0));
    const E = { ...(await WebAssembly.instantiate(bytes, {})).instance.exports };
    E.text = n => dec.decode(new Uint8Array(E.memory.buffer, E.text_ptr(), n));
    E.withString = (s, f) => { const b = enc.encode(s); const p = E.alloc(b.length || 1); new Uint8Array(E.memory.buffer, p, b.length).set(b); return f(p, b.length); };
    E.stats = () => Array.from(new Float64Array(E.memory.buffer, E.stats_ptr(), 17));
    E.held = () => { const p = E.strands_held(); return new Uint32Array(E.memory.buffer, p, E.words_len()).slice(); };
    /// After a GPU stretch of n clocks: its sites and counts into the engine; 1 once the answer is in.
    E.ran = (n, r) => {
      // The room first: making it may grow the memory, which detaches the old buffer.
      const k = r.held.length, len = k + r.tally.length + r.fires.length, p = E.words_ptr(len), w = new Uint32Array(E.memory.buffer, p, len);
      w.set(r.held); w.set(r.tally, k); w.set(r.fires, k + r.tally.length);
      return E.strands_gpu_ran(n, k, r.fires.length);
    };
    return E;
  }
  function settings(hash) {
    const q = new URLSearchParams(hash), num = (k, d) => q.has(k) ? +q.get(k) : d, seed = num("seed", 7);
    const own = new Set(["src", "clocks", "copies", "stretch", "seed"]);
    const sets = [...q.entries()].filter(([k]) => !own.has(k)).map(([k, v]) => `${k}=${v}`).concat(`seed=${seed}`);
    const apply = E => {
      E.strands_settings();
      for (const kv of sets) if (E.withString(kv, (p, n) => E.strands_set(p, n))) throw new Error(E.text(E.text_len()));
    };
    return { src: q.get("src") || "sort:1", seed, max: num("clocks", 1e9), copies: num("copies", 1), stretch: num("stretch", 256),
      make: (E, w, h, src) => { apply(E); return E.withString(src, (p, n) => E.strands_new(w, h, p, n)); }, apply };
  }
  /// An engine with the term loaded on a lattice sized as the player sizes it.
  async function loaded(S) {
    const E = await engine();
    const agents = E.withString(S.src, (p, n) => E.strands_probe(p, n));
    if (agents < 0) throw new Error(E.text(E.text_len()));
    S.apply(E);
    S.depth = E.strands_depth();
    let side = Math.ceil(Math.sqrt(agents) * (S.depth > 1 ? 7 : 8)) + 16, rc = 2;
    for (let t = 0; t < 6 && rc === 2; t++) { rc = S.make(E, side, side, S.src); if (rc === 2) side = Math.ceil(side * 1.4); }
    if (rc !== 0) throw new Error("load: " + E.text(E.text_len()));
    return { E, side, w: side, h: side, agents };
  }
  async function kernels(E) {
    if (!E.strands_gpu_shader()) throw new Error("the GPU cannot run these settings: " + E.text(E.text_len()));
    return StrandsGPU.open(E.text(E.text_len()));
  }
  // Sites by index, so two engines' lists compare whatever their order.
  function byIndex(held) {
    const idx = Array.from({ length: held.length / R }, (_, i) => i).sort((a, b) => held[a * R] - held[b * R]);
    const v = new Uint32Array(held.length); idx.forEach((j, i) => v.set(held.subarray(j * R, j * R + R), i * R)); return v;
  }
  const same = (a, b) => a.length === b.length && a.every((x, i) => x === b[i]);
  // Stats both must agree on (wasm.rs `stats_ptr`): all but the peaks, which the GPU's engine sees only between stretches.
  const NAMES = ["proposals", "clocks", "fires", "hops", "swaps", "folds", "flips", "strands", null, "blocked", "done", "agents", "walk_ok", "walk_fail", "pulses", null, "collected"];
  const secs = t0 => ((performance.now() - t0) / 1000).toFixed(1) + "s";

  /// The player's way of running the GPU (index.html `gpuStep`): stretches of the given sizes out
  /// `depth` at a time, each taken back into engine E in turn. A stretch that comes back with too
  /// little room drops those still out, and the GPU runs again from E's sites. Returns how many
  /// times that happened.
  async function pipeline(grid, E, first, seed, sizes, depth) {
    let next = 0, at = first, out = [], again = 0;
    const fill = () => { while (out.length < depth && next < sizes.length) { const n = sizes[next++]; out.push({ n, back: grid.stretch(at, seed, n) }); at += n; } };
    fill();
    while (out.length) {
      const s = out.shift(), r = await s.back;
      if (!r) {
        await Promise.all(out.map(o => o.back));
        at -= s.n + out.reduce((a, o) => a + o.n, 0); next -= 1 + out.length; out = [];
        grid.upload(E.held()); again++;
      } else E.ran(s.n, r);
      fill();
    }
    return again;
  }

  /// One engine stays on the CPU; the other is handed back and forth between GPU and CPU, the GPU
  /// running stretches of varying size several at a time, as the player does, and sometimes with
  /// too little room to copy its sites back. After every handover both must hold the same sites
  /// and counts.
  async function check(hash, say) {
    const S = settings(hash), a = await loaded(S), A = a.E, B = await engine();
    if (S.make(B, a.side, a.side, S.src) !== 0) return "FAIL load";
    const t0 = performance.now(), gpu = await kernels(B);
    say(`${S.src} on ${a.side}×${a.side}×${S.depth}, ${a.agents} agents; GPU: ${gpu.name} (ready in ${secs(t0)})`);
    const batches = [1, 7, 13, 64], grid = new StrandsGPU.Grid(gpu, a.side, a.side, S.depth, 64);
    let clocks = 0, stretch = 0, onGpu = 0, again = 0;
    while (clocks < S.max) {
      const n = Math.min(S.max - clocks, 40 + 30 * (stretch % 5)), first = clocks;
      A.fire_log_clear(); B.fire_log_clear();
      if (stretch % 3 === 2) B.strands_clocks(n);
      else {
        grid.upload(B.held());
        const sizes = [];
        for (let c = 0; c < n;) { const m = Math.min(batches[(stretch + c) % 4], n - c); sizes.push(m); c += m; }
        if (stretch % 7 === 4) grid.cap = 8 * StrandsGPU.REC;
        again += await pipeline(grid, B, first, S.seed, sizes, 1 + stretch % 4);
        onGpu += n;
      }
      const fin = A.strands_clocks(n);
      clocks += n; stretch++;
      const sa = A.stats(), sb = B.stats(), off = NAMES.map((k, i) => k && sa[i] !== sb[i] ? `${k} cpu ${sa[i]} handed ${sb[i]}` : null).filter(Boolean);
      if (!same(byIndex(A.held()), byIndex(B.held()))) return `FAIL clock ${clocks}: the sites differ`;
      if (off.length) return `FAIL clock ${clocks}: ${off.join(", ")}`;
      const log = E => Array.from(new Uint32Array(E.memory.buffer, E.fire_log_ptr(), E.fire_log_len())).sort((x, y) => x - y);
      if (!same(log(A), log(B))) return `FAIL clock ${clocks}: rewrites at cpu ${log(A)} handed ${log(B)}`;
      if (fin) break;
    }
    return `ok ${clocks} clocks (${onGpu} on the GPU, ${again} stretches run again) matched, ${A.stats()[2]} rewrites, answer ${A.text(A.strands_answer())}  ${secs(t0)}`;
  }

  /// Clocks a second on the CPU engine and on the GPU, to the answer (or `clocks`), the GPU
  /// handing its sites back to the engine every `stretch` clocks, as many stretches out at once as
  /// the player keeps for this browser's readback time. With
  /// `copies`, that many of the term's starting states side by side, for `clocks` clocks: how
  /// each scales with the work in a clock.
  async function bench(hash, say) {
    const S = settings(hash), a = await loaded(S);
    let make = async () => (await loaded(S)).E, w = a.side, h = a.side;
    if (S.copies > 1) {
      if (S.max >= 1e9) return "FAIL copies need clocks";
      const one = a.E.held(), side = a.side;
      let x0 = 1e9, y0 = 1e9, x1 = 0, y1 = 0;
      for (let i = 0; i < one.length; i += R) { const s = one[i], x = s % side, y = Math.floor(s / side) % side; x0 = Math.min(x0, x); y0 = Math.min(y0, y); x1 = Math.max(x1, x); y1 = Math.max(y1, y); }
      const M = 24, bw = x1 - x0 + 1 + M, bh = y1 - y0 + 1 + M, cols = Math.ceil(Math.sqrt(S.copies)), rows = Math.ceil(S.copies / cols);
      w = cols * bw + M; h = rows * bh + M;
      const held = new Uint32Array(one.length * S.copies);
      for (let k = 0; k < S.copies; k++) {
        const ox = M + (k % cols) * bw, oy = M + Math.floor(k / cols) * bh;
        for (let i = 0; i < one.length; i += R) {
          const s = one[i], x = s % side, y = Math.floor(s / side) % side, z = Math.floor(s / (side * side)), j = k * one.length + i;
          held[j] = (x - x0 + ox) + w * ((y - y0 + oy) + h * z);
          held.set(one.subarray(i + 1, i + R), j + 1);
        }
      }
      make = async () => { const E = await engine(); if (S.make(E, w, h, "L") !== 0) throw new Error("load"); E.ran(0, { held, tally: new Uint32Array(11), fires: new Uint32Array(0) }); return E; };
    }
    let line = `${S.src}${S.copies > 1 ? " ×" + S.copies : ""} on ${w}×${h}×${S.depth}:`;
    if (!/(^|&)cpu=0/.test(hash)) {
      const E = await make(), t0 = performance.now();
      let done = 0;
      while (!done && E.stats()[1] < S.max) done = E.strands_clocks(Math.min(50, S.max - E.stats()[1]));
      const s = E.stats();
      line += ` cpu ${(s[1] / ((performance.now() - t0) / 1000)).toFixed(0)} clocks/s (${s[1]} clocks${done ? ", answer in" : ""});`;
    }
    if (!/(^|&)gpu=0/.test(hash)) {
      const E = await make(), gpu = await kernels(E), grid = new StrandsGPU.Grid(gpu, w, h, S.depth, 64);
      const lat = await StrandsGPU.latency(gpu.device), depth = Math.max(2, Math.ceil(lat / 25) + 1);
      grid.upload(E.held());
      let clock = 0, done = 0, wait = 0, at = 0, out = [];
      const t0 = performance.now();
      while (!done && clock < S.max) {
        while (out.length < depth && at < S.max) { const n = Math.min(S.stretch, S.max - at); out.push({ n, back: grid.stretch(at, S.seed, n) }); at += n; }
        const s = out.shift(), tw = performance.now(), r = await s.back;
        wait += performance.now() - tw;
        if (!r) { await Promise.all(out.map(o => o.back)); out = []; at = clock; grid.upload(E.held()); continue; }
        done = E.ran(s.n, r);
        clock += s.n;
      }
      await Promise.all(out.map(o => o.back));
      const dt = (performance.now() - t0) / 1000;
      line += ` gpu ${(clock / dt).toFixed(0)} clocks/s (${clock} clocks${done ? ", answer in" : ""}; ${(100 * wait / 1000 / dt).toFixed(0)}% waiting on the GPU, readback ${lat.toFixed(0)} ms, ${depth} stretches out) ${gpu.name}`;
    }
    say(line);
    return "ok " + line;
  }
  return { check, bench };
})();
