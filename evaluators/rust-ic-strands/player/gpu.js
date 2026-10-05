// The chip's block schedule on WebGPU: crate/src/gpu/mod.rs's `Grid` for the browser, running the
// shader the engine makes for the loaded configuration (tables.rs `shader`). A grid moves a run's
// sites as the engine's `strands_held` gives them: each site's index, then its 10 words.
window.StrandsGPU = (() => {
  const SITE = 10, REC = SITE + 1, STRIDE = 256, CLOCK = 80;
  // busy.wgsl `counts`: the list lengths, the tally (`T_*`), then the rewrites' count and sites;
  // `NO_LIST`: a clock that fills no busy list.
  const TALLY = 11, FIRES_MAX = 4096, COUNTS = 16 + FIRES_MAX, NO_LIST = 3;
  const EMPTY = [~0 >>> 0, ~0 >>> 0, ~0 >>> 0, ~0 >>> 0, ~0 >>> 0, ~0 >>> 0, ~0 >>> 0, ~0 >>> 0, 0xFF, 0xFF000000];
  const groups = n => { const g = Math.max(1, Math.ceil(n / 64)); return [Math.min(g, 65535), Math.ceil(g / 65535)]; };
  const S = GPUBufferUsage;

  /// The kernels for one shader. Throws with the reason when the browser or GPU cannot run them.
  async function open(code) {
    if (!navigator.gpu) throw new Error("this browser has no WebGPU");
    const adapter = await navigator.gpu.requestAdapter({ powerPreference: "high-performance" });
    if (!adapter) throw new Error("WebGPU found no GPU");
    // A block's 8 sites and its working copies sit in 30 KB of workgroup memory.
    const need = { maxComputeWorkgroupStorageSize: 30720 };
    for (const [k, v] of Object.entries(need)) if (adapter.limits[k] < v) throw new Error(`this GPU offers ${adapter.limits[k]} of ${k}, the kernels need ${v}`);
    const limits = ["maxComputeWorkgroupStorageSize", "maxStorageBufferBindingSize", "maxBufferSize"];
    const device = await adapter.requestDevice({ requiredLimits: Object.fromEntries(limits.map(k => [k, adapter.limits[k]])) });
    device.pushErrorScope("validation");
    const module = device.createShaderModule({ code });
    const info = await module.getCompilationInfo();
    const errors = info.messages.filter(m => m.type === "error");
    if (errors.length) throw new Error("the shader does not compile: " + errors.slice(0, 3).map(m => `line ${m.lineNum}: ${m.message}`).join("; "));
    const buffer = (binding, type, dynamic) => ({ binding, visibility: GPUShaderStage.COMPUTE, buffer: { type, hasDynamicOffset: !!dynamic, ...(dynamic ? { minBindingSize: CLOCK } : {}) } });
    const layout = device.createBindGroupLayout({ entries: [buffer(0, "uniform", true), ...[1, 2, 3, 4, 5, 6].map(b => buffer(b, "storage"))] });
    const run = device.createPipelineLayout({ bindGroupLayouts: [layout] });
    const pipes = {};
    await Promise.all(["seed_busy", "clock_turns", "busy_pulses", "busy_gather"].map(async entryPoint => {
      pipes[entryPoint] = await device.createComputePipelineAsync({ layout: run, compute: { module, entryPoint } });
    }));
    const err = await device.popErrorScope();
    if (err) throw new Error("the kernels do not build: " + err.message);
    const a = adapter.info || {};
    return { device, layout, pipes, name: [a.vendor, a.architecture, a.description].filter(Boolean).join(" ") || "a GPU" };
  }

  /// A w×h×d lattice on the GPU running up to `batch` clocks per submission (mod.rs `Grid`).
  class Grid {
    constructor(gpu, w, h, d, batch) {
      // `busyHint`: about how many blocks are busy, which sets how many workgroups share them out.
      Object.assign(this, { gpu, w, h, d, batch, seeded: false, busyHint: 64, cap: 11 * 4096 });
      const dev = gpu.device, n = w * h * d, buf = (bytes, usage) => dev.createBuffer({ size: Math.max(4, bytes), usage });
      this.nb = [(w >> 1) + 1, (h >> 1) + 1, (d >> 1) + 1];
      const blocks = this.blocks();
      this.sites = buf(n * SITE * 4, S.STORAGE | S.COPY_DST | S.COPY_SRC);
      this.gathered = buf(n * REC * 4, S.STORAGE | S.COPY_SRC);
      this.pul = buf(2 * n * 4, S.STORAGE | S.COPY_DST);
      this.marks = buf(blocks * 4, S.STORAGE | S.COPY_DST);
      this.busy = buf(3 * blocks * 4, S.STORAGE);
      this.counts = buf(COUNTS * 4, S.STORAGE | S.COPY_DST | S.COPY_SRC);
      this.clocks = buf((batch + 2) * STRIDE, S.UNIFORM | S.COPY_DST);
      const storage = [this.sites, this.pul, this.gathered, this.marks, this.busy, this.counts];
      this.bind = dev.createBindGroup({ layout: gpu.layout, entries: [{ binding: 0, resource: { buffer: this.clocks, size: CLOCK } },
        ...storage.map((b, i) => ({ binding: i + 1, resource: { buffer: b } }))] });
    }
    blocks() { return this.nb[0] * this.nb[1] * this.nb[2]; }
    destroy() { for (const b of [this.sites, this.gathered, this.pul, this.marks, this.busy, this.counts, this.clocks]) b.destroy(); }

    /// Load these sites, every other site empty; each site's pulse goes to both pulse buffers.
    upload(held) {
      const n = this.w * this.h * this.d, q = this.gpu.device.queue;
      const state = new Uint32Array(n * SITE), pul = new Uint32Array(2 * n);
      for (let s = 0; s < n; s++) state.set(EMPTY, s * SITE);
      pul.fill(0xFF);
      for (let i = 0; i < held.length; i += REC) {
        const s = held[i];
        state.set(held.subarray(i + 1, i + REC), s * SITE);
        pul[s] = pul[n + s] = held[i + SITE] >>> 24;
      }
      q.writeBuffer(this.sites, 0, state);
      q.writeBuffer(this.pul, 0, pul);
      q.writeBuffer(this.counts, 0, new Uint32Array(16));
      q.writeBuffer(this.marks, 0, new Uint32Array(this.blocks()));
      this.seeded = false;
      this.busyHint = held.length / REC;
      this.cap = Math.max(this.cap, 2 * held.length);
    }

    /// One `Clock` (block.wgsl) at entry i: [tick, the next clock's tick, the stamp of the list it
    /// fills, that list, the list it empties, its pulse buffer, fused, the list it runs].
    clock(c, i, f) { c.set([this.w, this.h, this.d, f[0], ...this.nb, this.w * this.h * this.d, f[1], f[2], f[3], f[4], 0, 0, f[5], f[6], f[7]], i * STRIDE / 4); }

    /// Clocks `first .. first + n` (n up to the batch) in one submission, each one dispatch over
    /// its busy blocks (mod.rs `run` without `dense`); with `gather`, the sites holding anything
    /// at the end are gathered for `take`.
    run(first, seed, n, gather) {
      const g = this.gpu, P = g.pipes, mix = Math.imul(seed, 0x9E3779B9);
      const tick = t => ((t >>> 0) ^ mix) >>> 0, last = first + n - 1;
      // Clock t runs list t % 3 and fills list (t + 1) % 3, stamping its blocks t + 2.
      const turns = (t, next, par, fused) => [tick(t), tick(t + 1), (t + 2) >>> 0, next, (t + 2) % 3, par, fused, t % 3];
      const c = new Uint32Array((n + 2) * STRIDE / 4);
      for (let i = 0; i < n; i++) this.clock(c, i, turns(first + i, (first + i + 1) % 3, (first + i) & 1, i > 0 ? 1 : 0));
      this.clock(c, n, turns(last, NO_LIST, (last & 1) ^ 1, 0));
      this.clock(c, n + 1, [0, tick(first), (first + 1) >>> 0, first % 3, NO_LIST, 0, 0, NO_LIST]);
      g.device.queue.writeBuffer(this.clocks, 0, c);
      if (gather) g.device.queue.writeBuffer(this.counts, 12, new Uint32Array(1));
      const wide = Math.min(65535, Math.ceil((this.busyHint * 1.5 + 64) / 64));
      const enc = g.device.createCommandEncoder(), pass = enc.beginComputePass();
      if (!this.seeded) {
        pass.setBindGroup(0, this.bind, [(n + 1) * STRIDE]);
        pass.setPipeline(P.seed_busy); pass.dispatchWorkgroups(...groups(this.w * this.h * this.d));
        this.seeded = true;
      }
      pass.setPipeline(P.clock_turns);
      for (let i = 0; i < n; i++) { pass.setBindGroup(0, this.bind, [i * STRIDE]); pass.dispatchWorkgroups(wide); }
      pass.setBindGroup(0, this.bind, [n * STRIDE]);
      pass.setPipeline(P.busy_pulses); pass.dispatchWorkgroups(wide);
      if (gather) { pass.setPipeline(P.busy_gather); pass.dispatchWorkgroups(wide); }
      pass.end();
      g.device.queue.submit([enc.finish()]);
    }

    /// Copy words of `src` from word `at` out and wait for them.
    async read(parts) {
      const dev = this.gpu.device, total = parts.reduce((a, p) => a + p.words, 0);
      const staging = dev.createBuffer({ size: Math.max(4, total * 4), usage: S.MAP_READ | S.COPY_DST });
      const enc = dev.createCommandEncoder();
      let o = 0;
      for (const p of parts) { if (p.words) enc.copyBufferToBuffer(p.src, p.at * 4, staging, o * 4, p.words * 4); o += p.words; }
      dev.queue.submit([enc.finish()]);
      await staging.mapAsync(GPUMapMode.READ);
      const v = new Uint32Array(staging.getMappedRange().slice(0));
      staging.destroy();
      return v;
    }

    /// After a run that gathered: the sites holding anything (as `upload` takes them), what the
    /// turns did since the last take, and the sites of their first rewrites; then a fresh tally.
    async take() {
      const room = Math.min(this.cap, this.w * this.h * this.d * REC);
      const v = await this.read([{ src: this.counts, at: 0, words: COUNTS }, { src: this.gathered, at: 0, words: room }]);
      this.gpu.device.queue.writeBuffer(this.counts, 16, new Uint32Array(12));
      const n = v[3] * REC;
      let held = v.subarray(COUNTS, COUNTS + Math.min(n, room));
      if (n > room) {
        const rest = await this.read([{ src: this.gathered, at: room, words: n - room }]);
        const all = new Uint32Array(n); all.set(held); all.set(rest, room); held = all;
      }
      this.cap = Math.max(this.cap, Math.ceil(n * 1.5));
      this.busyHint = v[3];
      const fires = Math.min(v[4 + TALLY], FIRES_MAX);
      return { held, tally: v.subarray(4, 4 + TALLY), fires: v.subarray(5 + TALLY, 5 + TALLY + fires) };
    }
  }
  return { open, Grid, TALLY, REC };
})();
