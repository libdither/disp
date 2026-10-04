// The chip's block schedule on WebGPU: crate/src/gpu/mod.rs's `Grid` for the browser, running the
// shader the engine makes for the loaded configuration (tables.rs `shader`). A grid moves a run's
// sites as the engine's `strands_held` gives them: each site's index, then its 10 words.
window.StrandsGPU = (() => {
  const SITE = 10, REC = SITE + 1, STRIDE = 256, CLOCK = 80, REBUILD = 8;
  // tiles.wgsl `counts`: the list lengths, the tally (`T_*`), then the rewrites' count and sites.
  const TALLY = 11, FIRES_MAX = 4096, COUNTS = 16 + FIRES_MAX;
  const EMPTY = [~0 >>> 0, ~0 >>> 0, ~0 >>> 0, ~0 >>> 0, ~0 >>> 0, ~0 >>> 0, ~0 >>> 0, ~0 >>> 0, 0xFF, 0xFF000000];
  const groups = n => { const g = Math.max(1, Math.ceil(n / 64)); return [Math.min(g, 65535), Math.ceil(g / 65535)]; };
  const S = GPUBufferUsage;

  /// The kernels for one shader. Throws with the reason when the browser or GPU cannot run them.
  async function open(code) {
    if (!navigator.gpu) throw new Error("this browser has no WebGPU");
    const adapter = await navigator.gpu.requestAdapter({ powerPreference: "high-performance" });
    if (!adapter) throw new Error("WebGPU found no GPU");
    // A block's 8 sites and its working copies sit in 30 KB of workgroup memory; the kernels that
    // build the lists see 9 storage buffers.
    const need = { maxComputeWorkgroupStorageSize: 30720, maxStorageBuffersPerShaderStage: 9 };
    for (const [k, v] of Object.entries(need)) if (adapter.limits[k] < v) throw new Error(`this GPU offers ${adapter.limits[k]} of ${k}, the kernels need ${v}`);
    const limits = ["maxComputeWorkgroupStorageSize", "maxStorageBuffersPerShaderStage", "maxStorageBufferBindingSize", "maxBufferSize"];
    const device = await adapter.requestDevice({ requiredLimits: Object.fromEntries(limits.map(k => [k, adapter.limits[k]])) });
    device.pushErrorScope("validation");
    const module = device.createShaderModule({ code });
    const info = await module.getCompilationInfo();
    const errors = info.messages.filter(m => m.type === "error");
    if (errors.length) throw new Error("the shader does not compile: " + errors.slice(0, 3).map(m => `line ${m.lineNum}: ${m.message}`).join("; "));
    const buffer = (binding, type, dynamic) => ({ binding, visibility: GPUShaderStage.COMPUTE, buffer: { type, hasDynamicOffset: !!dynamic, ...(dynamic ? { minBindingSize: CLOCK } : {}) } });
    const layout = device.createBindGroupLayout({ entries: [buffer(0, "uniform", true), ...[1, 2, 3, 4, 5, 6, 7, 8].map(b => buffer(b, "storage"))] });
    // The indirect dispatch's arguments are written only by the kernels that build the lists,
    // which alone see them, so a dispatch never reads them as arguments while writing them.
    const argsLayout = device.createBindGroupLayout({ entries: [buffer(0, "storage")] });
    const run = device.createPipelineLayout({ bindGroupLayouts: [layout] });
    const list = device.createPipelineLayout({ bindGroupLayouts: [layout, argsLayout] });
    const kernels = { tile_mark: run, tile_clear: list, tile_compact: list, tile_args: list, tile_busy: run, busy_args: list, busy_turns: run, busy_pulses: run, busy_gather: run };
    const pipes = {};
    await Promise.all(Object.entries(kernels).map(async ([entryPoint, l]) => {
      pipes[entryPoint] = await device.createComputePipelineAsync({ layout: l, compute: { module, entryPoint } });
    }));
    const err = await device.popErrorScope();
    if (err) throw new Error("the kernels do not build: " + err.message);
    const a = adapter.info || {};
    return { device, layout, argsLayout, pipes, name: [a.vendor, a.architecture, a.description].filter(Boolean).join(" ") || "a GPU" };
  }

  /// A w×h×d lattice on the GPU running up to `batch` clocks per submission.
  class Grid {
    constructor(gpu, w, h, d, batch) {
      Object.assign(this, { gpu, w, h, d, batch, since: 0, cap: 11 * 4096 });
      const dev = gpu.device, n = w * h * d, buf = (bytes, usage) => dev.createBuffer({ size: Math.max(4, bytes), usage });
      this.tb = d === 1 ? [8, 8, 1] : [4, 4, 4];
      this.nb = [(w >> 1) + 1, (h >> 1) + 1, (d >> 1) + 1];
      this.nt = [0, 1, 2].map(i => Math.ceil(this.nb[i] / this.tb[i]));
      const tiles = this.tiles();
      this.sites = buf(n * SITE * 4, S.STORAGE | S.COPY_DST | S.COPY_SRC);
      this.gathered = buf(n * REC * 4, S.STORAGE | S.COPY_SRC);
      this.live = buf(n * 4, S.STORAGE | S.COPY_DST);
      this.pul = buf(2 * n * 4, S.STORAGE | S.COPY_DST);
      this.marks = buf(tiles * 4, S.STORAGE);
      this.list = buf(tiles * 4, S.STORAGE | S.COPY_DST);
      this.counts = buf(COUNTS * 4, S.STORAGE | S.COPY_DST | S.COPY_SRC);
      this.busy = buf(this.nb[0] * this.nb[1] * this.nb[2] * 4, S.STORAGE);
      this.args = buf(6 * 4, S.STORAGE | S.INDIRECT | S.COPY_DST);
      this.clocks = buf((2 * batch + 1) * STRIDE, S.UNIFORM | S.COPY_DST);
      const storage = [this.sites, this.pul, this.gathered, this.marks, this.list, this.counts, this.busy, this.live];
      this.bind = dev.createBindGroup({ layout: gpu.layout, entries: [{ binding: 0, resource: { buffer: this.clocks, size: CLOCK } },
        ...storage.map((b, i) => ({ binding: i + 1, resource: { buffer: b } }))] });
      this.argsBind = dev.createBindGroup({ layout: gpu.argsLayout, entries: [{ binding: 0, resource: { buffer: this.args } }] });
    }
    tiles() { return this.nt[0] * this.nt[1] * this.nt[2]; }
    destroy() { for (const b of [this.sites, this.gathered, this.live, this.pul, this.marks, this.list, this.counts, this.busy, this.args, this.clocks]) b.destroy(); }

    /// Load these sites, every other site empty; each site's pulse goes to both pulse buffers.
    /// Every tile is active until the first clock's rebuild.
    upload(held) {
      const n = this.w * this.h * this.d, q = this.gpu.device.queue;
      const state = new Uint32Array(n * SITE), pul = new Uint32Array(2 * n), live = new Uint32Array(n);
      for (let s = 0; s < n; s++) state.set(EMPTY, s * SITE);
      pul.fill(0xFF);
      for (let i = 0; i < held.length; i += REC) {
        const s = held[i];
        state.set(held.subarray(i + 1, i + REC), s * SITE);
        pul[s] = pul[n + s] = held[i + SITE] >>> 24;
        live[s] = 1;
      }
      q.writeBuffer(this.sites, 0, state);
      q.writeBuffer(this.pul, 0, pul);
      q.writeBuffer(this.counts, 0, new Uint32Array(16));
      q.writeBuffer(this.live, 0, live);
      q.writeBuffer(this.list, 0, Uint32Array.from({ length: this.tiles() }, (_, i) => i));
      q.writeBuffer(this.args, 0, new Uint32Array([this.tiles(), 1, 1, 0, 1, 1]));
      this.since = 0;
      this.cap = Math.max(this.cap, 2 * held.length);
    }

    /// One clock's `Clock` (block.wgsl), padded to the uniform stride.
    clock(c, i, tick, par, fused, list) {
      c.set([this.w, this.h, this.d, tick, ...this.nb, this.tiles(), ...this.tb, ...this.nt, par, fused, list], i * STRIDE / 4);
    }

    /// Clocks `first .. first + n` (n up to the batch) in one submission, as mod.rs `run` without
    /// `dense`; with `gather`, the sites holding anything at the end are gathered for `take`.
    run(first, seed, n, gather) {
      const g = this.gpu, P = g.pipes, mix = Math.imul(seed, 0x9E3779B9);
      const tick = t => ((t >>> 0) ^ mix) >>> 0, last = first + n - 1;
      const c = new Uint32Array((n + 1) * STRIDE / 4);
      for (let i = 0; i < n; i++) this.clock(c, i, tick(first + i), (first + i) & 1, i > 0 ? 1 : 0, (first + i) & 1);
      this.clock(c, n, tick(last), (last & 1) ^ 1, 0, last & 1);
      g.device.queue.writeBuffer(this.clocks, 0, c);
      if (gather) g.device.queue.writeBuffer(this.counts, 12, new Uint32Array(1));
      const enc = g.device.createCommandEncoder(), pass = enc.beginComputePass();
      pass.setBindGroup(1, this.argsBind);
      const [x, y] = groups(this.tiles());
      for (let i = 0; i < n; i++) {
        pass.setBindGroup(0, this.bind, [i * STRIDE]);
        if (this.since % REBUILD === 0) {
          pass.setPipeline(P.tile_clear); pass.dispatchWorkgroups(x, y);
          pass.setPipeline(P.tile_mark); pass.dispatchWorkgroupsIndirect(this.args, 0);
          pass.setPipeline(P.tile_compact); pass.dispatchWorkgroups(x, y);
          pass.setPipeline(P.tile_args); pass.dispatchWorkgroups(1);
        }
        this.since++;
        pass.setPipeline(P.tile_busy); pass.dispatchWorkgroupsIndirect(this.args, 0);
        pass.setPipeline(P.busy_args); pass.dispatchWorkgroups(1);
        pass.setPipeline(P.busy_turns); pass.dispatchWorkgroupsIndirect(this.args, 12);
      }
      pass.setBindGroup(0, this.bind, [n * STRIDE]);
      pass.setPipeline(P.busy_pulses); pass.dispatchWorkgroupsIndirect(this.args, 12);
      if (gather) { pass.setPipeline(P.busy_gather); pass.dispatchWorkgroupsIndirect(this.args, 12); }
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
      const fires = Math.min(v[4 + TALLY], FIRES_MAX);
      return { held, tally: v.subarray(4, 4 + TALLY), fires: v.subarray(5 + TALLY, 5 + TALLY + fires) };
    }
  }
  return { open, Grid, TALLY, REC };
})();
