//! The chip's block schedule on a GPU (wgpu on Vulkan): the kernels of `block.wgsl` over the
//! lattice's sites in one storage buffer, a batch of clocks per submission, each clock one dispatch
//! over the blocks holding something (`busy.wgsl`).

use crate::lattice::Params;
use std::num::NonZeroU64;
use wgpu::{BufferUsages as U, ShaderStages};

/// Words per site (bytes as lattice.rs `dump_state`).
pub const SITE: usize = 10;
/// Words per recorded turn or block (block.wgsl `REC`).
pub const REC: usize = 93;
/// Bytes between two `Clock` entries of the uniform buffer.
const STRIDE: u64 = 256;
/// Bytes of a `Clock`.
const CLOCK: u64 = 80;
/// Records replayed per dispatch.
const RECS_PER_DISPATCH: usize = 1 << 17;
/// Words of busy.wgsl `counts`: the list lengths, the tally, and the rewrites' sites.
const COUNTS: usize = 16 + FIRES_MAX;
/// Of the tally (busy.wgsl `T_*`): what the turns did since it was last taken.
pub const TALLY: usize = 11;
/// Rewrites whose sites are kept per tally.
pub const FIRES_MAX: usize = 4096;
/// busy.wgsl `NO_LIST`: a dense clock makes no busy list.
const NO_LIST: u32 = 3;

/// Invocations in a workgroup of the kernels that keep a block in workgroup memory (prelude.wgsl `WG`).
const WG: u32 = 16;
/// Workgroups of `size` covering n invocations (the kernels index by `g.x + g.y * num_workgroups.x * size`).
fn groups(n: u32, size: u32) -> (u32, u32) { let g = n.div_ceil(size); (g.min(65535), g.div_ceil(65535)) }

pub struct Gpu {
    pub device: wgpu::Device,
    pub queue: wgpu::Queue,
    pub name: String,
    layout: wgpu::BindGroupLayout,
    turns: wgpu::ComputePipeline,
    pulses: wgpu::ComputePipeline,
    vectors: wgpu::ComputePipeline,
    seed_busy: wgpu::ComputePipeline,
    clock_turns: wgpu::ComputePipeline,
    busy_pulses: wgpu::ComputePipeline,
    busy_gather: wgpu::ComputePipeline,
}

impl Gpu {
    /// The kernels for configuration p (see tables.rs `gpu_unfit`).
    pub fn new(p: &Params) -> Result<Gpu, String> {
        if let Some(why) = crate::tables::gpu_unfit(p) { return Err(format!("the GPU kernels cannot run this configuration: {why}")); }
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor { backends: wgpu::Backends::VULKAN, ..wgpu::InstanceDescriptor::new_without_display_handle() });
        let adapter = pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance, ..Default::default() })).map_err(|e| format!("no Vulkan adapter: {e}"))?;
        let info = adapter.get_info();
        let (device, queue) = pollster::block_on(adapter.request_device(&wgpu::DeviceDescriptor {
            label: Some("strands"), required_limits: adapter.limits(), ..Default::default() })).map_err(|e| format!("no device: {e}"))?;
        let scope = device.push_error_scope(wgpu::ErrorFilter::Validation);
        // Every loop in the kernels is bounded, so wgpu need not count their iterations; its
        // counters keep the driver from unrolling the loops and cost a quarter of a clock.
        let checks = wgpu::ShaderRuntimeChecks { force_loop_bounding: false, ..wgpu::ShaderRuntimeChecks::checked() };
        let source = wgpu::ShaderSource::Wgsl(crate::tables::shader(p).into());
        let module = unsafe { device.create_shader_module_trusted(wgpu::ShaderModuleDescriptor { label: Some("strands"), source }, checks) };
        let buffer = |binding, ty, dynamic, min: Option<u64>| wgpu::BindGroupLayoutEntry { binding, visibility: ShaderStages::COMPUTE, count: None,
            ty: wgpu::BindingType::Buffer { ty, has_dynamic_offset: dynamic, min_binding_size: min.and_then(NonZeroU64::new) } };
        let storage = wgpu::BufferBindingType::Storage { read_only: false };
        let layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor { label: Some("strands"), entries: &[
            buffer(0, wgpu::BufferBindingType::Uniform, true, Some(CLOCK)), buffer(1, storage, false, None), buffer(2, storage, false, None),
            buffer(3, storage, false, None), buffer(4, storage, false, None), buffer(5, storage, false, None), buffer(6, storage, false, None)] });
        let run_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor { label: Some("run"), bind_group_layouts: &[Some(&layout)], immediate_size: 0 });
        let pipeline = |entry, pl: &wgpu::PipelineLayout| device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor { label: Some(entry), layout: Some(pl), module: &module,
            entry_point: Some(entry), cache: None,
            // Every kernel writes a slot of workgroup memory before reading it, so it needs no clearing.
            compilation_options: wgpu::PipelineCompilationOptions { zero_initialize_workgroup_memory: false, ..Default::default() } });
        let gpu = Gpu {
            turns: pipeline("turns", &run_layout), pulses: pipeline("pulses", &run_layout), vectors: pipeline("vectors", &run_layout),
            seed_busy: pipeline("seed_busy", &run_layout), clock_turns: pipeline("clock_turns", &run_layout),
            busy_pulses: pipeline("busy_pulses", &run_layout), busy_gather: pipeline("busy_gather", &run_layout),
            name: format!("{} ({:?}, {})", info.name, info.backend, info.driver_info), device, queue, layout,
        };
        if let Some(e) = pollster::block_on(scope.pop()) { return Err(format!("shader: {e}")); }
        Ok(gpu)
    }

    fn buffer(&self, bytes: u64, usage: U) -> wgpu::Buffer {
        self.device.create_buffer(&wgpu::BufferDescriptor { label: None, size: bytes.max(4), usage, mapped_at_creation: false })
    }

    fn bind(&self, clocks: &wgpu::Buffer, storage: [&wgpu::Buffer; 6]) -> wgpu::BindGroup {
        let clk = wgpu::BindingResource::Buffer(wgpu::BufferBinding { buffer: clocks, offset: 0, size: NonZeroU64::new(CLOCK) });
        let mut entries = vec![wgpu::BindGroupEntry { binding: 0, resource: clk }];
        for (i, b) in storage.into_iter().enumerate() { entries.push(wgpu::BindGroupEntry { binding: i as u32 + 1, resource: b.as_entire_binding() }); }
        self.device.create_bind_group(&wgpu::BindGroupDescriptor { label: None, layout: &self.layout, entries: &entries })
    }

    /// Copy `words` words of `src` from word `at` out through `staging` and wait for them.
    fn read(&self, src: &wgpu::Buffer, at: usize, staging: &wgpu::Buffer, words: usize) -> Vec<u32> {
        if words == 0 { return vec![]; }
        let bytes = words as u64 * 4;
        let mut enc = self.device.create_command_encoder(&Default::default());
        enc.copy_buffer_to_buffer(src, at as u64 * 4, staging, 0, bytes);
        self.queue.submit([enc.finish()]);
        staging.map_async(wgpu::MapMode::Read, ..bytes, |r| r.expect("map the staging buffer"));
        self.device.poll(wgpu::PollType::wait_indefinitely()).expect("wait for the GPU");
        let view = staging.get_mapped_range(..bytes).expect("mapped");
        let v = bytemuck::cast_slice::<u8, u32>(&view).to_vec();
        drop(view);
        staging.unmap();
        v
    }

    /// Replay records (REC words each, block.wgsl `vectors`) in place: each record's sites become
    /// the sites after its turn or block, and its last two words the turn's touched positions and
    /// whether it was dropped.
    pub fn replay(&self, recs: &mut [u32]) {
        let n = (recs.len() / REC).min(RECS_PER_DISPATCH);
        if n == 0 { return; }
        let clocks = self.buffer(STRIDE, U::UNIFORM | U::COPY_DST);
        let small = || self.buffer(4, U::STORAGE);
        let (sites, pul, marks, busy) = (small(), small(), small(), small());
        let counts = self.buffer(COUNTS as u64 * 4, U::STORAGE);
        let buf = self.buffer((n * REC * 4) as u64, U::STORAGE | U::COPY_DST | U::COPY_SRC);
        let staging = self.buffer((n * REC * 4) as u64, U::MAP_READ | U::COPY_DST);
        let bind = self.bind(&clocks, [&sites, &pul, &buf, &marks, &busy, &counts]);
        for chunk in recs.chunks_mut(n * REC) {
            let m = (chunk.len() / REC) as u32;
            let mut c = [0u32; STRIDE as usize / 4];
            c[7] = m;
            self.queue.write_buffer(&clocks, 0, bytemuck::cast_slice(&c));
            self.queue.write_buffer(&buf, 0, bytemuck::cast_slice(chunk));
            let mut enc = self.device.create_command_encoder(&Default::default());
            {
                let mut pass = enc.begin_compute_pass(&Default::default());
                pass.set_pipeline(&self.vectors);
                pass.set_bind_group(0, &bind, &[0]);
                let (x, y) = groups(m, WG);
                pass.dispatch_workgroups(x, y, 1);
            }
            self.queue.submit([enc.finish()]);
            chunk.copy_from_slice(&self.read(&buf, 0, &staging, chunk.len()));
        }
    }
}

/// A lattice held on the GPU.
pub struct Grid<'g> {
    gpu: &'g Gpu,
    pub w: u32,
    pub h: u32,
    pub d: u32,
    batch: usize,
    /// About how many blocks are busy: the turn kernel's workgroups share them out, so this sets
    /// only how many run at once. Taken from the state at upload and from each gather.
    pub busy_hint: u32,
    /// The first clock needs its busy list made from the state (after an upload).
    seeded: bool,
    /// The pulse buffer the last clock wrote, which holds the field.
    last_par: usize,
    sites: wgpu::Buffer,
    staging: wgpu::Buffer,
    pul: wgpu::Buffer,
    marks: wgpu::Buffer,
    counts: wgpu::Buffer,
    gathered: wgpu::Buffer,
    clocks: wgpu::Buffer,
    bind: wgpu::BindGroup,
}

impl<'g> Grid<'g> {
    /// A w×h×d lattice running up to `batch` clocks per submission.
    pub fn new(gpu: &'g Gpu, w: u32, h: u32, d: u32, batch: usize) -> Grid<'g> {
        let n = (w * h * d) as u64;
        let blocks = ((w / 2 + 1) * (h / 2 + 1) * (d / 2 + 1)) as u64;
        let sites = gpu.buffer(n * SITE as u64 * 4, U::STORAGE | U::COPY_DST | U::COPY_SRC);
        let gathered = gpu.buffer(n * (SITE as u64 + 2) * 4, U::STORAGE | U::COPY_SRC);
        let staging = gpu.buffer((n * (SITE as u64 + 2)).max(COUNTS as u64) * 4, U::MAP_READ | U::COPY_DST);
        let pul = gpu.buffer(2 * n * 4, U::STORAGE | U::COPY_DST | U::COPY_SRC);
        let marks = gpu.buffer(blocks * 4, U::STORAGE | U::COPY_DST);
        let busy = gpu.buffer(3 * blocks * 4, U::STORAGE);
        let counts = gpu.buffer(COUNTS as u64 * 4, U::STORAGE | U::COPY_DST | U::COPY_SRC);
        let clocks = gpu.buffer((2 * batch.max(1) as u64 + 2) * STRIDE, U::UNIFORM | U::COPY_DST);
        let bind = gpu.bind(&clocks, [&sites, &pul, &gathered, &marks, &busy, &counts]);
        Grid { gpu, w, h, d, batch: batch.max(1), busy_hint: 64, seeded: false, last_par: 0, sites, staging, pul, marks, counts, gathered, clocks, bind }
    }

    fn sites(&self) -> usize { (self.w * self.h * self.d) as usize }
    fn blocks(&self) -> u32 { (self.w / 2 + 1) * (self.h / 2 + 1) * (self.d / 2 + 1) }

    /// Load a state of `SITE` words per site, and its demand field (a value a site) if it keeps one:
    /// each site's pulse word (its pulse, its field above it) goes to both pulse buffers.
    pub fn upload(&mut self, state: &[u32], field: Option<&[u8]>) {
        assert_eq!(state.len(), self.sites() * SITE, "a state of {SITE} words per site");
        const EMPTY: [u32; SITE] = [!0, !0, !0, !0, !0, !0, !0, !0, 0xFF, 0xFF00_0000];
        let fv = |i: usize| field.map_or(0, |f| f[i] as u32);
        let pul: Vec<u32> = state.chunks(SITE).enumerate().map(|(i, s)| (s[9] >> 24) | fv(i) << 8).collect();
        let q = &self.gpu.queue;
        q.write_buffer(&self.sites, 0, bytemuck::cast_slice(state));
        q.write_buffer(&self.pul, 0, bytemuck::cast_slice(&pul));
        q.write_buffer(&self.pul, pul.len() as u64 * 4, bytemuck::cast_slice(&pul));
        q.write_buffer(&self.counts, 0, bytemuck::cast_slice(&[0u32; 16]));
        q.write_buffer(&self.marks, 0, bytemuck::cast_slice(&vec![0u32; self.blocks() as usize]));
        self.busy_hint = state.chunks(SITE).enumerate().filter(|(i, s)| *s != EMPTY || fv(*i) != 0).count() as u32;
        self.seeded = false;
    }

    pub fn download(&self) -> Vec<u32> { self.gpu.read(&self.sites, 0, &self.staging, self.sites() * SITE) }

    /// The demand field after the last `run`, a value a site.
    pub fn download_field(&self) -> Vec<u8> {
        let n = self.sites();
        self.gpu.read(&self.pul, self.last_par * n, &self.staging, n).iter().map(|w| (w >> 8) as u8).collect()
    }

    /// The sites holding anything or a field after the last `run` that gathered: their indices,
    /// their `SITE` words each, and their fields.
    pub fn gathered(&mut self) -> (Vec<u32>, Vec<u32>, Vec<u8>) {
        let n = self.gpu.read(&self.counts, 3, &self.staging, 1)[0] as usize;
        let v = self.gpu.read(&self.gathered, 0, &self.staging, n * (SITE + 2));
        self.busy_hint = n as u32;
        let idx = v.chunks(SITE + 2).map(|r| r[0]).collect();
        let words = v.chunks(SITE + 2).flat_map(|r| r[1..=SITE].iter().copied()).collect();
        let field = v.chunks(SITE + 2).map(|r| r[SITE + 1] as u8).collect();
        (idx, words, field)
    }

    /// What the turns did since the last take (busy.wgsl `T_*`), and the sites of the first
    /// `FIRES_MAX` rewrites in no particular order; then a fresh tally.
    pub fn take_tally(&self) -> ([u32; TALLY], Vec<u32>) {
        let v = self.gpu.read(&self.counts, 4, &self.staging, COUNTS - 4);
        let fires = v[TALLY + 1..TALLY + 1 + (v[TALLY] as usize).min(FIRES_MAX)].to_vec();
        self.gpu.queue.write_buffer(&self.counts, 16, bytemuck::cast_slice(&[0u32; 12]));
        (v[..TALLY].try_into().unwrap(), fires)
    }

    /// One `Clock` (block.wgsl), padded to the uniform stride: [tick, the next clock's tick, the
    /// stamp of the list it fills, that list (`NO_LIST`: none), the list it empties, its pulse
    /// buffer, whether its pulse phase is fused, the list it runs], over n invocations.
    fn clock(&self, n: u32, f: [u32; 8]) -> [u32; STRIDE as usize / 4] {
        let mut c = [0; STRIDE as usize / 4];
        let [tick, tick_next, stamp, next, clear, par, fused, list] = f;
        c[..17].copy_from_slice(&[self.w, self.h, self.d, tick, self.w / 2 + 1, self.h / 2 + 1, self.d / 2 + 1, n,
            tick_next, stamp, next, clear, 0, 0, par, fused, list]);
        c
    }

    /// Clocks `first .. first + n` (n up to the batch) in one submission. `dense` runs every block,
    /// then every site's pulse phase. Otherwise each clock is one dispatch over its busy blocks,
    /// each but the batch's first starting with the previous clock's pulse phase; the batch closes
    /// with the last clock's pulse phase and, with `gather`, the sites holding anything are
    /// gathered (`gathered`). The tick is lattice.rs `tick`.
    pub fn run(&mut self, first: u64, seed: u64, n: usize, dense: bool, gather: bool) {
        assert!(n <= self.batch && n > 0, "{n} clocks in a batch of {}", self.batch);
        let (blocks, sites) = (self.blocks(), self.sites() as u32);
        let tick = |t: u64| t as u32 ^ (seed as u32).wrapping_mul(0x9E37_79B9);
        let (par, list) = (|t: u64| (t % 2) as u32, |t: u64| (t % 3) as u32);
        // Clock t runs list t % 3 and fills list (t + 1) % 3, stamping its blocks t + 2.
        let turns = |t: u64, next: u32, par: u32, fused: bool| [tick(t), tick(t + 1), (t + 2) as u32, next, list(t + 2), par, fused as u32, list(t)];
        let last = first + n as u64 - 1;
        self.last_par = (last % 2) as usize;
        let mut c = Vec::with_capacity((2 * n + 2) * STRIDE as usize / 4);
        for t in first..=last {
            if dense { c.extend(self.clock(blocks, turns(t, NO_LIST, par(t), false))); c.extend(self.clock(sites, turns(t, NO_LIST, par(t) ^ 1, false))); }
            else { c.extend(self.clock(0, turns(t, list(t + 1), par(t), t > first))); }
        }
        if !dense {
            // The close (the last clock's list and its pulse buffer), then the seed, which fills
            // the first clock's list as the clock before it would.
            c.extend(self.clock(0, turns(last, NO_LIST, par(last) ^ 1, false)));
            c.extend(self.clock(sites, [0, tick(first), first as u32 + 1, list(first), NO_LIST, 0, 0, NO_LIST]));
        }
        self.gpu.queue.write_buffer(&self.clocks, 0, bytemuck::cast_slice(&c));
        if gather { self.gpu.queue.write_buffer(&self.counts, 12, bytemuck::cast_slice(&[0u32])); }
        let g = self.gpu;
        let share = self.busy_hint.saturating_mul(3) / 2 + 64;
        let (wide, turn_groups) = (groups(share, 64).0, groups(share, WG).0);
        let mut enc = g.device.create_command_encoder(&Default::default());
        {
            let mut pass = enc.begin_compute_pass(&Default::default());
            if !dense && !self.seeded {
                pass.set_bind_group(0, &self.bind, &[(n as u32 + 1) * STRIDE as u32]);
                pass.set_pipeline(&g.seed_busy);
                let (x, y) = groups(sites, 64);
                pass.dispatch_workgroups(x, y, 1);
                self.seeded = true;
            }
            for i in 0..n as u32 {
                if dense {
                    for (k, (pipeline, m, size)) in [(&g.turns, blocks, WG), (&g.pulses, sites, 64)].into_iter().enumerate() {
                        pass.set_bind_group(0, &self.bind, &[(2 * i + k as u32) * STRIDE as u32]);
                        pass.set_pipeline(pipeline);
                        let (x, y) = groups(m, size);
                        pass.dispatch_workgroups(x, y, 1);
                    }
                    continue;
                }
                pass.set_bind_group(0, &self.bind, &[i * STRIDE as u32]);
                pass.set_pipeline(&g.clock_turns);
                pass.dispatch_workgroups(turn_groups, 1, 1);
            }
            if !dense {
                pass.set_bind_group(0, &self.bind, &[n as u32 * STRIDE as u32]);
                pass.set_pipeline(&g.busy_pulses);
                pass.dispatch_workgroups(wide, 1, 1);
                if gather {
                    pass.set_pipeline(&g.busy_gather);
                    pass.dispatch_workgroups(wide, 1, 1);
                }
            }
        }
        g.queue.submit([enc.finish()]);
    }
}
