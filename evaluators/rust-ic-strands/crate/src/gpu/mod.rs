//! The chip's block schedule on a GPU (wgpu on Vulkan): the kernels of `block.wgsl` over the
//! lattice's sites in one storage buffer, a batch of clocks per submission, run only where
//! something is (`tiles.wgsl`).

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
/// Clocks between rebuilds of the active tiles: no more than a tile is wide, so nothing outruns
/// the ring of neighbours each rebuild adds.
const REBUILD: u64 = 8;
/// Words of tiles.wgsl `counts`: the list lengths, the tally, and the rewrites' sites.
const COUNTS: usize = 16 + FIRES_MAX;
/// Of the tally (tiles.wgsl `T_*`): what the turns did since it was last taken.
pub const TALLY: usize = 11;
/// Rewrites whose sites are kept per tally.
pub const FIRES_MAX: usize = 4096;

/// Workgroups of 64 covering n invocations (the kernels index by `g.x + g.y * num_workgroups.x * 64`).
fn groups(n: u32) -> (u32, u32) { let g = n.div_ceil(64); (g.min(65535), g.div_ceil(65535)) }

pub struct Gpu {
    pub device: wgpu::Device,
    pub queue: wgpu::Queue,
    pub name: String,
    layout: wgpu::BindGroupLayout,
    args_layout: wgpu::BindGroupLayout,
    turns: wgpu::ComputePipeline,
    pulses: wgpu::ComputePipeline,
    vectors: wgpu::ComputePipeline,
    tile_mark: wgpu::ComputePipeline,
    tile_clear: wgpu::ComputePipeline,
    tile_compact: wgpu::ComputePipeline,
    tile_args: wgpu::ComputePipeline,
    tile_busy: wgpu::ComputePipeline,
    busy_args: wgpu::ComputePipeline,
    busy_turns: wgpu::ComputePipeline,
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
        let module = device.create_shader_module(wgpu::ShaderModuleDescriptor { label: Some("strands"), source: wgpu::ShaderSource::Wgsl(crate::tables::shader(p).into()) });
        let buffer = |binding, ty, dynamic, min: Option<u64>| wgpu::BindGroupLayoutEntry { binding, visibility: ShaderStages::COMPUTE, count: None,
            ty: wgpu::BindingType::Buffer { ty, has_dynamic_offset: dynamic, min_binding_size: min.and_then(NonZeroU64::new) } };
        let storage = wgpu::BufferBindingType::Storage { read_only: false };
        let layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor { label: Some("strands"), entries: &[
            buffer(0, wgpu::BufferBindingType::Uniform, true, Some(CLOCK)), buffer(1, storage, false, None), buffer(2, storage, false, None),
            buffer(3, storage, false, None), buffer(4, storage, false, None), buffer(5, storage, false, None), buffer(6, storage, false, None),
            buffer(7, storage, false, None), buffer(8, storage, false, None)] });
        // The indirect dispatch's arguments are written only by the kernels that build the list,
        // which alone see them, so a dispatch never reads them as arguments while writing them.
        let args_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor { label: Some("args"), entries: &[buffer(0, storage, false, None)] });
        let run_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor { label: Some("run"), bind_group_layouts: &[Some(&layout)], immediate_size: 0 });
        let list_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor { label: Some("list"), bind_group_layouts: &[Some(&layout), Some(&args_layout)], immediate_size: 0 });
        let pipeline = |entry, pl: &wgpu::PipelineLayout| device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor { label: Some(entry), layout: Some(pl), module: &module,
            entry_point: Some(entry), cache: None,
            // Every kernel writes a slot of workgroup memory before reading it, so the 30 KB need no clearing.
            compilation_options: wgpu::PipelineCompilationOptions { zero_initialize_workgroup_memory: false, ..Default::default() } });
        let gpu = Gpu {
            turns: pipeline("turns", &run_layout), pulses: pipeline("pulses", &run_layout), vectors: pipeline("vectors", &run_layout),
            tile_mark: pipeline("tile_mark", &run_layout),
            tile_clear: pipeline("tile_clear", &list_layout), tile_compact: pipeline("tile_compact", &list_layout), tile_args: pipeline("tile_args", &list_layout),
            tile_busy: pipeline("tile_busy", &run_layout), busy_args: pipeline("busy_args", &list_layout),
            busy_turns: pipeline("busy_turns", &run_layout), busy_pulses: pipeline("busy_pulses", &run_layout),
            busy_gather: pipeline("busy_gather", &run_layout),
            name: format!("{} ({:?}, {})", info.name, info.backend, info.driver_info), device, queue, layout, args_layout,
        };
        if let Some(e) = pollster::block_on(scope.pop()) { return Err(format!("shader: {e}")); }
        Ok(gpu)
    }

    fn buffer(&self, bytes: u64, usage: U) -> wgpu::Buffer {
        self.device.create_buffer(&wgpu::BufferDescriptor { label: None, size: bytes.max(4), usage, mapped_at_creation: false })
    }

    fn bind(&self, clocks: &wgpu::Buffer, storage: [&wgpu::Buffer; 8]) -> wgpu::BindGroup {
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
        let (sites, pul, marks, list, busy, live) = (small(), small(), small(), small(), small(), small());
        let counts = self.buffer(COUNTS as u64 * 4, U::STORAGE);
        let buf = self.buffer((n * REC * 4) as u64, U::STORAGE | U::COPY_DST | U::COPY_SRC);
        let staging = self.buffer((n * REC * 4) as u64, U::MAP_READ | U::COPY_DST);
        let bind = self.bind(&clocks, [&sites, &pul, &buf, &marks, &list, &counts, &busy, &live]);
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
                let (x, y) = groups(m);
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
    /// Blocks per tile and tiles per axis.
    tb: [u32; 3],
    nt: [u32; 3],
    /// Clocks run since the active tiles were last rebuilt from scratch at upload.
    since: u64,
    sites: wgpu::Buffer,
    staging: wgpu::Buffer,
    pul: wgpu::Buffer,
    list: wgpu::Buffer,
    counts: wgpu::Buffer,
    live: wgpu::Buffer,
    args: wgpu::Buffer,
    gathered: wgpu::Buffer,
    clocks: wgpu::Buffer,
    bind: wgpu::BindGroup,
    args_bind: wgpu::BindGroup,
}

impl<'g> Grid<'g> {
    /// A w×h×d lattice running up to `batch` clocks per submission.
    pub fn new(gpu: &'g Gpu, w: u32, h: u32, d: u32, batch: usize) -> Grid<'g> {
        let n = (w * h * d) as u64;
        let tb = if d == 1 { [8, 8, 1] } else { [4, 4, 4] };
        let nb = [w / 2 + 1, h / 2 + 1, d / 2 + 1];
        let nt = [0, 1, 2].map(|i| nb[i].div_ceil(tb[i]));
        let tiles = (nt[0] * nt[1] * nt[2]) as u64;
        let sites = gpu.buffer(n * SITE as u64 * 4, U::STORAGE | U::COPY_DST | U::COPY_SRC);
        let gathered = gpu.buffer(n * (SITE as u64 + 1) * 4, U::STORAGE | U::COPY_SRC);
        let staging = gpu.buffer((n * (SITE as u64 + 1)).max(COUNTS as u64) * 4, U::MAP_READ | U::COPY_DST);
        let live = gpu.buffer(n * 4, U::STORAGE | U::COPY_DST);
        let pul = gpu.buffer(2 * n * 4, U::STORAGE | U::COPY_DST);
        let marks = gpu.buffer(tiles * 4, U::STORAGE);
        let list = gpu.buffer(tiles * 4, U::STORAGE | U::COPY_DST | U::COPY_SRC);
        let counts = gpu.buffer(COUNTS as u64 * 4, U::STORAGE | U::COPY_DST | U::COPY_SRC);
        let busy = gpu.buffer((nb[0] * nb[1] * nb[2]) as u64 * 4, U::STORAGE);
        let args = gpu.buffer(6 * 4, U::STORAGE | U::INDIRECT | U::COPY_DST | U::COPY_SRC);
        let clocks = gpu.buffer((2 * batch.max(1) as u64 + 1) * STRIDE, U::UNIFORM | U::COPY_DST);
        let bind = gpu.bind(&clocks, [&sites, &pul, &gathered, &marks, &list, &counts, &busy, &live]);
        let args_bind = gpu.device.create_bind_group(&wgpu::BindGroupDescriptor { label: None, layout: &gpu.args_layout,
            entries: &[wgpu::BindGroupEntry { binding: 0, resource: args.as_entire_binding() }] });
        Grid { gpu, w, h, d, batch: batch.max(1), tb, nt, since: 0, sites, staging, pul, list, counts, live, args, gathered, clocks, bind, args_bind }
    }

    fn sites(&self) -> usize { (self.w * self.h * self.d) as usize }
    fn tiles(&self) -> u32 { self.nt[0] * self.nt[1] * self.nt[2] }

    /// Load a state of `SITE` words per site; each site's pulse also goes to both pulse buffers.
    /// Every tile is active until the first clock's rebuild.
    pub fn upload(&mut self, state: &[u32]) {
        assert_eq!(state.len(), self.sites() * SITE, "a state of {SITE} words per site");
        const EMPTY: [u32; SITE] = [!0, !0, !0, !0, !0, !0, !0, !0, 0xFF, 0xFF00_0000];
        let pul: Vec<u32> = state.chunks(SITE).map(|s| s[9] >> 24).collect();
        let live: Vec<u32> = state.chunks(SITE).map(|s| (s != EMPTY) as u32).collect();
        let all: Vec<u32> = (0..self.tiles()).collect();
        let q = &self.gpu.queue;
        q.write_buffer(&self.sites, 0, bytemuck::cast_slice(state));
        q.write_buffer(&self.pul, 0, bytemuck::cast_slice(&pul));
        q.write_buffer(&self.pul, pul.len() as u64 * 4, bytemuck::cast_slice(&pul));
        q.write_buffer(&self.counts, 0, bytemuck::cast_slice(&[0u32; 16]));
        q.write_buffer(&self.live, 0, bytemuck::cast_slice(&live));
        q.write_buffer(&self.list, 0, bytemuck::cast_slice(&all));
        q.write_buffer(&self.args, 0, bytemuck::cast_slice(&[self.tiles(), 1, 1, 0, 1, 1]));
        self.since = 0;
    }

    pub fn download(&self) -> Vec<u32> { self.gpu.read(&self.sites, 0, &self.staging, self.sites() * SITE) }

    /// The sites holding anything after the last `run` that gathered: their indices and their
    /// `SITE` words each.
    pub fn gathered(&self) -> (Vec<u32>, Vec<u32>) {
        let n = self.gpu.read(&self.counts, 3, &self.staging, 1)[0] as usize;
        let v = self.gpu.read(&self.gathered, 0, &self.staging, n * (SITE + 1));
        let idx = v.chunks(SITE + 1).map(|r| r[0]).collect();
        let words = v.chunks(SITE + 1).flat_map(|r| r[1..].iter().copied()).collect();
        (idx, words)
    }

    /// What the turns did since the last take (tiles.wgsl `T_*`), and the sites of the first
    /// `FIRES_MAX` rewrites in no particular order; then a fresh tally.
    pub fn take_tally(&self) -> ([u32; TALLY], Vec<u32>) {
        let v = self.gpu.read(&self.counts, 4, &self.staging, COUNTS - 4);
        let fires = v[TALLY + 1..TALLY + 1 + (v[TALLY] as usize).min(FIRES_MAX)].to_vec();
        self.gpu.queue.write_buffer(&self.counts, 16, bytemuck::cast_slice(&[0u32; 12]));
        (v[..TALLY].try_into().unwrap(), fires)
    }

    /// One clock's `Clock` (block.wgsl), padded to the uniform stride.
    fn clock(&self, tick: u32, n: u32, par: u32, fused: bool, list: u32) -> [u32; STRIDE as usize / 4] {
        let mut c = [0; STRIDE as usize / 4];
        c[..17].copy_from_slice(&[self.w, self.h, self.d, tick, self.w / 2 + 1, self.h / 2 + 1, self.d / 2 + 1, n,
            self.tb[0], self.tb[1], self.tb[2], self.nt[0], self.nt[1], self.nt[2], par, fused as u32, list]);
        c
    }

    /// Clocks `first .. first + n` (n up to the batch) in one submission. `dense` runs every block,
    /// then every site's pulse phase. Otherwise per clock: the active tiles rebuilt every few clocks,
    /// the busy blocks listed, and their turns, each but the batch's first starting with the
    /// previous clock's pulse phase; the batch closes with the last clock's pulse phase and, with
    /// `gather`, the sites holding anything are gathered (`gathered`). The tick is lattice.rs `tick`.
    pub fn run(&mut self, first: u64, seed: u64, n: usize, dense: bool, gather: bool) {
        assert!(n <= self.batch && n > 0, "{n} clocks in a batch of {}", self.batch);
        let blocks = (self.w / 2 + 1) * (self.h / 2 + 1) * (self.d / 2 + 1);
        let (sites, tiles) = (self.sites() as u32, self.tiles());
        let tick = |t: u64| t as u32 ^ (seed as u32).wrapping_mul(0x9E37_79B9);
        let par = |t: u64| (t % 2) as u32;
        let last = first + n as u64 - 1;
        let mut c = Vec::with_capacity((2 * n + 1) * STRIDE as usize / 4);
        for t in first..=last {
            if dense { c.extend(self.clock(tick(t), blocks, par(t), false, 0)); c.extend(self.clock(tick(t), sites, par(t) ^ 1, false, 0)); }
            else { c.extend(self.clock(tick(t), tiles, par(t), t > first, par(t))); }
        }
        if !dense { c.extend(self.clock(tick(last), tiles, par(last) ^ 1, false, par(last))); }
        self.gpu.queue.write_buffer(&self.clocks, 0, bytemuck::cast_slice(&c));
        if gather { self.gpu.queue.write_buffer(&self.counts, 12, bytemuck::cast_slice(&[0u32])); }
        let g = self.gpu;
        let mut enc = g.device.create_command_encoder(&Default::default());
        {
            let mut pass = enc.begin_compute_pass(&Default::default());
            pass.set_bind_group(1, &self.args_bind, &[]);
            for i in 0..n as u32 {
                if dense {
                    for (k, (pipeline, m)) in [(&g.turns, blocks), (&g.pulses, sites)].into_iter().enumerate() {
                        pass.set_bind_group(0, &self.bind, &[(2 * i + k as u32) * STRIDE as u32]);
                        pass.set_pipeline(pipeline);
                        let (x, y) = groups(m);
                        pass.dispatch_workgroups(x, y, 1);
                    }
                    continue;
                }
                pass.set_bind_group(0, &self.bind, &[i * STRIDE as u32]);
                if self.since % REBUILD == 0 {
                    let (x, y) = groups(tiles);
                    pass.set_pipeline(&g.tile_clear);
                    pass.dispatch_workgroups(x, y, 1);
                    pass.set_pipeline(&g.tile_mark);
                    pass.dispatch_workgroups_indirect(&self.args, 0);
                    pass.set_pipeline(&g.tile_compact);
                    pass.dispatch_workgroups(x, y, 1);
                    pass.set_pipeline(&g.tile_args);
                    pass.dispatch_workgroups(1, 1, 1);
                }
                self.since += 1;
                pass.set_pipeline(&g.tile_busy);
                pass.dispatch_workgroups_indirect(&self.args, 0);
                pass.set_pipeline(&g.busy_args);
                pass.dispatch_workgroups(1, 1, 1);
                pass.set_pipeline(&g.busy_turns);
                pass.dispatch_workgroups_indirect(&self.args, 12);
            }
            if !dense {
                pass.set_bind_group(0, &self.bind, &[n as u32 * STRIDE as u32]);
                pass.set_pipeline(&g.busy_pulses);
                pass.dispatch_workgroups_indirect(&self.args, 12);
                if gather {
                    pass.set_pipeline(&g.busy_gather);
                    pass.dispatch_workgroups_indirect(&self.args, 12);
                }
            }
        }
        g.queue.submit([enc.finish()]);
    }
}
