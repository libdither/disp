//! The chip's block schedule on a GPU (wgpu on Vulkan): the kernels of `block.wgsl` over the
//! lattice's sites in one storage buffer, a batch of clocks per submission, run only where
//! something is (`tiles.wgsl`).

use std::num::NonZeroU64;
use wgpu::{BufferUsages as U, ShaderStages};

/// Words per site (bytes as lattice.rs `dump_state`).
pub const SITE: usize = 10;
/// Words per recorded turn or block (block.wgsl `REC`).
pub const REC: usize = 93;
/// Bytes between two `Clock` entries of the uniform buffer.
const STRIDE: u64 = 256;
/// Bytes of a `Clock`.
const CLOCK: u64 = 64;
/// Records replayed per dispatch.
const RECS_PER_DISPATCH: usize = 1 << 17;
/// Clocks between rebuilds of the active tiles: no more than a tile is wide, so nothing outruns
/// the ring of neighbours each rebuild adds.
const REBUILD: u64 = 8;

/// The whole shader: the generated tables, then the stages in order.
pub fn shader() -> String {
    [crate::tables::wgsl().as_str(), include_str!("prelude.wgsl"), include_str!("collect.wgsl"), include_str!("fire.wgsl"),
     include_str!("moves.wgsl"), include_str!("block.wgsl"), include_str!("tiles.wgsl")].join("\n")
}

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
    tile_turns: wgpu::ComputePipeline,
    tile_pulses: wgpu::ComputePipeline,
    tile_mark: wgpu::ComputePipeline,
    tile_gather: wgpu::ComputePipeline,
    tile_clear: wgpu::ComputePipeline,
    tile_compact: wgpu::ComputePipeline,
    tile_args: wgpu::ComputePipeline,
}

impl Gpu {
    pub fn new() -> Result<Gpu, String> {
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor { backends: wgpu::Backends::VULKAN, ..wgpu::InstanceDescriptor::new_without_display_handle() });
        let adapter = pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance, ..Default::default() })).map_err(|e| format!("no Vulkan adapter: {e}"))?;
        let info = adapter.get_info();
        let (device, queue) = pollster::block_on(adapter.request_device(&wgpu::DeviceDescriptor {
            label: Some("strands"), required_limits: adapter.limits(), ..Default::default() })).map_err(|e| format!("no device: {e}"))?;
        let scope = device.push_error_scope(wgpu::ErrorFilter::Validation);
        let module = device.create_shader_module(wgpu::ShaderModuleDescriptor { label: Some("strands"), source: wgpu::ShaderSource::Wgsl(shader().into()) });
        let buffer = |binding, ty, dynamic, min: Option<u64>| wgpu::BindGroupLayoutEntry { binding, visibility: ShaderStages::COMPUTE, count: None,
            ty: wgpu::BindingType::Buffer { ty, has_dynamic_offset: dynamic, min_binding_size: min.and_then(NonZeroU64::new) } };
        let storage = wgpu::BufferBindingType::Storage { read_only: false };
        let layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor { label: Some("strands"), entries: &[
            buffer(0, wgpu::BufferBindingType::Uniform, true, Some(CLOCK)), buffer(1, storage, false, None), buffer(2, storage, false, None),
            buffer(3, storage, false, None), buffer(4, storage, false, None), buffer(5, storage, false, None)] });
        // The indirect dispatch's arguments are written only by the kernels that build the list,
        // which alone see them, so a dispatch never reads them as arguments while writing them.
        let args_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor { label: Some("args"), entries: &[buffer(0, storage, false, None)] });
        let run_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor { label: Some("run"), bind_group_layouts: &[Some(&layout)], immediate_size: 0 });
        let list_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor { label: Some("list"), bind_group_layouts: &[Some(&layout), Some(&args_layout)], immediate_size: 0 });
        let pipeline = |entry, pl: &wgpu::PipelineLayout| device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor { label: Some(entry), layout: Some(pl), module: &module,
            entry_point: Some(entry), compilation_options: Default::default(), cache: None });
        let gpu = Gpu {
            turns: pipeline("turns", &run_layout), pulses: pipeline("pulses", &run_layout), vectors: pipeline("vectors", &run_layout),
            tile_turns: pipeline("tile_turns", &run_layout), tile_pulses: pipeline("tile_pulses", &run_layout),
            tile_mark: pipeline("tile_mark", &run_layout), tile_gather: pipeline("tile_gather", &run_layout),
            tile_clear: pipeline("tile_clear", &list_layout), tile_compact: pipeline("tile_compact", &list_layout), tile_args: pipeline("tile_args", &list_layout),
            name: format!("{} ({:?}, {})", info.name, info.backend, info.driver_info), device, queue, layout, args_layout,
        };
        if let Some(e) = pollster::block_on(scope.pop()) { return Err(format!("shader: {e}")); }
        Ok(gpu)
    }

    fn buffer(&self, bytes: u64, usage: U) -> wgpu::Buffer {
        self.device.create_buffer(&wgpu::BufferDescriptor { label: None, size: bytes.max(4), usage, mapped_at_creation: false })
    }

    fn bind(&self, clocks: &wgpu::Buffer, storage: [&wgpu::Buffer; 5]) -> wgpu::BindGroup {
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
        let (sites, pul, marks, list) = (small(), small(), small(), small());
        let buf = self.buffer((n * REC * 4) as u64, U::STORAGE | U::COPY_DST | U::COPY_SRC);
        let staging = self.buffer((n * REC * 4) as u64, U::MAP_READ | U::COPY_DST);
        let bind = self.bind(&clocks, [&sites, &pul, &buf, &marks, &list]);
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
        let tile_sites = 8 * (tb[0] * tb[1] * tb[2]) as u64;
        let sites = gpu.buffer(n * SITE as u64 * 4, U::STORAGE | U::COPY_DST | U::COPY_SRC);
        let gathered = gpu.buffer(tiles * tile_sites * SITE as u64 * 4, U::STORAGE | U::COPY_SRC);
        let staging = gpu.buffer((n.max(tiles * tile_sites) * SITE as u64 + tiles + 4) * 4, U::MAP_READ | U::COPY_DST);
        let pul = gpu.buffer(n * 4, U::STORAGE | U::COPY_DST);
        let marks = gpu.buffer(tiles * 4, U::STORAGE);
        let list = gpu.buffer(tiles * 4, U::STORAGE | U::COPY_DST | U::COPY_SRC);
        let args = gpu.buffer(16, U::STORAGE | U::INDIRECT | U::COPY_DST | U::COPY_SRC);
        let clocks = gpu.buffer(2 * batch.max(1) as u64 * STRIDE, U::UNIFORM | U::COPY_DST);
        let bind = gpu.bind(&clocks, [&sites, &pul, &gathered, &marks, &list]);
        let args_bind = gpu.device.create_bind_group(&wgpu::BindGroupDescriptor { label: None, layout: &gpu.args_layout,
            entries: &[wgpu::BindGroupEntry { binding: 0, resource: args.as_entire_binding() }] });
        Grid { gpu, w, h, d, batch: batch.max(1), tb, nt, since: 0, sites, staging, pul, list, args, gathered, clocks, bind, args_bind }
    }

    fn sites(&self) -> usize { (self.w * self.h * self.d) as usize }
    fn tiles(&self) -> u32 { self.nt[0] * self.nt[1] * self.nt[2] }
    /// Sites per tile (some may lie off the lattice).
    pub fn tile_sites(&self) -> usize { 8 * (self.tb[0] * self.tb[1] * self.tb[2]) as usize }

    /// Load a state of `SITE` words per site; each site's pulse also goes to `pul`. Every tile is
    /// active until the first clock's rebuild.
    pub fn upload(&mut self, state: &[u32]) {
        assert_eq!(state.len(), self.sites() * SITE, "a state of {SITE} words per site");
        let pul: Vec<u32> = state.chunks(SITE).map(|s| s[9] >> 24).collect();
        let all: Vec<u32> = (0..self.tiles()).collect();
        let q = &self.gpu.queue;
        q.write_buffer(&self.sites, 0, bytemuck::cast_slice(state));
        q.write_buffer(&self.pul, 0, bytemuck::cast_slice(&pul));
        q.write_buffer(&self.list, 0, bytemuck::cast_slice(&all));
        q.write_buffer(&self.args, 0, bytemuck::cast_slice(&[self.tiles(), 1, 1, 0]));
        self.since = 0;
    }

    pub fn download(&self) -> Vec<u32> { self.gpu.read(&self.sites, 0, &self.staging, self.sites() * SITE) }

    /// The active tiles and their sites (`tile_sites` each, in tile order; off the lattice reads
    /// as empty), and where each tile's site i lies (none off the lattice).
    pub fn gather(&self) -> (Vec<u32>, Vec<u32>) {
        let mut enc = self.gpu.device.create_command_encoder(&Default::default());
        {
            let mut pass = enc.begin_compute_pass(&Default::default());
            pass.set_pipeline(&self.gpu.tile_gather);
            pass.set_bind_group(0, &self.bind, &[0]);
            pass.dispatch_workgroups_indirect(&self.args, 0);
        }
        self.gpu.queue.submit([enc.finish()]);
        let n = self.gpu.read(&self.args, 0, &self.staging, 4)[0] as usize;
        let list = self.gpu.read(&self.list, 0, &self.staging, n);
        let words = self.gpu.read(&self.gathered, 0, &self.staging, n * self.tile_sites() * SITE);
        (list, words)
    }

    /// The lattice site of tile t's site i, if on the lattice (tiles.wgsl `tile_site`).
    pub fn tile_site(&self, t: u32, i: usize) -> Option<u32> {
        let (e, i) = (self.tb.map(|b| 2 * b), i as u32);
        let t = [t % self.nt[0], t / self.nt[0] % self.nt[1], t / (self.nt[0] * self.nt[1])];
        let (x, y, z) = (t[0] * e[0] + i % e[0], t[1] * e[1] + i / e[0] % e[1], t[2] * e[2] + i / (e[0] * e[1]));
        (x < self.w && y < self.h && z < self.d).then(|| x + self.w * (y + self.h * z))
    }

    /// One clock's `Clock` (block.wgsl), padded to the uniform stride.
    fn clock(&self, tick: u32, n: u32) -> [u32; STRIDE as usize / 4] {
        let mut c = [0; STRIDE as usize / 4];
        c[..14].copy_from_slice(&[self.w, self.h, self.d, tick, self.w / 2 + 1, self.h / 2 + 1, self.d / 2 + 1, n,
            self.tb[0], self.tb[1], self.tb[2], self.nt[0], self.nt[1], self.nt[2]]);
        c
    }

    /// Clocks `first .. first + n` (n up to the batch) in one submission. `dense` runs every block
    /// and every site; otherwise only the active tiles, rebuilt every few clocks. The tick is
    /// lattice.rs `tick`.
    pub fn run(&mut self, first: u64, seed: u64, n: usize, dense: bool) {
        assert!(n <= self.batch, "{n} clocks in a batch of {}", self.batch);
        let blocks = (self.w / 2 + 1) * (self.h / 2 + 1) * (self.d / 2 + 1);
        let (sites, tiles) = (self.sites() as u32, self.tiles());
        // Per clock: for the tiles one entry, for the whole lattice one per kernel (its count).
        let mut c = Vec::with_capacity(2 * n * STRIDE as usize / 4);
        for i in 0..n as u64 {
            let tick = (first + i) as u32 ^ (seed as u32).wrapping_mul(0x9E37_79B9);
            if dense { c.extend(self.clock(tick, blocks)); c.extend(self.clock(tick, sites)); } else { c.extend(self.clock(tick, tiles)); }
        }
        self.gpu.queue.write_buffer(&self.clocks, 0, bytemuck::cast_slice(&c));
        let g = self.gpu;
        let mut enc = g.device.create_command_encoder(&Default::default());
        {
            let mut pass = enc.begin_compute_pass(&Default::default());
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
                    pass.set_bind_group(1, &self.args_bind, &[]);
                    pass.set_pipeline(&g.tile_clear);
                    let (x, y) = groups(tiles);
                    pass.dispatch_workgroups(x, y, 1);
                    pass.set_pipeline(&g.tile_mark);
                    pass.dispatch_workgroups_indirect(&self.args, 0);
                    pass.set_pipeline(&g.tile_compact);
                    pass.dispatch_workgroups(x, y, 1);
                    pass.set_pipeline(&g.tile_args);
                    pass.dispatch_workgroups(1, 1, 1);
                }
                self.since += 1;
                for pipeline in [&g.tile_turns, &g.tile_pulses] {
                    pass.set_pipeline(pipeline);
                    pass.dispatch_workgroups_indirect(&self.args, 0);
                }
            }
        }
        g.queue.submit([enc.finish()]);
    }
}
