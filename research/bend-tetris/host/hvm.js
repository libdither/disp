// Runs a Bend program that build.sh compiled to WebAssembly (host.c is the other half).
// Provides just enough WASI for HVM's C runtime: a clock, and writes to stdout and stderr.

export async function start(wasm, write) {
  let memory;
  const view = () => new DataView(memory.buffer);
  const wasi = {
    // Wall-clock nanoseconds, with random low digits: the game seeds its piece order from them.
    clock_time_get(id, precision, out) {
      view().setBigUint64(out, BigInt(Date.now()) * 1000000n + BigInt(Math.floor(Math.random() * 1e6)), true);
      return 0;
    },
    fd_write(fd, iovs, count, written) {
      let total = 0;
      for (let i = 0; i < count; i++) {
        const ptr = view().getUint32(iovs + 8 * i, true);
        const len = view().getUint32(iovs + 8 * i + 4, true);
        write(fd, new Uint8Array(memory.buffer, ptr, len).slice());
        total += len;
      }
      view().setUint32(written, total, true);
      return 0;
    },
    // Every descriptor is a character device, like a terminal.
    fd_fdstat_get(fd, out) {
      new Uint8Array(memory.buffer, out, 24).fill(0);
      view().setUint8(out, 2);
      return 0;
    },
    fd_prestat_get: () => 8, // EBADF: no directories are open
    proc_exit(code) {
      throw new Error(`the program exited with code ${code}`);
    },
  };
  const unsupported = () => 52; // ENOSYS
  const imports = { wasi_snapshot_preview1: new Proxy(wasi, { get: (fns, name) => fns[name] ?? unsupported }) };
  const { instance } = await WebAssembly.instantiate(wasm, imports);
  const { _initialize, boot, push_byte, resume } = instance.exports;
  memory = instance.exports.memory;
  _initialize();
  boot();
  return {
    // Queues input and runs the program until it wants more; false once it has finished.
    send(bytes) {
      for (const byte of bytes) push_byte(byte);
      return resume() === 1;
    },
  };
}
