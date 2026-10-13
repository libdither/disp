//! A two-way program (src/iso.rs) on the machine: forward to the end, then turned around back to the
//! beginning, with how long it took and how much it held.
//!   strands-iso 'CALL'      (as the player's two-way input box takes it: `add 2 3`, `tc #… 1 1`, …)
use rust_ic_strands::iso::{library, parse_call, show, Machine};

fn main() {
    let src = std::env::args().nth(1).expect("a call");
    let defs = library();
    let (main, input) = parse_call(&defs, &src).unwrap_or_else(|e| panic!("{e}"));
    let mut m = Machine::new(defs, &main, &input);
    m.share = !std::env::args().any(|a| a == "--copy");
    let (mut peak_prog, mut peak_data, mut peak_depth) = (m.live(), m.nodes(), 0);
    let t0 = std::time::Instant::now();
    let mut n = 0u64;
    while m.step().unwrap_or_else(|e| panic!("step {n}: {e}")) {
        n += 1;
        if n % 64 == 0 { peak_prog = peak_prog.max(m.live()); peak_data = peak_data.max(m.nodes()); }
        peak_depth = peak_depth.max(m.depth);
    }
    let out = m.payload();
    let shown = show(&out);
    println!("forward: {n} steps ({:.2}s); at most {peak_prog} instructions laid out, {peak_data} tree nodes held, {peak_depth} calls deep",
        t0.elapsed().as_secs_f64());
    println!("answer: {}", if shown.len() > 300 { format!("{}… ({} nodes)", &shown[..300], m.nodes()) } else { shown });
    m.flip();
    let back = m.run(u64::MAX).unwrap_or_else(|e| panic!("back: {e}"));
    println!("back: {back} steps; {}", if m.payload() == input && m.clock == 0 && m.depth == 0 { "the input, exactly" } else { "NOT the input" });
}
