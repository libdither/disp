//! Print the rule ROM as JSON (for the JS oracle / visualizer side).
//! Usage: cargo run --bin export-rules > ../rules.json
fn main() {
    rust_ca_lattice::rules::validate().expect("ROM must validate before export");
    print!("{}", rust_ca_lattice::rules::to_json());
}
