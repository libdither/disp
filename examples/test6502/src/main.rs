use disp::*;
use hex_literal::hex;

fn main() {
    let mut db = Database::default();
    let data = Data::new(&hex!("12 32"));
    let hash = db.add(data);

    println!("Hash: {}, Data: {:?}", hash, db.get(&hash).unwrap().as_bytes());
}
