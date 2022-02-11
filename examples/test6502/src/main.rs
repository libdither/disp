use disp::*;
use hex_literal::hex;

fn main() {
	let mut db = Datastore::default();
	let data = Data::new(hex!("12 32").to_vec());
	let hash = db.store(data);

	println!("Hash: {}, Data: {:?}", hash, db.get(&hash).unwrap().as_bytes());
}
