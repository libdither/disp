mod hash;
mod data;

use hash::Hash;
use data::Data;

fn main() {
    let mut db = data::Database::default();

    let data = Data::new("Hello, World!".as_bytes());
    println!("Have data: {:x?}", data.as_bytes());
    let hash = db.add(data);
    let data = db.get(&hash).unwrap().clone();
    let hash2 = db.add(data);
    let data = db.get(&hash2).unwrap();
    assert_eq!(hash, hash2);

    println!("Data: {}", std::str::from_utf8(data.into()).unwrap());
    println!("Hash: {}", hash);
}
