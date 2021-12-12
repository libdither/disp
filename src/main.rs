use std::io::Read;

use hashdb::*;



fn main() {
    let mut db = Datastore::new();

    let data = Data::new("Hello, World!\n".as_bytes());

    /* let mut buf = Vec::new();
    std::fs::File::open("/home/zyansheep/Documents/Personal/z_key.pub").unwrap().read_to_end(&mut buf).unwrap();
    let data = Data::new(&buf[..]); */

    println!("Have data: {:x?}", data.as_bytes());
    let hash = db.add(data);
    let data = db.get(&hash).unwrap().clone();
    let hash2 = db.add(data);
    let data = db.get(&hash2).unwrap();
    assert_eq!(hash, hash2);

    //println!("Data: {}", std::str::from_utf8(data.as_bytes()).unwrap());
    println!("Hash: {}", hash);
}
