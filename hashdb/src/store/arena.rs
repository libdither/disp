use std::{cell::RefCell, collections::{HashMap, hash_map::DefaultHasher}, hash::{Hash as StdHash, Hasher}};

use bumpalo::Bump;

use super::{HashType, TypeStore, UniqueHash};

pub fn get_hash<T: UniqueHash>(val: &T) -> u64 {
	let hasher = &mut DefaultHasher::new();
	val.unique_hash(hasher);
	hasher.finish()
}

pub struct LinkArena<'a> {
	arena: Bump,
	map: RefCell<HashMap<u64, *const ()>>, // Lookup map, takes hash and returns already allocated object
	p: std::marker::PhantomData<&'a ()>,
}
impl<'a> TypeStore<'a> for LinkArena<'a> {
    fn add<T: HashType<'a>>(&'a self, val: T) -> &'a T {
        self.alloc(val)
    }
	fn add_slice<'v, T: HashType<'a> + Clone>(&'a self, slice: &'v [T]) -> &'a [T] {
        self.arena.alloc_slice_clone(slice)
    }

    fn add_str<'v>(&'a self, string: &'v str) -> &'a str {
        self.arena.alloc_str(string)
    }

    
	
}

impl<'a> LinkArena<'a> {
	pub fn new() -> Self {
		Self {
			arena: Bump::new(),
			map: Default::default(),
			p: Default::default(),
		}
	}
	// Takes a reference to a T returns a T of lifetime 'a if a value was allready allocated
	fn check_for_dup<T: StdHash>(&self, hash: u64) -> Option<&'a T> {
		if let Some(&ptr) = self.map.borrow_mut().get(&hash) {
			// Safety: Only this type can create this hash, theoretically.
			// WARNING: this may cause UB on hash collision but I can't think of a better implementation at the moment
			unsafe { Some(&*ptr.cast::<T>()) }
		} else { None }
	}
	/// Return reference to existing allocation if exists, otherwise return new allocation
	pub fn alloc<T: HashType<'a>>(&'a self, val: T) -> &'a T {
		let hash = get_hash(&val);

		if let Some(val) = self.check_for_dup(hash) {
			val
		} else {
			let ret = self.arena.alloc(val) as &T;
			let ptr = (ret as *const T).cast();
			self.map.borrow_mut().insert(hash, ptr);
			
			ret
		}
	}
}