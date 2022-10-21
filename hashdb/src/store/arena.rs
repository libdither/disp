use std::{any::TypeId, cell::RefCell, collections::{HashMap, hash_map::DefaultHasher}, hash::{Hash as StdHash, Hasher}, iter, marker::PhantomData, rc::Rc};

use bumpalo::Bump;

use super::{TypeStorable, TypeStore};

pub struct LinkArena<'a> {
	arena: Bump,
	map: RefCell<HashMap<u64, *const ()>>, // Lookup map, takes hash and returns already allocated object
	reverse_links_map: RefCell<HashMap<(u64, TypeId), Rc<RefCell<Vec<*const ()>>>>>, // Reverse lookup map, str represents object type
	p: std::marker::PhantomData<&'a ()>,
}
impl<'a> TypeStore<'a> for LinkArena<'a> {
    fn add<T: TypeStorable + 'static>(&'a self, val: T) -> &'a T {
        self.alloc(val)
    }
}

/// Whether an object should add back-links from certain hashes to itself.
pub trait ReverseLinked {
	fn reverse_links(&self) -> impl Iterator<Item = u64>;
}
default impl<T: StdHash> ReverseLinked for T {
    fn reverse_links(&self) -> impl Iterator<Item = u64> {
		iter::empty::<u64>()
    }
} 

impl<'a> LinkArena<'a> {
	pub fn new() -> Self {
		Self {
			arena: Bump::new(),
			map: Default::default(),
			reverse_links_map: Default::default(),
			p: Default::default(),
		}
	}
	fn get_hash<T: StdHash>(val: &T) -> u64 {
		let hasher = &mut DefaultHasher::new();
		val.hash(hasher);
		hasher.finish()
	}
	// Takes a reference to a T returns a T of lifetime 'a if a value was allready allocated
	fn check_for_dup<T: StdHash>(&self, hash: u64) -> Option<&'a T> {
		let ptr = self.map.borrow_mut().get(&hash).cloned();
		// Safety: Only this type can create this hash, theoretically.
		// WARNING: this may cause UB on hash collision but I can't think of a better implementation at the moment
		unsafe {
			ptr.map(|ptr| &*ptr.cast::<T>())
		}
	}
	/// Return reference to existing allocation if exists, otherwise return new allocation
	pub fn alloc<T: TypeStorable + ReverseLinked + 'static>(&'a self, val: T) -> &'a T {
		let hash = Self::get_hash(&val);

		if let Some(val) = self.check_for_dup(hash) {
			val
		} else {
			let ret = self.arena.alloc(val) as &T;
			let ptr = (ret as *const T) as *const ();
			self.map.borrow_mut().insert(hash, ptr);

			// Store Reverse Links
			let mut reverse_links_map = self.reverse_links_map.borrow_mut();
			for multihash in T::reverse_links(ret) {
				let entry = reverse_links_map.entry((multihash, TypeId::of::<T>())).or_default();
				// Note: See safety note in find_reverse_links, ptr points to a T
				entry.borrow_mut().push(ptr);
			}
			ret
		}
	}
	/// Iterate over T's reverse_links that match type `L`
	pub fn find_reverse_links<T: TypeStorable, L: ReverseLinked + 'static>(&'a self, val: &T) -> Option<impl Iterator<Item = &'a L>> {
		let hash = Self::get_hash(val);
		let key = &(hash, TypeId::of::<L>());
		let links = self.reverse_links_map.borrow().get(key).cloned();

		links.map(|links: Rc<_>| {
			ReverseLinkIter::<'a, L> { links, index: 0, _phantom: Default::default() }
		})
	}
}

struct ReverseLinkIter<'a, L> {
	links: Rc<RefCell<Vec<*const ()>>>,
	index: usize,
	_phantom: PhantomData<&'a L>,
}
impl<'a, L> Iterator for ReverseLinkIter<'a, L> {
    type Item = &'a L;

    fn next(&mut self) -> Option<Self::Item> {
		self.links.borrow().get(self.index).map(|ptr| {
			// Safety: All ptrs put in Vec should be of TypeId L
			self.index += 1;
			unsafe { &*ptr.cast::<L>() }
		})
    }
}