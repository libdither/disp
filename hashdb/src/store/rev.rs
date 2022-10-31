
use std::{cell::RefCell, collections::HashMap, marker::PhantomData, rc::Rc};

use crate::{HashType, LinkArena};

use super::{RevHashType, RevTypeStore, TypeStore};


pub struct RevLinkStore<'a, A: TypeStore<'a>> {
	reverse_links_map: RefCell<HashMap<(u64, u64), Rc<RefCell<Vec<*const ()>>>>>, // Reverse lookup map, str represents object type
	store: &'a A,
	_phantom: PhantomData<&'a ()>
}

impl<'a, A: TypeStore<'a>> TypeStore<'a> for RevLinkStore<'a, A> {
    fn add<T: HashType<'a>>(&'a self, val: T) -> &'a T {
        self.store.add(val)
    }
}
impl<'a, A: TypeStore<'a>> RevTypeStore<'a> for RevLinkStore<'a, A> {
    fn rev_add<T: RevHashType<'a>>(&'a self, val: T) -> &'a T {

        let ret = self.store.add(val);
		let ptr = (ret as *const T) as *const ();
		// Store Reverse Links
		let mut reverse_links_map = self.reverse_links_map.borrow_mut();
		for hash in T::reverse_links(ret) {
			let entry = reverse_links_map.entry((hash, T::unique_id())).or_default();
			// Note: See safety note in find_reverse_links, ptr points to a T
			entry.borrow_mut().push(ptr);
		}
		// println!("Added reverse links {:?}", ret);

		ret
    }

	// TODO: Only the deprecated version of this where clause works for some reason.
	#[allow(deprecated_where_clause_location)]
	type Iter<'i: 'a, L: 'i> where Self: 'i = ReverseLinkIter<'i, L>;

	fn links<T: HashType<'a>, L: RevHashType<'a>>(&'a self, val: &T) -> Self::Iter<'a, L> {
		let hash = super::arena::get_hash(val);
		let key = &(hash, L::unique_id());
		let links = self.reverse_links_map.borrow().get(key).cloned();
		println!("Looking up reverse links for {:?}", val);
		ReverseLinkIter::<'a, L> { links, index: 0, _phantom: Default::default(), _type: Default::default() }
	}
}
impl<'a, A: TypeStore<'a>> RevLinkStore<'a, A> {
	pub fn new(store: &'a A) -> Self {
		RevLinkStore {
			reverse_links_map: RefCell::new(HashMap::new()),
			store,
			_phantom: Default::default(),
		}
	}
}

// Iterator that lives for lifetime 'i and returns references to objects L that also live for lifetime 'i
pub struct ReverseLinkIter<'i, L: 'i> {
	links: Option<Rc<RefCell<Vec<*const ()>>>>,
	index: usize,
	_type: PhantomData<L>,
	_phantom: PhantomData<&'i ()>,
}
impl<'i, L: 'i> Iterator for ReverseLinkIter<'i, L> {
    type Item = &'i L;

    fn next(&mut self) -> Option<Self::Item> {
		if let Some(links) = &mut self.links {
			links.borrow().get(self.index).map(|ptr| {
				// Safety: All ptrs put in Vec should be of TypeId L
				self.index += 1;
				unsafe { &*ptr.cast::<L>() }
			})
		} else { None }
		
    }
}

pub type RevLinkArena<'a> = RevLinkStore<'a, LinkArena<'a>>;