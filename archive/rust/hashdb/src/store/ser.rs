use std::collections::HashMap;

use rkyv::{AlignedVec, Fallible, ser::{ScratchSpace, Serializer, serializers::{AlignedSerializer, AllocScratch, AllocScratchError, FallbackScratch, HeapScratch, SharedSerializeMapError}}};

use crate::{Hash};

#[derive(Debug, Error)]
pub enum LinkSerializeError {
	#[error("scratch error: {0}")]
	AllocScratchError(#[from] AllocScratchError),
	#[error("shared map error: {0}")]
	SharedSerializeMapError(#[from] SharedSerializeMapError),
}

/// Serialize object trees into linked hashes and store into a Datastore
#[derive(Default)]
pub struct LinkSerializer {
	scratch: FallbackScratch<HeapScratch<1024>, AllocScratch>, // Scratch space
	serializer: AlignedSerializer<AlignedVec>,                 // Serializer
	pub pointer_map: HashMap<*const (), (Hash, usize)>,            // Check for shared pointers to avoid serializing the same object twice
}

impl LinkSerializer {
	pub fn get_vec(&mut self) -> AlignedVec {
		std::mem::take(&mut self.serializer).into_inner()
	}
}
impl Fallible for LinkSerializer {
	type Error = LinkSerializeError;
}

impl ScratchSpace for LinkSerializer {
	unsafe fn push_scratch(&mut self, layout: std::alloc::Layout) -> Result<std::ptr::NonNull<[u8]>, Self::Error> {
		Ok(self.scratch.push_scratch(layout)?)
	}

	unsafe fn pop_scratch(&mut self, ptr: std::ptr::NonNull<u8>, layout: std::alloc::Layout) -> Result<(), Self::Error> {
		Ok(self.scratch.pop_scratch(ptr, layout)?)
	}
}

impl Serializer for LinkSerializer {
	fn pos(&self) -> usize {
		self.serializer.pos()
	}

	fn write(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
		Ok(self.serializer.write(bytes).unwrap())
	}
}