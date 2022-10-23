#![allow(unused_imports)]
#![feature(proc_macro_quote)]

use std::{collections::hash_map::DefaultHasher, hash::{Hash, Hasher}};

use syn::{Attribute, Field, Fields, GenericParam, Generics, Ident, Item, ItemEnum, ItemImpl, ItemStruct, ItemType, ItemUnion, Lit, Meta, MetaList, NestedMeta, Token, Type, WhereClause, parse_macro_input, parse_quote, punctuated::Punctuated};
use quote::{format_ident, quote};
use proc_macro2::TokenStream;
use proc_macro::{TokenStream as ProcTokenStream};


/// ### `#[hashtype]` Macro
/// Adds derives for various traits such as `std::hash::Hash` and rkyv traits.
/// `#[subtype]` attribute markers for marking where a hash should be. (custom serialization logic for rkyv using `#[with(WithHashType)]` attribute)
/// Implements ReverseLinks traits for struct, enum, or union. reverse_links function will reflect usage of `#[subtype_reverse_link]` attribute
/// Implements UniqueId for any struct, enum, union, or type alias that has no type or const generics.
/// 
/// Also derives Hash, PartialEq, Eq, rkyv::Archive, rkyv::Serialize, and rkyv::Deserialize if applied to struct or enum.
/// 
/// ### Usage
/// ```
/// // Derives traits, doesn't implement HashType because of generic T.
/// #[hashtype]
/// struct Vec2<T> { first: T, second: T }
/// 
/// // Derives traits and HashType, Wraps &'a String with WithHashType wrapper for rkyv.
/// #[hashtype]
/// struct Name<'a> { #[hashtype] name: &'a String }
/// 
/// // Derives traits. Derives HashType with reverse linked "name" field.
/// #[hashtype(reverse_links("name"))]
/// struct NameList<'a> { name: Name<'a>, #[hashtype] rest: &'a NameList<'a> }
/// 
/// // Doesn't derive traits because not struct or enum, derives HashType with reverse linked "first" and "second" fields.
/// #[hashtype(reverse_links("first", "second"))]
/// type Vec2f32 = Vec2<f32>;
/// ```
#[proc_macro_attribute]
pub fn hashtype(_attr: ProcTokenStream, input: ProcTokenStream) -> ProcTokenStream {
	// Get input item (struct, enum, union, or type alias)
	let mut item = parse_macro_input!(input as Item);


	// List of iterators for specific fields, as well as a list of any used type parameters for those fields
	let iter_list = &mut Vec::<TokenStream>::new();
	let used_type_param_list = &mut Vec::<Ident>::new();
	// Helper function for adding `#[with(WithHashType)]` and `#[omit_bounds]` attributes 
	// and finding `#[subtype_reverse_link]` attribute markers for `ReverseLinks` impl generation.
	let mut replace_field_attrs = |field_index: usize, field: &mut Field, do_iter_gen: bool | {
		let attrs = &mut field.attrs;
		// Find #[subtype] and if found, remove it and add relevant annotations
		if let Some((idx, ident)) = attrs.into_iter().enumerate().flat_map(|(idx, attr)| attr.path.get_ident().map(|id: &Ident|(idx, id))).find(|(_, ident)| {
			ident == &"subtype" || ident == &"subtype_reverse_link"
		}) {
			if ident == &"subtype_reverse_link" && do_iter_gen {
				if let Type::Path(path) = &field.ty {
					if let Some(param) = path.path.get_ident() {
						used_type_param_list.push(param.clone());
					}
				}
				let field_ident = field.ident.clone().map(|id|id.to_string()).unwrap_or(field_index.to_string());
				let field_ident = format_ident!("{}", field_ident);
				
				iter_list.push(quote! {
					{
						use std::hash::{Hasher, Hash};
						let mut hasher = std::collections::hash_map::DefaultHasher::new();
						self.#field_ident.hash(&mut hasher);
						let hash = hasher.finish();
						std::iter::once(hash)
					}
					
				});
			}
			attrs.remove(idx);
			attrs.push(parse_quote! {
				#[with(hashdb::WithHashType)]
			});
			attrs.push(parse_quote! {
				#[omit_bounds]
			});
		}
	};

	// Collect field and attribute data and modify attributes
	match &mut item {
		Item::Union(ItemUnion { fields, .. }) | Item::Struct(ItemStruct { fields: Fields::Named(fields), .. }) => {
			for (idx, field) in fields.named.iter_mut().enumerate() { replace_field_attrs(idx, field, true) }
		}
		Item::Struct(ItemStruct { fields: Fields::Unnamed(fields), .. }) => {
			for (idx, field) in fields.unnamed.iter_mut().enumerate() { replace_field_attrs(idx, field, true) }
		}
		Item::Enum(ItemEnum { variants, .. }) => {
			for variant in variants {
				for (idx, field) in &mut variant.fields.iter_mut().enumerate() {
					replace_field_attrs(idx, field, false)
				}
			}
		}
		Item::Type(_) => {},
		_ => unimplemented!()
	}

	// Add rkyv and derive attributes to item is struct, enum, or union
	let derived_item = match &item {
		Item::Struct(ItemStruct { generics, .. })
		 | Item::Enum(ItemEnum { generics, .. })
		 | Item::Union(ItemUnion { generics, .. }) => {
			let lifetime = generics.lifetimes().next().expect("#[hashtype] requires object to have at least one lifetime").clone().lifetime;
			let deserialize_literal = format!("__D: ArchiveDeserializer<{lifetime}>");
			quote! {
				#[derive(Hash, PartialEq, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
				#[archive_attr(derive(bytecheck::CheckBytes))]
				#[archive(bound(serialize = "__S: ArchiveStore", deserialize = #deserialize_literal))]
				#item
			}
		}
		_ => quote!{ #item },
	};

	// Generate ReverseLinks impl for struct, enum, or union
	let reverse_links_impl = match &item {
		 | Item::Struct(ItemStruct {ident, generics, .. })
		 | Item::Enum(ItemEnum { ident, generics, .. })
		 | Item::Union(ItemUnion { ident, generics, .. }) => {
			// Generate `Iterator::chain` token stream
			let iters = iter_list.into_iter().fold(
				quote! { std::iter::empty() }, |xs, ts| {
					quote! { #xs.chain(#ts) }
				}
			);

			// Require HashType bound on any type parmeter that is used as a subtype_reverse_link for impl
			let mut impl_generics: Generics = generics.clone();
			impl_generics.params.iter_mut().for_each(|param| {
				if let GenericParam::Type(param) = param {
					// Check if param is actually used as reverse_link
					if used_type_param_list.into_iter().find(|ident| {
						**ident == param.ident
					}).is_some() {
						param.bounds.push(parse_quote! { hashdb::HashType });
					}
				}
			});
			let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
			quote! {
				impl #impl_generics hashdb::ReverseLinks for #ident #ty_generics #where_clause {
					fn reverse_links(&self) -> impl Iterator<Item = u64> {
						#iters
					}
				}
			}
		}
		_ => TokenStream::new()
	};

	//
	let unique_id_impl = match &item {
		Item::Type(ItemType { ident, generics, .. })
		 | Item::Struct(ItemStruct {ident, generics, .. })
		 | Item::Enum(ItemEnum { ident, generics, .. })
		 | Item::Union(ItemUnion { ident, generics, .. }) => {
			// Impl UniqueId only if there are no type or const params.
			if generics.type_params().next().is_none() && generics.const_params().next().is_none() {
				let hash = get_hash(&item);
				
				quote! {
					impl #generics hashdb::UniqueId for #ident #generics {
						
						fn unique_id() -> u64 {
							#hash
						}
					}
				}
			} else { quote! {  } }
		 }
		 _ => unimplemented!()
	};
	
    let output = quote! {
		#derived_item

		#reverse_links_impl

		#unique_id_impl
    };
	output.into()
}

// Generate hash helper
fn get_hash(hash_object: &impl Hash) -> u64 {
	let mut hasher = DefaultHasher::new();
	hash_object.hash(&mut hasher);
	hasher.finish()
}


// return HashType impl for some Data (struct, enum or union)
fn impl_hashtype(item: &syn::Ident) -> TokenStream {
	let hash = get_hash(item);
	quote! {
		impl ReverseLinks for #item {
			fn reverse_links(&self) -> impl Iterator<Item = u64> {
				std::iter::empty::<u64>()
			}
		}
		impl UniqueId for #item  {
			fn unique_id() -> u64 {
				#hash
			}
		}
	}
}

#[proc_macro]
pub fn impl_hashtype_for(input: ProcTokenStream) -> ProcTokenStream {
	let path = parse_macro_input!(input as Ident);
	let output = impl_hashtype(&path);

	output.into()
}

#[proc_macro]
pub fn impl_hashtype_for_many(input: ProcTokenStream) -> ProcTokenStream {
	let list = parse_macro_input!(input with Punctuated<Ident, Token![,]>::parse_terminated).into_iter();
	let mut output = TokenStream::new();
	output.extend(list.map(|path|impl_hashtype(&path)));

	output.into()
}