#![warn(missing_docs)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(redundant_imports)]
#![warn(unreachable_pub)]
#![warn(unused_crate_dependencies)]
// clippy groups
#![warn(clippy::pedantic)]
#![warn(clippy::cargo)]
#![warn(clippy::nursery)]
// clippy restriction lints
#![warn(clippy::absolute_paths)]
#![warn(clippy::allow_attributes)]
#![warn(clippy::allow_attributes_without_reason)]
#![warn(clippy::as_conversions)]
#![warn(clippy::assertions_on_result_states)]
#![warn(clippy::clone_on_ref_ptr)]
#![warn(clippy::empty_enum_variants_with_brackets)]
#![warn(clippy::empty_structs_with_brackets)]
#![warn(clippy::field_scoped_visibility_modifiers)]
#![warn(clippy::get_unwrap)]
#![warn(clippy::if_then_some_else_none)]
#![warn(clippy::impl_trait_in_params)]
#![warn(clippy::missing_assert_message)]
#![warn(clippy::mixed_read_write_in_expression)]
#![warn(clippy::module_name_repetitions)]
#![warn(clippy::multiple_inherent_impl)]
#![warn(clippy::redundant_test_prefix)]
#![warn(clippy::redundant_type_annotations)]
#![warn(clippy::renamed_function_params)]
#![warn(clippy::rest_pat_in_fully_bound_structs)]
#![warn(clippy::return_and_then)]
#![warn(clippy::same_name_method)]
#![warn(clippy::str_to_string)]
#![warn(clippy::string_to_string)]
#![warn(clippy::tests_outside_test_module)]
#![warn(clippy::try_err)]
#![warn(clippy::unneeded_field_pattern)]
#![warn(clippy::unused_result_ok)]

//! # wif-weave
//!
//! `wif-weave` is a collection of utilities for parsing `.wif` weaving files.
//!
//! ## Crate Features
//!
//! ### `async`
//!
//! Enable this for async reads and writes, using the async functionality from configparser, which uses tokio
//!

pub mod wif;
#[doc(inline)]
pub use wif::Section;
#[doc(inline)]
pub use wif::Wif;
