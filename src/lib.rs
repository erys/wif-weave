#![deny(missing_docs)]

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
pub use wif::Wif;
#[doc(inline)]
pub use wif::WifSection;
