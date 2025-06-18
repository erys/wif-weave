use crate::wif::{Section, WIF_DATE, WIF_DEVELOPERS, WIF_VERSION};

/// # Header section
///
/// Denoted by `[WIF]` in the file
///
/// ## Fields
///
/// ### Version
///
/// `.wif` specification version, always `1.0` or `1.1`
///
/// ### Date
///
/// Specification date, always `April 20, 1997` for `1.1`
///
/// ### Developers
///
/// Specification developers, always `wif@mhsoft.com`
///
/// ### Source Program
///
/// Program that wrote the wif
#[derive(Debug, PartialEq)]
pub struct Header {
    version: String,
    date: String,
    developers: String,
    source_program: String,
    source_version: Option<String>,
}

impl Default for Header {
    fn default() -> Self {
        Self {
            version: String::from(WIF_VERSION),
            date: String::from(WIF_DATE),
            developers: String::from(WIF_DEVELOPERS),
            source_program: String::from("wif-weave"),
            source_version: None,
        }
    }
}

impl Section for Header {
    const NAME: &'static str = "WIF";
    const REQUIRED: bool = true;
}
