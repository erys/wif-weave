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
#[derive(Debug, PartialEq, Clone)]
pub struct Header {
    version: String,
    date: String,
    developers: String,
    source_program: String,
    source_version: Option<String>,
}

// Informational sections
#[derive(Debug, Clone)]
pub struct ColorPalette {}
impl Section for ColorPalette {
    const NAME: &'static str = "COLOR PALETTE";
}
#[derive(Debug, Clone)]
pub struct WarpSymbolPalette {}
impl Section for WarpSymbolPalette {
    const NAME: &'static str = "WARP SYMBOL PALETTE";
}
#[derive(Debug, Clone)]
pub struct WeftSymbolPalette {}
impl Section for WeftSymbolPalette {
    const NAME: &'static str = "WEFT SYMBOL PALETTE";
}
#[derive(Debug, Clone)]
pub struct Text {}
impl Section for Text {
    const NAME: &'static str = "TEXT";
}
#[derive(Debug, Clone)]
pub struct Weaving {}
impl Section for Weaving {
    const NAME: &'static str = "WEAVING";
}
#[derive(Debug, Clone)]
pub struct Warp {}
impl Section for Warp {
    const NAME: &'static str = "WARP";
}
#[derive(Debug, Clone)]
pub struct Weft {}
impl Section for Weft {
    const NAME: &'static str = "WEFT";
}

// Beginning of Data Sections
#[derive(Debug, Clone)]
pub struct Notes {}
impl Section for Notes {
    const NAME: &'static str = "NOTES";
}
#[derive(Debug, Clone)]
pub struct TieUp {}
impl Section for TieUp {
    const NAME: &'static str = "TIEUP";
}

// Color and Symbol tables
#[derive(Debug, Clone)]
pub struct ColorTable {}
impl Section for ColorTable {
    const NAME: &'static str = "COLOR TABLE";
}
#[derive(Debug, Clone)]
pub struct WarpSymbolTable {}
impl Section for WarpSymbolTable {
    const NAME: &'static str = "WARP SYMBOL TABLE";
}
#[derive(Debug, Clone)]
pub struct WeftSymbolTable {}
impl Section for WeftSymbolTable {
    const NAME: &'static str = "WEFT SYMBOL TABLE";
}
// Warp Data Sections
#[derive(Debug, Clone)]
pub struct Threading {}
impl Section for Threading {
    const NAME: &'static str = "THREADING";
}
#[derive(Debug, Clone)]
pub struct WarpThickness {}
impl Section for WarpThickness {
    const NAME: &'static str = "WARP THICKNESS";
}
#[derive(Debug, Clone)]
pub struct WarpThicknessZoom {}
impl Section for WarpThicknessZoom {
    const NAME: &'static str = "WARP THICKNESS ZOOM";
}
#[derive(Debug, Clone)]
pub struct WarpSpacing {}
impl Section for WarpSpacing {
    const NAME: &'static str = "WARP SPACING";
}
#[derive(Debug, Clone)]
pub struct WarpSpacingZoom {}
impl Section for WarpSpacingZoom {
    const NAME: &'static str = "WARP SPACING ZOOM";
}
#[derive(Debug, Clone)]
pub struct WarpColors {}
impl Section for WarpColors {
    const NAME: &'static str = "WARP COLORS";
}
#[derive(Debug, Clone)]
pub struct WarpSymbols {}
impl Section for WarpSymbols {
    const NAME: &'static str = "WARP SYMBOLS";
}
// Weft Data Sections
#[derive(Debug, Clone)]
pub struct Treadling {}
impl Section for Treadling {
    const NAME: &'static str = "TREADLING";
}
#[derive(Debug, Clone)]
pub struct LiftPlan {}
impl Section for LiftPlan {
    const NAME: &'static str = "LIFTPLAN";
}
#[derive(Debug, Clone)]
pub struct WeftThickness {}
impl Section for WeftThickness {
    const NAME: &'static str = "WEFT THICKNESS";
}
#[derive(Debug, Clone)]
pub struct WeftThicknessZoom {}
impl Section for WeftThicknessZoom {
    const NAME: &'static str = "WEFT THICKNESS ZOOM";
}
#[derive(Debug, Clone)]
pub struct WeftSpacing {}
impl Section for WeftSpacing {
    const NAME: &'static str = "WEFT SPACING";
}
#[derive(Debug, Clone)]
pub struct WeftSpacingZoom {}
impl Section for WeftSpacingZoom {
    const NAME: &'static str = "WEFT SPACING ZOOM";
}
#[derive(Debug, Clone)]
pub struct WeftColors {}
impl Section for WeftColors {
    const NAME: &'static str = "WEFT COLORS";
}
#[derive(Debug, Clone)]
pub struct WeftSymbols {}
impl Section for WeftSymbols {
    const NAME: &'static str = "WEFT SYMBOLS";
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

enum Sections {
    Header {
        version: String,
        date: String,
        developers: String,
        source_program: String,
        source_version: Option<String>,
    },
}
impl Section for Sections {
    const NAME: &'static str = "WIF";
}
