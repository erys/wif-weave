use configparser::ini::{Ini, WriteOptions};
use sections::*;
use std::io;
use std::path::Path;

pub mod sections;

pub const WIF_DEVELOPERS: &str = "wif@mhsoft.com";
pub const WIF_DATE: &str = "April 20, 1997";
pub const WIF_VERSION: &str = "1.1";

trait Section {
    const NAME: &'static str;
    const REQUIRED: bool = false;
}

#[derive(Debug, Clone)]
pub struct Wif {
    pub header: Header,
    pub color_palette: Option<ColorPalette>,
    pub warp_symbol_palette: Option<WarpSymbolPalette>,
    pub weft_symbol_palette: Option<WeftSymbolPalette>,
    pub text: Option<Text>,
    pub weaving: Option<Weaving>,
    pub warp: Option<Warp>,
    pub weft: Option<Weft>,
    pub notes: Option<Notes>,
    pub tie_up: Option<TieUp>,
    pub color_table: Option<ColorTable>,
    pub warp_symbol_table: Option<WarpSymbolTable>,
    pub weft_symbol_table: Option<WeftSymbolTable>,
    pub threading: Option<Threading>,
    pub warp_thickness: Option<WarpThickness>,
    pub warp_thickness_zoom: Option<WarpThicknessZoom>,
    pub warp_spacing: Option<WarpSpacing>,
    pub warp_spacing_zoom: Option<WarpSpacingZoom>,
    pub warp_colors: Option<WarpColors>,
    pub warp_symbols: Option<WarpSymbols>,
    pub treadling: Option<Treadling>,
    pub lift_plan: Option<LiftPlan>,
    pub weft_thickness: Option<WeftThickness>,
    pub weft_thickness_zoom: Option<WeftThicknessZoom>,
    pub weft_spacing: Option<WeftSpacing>,
    pub weft_spacing_zoom: Option<WeftSpacingZoom>,
    pub weft_colors: Option<WeftColors>,
    pub weft_symbols: Option<WeftSymbols>,
}

#[cfg(feature = "async")]
impl Wif {
    pub async fn load_async<T: AsRef<Path>>(path: T) -> Result<Self, String> {
        let mut ini = Ini::new();
        ini.load_async(path).await?;
        Self::from_ini(&ini)
    }

    pub async fn write_async<T: AsRef<Path>>(&self, path: T) -> Result<(), io::Error> {
        self.to_ini().write_async(path).await
    }
}

impl Wif {
    fn write_options() -> WriteOptions {
        let mut options = WriteOptions::new();
        options.blank_lines_between_sections = 1;
        options
    }

    fn from_ini(ini: &Ini) -> Result<Self, String> {
        todo!();
    }

    pub fn load<T: AsRef<Path>>(path: T) -> Result<Self, String> {
        let mut ini = Ini::new();
        ini.load(path)?;
        Self::from_ini(&ini)
    }

    pub fn read(text: String) -> Result<Self, String> {
        let mut ini = Ini::new();
        ini.read(text)?;
        Self::from_ini(&ini)
    }

    fn to_ini(&self) -> Ini {
        todo!();
    }

    pub fn write<T: AsRef<Path>>(&self, path: T) -> Result<(), io::Error> {
        self.to_ini().pretty_write(path, &Self::write_options())
    }

    pub fn writes(&self) -> String {
        self.to_ini().pretty_writes(&Self::write_options())
    }
}
