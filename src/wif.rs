//! The wif module provides classes needed to extract data from a `.wif` file

use crate::wif::data::{ColorPalette, WifSequence};
use configparser::ini::{Ini, WriteOptions};
use data::WifParseable;
use indexmap::IndexMap;
use std::collections::{HashMap, HashSet};
use std::io;
use std::path::Path;
use std::str::FromStr;
use strum::{Display, EnumIter, EnumString, IntoEnumIterator, IntoStaticStr};
use thiserror::Error;

/// Default value for the developers field in the WIF header
pub const WIF_DEVELOPERS: &str = "wif@mhsoft.com";
/// Default value for the date field in the WIF header
pub const WIF_DATE: &str = "April 20, 1997";
/// Current version of `.wif`
pub const WIF_VERSION: &str = "1.1";

/// Representation of the data in a `.wif` file
#[derive(Debug, Clone, Default)]
pub struct Wif {
    inner_map: IndexMap<String, IndexMap<String, Option<String>>>,
    treadling: Option<WifSequence<Vec<u32>>>,
    threading: Option<WifSequence<Vec<u32>>>,
    lift_plan: Option<WifSequence<Vec<u32>>>,
    color_palette: Option<ColorPalette>,
    tie_up: Option<WifSequence<Vec<u32>>>,
}

pub mod data;

#[cfg(feature = "async")]
impl Wif {
    /// Asynchronously read a file and parse it into a [Wif]
    pub async fn load_async<T: AsRef<Path>>(
        path: T,
    ) -> Result<(Self, HashMap<WifSection, ParseError>), String> {
        let mut ini = Ini::new();
        let map = ini.load_async(path).await?;
        Ok(Self::from_ini(map))
    }

    /// Asynchronously write to a `.wif` file
    pub async fn write_async<T: AsRef<Path>>(&self, path: T) -> Result<(), io::Error> {
        self.to_ini()
            .pretty_write_async(path, &Self::write_options())
            .await
    }
}

impl Wif {
    fn write_options() -> WriteOptions {
        let mut options = WriteOptions::new();
        options.blank_lines_between_sections = 1;
        options
    }

    fn from_ini(
        mut map: IndexMap<String, IndexMap<String, Option<String>>>,
    ) -> (Self, HashMap<WifSection, ParseError>) {
        let mut errors = HashMap::new();

        (
            Wif {
                treadling: WifSection::Treadling.parse_and_pop(&mut map, &mut errors),
                threading: WifSection::Threading.parse_and_pop(&mut map, &mut errors),
                lift_plan: WifSection::LiftPlan.parse_and_pop(&mut map, &mut errors),
                color_palette: ColorPalette::maybe_build(
                    WifSection::ColorPalette.parse_and_pop(&mut map, &mut errors),
                    WifSection::ColorTable.parse_and_pop(&mut map, &mut errors),
                ),
                tie_up: WifSection::TieUp.parse_and_pop(&mut map, &mut errors),
                inner_map: map,
            },
            errors,
        )
    }

    /// Construct a [Wif] from a file.
    pub fn load<T: AsRef<Path>>(
        path: T,
    ) -> Result<(Self, HashMap<WifSection, ParseError>), String> {
        let mut ini = Ini::new();
        let map = ini.load(path)?;
        Ok(Self::from_ini(map))
    }

    /// Construct a [Wif] from the string contents of a `.wif`
    pub fn read(text: String) -> Result<(Self, HashMap<WifSection, ParseError>), String> {
        let mut ini = Ini::new();
        let map = ini.read(text)?;
        Ok(Self::from_ini(map))
    }

    fn to_ini(&self) -> Ini {
        let mut ini = Ini::new_cs();
        let ini_map = ini.get_mut_map();
        let mut inner = self.inner_map.clone();

        // Populate header
        let mut header = inner
            .shift_remove_entry(&WifSection::Header.to_string())
            .map(|e| e.1)
            .unwrap_or_default();
        header
            .entry(String::from("Version"))
            .or_insert(Some(WIF_VERSION.to_owned()));
        header
            .entry(String::from("Date"))
            .or_insert(Some(WIF_DATE.to_owned()));
        header
            .entry(String::from("Developers"))
            .or_insert(Some(WIF_DEVELOPERS.to_owned()));
        header
            .entry(String::from("Source Program"))
            .or_insert(Some(String::from("wif-weave")));
        ini_map.insert(WifSection::Header.to_string(), header);

        // Create contents
        ini_map.insert(WifSection::Contents.to_string(), IndexMap::new());
        inner.shift_remove_entry(&WifSection::Contents.index_map_key());

        // Add parsed sections
        WifSection::Threading.push_and_mark(ini_map, &self.threading);
        WifSection::Treadling.push_and_mark(ini_map, &self.treadling);
        WifSection::LiftPlan.push_and_mark(ini_map, &self.lift_plan);
        WifSection::TieUp.push_and_mark(ini_map, &self.tie_up);
        if let Some(palette) = self.color_palette() {
            palette.push_and_mark(ini_map);
        }

        // insert other sections
        for (key, section) in inner {
            // only insert valid sections
            if let Ok(wif_section) = WifSection::from_str(key.to_uppercase().as_str()) {
                ini_map.insert(wif_section.to_string(), section);
            }
        }

        ini
    }

    /// Write to a `.wif` file
    pub fn write<T: AsRef<Path>>(&self, path: T) -> Result<(), io::Error> {
        self.to_ini().pretty_write(path, &Self::write_options())
    }

    /// Write to a string in the `.wif` format
    pub fn writes(&self) -> String {
        self.to_ini().pretty_writes(&Self::write_options())
    }

    /// Returns the threading sequence if present
    pub fn threading(&self) -> &Option<WifSequence<Vec<u32>>> {
        &self.threading
    }

    /// If all threads only go through one heddle (standard), returns the threading.
    /// Err value is the first index with multiple heddles.
    pub fn single_threading(&self) -> Result<Option<WifSequence<u32>>, usize> {
        self.threading
            .as_ref()
            .map(|s| s.to_single_sequence())
            .transpose()
    }

    /// Returns the treadling sequence if present
    pub fn treadling(&self) -> &Option<WifSequence<Vec<u32>>> {
        &self.treadling
    }

    /// Returns the lift plan if present
    pub fn lift_plan(&self) -> &Option<WifSequence<Vec<u32>>> {
        &self.lift_plan
    }

    /// Returns the tie-up if present
    pub fn tie_up(&self) -> &Option<WifSequence<Vec<u32>>> {
        &self.tie_up
    }

    /// Returns the color palette if present. Corresponds to [ColorPalette][WifSection::ColorPalette] and [ColorTable][WifSection::ColorTable]
    pub fn color_palette(&self) -> &Option<ColorPalette> {
        &self.color_palette
    }

    /// Returns list of all sections present in the original `.wif`
    pub fn contents(&self) -> HashSet<WifSection> {
        use WifSection::*;
        let mut contents = HashSet::new();
        if self.treadling.is_some() {
            contents.insert(Treadling);
        }
        if self.threading.is_some() {
            contents.insert(Threading);
        }
        if self.tie_up.is_some() {
            contents.insert(TieUp);
        }
        if self.lift_plan.is_some() {
            contents.insert(LiftPlan);
        }
        if self.color_palette.as_ref().map(|p| p.colors()).is_some() {
            contents.insert(ColorTable);
        }
        if self
            .color_palette
            .as_ref()
            .map(|p| p.color_range())
            .is_some()
        {
            contents.insert(ColorPalette);
        }

        WifSection::iter().for_each(|sec| {
            if self.inner_map.contains_key(&sec.to_string().to_lowercase()) {
                contents.insert(sec);
            }
        });

        contents
    }

    /// Get the raw data for a section that is not yet parsed.
    pub fn get_section(
        &self,
        section: &WifSection,
    ) -> Result<Option<&IndexMap<String, Option<String>>>, String> {
        if Self::implemented(section) {
            Err(String::from("Must be retrieved with specific method"))
        } else {
            Ok(self.inner_map.get(&section.to_string().to_lowercase()))
        }
    }

    /// Returns whether the given section can be retrieved via a specialized method or via [get_section](Self::get_section)
    pub fn implemented(section: &WifSection) -> bool {
        matches!(
            section,
            WifSection::Contents
                | WifSection::ColorPalette
                | WifSection::ColorTable
                | WifSection::Threading
                | WifSection::Treadling
                | WifSection::TieUp
                | WifSection::LiftPlan
        )
    }
}

/// Error when parsing Wif file
#[derive(Error, Debug, Clone, PartialEq)]
pub enum ParseError {
    /// A required field is missing
    #[error("Required field {0} is missing")]
    MissingField(String),
    /// Missing value for a key
    #[error("Key {0} has no value")]
    MissingValue(String),
    /// Non integer keys in a section that only allows integers
    #[error("Keys must be positive integers, found {0}")]
    BadIntegerKey(String),
    /// Values of the wrong type for the section+key
    #[error("Values must be {expected_type}, found {value} at key {key}")]
    BadValueType {
        /// key where the issue occurred
        key: String,
        /// bad value
        value: String,
        /// expected type
        expected_type: String,
    },
}

/// Validation errors for [WifSequence]
#[derive(Error, Debug, PartialEq)]
pub enum SequenceError {
    /// An entry with an index of 0 was found in the sequence
    #[error("Found 0 index at entry {0}")]
    ZeroError(usize),

    /// An entry with an index less than the previous index was found in the sequence
    #[error(
        "Out of order entry at position {out_of_order_position}, index {out_of_order_index} is \
        smaller than previous index {last_ok_index}"
    )]
    OutOfOrderError {
        /// Index of last valid entry
        last_ok_index: usize,
        /// Position of error entry
        out_of_order_position: usize,
        /// Index of error entry
        out_of_order_index: usize,
    },

    /// An entry with the same index as the previous entry was found in the sequence
    #[error(
        "Duplicate index {duplicate_index} at positions {last_ok_position} and {error_position}"
    )]
    RepeatError {
        /// Position of last valid entry
        last_ok_position: usize,
        /// Position of error entry
        error_position: usize,
        /// Duplicate index
        duplicate_index: usize,
    },
}

/// Enum of all the possible sections in a `.wif` document (excluding private sections)
#[derive(EnumString, Debug, PartialEq, Eq, Hash, Clone, EnumIter, IntoStaticStr, Display)]
#[strum(use_phf, serialize_all = "UPPERCASE")]
pub enum WifSection {
    /// `WIF` section
    #[strum(serialize = "WIF")]
    Header,
    /// `CONTENTS` section
    Contents,
    /// `COLOR PALETTE` section
    #[strum(serialize = "COLOR PALETTE")]
    ColorPalette,
    /// `WEFT SYMBOL PALETTE` section
    #[strum(serialize = "WEFT SYMBOL PALETTE")]
    WeftSymbolPalette,
    /// `WARP SYMBOL PALETTE` section
    #[strum(serialize = "WARP SYMBOL PALETTE")]
    WarpSymbolPalette,
    /// `TEXT` section
    Text,
    /// `WEAVING` section
    Weaving,
    /// `WARP` section
    Warp,
    /// `WEFT` section
    Weft,
    /// `Notes` section
    Notes,
    /// `TIEUP` section
    TieUp,
    /// `COLOR TABLE` section
    #[strum(serialize = "COLOR TABLE")]
    ColorTable,
    /// `WARP SYMBOL TABLE` section
    #[strum(serialize = "WARP SYMBOL TABLE")]
    WarpSymbolTable,
    /// `WEFT SYMBOL TABLE` section
    #[strum(serialize = "WEFT SYMBOL TABLE")]
    WeftSymbolTable,
    /// `THREADING` section
    Threading,
    /// `WARP THICKNESS` section
    #[strum(serialize = "WARP THICKNESS")]
    WarpThickness,
    /// `WARP THICKNESS ZOOM` section
    #[strum(serialize = "WARP THICKNESS ZOOM")]
    WarpThicknessZoom,
    /// `WARP SPACING` section
    #[strum(serialize = "WARP SPACING")]
    WarpSpacing,
    /// `WARP SPACING ZOOM` section
    #[strum(serialize = "WARP SPACING ZOOM")]
    WarpSpacingZoom,
    /// `WARP COLORS` section
    #[strum(serialize = "WARP COLORS")]
    WarpColors,
    /// `WARP SYMBOLS` section
    #[strum(serialize = "WARP SYMBOLS")]
    WarpSymbols,
    /// `TREADLING` section
    Treadling,
    /// `LIFTPLAN` section
    LiftPlan,
    /// `WEFT THICKNESS` section
    #[strum(serialize = "WEFT THICKNESS")]
    WeftThickness,
    /// `WEFT THICKNESS ZOOM` section
    #[strum(serialize = "WEFT THICKNESS ZOOM")]
    WeftThicknessZoom,
    /// `WEFT SPACING` section
    #[strum(serialize = "WEFT SPACING")]
    WeftSpacing,
    /// `WEFT SPACING ZOOM` section
    #[strum(serialize = "WEFT SPACING ZOOM")]
    WeftSpacingZoom,
    /// `WEFT COLORS` section
    #[strum(serialize = "WEFT COLORS")]
    WeftColors,
    /// `WEFT SYMBOLS` section
    #[strum(serialize = "WEFT SYMBOLS")]
    WeftSymbols,
}

impl WifSection {
    fn index_map_key(&self) -> String {
        self.to_string().to_lowercase()
    }
    fn get_data<'a>(
        &self,
        map: &'a IndexMap<String, IndexMap<String, Option<String>>>,
    ) -> Option<&'a IndexMap<String, Option<String>>> {
        map.get(&self.index_map_key())
    }

    fn parse_and_pop<T: WifParseable>(
        &self,
        map: &mut IndexMap<String, IndexMap<String, Option<String>>>,
        error_map: &mut HashMap<Self, ParseError>,
    ) -> Option<T> {
        let data = self.get_data(map)?;
        let section = T::from_index_map(data);
        match section {
            Ok(section) => {
                map.shift_remove_entry(&self.index_map_key());
                Some(section)
            }
            Err(e) => {
                error_map.insert(self.clone(), e);
                None
            }
        }
    }

    fn push_and_mark<T: WifParseable>(
        &self,
        map: &mut IndexMap<String, IndexMap<String, Option<String>>>,
        section: &Option<T>,
    ) {
        if let Some(section) = section.as_ref() {
            map.insert(WifSection::Threading.to_string(), section.to_index_map());
            WifSection::Threading.mark_present(map);
        }
    }

    fn mark_present(&self, map: &mut IndexMap<String, IndexMap<String, Option<String>>>) {
        map.entry(Self::Contents.to_string())
            .or_default()
            .insert(self.to_string(), Some(String::from("1")));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use data::WifValue;
    use std::str::FromStr;

    #[test]
    fn test_parse_u32() {
        assert_eq!(u32::parse("1", "").unwrap(), 1);
        assert_eq!(u32::parse("  1   ", "").unwrap(), 1);
        assert_eq!(
            u32::parse("-1", "").unwrap_err(),
            ParseError::BadValueType {
                key: String::from(""),
                value: String::from("-1"),
                expected_type: String::from("non-negative integer")
            }
        );
        assert_eq!(
            u32::parse("a", "").unwrap_err(),
            ParseError::BadValueType {
                key: String::from(""),
                value: String::from("a"),
                expected_type: String::from("non-negative integer")
            }
        );
    }

    #[test]
    fn test_all_wif_fields_in_enum() {
        let fields = [
            "WIF",
            "CONTENTS",
            "COLOR PALETTE",
            "WARP SYMBOL PALETTE",
            "WEFT SYMBOL PALETTE",
            "TEXT",
            "WEAVING",
            "WARP",
            "WEFT",
            "NOTES",
            "TIEUP",
            "COLOR TABLE",
            "WARP SYMBOL TABLE",
            "WEFT SYMBOL TABLE",
            "THREADING",
            "WARP THICKNESS",
            "WARP THICKNESS ZOOM",
            "WARP SPACING",
            "WARP SPACING ZOOM",
            "WARP COLORS",
            "WARP SYMBOLS",
            "TREADLING",
            "LIFTPLAN",
            "WEFT THICKNESS",
            "WEFT THICKNESS ZOOM",
            "WEFT SPACING",
            "WEFT SPACING ZOOM",
            "WEFT COLORS",
            "WEFT SYMBOLS",
        ];

        for field in fields {
            assert!(
                WifSection::from_str(field).is_ok(),
                "{field} is not in enum"
            );
        }
    }
}
