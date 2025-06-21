//! The wif module provides classes needed to extract data from a `.wif` file

use configparser::ini::{Ini, WriteOptions};
use indexmap::IndexMap;
use std::cmp::Ordering;
use std::collections::HashSet;
use std::io;
use std::num::ParseIntError;
use std::path::Path;
use strum::{Display, EnumIter, EnumString, IntoEnumIterator, IntoStaticStr};
use thiserror::Error;

/// Default value for the developers field in the WIF header
pub const WIF_DEVELOPERS: &str = "wif@mhsoft.com";
/// Default value for the date field in the WIF header
pub const WIF_DATE: &str = "April 20, 1997";
/// Current version of `.wif`
pub const WIF_VERSION: &str = "1.1";

/// Representation of the data in a `.wif` file
#[derive(Debug, Clone)]
pub struct Wif {
    inner_map: IndexMap<String, IndexMap<String, Option<String>>>,
    treadling: Option<WifSequence<Vec<u32>>>,
    threading: Option<WifSequence<Vec<u32>>>,
    lift_plan: Option<WifSequence<Vec<u32>>>,
    color_palette: Option<ColorPalette>,
    tie_up: Option<WifSequence<Vec<u32>>>,
}

#[cfg(feature = "async")]
impl Wif {
    /// Asynchronously read a file and parse it into a [Wif]
    pub async fn load_async<T: AsRef<Path>>(path: T) -> Result<Self, String> {
        let mut ini = Ini::new();
        ini.load_async(path).await?;
        Self::from_ini(&ini)
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

    fn from_ini(ini: &Ini) -> Result<Self, String> {
        todo!();
    }

    /// Construct a [Wif] from a file.
    pub fn load<T: AsRef<Path>>(path: T) -> Result<Self, String> {
        let mut ini = Ini::new();
        ini.load(path)?;
        Self::from_ini(&ini)
    }

    /// Construct a [Wif] from the string contents of a `.wif`
    pub fn read(text: String) -> Result<Self, String> {
        let mut ini = Ini::new();
        ini.read(text)?;
        Self::from_ini(&ini)
    }

    fn to_ini(&self) -> Ini {
        todo!();
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

    /// If all threads only go through one heddle (standard), returns the threading
    pub fn single_threading(&self) -> Result<Option<WifSequence<u32>>, String> {
        todo!()
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
        if self.color_palette.as_ref().map(|p| &p.colors).is_some() {
            contents.insert(ColorTable);
        }
        if self
            .color_palette
            .as_ref()
            .map(|p| &p.color_range)
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
#[derive(Error, Debug, Clone)]
pub enum ParseError {
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

/// Trait for entries in "array like" sections of `.wif`
pub trait SequenceValue {
    /// Whether this value should be treated as there or not
    fn present(&self) -> bool;
    /// parse from the string value in the `.wif`. Error is the expected type
    fn parse(string_value: &str) -> Result<Self, String>
    where
        Self: Sized;
}

/// Color palette in a `.wif`. Other sections may reference colors in this palette by index
#[derive(Clone, PartialEq, Debug)]
pub struct ColorPalette {
    color_range: Option<(u32, u32)>,
    colors: Option<WifSequence<WifColor>>,
}

impl ColorPalette {
    /// The color range of entries in the palette. May be 0-255, but some popular programs also use 0-999
    pub fn color_range(&self) -> Option<(u32, u32)> {
        self.color_range
    }

    /// The colors in the palette
    pub fn colors(&self) -> Option<&WifSequence<WifColor>> {
        self.colors.as_ref()
    }
}

/// An RGB tuple representing a thread color. Note that the color range is not always 0-255.
/// The actual range is specified in [ColorPalette]
#[derive(Clone, PartialEq, Debug)]
pub struct WifColor(u32, u32, u32);

impl SequenceValue for WifColor {
    fn present(&self) -> bool {
        true
    }

    fn parse(string_value: &str) -> Result<Self, String>
    where
        Self: Sized,
    {
        let values = string_value
            .split(',')
            .map(|s| s.trim().parse::<u32>())
            .collect::<Result<Vec<u32>, ParseIntError>>()
            .map_err(|_| String::from("color triple"))?;
        if values.len() != 3 {
            Err(String::from("color triple"))
        } else {
            Ok(WifColor(values[0], values[1], values[2]))
        }
    }
}

impl SequenceValue for u32 {
    fn present(&self) -> bool {
        *self > 0
    }
    fn parse(string_value: &str) -> Result<Self, String>
    where
        Self: Sized,
    {
        string_value
            .trim()
            .parse::<u32>()
            .map_err(|_| String::from("non-negative integer"))
    }
}
impl SequenceValue for Vec<u32> {
    fn present(&self) -> bool {
        !self.is_empty()
    }
    fn parse(string_value: &str) -> Result<Self, String>
    where
        Self: Sized,
    {
        let result: Result<Self, ParseIntError> = string_value
            .split(",")
            .map(|s| s.trim().parse::<u32>())
            .collect();
        result.map_err(|_| String::from("list of shafts"))
    }
}

/// # Represents a single threading or treadling entry
///
/// A value of `0` represents no thread/treadle.
#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct SequenceEntry<T>
where
    T: SequenceValue + Clone,
{
    index: usize,
    value: T,
}

impl<T: SequenceValue + Clone> SequenceEntry<T> {
    /// Index of entry
    pub fn index(&self) -> usize {
        self.index
    }

    /// Returns the value of the entry.
    ///
    /// 0 indicates no entry at this index. To get [None] use [value_option][Self::value_option]
    pub fn value(&self) -> &T {
        &self.value
    }

    /// Returns value of the entry as an [Option]
    ///
    /// Similar to [value][Self::value] but returns [None] instead of `0`
    pub fn value_option(&self) -> Option<&T> {
        if self.value.present() {
            Some(&self.value)
        } else {
            None
        }
    }
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

/// # Represents the sequence of numbers that compose a threading or treadling
///
/// [SequenceEntry]'s in the vector should be in order by in order by index, with no duplicates or
/// missing entries, but this is not guaranteed when constructed from a `.wif` file.
#[derive(PartialEq, Debug, Clone, Default)]
pub struct WifSequence<T: Clone + SequenceValue>(Vec<SequenceEntry<T>>);

impl<T: Clone + SequenceValue + Default> WifSequence<T> {
    /// Constructs a new [WifSequence] from an array. This sequence will always be valid
    pub fn from_array(sequence: &[T]) -> Self {
        WifSequence(
            sequence
                .iter()
                .enumerate()
                .map(|(index, value)| SequenceEntry {
                    index: index + 1,
                    value: value.clone(),
                })
                .collect(),
        )
    }

    /// Same as [from_array](Self::from_array) but it accepts [None] in place of 0 values
    pub fn from_option_array(sequence: &[Option<T>]) -> Self {
        WifSequence(
            sequence
                .iter()
                .enumerate()
                .map(|(index, value)| {
                    let value = value.as_ref();
                    SequenceEntry {
                        index: index + 1,
                        value: value.unwrap_or(&Default::default()).clone(),
                    }
                })
                .collect(),
        )
    }

    /// Constructs a sequence from an [IndexMap]. Returns a parse error on invalid keys or values
    pub fn from_index_map(
        conf_data: &IndexMap<String, Option<String>>,
    ) -> Result<Self, ParseError> {
        Ok(WifSequence(
            conf_data
                .iter()
                .map(|(key, value)| {
                    let value = value
                        .as_ref()
                        .ok_or(ParseError::MissingValue(key.clone()))?;
                    let index = key
                        .parse::<usize>()
                        .map_err(|_| ParseError::BadIntegerKey(key.clone()))?;

                    let value = T::parse(value).map_err(|e| ParseError::BadValueType {
                        key: key.clone(),
                        value: value.clone(),
                        expected_type: e,
                    })?;
                    Ok::<SequenceEntry<T>, ParseError>(SequenceEntry { index, value })
                })
                .collect::<Result<Vec<SequenceEntry<T>>, ParseError>>()?,
        ))
    }

    /// Validates indices within the sequence to ensure that they are non-zero and strictly increasing
    ///
    /// ```
    /// # use indexmap::indexmap;
    /// # use wif_weave::wif::{SequenceError, WifSequence};
    /// let map = indexmap! {
    ///     String::from("0") => Some(String::from("1"))    
    /// };
    /// let sequence = WifSequence::<u32>::from_index_map(&map);
    /// assert_eq!(SequenceError::ZeroError(0), sequence.unwrap().validate().unwrap_err());
    /// ```
    pub fn validate(&self) -> Result<(), SequenceError> {
        if !self.0.is_empty() && self.0[0].index == 0 {
            return Err(SequenceError::ZeroError(0));
        }

        for i in 0..(self.0.len() - 1) {
            let pair = &self.0[i..(i + 2)];
            let ok_index = pair[0].index;
            let maybe_index = pair[1].index;
            match ok_index.cmp(&maybe_index) {
                Ordering::Less => continue,
                Ordering::Equal => {
                    return Err(SequenceError::RepeatError {
                        last_ok_position: i,
                        error_position: i + 1,
                        duplicate_index: ok_index,
                    });
                }
                Ordering::Greater => {
                    return Err(SequenceError::OutOfOrderError {
                        last_ok_index: ok_index,
                        out_of_order_index: maybe_index,
                        out_of_order_position: i + 1,
                    });
                }
            }
        }
        Ok(())
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_parse_vec() {
        assert_eq!(Vec::parse("1,4,6,8").unwrap(), vec![1, 4, 6, 8]);
        assert_eq!(Vec::parse("1 ,4 ,6, 8    ").unwrap(), vec![1, 4, 6, 8]);
        assert_eq!(Vec::parse("1,4,6,8,a").unwrap_err(), "list of shafts");
        assert_eq!(Vec::parse("asdlf").unwrap_err(), "list of shafts");
        assert_eq!(Vec::parse("-1").unwrap_err(), "list of shafts");
    }

    #[test]
    fn test_parse_color() {
        assert_eq!(WifColor::parse("1,0,5").unwrap(), WifColor(1, 0, 5));
        assert_eq!(WifColor::parse("1,0,5,7").unwrap_err(), "color triple");
        assert_eq!(WifColor::parse("1   ,0,5    ").unwrap(), WifColor(1, 0, 5));
        assert_eq!(WifColor::parse("1,").unwrap_err(), "color triple");
    }

    #[test]
    fn test_parse_u32() {
        assert_eq!(u32::parse("1").unwrap(), 1);
        assert_eq!(u32::parse("  1   ").unwrap(), 1);
        assert_eq!(u32::parse("-1").unwrap_err(), "non-negative integer");
        assert_eq!(u32::parse("a").unwrap_err(), "non-negative integer");
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
