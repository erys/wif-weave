//! Modules for handling the data types within a wif file

use crate::wif::{ParseError, SequenceError};
use indexmap::IndexMap;
use std::cmp::Ordering;
use std::num::ParseIntError;

/// Trait for values in `.wif` that are parseable from a string
pub trait WifValue {
    /// Expected type to find in the wif file
    const EXPECTED_TYPE: &'static str;
    /// Whether this value should be treated as there or not
    fn present(&self) -> bool;
    /// Parse from the string value in the `.wif`. Error is the expected type
    fn parse(string_value: &str, key_for_err: &str) -> Result<Self, ParseError>
    where
        Self: Sized;

    /// Construct a parse error
    fn type_error(string_value: &str, key_for_err: &str) -> ParseError {
        ParseError::BadValueType {
            value: string_value.to_owned(),
            key: key_for_err.to_owned(),
            expected_type: Self::EXPECTED_TYPE.to_owned(),
        }
    }

    /// Parse an array from the value
    fn parse_arr(string_value: &str, key_for_err: &str) -> Result<Vec<u32>, ParseError> {
        string_value
            .split(',')
            .map(|s| s.trim().parse::<u32>())
            .collect::<Result<Vec<u32>, ParseIntError>>()
            .map_err(|_| Self::type_error(string_value, key_for_err))
    }
}

/// Color palette in a `.wif`. Other sections may reference colors in this palette by index
#[derive(Clone, PartialEq, Debug)]
pub struct ColorPalette {
    color_range: Option<ColorMetadata>,
    colors: Option<WifSequence<WifColor>>,
}

impl ColorPalette {
    /// Constructs a new [ColorPalette]. If both arguments are [None], returns [None]
    pub fn maybe_build(
        color_range: Option<ColorMetadata>,
        colors: Option<WifSequence<WifColor>>,
    ) -> Option<Self> {
        if color_range.is_none() && colors.is_none() {
            None
        } else {
            Some(Self {
                color_range,
                colors,
            })
        }
    }

    /// The color range of entries in the palette. May be 0-255, but some popular programs also use 0-999
    pub fn color_range(&self) -> Option<ColorMetadata> {
        self.color_range
    }

    /// The colors in the palette
    pub fn colors(&self) -> Option<&WifSequence<WifColor>> {
        self.colors.as_ref()
    }
}

/// An RGB tuple representing a thread color. Note that the color range is not always 0-255.
/// The actual range is specified in [ColorPalette]
#[derive(Clone, PartialEq, Debug, Default)]
pub struct WifColor(u32, u32, u32);

impl WifValue for WifColor {
    const EXPECTED_TYPE: &'static str = "color triple";

    fn present(&self) -> bool {
        true
    }

    fn parse(string_value: &str, key_for_err: &str) -> Result<Self, ParseError> {
        let values = Self::parse_arr(string_value, key_for_err)?;
        if values.len() != 3 {
            Err(Self::type_error(string_value, key_for_err))
        } else {
            Ok(WifColor(values[0], values[1], values[2]))
        }
    }
}

impl WifValue for u32 {
    const EXPECTED_TYPE: &'static str = "non-negative integer";

    fn present(&self) -> bool {
        *self > 0
    }
    fn parse(string_value: &str, key_for_err: &str) -> Result<Self, ParseError> {
        string_value
            .trim()
            .parse::<u32>()
            .map_err(|_| Self::type_error(string_value, key_for_err))
    }
}

impl WifValue for Vec<u32> {
    const EXPECTED_TYPE: &'static str = "list of shafts";
    fn present(&self) -> bool {
        !self.is_empty()
    }
    fn parse(string_value: &str, key_for_err: &str) -> Result<Self, ParseError> {
        Self::parse_arr(string_value, key_for_err)
    }
}

/// # Represents a single threading or treadling entry
///
/// A value of `0` represents no thread/treadle.
#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct SequenceEntry<T>
where
    T: WifValue + Clone,
{
    index: usize,
    value: T,
}

impl<T: WifValue + Clone> SequenceEntry<T> {
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

/// For data types that can be extracted from a wif
pub trait WifParseable {
    /// Parse the data from a section of the wif file
    fn from_index_map(conf_data: &IndexMap<String, Option<String>>) -> Result<Self, ParseError>
    where
        Self: Sized;
}

/// The color metadata in the `COLOR PALETTE` section of a wif. We only care about the range for the rgb values.
#[derive(PartialEq, Debug, Clone, Copy)]
pub struct ColorMetadata(u32, u32);

impl WifValue for ColorMetadata {
    const EXPECTED_TYPE: &'static str = "color range";
    fn present(&self) -> bool {
        true
    }

    fn parse(string_value: &str, key_for_err: &str) -> Result<Self, ParseError> {
        let values = Self::parse_arr(string_value, key_for_err)?;

        if values.len() != 2 {
            Err(Self::type_error(string_value, key_for_err))
        } else {
            Ok(ColorMetadata(values[0], values[1]))
        }
    }
}

impl WifParseable for ColorMetadata {
    fn from_index_map(conf_data: &IndexMap<String, Option<String>>) -> Result<Self, ParseError> {
        let value = conf_data.get("range");

        let Some(Some(value)) = value else {
            return Err(ParseError::MissingField(String::from("range")));
        };

        Self::parse(value, "range")
    }
}

/// # Represents the sequence of numbers that compose a threading or treadling
///
/// [SequenceEntry]'s in the vector should be in order by in order by index, with no duplicates or
/// missing entries, but this is not guaranteed when constructed from a `.wif` file.
#[derive(PartialEq, Debug, Clone, Default)]
pub struct WifSequence<T: Clone + WifValue>(Vec<SequenceEntry<T>>);

impl<T: Clone + WifValue + Default> WifParseable for WifSequence<T> {
    /// Constructs a sequence from an [IndexMap]. Returns a parse error on invalid keys or values
    fn from_index_map(conf_data: &IndexMap<String, Option<String>>) -> Result<Self, ParseError> {
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

                    let value = T::parse(value, key)?;
                    Ok::<SequenceEntry<T>, ParseError>(SequenceEntry { index, value })
                })
                .collect::<Result<Vec<SequenceEntry<T>>, ParseError>>()?,
        ))
    }
}

impl<T: Clone + WifValue + Default> WifSequence<T> {
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

    /// Validates indices within the sequence to ensure that they are non-zero and strictly increasing
    ///
    /// ```
    /// # use indexmap::indexmap;
    /// # use wif_weave::wif::{SequenceError};
    /// use wif_weave::wif::data::{WifParseable, WifSequence};
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
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_vec() {
        assert_eq!(Vec::parse("1,4,6,8", "").unwrap(), vec![1, 4, 6, 8]);
        assert_eq!(Vec::parse("1 ,4 ,6, 8    ", "").unwrap(), vec![1, 4, 6, 8]);
        assert_eq!(
            Vec::parse("1,4,6,8,a", "").unwrap_err(),
            ParseError::BadValueType {
                key: String::from(""),
                value: String::from("1,4,6,8,a"),
                expected_type: String::from("list of shafts")
            }
        );
        assert_eq!(
            Vec::parse("asdlf", "").unwrap_err(),
            ParseError::BadValueType {
                key: String::from(""),
                value: String::from("asdlf"),
                expected_type: String::from("list of shafts")
            }
        );
        assert_eq!(
            Vec::parse("-1", "").unwrap_err(),
            ParseError::BadValueType {
                key: String::from(""),
                value: String::from("-1"),
                expected_type: String::from("list of shafts")
            }
        );
    }

    #[test]
    fn test_parse_color() {
        assert_eq!(WifColor::parse("1,0,5", "").unwrap(), WifColor(1, 0, 5));
        assert_eq!(
            WifColor::parse("1,0,5,7", "").unwrap_err(),
            ParseError::BadValueType {
                value: String::from("1,0,5,7"),
                key: String::from(""),
                expected_type: String::from("color triple")
            }
        );
        assert_eq!(
            WifColor::parse("1   ,0,5    ", "").unwrap(),
            WifColor(1, 0, 5)
        );
        assert_eq!(
            WifColor::parse("1,", "").unwrap_err(),
            ParseError::BadValueType {
                key: String::from(""),
                value: String::from("1,"),
                expected_type: String::from("color triple")
            }
        );
    }
}
