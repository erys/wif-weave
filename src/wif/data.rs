//! Module for handling the data types within a wif file

use crate::Section;
use crate::wif::{ParseError, SequenceError};
use indexmap::{IndexMap, indexmap};
use std::cmp::Ordering;
use std::num::ParseIntError;
use std::vec;

/// Trait for values in `.wif` that are parseable from a string
pub trait WifValue {
    /// Expected type to find in the wif file
    const EXPECTED_TYPE: &'static str;
    /// Whether this value should be treated as there or not
    fn present(&self) -> bool;
    /// Parse from the string value in the `.wif`.
    ///
    /// # Errors
    /// When the value can't be parsed into the expected type
    fn parse(string_value: &str, key_for_err: &str) -> Result<Self, ParseError>
    where
        Self: Sized;

    /// Construct a parse error
    #[must_use]
    fn type_error(string_value: &str, key_for_err: &str) -> ParseError {
        ParseError::BadValueType {
            value: string_value.to_owned(),
            key: key_for_err.to_owned(),
            expected_type: Self::EXPECTED_TYPE.to_owned(),
        }
    }

    /// Parse an array from the value
    ///
    /// # Errors
    /// If the values aren't a comma separated list of positive integers
    fn parse_arr(string_value: &str, key_for_err: &str) -> Result<Vec<u32>, ParseError> {
        string_value
            .split(',')
            .map(|s| s.trim().parse::<u32>())
            .collect::<Result<Vec<u32>, ParseIntError>>()
            .map_err(|_| Self::type_error(string_value, key_for_err))
    }

    /// Serialize into a string
    fn to_wif_string(&self) -> String;
}

/// Color palette in a `.wif`. Other sections may reference colors in this palette by index
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ColorPalette {
    color_range: Option<ColorMetadata>,
    colors: Option<WifSequence<WifColor>>,
}

impl ColorPalette {
    /// Constructs a new [`ColorPalette`]. If both arguments are [None], returns [None]
    #[must_use]
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
    #[must_use]
    pub const fn color_range(&self) -> Option<ColorMetadata> {
        self.color_range
    }

    /// The colors in the palette
    #[must_use]
    pub const fn colors(&self) -> Option<&WifSequence<WifColor>> {
        self.colors.as_ref()
    }

    /// Serialize into 2 sections of the wif
    pub fn push_and_mark(&self, map: &mut IndexMap<String, IndexMap<String, Option<String>>>) {
        Section::ColorPalette.push_and_mark(map, self.color_range.as_ref());
        Section::ColorTable.push_and_mark(map, self.colors());
        if let Some(colors) = self.colors() {
            map.entry(Section::ColorPalette.to_string())
                .or_default()
                .insert(String::from("Entries"), Some(format!("{}", colors.0.len())));
        }
    }
}

/// An RGB tuple representing a thread color. Note that the color range is not always 0-255.
/// The actual range is specified in [`ColorPalette`]
#[derive(Clone, PartialEq, Eq, Debug, Copy)]
pub struct WifColor(pub u32, pub u32, pub u32);

impl WifValue for WifColor {
    const EXPECTED_TYPE: &'static str = "color triple";

    fn present(&self) -> bool {
        true
    }

    fn parse(string_value: &str, key_for_err: &str) -> Result<Self, ParseError> {
        let values = Self::parse_arr(string_value, key_for_err)?;
        match values.len() {
            3 => Ok(Self(values[0], values[1], values[2])),
            _ => Err(Self::type_error(string_value, key_for_err)),
        }
    }

    fn to_wif_string(&self) -> String {
        format!("{0},{1},{2}", self.0, self.1, self.2)
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
            .parse::<Self>()
            .map_err(|_| Self::type_error(string_value, key_for_err))
    }

    fn to_wif_string(&self) -> String {
        self.to_string()
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

    fn to_wif_string(&self) -> String {
        self.iter()
            .map(ToString::to_string)
            .collect::<Vec<String>>()
            .join(",")
    }
}

/// # Represents a single threading or treadling entry
///
/// A value of `0` represents no thread/treadle.
#[derive(Debug, Clone, PartialOrd, PartialEq, Eq)]
pub struct SequenceEntry<T>
where
    T: WifValue + Clone,
{
    index: usize,
    value: T,
}

impl<T: WifValue + Clone> SequenceEntry<T> {
    /// Index of entry
    pub const fn index(&self) -> usize {
        self.index
    }

    /// Returns the value of the entry.
    ///
    /// 0 indicates no entry at this index. To get [`None`] use [`value_option`][Self::value_option]
    pub const fn value(&self) -> &T {
        &self.value
    }

    /// Returns value of the entry as an [Option]
    ///
    /// Similar to [`value`][Self::value] but returns [`None`] instead of `0`
    pub fn value_option(&self) -> Option<&T> {
        self.value.present().then_some(&self.value)
    }
}

impl SequenceEntry<Vec<u32>> {
    fn to_single(&self) -> Result<SequenceEntry<u32>, usize> {
        let new_value = match self.value.len() {
            0 => 0,
            1 => self.value[0],
            _ => return Err(self.index),
        };
        Ok(SequenceEntry {
            index: self.index,
            value: new_value,
        })
    }
}

/// For data types that can be extracted from a wif
pub trait WifParseable {
    /// Parse the data from a section of the wif file
    ///
    /// # Errors
    /// If the keys or values aren't the expected types
    fn from_index_map(conf_data: &IndexMap<String, Option<String>>) -> Result<Self, ParseError>
    where
        Self: Sized;

    /// Serialize into an index map
    fn to_index_map(&self) -> IndexMap<String, Option<String>>;
}

/// The color metadata in the `COLOR PALETTE` section of a wif. We only care about the range for the rgb values.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct ColorMetadata(pub u32, pub u32);

impl WifValue for ColorMetadata {
    const EXPECTED_TYPE: &'static str = "color range";
    fn present(&self) -> bool {
        true
    }

    fn parse(string_value: &str, key_for_err: &str) -> Result<Self, ParseError> {
        let values = Self::parse_arr(string_value, key_for_err)?;

        match values.len() {
            2 => Ok(Self(values[0], values[1])),
            _ => Err(Self::type_error(string_value, key_for_err)),
        }
    }

    fn to_wif_string(&self) -> String {
        format!("{0},{1}", self.0, self.1)
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

    fn to_index_map(&self) -> IndexMap<String, Option<String>> {
        indexmap! {
            String::from("Range") => Some(self.to_wif_string())
        }
    }
}

/// # Represents the sequence of numbers that compose a threading or treadling
///
/// [`SequenceEntry`]'s in the vector should be in order by in order by index, with no duplicates or
/// missing entries, but this is not guaranteed when constructed from a `.wif` file.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct WifSequence<T: Clone + WifValue>(pub Vec<SequenceEntry<T>>);

impl WifSequence<Vec<u32>> {
    /// Converts a sequence of arrays to a sequence of numbers. Err value is the first index with multiple numbers
    ///
    /// # Errors
    /// The error is the first index with multiple values
    pub fn to_single_sequence(&self) -> Result<WifSequence<u32>, usize> {
        Ok(WifSequence(
            self.0
                .iter()
                .map(SequenceEntry::to_single)
                .collect::<Result<Vec<SequenceEntry<u32>>, usize>>()?,
        ))
    }
}

/// Iterator for a [`WifSequence`], returns a default when indices are skipped, returns clones of entries
#[derive(Debug)]
pub struct SequenceIterDefault<'a, T: Clone + WifValue + Default> {
    /// index of iterator
    index: usize,
    /// index into Vec
    inner_index: usize,
    sequence: &'a WifSequence<T>,
}

impl<T: Clone + WifValue + Default> Iterator for SequenceIterDefault<'_, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.index += 1;
        if self.inner_index >= self.sequence.0.len() {
            return None;
        }

        if self.index > self.sequence.0[self.inner_index].index {
            Some(Default::default())
        } else {
            self.inner_index += 1;
            Some(self.sequence.0[self.inner_index - 1].value.clone())
        }
    }
}

/// Iterator for a [`WifSequence`], returns items as Option, returning `Some(None)` on skipped
/// indices, and `Some(Some(T))` on present ones
#[derive(Debug)]
pub struct SequenceIterOption<'a, T: Clone + WifValue> {
    /// index of iterator
    index: usize,
    /// index into Vec
    inner_index: usize,
    sequence: &'a WifSequence<T>,
}

impl<'a, T: Clone + WifValue> Iterator for SequenceIterOption<'a, T> {
    type Item = Option<&'a T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.index += 1;
        if self.inner_index >= self.sequence.0.len() {
            return None;
        }

        if self.index > self.sequence.0[self.inner_index].index {
            Some(None)
        } else {
            self.inner_index += 1;
            Some(Some(&self.sequence.0[self.inner_index - 1].value))
        }
    }
}

impl<T: Clone + WifValue> IntoIterator for WifSequence<T> {
    type Item = SequenceEntry<T>;
    type IntoIter = vec::IntoIter<SequenceEntry<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T: Clone + WifValue> WifParseable for WifSequence<T> {
    /// Constructs a sequence from an [`IndexMap`]. Returns a parse error on invalid keys or values
    fn from_index_map(conf_data: &IndexMap<String, Option<String>>) -> Result<Self, ParseError> {
        Ok(Self(
            conf_data
                .iter()
                .map(|(key, value)| {
                    let Some(value) = value.as_ref() else {
                        return Err(ParseError::MissingValue(key.clone()));
                    };
                    let Ok(index) = key.parse::<usize>() else {
                        return Err(ParseError::BadIntegerKey(key.clone()));
                    };

                    let value = T::parse(value, key)?;
                    Ok::<SequenceEntry<T>, ParseError>(SequenceEntry { index, value })
                })
                .collect::<Result<Vec<SequenceEntry<T>>, ParseError>>()?,
        ))
    }

    fn to_index_map(&self) -> IndexMap<String, Option<String>> {
        let mut map = IndexMap::new();
        self.0.iter().for_each(|e| {
            map.insert(e.index.to_string(), Some(e.value.to_wif_string()));
        });

        map
    }
}

impl<T: Clone + WifValue + Default> WifSequence<T> {
    /// Same as [`from_array`](Self::from_array) but it accepts [None] in place of 0 values
    pub fn from_option_array(sequence: &[Option<T>]) -> Self {
        Self(
            sequence
                .iter()
                .enumerate()
                .map(|(index, value)| {
                    let value = value.as_ref();
                    SequenceEntry {
                        index: index + 1,
                        value: value.map_or_else(Default::default, Clone::clone),
                    }
                })
                .collect(),
        )
    }

    /// Returns an owned iterator that returns default values for skipped indices
    #[must_use]
    pub const fn default_iter(&self) -> SequenceIterDefault<T> {
        SequenceIterDefault {
            index: 0,
            inner_index: 0,
            sequence: self,
        }
    }
}

impl<T: Clone + WifValue> WifSequence<T> {
    /// Constructs a new [`WifSequence`] from an array. This sequence will always be valid
    pub fn from_array(sequence: &[T]) -> Self {
        Self(
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

    /// Creates an iterator that returns the values in the sequence, wrapped in an option, with `None` for missing values
    #[must_use]
    pub const fn option_iter(&self) -> SequenceIterOption<T> {
        SequenceIterOption {
            index: 0,
            inner_index: 0,
            sequence: self,
        }
    }

    /// Validates indices within the sequence to ensure that they are non-zero and strictly increasing
    ///
    /// # Errors
    /// Returns an error if indices are out of order.
    ///
    /// # Examples
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
                Ordering::Less => {}
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
    fn parse_vec() {
        assert_eq!(Vec::parse("1,4,6,8", "").unwrap(), vec![1, 4, 6, 8]);
        assert_eq!(Vec::parse("1 ,4 ,6, 8    ", "").unwrap(), vec![1, 4, 6, 8]);
        assert_eq!(
            Vec::parse("1,4,6,8,a", "").unwrap_err(),
            ParseError::BadValueType {
                key: String::new(),
                value: String::from("1,4,6,8,a"),
                expected_type: String::from("list of shafts")
            }
        );
        assert_eq!(
            Vec::parse("asdlf", "").unwrap_err(),
            ParseError::BadValueType {
                key: String::new(),
                value: String::from("asdlf"),
                expected_type: String::from("list of shafts")
            }
        );
        assert_eq!(
            Vec::parse("-1", "").unwrap_err(),
            ParseError::BadValueType {
                key: String::new(),
                value: String::from("-1"),
                expected_type: String::from("list of shafts")
            }
        );
    }

    #[test]
    fn parse_color() {
        assert_eq!(WifColor::parse("1,0,5", "").unwrap(), WifColor(1, 0, 5));
        assert_eq!(
            WifColor::parse("1,0,5,7", "").unwrap_err(),
            ParseError::BadValueType {
                value: String::from("1,0,5,7"),
                key: String::new(),
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
                key: String::new(),
                value: String::from("1,"),
                expected_type: String::from("color triple")
            }
        );
    }
}
