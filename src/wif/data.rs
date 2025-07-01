//! Module for handling the data types within a wif file

use crate::Section;
use crate::wif::{ParseError, SequenceError};
use indexmap::{IndexMap, indexmap};
use std::cmp::Ordering;
use std::num::ParseIntError;
use std::vec;

const TRUES: [&str; 6] = ["true", "yes", "t", "y", "on", "1"];
const FALSES: [&str; 6] = ["false", "no", "f", "n", "off", "0"];

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

impl WifValue for bool {
    const EXPECTED_TYPE: &'static str = "ini boolean";

    fn present(&self) -> bool {
        true
    }

    fn parse(string_value: &str, key_for_err: &str) -> Result<Self, ParseError>
    where
        Self: Sized,
    {
        let lower = string_value.trim().to_lowercase();
        if TRUES.contains(&lower.as_str()) {
            Ok(true)
        } else if FALSES.contains(&lower.as_str()) {
            Ok(false)
        } else {
            Err(Self::type_error(string_value, key_for_err))
        }
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

impl WifValue for (u32, u32) {
    const EXPECTED_TYPE: &'static str = "Integer pair";

    fn present(&self) -> bool {
        true
    }

    fn parse(string_value: &str, key_for_err: &str) -> Result<Self, ParseError>
    where
        Self: Sized,
    {
        let vec = Self::parse_arr(string_value, key_for_err)?;
        match vec.len() {
            2 => Ok((vec[0], vec[1])),
            _ => Err(Self::type_error(string_value, key_for_err)),
        }
    }

    fn to_wif_string(&self) -> String {
        format!("{}, {}", self.0, self.1)
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
    fn from_index_map(conf_data: &IndexMap<String, Option<String>>) -> (Self, Vec<ParseError>)
    where
        Self: Sized;

    /// Serialize into an index map
    fn to_index_map(&self) -> IndexMap<String, Option<String>>;
}

/// The color metadata in the `COLOR PALETTE` section of a wif. We only care about the range for the rgb values.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ColorMetadata(ParsedValue<(u32, u32)>);

impl ColorMetadata {
    pub(crate) const fn missing() -> Self {
        Self(ParsedValue(Err((
            ParseError::MissingDependentSection {
                missing_section: Section::ColorPalette,
                dependent_section: Section::ColorTable,
            },
            None,
        ))))
    }

    pub(crate) const fn inner(&self) -> &ParsedValue<(u32, u32)> {
        &self.0
    }

    pub(crate) const fn as_option(&self) -> Option<&Self> {
        match self.0.0 {
            Err((ParseError::MissingDependentSection { .. }, ..)) => None,
            _ => Some(self),
        }
    }
}

impl WifParseable for ColorMetadata {
    fn from_index_map(conf_data: &IndexMap<String, Option<String>>) -> (Self, Vec<ParseError>) {
        let parsed: ParsedValue<(u32, u32)> = ParsedValue::parse_required("range", conf_data);
        let errors: Vec<ParseError> = parsed
            .0
            .as_ref()
            .err()
            .map_or(vec![], |e| vec![e.0.clone()]);

        (Self(parsed), errors)
    }

    fn to_index_map(&self) -> IndexMap<String, Option<String>> {
        indexmap! {
            String::from("Range") => self.0.to_wif_string()
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
    fn from_index_map(conf_data: &IndexMap<String, Option<String>>) -> (Self, Vec<ParseError>) {
        let mut sequence = Vec::new();
        let mut errors = Vec::new();
        for (key, value) in conf_data {
            let Some(value) = value.as_ref() else {
                errors.push(ParseError::MissingValue(key.clone()));
                continue;
            };
            let Ok(index) = key.parse::<usize>() else {
                errors.push(ParseError::BadIntegerKey(key.clone()));
                continue;
            };

            let result = T::parse(value, key);
            match result {
                Ok(value) => sequence.push(SequenceEntry { index, value }),
                Err(e) => errors.push(e),
            }
        }
        (Self(sequence), errors)
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
    /// let sequence = WifSequence::<u32>::from_index_map(&map).0;
    /// assert_eq!(SequenceError::Zero(0), sequence.validate().unwrap_err());
    /// ```
    pub fn validate(&self) -> Result<(), SequenceError> {
        if !self.0.is_empty() && self.0[0].index == 0 {
            return Err(SequenceError::Zero(0));
        }

        for i in 0..(self.0.len() - 1) {
            let pair = &self.0[i..(i + 2)];
            let ok_index = pair[0].index;
            let maybe_index = pair[1].index;
            match ok_index.cmp(&maybe_index) {
                Ordering::Less => {}
                Ordering::Equal => {
                    return Err(SequenceError::Repeat {
                        last_ok_position: i,
                        error_position: i + 1,
                        duplicate_index: ok_index,
                    });
                }
                Ordering::Greater => {
                    return Err(SequenceError::OutOfOrder {
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

/// Value parsed from a wif field. If parsing failed, the original value is inside the `Err`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedValue<T>(Result<T, (ParseError, Option<String>)>)
where
    T: WifValue;

impl<T> ParsedValue<T>
where
    T: WifValue,
{
    fn parse(wif_string: &str, key: &str) -> Self {
        let result = T::parse(wif_string, key);
        Self(result.map_err(|e| (e, Some(wif_string.to_owned()))))
    }

    /// Get as result
    ///
    /// # Errors
    /// Original parse error and string value
    pub const fn as_result(&self) -> Result<&T, &(ParseError, Option<String>)> {
        self.0.as_ref()
    }

    /// Gets inner value as an option
    pub fn as_option(&self) -> Option<&T> {
        self.0.as_ref().ok()
    }

    /// Get error from result
    pub fn error(&self) -> Option<&ParseError> {
        self.0.as_ref().map_err(|e| &e.0).err()
    }

    pub(crate) fn parse_optional(
        key: &str,
        map: &IndexMap<String, Option<String>>,
    ) -> Option<Self> {
        match map.get(&key.to_lowercase()) {
            None => None,
            Some(None) => Some(Self(Err((ParseError::MissingValue(key.to_owned()), None)))),
            Some(Some(wif_string)) => Some(Self::parse(wif_string, key)),
        }
    }

    pub(crate) fn parse_required(key: &str, map: &IndexMap<String, Option<String>>) -> Self {
        let lower_key = key.to_lowercase();
        match map.get(&lower_key) {
            None => Self(Err((ParseError::MissingField(key.to_owned()), None))),
            Some(None) => Self(Err((ParseError::MissingValue(key.to_owned()), None))),
            Some(Some(wif_string)) => Self::parse(wif_string, key),
        }
    }

    /// Get the value as a string. If parsing failed, returns the original string value
    pub fn to_wif_string(&self) -> Option<String> {
        match &self.0 {
            Ok(v) => Some(v.to_wif_string()),
            Err((_, wif_string)) => wif_string.clone(),
        }
    }

    pub(crate) fn insert(&self, key: String, map: &mut IndexMap<String, Option<String>>) {
        map.insert(key, self.to_wif_string());
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
