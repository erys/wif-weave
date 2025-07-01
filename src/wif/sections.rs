//! Structures for specific sections in the weaving file

use crate::Section;
use crate::wif::ParseError;
use crate::wif::data::{ColorMetadata, ParsedValue, WifColor, WifParseable, WifSequence};
use indexmap::IndexMap;

/// Color palette in a `.wif`. Other sections may reference colors in this palette by index
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ColorPalette {
    color_range: ColorMetadata,
    colors: Option<WifSequence<WifColor>>,
}

impl ColorPalette {
    /// Constructs a new [`ColorPalette`]. If both arguments are [None], returns [None]
    #[must_use]
    pub fn maybe_build(
        color_range: Option<ColorMetadata>,
        colors: Option<WifSequence<WifColor>>,
    ) -> Option<Self> {
        match (color_range, colors) {
            (None, None) => None,
            (None, colors) => Some(Self {
                color_range: ColorMetadata::missing(),
                colors,
            }),
            (Some(color_range), colors) => Some(Self {
                color_range,
                colors,
            }),
        }
    }

    /// The color range of entries in the palette. May be 0-255, but some popular programs also use 0-999
    #[must_use]
    pub fn color_range(&self) -> Option<&(u32, u32)> {
        self.color_range.inner().as_option()
    }

    /// The colors in the palette
    #[must_use]
    pub const fn colors(&self) -> Option<&WifSequence<WifColor>> {
        self.colors.as_ref()
    }

    /// Serialize into 2 sections of the wif
    pub fn push_and_mark(&self, map: &mut IndexMap<String, IndexMap<String, Option<String>>>) {
        Section::ColorPalette.push_and_mark(map, self.color_range.as_option());
        Section::ColorTable.push_and_mark(map, self.colors());
        if let Some(colors) = self.colors() {
            map.entry(Section::ColorPalette.to_string())
                .or_default()
                .insert(String::from("Entries"), Some(format!("{}", colors.0.len())));
        }
    }
}

/// WEAVING section of wif file
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Weaving {
    shafts: ParsedValue<u32>,
    treadles: Option<ParsedValue<u32>>,
    rising_shed: Option<ParsedValue<bool>>,
}

impl Weaving {
    const SHAFTS: &'static str = "Shafts";
    const TREADLES: &'static str = "Treadles";
    const RISING_SHED: &'static str = "Rising Shed";

    /// Shaft count
    #[must_use]
    pub const fn shafts(&self) -> &ParsedValue<u32> {
        &self.shafts
    }

    /// Treadle count
    #[must_use]
    pub const fn treadles(&self) -> Option<&ParsedValue<u32>> {
        self.treadles.as_ref()
    }

    /// Whether it's a rising shed draft
    #[must_use]
    pub const fn rising_shed(&self) -> Option<&ParsedValue<bool>> {
        self.rising_shed.as_ref()
    }
}

impl WifParseable for Weaving {
    fn from_index_map(conf_data: &IndexMap<String, Option<String>>) -> (Self, Vec<ParseError>) {
        let new = Self {
            shafts: ParsedValue::parse_required(Self::SHAFTS, conf_data),
            treadles: ParsedValue::parse_optional(Self::TREADLES, conf_data),
            rising_shed: ParsedValue::parse_optional(Self::RISING_SHED, conf_data),
        };
        let errors: Vec<ParseError> = [
            new.shafts.error(),
            new.treadles.as_ref().and_then(ParsedValue::error),
            new.rising_shed.as_ref().and_then(ParsedValue::error),
        ]
        .into_iter()
        .filter_map(Option::<&ParseError>::cloned)
        .collect();

        (new, errors)
    }

    fn to_index_map(&self) -> IndexMap<String, Option<String>> {
        let mut map = IndexMap::new();
        self.shafts.insert(Self::SHAFTS.to_owned(), &mut map);
        if let Some(treadles) = &self.treadles {
            treadles.insert(Self::TREADLES.to_owned(), &mut map);
        }
        if let Some(rising_shed) = &self.rising_shed {
            rising_shed.insert(Self::RISING_SHED.to_owned(), &mut map);
        }

        map
    }
}
