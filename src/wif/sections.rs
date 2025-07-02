//! Structures for specific sections in the weaving file

use crate::Section;
use crate::wif::ParseError;
use crate::wif::data::{
    ColorMetadata, OptionalValue, ParsedValue, ThreadUnit, WifColor, WifDecimal, WifParseable,
    WifSequence, WifValue,
};
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
    pub fn color_range(&self) -> Option<&(usize, usize)> {
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
    shafts: ParsedValue<usize>,
    treadles: OptionalValue<usize>,
    rising_shed: OptionalValue<bool>,
}

impl Weaving {
    const SHAFTS: &'static str = "Shafts";
    const TREADLES: &'static str = "Treadles";
    const RISING_SHED: &'static str = "Rising Shed";

    /// Shaft count
    #[must_use]
    pub const fn shafts(&self) -> &ParsedValue<usize> {
        &self.shafts
    }

    /// Treadle count
    #[must_use]
    pub const fn treadles(&self) -> Option<&ParsedValue<usize>> {
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
        let errors: Vec<ParseError> = transform_errors([
            new.shafts.error(),
            field_error(&new.treadles),
            field_error(&new.treadles),
        ]);

        (new, errors)
    }

    fn to_index_map(&self) -> IndexMap<String, Option<String>> {
        let mut map = IndexMap::new();
        self.shafts.insert(Self::SHAFTS.to_owned(), &mut map);
        maybe_insert(&self.treadles, Self::TREADLES, &mut map);
        maybe_insert(&self.rising_shed, Self::RISING_SHED, &mut map);

        map
    }
}

/// Info in the Warp or Weft section of a wif
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WarpWeft {
    threads: ParsedValue<usize>,
    color_index: OptionalValue<usize>,
    units: OptionalValue<ThreadUnit>,
    spacing: OptionalValue<WifDecimal>,
    thickness: OptionalValue<WifDecimal>,
    spacing_zoom: OptionalValue<WifDecimal>,
    thickness_zoom: OptionalValue<WifDecimal>,
}

impl WarpWeft {
    const THREADS: &'static str = "Threads";
    const COLOR_INDEX: &'static str = "Color";
    const UNITS: &'static str = "Units";
    const SPACING: &'static str = "Spacing";
    const THICKNESS: &'static str = "Thickness";
    const SPACING_ZOOM: &'static str = "Spacing Zoom";
    const THICKNESS_ZOOM: &'static str = "Thickness Zoom";

    /// Number of threads in warp/weft
    #[must_use]
    pub const fn threads(&self) -> &ParsedValue<usize> {
        &self.threads
    }

    /// Default color as index in color palette
    #[must_use]
    pub const fn color_index(&self) -> Option<&ParsedValue<usize>> {
        self.color_index.as_ref()
    }

    /// Unit for warp/weft
    #[must_use]
    pub const fn units(&self) -> Option<&ParsedValue<ThreadUnit>> {
        self.units.as_ref()
    }

    /// Spacing for warp/weft
    #[must_use]
    pub const fn spacing(&self) -> Option<&ParsedValue<WifDecimal>> {
        self.spacing.as_ref()
    }

    /// Thickness for warp/weft
    #[must_use]
    pub const fn thickness(&self) -> Option<&ParsedValue<WifDecimal>> {
        self.thickness.as_ref()
    }

    /// Spacing zoom for warp/weft
    #[must_use]
    pub const fn spacing_zoom(&self) -> Option<&ParsedValue<WifDecimal>> {
        self.spacing_zoom.as_ref()
    }

    /// Thickness zoom for warp/weft
    #[must_use]
    pub const fn thickness_zoom(&self) -> Option<&ParsedValue<WifDecimal>> {
        self.thickness_zoom.as_ref()
    }
}

impl WifParseable for WarpWeft {
    fn from_index_map(conf_data: &IndexMap<String, Option<String>>) -> (Self, Vec<ParseError>)
    where
        Self: Sized,
    {
        let new = Self {
            threads: ParsedValue::parse_required(Self::THREADS, conf_data),
            color_index: ParsedValue::parse_optional(Self::COLOR_INDEX, conf_data),
            units: ParsedValue::parse_optional(Self::UNITS, conf_data),
            spacing: ParsedValue::parse_optional(Self::SPACING, conf_data),
            thickness: ParsedValue::parse_optional(Self::THICKNESS, conf_data),
            spacing_zoom: ParsedValue::parse_optional(Self::SPACING_ZOOM, conf_data),
            thickness_zoom: ParsedValue::parse_optional(Self::THICKNESS_ZOOM, conf_data),
        };
        let errors = transform_errors([
            new.threads.error(),
            field_error(&new.color_index),
            field_error(&new.units),
            field_error(&new.spacing),
            field_error(&new.thickness),
            field_error(&new.spacing_zoom),
            field_error(&new.thickness_zoom),
        ]);

        (new, errors)
    }

    fn to_index_map(&self) -> IndexMap<String, Option<String>> {
        let mut map = IndexMap::new();
        self.threads.insert(Self::THREADS.to_owned(), &mut map);
        maybe_insert(&self.color_index, Self::COLOR_INDEX, &mut map);
        maybe_insert(&self.units, Self::UNITS, &mut map);
        maybe_insert(&self.spacing, Self::SPACING, &mut map);
        maybe_insert(&self.thickness, Self::THICKNESS, &mut map);
        maybe_insert(&self.spacing_zoom, Self::SPACING_ZOOM, &mut map);
        maybe_insert(&self.thickness_zoom, Self::THICKNESS_ZOOM, &mut map);

        map
    }
}

#[expect(clippy::ref_option, reason = "Values are in this form")]
fn field_error<T: WifValue>(field_value: &OptionalValue<T>) -> Option<&ParseError> {
    field_value.as_ref().and_then(ParsedValue::error)
}

fn transform_errors<'a, T>(errors: T) -> Vec<ParseError>
where
    T: IntoIterator<Item = Option<&'a ParseError>>,
{
    errors
        .into_iter()
        .filter_map(Option::<&ParseError>::cloned)
        .collect()
}

#[expect(
    clippy::ref_option,
    reason = "Internal function, keys come in this form"
)]
fn maybe_insert<T: WifValue>(
    value: &OptionalValue<T>,
    key: &str,
    map: &mut IndexMap<String, Option<String>>,
) {
    if let Some(value) = value {
        value.insert(key.to_owned(), map);
    }
}
