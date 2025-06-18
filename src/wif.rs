use configparser::ini::{Ini, WriteOptions};
use std::io;
use std::path::Path;

mod sections;

pub const WIF_DEVELOPERS: &str = "wif@mhsoft.com";
pub const WIF_DATE: &str = "April 20, 1997";
pub const WIF_VERSION: &str = "1.1";

trait Section {
    const NAME: &'static str;
    const REQUIRED: bool = false;
}

#[derive(Debug)]
pub struct Wif {
    header: sections::Header,
}

#[cfg(feature = "async")]
impl Wif {
    pub async fn load_async<T: AsRef<Path>>(path: T) -> Result<Self, String> {
        let mut ini = Ini::new();
        ini.load_async(path).await?;
        Self::from(&ini)
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

    fn from(ini: &Ini) -> Result<Self, String> {
        todo!();
    }

    pub fn load<T: AsRef<Path>>(path: T) -> Result<Self, String> {
        let mut ini = Ini::new();
        ini.load(path)?;
        Self::from(&ini)
    }

    pub fn read(text: String) -> Result<Self, String> {
        let mut ini = Ini::new();
        ini.read(text)?;
        Self::from(&ini)
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
