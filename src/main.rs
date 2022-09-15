#![warn(clippy::pedantic)]
#![allow(clippy::module_name_repetitions)]
#![cfg_attr(feature = "strict", deny(warnings))]

use crate::{
    build::{build, FsEntry},
    extract::{extract, read_index},
};
use clap::Parser;
use std::{error::Error, fs, fs::File, path::PathBuf};

#[macro_use]
mod macros;

mod build;
mod extract;
mod script;
mod utils;
mod xor;

#[derive(Parser)]
enum Command {
    Build(Build),
    Extract(Extract),
}

fn main() -> Result<(), Box<dyn Error>> {
    match Command::parse() {
        Command::Build(cmd) => cmd.run(),
        Command::Extract(cmd) => cmd.run(),
    }
}

#[derive(Parser)]
struct Build {
    input: PathBuf,
    #[clap(short)]
    output: PathBuf,
}

impl Build {
    fn run(self) -> Result<(), Box<dyn Error>> {
        let mut out = File::create(&self.output)?;

        build(&mut out, |path| {
            let path = self.input.join(path);
            let metadata = fs::metadata(&path)?;
            if metadata.is_dir() {
                let names: Result<Vec<_>, Box<dyn Error>> = fs::read_dir(&path)?
                    .map(|e| Ok(e?.file_name().into_string().unwrap()))
                    .collect();
                Ok(FsEntry::Dir(names?))
            } else {
                Ok(FsEntry::File(fs::read(path)?))
            }
        })
    }
}

#[derive(Parser)]
struct Extract {
    input: PathBuf,
    #[clap(short)]
    output: PathBuf,
}

impl Extract {
    fn run(self) -> Result<(), Box<dyn Error>> {
        let mut input = self.input.into_os_string().into_string().unwrap();
        if !input.ends_with("he0") {
            return Err("input path must end with \"he0\"".into());
        }

        if !self.output.exists() {
            fs::create_dir(&self.output)?;
        }

        let mut f = File::open(&input)?;
        let index = read_index(&mut f)?;
        drop(f);

        let disk_number = 2;
        input.truncate(input.len() - 3);
        input.push_str("(b)"); // Disk 1 is (a), disk 2 is (b), etc
        let mut f = File::open(&input)?;
        extract(&index, disk_number, &mut f, &mut |path, data| {
            let path = self.output.join(path);
            fs::create_dir_all(path.parent().unwrap())?;
            fs::write(path, data)?;
            Ok(())
        })?;
        drop(f);
        Ok(())
    }
}
