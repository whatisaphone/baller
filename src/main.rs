#![warn(clippy::pedantic)]
#![allow(clippy::module_name_repetitions)]
#![cfg_attr(feature = "strict", deny(warnings))]

use crate::{
    build::{build, FsEntry},
    extract::extract,
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
        if !self.output.exists() {
            fs::create_dir(&self.output)?;
        }

        let mut s = File::open(&self.input)?;

        extract(&mut s, &mut |path, data| {
            let path = self.output.join(path);
            fs::create_dir_all(path.parent().unwrap())?;
            fs::write(path, data)?;
            Ok(())
        })
    }
}
