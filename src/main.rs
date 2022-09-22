#![warn(clippy::pedantic)]
#![allow(clippy::module_name_repetitions, clippy::never_loop)]
#![cfg_attr(feature = "strict", deny(warnings))]

use crate::{
    build::{build, FsEntry},
    config::Config,
    extract::{extract, read_index, Index},
};
use clap::Parser;
use std::{
    error::Error,
    fs,
    fs::File,
    path::{Path, PathBuf},
};

#[macro_use]
mod macros;

mod build;
mod config;
mod extract;
mod script;
#[cfg(test)]
mod tests;
mod utils;
mod xor;

#[derive(Parser)]
enum Command {
    Build(Build),
    Extract(Extract),
}

fn main() {
    // In debug builds, the stack overflows in `decompile_blocks` for deeply nested
    // scripts. Run in a thread with a larger stack.
    #[cfg(debug_assertions)]
    std::thread::Builder::new()
        .stack_size(8 << 20)
        .spawn(main_thread)
        .unwrap()
        .join()
        .unwrap();

    #[cfg(not(debug_assertions))]
    main_thread();
}

fn main_thread() {
    let r = match Command::parse() {
        Command::Build(cmd) => cmd.run(),
        Command::Extract(cmd) => cmd.run(),
    };
    r.unwrap();
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
    #[clap(short)]
    config_path: Option<PathBuf>,
}

impl Extract {
    fn run(self) -> Result<(), Box<dyn Error>> {
        let config = match self.config_path {
            Some(path) => Config::from_ini(&fs::read_to_string(&path)?)?,
            None => Config::default(),
        };

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

        input.truncate(input.len() - 3);

        Self::disk(&index, &config, &mut input, &self.output, 1)?;
        Self::disk(&index, &config, &mut input, &self.output, 2)?;
        Ok(())
    }

    fn disk(
        index: &Index,
        config: &Config,
        input: &mut String,
        output: &Path,
        disk_number: u8,
    ) -> Result<(), Box<dyn Error>> {
        input.push('(');
        input.push((disk_number + b'a' - 1).into());
        input.push(')');

        let mut f = File::open(&input)?;
        extract(index, disk_number, config, &mut f, &mut |path, data| {
            let path = output.join(path);
            fs::create_dir_all(path.parent().unwrap())?;
            fs::write(path, data)?;
            Ok(())
        })?;
        drop(f);

        input.truncate(input.len() - 3);
        Ok(())
    }
}
