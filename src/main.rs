#![warn(clippy::pedantic, clippy::cargo)]
#![allow(
    clippy::match_on_vec_items,
    clippy::match_wildcard_for_single_variants,
    clippy::module_name_repetitions,
    clippy::never_loop,
    clippy::single_match,
    clippy::single_match_else
)]
#![cfg_attr(feature = "strict", deny(warnings))]

use crate::{
    blocks::{push_disk_number, strip_disk_number},
    build::{build, FsEntry},
    config::Config,
    extract::{dump_index, extract, read_index, Index},
    extract2::extract2,
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

mod blocks;
mod build;
mod config;
mod extract;
mod extract2;
mod script;
#[cfg(test)]
mod tests;
mod utils;
mod xor;

#[derive(Parser)]
enum Command {
    Build(Build),
    Extract(Extract),
    Extract2(Extract2),
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
        Command::Extract2(cmd) => cmd.run(),
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
    #[clap(short, long)]
    output: PathBuf,
    #[clap(short, long = "config")]
    config_path: Option<PathBuf>,
    #[clap(long)]
    aside: bool,
    #[clap(long, hidden(true))]
    publish_scripts: bool,
}

impl Extract {
    fn run(self) -> Result<(), Box<dyn Error>> {
        let mut config = match self.config_path {
            Some(path) => Config::from_ini(&fs::read_to_string(&path)?)?,
            None => Config::default(),
        };
        config.aside = self.aside;

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

        let mut dump = String::with_capacity(1 << 16);
        dump_index(&mut dump, &index)?;
        fs::write(&self.output.join("index.txt"), &dump)?;

        input.truncate(input.len() - 3);

        for disk_number in 1..=2 {
            Self::disk(
                &index,
                &config,
                &mut input,
                &self.output,
                disk_number,
                self.publish_scripts,
            )?;
        }
        Ok(())
    }

    fn disk(
        index: &Index,
        config: &Config,
        input: &mut String,
        output: &Path,
        disk_number: u8,
        publish_scripts: bool,
    ) -> Result<(), Box<dyn Error>> {
        push_disk_number(input, disk_number);

        let mut f = File::open(&input)?;
        extract(
            index,
            disk_number,
            config,
            publish_scripts,
            &mut f,
            &mut fs_writer(output),
        )?;
        drop(f);

        strip_disk_number(input);
        Ok(())
    }
}

#[derive(Parser)]
struct Extract2 {
    input: PathBuf,
    #[clap(short, long)]
    output: PathBuf,
    #[clap(short, long = "config")]
    config_path: Option<PathBuf>,
}

impl Extract2 {
    fn run(self) -> Result<(), Box<dyn Error>> {
        let input = self.input.into_os_string().into_string().unwrap();
        if !input.ends_with("he0") {
            return Err("input path must end with \"he0\"".into());
        }

        if !self.output.exists() {
            fs::create_dir(&self.output)?;
        }

        extract2(input, &mut fs_writer(&self.output))?;
        Ok(())
    }
}

fn fs_writer(root: &Path) -> impl Fn(&str, &[u8]) -> Result<(), Box<dyn Error>> + '_ {
    |path, data| {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap())?;
        fs::write(path, data)?;
        Ok(())
    }
}
