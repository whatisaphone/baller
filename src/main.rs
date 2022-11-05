#![warn(clippy::pedantic, clippy::cargo)]
#![allow(
    clippy::match_on_vec_items,
    clippy::match_wildcard_for_single_variants,
    clippy::module_name_repetitions
)]
#![cfg_attr(feature = "strict", deny(warnings))]

use crate::{
    blocks::{push_disk_number, strip_disk_number, DiskNumber},
    collision::ExtractCollisionOptions,
    compiler::build_disk,
    config::Config,
    extract::extract2,
    extract_old::extract,
    index::{dump_index, read_index, Index},
    raw_build::{raw_build, FsEntry},
};
use clap::{Parser, Subcommand};
use std::{
    error::Error,
    fs,
    fs::File,
    mem,
    path::{Path, PathBuf},
};

#[macro_use]
mod macros;

mod blocks;
mod collision;
mod compiler;
mod config;
mod extract;
mod extract_old;
mod index;
mod raw_build;
mod script;
#[cfg(test)]
mod tests;
mod utils;
mod xor;

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
    Main::parse().run().unwrap();
}

/// A modding tool for Backyard Baseball 2001
#[derive(Parser)]
#[clap(disable_help_subcommand = true)]
enum Main {
    #[clap(subcommand)]
    Project(Project),
    #[clap(subcommand)]
    Collision(Collision),
    #[clap(hide = true)]
    ExtractOld(ExtractOld),
    RawBuild(RawBuild),
}

impl Main {
    fn run(self) -> Result<(), Box<dyn Error>> {
        match self {
            Self::Project(cmd) => cmd.run(),
            Self::Collision(cmd) => cmd.run(),
            Self::ExtractOld(cmd) => cmd.run(),
            Self::RawBuild(cmd) => cmd.run(),
        }
    }
}

/// Work with project files and compiled files - .he0/.(a)/.(b)
#[derive(Subcommand)]
enum Project {
    Extract(ProjectExtract),
    Update(ProjectUpdate),
}

impl Project {
    fn run(self) -> Result<(), Box<dyn Error>> {
        match self {
            Self::Extract(cmd) => cmd.run(),
            Self::Update(cmd) => cmd.run(),
        }
    }
}

/// Modify a compiled project in place
#[derive(Parser)]
struct ProjectUpdate {
    /// The project directory which contains project.txt
    #[clap(value_name = "INPUT")]
    input: PathBuf,
    /// The compiled index (.he0) to update
    #[clap(short, long, value_name = "OUTPUT")]
    output: String,
    /// The number of the disk to rebuild
    #[clap(short, long, value_name = "N")]
    disk_number: DiskNumber,
}

impl ProjectUpdate {
    fn run(self) -> Result<(), Box<dyn Error>> {
        build_disk(self.output, self.disk_number, fs_reader(&self.input))
    }
}

/// Build an isolated resource file outside of a project
#[derive(Parser)]
struct RawBuild {
    /// The directory containing raw block data
    #[clap(value_name = "INPUT")]
    input: PathBuf,
    /// The output file to write
    #[clap(short, long, value_name = "OUTPUT")]
    output: PathBuf,
}

impl RawBuild {
    fn run(self) -> Result<(), Box<dyn Error>> {
        let mut out = File::create(&self.output)?;

        raw_build(&mut out, fs_reader(&self.input))
    }
}

/// The deprecated version of the extract command.
///
/// This needs to stick around until --publish-scripts is ported.
#[derive(Parser)]
struct ExtractOld {
    input: PathBuf,
    #[clap(short, long)]
    output: PathBuf,
    #[clap(short, long = "config")]
    config_path: Option<PathBuf>,
    #[clap(long)]
    aside: bool,
    #[clap(long)]
    publish_scripts: bool,
}

impl ExtractOld {
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
        disk_number: DiskNumber,
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

/// Decompile a project
#[derive(Parser)]
struct ProjectExtract {
    /// The index file (.he0) to extract
    #[clap(value_name = "INPUT")]
    input: PathBuf,
    /// The directory in which to write the project
    #[clap(short, long, value_name = "OUTPUT")]
    output: PathBuf,
    /// Configuration for the script decompiler
    #[clap(short, long = "config", value_name = "CONFIG")]
    config_path: Option<PathBuf>,
    /// Emit numbers next to resolved names
    #[clap(long)]
    aside: bool,
}

impl ProjectExtract {
    fn run(mut self) -> Result<(), Box<dyn Error>> {
        let input = mem::take(&mut self.input)
            .into_os_string()
            .into_string()
            .unwrap();
        if !input.ends_with("he0") {
            return Err("input path must end with \"he0\"".into());
        }

        let config = self.load_config()?;

        if !self.output.exists() {
            fs::create_dir(&self.output)?;
        }

        extract2(input, config.as_ref(), &mut fs_writer(&self.output))?;
        Ok(())
    }

    fn load_config(&self) -> Result<Option<Config>, Box<dyn Error>> {
        let Some(path) = &self.config_path else { return Ok(None) };
        let mut config = Config::from_ini(&fs::read_to_string(path)?)?;
        config.aside = self.aside;
        Ok(Some(config))
    }
}

/// Work with collision files - .he9
#[derive(Parser)]
enum Collision {
    Extract(CollisionExtract),
}

impl Collision {
    fn run(self) -> Result<(), Box<dyn Error>> {
        match self {
            Self::Extract(cmd) => cmd.run(),
        }
    }
}

/// Extract collision data
#[derive(Parser)]
struct CollisionExtract {
    /// The collision file (.he9) to be extracted
    #[clap(value_name = "INPUT")]
    input: PathBuf,
    /// The directory in which to write the output
    #[clap(short, long, value_name = "OUTPUT")]
    output: PathBuf,
    /// Add debugging balls to the 3D model
    #[clap(long)]
    balls: bool,
    /// Add a debugging grid to the 3D model
    #[clap(long)]
    grid: bool,
}

impl CollisionExtract {
    fn run(self) -> Result<(), Box<dyn Error>> {
        if !self.output.exists() {
            fs::create_dir(&self.output)?;
        }

        collision::extract(&self.input, self.output, &ExtractCollisionOptions {
            balls: self.balls,
            grid: self.grid,
        })?;
        Ok(())
    }
}

fn fs_reader(root: &Path) -> impl Fn(&str) -> Result<FsEntry, Box<dyn Error>> + Copy + '_ {
    |path| {
        let path = root.join(path);
        let metadata = fs::metadata(&path)?;
        if metadata.is_dir() {
            let names: Result<Vec<_>, Box<dyn Error>> = fs::read_dir(&path)?
                .map(|e| Ok(e?.file_name().into_string().unwrap()))
                .collect();
            Ok(FsEntry::Dir(names?))
        } else {
            Ok(FsEntry::File(fs::read(path)?))
        }
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
