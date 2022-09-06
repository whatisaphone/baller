#![warn(clippy::pedantic)]
#![allow(clippy::module_name_repetitions)]
#![cfg_attr(feature = "strict", deny(warnings))]

use crate::{build::Build, extract::Extract};
use clap::Parser;
use std::error::Error;

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
