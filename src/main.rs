#![warn(clippy::pedantic)]
#![allow(clippy::module_name_repetitions)]
#![cfg_attr(feature = "strict", deny(warnings))]

use crate::extract::Extract;
use clap::Parser;
use std::error::Error;

mod extract;
mod script;
mod utils;
mod xor;

#[derive(Parser)]
enum Command {
    Extract(Extract),
}

fn main() -> Result<(), Box<dyn Error>> {
    match Command::parse() {
        Command::Extract(cmd) => cmd.run(),
    }
}
