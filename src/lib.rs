#![warn(clippy::pedantic)]
#![allow(
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::module_name_repetitions
)]
#![cfg_attr(feature = "strict", deny(warnings))]

pub use crate::{
    build::{build, FsEntry},
    config::Config,
    extract::{extract, read_index},
};

#[macro_use]
mod macros;

mod build;
mod config;
mod extract;
mod script;
mod utils;
mod xor;
