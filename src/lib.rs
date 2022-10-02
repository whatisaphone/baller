#![warn(clippy::pedantic)]
#![allow(
    clippy::match_on_vec_items,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::module_name_repetitions,
    clippy::never_loop,
    clippy::single_match,
    clippy::single_match_else
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
#[cfg(test)]
mod tests;
mod utils;
mod xor;
