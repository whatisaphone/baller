#![warn(clippy::pedantic, clippy::cargo)]
#![allow(
    clippy::match_on_vec_items,
    clippy::match_wildcard_for_single_variants,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::module_name_repetitions
)]
#![cfg_attr(feature = "strict", deny(warnings))]

pub use crate::{
    compiler::build_disk,
    config::Config,
    extract::extract2,
    extract_old::extract,
    index::{dump_index, read_index},
    raw_build::{raw_build, FsEntry},
};

#[macro_use]
mod macros;

mod blocks;
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
