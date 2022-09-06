#![allow(dead_code)]
#![warn(clippy::pedantic)]
#![allow(
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::module_name_repetitions
)]
#![cfg_attr(feature = "strict", deny(warnings))]

pub use crate::{
    build::{build, FsEntry},
    extract::extract,
};

mod build;
mod extract;
mod script;
mod utils;
mod xor;
