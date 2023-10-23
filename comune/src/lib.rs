#![allow(unused_imports)]

#![allow(incomplete_features)]
#![feature(adt_const_params)]
#![feature(return_position_impl_trait_in_trait)]
#![feature(never_type)]
#![feature(generic_const_exprs)]
#![feature(assert_matches)]

pub mod lexer;
pub mod parser;
pub mod compiler;
mod try_as;
