mod tree;

use winnow::{
    token::{rest, take},
    Parser, Result,
};
use winnow_test::{is_at_question_start, next_question};

pub fn prelude_parser<'s>(input: &mut &'s str) -> Result<&'s str> {
    if is_at_question_start(input) {
        return Ok("");
    }
    let next_question_start = next_question(input);
    match next_question_start {
        Some(indx) => take(indx).parse_next(input),
        None => rest.parse_next(input),
    }
}

pub fn question_parser<'s>(input: &mut &'s str) -> Result<&'s str> {
    let next_question_index = next_question(input);
    println!("parsing question: {} {:#?}", input, next_question_index);
    match next_question_index {
        Some(indx) => {
            let res = Ok(&input[0..indx]);
            *input = &input[indx..];
            res
        }
        None => rest.parse_next(input),
    }
}

fn main() {
    let input = "[Q1, displayif=\"bobo\"] la la la [1] ss [2] gg [Q2] la la la ... ";
    println!("at qstart: {}", is_at_question_start(input));
    let input = "<loop> [L1] this ...";
    println!("at qstart: {}", is_at_question_start(input));

    //println!("at qstart: {}", is_at_question_start(input));
    let mut input = "// lala this is a test...  [G1] this ...";
    let out = prelude_parser.parse_next(&mut input);
    println!("at qstart: {:?} >{}<", out, input);
    let mut input = "// lala this is a test...  [_G1] this ...";
    let out = prelude_parser.parse_next(&mut input);
    println!("at qstart: {:?} >{}<", out, input);
    let mut input = "[_G1] this ...";
    let out = question_parser.parse_next(&mut input);
    println!("pques: {:?} >{}<", out, input);

    let mut input = "<grid> [_G1] this ... [G2] more..";
    let out = question_parser.parse_next(&mut input);
    println!("parse quest: {:?} >{}<", out, input);
}
