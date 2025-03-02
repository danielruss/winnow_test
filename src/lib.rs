use winnow::{
    combinator::{eof, preceded, repeat, repeat_till, terminated},
    error::ContextError,
    token::{rest, take, take_until},
    Parser, Result,
};

pub fn is_at_question_start(input: &str) -> bool {
    let mut iter = input.chars();
    let opt_char1 = iter.next();
    let opt_char2 = iter.next();
    match (opt_char1, opt_char2) {
        (Some(c1), Some(c2)) if c1 == '[' && c2.is_uppercase() => true,
        (_, _) => {
            if input.len() < 6 {
                return false;
            }
            &input[0..5] == "<loop" || &input[0..5] == "|grid"
        }
    }
}

pub fn next_question(input: &str) -> Option<usize> {
    let mut iter = input.char_indices();
    // we dont want to return 0.
    iter.next();
    while let Some((indx1, _)) = iter.next() {
        let slice = &input[indx1..];
        if is_at_question_start(slice) {
            return Some(indx1);
        }
    }
    None
}

pub fn until_next_question_start<'s>(input: &mut &'s str) -> Result<&'s str> {
    let next_question_start = next_question(input);
    match next_question_start {
        Some(indx) => take(indx).parse_next(input),
        None => rest.parse_next(input),
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Module {
    preamble: Option<String>,
    items: Vec<ModuleItem>,
}
impl Module {
    pub fn create() -> Self {
        Module {
            preamble: None,
            items: vec![],
        }
    }

    pub fn preamble(&mut self, preamble: &str) {
        self.preamble = Some(preamble.to_owned())
    }
    pub fn add_module_item(&mut self, item: ModuleItem) {
        self.items.push(item);
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
#[allow(dead_code)]
pub struct Question {
    markdown: String,
}
impl Question {
    pub fn new(markdown: &str) -> Self {
        Question {
            markdown: markdown.to_owned(),
        }
    }
}
#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Grid {
    question_markdown: Vec<String>,
    response_markdown: Vec<String>,
}

impl Grid {
    pub fn new() -> Self {
        Grid {
            question_markdown: vec![],
            response_markdown: vec![],
        }
    }
    pub fn add_question(&mut self, markdown: &str) {
        self.question_markdown.push(markdown.trim().to_owned());
    }
    pub fn add_response(&mut self, markdown: &str) {
        self.response_markdown.push(markdown.trim().to_owned());
    }
}

impl From<String> for Question {
    fn from(markdown: String) -> Self {
        Question { markdown }
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum ModuleItem {
    Question(Question),
    Grid(Grid),
}

pub fn module_item_parser<'s>(input: &mut &'s str) -> Result<ModuleItem> {
    debug_assert!(input.len() == 0 || is_at_question_start(input));
    match &input[0..5] {
        "<loop" => {
            println!("... I got a loop {}", &input[0..5]);
            todo!();
        }
        "|grid" => {
            println!("... I got a grid {}", &input[0..5]);
            let grid = grid_parser.parse_next(input)?;
            Ok(grid)
        }
        _ => {
            println!("... I got a question");
            let question = question_parser.parse_next(input)?;
            Ok(question)
        }
    }
}

pub fn question_parser<'s>(input: &mut &'s str) -> Result<ModuleItem> {
    let question_markdown = until_next_question_start(input)?;
    Ok(ModuleItem::Question(Question::new(question_markdown)))
}
pub fn grid_parser<'s>(input: &mut &'s str) -> Result<ModuleItem> {
    let mut x: Vec<&str> =
        preceded("|", repeat(4, terminated(take_until(1.., "|"), "|"))).parse_next(input)?;
    let _y: Result<&str, ContextError> = until_next_question_start(input);

    let grid_response_markdown = x.pop().unwrap_or("");
    let grid_question_markdown = x.pop().unwrap_or("");

    /*
    if !is_at_question_start(grid_question_markdown) {
        until_next_question_start.parse_next(&mut grid_question_markdown)?;
    }
    */

    let mut grid = Grid::new();

    grid_question_markdown
        .split(";")
        .filter(|q| q.len() > 0)
        .for_each(|q| grid.add_question(q));
    grid_response_markdown
        .split(";")
        .filter(|r| r.len() > 0)
        .for_each(|r| grid.add_response(r));

    println!("========> grid.grid:\n{:?}", grid);
    println!("<========");
    Ok(ModuleItem::Grid(grid))
}

pub fn module_parser<'s>(input: &mut &'s str) -> Result<Module> {
    // handle the prelude...
    let mut module = Module::create();

    if !is_at_question_start(input) {
        let preamble_res = until_next_question_start.parse_next(input)?;
        module.preamble(preamble_res);
    }

    let vec_question_res: Result<(Vec<ModuleItem>, &str), ContextError> =
        repeat_till(0.., module_item_parser, eof).parse_next(input);
    assert!(vec_question_res.is_ok());
    if let Ok((question_vector, _)) = vec_question_res {
        question_vector
            .into_iter()
            .for_each(|q| module.add_module_item(q));
    }

    println!("{:?}", module);
    Ok(module)
}

#[cfg(test)]
mod tests {

    use winnow::Parser;

    #[allow(unused_imports)]
    use super::*;

    const MODULE_TEXT: &str = r"// this is in the prelude
[INTRO] hello
[1] I like candy
[2] My dog is upstairs

[Y] hello again
(1) ping pong
(2) snow joe

[END] So long and thanks for all the fish
";

    #[test]
    pub fn test1() {
        let input = MODULE_TEXT.to_owned();
        let res_mod = module_parser.parse_next(&mut input.as_str());
        println!("{:?}", res_mod);
    }

    #[test]
    pub fn test_make_question() {
        let md = "hello there";
        let q = ModuleItem::Question(Question::new(md));
        match q {
            ModuleItem::Question(v) => assert_eq!(v.markdown, md),
            ModuleItem::Grid(_) => assert!(false),
        }
        let q = ModuleItem::Question(Question::from(md.to_owned()));
        match q {
            ModuleItem::Question(v) => assert_eq!(v.markdown, md),
            ModuleItem::Grid(_) => assert!(false),
        }
    }

    #[test]
    pub fn test_read_module() {
        let markdown = std::fs::read_to_string("test_module.txt").unwrap();

        let module_res: Result<Module, String> = match module_parser(&mut markdown.as_str()) {
            Err(e) => Err(e.to_string()),
            Ok(module) => Ok(module),
        };

        if let Ok(module) = module_res {
            println!("{:?}", module)
        };
    }
}
