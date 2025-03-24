use std::cmp::min;
#[allow(unused_imports)]
use winnow::{
    ascii::{alphanumeric1, digit0, space0, space1, till_line_ending},
    combinator::{
        alt, delimited, eof, not, opt, preceded, repeat, repeat_till, separated, terminated,
    },
    error::{AddContext, ContextError, ErrMode, ParserError, StrContext, StrContextValue},
    stream::AsChar,
    stream::Stream,
    token::{any, literal, one_of, rest, take, take_till, take_until, take_while},
    Parser, Result,
};

#[allow(dead_code)]
fn char_substr<'i>(input: &'i str, i: usize) -> &'i str {
    let l = min(i, input.len());
    if let Some(x) = input.char_indices().nth(l) {
        let r = input.split_at(x.0);
        r.0
    } else {
        input
    }
}

#[allow(dead_code)]
fn qname<'s>(input: &mut &'s str) -> Result<&'s str> {
    let mut iter = input.char_indices();
    while let Some((index, char)) = iter.next() {
        if !char.is_uppercase() || char == '_' {
            if index == 0 {
                return Err(ContextError::new());
            }
            return Ok(&input[0..(index - 1)]);
        }
    }

    Ok(input)
}

#[allow(dead_code)]
fn cap<'s>(input: &mut &str) -> Result<char> {
    let fc = input.next_token();
    match fc {
        Some(ch) if ch.is_uppercase() => Ok(ch),
        _ => Err(ContextError::new()),
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
pub enum SkipType {
    Hard,
    Soft,
    None,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct QuestionTag {
    question_id: String,
    skip: SkipType,
    displayif: Option<String>,
}
impl QuestionTag {
    pub fn new(question_id: &str, c: Option<char>, displayif: Option<&str>) -> Self {
        let s = match c {
            Some(x) if x == '?' => SkipType::Soft,
            Some(x) if x == '!' => SkipType::Hard,
            None => SkipType::None,
            // this is an invalid state...
            Some(_) => SkipType::None,
        };
        let mut dif = None;
        if let Some(txt) = displayif {
            dif = Some(txt.to_owned())
        }
        QuestionTag {
            question_id: question_id.to_owned(),
            skip: s,
            displayif: dif,
        }
    }
}
#[derive(Debug, Hash, PartialEq, Eq)]
#[allow(dead_code)]
pub struct Question {
    tag: QuestionTag,
    markdown: String,
}
impl Question {
    pub fn new(tag: QuestionTag, markdown: &str) -> Self {
        Question {
            tag,
            markdown: markdown.to_owned(),
        }
    }
    pub fn old_new(markdown: &str) -> Self {
        Question {
            tag: QuestionTag::new("", None, None),
            markdown: markdown.to_owned(),
        }
    }
}
impl From<String> for Question {
    fn from(markdown: String) -> Self {
        Question {
            tag: QuestionTag::new("", None, None),
            markdown,
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

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Loop {
    loop_markdown: String,
    questions: Vec<Question>,
}
impl Loop {
    pub fn new(markdown: &str) -> Self {
        Loop {
            loop_markdown: markdown.to_owned(),
            questions: vec![],
        }
    }
    pub fn parsed(markdown: &str, loop_questions: Vec<Question>) -> Self {
        Loop {
            loop_markdown: markdown.to_owned(),
            questions: loop_questions,
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum ModuleItem {
    Question(Question),
    Grid(Grid),
    Loop(Loop),
}

pub fn module_item_parser<'s>(input: &mut &'s str) -> Result<ModuleItem> {
    debug_assert!(input.len() == 0 || is_at_question_start(input));
    match &input[0..5] {
        "<loop" => {
            println!("... I got a loop {}", &input[0..5]);
            let loop_item = loop_parser.parse_next(input)?;
            Ok(loop_item)
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
    Ok(ModuleItem::Question(Question::old_new(question_markdown)))
}
pub fn loop_parser<'s>(input: &mut &'s str) -> Result<ModuleItem> {
    let (loop_markdown, _) = (take_until(1.., "</loop>"), "</loop>").parse_next(input)?;
    let _: Result<&str, ContextError> = until_next_question_start(input);
    println!("======> {} <======= ===>{} <==", loop_markdown, input);
    Ok(ModuleItem::Loop(Loop::new(loop_markdown)))
}
pub fn grid_parser<'s>(input: &mut &'s str) -> Result<ModuleItem> {
    let mut x: Vec<&str> =
        preceded("|", repeat(4, terminated(take_until(1.., "|"), "|"))).parse_next(input)?;
    let _y: Result<&str, ContextError> = until_next_question_start(input);

    let grid_response_markdown = x.pop().unwrap_or("");
    let grid_question_markdown = x.pop().unwrap_or("");

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

fn is_at_question(end: bool, input: &str) -> bool {
    let mut iter = input.chars();
    let opt_char1 = iter.next();
    let opt_char2 = iter.next();
    match (opt_char1, opt_char2) {
        (Some(c1), Some(c2)) if c1 == '[' && c2.is_uppercase() || c2 == '_' => true,
        (Some(_), Some(_)) => {
            if input.len() < 6 || !input.is_char_boundary(5) {
                return false;
            }
            &input[0..5] == "<loop" || &input[0..5] == "|grid"
        }
        _ => end,
    }
}

pub fn is_at_question_start(input: &str) -> bool {
    is_at_question(false, input)
}
pub fn is_at_question_end(input: &str) -> bool {
    is_at_question(true, input)
}

pub fn is_valid_id_char(c: char) -> bool {
    c.is_uppercase() || c.is_numeric() || c == '_'
}
pub fn is_valid_id_start_char(c: char) -> bool {
    c.is_uppercase() || c == '_'
}
pub fn arg_name<'i>(input: &mut &'i str) -> Result<&'i str> {
    take_while(1.., |c: char| c.is_alphanum() || c == '_').parse_next(input)
}
pub fn arg_value<'i>(input: &mut &'i str) -> Result<&'i str> {
    let delimiter: Option<char> = opt(one_of(['\'', '"'])).parse_next(input)?;
    match delimiter {
        Some(c) => {
            let delimiter_literal = literal(c);
            terminated(take_while(1.., |v: char| v != c), delimiter_literal).parse_next(input)
        }
        None => {
            take_while(1.., |c: char| !c.is_whitespace() && c != ']' && c != '|').parse_next(input)
        }
    }
}

pub fn question_name<'i>(input: &mut &'i str) -> Result<&'i str> {
    let start = input.checkpoint();
    match any::<&str, ErrMode<ContextError>>.parse_next(input) {
        Ok(c) if is_valid_id_start_char(c) => {}
        Ok(_) => {
            //eprintln!("Expected a cap/_ received {}", c);
            input.reset(&start);
            let e = ContextError::from_input(input)
                .add_context(input, &start, StrContext::Label("Question Name Error"))
                .add_context(
                    input,
                    &start,
                    StrContext::Expected(StrContextValue::Description("a capital letter")),
                );
            return Err(e);
        }
        Err(e) => {
            e.add_context(
                input,
                &start,
                StrContext::Expected(StrContextValue::Description("Not a capital letter")),
            );
        }
    }
    input.reset(&start);

    take_while(1.., |c: char| is_valid_id_char(c)).parse_next(input)
}

// id="D_1012" or lala='ding dong'
pub fn args<'i>(input: &mut &'i str) -> Result<&'i str> {
    let original_input = *input;

    let r = (space0, arg_name, space0, '=', space0, arg_value, space0).parse_next(input);

    println!("{} {:?}", input, r);
    match r {
        Ok(_) => {
            let z = &original_input[..original_input.len() - input.len()];
            println!("ARG: {}", z);
            Ok(z)
        }
        Err(e) => {
            let z = &original_input[..original_input.len() - input.len()];
            println!("ERR: {}", z);
            Err(e)
        }
    }
}

// unlike other args, display if does not require quotes
pub fn question_displayif<'i>(input: &mut &'i str) -> Result<&'i str> {
    let (_, _, _, _, _, displayif) = (
        space0,
        "displayif",
        space0,
        '=',
        space0,
        take_while(1.., |l| l != ']'),
    )
        .parse_next(input)?;
    Ok(displayif)
}

pub fn question_tag<'i>(input: &mut &'i str) -> Result<QuestionTag> {
    let (question_id, skip_char, displayif) = (
        preceded('[', question_name),
        opt(one_of(['?', '!'])),
        terminated(opt(preceded(',', question_displayif)), "]"),
    )
        .parse_next(input)?;
    return Ok(QuestionTag::new(question_id, skip_char, displayif));
}

#[allow(dead_code)]
fn process_question<'s>(input: &mut &'s str, indx: usize) -> Result<&'s str> {
    if indx > 0 {
        return take(indx).parse_next(input);
    } else {
        return Err(ParserError::from_input(input));
    }
}

pub fn question_end<'s>(input: &mut &'s str) -> Result<bool> {
    let mut iter = input.chars();
    let c1 = iter.next();

    match c1 {
        Some('[') => {
            if let Some(c) = iter.next() {
                if is_valid_id_start_char(c) {
                    return Ok(true);
                }
            }
            Err(ParserError::from_input(input))
        }
        Some(c) if c == '|' || c == '<' => {
            let check = &input[1..5];
            if check == "grid" || check == "loop" {
                return Ok(true);
            }
            Err(ParserError::from_input(input))
        }
        Some(_) => Err(ParserError::from_input(input)),
        None => Ok(true),
    }
}

pub fn input_text<'s>(input: &mut &'s str) -> Result<String> {
    let result: Result<(&str, (), Option<&str>), ContextError> =
        ("|__|", not('_'), opt(terminated(args, "|"))).parse_next(input);

    match result {
        Ok((_, _, optional_arg)) => {
            let arg = optional_arg.unwrap_or("");
            Ok(format!("<input type=\"text\" {}>", arg))
        }
        Err(e) => Err(e),
    }
}

pub fn input_radio<'s>(input: &mut &'s str) -> Result<String> {
    if is_at_question_start(input) {
        return Err(ParserError::from_input(input));
    }

    let res: Result<(&str, Option<char>), ContextError> =
        delimited('[', (digit0, opt('*')), ']').parse_next(input);

    if let Ok((value, reset)) = res {
        let reset_value = reset.map(|_| "data-reset=\"true\"").unwrap_or("");
        Ok(format!(
            "<input type=\"radio\" value=\"{}\" {}>",
            value, reset_value
        ))
    } else {
        Err(ParserError::from_input(input))
    }
}
pub fn input_checkbox<'s>(input: &mut &'s str) -> Result<String> {
    let res = delimited('(', digit0, ')').parse_next(input);
    match res {
        Ok(value) => Ok(format!("<input type=\"radio\" value=\"{}\">", value)),
        Err(e) => Err(e),
    }
}

pub fn question_markdown<'s>(input: &mut &'s str) -> Result<String> {
    let x: Result<(Vec<String>, bool)> = repeat_till(
        1..,
        alt((
            input_radio,
            input_text,
            popup,
            any.map(|c: char| c.to_string()),
        )),
        question_end,
    )
    .parse_next(input);

    if let Ok((strings, _)) = x {
        return strings
            .into_iter()
            .reduce(|acc, s| acc + &s)
            .ok_or(ParserError::from_input(input));
    }
    Err(ParserError::from_input(input))
}

#[derive(Debug)]
struct Popup {
    button_text: String,
    title: Option<String>,
    popup_text: String,
}

impl Popup {
    fn from_vec<T>(mut parts: Vec<T>) -> Result<Popup, &'static str>
    where
        T: Into<String>,
    {
        match parts.len() {
            2 => Ok(Popup {
                button_text: parts.remove(0).into(),
                title: None,
                popup_text: parts.remove(0).into(),
            }),
            3 => Ok(Popup {
                button_text: parts.remove(0).into(),
                title: Some(parts.remove(0).into()),
                popup_text: parts.remove(0).into(),
            }),
            _ => Err("Popup require a vector of length two or three"),
        }
    }
}
impl From<Popup> for String {
    fn from(item: Popup) -> Self {
        format!("<a tabindex=\"0\" class=\"popover-dismiss btn\" role=\"button\" data-toggle=\"popover\" data-bs-toggle=\"popover\" data-trigger=\"focus\" data-bs-trigger=\"focus\" data-content=\"{}\" data-bs-content=\"{}\" data-bs-original-title=\"{}\">{}</a>",
&item.popup_text,&item.popup_text,&item.title.unwrap_or("".to_string()),&item.button_text)
    }
}

/*
<a tabindex="0" class="popover-dismiss btn" role="button" data-toggle="popover" data-bs-toggle="popover" data-trigger="focus" data-bs-trigger="focus" data-content="this is the info text" data-bs-content="this is the info text" data-bs-original-title="This is the title">info text</a>
<a tabindex="0" class="popover-dismiss btn" role="button" title="" data-toggle="popover" data-bs-toggle="popover" data-trigger="focus" data-bs-trigger="focus" data-content="this is the info text with no title" data-bs-content="this is the info text with no title">info text</a>
 */
fn popup<'i>(input: &mut &'i str) -> Result<String> {
    let _: Result<&str> = "|popup|".parse_next(input);
    let res: Result<Vec<&str>> = repeat(
        2..=3,
        terminated(take_while(1.., |c: char| c != '|' && c != '\n'), '|'),
    )
    .parse_next(input);

    if let Ok(parts) = res {
        let p = Popup::from_vec(parts).map_err(|_| ContextError::from_input(input));
        return p.map(|x: Popup| x.into());
    }
    Err(ParserError::from_input(input))
}

pub fn comment<'i>(input: &mut &'i str) -> Result<&'i str> {
    preceded("//", till_line_ending).parse_next(input)
}
pub fn til_comment<'i>(input: &mut &'i str) -> Result<&'i str> {
    take_till(1.., ['/', '[']).parse_next(input)
}
pub fn parse_preable<'i>(input: &mut &'i str) -> Result<String> {
    repeat_till(0.., alt((comment, til_comment)), question_end)
        .parse_next(input)
        .map(|r: (String, bool)| r.0)
}
pub fn question<'i>(input: &mut &'i str) -> Result<Question> {
    let res = (question_tag, question_markdown).parse_next(input);
    if let Ok((question_tag, markdown)) = res {
        println!("...question: {}\n{}", &question_tag.question_id, &markdown);
        Ok(Question::new(question_tag, &markdown))
    } else {
        eprintln!("... Error at {}", input);
        Err(res.expect_err("Error parsing markdown"))
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn l2() {
        let mut input = "[hello";
        let x = question_tag(&mut input);
        println!("{:?} {}", x, input);

        input = "[Hello";
        let x = question_tag(&mut input);
        println!("{:?} {}", x, input);

        input = "[12Hello";
        let x = question_tag(&mut input);
        println!("{:?} {}", x, input);

        input = "[Q1_1232]";
        let x = question_tag(&mut input);
        println!("{:?} {}", x, input);
        input = "[Q1_1232?]";
        let x = question_tag(&mut input);
        println!("{:?} {}", x, input);
        input = "[Q1_1232!]";
        let x = question_tag(&mut input);
        println!("{:?} {}", x, input);
    }

    #[test]
    fn t1() {
        let mut input = r"[D_797626610?,displayif=equals(D_384191091,412790539)] Which of these describes you best? Select all that apply.
[664571574] Dutch
[163149180] English
[192776753] European
[733789220] French
[418464677] German
[859329001] Irish
[267472307] Italian
[918190932] Norwegian
[381749264] Polish
[704661219] Scottish
[773342525] Spanish
[533527231] Swedish
[807835037] None of these fully describe me: Please describe |__|id=D_774928994|
[178420302*] Don't know
[746038746*] Prefer not to answer

[END] all done";
        let q = question(&mut input);
        println!("{:?}", q);
        assert!(q.is_ok());
        let q = q.unwrap();
        assert_eq!(q.tag.skip, SkipType::Soft);
        println!("\n================\n{}\n================\\n", q.markdown);

        let q2 = question(&mut input);
        assert!(q2.is_ok());
        let q2 = q2.unwrap();
        assert_eq!(q2.tag.skip, SkipType::None);
        println!("{:?}", q2);
    }

    #[test]
    fn arg_test() {
        let mut input = "id = 4";
        let r = args.parse_next(&mut input);
        assert!(r.is_ok());
        assert_eq!(r, Ok("id = 4"));

        let mut input = "id=D_774928994|";
        let r = args.parse_next(&mut input);
        assert!(r.is_ok());
        assert_eq!(r, Ok("id=D_774928994"));
        assert_eq!(input, "|")
    }
    #[test]
    fn pretest() {
        let mut input = r"// lala
        // hi hi [A1] ll
        [Q1] this is a question
        ";
        let r = parse_preable(&mut input);
        println!("{:?}\ninput\n{}", r, input);
    }
    #[test]
    fn test_start() {
        assert_eq!(is_at_question_start("[Q1] yes "), true);
        assert_eq!(is_at_question_start("// [Q1] yes "), false);
        assert_eq!(is_at_question_end("[Q1] yes "), true);
        assert_eq!(is_at_question_end(""), true);
    }

    #[test]
    fn popup_test() {
        let mut input: &str = "|popup|button text|title|popup text|";
        let res = popup.parse_next(&mut input);
        assert!(res.is_ok());
        //        let res = res.unwrap();
        //        assert_eq!(res.button_text, "button text");
        //        assert_eq!(res.title, Some("title".to_string()));
        //        assert_eq!(res.popup_text, "popup text");

        //        input = "|popup|button text|popup text|";
        //        let res = popup.parse_next(&mut input);
        //        assert!(res.is_ok());
        //        let res = res.unwrap();
        //        assert_eq!(res.button_text, "button text");
        //        assert_eq!(res.title, None);
        //        assert_eq!(res.popup_text, "popup text");
    }
}
