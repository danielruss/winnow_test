use reqwest::{get, Error};
use winnow_test::{parse_preable, question};

#[tokio::main]
async fn main() -> Result<(), Error> {
    let contents = get("https://raw.githubusercontent.com/episphere/questionnaire/refs/heads/main/prod/module1.txt")
    .await?
    .text()
    .await?;
    let mut input = contents.as_str();

    let preamble = parse_preable(&mut input).unwrap_or("".to_string());
    let q1 = question(&mut input);
    let q2 = question(&mut input);
    let q3 = question(&mut input);
    let q4 = question(&mut input);
    let q5 = question(&mut input);
    let q6 = question(&mut input);

    println!("=Preamble====\n{}\n=============\n", preamble);
    println!(
        "Preamble len={}\n===========\n{:?}\n===========\n{:?}\n===========\n{:?}\n===========\n{:?}\n===========\n{:?}\n===========\n{:?}\n===========input:\n{}",
        preamble.len(),
        q1,
        q2,
        q3,
        q4,
        q5,
        q6,
        &input[0..400]
    );
    Ok(())
}
