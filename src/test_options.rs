use std::num::ParseIntError;
use std::str::FromStr;

use super::cli_options::parse_args;
use super::cli_options::Options;

#[derive(Debug, PartialEq)]
pub enum TestResult {
    Panic,
    Success, // With an unspecified value
    Output(String), // With an expected value
             // TODO: ReturnValue(i32),
}

impl FromStr for TestResult {
    type Err = ParseIntError;

    fn from_str(res_: &str) -> Result<Self, Self::Err> {
        let res: Vec<&str> = res_.trim_matches(')').split('(').collect();
        if res[0] == "Panic" {
            return Ok(TestResult::Panic);
        }
        if res[0] == "Success" {
            return Ok(TestResult::Success);
        }
        if res[0] == "Output" {
            return Ok(TestResult::Output(res[1].to_owned()));
        }
        panic!(
            "Unexpected value in test configuration for expected test result: \"{}\".",
            res_
        );
        // let arg = res.strip_prefix("ReturnValue(").expect("Unexpected test result value.");
        // let arg_as_i32 = arg.parse::<i32>()?;
        // Ok(TestResult::ReturnValue(arg_as_i32))
    }
}

#[derive(Debug, PartialEq)]
pub struct TestOptions {
    pub expected: TestResult,
    pub opts: Options,
}

impl FromStr for TestOptions {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let args: Vec<&str> = s.lines().collect();
        let opts = parse_args(args[1..].to_vec());
        let expected = TestResult::from_str(&args[0])?;
        Ok(TestOptions { expected, opts })
    }
}
