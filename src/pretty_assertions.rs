use crate::errors::TError;

// Wrapper around string slice that makes debug output `{:?}` to print string same way as `{}`.
// Used in different `assert*!` macros in combination with `pretty_assertions` crate to make
// test failures to show nice diffs.
#[derive(PartialEq, Eq)]
pub struct MultiPretty<T>(pub T);

/// Make diff to display string as multi-line string
impl<'a> std::fmt::Debug for MultiPretty<&'a str> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

impl std::fmt::Debug for MultiPretty<String> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

macro_rules! assert_str_eq {
    ($left:expr, $right:expr) => {
        pretty_assertions::assert_eq!(
            crate::pretty_assertions::MultiPretty($left),
            crate::pretty_assertions::MultiPretty($right.to_string())
        );
    };
}

pub fn assert_no_err<T: std::fmt::Debug, E: std::fmt::Display>(
    res: Result<T, E>,
) -> Result<T, TError>
where
    TError: From<E>,
{
    Ok(res.map_err(|err| {
        eprintln!("{0}", &err);
        err
    })?)
}

pub fn assert_eq_err<T: PartialEq + std::fmt::Debug, E: std::fmt::Display>(
    res: Result<T, E>,
    rhs: T,
) -> Result<(), TError>
where
    TError: From<E>,
{
    assert_eq!(assert_no_err(res)?, rhs);
    Ok(())
}
