#![allow(dead_code)]
#![allow(unused_macros)]
// Wrapper around string slice that makes debug output `{:?}` to print string same way as `{}`.
// Used in different `assert*!` macros in combination with `pretty_assertions` crate to make
// test failures to show nice diffs.
pub struct MultiPretty<T>(pub T);

impl<T, U: PartialEq<T>> PartialEq<MultiPretty<T>> for MultiPretty<U> {
    fn eq(&self, other: &MultiPretty<T>) -> bool {
        self.0 == other.0
    }
}

/// Make diff to display string as multi-line string
impl std::fmt::Debug for MultiPretty<&'_ str> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

impl std::fmt::Debug for MultiPretty<String> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

#[macro_export]
macro_rules! include_strs_impl {
    ($($line:expr)*) => {
        &[
            $(stringify!($line),)*
        ]
    }
}

#[macro_export]
macro_rules! include_strs {
    ($file:expr) => {
        $crate::include_strs_impl!(include!($file))
    };
}

#[macro_export]
macro_rules! assert_str_eq {
    ($left:expr, $right:expr) => {
        pretty_assertions::assert_eq!(
            $crate::more_pretty_assertions::MultiPretty($left.to_string()),
            $crate::more_pretty_assertions::MultiPretty($right.to_string())
        );
    };
}

pub fn assert_no_err<T: std::fmt::Debug, E: std::fmt::Debug>(res: Result<T, E>) -> Result<T, E> {
    res.map_err(|err| {
        eprintln!("{err:?}");
        err
    })
}

pub fn assert_eq_err<T: PartialEq + std::fmt::Debug, E: std::fmt::Debug>(
    res: Result<T, E>,
    rhs: T,
) -> Result<(), E> {
    assert_eq!(assert_no_err(res)?, rhs);
    Ok(())
}
