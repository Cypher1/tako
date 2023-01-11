use std::fmt;

#[derive(PartialEq, Eq, Clone, PartialOrd, Ord, Hash, Copy, Debug)]
pub enum Tribool {
    True,
    False,
    Unknown,
}

impl fmt::Display for Tribool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as std::fmt::Debug>::fmt(self, f)
    }
}

use Tribool::{False, True, Unknown};
impl Tribool {
    #[must_use]
    pub fn not(&self) -> Self {
        match self {
            True => False,
            False => True,
            Unknown => Unknown,
        }
    }

    #[must_use]
    pub fn or(&self, other: &Self) -> Self {
        match (self, other) {
            (True, _) | (_, True) => True,
            (Unknown, _) | (_, Unknown) => Unknown,
            (False, False) => False,
        }
    }

    #[must_use]
    pub fn and(&self, other: &Self) -> Self {
        match (self, other) {
            (False, _) | (_, False) => False,
            (Unknown, _) | (_, Unknown) => Unknown,
            (True, True) => True,
        }
    }

    #[must_use]
    pub fn is_true(&self) -> bool {
        self == &True
    }
    #[must_use]
    pub fn maybe_true(&self) -> bool {
        self.is_true() || self.is_unknown()
    }
    #[must_use]
    pub fn is_false(&self) -> bool {
        self == &False
    }
    #[must_use]
    pub fn maybe_false(&self) -> bool {
        self.is_false() || self.is_unknown()
    }
    #[must_use]
    pub fn is_unknown(&self) -> bool {
        self == &Unknown
    }
}

pub fn any_true<I>(vals: I) -> Tribool
where
    I: Iterator<Item = Tribool>,
{
    let mut st = False;
    for i in vals {
        st = st.or(&i);
    }
    st
}

pub fn all_true<I>(vals: I) -> Tribool
where
    I: Iterator<Item = Tribool>,
{
    let mut st = True;
    for i in vals {
        st = st.and(&i);
    }
    st
}
