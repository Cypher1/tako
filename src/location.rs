use crate::concepts::File;
use soa_derive::StructOfArray;
use std::fmt;

type IndexIntoFile = u32;
// This is chosen as it's large enough to index the whole of
// gcc as a single '.c' file at a per-byte level,
// and `u16` is too small.
// Source: https://people.csail.mit.edu/smcc/projects/single-file-programs

#[derive(StructOfArray, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Location {
    location: IndexIntoFile,
    length: IndexIntoFile,
}

#[derive(PartialEq, Eq, Clone, Ord, PartialOrd)]
pub struct UserFacingLocation {
    pub filename: String,
    pub line: u32,
    pub col: u32,
}

impl std::fmt::Display for UserFacingLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as std::fmt::Debug>::fmt(self, f)
    }
}

impl std::fmt::Debug for UserFacingLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.filename, self.line, self.col)
    }
}

impl UserFacingLocation {
    fn new(filename: &str, line: u32, col: u32) -> Self {
        Self {
            filename: filename.to_string(),
            line,
            col,
        }
    }

    pub fn from(file: &File, location: &Location) -> Self {
        // TODO: Consider walking the module tree to get a fully qualified module name.
        let mut loc = UserFacingLocation::new(&file.path, 1, 1);
        let mut contents = file.contents.chars().peekable();
        for _ in 0..location.location {
            loc.next(&mut contents);
        }
        loc
    }

    pub fn next(&mut self, chars: &mut std::iter::Peekable<std::str::Chars>) {
        let ch = chars.peek();
        if ch.is_none() {
            return;
        }
        let nl = ch == Some(&'\n');
        let lf = ch == Some(&'\r');
        chars.next();
        self.col += 1;
        if nl || lf {
            if lf && chars.peek() == Some(&'\n') {
                chars.next();
            }
            self.line += 1;
            self.col = 1;
        }
    }
}
