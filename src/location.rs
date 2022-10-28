use std::fmt;
use crate::concepts::File;
use soa_derive::StructOfArray;

type IndexIntoFile = usize;

#[derive(StructOfArray, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[soa_attr(Vec, cfg_attr(test, derive(Debug)))]
pub struct Location {
    location: IndexIntoFile,
    file_id: FileId,
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
            filename: Some(filename.to_string()),
            line,
            col,
        }
    }

    pub fn from(file: &File, location: &Location) -> Self {
        let mut loc = UserFacingLocation::new(file.path, 1, 1);
        let mut contents = &file.contents;
        for _ in 0..location.location {
            loc.next(file.contents);
        }
        loc
    }

    pub fn next(&mut self, chars: &mut std::iter::Peekable<std::str::Chars>) {
        // TODO: Consider just keeping the offsets and then recovering line
        // info later.
        let ch = chars.peek();
        if ch == None {
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
