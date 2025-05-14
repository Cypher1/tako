use std::{
    fmt,
    path::{Path, PathBuf},
};

pub type IndexIntoFile = u16;
pub type SymbolLength = u8;
// This is chosen as it's large enough to index the whole of
// gcc as a single '.c' file at a per-byte level,
// and `u16` is too small.
// Source: https://people.csail.mit.edu/smcc/projects/single-file-programs

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Location {
    // These are byte indexes and byte lengths. They may need to be interpreted before being shown
    // to the user.
    pub start: IndexIntoFile,
    pub length: SymbolLength,
}

impl Location {
    #[cfg(test)]
    #[must_use]
    pub fn dummy_for_test() -> Self {
        Self {
            start: 0,
            length: 0,
        }
    }

    pub fn from_range(start: IndexIntoFile, end: IndexIntoFile) -> Self {
        // Todo: start<end
        let start = std::cmp::min(start, end);
        let length = (std::cmp::max(start, end) - start).try_into().unwrap();
        Self {
            start,
            length
        }
    }

    pub fn end(&self) -> IndexIntoFile {
        self.start+(self.length as u16)
    }

    pub fn merge(self, other: Self) -> Self {
        let start = std::cmp::min(self.start, other.start);
        let end = std::cmp::max(self.end(), other.end());
        Self {
            start,
            length: (end-start) as u8,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Ord, PartialOrd)]
pub struct UserFacingLocation {
    pub filename: PathBuf,
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
        write!(f, "{}", self.filename.display())?;
        if self.line != 0 || self.col != 0 {
            write!(f, ":{}:{}", self.line, self.col)?;
        }
        Ok(())
    }
}

impl UserFacingLocation {
    fn new(filename: &Path, line: u32, col: u32) -> Self {
        Self {
            filename: filename.to_path_buf(),
            line,
            col,
        }
    }

    #[must_use]
    pub fn from_path(path: &Path) -> Self {
        Self::new(path, 0, 0)
    }

    #[must_use]
    pub fn from(path: &Path, contents: &str, location: &Location) -> Self {
        // TODO(usability): Consider walking the module tree to get a fully qualified module name.
        let mut loc = Self::new(path, 1, 1);
        let mut contents = contents.chars().peekable();
        for _ in 0..location.start {
            loc.next(&mut contents);
        }
        loc
    }

    pub fn next(&mut self, chars: &mut std::iter::Peekable<std::str::Chars<'_>>) {
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
