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
    pub start: IndexIntoFile,
    pub length: SymbolLength,
}

impl Location {
    #[cfg(test)]
    pub fn dummy_for_test() -> Self {
        Self {
            start: 0,
            length: 0,
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

    pub fn from_path(path: &Path) -> Self {
        UserFacingLocation::new(path, 0, 0)
    }

    pub fn from(path: &Path, contents: &str, location: &Location) -> Self {
        // TODO(usability): Consider walking the module tree to get a fully qualified module name.
        let mut loc = UserFacingLocation::new(path, 1, 1);
        let mut contents = contents.chars().peekable();
        for _ in 0..location.start {
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
