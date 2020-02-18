use std::fmt;

#[derive(PartialEq, Clone)]
pub struct Loc {
    pub filename: Option<String>,
    pub line: i32,
    pub col: i32,
}

impl std::fmt::Debug for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.filename {
            Some(file) => write!(f, "{} ", file),
            None => write!(f, ""),
        }?;
        write!(f, "at line {}, column {}", self.line, self.col)
    }
}

impl Loc {
    pub fn next(&mut self, ch: Option<&char>) {
        if ch == None {
        } else if ch == Some(&'\n') {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
    }
}

impl Default for Loc {
    fn default() -> Self {
        Loc {
            filename: None,
            line: 1,
            col: 1,
        }
    }
}
