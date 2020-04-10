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
    pub fn next(&mut self, mut chars: &mut std::iter::Peekable<std::str::Chars>) {
        if chars.peek() == None {
        } else if chars.peek() == Some(&'\n') {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        chars.next();
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
