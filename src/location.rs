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
    pub fn next(&mut self, chars: &mut std::iter::Peekable<std::str::Chars>) {
        // TODO: Consider just keeping the offsets and then recovering line
        // info later.
        if chars.peek() == None {
            return;
        }

        let nl = chars.peek() == Some(&'\n');
        let lf = chars.peek() == Some(&'\r');
        chars.next();
        if nl || lf {
            if lf && chars.peek() == Some(&'\n') {
                chars.next();
            }
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
