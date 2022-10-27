use std::fmt;

#[derive(PartialEq, Eq, Clone, Copy, Ord, PartialOrd, Hash)]
pub struct Pos {
    pub line: u32,
    pub col: u32,
}

impl std::fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl Pos {
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

impl Default for Pos {
    fn default() -> Self {
        Pos { line: 1, col: 1 }
    }
}

#[derive(PartialEq, Eq, Clone, Ord, PartialOrd, Hash, Default)]
pub struct Loc {
    pub filename: Option<String>,
    pub pos: Pos,
}

impl Loc {
    pub fn new(filename: &str, line: u32, col: u32) -> Loc {
        Loc {
            filename: Some(filename.to_string()),
            pos: Pos { line, col },
        }
    }
}

impl std::fmt::Debug for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.filename {
            Some(file) => write!(f, "{}:", file),
            None => write!(f, ""),
        }?;
        write!(f, "{:?}", self.pos)
    }
}

impl Loc {
    pub fn next(&mut self, chars: &mut std::iter::Peekable<std::str::Chars>) {
        self.pos.next(chars);
    }
}
