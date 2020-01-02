#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct Loc {
    filename: Option<String>,
    line: i32,
    col: i32,
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
        Loc {filename: None, line: 1, col: 1}
    }
}
