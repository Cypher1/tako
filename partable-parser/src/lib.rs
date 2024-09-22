// use static_assertions::*;

#[derive(Clone, Copy, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub enum Direction {
    I = 0b000, // Ignore

    L = 0b010, // Left
    R = 0b001, // Right
    B = 0b011, // Either

    M = 0b100, // Merge (merge the two tokens together)
    C = 0b101, // Close (i.e. return first child)
}

pub trait Symbol<const SIZE: usize>: Copy + Clone + std::hash::Hash + PartialEq + PartialOrd + Eq + Ord {
    const SIZE: usize = SIZE;

    const TABLE: DirTable< SIZE >;
}

pub struct DirTable<const N: usize> {
    table: [[Direction; N]; N]
}

impl <const N: usize> DirTable<N> {
    pub fn dir<S: Symbol<N> + Into<usize>>(&self, left: S, right: S) -> Direction {
        self.table[left.into()][right.into()]
    }
}

pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;
    // mod simple;
    // mod bottom_up_merging;
    // mod tables;
    mod table;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
