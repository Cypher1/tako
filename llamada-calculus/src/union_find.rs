use std::collections::HashMap;

pub trait Relative: Clone + Default {
    fn flip(self) -> Self {
        self
    }
    fn combine(self, other: Self) -> Self;
}

impl Relative for () {
    fn combine(self, _other: Self) -> Self {}
}

type Pair<X> = (X, X);

#[derive(Debug)]
pub struct Nodes<ID, Relation = ()> {
    parent: HashMap<ID, (ID, Relation)>,
}

impl<ID: std::hash::Hash + Eq, Relation> Default for Nodes<ID, Relation> {
    fn default() -> Self {
        Self {
            parent: HashMap::default(),
        }
    }
}
impl<ID: Copy + std::hash::Hash + Eq + Ord + PartialOrd, Relation: Relative> Nodes<ID, Relation> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_group(&mut self, id: ID) -> ID {
        self.describe(id).0
    }
    pub fn describe(&mut self, id: ID) -> (ID, Relation) {
        // TODO: Look up a better union find algo.
        let mut curr = id;
        let mut curr_rel = Relation::default();
        loop {
            let (parent, parent_rel) = self.get_parent_and_relation(curr);
            if parent == curr {
                return (parent, curr_rel);
            }
            let (grand_parent, grand_parent_rel) = self.get_parent_and_relation(parent);
            self.parent.insert(
                curr,
                (grand_parent, parent_rel.clone().combine(grand_parent_rel)),
            );
            curr = parent;
            curr_rel = curr_rel.combine(parent_rel);
        }
    }

    pub fn describe_const(&self, id: ID) -> (ID, Relation) {
        let mut curr = id;
        let mut curr_rel = Relation::default();
        loop {
            let (parent, parent_rel) = self.get_parent_and_relation(curr);
            if parent == curr {
                return (parent, curr_rel);
            }
            curr_rel = curr_rel.combine(parent_rel);
            curr = parent;
        }
    }

    fn get_parent_and_relation(&self, id: ID) -> (ID, Relation) {
        self.parent
            .get(&id)
            .cloned()
            .unwrap_or_else(|| (id, Relation::default()))
    }

    pub fn union(&mut self, a: ID, b: ID) -> ID {
        self.add_relation(a, b, Relation::default())
    }

    pub fn add_relation(&mut self, a: ID, b: ID, mut a_to_b_rel: Relation) -> ID {
        let (mut pa, mut a_to_pa_rel) = self.describe(a);
        let (mut pb, mut b_to_pb_rel) = self.describe(b);
        if pa != pb {
            if pa < pb {
                std::mem::swap(&mut pa, &mut pb);
                std::mem::swap(&mut a_to_pa_rel, &mut b_to_pb_rel);
                a_to_b_rel = a_to_b_rel.flip();
            }
            let pa_to_pb_rel = a_to_b_rel.combine(a_to_pa_rel.flip()).combine(b_to_pb_rel);
            self.parent.insert(pa, (pb, pa_to_pb_rel));
        }
        a
    }

    pub fn get_relation(&mut self, a: ID, b: ID) -> Result<(ID, Relation), Pair<(ID, Relation)>> {
        let (ga, a_rel) = self.describe(a);
        let (gb, b_rel) = self.describe(b);
        if ga != gb {
            return Err(((ga, a_rel), (gb, b_rel)));
        }
        let rel = b_rel.flip().combine(a_rel);
        Ok((ga, rel))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    impl<
            ID: Copy + Ord + PartialOrd + std::hash::Hash + Eq,
            Relation: Clone + Default + Relative,
        > Nodes<ID, Relation>
    {
        fn get_parent(&self, id: ID) -> ID {
            self.get_parent_and_relation(id).0 // TODO: Optimise?
        }
    }

    #[test]
    fn self_parents() {
        let mut nodes: Nodes<i32> = Nodes::new();
        let a = 1;

        assert_eq!(nodes.get_parent(a), a);
        nodes.union(a, a);
        assert_eq!(nodes.get_parent(a), a);
    }

    #[test]
    fn parent_a_b() {
        let mut nodes: Nodes<i32> = Nodes::new();
        let a = 1;
        let b = 2;

        assert_eq!(nodes.get_parent(a), a);
        assert_eq!(nodes.get_parent(b), b);
        nodes.union(a, b);
        assert_eq!(nodes.get_parent(a), nodes.get_parent(b));
    }

    #[test]
    fn parent_b_a() {
        let mut nodes: Nodes<i32> = Nodes::new();
        let a = 1;
        let b = 2;

        assert_eq!(nodes.get_parent(a), a);
        assert_eq!(nodes.get_parent(b), b);
        nodes.union(b, a);
        assert_eq!(nodes.get_parent(a), nodes.get_parent(b));
    }

    #[test]
    fn parent_many() {
        let mut nodes: Nodes<i32> = Nodes::new();
        for i in 0..100 {
            nodes.union(i, i + 1);
        }
        for i in 101..200 {
            nodes.union(i + 1, i);
        }
        nodes.union(100, 200);
        for i in 0..200 {
            assert_eq!(nodes.get_group(i), 0);
        }
    }

    #[test]
    fn parent_many_amortized() {
        let mut nodes: Nodes<i32> = Nodes::new();
        for i in 0..100 {
            nodes.union(i, i + 1);
        }
        for i in 101..200 {
            nodes.union(i + 1, i);
        }

        for i in 0..100 {
            assert_eq!(nodes.get_group(i), 0);
        }
        for i in 101..200 {
            assert_eq!(nodes.get_group(i), 101);
        }
        nodes.union(100, 200);
        for i in 0..200 {
            assert_eq!(nodes.get_group(i), 0);
            assert_eq!(nodes.get_parent(i), 0, "parent for {i}");
        }
    }

    #[derive(Default, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
    struct V2I32 {
        x: i32,
        y: i32,
    }

    impl Relative for V2I32 {
        fn flip(self) -> Self {
            Self {
                x: -self.x,
                y: -self.y,
            }
        }
        fn combine(self, other: Self) -> Self {
            Self {
                x: self.x + other.x,
                y: self.y + other.y,
            }
        }
    }

    #[test]
    fn relative_nodes() {
        let mut nodes: Nodes<i32, V2I32> = Nodes::new();

        let a = 1;
        let b = 2;
        let c = 3;
        let d = 4;

        nodes.add_relation(a, b, V2I32 { x: 1, y: 0 });
        assert_eq!(nodes.get_relation(a, b), Ok((1, V2I32 { x: 1, y: 0 })));
        assert_eq!(nodes.get_relation(b, a), Ok((1, V2I32 { x: -1, y: 0 })));

        nodes.add_relation(b, c, V2I32 { x: 0, y: 2 });
        assert_eq!(nodes.get_relation(a, c), Ok((1, V2I32 { x: 1, y: 2 })));
        assert_eq!(nodes.get_relation(c, a), Ok((1, V2I32 { x: -1, y: -2 })));
        assert_eq!(nodes.get_relation(b, c), Ok((1, V2I32 { x: 0, y: 2 })));
        assert_eq!(nodes.get_relation(c, b), Ok((1, V2I32 { x: 0, y: -2 })));
        assert_eq!(nodes.get_relation(a, b), Ok((1, V2I32 { x: 1, y: 0 })));
        assert_eq!(nodes.get_relation(b, a), Ok((1, V2I32 { x: -1, y: 0 })));

        nodes.add_relation(a, d, V2I32 { x: 0, y: -2 });

        assert_eq!(nodes.get_relation(d, c), Ok((1, V2I32 { x: 1, y: 4 })));
        assert_eq!(nodes.get_relation(c, d), Ok((1, V2I32 { x: -1, y: -4 })));
        // assert_eq!(nodes.get_parent(i), 0, "parent for {i}");
    }

    #[derive(Default, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
    struct Field {
        name: String,
        ty: u32,
    }

    #[derive(Default, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
    struct FieldDiff {
        add: Vec<Field>,
        remove: Vec<Field>,
    }

    impl FieldDiff {
        fn add_field(&mut self, name: &str, ty: u32) -> &mut Self {
            self.add.push(Field {
                name: name.to_owned(),
                ty,
            });
            self
        }
        fn remove_field(&mut self, name: &str, ty: u32) -> &mut Self {
            self.remove.push(Field {
                name: name.to_owned(),
                ty,
            });
            self
        }
        fn build(f: impl FnOnce(&mut Self)) -> Self {
            let mut this = Self::default();
            f(&mut this);
            this
        }
    }

    impl Relative for FieldDiff {
        fn flip(self) -> Self {
            Self {
                add: self.remove,
                remove: self.add,
            }
        }
        fn combine(mut self, mut other: Self) -> Self {
            self.add.append(&mut other.add);
            self.remove.append(&mut other.remove);
            // TODO: Cancel?
            Self {
                add: self
                    .add
                    .iter()
                    .filter(|it| !self.remove.contains(it))
                    .cloned()
                    .collect(),
                remove: self
                    .remove
                    .into_iter()
                    .filter(|it| !self.add.contains(it))
                    .collect(),
            }
        }
    }

    #[test]
    fn relative_types() {
        let mut nodes: Nodes<i32, FieldDiff> = Nodes::new();

        let a = 1;
        let b = 2;
        let c = 3;
        let d = 4;

        nodes.add_relation(
            a,
            b,
            FieldDiff::build(|it| {
                it.add_field("x", 2);
                it.remove_field("y", 3);
            }),
        );
        assert_eq!(
            nodes.get_relation(a, b),
            Ok((
                1,
                FieldDiff::build(|it| {
                    it.add_field("x", 2);
                    it.remove_field("y", 3);
                })
            ))
        );
        assert_eq!(
            nodes.get_relation(b, a),
            Ok((
                1,
                FieldDiff::build(|it| {
                    it.remove_field("x", 2);
                    it.add_field("y", 3);
                })
            ))
        );

        nodes.add_relation(
            b,
            c,
            FieldDiff::build(|it| {
                it.add_field("y", 3);
                it.remove_field("z", 4);
            }),
        );
        assert_eq!(
            nodes.get_relation(a, c),
            Ok((
                1,
                FieldDiff::build(|it| {
                    it.add_field("x", 2);
                    it.remove_field("z", 4);
                })
            ))
        );
        assert_eq!(
            nodes.get_relation(c, a),
            Ok((
                1,
                FieldDiff::build(|it| {
                    it.remove_field("x", 2);
                    it.add_field("z", 4);
                })
            ))
        );
        assert_eq!(
            nodes.get_relation(b, c),
            Ok((
                1,
                FieldDiff::build(|it| {
                    it.add_field("y", 3);
                    it.remove_field("z", 4);
                })
            ))
        );
        assert_eq!(
            nodes.get_relation(c, b),
            Ok((
                1,
                FieldDiff::build(|it| {
                    it.remove_field("y", 3);
                    it.add_field("z", 4);
                })
            ))
        );
        assert_eq!(
            nodes.get_relation(a, b),
            Ok((
                1,
                FieldDiff::build(|it| {
                    it.add_field("x", 2);
                    it.remove_field("y", 3);
                })
            ))
        );
        assert_eq!(
            nodes.get_relation(b, a),
            Ok((
                1,
                FieldDiff::build(|it| {
                    it.remove_field("x", 2);
                    it.add_field("y", 3);
                })
            ))
        );

        nodes.add_relation(
            a,
            d,
            FieldDiff::build(|it| {
                it.add_field("x", 2);
                it.remove_field("y", 3);
            }),
        );

        assert_eq!(
            nodes.get_relation(d, b),
            Ok((1, FieldDiff::build(|_it| {})))
        );
        assert_eq!(
            nodes.get_relation(b, d),
            Ok((1, FieldDiff::build(|_it| {})))
        );

        assert_eq!(
            nodes.get_relation(d, c),
            Ok((
                1,
                FieldDiff::build(|it| {
                    it.add_field("y", 3);
                    it.remove_field("z", 4);
                })
            ))
        );
        assert_eq!(
            nodes.get_relation(c, d),
            Ok((
                1,
                FieldDiff::build(|it| {
                    it.remove_field("y", 3);
                    it.add_field("z", 4);
                })
            ))
        );
        // assert_eq!(nodes.get_parent(i), 0, "parent for {i}");
    }
}
