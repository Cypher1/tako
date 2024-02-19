use num_traits::bounds::Bounded;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

use static_assertions::assert_eq_size;
assert_eq_size!(TypedIndex<Vec<u8>, u8>, [u8; 1]);
assert_eq_size!(Option<TypedIndex<Vec<u8>, u8>>, [u8; 2]); // TODO(perf): Option optimisation
assert_eq_size!(TypedIndex<Vec<u32>, u32>, [u32; 1]);
assert_eq_size!(Option<TypedIndex<Vec<u32>, u32>>, [u32; 2]); // TODO(perf): Option optimisation

#[repr(transparent)]
pub struct TypedIndex<T, Idx = u32, Container: Index<usize> = Vec<T>> {
    index: Idx,
    ty: PhantomData<T>,
    container: PhantomData<Container>,
}

impl<T, Idx: Bounded, Container: Index<usize>> Bounded for TypedIndex<T, Idx, Container> {
    fn min_value() -> Self {
        Self::from_raw(<Idx as Bounded>::min_value())
    }

    fn max_value() -> Self {
        Self::from_raw(<Idx as Bounded>::max_value())
    }
}

impl<T, Idx, Container: Index<usize>> TypedIndex<T, Idx, Container> {
    pub const fn from_raw(index: Idx) -> Self {
        Self {
            index,
            ty: PhantomData,
            container: PhantomData,
        }
    }
}

impl<T, Idx: Copy> TypedIndex<T, Idx> {
    pub fn raw_index(&self) -> Idx {
        self.index
    }
}
impl<T, Idx: std::hash::Hash> std::hash::Hash for TypedIndex<T, Idx> {
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        self.index.hash(h)
    }
}
impl<T, Idx: PartialOrd> PartialOrd for TypedIndex<T, Idx> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.index.partial_cmp(&other.index)
    }
}
impl<T, Idx: Ord> Ord for TypedIndex<T, Idx> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}

impl<T, Idx: PartialEq> PartialEq for TypedIndex<T, Idx> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}
impl<T, Idx: PartialEq> Eq for TypedIndex<T, Idx> {}

impl<T, Idx: Clone> Clone for TypedIndex<T, Idx> {
    fn clone(&self) -> Self {
        Self {
            index: self.index.clone(),
            ty: PhantomData,
            container: PhantomData,
        }
    }
}
impl<T, Idx: Copy> Copy for TypedIndex<T, Idx> {}

impl<T, Idx: Bounded + std::fmt::Debug + PartialEq> std::fmt::Debug for TypedIndex<T, Idx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}_", std::any::type_name::<T>())?;
        if self == &Self::max_value() {
            write!(f, "MAX")
        } else {
            write!(f, "{:?}", self.index)
        }
    }
}

impl<T, Idx: std::fmt::Debug + std::convert::TryInto<usize>, Container: Index<usize>>
    TypedIndex<T, Idx, Container>
{
    pub fn get(self, container: &Container) -> &Container::Output
    where
        Container: Index<usize>,
    {
        &container[self
            .index
            .try_into()
            .unwrap_or_else(|_| panic!("Index too large for accessing into container as $TYPE"))]
    }
}
impl<T, Idx: std::fmt::Debug + std::convert::TryInto<usize>, Container: IndexMut<usize>>
    TypedIndex<T, Idx, Container>
{
    pub fn get_mut(self, container: &mut Container) -> &mut Container::Output
    where
        Container: Index<usize>,
    {
        &mut container[self
            .index
            .try_into()
            .unwrap_or_else(|_| panic!("Index too large for accessing into container as $TYPE"))]
    }
}

impl<T, Idx: std::fmt::Debug + std::convert::TryInto<usize> + std::convert::TryFrom<usize>>
    TypedIndex<T, Idx, Vec<T>>
{
    #[allow(clippy::ptr_arg)]
    pub fn next(container: &Vec<T>) -> Result<Self, <Idx as std::convert::TryFrom<usize>>::Error> {
        Ok(Self::from_raw(Idx::try_from(container.len())?))
    }
    pub fn new(
        container: &mut Vec<T>,
        value: T,
    ) -> Result<Self, <Idx as std::convert::TryFrom<usize>>::Error> {
        let id = Self::next(container)?;
        container.push(value);
        Ok(id)
    }
}
