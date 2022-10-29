use std::marker::PhantomData;
use std::ops::Index;
use soa_derive::StructOfArray;

pub struct TypedIndex<T, Idx=u32, Container: Index<usize> =Vec<T>> {
    index: Idx,
    ty: PhantomData<T>,
    container: PhantomData<Container>,
}

impl<T, Idx, Container: Index<usize>> TypedIndex<T, Idx, Container> {
    pub fn new(index: Idx) -> Self {
        Self {
            index,
            ty: PhantomData,
            container: PhantomData,
        }
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

impl<T, Idx: std::fmt::Debug> std::fmt::Debug for TypedIndex<T, Idx> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}_{:?}", std::any::type_name::<T>(), self.index)
    }
}

impl<T, Idx: std::fmt::Debug + std::convert::TryInto<usize>, Container: Index<usize>> TypedIndex<T, Idx, Container> {
    pub fn get(&self, container: &Container) -> &Container::Output where Container: Index<usize> {
        &container[self.index.try_into().unwrap_or_else(|_|panic!("Index too large for accessing into container as usize"))]
    }
    pub fn get_mut(&self, container: &mut Container) -> &mut Container::Output where Container: Index<usize> {
        &mut container[self.index.try_into().unwrap_or_else(|_|panic!("Index too large for accessing into container as usize"))]
    }
}
