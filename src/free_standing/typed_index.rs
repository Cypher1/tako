use std::marker::PhantomData;
use std::ops::Index;

pub struct TypedIndex<T, Idx=u32, Container=()> {
    index: Idx,
    ty: PhantomData<T>,
    container: PhantomData<Container>,
}

impl<T, Idx, Container> TypedIndex<T, Idx, Container> {
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
        }
    }
}
impl<T, Idx: Copy> Copy for TypedIndex<T, Idx> {}

impl<T, Idx: std::fmt::Debug> std::fmt::Debug for TypedIndex<T, Idx> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}_{}", std::any::type_name::<T>(), self.index)
    }
}

use soa_derive::StructOfArray;
impl<T, Idx> TypedIndex<T, Idx> {
    pub fn get(&self, container: &Vec<T>) -> &T {
        &container.get(self.index)
    }
    pub fn get_mut(&self, container: &mut Vec<T>) -> &mut T {
        &mut container.get(self.index)
    }
    pub fn get_inner_mut(&self, container: Vec<&mut T>) -> &mut T {
        container.get(self.index)
    }
    pub fn get_soa<C: StructOfArray>(&self, container: &C) -> &T {
        &container.get(self.index)
    }
    pub fn get_soa_mut<C: StructOfArray>(&self, container: &mut C) -> &mut T {
        &mut container.get_mut(self.index)
    }
}
