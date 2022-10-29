use std::marker::PhantomData;

pub struct TypedIndex<T, Index=u32> {
    index: Index,
    ty: PhantomData<T>,
}

impl<T, I> TypedIndex<T, I> {
    pub fn new(index: I) -> Self {
        Self {
            index,
            ty: PhantomData,
        }
    }
}

impl<T, I: PartialEq> PartialEq for TypedIndex<T, I> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}
impl<T, I: PartialEq> Eq for TypedIndex<T, I> {}

impl<T, I: Clone> Clone for TypedIndex<T, I> {
    fn clone(&self) -> Self {
        Self {
            index: *self.index,
            ty: PhantomData,
        }
    }
}
impl<T, I: Copy> Copy for TypedIndex<T, I> {}

impl<T, I: std::fmt::Debug> std::fmt::Debug for TypedIndex<T, I> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}_{}", std::any::type_name::<T>(), self.index)
    }
}

