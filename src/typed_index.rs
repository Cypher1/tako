use std::marker::PhantomData;

struct TypedIndex<T, Index=u32> {
    index: Index,
    ty: PhantomData<T>,
}

impl<T, I: PartialEq> PartialEq<T> for TypedIndex<T, I> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}
impl<T, I: Eq> Eq<T> for TypedIndex<T, I> {}

impl<T, I: Clone> Clone<T> for TypedIndex<T, I> {
    fn clone(&self) -> Self {
        Self {
            index: *self.index,
            ty: PhantomData,
        }
    }
}
impl<T, I: Copy> Copy<T> for TypedIndex<T, I> {}

impl<T, I: std::fmt::Debug> std::fmt::Debug for TypedIndex<T, I> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}_{}", std::any::type_name::<T>(), self.index)
    }
}

