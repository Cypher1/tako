#[macro_export]
macro_rules! map(
    {} => {::std::collections::HashMap::new()};
    { $($key:expr => $value:expr),* $(,)? } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key.into(), $value.into());
            )*
            m
        }
     };
);

#[macro_export]
macro_rules! hash_set(
    {} => {::std::collections::HashSet::new()};
    { $($value:expr),* $(,)? } => {
        {
            let mut m = ::std::collections::HashSet::new();
            $(
                m.insert($value.into());
            )*
            m
        }
     };
);

#[macro_export]
macro_rules! set(
    {} => {::std::collections::BTreeSet::new()};
    { $($value:expr),* $(,)? } => {
        {
            let mut m = ::std::collections::BTreeSet::new();
            $(
                m.insert($value.into());
            )*
            m
        }
     };
);

#[macro_export]
macro_rules! dict(
    {} => {::std::collections::BTreeMap::new()};
    { $($key:expr => $value:expr),* $(,)? } => {
        {
            let mut m = ::std::collections::BTreeMap::new();
            $(
                m.insert($key.into(), $value.into());
            )*
            m
        }
     };
);

#[macro_export]
macro_rules! arc_slice(
    { $($value:expr),* $(,)? } => {
        {
            std::sync::Arc::new([
            $(
                $value,
            )*])
        }
     };
);
