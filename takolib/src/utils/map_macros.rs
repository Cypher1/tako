#[macro_export]
macro_rules! map(
    {} => {::std::collections::HashMap::new()};
    { $($key:expr => $value:expr),* } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key, $value);
            )*
            m
        }
     };
);

#[macro_export]
macro_rules! hash_set(
    {} => {::std::collections::HashSet::new()};
    { $($value:expr),* } => {
        {
            let mut m = ::std::collections::HashSet::new();
            $(
                m.insert($value);
            )*
            m
        }
     };
);

#[macro_export]
macro_rules! set(
    {} => {::std::collections::BTreeSet::new()};
    { $($value:expr),* } => {
        {
            let mut m = ::std::collections::BTreeSet::new();
            $(
                m.insert($value);
            )*
            m
        }
     };
);

#[macro_export]
macro_rules! dict(
    {} => {::std::collections::BTreeSet::new()};
    { $($key:expr => $value:expr),* } => {
        {
            let mut m = ::std::collections::BTreeSet::new();
            $(
                m.insert(($key.to_string(), $value));
            )*
            m
        }
     };
);

#[macro_export]
macro_rules! rec(
    {} => {$crate::primitives::Val::Struct(::std::vec::Vec::new())};
    { $($key:expr => $value:expr),* } => {
        {
            let m = vec![
            $(
                ($key.to_string(), $value),
            )*];
            $crate::primitives::Val::Struct(m)
        }
     };
);
