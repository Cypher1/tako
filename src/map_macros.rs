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
macro_rules! str_map(
    {} => {::std::collections::HashMap::new()};
    { $($key:expr => $value:expr),* } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key.to_string(), $value);
            )*
            m
        }
     };
);