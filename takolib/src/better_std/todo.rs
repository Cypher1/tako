#[macro_export]
macro_rules! todo(
    ($($arg:tt)+) => {{
        use log::error;
        error!($($arg)+);
        panic!("Unimplemented");
    }};
    () => {{
        todo!("Unimplemented {}:{}:{}", file!(), line!(), column!());
    }};
);
