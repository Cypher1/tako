#[macro_export]
macro_rules! todo(
    ($($arg:tt)+) => {{
        let message = format!("Unimplemented {}:{}:{}", file!(), line!(), column!());
        let user_message = format!($($arg)+);
        log::error!("{message}\n{user_message}");
        panic!("{message}\n{user_message}");
    }};
    () => {{
        let message = format!("Unimplemented {}:{}:{}", file!(), line!(), column!());
        log::error!("{message}");
        panic!("{message}");
    }};
);
