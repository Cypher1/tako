// use std::sync::Arc;

#[salsa::query_group(HelloWorldStorage)]
trait HelloWorld: salsa::Database {
    #[salsa::input]
    fn input_string(&self, key: ()) -> String;

    // This is a *derived query*, meaning its value is specified by
    // a function (see Step 2, below).
    fn length(&self, key: ()) -> usize;

    // This is a *derived query*, meaning its value is specified by
    // a function (see Step 2, below).
    fn length_times_two(&self, key: ()) -> usize;
}

fn length(db: &dyn HelloWorld, (): ()) -> usize {
    // Read the input string:
    let input_string = db.input_string(());

    println!("Calculating length... ({})", input_string.len());
    // Return its length:
    input_string.len()
}

fn length_times_two(db: &dyn HelloWorld, (): ()) -> usize {
    println!("Calculating length_times_two... ({})", db.length(()));
    2 * db.length(())
}

#[salsa::database(HelloWorldStorage)]
#[derive(Default)]
struct DatabaseStruct {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for DatabaseStruct {}

// This shows how to use a query.
pub fn test_salsa() {
    let mut db = DatabaseStruct::default();

    let init = "Hello, world".to_string();
    db.set_input_string((), init.clone());

    println!("Now, the length_times_two is {}.", db.length_times_two(()));
    db.set_input_string((), init);
    println!("Now, the length_times_two is {}.", db.length_times_two(()));

    db.set_input_string((), format!("Hello, world"));

    println!("Now, the length_times_two is {}.", db.length_times_two(()));
}
