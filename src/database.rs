use std::sync::Arc;

#[salsa::query_group(LoaderStorage)]
pub trait Loader: salsa::Database {
    #[salsa::input]
    fn file(&self, key: String) -> Arc<String>;

    // This is a *derived query*, meaning its value is specified by
    // a function (see Step 2, below).
    fn length(&self, key: String) -> usize;

    // This is a *derived query*, meaning its value is specified by
    // a function (see Step 2, below).
    fn length_times_two(&self, key: String) -> usize;
}

fn length(db: &dyn Loader, filename: String) -> usize {
    // Read the input string:
    let file = db.file(filename);

    println!("CALCULATING length... ({})", file.len());
    // Return its length:
    file.len()
}

fn length_times_two(db: &dyn Loader, filename: String) -> usize {
    println!("CALCULATING length_times_two... ({})", db.length(filename.clone()));
    2 * db.length(filename)
}

#[salsa::database(LoaderStorage)]
#[derive(Default)]
pub struct DB {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for DB {}
