
fn main() {
    let mut s = "1";
    while s < 1_000_000 {
        s = format!("({}+{})", &s, &s);
    }
    println!("{}", s);
}
