
fn main() {
    let mut s = "1".to_string();
    while s.len() < 1_000_000 {
        s = format!("({}+{})", &s, &s);
    }
    println!("{}", s);
}
