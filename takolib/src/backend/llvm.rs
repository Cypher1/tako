use super::Backend;

// use inkwell;

struct Llvm {}

impl Backend for Llvm {
    fn add_binary(&mut self) {}
    fn add_library(&mut self) {}
    fn add_function(&mut self) {}
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn llvm_is_included() {
        print!("LLVM feature on");
    }
}
