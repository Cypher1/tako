fn main() {
    let src_dir = std::path::Path::new("src");

    let mut c_config = cc::Build::new();
    c_config.std("c11").include(src_dir);

    c_config
        .flag("-Wno-unused-parameter")
        .flag("-Wno-unused-but-set-variable")
        .flag("-Wno-trigraphs");

    #[cfg(target_env = "msvc")]
    c_config.flag("-utf-8");

    #[cfg(feature = "c2rust")]
    tree_sitter_wasm_build_tool::add_wasm_headers(&mut c_config).unwrap();

    let parser_path = src_dir.join("parser.c");
    c_config.file(&parser_path);
    println!("cargo:rerun-if-changed={}", parser_path.to_str().unwrap());

    // NOTE: if your language uses an external scanner, uncomment this block:
    /*
    let scanner_path = src_dir.join("scanner.c");
    c_config.file(&scanner_path);
    println!("cargo:rerun-if-changed={}", scanner_path.to_str().unwrap());
    */

    c_config.compile("tree-sitter-tako");
}
