fn main() {
    let package = "tree-sitter-erlang";
    let source_directory = format!("../../../{}/src", package);
    let source_file = format!("{}/parser.c", source_directory);

    println!("cargo:rerun-if-changed={}", source_file);

    cc::Build::new()
        .file(source_file)
        .include(source_directory)
        .compile(&package);
}
