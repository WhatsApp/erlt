use tree_sitter::Language;

extern "C" {
    fn tree_sitter_erlang_elp() -> Language;
}

pub fn language() -> Language {
    unsafe { tree_sitter_erlang_elp() }
}
