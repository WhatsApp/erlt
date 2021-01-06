use std::path::Path;

use anyhow::{Context, Result};
use serde::Deserialize;
use xshell::{read_file, write_file};

use crate::project_root;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Mode {
    Overwrite,
    Verify,
}

#[derive(Debug)]
enum NodeType {
    Punctuation(u16, String),
    Keyword(u16, String),
    Literal(u16, String),
    Node(u16, String, Vec<String>),
}

pub struct CodegenCmd {
    pub mode: Mode,
}

impl CodegenCmd {
    pub fn run(self) -> Result<()> {
        let node_types = read_node_types()?;
        generate_syntax_kinds(&node_types)?;
        Ok(())
    }
}

fn generate_syntax_kinds(_node_types: &[NodeType]) -> Result<()> {
    println!("nodes: {:?}", read_node_types()?);
    Ok(())
}

fn update(path: &Path, contents: &str, mode: Mode) -> Result<()> {
    match read_file(path) {
        Ok(old_contents) if &old_contents == contents => {
            return Ok(());
        }
        _ => {}
    }
    if mode == Mode::Verify {
        anyhow::bail!("`{}` is not up-to-date", path.display());
    }
    eprintln!("updating {}", path.display());
    write_file(path, contents)?;
    return Ok(());
}
enum NameType {
    Punctuation(String),
    Identifier(String),
}
#[derive(Deserialize)]
struct RawNodeType {
    #[serde(rename = "type")]
    name: String,
    named: bool,
    children: Option<RawNodeChildren>,
}

#[derive(Deserialize)]
struct RawNodeChildren {
    // multiple: bool,
    // required: bool,
    types: Vec<RawNodeChild>,
}

#[derive(Deserialize)]
struct RawNodeChild {
    #[serde(rename = "type")]
    name: String,
    named: bool,
}

fn read_node_types() -> Result<Vec<NodeType>> {
    let node_types_path = project_root().join("../tree-sitter-erlang/src/node-types.json");
    let node_types_string = read_file(&node_types_path)
        .with_context(|| format!("accessing node types file in {}", node_types_path.display()))?;
    let node_types: Vec<RawNodeType> = serde_json::from_str(&node_types_string)
        .with_context(|| format!("parsing node types file in {}", node_types_path.display()))?;
    let language = tree_sitter_erlang::language();

    node_types
        .iter()
        .map(|node| {
            let id = language.id_for_node_kind(&node.name, node.named);
            match node {
                RawNodeType { name, named: true, children: Some(children) } => {
                    let children =
                        children.types.iter().map(map_child_name).collect::<Result<_>>()?;
                    Ok(NodeType::Node(id, name.to_ascii_uppercase(), children))
                }
                RawNodeType { name, named: true, .. } => {
                    Ok(NodeType::Literal(id, name.to_ascii_uppercase()))
                }
                RawNodeType { name, named: false, .. } => match map_name(name)? {
                    NameType::Punctuation(name) => Ok(NodeType::Punctuation(id, name)),
                    NameType::Identifier(name) => Ok(NodeType::Keyword(id, name + "_KW")),
                },
            }
        })
        .collect()
}

fn map_name(name: &str) -> Result<NameType> {
    use NameType::*;

    match name {
        "!" => Ok(Punctuation("BANG".into())),
        "@" => Ok(Punctuation("AT".into())),
        "#" => Ok(Punctuation("POUND".into())),
        "$" => Ok(Punctuation("DOLLAR".into())),
        "%" => Ok(Punctuation("PERCENT".into())),
        "^" => Ok(Punctuation("CARET".into())),
        "&" => Ok(Punctuation("AMP".into())),
        "*" => Ok(Punctuation("STAR".into())),
        "(" => Ok(Punctuation("LPAREN".into())),
        ")" => Ok(Punctuation("RPAREN".into())),
        "-" => Ok(Punctuation("DASH".into())),
        "+" => Ok(Punctuation("PLUS".into())),
        "=" => Ok(Punctuation("EQ".into())),
        "{" => Ok(Punctuation("LBRACE".into())),
        "}" => Ok(Punctuation("RBRACE".into())),
        "[" => Ok(Punctuation("LBRACK".into())),
        "]" => Ok(Punctuation("RBRACK".into())),
        "\\" => Ok(Punctuation("BSLASH".into())),
        "|" => Ok(Punctuation("PIPE".into())),
        ":" => Ok(Punctuation("COLON".into())),
        ";" => Ok(Punctuation("SEMI".into())),
        "\"" => Ok(Punctuation("DQUOTE".into())),
        "'" => Ok(Punctuation("SQUOTE".into())),
        "<" => Ok(Punctuation("LT".into())),
        ">" => Ok(Punctuation("GT".into())),
        "," => Ok(Punctuation("COMMA".into())),
        "." => Ok(Punctuation("DOT".into())),
        "?" => Ok(Punctuation("QMARK".into())),
        "->" => Ok(Punctuation("ARROW".into())),
        _ => {
            if name.chars().all(|c| ('a'..='z').contains(&c) || c == '_') {
                Ok(Identifier(name.to_ascii_uppercase()))
            } else {
                anyhow::bail!("invalid node name: {}", name)
            }
        }
    }
}

fn map_child_name(child: &RawNodeChild) -> Result<String> {
    match map_name(&child.name)? {
        NameType::Punctuation(name) => Ok(name),
        NameType::Identifier(name) if child.named => Ok(name),
        NameType::Identifier(name) => Ok(name + "_KW"),
    }
}
