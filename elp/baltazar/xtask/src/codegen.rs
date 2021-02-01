use std::{collections::HashMap, path::Path};

use anyhow::{Context, Result};
use quote::{format_ident, quote};
use serde::Deserialize;
use xshell::{read_file, write_file};

use crate::{project_root, reformat};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Mode {
    Overwrite,
    Verify,
}

#[derive(Debug)]
enum NodeType {
    Punct(u16, String),
    Keyword(u16, String),
    Literal(u16, String),
    Enum(u16, Enum),
    Node(u16, Node),
}

#[derive(Debug)]
struct Enum {
    name: String,
    variants: Vec<String>,
}

#[derive(Debug)]
struct Node {
    name: String,
    fields: Vec<Field>,
}

#[derive(Debug)]
struct Field {
    name: String,
    multiple: bool,
    required: bool,
    types: Vec<String>,
}

impl NodeType {
    fn id(&self) -> u16 {
        match self {
            NodeType::Punct(id, _) => *id,
            NodeType::Keyword(id, _) => *id,
            NodeType::Literal(id, _) => *id,
            NodeType::Enum(id, _) => *id,
            NodeType::Node(id, _) => *id,
        }
    }

    fn name(&self) -> &str {
        match self {
            NodeType::Punct(_, name) => name,
            NodeType::Keyword(_, name) => name,
            NodeType::Literal(_, name) => name,
            NodeType::Enum(_, Enum { name, .. }) => name,
            NodeType::Node(_, Node { name, .. }) => name,
        }
    }
}

pub struct CodegenCmd {
    pub mode: Mode,
}

impl CodegenCmd {
    pub fn run(self) -> Result<()> {
        let node_types = read_node_types()?;

        let syntax_kinds_file = project_root().join("crates/syntax/src/generated/syntax_kind.rs");
        let syntax_kinds = generate_syntax_kinds(&node_types)?;
        update(&syntax_kinds_file, &syntax_kinds, self.mode)?;

        let nodes_file = project_root().join("crates/syntax/src/generated/nodes.rs");
        let nodes = generate_nodes(&node_types)?;
        update(&nodes_file, &nodes, self.mode)?;

        Ok(())
    }
}

fn generate_syntax_kinds(node_types: &[NodeType]) -> Result<String> {
    let all_kinds: Vec<_> = node_types
        .iter()
        .map(|node_type| {
            let name = format_ident!("{}", to_upper_snake_case(node_type.name()));
            let id = node_type.id();
            quote! { #name = #id }
        })
        .collect();

    let all_keywords: Vec<_> = node_types
        .iter()
        .filter_map(|node_type| match node_type {
            NodeType::Keyword(_id, name) => Some(format_ident!("{}", to_upper_snake_case(name))),
            _ => None,
        })
        .collect();

    let all_puncts: Vec<_> = node_types
        .iter()
        .filter_map(|node_type| match node_type {
            NodeType::Punct(_id, name) => Some(format_ident!("{}", to_upper_snake_case(name))),
            _ => None,
        })
        .collect();

    let all_literals: Vec<_> = node_types
        .iter()
        .filter_map(|node_type| match node_type {
            NodeType::Literal(_id, name) => Some(format_ident!("{}", to_upper_snake_case(name))),
            _ => None,
        })
        .collect();

    let ast = quote! {
        #![allow(bad_style, missing_docs, unreachable_pub)]
        /// The kind of syntax node, e.g. `ATOM`, `IF_KW`, or `DOT`.
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
        #[repr(u16)]
        pub enum SyntaxKind {
            #(#all_kinds,)*
            ERROR = u16::MAX
        }

        use self::SyntaxKind::*;

        impl SyntaxKind {
            pub fn is_keyword(self) -> bool {
                match self {
                    #(#all_keywords)|* => true,
                    _ => false
                }
            }

            pub fn is_punct(self) -> bool {
                match self {
                    #(#all_puncts)|* => true,
                    _ => false
                }
            }

            pub fn is_literal(self) -> bool {
                match self {
                    #(#all_literals)|* => true,
                    _ => false
                }
            }

        }
    };

    reformat(&ast.to_string())
}

fn generate_nodes(node_types: &[NodeType]) -> Result<String> {
    let defs = node_types.iter().filter_map(|node| {
        match node {
            NodeType::Keyword(_, _) => None,
            NodeType::Punct(_, _) => None,
            NodeType::Literal(_, name) => {
                let name = format_ident!("{}", name);
                Some(quote! {
                    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                    pub struct #name {
                        pub(crate) syntax: SyntaxNode,
                    }
                })
            }
            NodeType::Enum(_, en) => {
                let variants: Vec<_> =
                    en.variants.iter().map(|var| format_ident!("{}", var)).collect();
                let name = format_ident!("{}", en.name);
                Some(quote! {
                    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                    pub enum #name {
                        #(#variants(#variants),)*
                    }
                })
            }
            NodeType::Node(_, node) => {
                let name = format_ident!("{}", node.name);
                // let kind = format_ident!("{}", to_upper_snake_case(&node.name));
                Some(quote! {
                    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                    pub struct #name {
                        pub(crate) syntax: SyntaxNode,
                    }
                })
            }
        }
    });

    let ast = quote! {
        use crate::syntax::SyntaxNode;

        #(#defs)*
    };

    Ok(reformat(&ast.to_string())?)
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
    subtypes: Option<Vec<RawType>>,
    fields: Option<HashMap<String, RawField>>,
}

#[derive(Deserialize)]
struct RawField {
    multiple: bool,
    required: bool,
    types: Vec<RawType>,
}

#[derive(Deserialize)]
struct RawType {
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
                RawNodeType { name, named: _, subtypes: Some(subtypes), .. } => {
                    let variants = subtypes.iter().map(map_type).collect::<Result<_>>()?;
                    let name = to_camel_case(name.trim_start_matches("_"));
                    Ok(NodeType::Enum(id, Enum { name, variants }))
                }
                RawNodeType { name, named: true, fields: Some(fields), .. } if fields.len() > 0 => {
                    let fields = fields
                        .iter()
                        .map(|(name, field)| map_field(name, field))
                        .collect::<Result<_>>()?;
                    let name = to_camel_case(name);
                    Ok(NodeType::Node(id, Node { name, fields }))
                }
                RawNodeType { name, named: true, .. } => {
                    Ok(NodeType::Literal(id, to_camel_case(name)))
                }
                RawNodeType { name, named: false, .. } => match map_name(name)? {
                    NameType::Punctuation(name) => Ok(NodeType::Punct(id, name)),
                    NameType::Identifier(name) => Ok(NodeType::Keyword(id, name + "Kw")),
                },
            }
        })
        .collect()
}

fn map_name(name: &str) -> Result<NameType> {
    use NameType::*;

    match name {
        "!" => Ok(Punctuation("Bang".into())),
        "@" => Ok(Punctuation("At".into())),
        "#" => Ok(Punctuation("Pound".into())),
        "$" => Ok(Punctuation("Dollar".into())),
        "%" => Ok(Punctuation("Percent".into())),
        "^" => Ok(Punctuation("Caret".into())),
        "&" => Ok(Punctuation("Amp".into())),
        "*" => Ok(Punctuation("Star".into())),
        "(" => Ok(Punctuation("Lparen".into())),
        ")" => Ok(Punctuation("Rparen".into())),
        "-" => Ok(Punctuation("Dash".into())),
        "+" => Ok(Punctuation("Plus".into())),
        "=" => Ok(Punctuation("Eq".into())),
        "{" => Ok(Punctuation("Lbrace".into())),
        "}" => Ok(Punctuation("Rrace".into())),
        "[" => Ok(Punctuation("Lbrack".into())),
        "]" => Ok(Punctuation("Rbrach".into())),
        "\\" => Ok(Punctuation("Bslash".into())),
        "|" => Ok(Punctuation("Pipe".into())),
        ":" => Ok(Punctuation("Colon".into())),
        ";" => Ok(Punctuation("Semi".into())),
        "\"" => Ok(Punctuation("Dquote".into())),
        "'" => Ok(Punctuation("Squote".into())),
        "<" => Ok(Punctuation("Lt".into())),
        ">" => Ok(Punctuation("Gt".into())),
        "," => Ok(Punctuation("Comma".into())),
        "." => Ok(Punctuation("Dot".into())),
        "?" => Ok(Punctuation("Qmark".into())),
        "->" => Ok(Punctuation("Arrow".into())),
        _ => {
            if name.chars().all(|c| ('a'..='z').contains(&c) || c == '_') {
                Ok(Identifier(to_camel_case(name)))
            } else {
                anyhow::bail!("invalid node name: {}", name)
            }
        }
    }
}

fn to_upper_snake_case(s: &str) -> String {
    let mut buf = String::with_capacity(s.len());
    let mut prev = false;
    for c in s.chars() {
        if c.is_ascii_uppercase() && prev {
            buf.push('_')
        }
        prev = true;

        buf.push(c.to_ascii_uppercase());
    }
    buf
}

fn to_camel_case(s: &str) -> String {
    let mut buf = String::with_capacity(s.len());
    let mut new_word = true;
    for c in s.chars() {
        if c == '_' {
            new_word = true;
        } else if new_word {
            buf.push(c.to_ascii_uppercase());
            new_word = false;
        } else {
            buf.push(c.to_ascii_lowercase());
        }
    }
    buf
}

fn map_type(child: &RawType) -> Result<String> {
    match map_name(&child.name)? {
        NameType::Punctuation(name) => Ok(name),
        NameType::Identifier(name) if child.named => Ok(name),
        NameType::Identifier(name) => Ok(name + "Kw"),
    }
}

fn map_field(name: &str, field: &RawField) -> Result<Field> {
    Ok(Field {
        name: name.to_owned(),
        multiple: field.multiple,
        required: field.required,
        types: field.types.iter().map(map_type).collect::<Result<_>>()?,
    })
}
