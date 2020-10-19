# Editing GitHub Workflows

We use dhall to generate the yamls for our GitHub workflows.

## Prerequisites

Required:
- (Mac OS) `brew install dhall`
- other systems: other package managers

Recommended:

- [vscode-language-dhall](https://github.com/PanAeon/vscode-language-dhall)
- [vscode-dhall-lsp-server](https://github.com/PanAeon/vscode-dhall-lsp-server)
- For best IDE experience: `cabal install dhall-lsp-server` which requires GHC Haskell and cabal.

## Edit and Generate

- Edit the dhall. Feel free to write in ascii: `\(foo: T) -> bar`
- Use the VSCode extension to auto-format. This converts to standard spacing and converts to the gratuitous unicode characters: `\(foo: T) -> bar` becomes `λ(foo : T) → bar`.
- `make` in this directory
