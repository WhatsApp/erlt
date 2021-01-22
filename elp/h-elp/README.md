# Haskell Implementation of Erlang Language Platform POC

Using

- [tree-sitter](http://hackage.haskell.org/package/tree-sitter)
- [lsp](http://hackage.haskell.org/package/lsp)

And modelled on [haskell-language-server](https://github.com/haskell/haskell-language-server)

## Compiling

### Install GHC 8.10.3

Install ghcup via https://www.haskell.org/ghcup/

Run `ghcup tui` and install GHC 8.10.3 (and HLS, optional)

```
cabal update
cabal configure --enable-tests
```

Because we are building (for now) a forked haskell-tree-sitter, we
need to update the submodules.

The exact directory to change into depends on the current hash, but it will be clear.

```
cd dist-newstyle/src/haskell-t_-****HASH_VALUE****
git submodule update --init --recursive tree-sitter
cd ../../..
```

```
cabal build
```
