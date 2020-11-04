# README for extension developers (Mac OS)

## Prerequisites

* Install VSCode

```sh
npm install -g vsce
npm install -g typescript
```

## Quickstart

Make sure you are in the `vscode` subdirectory when doing these
commands.  It is done in a standalone way so it can merge into the
upstream source at some point.

```sh
cd vscode # If not already there (this directory)
git submodule update --init
npm install
npm run compile
vsce package
```

Then install the resulting `erlt-ls-0.0.14.vsix` (version number may change)
into VS Code using the `Install from VSIX...` option from the `...` dropdown
in the `Extensions` side bar.

## (Sub)-Repository organisation

The ErlT IDE support is based on current open source Erlang IDE
support ([vscode extension](https://github.com/erlang-ls/vscode) and
[erlang_ls language server](https://github.com/erlang-ls/erlang_ls)).

Because this repository is not (yet) public, the changes to these
repositories are being captured here, with the link to the upstream
repos being managed by
[git-subrepo](https://github.com/ingydotnet/git-subrepo).  This has
the advantage that for developers actively working in this area
changes can be managed between this repo and upstream, but it is
completely transparent to other users, so they do not have to manage
permission to the forked upstream repositories.

## My (AZ) development workflow

```bash
cd vscode/erlang_ls
make
```

Then in vscode I set the `Erlang_ls: Server Path` property to
`$PATH_TO_REPO/vscode/erlang_ls/_build/default/bin/erlang_ls`

This allows me to work on the erlang_ls code, save, make, restart the
session and see the changes immediately
