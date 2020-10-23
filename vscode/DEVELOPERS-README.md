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
cd vscode # If not already there
git submodule update --init
npm install
npm run compile
vsce package
```

Then install the resulting `erlt-ls-0.0.14.vsix` (version number may change)
into VS Code using the `Install from VSIX...` option from the `...` dropdown
in the `Extensions` side bar.
