# ErlT: Erlang with Types

ErlT is an experimental extension of the Erlang programming language which aims
to empower Erlang writers to work even more efficiently, especially in WhatsApp-scale
code bases.

## Quick Links

 - **[Play](./play/README.md)** - The best way to get started is to read this README, then `../scripts/rebar3 new erlt <app-name>` in the `play` directory. Alternatively, with rebar3 version 3.14.2 or newer, `rebar3 new erlt <app-name>`.
- **[ErlT Overview](./LANGUAGE_OVERVIEW.md)** - A quick tour of the new language features
- **[ErlT Reference](./doc/README.md)** - A detailed breakdown of the differences between Erlang and ErlT
 -  A collection of small ErlT projects that demonstrate how the language can be used
- **[Tests](./tests/README.md)**

## Introduction

ErlT builds on Erlang to provide greater support for working with large Erlang
codebases. It does this by introducing static-types in a way which opens the
door to greater IDE integration, safer refactorings, and faster, more specific
feedback from the compiler.

By default, ErlT is checked (i.e. statically typed), but it also allows some
functions or modules to be unchecked (i.e. dynamically typed), both to aid
migration and to allow code to be written for which we don't yet have a good
typing model, most notably around concurrency - we provide only basic, low-level
support for `receive`. ErlT requires adding specs to unchecked code in order for
it to be accessible and typeable from checked code.

Notably, checked ErlT is currently limited to only the sequential parts of Erlang,
with some features banned, in particular:
- Atoms
- Erlang maps
- Dynamic applications (`M:F(A)`)

## Getting started using ErlT

See [./play/README.md](./play/README.md)

## IDE Support

### Emacs

Limited emacs support, provided as a thin layer on top of
`erlang-mode`.

*Features supported*

- Everything in Erlang mode.  This is not necessarily a good thing,
  some of them will need to be adapted still.  Please report problems
  as issues in this repo.

- Highlighting/font-lock. This is using the Erlang highlighting at the
  moment, will be tweaked for ErlT in time.

- Mechanics to start a language server (erlang_ls). This does not
  support ErlT at present, so the results are not good.  But it is a
  plumbing step, it is easier to keep it in than to remove it now,
  only to add it again shortly.  Time it short.

*Installation*

It can be enabled by putting the following into your `~/.emacs`

The snipper below uses REPO_PATH to stand for the actual place you
have put the code on your machine.

```elisp
;; Install the official Erlang mode
(package-require 'erlang)

;; Include the Language Server Protocol Clients
(package-require 'lsp-mode)

;; Enable LSP for Erlang files
(add-hook 'erlang-mode-hook #'lsp)

(add-to-list 'load-path "<REPO_PATH>/elisp")
(require 'lsp-erlt)
(require 'erlt-mode)
```

If you get stuck, take a look at the [erlang_ls instructions](https://erlang-ls.github.io/editors/emacs/)

Note: for `lsp-mode` it offers to start the server, this is a bit
pointless at the moment, I just blacklist the project or choose `n`.

But given `erlt-mode` is a derivative of `erlang-mode`, it is simpler
to do it this way, as we will have a language server real soon now.

### VS Code

**Coming soon**

## Working on the ErlT project itself

See [./CONTRIBUTING.md](./CONTRIBUTING.md)
