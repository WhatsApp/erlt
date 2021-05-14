## Editor Support

### Emacs

Limited emacs support, provided as a thin layer on top of
`erlang-mode`.

*Features supported*

- Everything in Erlang mode.  This is not necessarily a good thing,
  some of them will need to be adapted still.  Please report problems
  as issues in this repo.

- Highlighting/font-lock. This is using the Erlang highlighting at the
  moment, will be tweaked for ErlT in time.

It can be enabled by putting the following into your `~/.emacs`

The snipper below uses REPO_PATH to stand for the actual place you
have put the code on your machine.

```elisp
;; Install the official Erlang mode
(package-require 'erlang)

(add-to-list 'load-path "<REPO_PATH>/editors/elisp")
(require 'erlt-mode)
```
