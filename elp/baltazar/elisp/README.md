# Setting up emacs support for baltazar

Add the following to your `.emacs`, using the appropriate path to this
directory on your machine.

```elisp
(add-to-list 'load-path "/Users/alanzimm/repos/WhatsApp/erlt/elp/baltazar/elisp")
(require 'elp-mode)
(require 'lsp-elp)

(add-hook 'elp-mode-hook #'lsp)
```

We also have to manage the erlang-mode server, to only start when not
in this derived mode.  See
https://github.com/emacs-lsp/lsp-mode/issues/2566

```elisp
(add-hook 'erlang-mode-hook #'lsp-derived-from-erlang-mode)

(defun lsp-derived-from-erlang-mode ()
"Call the lsp mode hook unless we are derived from erlang-mode."
    (unless (derived-mode-p 'erlang-mode)
      (lsp)))
```

# Development

Put the locally built `baltazar` on the path as `baltazar-t`.

```bash
sudo ln -s /Users/alanzimm/repos/WhatsApp/erlt/elp/baltazar/target/debug/baltazar /usr/local/bin/baltazar-t
```

Then customize `lsp-elp-server-path` to be `baltazar-t`

```elisp
(setq lsp-elp-server-path "baltazar-t")
```

Back to normal, using the version via `cargo install` (does not currently work)

```elisp
(setq lsp-elp-server-path "baltazar")
```
