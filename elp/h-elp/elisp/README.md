# Setting up emacs support for h-elp

Add the following to your `.emacs`, using the appropriate path to this
directory on your machine.

```elisp
(add-to-list 'load-path "/Users/alanzimm/repos/WhatsApp/erlt/elp/h-elp/elisp")
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

Put the locally built `h-elp` on the path as `h-elp-t`.

```bash
sudo ln -s /Users/alanzimm/repos/WhatsApp/erlt/elp/h-elp/dist-newstyle/build/x86_64-osx/ghc-8.10.3/h-elp-0.1.0.0/x/h-elp/build/h-elp/h-elp /usr/local/bin/h-elp-t
```

Then customize `lsp-elp-server-path` to be `h-elp-t`

```elisp
(setq lsp-elp-server-path "h-elp-t")
```

Back to normal, using the version via `cabal install exe:h-elp`

```elisp
(setq lsp-elp-server-path "h-elp")
```
