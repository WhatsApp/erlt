# Tree-sitter grammar for Erlang.

## Usage

```base
npm ci
npm run generate
```

## Testing the grammar

```bash
npm run test
```

As a by-product, the test will put a compiled version of the grammar
into `~/.tree-sitter`.

## Resources

[Guide to your first Tree-sitter grammar](https://gist.github.com/Aerijo/df27228d70c633e088b0591b8857eeef)

## MACOS notes

The emacs tree-sitter package has the following in its source, which
renames the `~/.tree-sitter/bin/erlang_elp.so` file to be
`~/.tree-sitter/bin/erlang_elp.dylib`.

```elisp
      ;; On macOS, rename .so => .dylib, because we will make a "universal"
      ;; bundle.
      (when (eq system-type 'darwin)
        ;; This renames existing ".so" files as well.
        (let ((default-directory tree-sitter-langs--bin-dir))
          (dolist (file (directory-files default-directory))
            (when (string-suffix-p ".so" file)
              (let ((new-name (concat (file-name-base file) ".dylib")))
                (when (file-exists-p new-name)
                  (delete-file new-name))
                (rename-file file new-name))))))
```

## emacs experimenting

Setup from `~/.emacs`

```elisp
;; From https://ubolonton.github.io/emacs-tree-sitter/installation/
(package-install 'tree-sitter)
(package-install 'tree-sitter-langs)
(require 'tree-sitter)
(require 'tree-sitter-langs)
(add-to-list 'tree-sitter-major-mode-language-alist '(erlang-mode . erlang_elp))
(define-derived-mode sexp-mode
  text-mode "Sexp"
  "Major mode for sexp."
  )
(add-to-list 'tree-sitter-major-mode-language-alist '(sexp-mode . sexp))
(global-tree-sitter-mode)
```
