;;; elp-mode.el --- Major modes for editing and running Elp -*- lexical-binding: t; -*-

;;; Commentary:

;; Derived from Erlang mode, with modifications for elp as it evolves

;;; Code:

;; Note: if we derive from Erlang, it launches erlang_ls too, because
;; of calling the parent hooks.
(define-derived-mode elp-mode
  erlang-mode "Erlang ELP"
  "Major mode for Erlang, using ELP."
  (setq-local case-fold-search nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(elp\\|P\\)\\'" . elp-mode))

(provide 'elp-mode)
;;; elp-mode.el ends here
