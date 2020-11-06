;;; erlt-mode.el --- Major modes for editing and running ErlT -*- lexical-binding: t; -*-

;;; Commentary:

;; Derived from Erlang mode, with modifications for erlt as it evolves

;;; Code:

;;AZ:note yaml-mode.el is a good template/reference example
(define-derived-mode erlt-mode
  erlang-mode "ErlT"
  "Major mode for ErlT."
  (setq-local case-fold-search nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(erlt\\|P\\)\\'" . erlt-mode))

(provide 'erlt-mode)
;;; erlt-mode.el ends here

