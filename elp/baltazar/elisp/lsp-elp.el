;;; lsp-elp.el --- Elp Client settings         -*- lexical-binding: t; -*-

;; Shamelessly adapted from

;; Copyright (C) 2019 Roberto Aloi

;; Author: Roberto Aloi
;; Keywords: erlang lsp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; lsp-elp client

;;; Code:

(require 'lsp-mode)

(defgroup lsp-elp nil
  "LSP support for the Erlang programming language, using baltazar"
  :group 'lsp-mode
  :link '(url-link "https://github.com/WhatsApp/erlt/tree/master/elp/baltazar"))

(defcustom lsp-elp-server-path
  "baltazar-t"
  "Path to the Erlang ELP Language Server binary."
  :group 'lsp-elp
  :risky t
  :type 'file)

(defun lsp-elp-server-start-fun (port)
  `(,lsp-elp-server-path
    "--transport" "tcp"
    "--port" ,(number-to-string port)))

(defun lsp-elp-server-connection ()
  (lsp-stdio-connection `(,lsp-elp-server-path)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-elp-server-connection)
                  :major-modes '(elp-mode)
                  :priority 1
                  :server-id 'baltazar))

(add-to-list 'lsp-language-id-configuration '(elp-mode . "elp"))
(add-to-list 'lsp-language-id-configuration '(".*\\.elp$" . "elp"))

(provide 'lsp-elp)
;;; lsp-elp.el ends here
