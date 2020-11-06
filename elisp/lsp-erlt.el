;;; lsp-erlt.el --- ErlT Client settings         -*- lexical-binding: t; -*-

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

;; lsp-erlt client

;;; Code:

(require 'lsp-mode)

(defgroup lsp-erlt nil
  "LSP support for the Erlang programming language, using erlang-ls"
  :group 'lsp-mode
  :link '(url-link "https://github.com/erlang-ls/erlang_ls"))

(defcustom lsp-erlt-server-path
  "erlang_ls"
  "Path to the Erlang Language Server binary."
  :group 'lsp-erlt
  :risky t
  :type 'file)

(defcustom lsp-erlt-server-connection-type
  'stdio
  "Type of connection to use with the Erlang Language Server: tcp or stdio."
  :group 'lsp-erlt
  :risky t
  :type 'symbol)

(defun lsp-erlt-server-start-fun (port)
  `(,lsp-erlt-server-path
    "--transport" "tcp"
    "--port" ,(number-to-string port)))

(defun lsp-erlt-server-connection ()
  (if (eq lsp-erlt-server-connection-type 'tcp)
      (lsp-tcp-connection 'lsp-erlt-server-start-fun)
    (lsp-stdio-connection `(,lsp-erlt-server-path "--transport" "stdio"))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-erlt-server-connection)
                  :major-modes '(erlt-mode)
                  :priority -1
                  :server-id 'erlang-ls-t))

(provide 'lsp-erlt)
;;; lsp-erlt.el ends here
