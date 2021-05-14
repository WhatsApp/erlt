;;; Copyright (c) 2020 Facebook, Inc. and its affiliates.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

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

