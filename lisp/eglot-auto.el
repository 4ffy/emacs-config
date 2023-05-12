;;; eglot-auto.el --- Start Eglot only when a language server is found.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Cameron Norton

;; Author: Cameron Norton <cameron.norton@gmail.com>
;; Keywords: tools

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

;; If you want to enable Eglot for all programming modes, you might try hooking
;; `eglot-ensure' to `prog-mode'.  If you don't have a language server for all
;; languages, this approach has some problems.  First, an error message pops up
;; when opening a file.  This is not a big deal, but it can be rather annoying.
;; A more serious issue is that if you want to use `flymake-mode', Eglot
;; replaces all Flymake backends with itself, rendering Flymake useless for
;; languages where you don't have a language server.
;;
;; Ideally, Eglot should only be started if a language server is actually
;; available.  This is what eglot-auto tries to accomplish.  To start Eglot
;; only when a language server is found, hook `eglot-auto-start-eglot-maybe'
;; into `prog-mode'.

;;; Code:

(require 'cl-lib)
(require 'eglot)

(defun eglot-auto--equal-or-member (item item-or-list)
  "Determine whether ITEM is equal to or a member of ITEM-OR-LIST."
  (if (atom item-or-list)
      (equal item item-or-list)
    (member item item-or-list)))

(defun eglot-auto--find-lsp-server (major-mode-symbol)
  "Try to find an LSP server executable for MAJOR-MODE-SYMBOL."
  (let*
      ;; Search `eglot-server-programs' for a matching contact.
      ((contact
        (cdr
         (cl-assoc
          major-mode-symbol
          eglot-server-programs
          :test 'eglot-auto--equal-or-member)))
       ;; The executable should be the first element of the contact. Some
       ;; contacts are functions that return the "real" contact value. If this
       ;; is the case, try calling the function to get the proper value.
       (executable-name
        (car
         (if (byte-code-function-p contact)
             (condition-case nil
                 (funcall contact)
               (error nil))
           contact))))
    (when executable-name
      (executable-find executable-name))))

;;;###autoload
(defun eglot-auto-start-eglot-maybe ()
  "Run `eglot-ensure' if an LSP server is available for the current major mode."
  (interactive)
  (when (eglot-auto--find-lsp-server major-mode)
    (eglot-ensure)))

(provide 'eglot-auto)
;;; eglot-auto.el ends here
