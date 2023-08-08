;;;=============================================================================
;;; TREE SITTER CONFIG
;;;=============================================================================

;;; List of tree sitter languages.
(defvar my-treesit-language-list
  '((bash
     sh-mode
     bash-ts-mode
     "https://github.com/tree-sitter/tree-sitter-bash")
    (c
     c-mode
     c-ts-mode
     "https://github.com/tree-sitter/tree-sitter-c")
    (cpp
     c++-mode
     c++-ts-mode
     "https://github.com/tree-sitter/tree-sitter-cpp")
    (go
     go-mode
     go-ts-mode
     "https://github.com/tree-sitter/tree-sitter-go")
    (java
     java-mode
     java-ts-mode
     "https://github.com/tree-sitter/tree-sitter-java")
    (json
     json-mode
     json-ts-mode
     "https://github.com/tree-sitter/tree-sitter-json")
    (python
     python-mode
     python-ts-mode
     "https://github.com/tree-sitter/tree-sitter-python")
    (rust
     rust-mode
     rust-ts-mode
     "https://github.com/tree-sitter/tree-sitter-rust")
    (yaml
     yaml-mode
     yaml-ts-mode
     "https://github.com/ikatyang/tree-sitter-yaml"))
  "List of languages that I wish to use treesit modes for.

Each element in the list should have the form

    (LANG LANG-MODE LANG-TS-MODE . (URL REVISION SOURCE-DIR CC C++)

LANG is the tree-sitter language symbol as used by
`treesit-language-source-alist' and `treesit-available-p'.

LANG-MODE and LANG-TS-MODE are the original and tree-sitter
replacement modes to be entered into `major-mode-remap-alist'.

The remainder is information about how to download and compile a
grammar as given in `treesit-language-source-alist'.")

;;; Set tree sitter grammar sources by adding the 1st and >=4th entries to
;;; `treesit-language-source-alist'
(dolist (language my-treesit-language-list)
  (add-to-list 'treesit-language-source-alist
               `(,(car language) . ,(cdddr language))))

;;; Compile tree sitter grammars if possible.
(when (compiler-available-p)
  (dolist (grammar treesit-language-source-alist)
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

;;; Map major modes to their tree sitter equivalents if their grammar is
;;; available.
(dolist (language my-treesit-language-list)
  (let ((language-symbol (car language))
        (mode-name (cadr language))
        (ts-mode-name (caddr language)))
    (when (treesit-ready-p language-symbol t)
      (add-to-list 'major-mode-remap-alist `(,mode-name . ,ts-mode-name)))))
