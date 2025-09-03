;;; -*- lexical-binding: t; -*-
;;;=============================================================================
;;; TREE SITTER CONFIG
;;;=============================================================================

(require 'treesit)

(defun cn/treesit-recompile-all-grammars ()
  "Recompile all Tree-sitter grammars with a fresh git clone."
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (treesit-install-language-grammar (car grammar))))

;;; List of Tree-sitter languages.
(setopt
 treesit-language-source-alist
 '((bash "https://github.com/tree-sitter/tree-sitter-bash")
   (c "https://github.com/tree-sitter/tree-sitter-c")
   (cmake "https://github.com/uyha/tree-sitter-cmake")
   (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
   (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
   (go "https://github.com/tree-sitter/tree-sitter-go")
   (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
   (java "https://github.com/tree-sitter/tree-sitter-java")
   (json "https://github.com/tree-sitter/tree-sitter-json")
   (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
   (python "https://github.com/tree-sitter/tree-sitter-python")
   (rust "https://github.com/tree-sitter/tree-sitter-rust")
   (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml")
   (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

;;; Compile missing tree sitter grammars if possible.
(when (cn/build-available-p)
  (dolist (grammar treesit-language-source-alist)
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

;;; Set up grammars for use.  Some Tree-sitter modes are replacements for older
;;; modes, while others are standalone.  Replacements can be added to
;;; `major-mode-remap-alist' to take advantage of existing `auto-mode-alist'
;;; entries and such.

;;; Remappable:

(use-package sh-script
  :when (treesit-ready-p 'bash t)
  :init (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode)))

(use-package c-ts-mode
  :when (and (treesit-ready-p 'c t) (treesit-ready-p 'cpp t))
  :custom
  (c-ts-mode-indent-offset tab-width)
  (c-ts-mode-indent-style 'linux)
  :init
  (dolist (pair
           '((c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)))
    (add-to-list 'major-mode-remap-alist pair)))

(use-package csharp-mode
  :when (treesit-ready-p 'c-sharp t)
  :init (add-to-list 'major-mode-remap-alist '(csharp-mode . csharp-ts-mode)))

(use-package java-ts-mode
  :when (treesit-ready-p 'java t)
  :init (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode)))

(use-package python
  :when (treesit-ready-p 'python t)
  :init (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

;;; Manual:

(use-package cmake-ts-mode
  :when (treesit-ready-p 'cmake t)
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package go-ts-mode
  :when (treesit-ready-p 'go t)
  :custom (go-ts-mode-indent-offset tab-width)
  :mode "\\.go\\'")

(use-package go-mod-ts-mode
  :when (treesit-ready-p 'gomod t)
  :mode "go\\.mod\\'")

(use-package json-ts-mode
  :when (treesit-ready-p 'json t)
  :mode "\\.json\\'")

(use-package lua-ts-mode
  :when (treesit-ready-p 'lua t)
  :mode "\\.lua\\'"
  :interpreter "lua")

(use-package rust-ts-mode
  :when (treesit-ready-p 'rust t)
  :mode "\\.rs\\'")

(use-package toml-ts-mode
  :when (treesit-ready-p 'toml t)
  :mode "\\.toml\\'")

(use-package yaml-ts-mode
  :when (treesit-ready-p 'yaml t)
  :mode ("\\.\\(e?ya?\\|ra\\)ml\\'" "\\.clang-format\\'"))
