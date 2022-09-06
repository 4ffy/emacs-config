;;;=============================================================================
;;; PACKAGES
;;;=============================================================================

;; melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package
(eval-when-compile
  (add-to-list 'load-path
               "/home/cameron/.config/emacs/elpa/use-package-20210207.1926")
  (require 'use-package))

;; AUCTeX (LaTeX)
(use-package tex
  :ensure auctex
  :config
  (setq-default TeX-PDF-mode t
                TeX-auto-save t
                TeX-engine 'xetex
                TeX-master nil
                TeX-parse-self t
                reftex-plug-into-AUCTeX t)
  
  (if (equal "Newton" (system-name))
      (setq-default preview-scale-function 0.8))
  
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'electric-pair-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'line-number-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (progn
                                 (load-theme 'adwaita t)
                                 (set-background-color "AntiqueWhite")))))

;; Company (popup autocompletion)
(use-package company :ensure t
  :hook ((prog-mode . company-mode)
         (LaTeX-mode . company-mode)))
  
;; Eglot (LSP)
(use-package eglot :ensure t
  :hook (prog-mode . eglot-ensure))

;; Elfeed (RSS)
(use-package elfeed :ensure t
  :config
  (setq-default elfeed-db-directory "~/.cache/emacs/elfeed"
                elfeed-search-filter "@1-week-ago !\[$\] ")
  (load "~/.config/emacs/feeds.el")) ;feed list to its own file

;; Elpy (Python)
(use-package elpy :ensure t
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable))

;; Fennel mode
(use-package fennel-mode :ensure t)

;; Flycheck (Improved syntax checking)
(use-package flycheck :ensure t
  :hook ((prog-mode . flycheck-mode)
         (LaTeX-mode . flycheck-mode)
         (elpy-mode . flycheck-mode)))

;; Go mode
(use-package go-mode :ensure t)

;; JSON mode
(use-package json-mode :ensure t)

;; Lua mode
(use-package lua-mode :ensure t)

;; Magit (git)
(use-package magit :ensure t
  :bind ("C-c g" . magit-status))

;; Markdown mode
(use-package markdown-mode :ensure t
  :config (add-hook 'markdown-mode-hook 'flyspell-mode))

;; Neotree
(use-package neotree :ensure t
  :bind (("<f8>" . neotree-toggle)
         ("C-c t" . neotree-dir)))

;; Restart Emacs
(use-package restart-emacs :ensure t)

;; Rust mode
(use-package rust-mode :ensure t)

;; Simple modeline
(use-package simple-modeline :ensure t
  :init (simple-modeline-mode))

;; Vertico (completion)
(use-package vertico :ensure t
  :init (vertico-mode))

;; Vterm (improved terminal)
(use-package vterm :ensure t)
(use-package eshell-vterm :ensure t
  :after eshell
  :config (eshell-vterm-mode))

;; YAML mode
(use-package yaml-mode :ensure t)
