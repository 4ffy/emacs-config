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
               (concat user-emacs-directory "elpa/use-package-20221028.1858"))
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

  ;; Use a different image size on laptop.
  (if (equal "Renda" (system-name))
      (setq-default preview-scale-function 0.8))

  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'electric-pair-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'line-number-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode))

;; LaTeX preview pane
(use-package latex-preview-pane :ensure t
  :config
  (add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)
  (setq-default pdf-latex-command "xelatex"))

;; CIDER (Clojure)
(use-package cider :ensure t
  :hook (clojure-mode . cider-mode))

;; Company (popup autocompletion)
(use-package company :ensure t
  :diminish company-mode
  :hook ((prog-mode . company-mode)
         (LaTeX-mode . company-mode)))

;; Diminish (Hide modeline clutter)
(use-package diminish :ensure t
  :init
  (diminish 'abbrev-mode)
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode))

;; Eglot (LSP)
(use-package eglot :ensure t
  :config (setq-default eglot-autoreconnect nil)
  :hook (prog-mode . eglot-ensure))

;; Elfeed (RSS)
(use-package elfeed :ensure t
  :config
  (setq-default elfeed-search-filter "@1-week-ago !\[$\] "
                elfeed-db-directory (concat user-emacs-directory "elfeed"))
  (load-config-file "feeds.el")) ;feed list to its own file

;; Elpy (Python)
(use-package elpy :ensure t
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable))

;; Exec path from shell (For eshell $PATH, not necessary on Windows)
(use-package exec-path-from-shell :ensure t
  :init
  (unless (equal system-type "windows-nt")
    (exec-path-from-shell-initialize)))

;; Fennel mode
(use-package fennel-mode :ensure t)

;; Flycheck (Improved syntax checking)
(use-package flycheck :ensure t
  :diminish flycheck-mode
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

;; Simple modeline
(use-package simple-modeline :ensure t
  :init (simple-modeline-mode))

;; Vertico (completion)
(use-package vertico :ensure t
  :init (vertico-mode))

;; Undo tree
(use-package undo-tree :ensure t
  :diminish undo-tree-mode
  :init
  (setq-default undo-tree-auto-save-history nil
                undo-tree-enable-undo-in-region t)
  (global-undo-tree-mode))

;; Vterm (improved terminal, not available on Windows)
(unless (equal system-type "windows-nt")
  (use-package vterm :ensure t
    :init
    (defun vterm-other-window ()
      "Create or switch to a vterm buffer in another window."
      (interactive)
      (with-current-buffer (get-buffer-create "*vterm*")
        (if (not (equal major-mode 'vterm-mode)) (vterm-mode)))
      (switch-to-buffer-other-window "*vterm*"))

    (defun vterm-create-new-buffer ()
      "Create and switch to a new vterm buffer."
      (interactive)
      (let ((new-buffer (generate-new-buffer "*vterm*")))
        (with-current-buffer new-buffer (vterm-mode))
        (switch-to-buffer new-buffer)))

    (add-hook 'vterm-exit-functions
              (lambda (_ _)
                (if (and (equal major-mode 'vterm-mode) (one-window-p))
                    (delete-frame (selected-frame) t))))

    :bind ("C-x 4 v" . 'vterm-other-window)))

(use-package eshell-vterm :ensure t
  :after eshell
  :config (eshell-vterm-mode))

;; YAML mode
(use-package yaml-mode :ensure t)
