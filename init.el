;;;=============================================================================
;;; GENERAL SETTINGS
;;;=============================================================================

;;Theme
(load "~/.config/emacs/wombat-custom-theme.el")
(load-theme 'wombat-custom t)

;;Font selection - use a smaller font on laptop.
(if (equal "Newton" (system-name))
    (set-frame-font "Liberation Mono 13")
  (set-frame-font "Liberation Mono 14"))

;;Clean up the interface a bit.
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(setq-default inhibit-startup-screen t
              initial-scratch-message nil
              window-resize-pixelwise t
              frame-resize-pixelwise t
              cursor-type 'bar)

;;Org mode for *scratch* buffer
(setq-default initial-major-mode 'org-mode)

;;Display clock on mode line
(display-time-mode t)

;;Global column numbering, with a visible fill column for editing modes
(setq-default fill-column 80
              column-number-mode t)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook 'display-fill-column-indicator-mode)

;;Pair parenthesis when programming
(add-hook 'prog-mode-hook 'electric-pair-mode)

;;Display line numbers when programming
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;Automatic newlines in text modes.
(add-hook 'text-mode-hook 'auto-fill-mode)

;;Indentation settings
(setq-default tab-width 4
              indent-tabs-mode nil
              c-default-style "linux"
              c-basic-offset 4)

;;Answer questions with y/n
(setq-default use-short-answers t)

;;Stop cluttering my directories with #autosave# files.
;;I accept any data loss from a crash (until it actually happens, probably).
(setq-default make-backup-files nil
              auto-save-mode nil
              backup-directory-alist ;banish to /tmp if files made anyway.
              `((".*" . ,temporary-file-directory))
              auto-save-file-name-transforms
              `((".*" ,temporary-file-directory t)))

;;Guess major mode from file name
(setq-default major-mode
              (lambda ()
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

;;EWW web browser settings
(setq-default browse-url-browser-function 'browse-web
              eww-search-prefix "https://searx.be/?q="
              shr-max-image-proportion 0.5
              shr-max-width 80
              shr-use-colors nil
              shr-use-fonts nil
              shr-width 80
              url-cache-directory "~/.cache/emacs/url/cache"
              url-cookie-confirmation t
              url-cookie-file "/dev/null") ;no cookies

;;List directories first in dired
(setq-default dired-listing-switches
              "-al --color=auto --group-directories-first")

;;Use ibuffer for buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;Kill current buffer
(global-set-key (kbd "C-c k") (lambda () (interactive)
                                (if (yes-or-no-p "Kill current buffer?")
                                    (kill-current-buffer))))

;;Regex search by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;;Move customizations to their own file.
(setq-default custom-file "~/.config/emacs/customize.el")
(load custom-file)

;;Load macros from file.
(load "~/.config/emacs/macros.el")

;;;=============================================================================
;;; PACKAGES
;;;=============================================================================

;;melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;use-package
(eval-when-compile
  (add-to-list 'load-path
               "/home/cameron/.config/emacs/elpa/use-package-20210207.1926")
  (require 'use-package))

;;AUCTeX (LaTeX)
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

;;Company (popup autocompletion)
(use-package company :ensure t
  :hook ((prog-mode . company-mode)
         (LaTeX-mode . company-mode)))
  
;;Eglot (LSP)
(use-package eglot :ensure t
  :hook (prog-mode . eglot-ensure))

;;Elfeed (RSS)
(use-package elfeed :ensure t
  :config
  (setq-default elfeed-db-directory "~/.cache/emacs/elfeed"
                elfeed-search-filter "@1-week-ago !\[$\] ")
  (load "~/.config/emacs/feeds.el")) ;feed list to its own file

;;Elpy (Python)
(use-package elpy :ensure t
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable))

;;Fennel mode
(use-package fennel-mode :ensure t)

;Flycheck (Improved syntax checking)
(use-package flycheck :ensure t
  :hook ((prog-mode . flycheck-mode)
         (LaTeX-mode . flycheck-mode)
         (elpy-mode . flycheck-mode)))

;;Go mode
(use-package go-mode :ensure t)

;;Lua mode
(use-package lua-mode :ensure t)

;;Magit (git)
(use-package magit :ensure t
  :bind ("C-c g" . magit-status))

;;Markdown mode
(use-package markdown-mode :ensure t
  :config (add-hook 'markdown-mode-hook 'flyspell-mode))

;;Neotree
(use-package neotree :ensure t
  :bind (("<f8>" . neotree-toggle)
         ("C-c t" . neotree-dir)))

;;Rust mode
(use-package rust-mode :ensure t)

;;Simple modeline
(use-package simple-modeline :ensure t
  :init (simple-modeline-mode))

;;Vertico (completion)
(use-package vertico :ensure t
  :init (vertico-mode))

