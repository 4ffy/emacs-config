;;;=============================================================================
;;; PACKAGES
;;;=============================================================================

;; Prepare package archives.
(use-package package
  :custom (package-native-compile t)
  :init
  (add-to-list 'load-path (file-name-concat user-emacs-directory "lisp"))
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))



;; AUCTeX (LaTeX)
(use-package tex
  :ensure auctex
  :pin gnu
  :mode ("\\.[tT]e[xX]\\'" . LaTeX-mode)
  :mode ("\\.bib\\'" . bibtex-mode)
  :diminish reftex-mode
  :custom
  (TeX-PDF-mode t)
  (TeX-auto-save t)
  (TeX-engine 'xetex)
  (TeX-master nil)
  (TeX-parse-self t)
  (reftex-plug-into-AUCTeX t)
  :config
  ;; Use a different image size on laptop.
  (when (cn/my-laptop-p)
    (setq-default preview-scale-function 0.8))
  ;; Add prog-mode hooks to LaTeX-mode.
  (dolist (hook prog-mode-hook)
    (add-hook 'LaTeX-mode-hook hook))
  :hook
  ;; LaTeX-specific hooks go here.
  ((LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . flyspell-mode)
   (LaTeX-mode . prettify-symbols-mode)
   (LaTeX-mode . turn-on-reftex)))

;; Avy (fast navigation)
(use-package avy
  :ensure t
  :pin gnu
  :demand t
  :bind
  (("C-c j" . avy-goto-word-1)
   ("C-c J" . avy-goto-char-2)))

;; Company (popup autocompletion)
(use-package company
  :ensure t
  :pin gnu
  :diminish company-mode
  :hook (prog-mode comint-mode))

;; Consult (improved completing-read functions)
(use-package consult
  :ensure t
  :pin gnu
  :demand t
  :bind
  (("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x p b" . consult-project-buffer)
   ("C-x r b" . consult-bookmark)
   ("M-y" . consult-yank-pop)
   ("M-g g" . consult-goto-line)
   ("C-c r" . consult-grep)             ; C-c r because C-c g is for magit
   ("C-c o" . consult-outline)))

;; Devil (prefix-less editing)
(use-package devil
  :ensure t
  :pin nongnu
  :diminish devil-mode
  :bind ("C-;" . devil-mode)
  :init
  (global-devil-mode)
  (devil-set-key (kbd ";")))

;; Diminish (Hide modeline clutter)
(use-package diminish
  :ensure t
  :pin gnu
  :init
  (diminish 'abbrev-mode)
  (diminish 'auto-fill-function)
  (diminish 'visual-line-mode))

;; Editorconfig support
(use-package editorconfig
  :ensure t
  :pin nongnu
  :custom (editorconfig-mode-lighter "")
  :init (editorconfig-mode))

;; Eglot (LSP)
(use-package eglot
  :ensure t
  :pin gnu
  :defer t
  :custom
  (eglot-autoshutdown t)
  (eglot-ignored-server-capabilities '(:inlayHintProvider)))

;; Local eglot-auto package.
(use-package eglot-auto
  :bind ("C-c e" . eglot-auto-start-eglot-maybe))

;; Elfeed (RSS)
(use-package elfeed
  :ensure t
  :pin melpa
  :defer t
  :custom
  (elfeed-search-filter "@1-week-ago !\[$\] ")
  (elfeed-db-directory (file-name-concat user-emacs-directory "elfeed"))
  :config
  (cn/load-config-file "feeds.el"))  ; feed list to its own file

;; Elisp auto formatting
(use-package elisp-autofmt
  :ensure t
  :pin melpa
  :defer t)

;; Emacs Everywhere
;; Note that xclip, xdotool, xprop, and xwininfo must be installed.
(use-package emacs-everywhere
  :ensure t
  :pin melpa
  :when (equal system-type 'gnu/linux)
  :defer t)

;; Engrave faces (for org latex export)
(use-package engrave-faces
  :ensure t
  :pin gnu
  :defer t)

;; ERC nick highlighing
(use-package erc-hl-nicks
  :ensure t
  :pin melpa
  :after erc)

;; ESS
(use-package ess
  :ensure t
  :pin melpa
  :defer t)

;; Exec path from shell (For eshell $PATH)
(use-package exec-path-from-shell
  :ensure t
  :pin melpa
  :unless (equal system-type 'windows-nt)
  :init (exec-path-from-shell-initialize))

;; Fennel mode
(use-package fennel-mode
  :ensure t
  :pin melpa
  :mode "\\.fnl\\'"
  :interpreter "fennel")

;; Flycheck (Improved syntax checking)
(use-package flycheck
  :ensure t
  :pin melpa
  :hook prog-mode)

;; Make Flycheck work with Eglot.
(use-package flycheck-eglot
  :ensure t
  :pin melpa
  :after (flycheck eglot)
  :config (global-flycheck-eglot-mode 1))

;; Go mode
(use-package go-mode
  :ensure t
  :pin nongnu
  :mode "\\.go\\'")

;; Hy mode
(use-package hy-mode
  :ensure t
  :pin melpa
  :mode "\\.hy\\'"
  :interpreter "hy")

;; JSON Mode
(use-package json-mode
  :ensure t
  :pin gnu
  :mode "\\.json\\'")

;; Keepass mode
(use-package keepass-mode
  :ensure t
  :pin melpa
  :mode "\\.kdbx?\\'")

;; Link hints
(use-package link-hint
  :ensure t
  :pin melpa
  :bind
  (("C-c l f" . link-hint-open-link)
   ("C-c l c" . link-hint-copy-link)))

;; Lua mode
(use-package lua-mode
  :ensure t
  :pin nongnu
  :mode "\\.lua\\'"
  :interpreter "lua")

;; Magit (git)
(use-package magit
  :ensure t
  :pin nongnu
  :custom (magit-diff-refine-hunk t)
  :bind ("C-c g" . magit-status))

;; Marginalia (completion annotations)
(use-package marginalia
  :ensure t
  :pin gnu
  :init (marginalia-mode))

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :pin nongnu
  :mode "\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'"
  :config (add-hook 'markdown-mode-hook 'flyspell-mode))

;; Orderless
(use-package orderless
  :ensure t
  :pin gnu
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Org Roam
(use-package org-roam
  :ensure t
  :pin melpa
  :demand t
  :when (cn/build-available-p)
  :custom
  (org-roam-directory (file-name-concat (getenv "HOME") "Roam"))
  (org-roam-dailies-directory "daily/")
  (org-roam-completion-everywhere t)
  :bind
  (("C-c n c" . org-roam-capture)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n r" . org-roam-refile)
   ("C-c n t" . org-roam-tag-add)
   :map org-mode-map
   ("C-M-i" . completion-at-point))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)

  (defun cn/org-roam-node-find-by-tag ()
    "Open an org-roam node by tag and name.  First, prompt for a tag,
then display a filtered list of nodes with that tag to select from."
    (interactive)
    (let ((tag (completing-read "Tag: " (org-roam-tag-completions))))
      (org-roam-node-find
       nil nil
       #'(lambda (node) (member tag (org-roam-node-tags node))))))

  (defun cn/org-roam-insert-nodes-with-tag ()
    "Insert a list of links to org-roam nodes that have the selected tag.
This is useful for quickly collecting nodes on a given topic."
    (interactive)
    (let* ((tag (completing-read "Tag: " (org-roam-tag-completions)))
           (nodes-with-tag
            (sort
             (seq-filter
              (lambda (node)
                (member tag (org-roam-node-tags node)))
              (org-roam-node-list))
             (lambda (node-1 node-2)
               (string<
                (downcase (org-roam-node-title node-1))
                (downcase (org-roam-node-title node-2)))))))
      (save-excursion
        (dolist (node nodes-with-tag)
          (insert
           (format "\n - %s"
                   (org-link-make-string
                    (concat
                     "id:" (org-roam-node-id node))
                    (org-roam-node-title node))))))
      (delete-char 1))))

;; Paredit
(use-package paredit
  :ensure t
  :pin nongnu
  :hook
  (fennel-mode hy-mode lisp-data-mode racket-mode scheme-mode))

;; Switch Python virtual environment
(use-package pyvenv
  :ensure t
  :pin melpa
  :defer t)

;; Racket mode
(use-package racket-mode
  :ensure t
  :pin nongnu
  :mode "\\.rkt[dl]?\\'"
  :interpreter "racket")

;; Rust mode
(use-package rust-mode
  :ensure t
  :pin nongnu
  :mode "\\.rs\\'")

;; Simple modeline
(use-package simple-modeline
  :ensure t
  :pin melpa
  :init (simple-modeline-mode))

;; SLIME
(use-package slime
  :ensure t
  :pin melpa
  :hook lisp-mode
  :init (setq-default inferior-lisp-program "sbcl"))

;; Automatic title case commands.
(use-package titlecase
  :ensure t
  :pin melpa
  :defer t)

;; Vertico (icomplete-vertical but better)
(use-package vertico
  :ensure t
  :pin gnu
  :init (vertico-mode))

;; Vterm (improved terminal, not available on Windows)
(use-package vterm
  :ensure t
  :pin melpa
  :demand t
  :when (cn/cmake-build-available-p)
  :unless (equal system-type 'windows-nt)
  :init
  (defun cn/vterm-create-new-buffer ()
    "Create and switch to a new vterm buffer."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'vterm)))

  (add-hook
   'vterm-exit-functions
   (lambda (_ _)
     (when (and (equal major-mode 'vterm-mode) (one-window-p))
       (delete-frame (selected-frame) t))))

  :bind
  (("C-c v" . vterm)
   ("C-x 4 v" . vterm-other-window)))

(use-package eshell-vterm
  :ensure t
  :pin melpa
  :unless (equal system-type 'windows-nt)
  :requires vterm
  :hook eshell-mode
  :config
  (dolist (command '("emacs" "emacsclient" "nvim" "vim"))
    (add-to-list 'eshell-visual-commands command)))

;; Writeable grep buffer
(use-package wgrep
  :ensure t
  :pin nongnu)

;; YAML mode
(use-package yaml-mode
  :ensure t
  :pin nongnu
  :mode "\\.\\(e?ya?\\|ra\\)ml\\'")

;; Yasnippet
(use-package yasnippet
  :ensure t
  :pin gnu
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
         ("<tab>" . nil)
         ("TAB" . nil)
         ("C-c y" . yas-expand))
  :init (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t
  :pin nongnu
  :after yasnippet)
