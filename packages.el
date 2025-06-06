;;; -*- lexical-binding: t; -*-
;;;=============================================================================
;;; PACKAGES
;;;=============================================================================

;; Prepare package archives.
(use-package package
  :custom
  (package-archive-priorities '(("gnu" . 3) ("nongnu" . 2) ("melpa" . 1)))
  (package-native-compile t)
  :init
  (add-to-list 'load-path (file-name-concat user-emacs-directory "lisp"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
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

;; Clang Format (provided by system clang package)
(use-package clang-format
  :when (and (equal system-type 'gnu/linux) (executable-find "clang"))
  :load-path "/usr/share/clang"
  :commands (clang-format clang-format-buffer clang-format-region))

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
   ("C-c o" . consult-outline))
  :config
  (keymap-global-set
   "C-c r" ; C-c r because C-c g is for magit
   (if (executable-find "rg")
       'consult-ripgrep
     'consult-grep))
  (dolist (regexp '("\\.gpg\\'" "\\.kdbx?\\'"))
    (add-to-list 'consult-preview-excluded-files regexp)))

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

;; Eat
(use-package eat
  :ensure t
  :pin nongnu
  :demand t
  :unless (equal system-type 'windows-nt)
  :custom
  (eat-enable-shell-prompt-annotation nil)
  (eat-message-handler-alist
   '(("compile" . compile)
     ("dired" . dired)
     ("eww" . eww)
     ("magit" . magit-status-setup-buffer)
     ("man" . man)))
  :init
  (defalias 'cn/eat-create-new-buffer
    (kmacro "C-u M-x e a t <return>")
    "Create and switch to a new eat buffer.")
  (add-hook
   'eat-exit-hook
   #'(lambda (_)
       (kill-buffer (current-buffer))
       (when (one-window-p)
         (delete-frame))))
  (add-hook
   `eat-mode-hook
   #'(lambda () (setq-local scroll-conservatively most-positive-fixnum)))
  :bind
  (("C-c v" . eat)
   ("C-x 4 v" . eat-other-window)))

;; Elfeed (RSS)
(use-package elfeed
  :ensure t
  :pin melpa
  :custom
  (elfeed-search-filter "@1-week-ago !\[$\] +unread ")
  (elfeed-db-directory (file-name-concat user-emacs-directory "elfeed"))
  :config
  (cn/load-config-file "feeds.el"))  ; feed list to its own file

;; Elisp auto formatting
(use-package elisp-autofmt
  :ensure t
  :pin melpa)

(use-package embark
  :ensure t
  :pin gnu
  :bind ("C-c a" . embark-act))

(use-package embark-consult
  :ensure t
  :pin gnu)

;; Engrave faces (for org latex export)
(use-package engrave-faces
  :ensure t
  :pin gnu)

;; ERC nick highlighing
(use-package erc-hl-nicks
  :ensure t
  :pin melpa
  :after erc)

;; ESS
(use-package ess
  :ensure t
  :pin gnu)

;; Fennel mode
(use-package fennel-mode
  :ensure t
  :pin melpa
  :mode "\\.fnl\\'"
  :interpreter "fennel")

;; Fish mode.
(use-package fish-mode
  :ensure t
  :pin melpa
  :mode "\\.fish\\'"
  :interpreter "fish")

;; Google translate
(use-package google-translate-default-ui
  :ensure google-translate
  :custom
  (google-translate-output-destination 'echo-area)
  (google-translate-show-phonetic t)
  (google-translate-translation-to-kill-ring t)
  :bind
  (("C-c t t" . google-translate-at-point)
   ("C-c t q" . google-translate-query-translate)
   ("C-c t b" . google-translate-buffer))
  :config
  (advice-add
   #'google-translate-buffer
   :around
   (lambda (fn &rest args)
     (let ((google-translate-output-destination nil))
       (apply fn args)))))

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
  :config
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'markdown-view-mode-hook 'visual-line-mode))

;; Meson mode
(use-package meson-mode
  :ensure t
  :pin melpa
  :mode "/meson\\(\\.build\\|_options\\.txt\\)\\'")

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
  (fennel-mode lisp-data-mode scheme-mode))

;; Switch Python virtual environment
(use-package pyvenv
  :ensure t
  :pin melpa)

;; Simple modeline
(use-package simple-modeline
  :ensure t
  :pin melpa
  :init (simple-modeline-mode))

;; SLIME
(use-package slime
  :ensure t
  :pin nongnu
  :custom (inferior-lisp-program "sbcl")
  :hook lisp-mode)

;; Automatic title case commands.
(use-package titlecase
  :ensure t
  :pin melpa)

;; Soft wrap lines at fill column
(use-package visual-fill-column
  :ensure t
  :pin nongnu
  :custom (visual-fill-column-enable-sensible-window-split t)
  :init (add-hook 'visual-line-mode-hook
                  (lambda () (visual-fill-column-mode
                              (if visual-line-mode 1 -1)))))

;; Vertico (icomplete-vertical but better)
(use-package vertico
  :ensure t
  :pin gnu
  :init (vertico-mode))

;; Writeable grep buffer
(use-package wgrep
  :ensure t
  :pin nongnu)

;; Yasnippet
(use-package yasnippet
  :ensure t
  :pin gnu
  :diminish yas-minor-mode
  :custom (yas-use-menu nil)
  :bind (:map yas-minor-mode-map
              ("<tab>" . nil)
              ("TAB" . nil)
              ("C-c y" . yas-expand))
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  :pin nongnu
  :after yasnippet)

;; Local Zeal docs package.
(use-package zeal
  :bind
  (("C-h z" . zeal-lookup-symbol)
   ("C-h Z" . zeal-display-docset)))
