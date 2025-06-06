;; -*- lexical-binding: t; -*-

;; Inhibit garbage collection during startup.
(setq gc-cons-threshold most-positive-fixnum)

;; This function has to be at the top.
(defun cn/my-laptop-p ()
  "Determine if the current system is my laptop."
  (string-equal "Renda" (system-name)))

;; Get this in early.
(require 'use-package)
(setopt use-package-always-defer t)



;;;;;;;;;;;;;;;;;;;;
;; BASIC SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;

;; Load theme, then darken the background and lighten the cursor.
(load-theme 'wombat t)
(custom-theme-set-faces
 'wombat
 '(cursor ((t (:background "#f6f3e8"))))
 '(default ((t (:foreground "#f6f3e8" :background "#181818")))))

;; Font selection - use a larger font on laptop
(let* ((font-family "Liberation Mono")
       (font-size (if (cn/my-laptop-p) 11 12))
       (font-string (format "%s %d" font-family font-size)))
  (add-to-list 'initial-frame-alist `(font . ,font-string))
  (add-to-list 'default-frame-alist `(font . ,font-string))
  (set-frame-font font-string t))

;; CJK Fallback fonts. "(emacs) Modifying Fontsets" claims that han script
;; covers all CJK characters, but I still have to set kana for this to work in
;; Emacs 29. Also, `set-fontset-font' does not reliably work at Emacs startup,
;; so I have to wrap this in a function and call it when a frame is created.
(defun cn/cjk-font-setup (_)
  "Set CJK fonts for the default fontset."
  (interactive)
  (dolist (script '(han kana cjk-misc))
    (set-fontset-font t script "Noto Sans CJK JP"))
  (set-fontset-font t 'hangul "Noto Sans CJK KR"))

(add-hook 'after-make-frame-functions 'cn/cjk-font-setup)

;; Basic settings. These should all be "Emacs settings." That is, customizations
;; for builtin packages and standalone components should go in separate
;; use-package declarations further down.
(use-package emacs
  :custom
  (apropos-do-all t)
  (auto-revert-verbose nil)
  (auto-save-default t)
  (auto-save-file-name-transforms
   `((".*" ,(file-name-concat user-emacs-directory "auto-save/") t)))
  (auto-save-no-message t)
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (bookmark-save-flag 1)
  (column-number-mode t)
  (completion-ignore-case t)
  (completions-detailed t)
  (cursor-type 'bar)
  (custom-file (file-name-concat user-emacs-directory "customize.el"))
  (delete-by-moving-to-trash t)
  (describe-bindings-outline t)
  (display-time-24hr-format t)
  (display-time-default-load-average nil)
  (fill-column 80)
  (frame-inhibit-implied-resize t)
  (frame-resize-pixelwise t)
  (header-line-format t)
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (initial-major-mode 'org-mode)
  (initial-scratch-message nil)
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  (major-mode (lambda ()
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
  (make-backup-files nil)
  (minibuffer-beginning-of-buffer-movement t)
  (mode-line-compact 'long)
  (next-line-add-newlines t)
  (display-raw-bytes-as-hex t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (repeat-exit-timeout 1)
  (require-final-newline t)
  (save-interprogram-paste-before-kill t)
  (sentence-end-double-space nil)
  (switch-to-buffer-obey-display-actions t)
  (tab-width 4)
  (use-short-answers t)
  (user-full-name "Cameron Norton")
  (user-mail-address "cameron.norton@gmail.com")
  (view-read-only t)
  (visual-line-fringe-indicators '(nil right-curly-arrow))
  (window-resize-pixelwise t))

;; Various interface modes
(display-time-mode 1)
(repeat-mode 1)
(tooltip-mode -1)
(add-to-list 'default-frame-alist '(menu-bar-lines . nil))
(add-to-list 'default-frame-alist '(tool-bar-lines . nil))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

;; Various hooks
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Add local script directory to `exec-path'.
(when (equal system-type 'gnu/linux)
  (add-to-list 'exec-path (file-name-concat (getenv "HOME") ".local/bin")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION DECLARATIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cn/ansi-colorize-buffer ()
  "Apply ANSI escape code colors to a buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun cn/build-available-p ()
  "Determine whether the variable `exec-path' has a compiler and make.
gcc and clang are valid compilers."
  (and (or (executable-find "gcc") (executable-find "clang"))
       (executable-find "make")))

(defun cn/cmake-build-available-p ()
  "Determine whether the variable `exec-path' has a compiler, make, and cmake."
  (and (cn/build-available-p) (executable-find "cmake")))

(defun cn/kill-all-buffers ()
  "Prompt to kill all buffers, leaving an empty *scratch* buffer."
  (interactive)
  (when (yes-or-no-p "Kill all buffers?")
    (progn
      (mapc 'kill-buffer (buffer-list))
      (delete-other-windows)
      (scratch-buffer))))

(defun cn/kill-all-other-buffers ()
  "Prompt to kill all buffers except for the active buffer."
  (interactive)
  (when (yes-or-no-p "Kill all other buffers?")
    (dolist (buffer (buffer-list))
      (unless (equal buffer (current-buffer))
        (kill-buffer buffer)))
    (delete-other-windows)
    (get-scratch-buffer-create)))

(defun cn/load-config-file (file-name)
  "Load the file FILE-NAME relative to the Emacs config directory."
  (load (file-name-concat user-emacs-directory file-name)))

(defun cn/switch-theme (theme)
  "Disable all currently active themes and enable THEME."
  (interactive (list
                (intern
                 (completing-read
                  "Load custom theme: "
                  (mapcar #'symbol-name (custom-available-themes))))))
  ;; Loading the theme first allows `load-theme' to do error checking for me.
  (load-theme theme)
  (mapc #'disable-theme (cdr custom-enabled-themes)))

(defun cn/unfill-paragraph (&optional region)
  "Take a multi-line paragraph and turn it into a single line of text.
This function is the opposite of `fill-paragraph'.

If REGION is non-nil, unfill all paragraphs in the active region."
  (interactive "*p")
  (let ((fill-column (point-max))
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun cn/unescape-url (start end)
  "Unescape percent-encoded characters between START and END.
Interactively, unescape characters in the active region."
  (interactive "*r")
  (when (or (not (called-interactively-p 'any)) (use-region-p))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "%\\([[:xdigit:]]\\{2\\}\\)" end t)
        (let ((unescaped
               (char-to-string (cl-parse-integer (match-string 1) :radix 16))))
          (replace-match unescaped t t))))))



;;;;;;;;;;;;;;;;;;
;; KEY BINDINGS ;;
;;;;;;;;;;;;;;;;;;

;; Improved DWIM commands.
(keymap-global-set "M-c" 'capitalize-dwim)
(keymap-global-set "M-l" 'downcase-dwim)
(keymap-global-set "M-u" 'upcase-dwim)

;; Custom kill buffer commands
(keymap-global-set "C-c k" 'cn/kill-current-buffer)
(keymap-global-set "C-c K" 'cn/kill-all-buffers)

;; Use ibuffer for buffer list
(keymap-global-set "C-x C-b" 'ibuffer)

;; Regex search by default
(keymap-global-set "C-s" 'isearch-forward-regexp)
(keymap-global-set "C-r" 'isearch-backward-regexp)

;; Window dedication toggle
(keymap-global-set "C-c d" 'toggle-window-dedicated)

;; Unfill paragraph
(keymap-global-set "M-Q" 'cn/unfill-paragraph)

;; Toggle menu bar
(keymap-global-set "<f12>" (lambda () (interactive) (menu-bar-mode 'toggle)))

;; Make window resizing more coarse
;; There is no shrink window vertically command bound by default
(keymap-global-set "C-x ^" (lambda () (interactive)
                             (enlarge-window 10)))
(keymap-global-set "C-x {" (lambda () (interactive)
                             (shrink-window-horizontally 10)))
(keymap-global-set "C-x }" (lambda () (interactive)
                             (enlarge-window-horizontally 10)))

;; Toggle proportional fonts
(keymap-global-set "C-c p" 'variable-pitch-mode)

;; Swap windows faster
(keymap-global-set "M-o" 'other-window)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETTINGS FOR MAJOR BUILTIN PACKAGES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Auto revert settings
(use-package autorevert
  :custom
  (global-auto-revert-mode-text "")
  (auto-revert-mode-text "")
  (auto-revert-tail-mode-text ""))

;; C-style language settings
(use-package cc-mode
  :custom
  (c-basic-offset tab-width)
  (c-default-style '((java-mode . "java")
                     (other . "linux")))
  (c-doc-comment-style '((java-mode . javadoc)
                         (other . doxygen))))

;; Compilation settings
(use-package compile
  :custom
  (compile-command "make -k -j$(nproc)")
  (compilation-scroll-output t)
  (compilation-skip-threshold 2)
  :hook (compilation-filter . ansi-color-compilation-filter))

;; Extra Dired goodies.
(use-package dired-x)

;; Dired settings
(use-package dired
 :custom
 (dired-auto-revert-buffer 'dired-directory-changed-p)
 (dired-do-revert-buffer t)
 (dired-dwim-target t)
 (dired-guess-shell-alist-user
  (when (equal system-type 'gnu/linux)
    '(("\\.7z\\'" "unar" "7z x")
      ("\\.avi\\'" #1= "xdg-open")
      ("\\.com\\'" "dosbox")
      ("\\.exe\\'" "wine" "dosbox")
      ("\\.gif\\'" #1#)
      ("\\.i?pk[37]\\'" "gzdoom -file * > /dev/null")
      ("\\.jpe?g\\'" #1#)
      ("\\.mid\\'" "fluidsynth -i" "aplaymidi -p 28:0")
      ("\\.mkv\\'" #1#)
      ("\\.mp[34]\\'" #1#)
      ("\\.mpe?g\\'" #1#)
      ("\\.og[gv]\\'" #1#)
      ("\\.png\\'" #1#)
      ("\\.pke\\'" "eternity -file")
      ("\\.py\\'" "python")
      ("\\.wad\\'" "woof -file" "gzdoom -file * > /dev/null")
      ("\\.wav\\'" #1#)
      ("\\.webm\\'" #1#)
      ("\\.wmv\\'" #1#)
      ("\\.zip\\'" "unar")
      ("\\`PKGBUILD\\'" "makepkg"))))
 (dired-listing-switches
  (if (and (executable-find "ls")
           (string-match
            "GNU coreutils" (shell-command-to-string "ls --version")))
      "-ADXghl --group-directories-first"
    "-AXghl --group-directories-first"))
 :bind (:map dired-mode-map ("N" . dired-create-empty-file)))

;; Doc view Settings
(use-package doc-view
  :custom
  (doc-view-continuous t)
  (doc-view-resolution 200))

;; Ediff
(use-package ediff
  :custom (ediff-window-setup-function 'ediff-setup-windows-plain))

;; Editorconfig
(use-package editorconfig
  :init (editorconfig-mode))

;; Eglot (LSP)
(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  :bind
  (("C-c e c" . eglot-code-actions)
   ("C-c e e" . (lambda () (interactive) (eglot-ensure)))
   ("C-c e f" . eglot-code-action-quickfix)
   ("C-c e g" . eglot-reconnect)
   ("C-c e k" . eglot-shutdown)
   ("C-c e K" . eglot-shutdown-all)
   ("C-c e r" . eglot-rename))
  :config
  (add-to-list 'eglot-server-programs '(fennel-mode "fennel-ls")))

;; Eldoc
(use-package eldoc
  :custom (eldoc-minor-mode-string ""))

;; ERC
(use-package erc
  :custom
  (erc-autojoin-channels-alist '(("libera\\.chat" "#emacs")))
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 14)
  (erc-hide-list '("JOIN" "NICK" "PART" "QUIT"))
  :hook
  ((erc-mode . erc-notifications-mode)
   (erc-mode . erc-scrolltobottom-enable)
   (erc-mode
    . (lambda () (setq-local scroll-conservatively most-positive-fixnum)))))

;;; EWW web browser settings
(use-package eww
  :custom (eww-search-prefix "https://searx.be/search?q="))

(use-package flymake
  :custom (flymake-mode-line-lighter "")
  :hook prog-mode)

;; Extra generic modes
(use-package generic-x)

;; Debugger settings
(use-package gud
  :custom
  (gud-highlight-current-line t)
  (gdb-show-main t))

;; Hexl hex editor
(use-package hexl
  :custom (hexl-bits 8))

;; Ibuffer settings
(use-package ibuffer
  :custom
  (ibuffer-saved-filter-groups
   '(("Default"
      ("Files" (visiting-file))
      ("Directories" (used-mode . dired-mode))
      ("Magit" (name . "magit"))
      ("Terminals" (used-mode . eat-mode))
      ("IRC" (used-mode . erc-mode))
      ("Special" (starred-name)))))
  (ibuffer-save-with-custom nil)
  (ibuffer-show-empty-filter-groups nil)
  :init
  (add-hook
   'ibuffer-mode-hook
   (lambda () (ibuffer-switch-to-saved-filter-groups "Default"))))

;; Org mode settings
(use-package org
  :custom
  (org-babel-load-languages '((emacs-lisp . t) (python . t) (R . t)))
  (org-confirm-babel-evaluate nil)
  (org-enforce-todo-dependencies t)
  (org-export-with-smart-quotes t)
  (org-latex-compiler "xelatex")
  (org-return-follows-link t)
  (org-src-preserve-indentation t))

;; Proced settings
(use-package proced
  :custom
  (proced-auto-update-flag t)
  (proced-format 'custom)
  (proced-sort 'rss)
  :config
  (add-to-list
   'proced-format-alist '(custom user pid tree pcpu rss (args comm))))

;; HTML renderer settings
(use-package shr
  :custom
  (shr-max-image-proportion 0.5)
  (shr-max-width 80)
  (shr-use-colors nil)
  (shr-use-fonts nil)
  (shr-width 80))

;; Tramp (remote editing)
(use-package tramp
  :commands tramp-revert-buffer-with-sudo
  :init (defalias 'sudoedit #'tramp-revert-buffer-with-sudo))

;; URL settings
(use-package url
  :custom
  (browse-url-browser-function 'browse-web)
  (url-cookie-confirmation nil))



;;;;;;;;;;;;;;;;;;;;;;
;; LOAD OTHER FILES ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Load customizations
(load custom-file)

;; Load packages
(cn/load-config-file "packages.el")

;; Load macros
(cn/load-config-file "macros.el")

;; Load mu4e if present
;; mu also needs to be set up but that's harder to test for
(when (locate-library "mu4e")
  (cn/load-config-file "mu4e-config.el"))

;; Load tree sitter if applicable.
(when (treesit-available-p)
  (cn/load-config-file "treesit-config.el"))



;; Reset garbage collection.
(setopt gc-cons-threshold (* 16 1024 1024))
(setopt gc-cons-percentage 0.2)
(garbage-collect)
