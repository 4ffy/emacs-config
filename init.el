;; Inhibit garbage collection during startup.
(setq gc-cons-threshold most-positive-fixnum)

;; This function has to be at the top.
(defun cn/my-laptop-p ()
  "Determine if the current system is my laptop."
  (string-equal "Renda" (system-name)))



;;;;;;;;;;;;;;;;;;;;
;; BASIC SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;

;; Load theme, then darken the background and lighten the cursor. For some
;; reason, `set-face-background' does not work on the cursor face at startup,
;; but we can use the function `custom-set-faces' instead.
(load-theme 'wombat t)
(set-face-background 'default "#181818")
(custom-set-faces `(cursor ((t (:background "#f6f3e8")))))

;; Font selection - use a larger font on laptop
(let* ((font-family "Liberation Mono")
       (font-size (if (cn/my-laptop-p) 13 12))
       (font-string (format "%s %d" font-family font-size)))
  (add-to-list 'initial-frame-alist `(font . ,font-string))
  (add-to-list 'default-frame-alist `(font . ,font-string))
  (set-frame-font font-string t))

;; CJK Fallback fonts. "(emacs) Modifying Fontsets" claims that han script
;; covers all CJK characters, but I still have to set kana for this to work in
;; Emacs 29. Also, `set-fontset-font' does not reliably work at Emacs startup,
;; so I have to wrap this in a function and call it when a frame is created.
(defun cn/cjk-font-setup (&optional frame)
  "Set CJK fonts for the default fontset.
FRAME is unused and is present only to satisfy
`after-make-frame-functions', which calls its hooks with the new
frame as a parameter."
  (interactive)
  (dolist (script '(han kana cjk-misc))
    (set-fontset-font t script "Noto Sans CJK JP"))
  (set-fontset-font t 'hangul "Noto Sans CJK KR"))

(add-hook 'after-make-frame-functions 'cn/cjk-font-setup)

;; Basic settings. These should all be "Emacs settings." That is, customizations
;; for builtin packages and standalone components should go in separate
;; use-package declarations further down.
(setq-default apropos-do-all t
              auto-revert-verbose nil
              auto-save-default t
              auto-save-file-name-transforms
              `((".*" ,(file-name-concat user-emacs-directory "auto-save/") t))
              auto-save-no-message t
              backup-directory-alist `((".*" . ,temporary-file-directory))
              bookmark-save-flag 1
              column-number-mode t
              completion-ignore-case t
              completions-detailed t
              cursor-type 'bar
              custom-file (file-name-concat user-emacs-directory "customize.el")
              display-time-24hr-format t
              describe-bindings-outline t
              fill-column 80
              frame-inhibit-implied-resize t
              frame-resize-pixelwise t
              header-line-format t
              indent-tabs-mode nil
              inhibit-startup-screen t
              initial-major-mode 'org-mode
              initial-scratch-message nil
              isearch-lazy-count t
              lazy-count-prefix-format "(%s/%s) "
              lazy-count-suffix-format nil
              major-mode (lambda ()
                           (unless buffer-file-name
                             (let ((buffer-file-name (buffer-name)))
                               (set-auto-mode))))
              make-backup-files nil
              minibuffer-beginning-of-buffer-movement t
              mode-line-compact 'long
              next-line-add-newlines t
              read-buffer-completion-ignore-case t
              read-file-name-completion-ignore-case t
              repeat-exit-timeout 1
              require-final-newline t
              save-interprogram-paste-before-kill t
              switch-to-buffer-obey-display-actions t
              tab-width 4
              use-short-answers t
              user-full-name "Cameron Norton"
              user-mail-address "cameron.norton@gmail.com"
              view-read-only t
              window-resize-pixelwise t)

;; Various interface modes
(display-time-mode t)
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION DECLARATIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cn/load-config-file (file-name)
  "Load the file FILE-NAME relative to the Emacs config directory."
  (load (file-name-concat user-emacs-directory file-name)))

(defun cn/build-available-p ()
  "Determine whether the variable `exec-path' has a compiler and make.
gcc and clang are valid compilers."
  (and (or (not (null (executable-find "gcc")))
           (not (null (executable-find "clang"))))
       (not (null (executable-find "make")))))

(defun cn/cmake-build-available-p ()
  "Determine whether the variable `exec-path' has a compiler, make, and cmake."
  (and (cn/build-available-p) (not (null (executable-find "cmake")))))

(defun cn/kill-current-buffer ()
  "Prompt to kill the current buffer.
If there are multiple windows open, also delete the window
formerly containing the killed buffer."
  (interactive)
  (when (yes-or-no-p "Kill current buffer?")
    (kill-buffer (current-buffer))
    (when (not (one-window-p))
      (delete-window))))

(defun cn/kill-all-buffers ()
  "Prompt to kill all buffers, leaving an empty *scratch* buffer."
  (interactive)
  (when (yes-or-no-p "Kill all buffers?")
    (progn
      (mapc 'kill-buffer (buffer-list))
      (delete-other-windows))))

(defun cn/kill-all-other-buffers ()
  "Prompt to kill all buffers except for the active buffer."
  (interactive)
  (when (yes-or-no-p "Kill all other buffers?")
    (dolist (buffer (buffer-list))
      (unless (equal buffer (current-buffer))
        (kill-buffer buffer)))))

(defun cn/ansi-colorize-buffer ()
  "Apply ANSI escape code colors to a buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun cn/toggle-window-dedication ()
  "Toggle window dedication for the current window."
  (interactive)
  (progn
    (set-window-dedicated-p
     (selected-window) (not (window-dedicated-p (selected-window))))
    (message
     (format "Window dedication %s."
             (if (window-dedicated-p)
                 "enabled"
               "disabled")))))

(defun cn/unfill-paragraph (&optional region)
  "Take a multi-line paragraph and turn it into a single line of text.
This function is the opposite of `fill-paragraph'.

If REGION is non-nil, unfill all paragraphs in the active region."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 '(t)))
  (let ((fill-column (point-max))
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))



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
(keymap-global-set "C-c d" 'cn/toggle-window-dedication)

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETTINGS FOR MAJOR BUILTIN PACKAGES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-style language settings
(use-package cc-mode
  :custom
  (c-basic-offset 4)
  (c-default-style '((java-mode . "java")
                     (other . "linux")))
  (c-doc-comment-style '((java-mode . javadoc)
                         (other . doxygen))))

;; Compilation settings
(use-package compile
  :custom (compile-command "make -k -j$(nproc)")
  :hook (compilation-filter . ansi-color-compilation-filter))

;; Extra Dired goodies.
(use-package dired-x)

;; Dired settings
(use-package dired
  :custom
  (dired-auto-revert-buffer 'dired-directory-changed-p)
  (dired-do-revert-buffer t)
  (dired-dwim-target t)
  (dired-listing-switches (if (string-match
                               "GNU coreutils"
                               (shell-command-to-string "ls --version"))
                              "-Dahl --color=auto --group-directories-first"
                            "-ahl --group-directories-first"))
  :bind (:map dired-mode-map ("N" . dired-create-empty-file)))

;; Doc view Settings
(use-package doc-view
  :custom
  (doc-view-continuous t)
  (doc-view-resolution 200))

;; Ediff
(use-package ediff
  :custom (ediff-window-setup-function 'ediff-setup-windows-plain))

;; ERC
(use-package erc
  :custom
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 15)
  (erc-hide-list '("JOIN" "PART" "QUIT")))

;;; EWW web browser settings
(use-package eww
  :custom (eww-search-prefix "https://searx.be/search?q="))

;; Extra generic modes
(use-package generic-x)

;; Debugger settings
(use-package gud
  :custom (gdb-show-main t))

;; Hexl hex editor
(use-package hexl
  :custom (hexl-bits 8))

;; Org mode settings
(use-package org
  :custom
  (org-babel-load-languages '((emacs-lisp . t) (python . t) (R . t)))
  (org-confirm-babel-evaluate nil)
  (org-enforce-todo-dependencies t)
  (org-export-with-smart-quotes t)
  (org-latex-compiler "xelatex")
  (org-return-follows-link t))

;; HTML renderer settings
(use-package shr
  :custom
  (shr-max-image-proportion 0.5)
  (shr-max-width 80)
  (shr-use-colors nil)
  (shr-use-fonts nil)
  (shr-width 80))

;; URL settings
(use-package url
  :custom
  (browse-url-browser-function 'browse-web)
  (url-cookie-confirmation nil)
  (url-cookie-file null-device "no cookies"))



;;;;;;;;;;;;;;;;;;;;;;
;; LOAD OTHER FILES ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Load customizations
(load custom-file)

;; Load packages
(cn/load-config-file "packages.el")

;; Load eshell settings
(cn/load-config-file "eshell-config.el")

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
(setq gc-cons-threshold 3200000)
(garbage-collect)
