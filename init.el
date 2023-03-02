;; Inhibit garbage collection during startup.
(setq gc-cons-threshold most-positive-fixnum)

;; Prepare package system.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Prepare use-package.
(add-to-list 'package-pinned-packages '(use-package . "gnu"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; This function has to be at the top.
(defun my-laptop-p ()
  "Determine if the current system is my laptop."
  (equal "Renda" (system-name)))



;;;;;;;;;;;;;;;;;;;;
;; BASIC SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;

;; Load theme, then darken the background and lighten the cursor. For some
;; reason, `set-face-background' does not work on the cursor face at startup,
;; but we can use the function `custom-set-faces' instead.
(load-theme 'wombat t)
(set-face-background 'default "#1b1b1b")
(custom-set-faces `(cursor ((t (:background "#f6f3e8")))))

;; Font selection - use a larger font on laptop
(let* ((font-family "Liberation Mono")
       (font-size (if (my-laptop-p) 13 12))
       (font-string (format "%s %d" font-family font-size)))
  (add-to-list 'initial-frame-alist `(font . ,font-string))
  (add-to-list 'default-frame-alist `(font . ,font-string))
  (set-frame-font font-string t))

;; Basic settings.
(setq-default auto-revert-verbose nil
              auto-save-default nil
              auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
              auto-save-mode nil
              auto-save-no-message t
              backup-directory-alist `((".*" . ,temporary-file-directory))
              browse-url-browser-function 'browse-web
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
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
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

(defun load-config-file (file-name)
  "Load the file FILE-NAME relative to the Emacs config directory."
  (load (file-name-concat user-emacs-directory file-name)))

(defun compiler-available-p ()
  "Determine whether the variable `exec-path' has a compiler.
gcc and cmake are valid compilers."
  (or (not (null (executable-find "gcc")))
      (not (null (executable-find "clang")))))

(defun make-build-available-p ()
  "Determine whether the variable `exec-path' has a compiler and make."
  (and (compiler-available-p)
       (not (null (executable-find "make")))))

(defun cmake-build-available-p ()
  "Determine whether the variable `exec-path' has a compiler, make, and cmake."
  (and (make-build-available-p) (not (null (executable-find "cmake")))))

(defun kill-current-buffer ()
  "Prompt to kill the current buffer.
If there are multiple windows open, also delete the window
formerly containing the killed buffer."
  (interactive)
  (when (yes-or-no-p "Kill current buffer?")
    (kill-buffer (current-buffer))
    (when (not (one-window-p))
      (delete-window))))

(defun kill-all-buffers ()
  "Prompt to kill all buffers, leaving an empty *scratch* buffer."
  (interactive)
  (when (yes-or-no-p "Kill all buffers?")
    (progn
      (mapc 'kill-buffer (buffer-list))
      (delete-other-windows))))

(defun kill-all-other-buffers ()
  "Prompt to kill all buffers except for the active buffer."
  (interactive)
  (when (yes-or-no-p "Kill all other buffers?")
    (dolist (buffer (buffer-list))
      (unless (equal buffer (current-buffer))
        (kill-buffer buffer)))))

(defun ansi-colorize-buffer ()
  "Apply ANSI escape code colors to a buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun window-dedication-toggle ()
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

(defun unfill-paragraph (&optional region)
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

;; Custom kill buffer commands
(global-set-key (kbd "C-c k") 'kill-current-buffer)
(global-set-key (kbd "C-c K") 'kill-all-buffers)

;; Use ibuffer for buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Regex search by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Window dedication toggle
(global-set-key (kbd "C-c d") 'window-dedication-toggle)

;; Unfill paragraph
(global-set-key (kbd "M-Q") 'unfill-paragraph)

;; Toggle menu bar
(global-set-key (kbd "<f12>") (lambda () (interactive) (menu-bar-mode 'toggle)))

;; Make window resizing more coarse
;; There is no shrink window vertically command bound by default
(global-set-key (kbd "C-x ^") (lambda () (interactive)
                                (enlarge-window 10)))
(global-set-key (kbd "C-x {") (lambda () (interactive)
                                (shrink-window-horizontally 10)))
(global-set-key (kbd "C-x }") (lambda () (interactive)
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

;; Dired settings
(use-package dired
  :custom
  (dired-do-revert-buffer t)
  (dired-dwim-target t)
  (dired-auto-revert-buffer 'dired-directory-changed-p)
  (dired-kill-when-opening-new-dired-buffer t)
  :bind (:map dired-mode-map ("N" . dired-create-empty-file))
  :hook (dired-mode . auto-revert-mode)
  :init
  (setq-default dired-listing-switches
                (if (equal system-type 'gnu/linux)
                    "-Dahl --color=auto --group-directories-first"
                  "-ahl --group-directories-first")))

;; Extra Dired goodies
(use-package dired-x)

;; Doc view Settings
(use-package doc-view
  :custom
  (doc-view-continuous t)
  (doc-view-resolution 200))

;;; EWW web browser settings
(use-package eww
  :custom (eww-search-prefix "https://searx.be/search?q="))

;; Extra generic modes
(use-package generic-x)

;; Debugger settings
(use-package gud
  :custom (gdb-show-main t))

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
  (url-cookie-confirmation nil)
  (url-cookie-file null-device "no cookies"))



;;;;;;;;;;;;;;;;;;;;;;
;; LOAD OTHER FILES ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Load customizations
(load custom-file)

;; Load packages
(load-config-file "packages.el")

;; Load eshell settings
(load-config-file "eshell-config.el")

;; Load macros
(load-config-file "macros.el")

;; Load mu4e if present and offlineimap is configured
;; mu also needs to be set up but that's harder to test for
(when (and (locate-library "mu4e")
           (file-exists-p (file-name-concat (getenv "HOME") ".offlineimaprc")))
  (load-config-file "mu4e-config.el"))



;; Reset garbage collection.
(setq gc-cons-threshold 800000)
(garbage-collect)
