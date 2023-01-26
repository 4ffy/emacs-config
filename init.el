;;;=============================================================================
;;; GENERAL SETTINGS
;;;=============================================================================

(setq gc-cons-threshold most-positive-fixnum)

;; Me
(setq-default user-full-name "Cameron Norton"
              user-mail-address "cameron.norton@gmail.com")

(defun load-config-file (file-name)
  "Load the file FILE-NAME relative to the Emacs config directory."
  (load (file-name-concat user-emacs-directory file-name)))

;; Load theme
(load-theme 'wombat t)
; Darken the background, lighten the cursor. For some reason,
; `set-face-background' does not work on the cursor face at startup.
(set-face-background 'default "#1b1b1b")
(custom-set-faces `(cursor ((t (:background "#f6f3e8")))))

;; Load eshell settings
(load-config-file "eshell-config.el")

;; Load macros
(load-config-file "macros.el")

;; Load packages
(load-config-file "packages.el")

;; Load customizations
(setq-default custom-file (file-name-concat user-emacs-directory "customize.el"))
(load custom-file)

;; Load mu4e if present and offlineimap is configured
;; mu also needs to be set up but that's harder to test for
(when (and (locate-library "mu4e")
           (file-exists-p (file-name-concat (getenv "HOME") ".offlineimaprc")))
  (load-config-file "mu4e-config.el"))

;; Add local executable directory to $PATH
(add-to-list 'exec-path "$HOME/.local/bin")

;; Set authentication info file
(setq-default auth-sources
              `((:source ,(file-name-concat (getenv "HOME") ".authinfo.gpg"))))

;; Font selection - use a larger font on laptop
(if (equal "Renda" (system-name))
    (progn
      (add-to-list 'initial-frame-alist '(font . "Liberation Mono 13"))
      (add-to-list 'default-frame-alist '(font . "Liberation Mono 13"))
      (set-frame-font "Liberation Mono 13" t))
  (progn
    (add-to-list 'initial-frame-alist '(font . "Liberation Mono 12"))
    (add-to-list 'default-frame-alist '(font . "Liberation Mono 12"))
    (set-frame-font "Liberation Mono 12" t)))

;; Various interface settings
(display-time-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))
(setq-default auto-revert-verbose nil
              completion-ignore-case t
              completions-detailed t
              cursor-type 'bar
              describe-bindings-outline t
              frame-inhibit-implied-resize t
              frame-resize-pixelwise t
              header-line-format t
              inhibit-startup-screen t
              initial-scratch-message nil
              minibuffer-beginning-of-buffer-movement t
              mode-line-compact 'long
              next-line-add-newlines t
              read-buffer-completion-ignore-case t
              read-file-name-completion-ignore-case t
              require-final-newline t
              save-interprogram-paste-before-kill t
              switch-to-buffer-obey-display-actions t
              use-short-answers t
              view-read-only t
              window-resize-pixelwise t)

;; Org mode for *scratch* buffer
(setq-default initial-major-mode 'org-mode)

;; Prog mode hooks
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Text mode hooks
(add-hook 'text-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Strip unnecessary whitespace when saving files
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Make scripts executable when saving.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Support ANSI colors in compilation buffer
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Global column numbering, with a visible fill column for editing modes
(setq-default fill-column 80
              column-number-mode t)

;; Indentation settings
(setq-default tab-width 4
              indent-tabs-mode nil)

;; Settings for C and friends
(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "linux"))
              c-doc-comment-style '((java-mode . javadoc)
                                    (c-mode . doxygen)
                                    (c++-mode . doxygen)))

;; Stop cluttering my directories with #autosave# files
;; I accept any data loss from a crash or unwise `kill-all-buffers'
;; (until it actually happens, probably).
(setq-default auto-save-default nil
              auto-save-mode nil
              auto-save-no-message t
              make-backup-files nil
              ;banish to /tmp if files made anyway.
              backup-directory-alist
              `((".*" . ,temporary-file-directory))
              auto-save-file-name-transforms
              `((".*" ,temporary-file-directory t)))

;; Guess major mode from file name
(setq-default major-mode
              (lambda ()
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

;; EWW web browser settings
(setq-default browse-url-browser-function 'browse-web
              eww-search-prefix "https://searx.be/search?q="
              shr-max-image-proportion 0.5
              shr-max-width 80
              shr-use-colors nil
              shr-use-fonts nil
              shr-width 80
              url-cookie-confirmation nil
              url-cookie-file null-device) ;no cookies

;; Dired settings
(require 'dired-x)
(setq-default dired-do-revert-buffer t
              dired-auto-revert-buffer 'dired-directory-changed-p
              dired-kill-when-opening-new-dired-buffer t
              dired-listing-switches
              "-Dahl --color=auto --group-directories-first")
(define-key dired-mode-map (kbd "N") 'dired-create-empty-file)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Doc view Settings
(setq-default doc-view-continuous t
              doc-view-resolution 200)

;; Org mode settings
(setq-default org-enforce-todo-dependencies t
              org-export-with-smart-quotes t
              org-latex-compiler "xelatex"
              org-return-follows-link t)

;; Extra generic modes
(require 'generic-x)

(setq-default compile-command "make -k -j$(nproc)")

(defun kill-current-buffer ()
  "Prompt to kill the current buffer.
If there are multiple windows open, also delete the window
formerly containing the killed buffer."
  (interactive)
  (when (yes-or-no-p "Kill current buffer?")
    (kill-buffer (current-buffer))
    (when (not (one-window-p)) (delete-window))))

(defun kill-all-buffers ()
  "Prompt to kill all buffers, leaving an empty *scratch* buffer."
  (interactive)
  (if (yes-or-no-p "Kill all buffers?")
        (progn (mapc 'kill-buffer (buffer-list))
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

(defun cmake-build ()
  "Build a cmake project from its root directory."
  (if (file-exists-p "CMakeLists.txt")
      (compile "cmake -B build && make -k -C build")
    (message "CMakeLists.txt not found.")))

(defun window-dedication-toggle ()
  "Toggle window dedication for the current window."
  (interactive)
  (progn
    (set-window-dedicated-p (selected-window)
                            (not (window-dedicated-p (selected-window))))
    (message (format "Window dedication %s." (if (window-dedicated-p)
                                                 "enabled"
                                               "disabled")))))

(defun unfill-paragraph (&optional region)
  "Take a multi-line paragraph and turn it into a single line of text.
This function is the opposite of `fill-paragraph'.

If REGION is non-nil, unfill all paragraphs in the active region."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

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

(setq gc-cons-threshold 800000) ; Default value
(garbage-collect)
