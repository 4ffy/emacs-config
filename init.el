;;;=============================================================================
;;; GENERAL SETTINGS
;;;=============================================================================

;; Me
(setq-default user-full-name "Cameron Norton"
              user-mail-address "cameron.norton@gmail.com")

(defun load-config-file (file-name)
  "Load the file FILE-NAME relative to the Emacs config directory."
  (load (concat user-emacs-directory file-name)))

;; Load theme
(load-config-file "wombat-custom-theme.el")
(load-theme 'wombat-custom t)

;; Load eshell settings
(load-config-file "eshell-config.el")

;; Load macros
(load-config-file "macros.el")

;; Load packages
(load-config-file "packages.el")

;; Load customizations
(setq-default custom-file (concat user-emacs-directory "customize.el"))
(load custom-file)

;; Load mu4e if offlineimap is set up
;; mu also needs to be set up but that's harder to test for
(when (file-exists-p (concat (getenv "HOME") "/.offlineimaprc"))
  (load-config-file "mu4e-config.el"))

;; Add local executable directory to $PATH
(add-to-list 'exec-path "$HOME/.local/bin")

;; Set authentication info file
(setq-default auth-sources
              `((:source ,(concat (getenv "HOME") "/.authinfo.gpg"))))

;; Font selection - use a larger font on laptop
(if (equal "Renda" (system-name))
    (progn
      (setq-default initial-frame-alist '((font . "Liberation Mono 13")))
      (setq-default default-frame-alist '((font . "Liberation Mono 13")))
      (set-frame-font "Liberation Mono 13" t))
  (progn
    (setq-default initial-frame-alist '((font . "Liberation Mono 12")))
    (setq-default default-frame-alist '((font . "Liberation Mono 12")))
    (set-frame-font "Liberation Mono 12" t)))

;; Interface settings
(display-time-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))
(setq-default auto-revert-verbose nil
              completions-detailed t
              cursor-type 'bar
              describe-bindings-outline t
              frame-resize-pixelwise t
              header-line-format t
              inhibit-startup-screen t
              initial-scratch-message nil
              minibuffer-beginning-of-buffer-movement t
              mode-line-compact 'long
              next-line-add-newlines t
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
(add-hook 'before-save-hook 'delete-trailing-lines)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Make scripts executable when saving.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

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
;; I accept any data loss from a crash (until it actually happens, probably).
(setq-default auto-save-mode nil
              auto-save-no-message t
              make-backup-files nil
              backup-directory-alist ;banish to /tmp if files made anyway.
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
              url-cookie-confirmation t
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
              org-latex-compiler "xelatex")

;; Extra generic modes
(require 'generic-x)

(defun kill-current-buffer ()
  "Prompt to kill current buffer."
  (interactive)
  (if (yes-or-no-p "Kill current buffer?")
  (let ((frame (selected-frame)))
    (if (and (frame-live-p frame)
             (not (window-minibuffer-p (frame-selected-window frame))))
        (kill-buffer (current-buffer))
      (abort-recursive-edit)))))

(defun kill-all-buffers ()
  "Prompt to kill all buffers, leaving an empty *scratch* buffer."
  (interactive)
  (if (yes-or-no-p "Kill all buffers?")
        (progn (mapc 'kill-buffer (buffer-list))
               (delete-other-windows))))

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

;; Support ANSI colors in compilation buffer
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

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

;; Make window resizing more coarse
;; There is no shrink window vertically command bound by default
(global-set-key (kbd "C-x ^") (lambda () (interactive)
                                (enlarge-window 10)))
(global-set-key (kbd "C-x {") (lambda () (interactive)
                                (shrink-window-horizontally 10)))
(global-set-key (kbd "C-x }") (lambda () (interactive)
                                (enlarge-window-horizontally 10)))
