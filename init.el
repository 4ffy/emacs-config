;;;=============================================================================
;;; GENERAL SETTINGS
;;;=============================================================================

;; Load theme
(load "~/.config/emacs/wombat-custom-theme.el")
(load-theme 'wombat-custom t)

;; Load customizations
(setq-default custom-file "~/.config/emacs/customize.el")
(load custom-file)

;; Load macros
(load "~/.config/emacs/macros.el")

;; Load packages
(load "~/.config/emacs/packages.el")

;; Font selection - use a larger font on laptop
(if (equal "Newton" (system-name))
    (progn
      (setq-default initial-frame-alist '((font . "Liberation Mono 13")))
      (setq-default default-frame-alist '((font . "Liberation Mono 13")))
      (set-frame-font "Liberation Mono 13" t))
  (progn
    (setq-default initial-frame-alist '((font . "Liberation Mono 12")))
    (setq-default default-frame-alist '((font . "Liberation Mono 12")))
    (set-frame-font "Liberation Mono 12" t)))

;; Clean up the interface a bit
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(display-time-mode t)
(scroll-bar-mode -1)
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))
(setq-default inhibit-startup-screen t
              initial-scratch-message nil
              window-resize-pixelwise t
              frame-resize-pixelwise t
              cursor-type 'bar
              header-line-format t
              switch-to-buffer-obey-display-actions t)

;; Org mode for *scratch* buffer
(setq-default initial-major-mode 'org-mode)

;; Global column numbering, with a visible fill column for editing modes
(setq-default fill-column 80
              column-number-mode t)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook 'display-fill-column-indicator-mode)

;; Pair parentheses when programming
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Display line numbers when programming
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Automatic newlines in text modes
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Indentation settings
(setq-default tab-width 4
              indent-tabs-mode nil
              c-default-style "linux"
              c-basic-offset 4)

;; Answer questions with y/n
(setq-default use-short-answers t)

;; Stop cluttering my directories with #autosave# files
;; I accept any data loss from a crash (until it actually happens, probably).
(setq-default make-backup-files nil
              auto-save-mode nil
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
              url-cache-directory "~/.cache/emacs/url/cache"
              url-cookie-confirmation t
              url-cookie-file "/dev/null") ;no cookies

;; List directories first in dired
(setq-default dired-listing-switches
              "-al --color=auto --group-directories-first")

;; Use ibuffer for buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Regex search by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Custom kill buffer commands
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

(global-set-key (kbd "C-c k") 'kill-current-buffer)
(global-set-key (kbd "C-c K") 'kill-all-buffers)

;; Customize eshell prompt
(defun with-foreground (str face-name)
  "Apply the foreground color of the face FACE-NAME to the string STR."
  (propertize str 'face `(:foreground ,(face-foreground face-name))))

(defun my-eshell-prompt ()
  "Custom shell prompt for eshell."
  (concat
   "\n┌ "
   (with-foreground (concat (number-to-string eshell-last-command-status) " ")
                    (if (zerop eshell-last-command-status)
                        'ansi-color-green
                      'ansi-color-red))
   (format-time-string "%T ")
   (with-foreground (concat (user-login-name) "@" (system-name) " ")
                    (if (zerop (user-uid)) 'ansi-color-red 'ansi-color-magenta))
   (with-foreground (eshell/pwd) 'ansi-color-cyan)
   "\n└ "
   (with-foreground (if (zerop (user-uid)) "#" "λ") 'ansi-color-yellow)
   (with-foreground " " 'ansi-color-white)))

(setq-default eshell-prompt-function 'my-eshell-prompt)
(setq-default eshell-prompt-regexp "└ [#λ] ")

;; Open eshell buffers in other windows.
(defun eshell-other-window ()
  "Create or switch to an eshell buffer in another window."
  (interactive)
  (with-current-buffer (get-buffer-create "*eshell*")
    (if (not (equal major-mode 'eshell-mode)) (eshell-mode)))
  (switch-to-buffer-other-window "*eshell*"))

(global-set-key (kbd "C-x 4 s") 'eshell-other-window)

;; Support better colors in eshell.
(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))

;; Support ANSI colors in compilation buffer
(defun ansi-colorize-buffer ()
  (interactive)
  "Apply ANSI escape code colors to a buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'ansi-colorize-buffer)

;; Custom compilation command for cmake projects
(defun cmake-build ()
  "Build a cmake project from its root directory."
  (if (file-exists-p "CMakeLists.txt")
      (compile "cmake -B build && make -k -C build")
    (message "CMakeLists.txt not found.")))

;; Toggle window dedication
(defun window-dedication-toggle ()
  "Toggle window dedication for the current window."
  (interactive)
  (progn
    (set-window-dedicated-p (selected-window)
                            (not (window-dedicated-p (selected-window))))
    (message (format "Window dedication %s." (if (window-dedicated-p)
                                                 "enabled"
                                               "disabled")))))

(global-set-key (kbd "C-c d") 'window-dedication-toggle)
