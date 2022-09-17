;;;=============================================================================
;;; ESHELL
;;;=============================================================================

(defun with-foreground (str face-name)
  "Apply the foreground color of the face FACE-NAME to the string STR."
  (propertize str 'face `(:foreground ,(face-foreground face-name))))

;; Customize eshell prompt
(defun my-eshell-prompt ()
  "Custom shell prompt for eshell."
  (concat
   "\n┌ "
   (with-foreground (concat (number-to-string eshell-last-command-status) " ")
                    (if (zerop eshell-last-command-status)
                        'ansi-color-green 'ansi-color-red))
   (format-time-string "%T ")
   (with-foreground (concat (user-login-name) "@" (system-name) " ")
                    (if (zerop (user-uid)) 'ansi-color-red 'ansi-color-magenta))
   (with-foreground (eshell/pwd) 'ansi-color-cyan)
   "\n└ "
   (with-foreground (if (zerop (user-uid)) "#" "λ") 'ansi-color-yellow)
   (with-foreground " " 'ansi-color-white)))

(setq-default eshell-prompt-function 'my-eshell-prompt)
(setq-default eshell-prompt-regexp "└ [#λ] ")

(defun eshell-other-window ()
  "Create or switch to an eshell buffer in another window."
  (interactive)
  (with-current-buffer (get-buffer-create "*eshell*")
    (if (not (equal major-mode 'eshell-mode)) (eshell-mode)))
  (switch-to-buffer-other-window "*eshell*"))

(global-set-key (kbd "C-x 4 s") 'eshell-other-window)

(defun eshell-create-new-buffer ()
  "Create and switch to a new eshell buffer."
  (interactive)
  (let ((new-buffer (generate-new-buffer "*eshell*")))
    (with-current-buffer new-buffer (eshell-mode))
    (switch-to-buffer new-buffer)))

;; Support better colors in eshell.
(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))
