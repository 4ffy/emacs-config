;;;=============================================================================
;;; ESHELL CONFIG
;;;=============================================================================

(defun cn/with-foreground (str face-name)
  "Apply the foreground color of the face FACE-NAME to the string STR."
  (propertize str 'face `(:foreground ,(face-foreground face-name))))

;; Customize eshell prompt
(defun cn/eshell-prompt ()
  "Custom shell prompt for eshell."
  (concat
   "\n┌ "
   (cn/with-foreground
    (concat (number-to-string eshell-last-command-status) " ")
    (if (zerop eshell-last-command-status)
        'ansi-color-green
      'ansi-color-red))
   (format-time-string "%T ")
   (cn/with-foreground
    (concat (user-login-name) "@" (system-name) " ")
    (if (zerop (user-uid))
        'ansi-color-red
      'ansi-color-magenta))
   (cn/with-foreground (eshell/pwd) 'ansi-color-cyan)
   "\n└ "
   (cn/with-foreground
    (if (zerop (user-uid))
        "#"
      "λ")
    'ansi-color-yellow)
   (cn/with-foreground " " 'ansi-color-white)))

(setq-default eshell-prompt-function 'cn/eshell-prompt)
(setq-default eshell-prompt-regexp "└ [#λ] ")

(defun cn/eshell-other-window ()
  "Create or switch to an eshell buffer in another window."
  (interactive)
  (with-current-buffer (get-buffer-create "*eshell*")
    (unless (equal major-mode 'eshell-mode)
      (eshell-mode)))
  (switch-to-buffer-other-window "*eshell*"))

(global-set-key (kbd "C-x 4 s") 'cn/eshell-other-window)

(defun cn/eshell-create-new-buffer ()
  "Create and switch to a new eshell buffer."
  (interactive)
  (let ((new-buffer (generate-new-buffer "*eshell*")))
    (with-current-buffer new-buffer
      (eshell-mode))
    (switch-to-buffer new-buffer)))

;; Support better colors in eshell.
(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))
