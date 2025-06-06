;;; -*- lexical-binding: t; -*-
(require 'exwm)
(require 'exwm-systemtray)

(setopt exwm-workspace-number 4)

(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

(setopt exwm-input-global-keys
        `(
          (,(kbd "s-SPC") . exwm-reset)
          (,(kbd "s-W") . exwm-workspace-switch)
          (,(kbd "s-r") . dmenu)
          (,(kbd "s-w") . (lambda () (interactive)
                            (start-process-shell-command
                             "librewolf" nil "librewolf")))
          (,(kbd "s-f") . exwm-layout-toggle-fullscreen)
          (,(kbd "s-&") . (lambda (command)
                            (interactive (list (read-shell-command "$ ")))
                            (start-process-shell-command
                             command nil command)))
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" (% i 10))) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,(1- i)))))
                    (number-sequence 1 10))
          ))

(setopt exwm-input-simulation-keys
        `(
          ; movement
          (,(kbd "C-b") . ,(kbd "<left>"))
          (,(kbd "C-f") . ,(kbd "<right>"))
          (,(kbd "C-p") . ,(kbd "<up>"))
          (,(kbd "C-n") . ,(kbd "<down>"))
          (,(kbd "C-a") . ,(kbd "<home>"))
          (,(kbd "C-e") . ,(kbd "<end>"))
          (,(kbd "C-v") . ,(kbd "<next>"))
          (,(kbd "M-v") . ,(kbd "<prior>"))
          (,(kbd "M-f") . ,(kbd "C-<right>"))
          (,(kbd "M-b") . ,(kbd "C-<left>"))
          (,(kbd "M-<") . ,(kbd "<home>"))
          (,(kbd "M->") . ,(kbd "<end>"))
          ; region, killing, and yanking
          (,(kbd "C-d") . ,(kbd "<delete>"))
          (,(kbd "M-d") . ,(kbd "C-S-<right> C-x"))
          (,(kbd "C-k") . ,(kbd "S-<end> C-x"))
          (,(kbd "C-w") . ,(kbd "C-x"))
          (,(kbd "M-w") . ,(kbd "C-c"))
          (,(kbd "C-y") . ,(kbd "C-v"))
          (,(kbd "C-x h") . ,(kbd "C-a"))
          ; actions
          (,(kbd "C-s") . ,(kbd "C-f"))
          (,(kbd "C-/") . ,(kbd "C-z"))
          (,(kbd "C-x C-s") . ,(kbd "C-s"))
          ))

(when (cn/my-laptop-p) (display-battery-mode 1))
(menu-bar-mode 1)
(exwm-systemtray-enable)
(exwm-enable)

(when (string-match
       "enabled"
       (shell-command-to-string "systemctl --user is-enabled emacs.service"))
  (shell-command "systemctl --user stop emacs.service"))

(when (string-match
       "enabled"
       (shell-command-to-string "systemctl --user is-enabled xremap.service"))
  (shell-command "systemctl --user stop xremap.service"))

(shell-command "setxkbmap -option 'ctrl:nocaps'")
(shell-command "setxkbmap -option 'shift:both_capslock'")
(start-process-shell-command "nm-applet" nil "nm-applet")
(start-process-shell-command "volumeicon" nil "volumeicon")
