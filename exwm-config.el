(require 'exwm)
(require 'exwm-systemtray)

(setq-default exwm-workspace-number 4)

(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

(setq-default exwm-input-global-keys
              `(
                (,(kbd "s-SPC") . exwm-reset)
                (,(kbd "s-W") . exwm-workspace-switch)
                (,(kbd "s-r") . dmenu)
                (,(kbd "s-w") . (lambda () (interactive)
                                  (start-process-shell-command
                                   "librewolf" nil "librewolf")))
                (,(kbd "s-f") . exwm-layout-toggle-fullscreen)
                (,(kbd "s-&") . (lambda (command)
                                  (interactive (list (read-shell-command "$ ")))))
                ,@(mapcar (lambda (i)
                            `(,(kbd (format "s-%d" i)) .
                              (lambda ()
                                (interactive)
                                (exwm-workspace-switch-create ,(% (1- i) 10)))))
                          (number-sequence 0 9))
                ))

(setq-default exwm-input-simulation-keys
              `(
                (,(kbd "C-b") . [left])
                (,(kbd "C-f") . [right])
                (,(kbd "C-p") . [up])
                (,(kbd "C-n") . [down])
                (,(kbd "C-a") . [home])
                (,(kbd "C-e") . [end])
                (,(kbd "M-v") . [prior])
                (,(kbd "C-v") . [next])
                (,(kbd "C-d") . [delete])
                (,(kbd "C-k") . [S-end delete])
                (,(kbd "C-w") . [?\C-x])
                (,(kbd "M-w") . [?\C-c])
                (,(kbd "C-y") . [?\C-v])
                (,(kbd "C-/") . [?\C-z])
                (,(kbd "C-s") . [?\C-f])
                (,(kbd "C-x h") . [?\C-a])
                (,(kbd "M-<") . [home])
                (,(kbd "M->") . [end])
                ))

(menu-bar-mode)
(exwm-systemtray-enable)
(exwm-enable)

(shell-command "setxkbmap -option 'ctrl:nocaps'")
(start-process-shell-command "nm-applet" nil "nm-applet")
(start-process-shell-command "volumeicon" nil "volumeicon")
