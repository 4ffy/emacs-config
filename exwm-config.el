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

(setq-default exwm-input-simulation-keys
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
                (,(kbd "C-k") . ,(kbd "S-<end> <delete>"))
                (,(kbd "C-w") . ,(kbd "C-x"))
                (,(kbd "M-w") . ,(kbd "C-c"))
                (,(kbd "C-y") . ,(kbd "C-v"))
                (,(kbd "C-x h") . ,(kbd "C-a"))
                ; actions
                (,(kbd "C-s") . ,(kbd "C-f"))
                (,(kbd "C-/") . ,(kbd "C-z"))
                (,(kbd "C-x C-s") . ,(kbd "C-s"))
                ))

(menu-bar-mode 1)
(exwm-systemtray-enable)
(exwm-enable)

(when (equal "Renda" (system-name)) (display-battery-mode 1))

(shell-command "setxkbmap -option 'ctrl:nocaps'")
(shell-command "setxkbmap -option 'shift:both_capslock'")
(start-process-shell-command "nm-applet" nil "nm-applet")
(start-process-shell-command "volumeicon" nil "volumeicon")
