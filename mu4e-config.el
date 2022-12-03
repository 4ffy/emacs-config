;;;=============================================================================
;;; MU4E CONFIG
;;;=============================================================================

(require 'mu4e)
(require 'smtpmail)

;; User agent
(setq-default mail-user-agent 'mu4e-user-agent)

;; Email folders
(setq-default mu4e-drafts-folder "/[Gmail].Drafts"
              mu4e-sent-folder   "/[Gmail].Sent Mail"
              mu4e-trash-folder  "/[Gmail].Trash")

;; Don't save messages to "Sent Mail" as Gmail already does this
(setq-default mu4e-sent-messages-behavior 'delete)

;; Quick shocuts to mail folders
(setq-default mu4e-maildir-shortcuts
              '((:maildir "/INBOX"             :key ?i)
                (:maildir "/[Gmail].Drafts"    :key ?d)
                (:maildir "/[Gmail].Sent Mail" :key ?s)
                (:maildir "/[Gmail].Trash"     :key ?t)))

;; Set up mail fetching
(setq-default mu4e-get-mail-command "offlineimap")

;; Sending mail
(setq-default message-send-mail-function 'smtpmail-send-it
              mu4e-compose-signature user-full-name
              smtpmail-auth-credentials '(("smtp.gmail.com" 587 user-mail-address nil))
              smtpmail-default-smtp-server "smtp.gmail.com"
              smtpmail-smtp-server "smtp.gmail.com"
              smtpmail-smtp-service 587
              smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
              starttls-use-gnutls t)

;; Close buffer on message exit
(setq-default message-kill-buffer-on-exit t)

;; Don't use ido
(setq-default mu4e-completing-read-function nil)
