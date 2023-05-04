;;;=============================================================================
;;; MU4E CONFIG
;;;=============================================================================

;;; Basic mail settings
(use-package emacs
  :custom
  (message-kill-buffer-on-exit t)
  (starttls-use-gnutls t))

;;; SMTP settings
(use-package smtpmail
  :custom
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-auth-credentials '(("smtp.gmail.com" 587 user-mail-address nil)))
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))))

;;; Set up mu4e itself.
(use-package mu4e
  :defer t
  :commands mu4e
  :custom
  ;; Set up mail folders.
  (mu4e-drafts-folder "/[Gmail].Drafts")
  (mu4e-sent-folder   "/[Gmail].Sent Mail")
  (mu4e-trash-folder  "/[Gmail].Trash")
  ;; Set up mail folder shortcuts.
  (mu4e-maildir-shortcuts
   '((:maildir "/INBOX"             :key ?i)
     (:maildir "/[Gmail].Drafts"    :key ?d)
     (:maildir "/[Gmail].Sent Mail" :key ?s)
     (:maildir "/[Gmail].Trash"     :key ?t)))
  ;; Other mu4e settings.
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-completing-read-function 'completing-read)
  (mu4e-compose-signature user-full-name)
  (mu4e-get-mail-command "offlineimap")
  (mu4e-sent-messages-behavior 'delete)
  :config
  ;; Open mu4e in same window.
  (add-to-list 'display-buffer-alist
               `(,(regexp-quote mu4e-main-buffer-name)
                 (display-buffer-same-window))))
