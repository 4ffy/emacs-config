;;; -*- lexical-binding: t; -*-

(defvar-local cn/mode-line-input-method
    `(current-input-method
      (:propertize
       ("" current-input-method-title) help-echo
       (concat
        "Current input method: " current-input-method
        "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method")
       local-map ,mode-line-input-method-map mouse-face mode-line-highlight))
  "Mode line construct to report the current input method.
Isolated from `mode-line-mule-info', without the multilingual environment.")
(put 'cn/mode-line-input-method 'risky-local-variable t)

(defvar-local cn/mode-line-mule-info
    `(,(propertize "%z"
                   'help-echo
                   'mode-line-mule-info-help-echo
                   'mouse-face
                   'mode-line-highlight
                   'local-map
                   mode-line-coding-system-map)
      (:eval (mode-line-eol-desc)))
  "Mode line construct to report the multilingual environment.
Isolated from `mode-line-mule-info', without the input method.")
(put 'cn/mode-line-mule-info 'risky-local-variable t)

(defun cn/mode-line-modified ()
  "Build a mode-line construct for buffer modification/read-only state.
Modified from simple-modeline package."
  (if (not (string-match-p "\\*.*\\*" (buffer-name)))
      (let* ((read-only (and buffer-read-only (buffer-file-name)))
             (modified (buffer-modified-p)))
        (propertize
         (if read-only "" (if modified "●" "○"))
         'face `(:inherit
                 ,(if modified
                      'font-lock-variable-name-face
                    (if read-only
                        'error
                      'mode-line)))
         'help-echo (format
                     "Buffer is %s and %smodified\nmouse-1: Toggle read-only status."
                     (if read-only
                         "read-only"
                       "writable")
                     (if modified
                         ""
                       "not "))
         'local-map (let ((map (make-sparse-keymap)))
                      (define-key
                       map (vector 'mode-line 'mouse-1)
                       (lambda (event)
                         (interactive "e")
                         (with-selected-window (posn-window (event-start event))
                           (read-only-mode 'toggle))))
                      map)
         'mouse-face 'mode-line-highlight))))

(use-package emacs
  :custom
  (mode-line-format
   '(" "
     (:eval (cn/mode-line-modified))
     mode-line-window-dedicated
     " "
     (:propertize "%b" face mode-line-buffer-id)
     " "
     mode-line-position
     mode-line-format-right-align
     cn/mode-line-input-method
     " "
     cn/mode-line-mule-info
     (vc-mode vc-mode)
     " "
     mode-line-modes
     mode-line-misc-info
     " "))
  (mode-line-percent-position nil)
  (mode-line-position-column-line-format '("%l:%c")))
