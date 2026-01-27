;;; -*- lexical-binding: t; -*-
(defalias 'cn/format-LaTeX-problems
  (kmacro "C-a \\ p r o b l e m { <right> <backspace> C-e } <return> <return> \\ - <return> C-n"))

(defalias 'cn/insert-doxygen-comment
   (kmacro "/ * ! * / C-b C-b <return> <return> C-p * S-SPC <tab>"))

(defalias 'cn/insert-javadoc-comment
  (kmacro "/ * * * / C-b C-b <return> <return> C-p * C-p <tab> C-n <tab> C-n <tab> C-p SPC"))

(defalias 'cn/rectangle-number-lines
   (kmacro "C-u C-x r N <return> C-a C-f 0")
   "Like `rectangle-number-lines' but pad with `0' instead of ` '.")
