(defalias 'cn/format-LaTeX-problems
  (kmacro "C-a \\ p r o b l e m { <right> <backspace> C-e } <return> <return> \\ - <return> C-n"))

(defalias 'cn/insert-javadoc-comment
  (kmacro "/ * * * / C-b C-b <return> <return> C-p * C-p <tab> C-n <tab> C-n <tab> C-p SPC"))

(defalias 'cn/unfill-and-copy-buffer
  (kmacro "C-x h M-x u n f i l l - p a r a g r a p h <return> C-x h M-w C-x h M-q"))
