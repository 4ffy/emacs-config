(fset 'format-LaTeX-problems
      (kmacro-lambda-form [?\C-a ?\\ ?p ?r ?o ?b ?l ?e ?m ?\{ right backspace ?\C-e ?\} return return ?\\ ?- return ?\C-n] 0 "%d"))

(fset 'insert-javadoc-comment
   (kmacro-lambda-form [?/ ?* ?* ?* ?/ ?\C-b ?\C-b return return ?\C-p ?* ?\C-p tab ?\C-n tab ?\C-n tab ?\C-p ? ] 0 "%d"))

(fset 'calc-full-window
   (kmacro-lambda-form [?\C-x ?1 ?\M-x ?c ?a ?l ?c return ?\C-x ?o ?\C-x ?o ?\C-x ?0 ?\C-x ?o] 0 "%d"))
