(fset 'format-LaTeX-problems
      (kmacro-lambda-form [?\C-a ?\\ ?p ?r ?o ?b ?l ?e ?m ?\{ right backspace ?\C-e ?\} return return ?\\ ?- return ?\C-n] 0 "%d"))

(fset 'insert-javadoc-comment
   (kmacro-lambda-form [?\C-a ?/ ?* ?* ?* ?/ ?\C-b ?\C-b return return ?\C-p ?* ?\C-p tab ?\C-n tab ?\C-n tab ?\C-p ? ] 0 "%d"))

