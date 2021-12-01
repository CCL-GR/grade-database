(defmacro ls () (uiop:run-program "ls" :output :string))
