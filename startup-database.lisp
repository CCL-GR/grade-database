(defun abspath
       (path-string &optional (dir-name (uiop:getcwd)))
   (uiop:unix-namestring
    (uiop:ensure-absolute-pathname
     (uiop:merge-pathnames*
      (uiop:parse-unix-namestring path-string))
     dir-name)))

(load (abspath "/home/carson/code/lisp/string-libs.lisp"))
(ql:quickload :cl-csv)
(load (abspath "/home/carson/code/lisp/csv-handling.lisp"))
(load (abspath "/home/carson/code/lisp/grade-database.lisp"))
