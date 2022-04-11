(defclass student-note ()
  ((student-name
    :initarg :name
    :accessor name)
   (student-grade
    :initarg :grade
    :accessor cnum)
   (notes
    :initarg :notes
    :accessor notes)))
