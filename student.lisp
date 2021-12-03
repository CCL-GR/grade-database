(defclass student ()
  ((name
   :initarg :name
   :accessor name)
   (grade-level
   :initarg :grade-level
   :accessor grade-level)
   (score
    :initarg :score
    :accessor score)))
