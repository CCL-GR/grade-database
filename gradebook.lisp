(defclass gradebook ()
  ((gradebook
    :initarg nil
    :accessor gradebook)
   (name
    :initarg :name
    :accessor student-name)
   (grade-level
    :initarg nil
    :accessor grade-level)))
