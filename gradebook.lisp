(defclass gradebook ()
  ((set-of-grades
    :initarg nil
    :accessor gradebook)
   (student-name
    :initarg :name
    :accessor name)
   (grade-level
    :initarg nil
    :accessor grade-level)))
