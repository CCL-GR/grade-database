(defclass gradebook ()
  ((set-of-grades
    :initarg :set-of-grades
    :accessor set-of-grades)
   (student-name
    :initarg :name
    :accessor name)
   (grade-level
    :initarg :grade-level
    :accessor grade-level)))

; a test student's gradebook
(defvar *staged-gradebook-entry* (make-instance 'gradebook
                                        :name "Peter"
                                        :set-of-grades '(((0 1) (1 5) (2 3) (3 4)) (0 1 2 3 4))
                                        :grade-level 7))

