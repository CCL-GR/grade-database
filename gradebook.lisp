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
(defvar *gradebook* nil)
(defvar *staged-gradebook-entry* (make-instance 'gradebook
                                        :name ""
                                        :set-of-grades '(((0 1) (1 5) (2 3) (3 4)) (0 1 2 3 4))
                                        :grade-level 0))

(defmacro stage-gradebook-entry (student)
  "mutates the gradebook entry to be a specific student object representation"
  `(progn
     (setf (name *staged-gradebook-entry*) (name ,student))
     (setf (set-of-grades *staged-gradebook-entry*) (cons (concept-quiz ,student) (grades ,student)))
     (setf (grade-level *staged-gradebook-entry*) (grade-level ,student))))
