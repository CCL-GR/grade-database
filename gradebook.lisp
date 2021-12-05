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
                                        :name "Peter"
                                        :set-of-grades '(((0 1) (1 5) (2 3) (3 4)) (0 1 2 3 4))
                                        :grade-level 7))

(defmacro stage-gradebook-entry (student)
  "mutates the gradebook entry to be a specific student object representation"
  `(progn
     (setf (name *staged-gradebook-entry*) (name ,student))
     (setf (set-of-grades *staged-gradebook-entry*) (grades ,student))
     (setf (grade-level *staged-gradebook-entry*) (grade-level ,student))))

(defun add-student-to-gradebook (student)
  "Adds a single student object to the gradebook"
  (progn
    (stage-gradebook-entry student)
    (setf *gradebook* (cons *staged-gradebook-entry* *gradebook*))))

(defun save-gradebook-to-db (gradebook filename)
  "Saves gradebook to file TODO: Get this working"
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (loop for stu in gradebook do
      (print (grades stu) out)))))
