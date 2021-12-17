(defclass gradebook ()
   ((students
    :initarg :students
    :accessor students)
   (grade-level
    :initarg :grade-level
    :accessor grade-level)))

(defvar *c1* (make-instance 'concept :concept-number 1 :concept-name "me1"))
(defvar *c2* (make-instance 'concept :concept-number 2 :concept-name "me2"))
(defvar *c3* (make-instance 'concept :concept-number 3 :concept-name "me3"))
(defvar *c4* (make-instance 'concept :concept-number 4 :concept-name "me4"))
(defvar *c5* (make-instance 'concept :concept-number 5 :concept-name "me5"))
(defvar *c6* (make-instance 'concept :concept-number 6 :concept-name "me6"))
(defvar *c7* (make-instance 'concept :concept-number 7 :concept-name "me7"))


(defvar *a* (make-instance 'student :name "a" :grade-level 7 :score 1 :grades '(1 2 3 4 5) :concept-scores (list *c1* *c2* *c3* *c4*)))
(defvar *b* (make-instance 'student :name "b" :grade-level 7 :score 2 :grades '(2 2 3 4 5) :concept-scores (list *c3* *c4* *c5* *c6*)))
(defvar *c* (make-instance 'student :name "c" :grade-level 7 :score 3 :grades '(3 3 3 4 5) :concept-scores (list *c7* *c6* *c5* *c4*)))


; a test student's gradebook
(defvar *gradebook* nil)
(defvar *staged-gradebook-entry* (make-instance 'gradebook
                                                :students (list *a* *b* *c*)
                                                :grade-level 7))

(defmacro stage-gradebook-entry (student)
  "mutates the gradebook entry to be a specific student object representation"
  `(progn
     (setf (name *staged-gradebook-entry*) (name ,student))
     (setf (set-of-grades *staged-gradebook-entry*) (cons (concept-quiz ,student) (grades ,student)))
     (setf (grade-level *staged-gradebook-entry*) (grade-level ,student))))

(defun concept-score-entered-p (num &optional (gradebook *staged-gradebook-entry*))
  "returns t if all students in the gradebook have the concept-score listed, nil if one is missing it"
  (not (loop for stu in (students gradebook) thereis (not (exists-concept-p num (concept-scores stu))))))
