(defclass student ()
  ((name
   :initarg :name
   :accessor name)
   (grade-level
   :initarg :grade-level
   :accessor grade-level)
   (score
    :initarg :score
    :accessor score)
   (grades
    :initarg :grades
    :accessor grades)
   (concept-scores
    :initarg :concept-scores
    :accessor concept-scores)))

(defvar *student-roster* (list 'a 'b 'c))
(defvar *staged-student* (make-instance 'student
                                        :name ""
                                        :grade-level 0
                                        :score 0
                                        :grades '(0 1 2 3 4)
                                        :concept-scores '((0 1) (1 5) (2 5) (3 5) (4 4))))

(defmacro stage-student (name grade-level score grades concept-scores)
  "mutates the staged student to have the attributes of a given student"
 `(progn
    (setf (name *staged-student*) ,name)
    (setf (grade-level *staged-student*) ,grade-level)
    (setf (score *staged-student*) ,score)
    (setf (grades *staged-student*) ,grades)
    (setf (concept-scores *staged-student*) ,concept-scores)))

(defmacro dump-scores (student grade-set)
  `(format t "~a" (if (equalp ,grade-set 'grades)
                      (grades ,student)
                      (concept-scores ,student))))

(defvar *staged-student-data* (list 'student "some-name" 7 100 '(5 5 5 5 5) '((0 5) (1 5) (2 5) (3 5) (4 5))))
(defvar *staged-student-data-set* (list *staged-student-data* *staged-student-data* *staged-student-data*))

(defmacro instantiate-student (student-data)
  "takes a list of student data and creates a student object - list is to be SOP for creating/saving a student"
  `(let ((name (second ,student-data))
        (grade-level (third ,student-data))
        (score (fourth ,student-data))
        (grades (fifth ,student-data))
        (concept-scores (sixth ,student-data)))
     (make-instance 'student :name name
                             :grade-level grade-level
                             :score score
                             :grades grades
                             :concept-scores concept-scores)))

(defun load-student (student student-data)
  "Used to mapcar students into the roster"
  (setf student (instantiate-student student-data)))

(defun load-student-roster ()
  "takes in the data for the students and puts them in the roster"
  (setf *student-roster* (mapcar #'load-student *student-roster* *staged-student-data-set*)))

(defmacro dump-student-data (student)
  "takes in a student object and converts it to a list"
  `(list 'student
         (name ,student)
         (grade-level ,student)
         (score ,student)
         (grades ,student)
         (concept-scores ,student)))
