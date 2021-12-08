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

(defvar *staged-student-data* (list 'student "" 7 100 '(5 5 5 5 5) '((0 5) (1 5) (2 5) (3 5) (4 5))))

(defmacro instantiate-student (roster student-data)
  "Sets the first value of the student-roster (to be expanded for loading and saving purposes)"
  `(setf (car ,roster) (make-instance 'student :name "AAA"
                                        :grade-level 7
                                        :score 100
                                        :grades '(0 1 2 3 4)
                                        :concept-scores '((0 1) (1 5) (2 5) (3 5) (4 4)))))
