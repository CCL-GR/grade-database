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
   (comments
    :initarg :comments
    :accessor comments)))

(defvar *student-roster* (list 'a 'b 'c))
(defvar *staged-student* (make-instance 'student
                                        :name ""
                                        :grade-level 0
                                        :score 0
                                        :grades '(0 1 2 3 4)
                                        :comments '((0 1) (1 5) (2 5) (3 5) (4 4))))

(defmacro stage-student (name grade-level score grades comments)
  "mutates the staged student to have the attributes of a given student"
 `(progn
    (setf (name *staged-student*) ,name)
    (setf (grade-level *staged-student*) ,grade-level)
    (setf (score *staged-student*) ,score)
    (setf (grades *staged-student*) ,grades)
    (setf (comments *staged-student*) ,comments)))

(defmacro dump-scores (student grade-set)
  `(format t "~a" (if (equalp ,grade-set 'grades)
                      (grades ,student)
                      (comments ,student))))

(defvar *staged-student-data* (list 'student "some-name" 7 100 '(5 5 5 5 5) '((0 5) (1 5) (2 5) (3 5) (4 5))))
(defvar *staged-student-data-set* (list *staged-student-data* *staged-student-data* *staged-student-data*)) ;currently fake data - check where it's used before shipping

(defmacro instantiate-student (student-data)
  "takes a list of student data and creates a student object - list is to be SOP for creating/saving a student"
  `(let ((name (second ,student-data))
        (grade-level (third ,student-data))
        (score (fourth ,student-data))
        (grades (fifth ,student-data))
        (comments (sixth ,student-data)))
     (make-instance 'student :name name
                             :grade-level grade-level
                             :score score
                             :grades grades
                             :comments comments)))

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
         (comments ,student)))

(defun alist->stu (db-student)
  "Takes a student alist from the *grade-db* and sets the variable *made-student* to them
so that they can be manipulated as a student object"
   (setf *staged-student* (instantiate-student (list 'student
                                                   (second db-student)
                                                   (fourth db-student)
                                                   (tenth db-student)
                                                   (sixth db-student)
                                                   (nth 11 db-student)))))

(defun stu->alist (student)
  "updates comments and grades for given students"
  (progn
    (update (where :name (name student)) :comments (comments student))
    (update (where :name (name student)) :grade (grades student))))

(defun add-student-comment (name comment &optional (student *staged-student*))
  (progn
    (alist->stu (car (select (where :name name))))
    (update (where :name name) :comments (cons comment (comments student)))
    (setf (comments student) (cons comment (comments student)))))
