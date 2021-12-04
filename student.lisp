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
    :accessor grades)))

(defvar *staged-student* (make-instance 'student
                                        :name "Peter"
                                        :grade-level 7
                                        :score 100
                                        :grades '(((0 1) (1 5) (2 3) (3 4)) (0 1 2 3 4))))

(defmacro stage-student (name grade-level score grades)
  "mutates the staged student to have the attributes of a given student"
 `(progn
    (setf (name *staged-student*) ,name)
    (setf (grade-level *staged-student*) ,grade-level)
    (setf (score *staged-student*) ,score)
    (setf (grades *staged-student*) ,grades)))

(defun save-gradebook-to-db (gradebook filename)
  "Saves gradebook to file TODO: Get this working"
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (loop for stu in gradebook do
      (print (grades stu) out)))))
