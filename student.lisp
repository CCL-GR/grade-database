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

(defun save-gradebook-to-db (gradebook filename)
  "Saves gradebook to file"
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (loop for stu in gradebook do
      (print (grades stu) out)))))
