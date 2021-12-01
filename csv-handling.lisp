(defvar *grade-db-csv* nil)

(defmacro stage-csv (name)
  "use name as: #Pfilename.csv"
  `(setf *grade-db-csv* (cl-csv:read-csv ,name)))

(defun dump-csv-format ()
  "Dumps the student database into a CSV-friendly output"
  (dolist (student *grade-db*)
    (format t "~a," (getf student :name))
    (format t "~{~a~^,~}~%" (getf student :grade))))

(defmacro get-csv-first-word (coerced-csv)
  `(format nil "~a" (until-first-space (caar ,coerced-csv))))

(defmacro get-csv-nth-word (n coerced-csv)
  `(format nil "~a" (nth ,n (car ,coerced-csv))))

(defmacro get-csv-name (coerced-csv)
  `(format nil "~a ~a" (get-csv-nth-word 1 ,coerced-csv) (get-csv-first-word ,coerced-csv)))

(defmacro get-student-by-pinyin (pinyin attr)
  `(getf (car (select (where :pinyin ,pinyin))) ,attr))

(defun make-additional-grade-using-pinyin (pinyin assignment-grade)
  (setf (get-student-by-pinyin pinyin :grade)
        (cons assignment-grade
              (get-student-by-pinyin pinyin :grade))))

(defmacro find-student-grades-by-csv (student)
  `(format nil "~a" (get-student (get-csv-name ,student) :grade)))

(defun push-csv-grades (&optional (coerced-csv *grade-db-csv*))
  (loop for student in coerced-csv
        do (make-additional-grade-using-pinyin (flip-name (car student)) (parse-integer (second student)))))

(defun current-date-string ()
  "Returns current date as a string."
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
                       (get-decoded-time)
    (declare (ignore sec min hr dow dst-p tz))
    (format nil "~a-~a-~a" yr mon day)))
