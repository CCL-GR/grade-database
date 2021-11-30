(defvar *master-grades* (list 15 27 11 12 18 5 3 3 5 5 5 10 5 1 2 2))
(defvar *grades* nil) ;Frequently clobbered to make grade variable for student
(defvar *names* nil) ;used to hold a place for names on occassion
(defvar *scores* nil) ;same as grades, but scores
(defvar *grade-db* nil) ;The database that holds all students
(defvar *grade-db-file* "fall_grades2021.db")
(defvar *classes-held* (list nil))

(defvar *class-dates* (list "2021-6-2" "2021-6-1" "2021-5-27" "2021-5-26" "2021-5-25"))

(defun make-student (name class score grade pinyin)
  "creates a 'student object' to be the basis of the database"
  (list :name name :class class :score score :grade grade :attendance '(1 1) :pinyin pinyin))

(defun add-student (student) (push student *grade-db*)) ;Adds single stu

(defun select (selector-fn)
  "creates (list students) who pass selector-fn's boolean"
  (remove-if-not selector-fn *grade-db*))

(defun where (&key name class score grade pinyin)
  "Creates boolean of whether or not plist keys match provided search term"
  #'(lambda (student)
      (and
       (if name  (equal (getf student :name)  name)  t)
       (if class (equal (getf student :class) class) t)
       (if score (equal (getf student :score) score) t)
       (if grade (equal (getf student :grade) grade) t)
       (if pinyin (equal (getf student :pinyin) pinyin) t))))

(defun select-attribute (attr &optional (db *grade-db*))
  "Gets a plist of just the attributes for students in order"
  (if (equal attr :class)
      (remove-duplicates (loop for students in db collect (getf students attr)))
      (loop for students in db collect (getf students attr))))

(defun prompt-read (prompt)
  "Generic read prompt - used in looping grade pushing"
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun load-grade-db (filename)
  "Allows db access - NOTE Clobbers *grade-db*"
  (progn
    (setf *grade-db-file* filename)
    (with-open-file (in filename)
      (with-standard-io-syntax
        (setf *grade-db* (read in))))))

(defun update (selector-fn &key name class score grade pinyin)
  "NOTE: Clobbers a single value in the loaded database"
  (setf *grade-db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if name (setf (getf row :name) name))
               (if class (setf (getf row :class) class))
               (if score (setf (getf row :score) score))
               (if grade (setf (getf row :grade) grade))
               (if pinyin (setf (getf row :pinyin) pinyin)))
             row)
         *grade-db*)))

(defun pop-attribute (attr)
  "Grabs the attribute-value pair and moves them to the front of the list"
  #'(lambda (student)
      (let ((attribute-value-pair (list attr (getf student attr))))
        (progn
          (remf student attr)
          (append attribute-value-pair student)))))

(defun reorder-student (attr student)
  "NOTE: Clobbers *grade-db* sets single student attribute to the front"
  (setf *grade-db* (cons (funcall (pop-attribute attr) student)
                           (remove student *grade-db*))))

(defun pop-column (attr db)
  "NOTE: Clobbers. Moves an attribute to the front of the plist for all students in db"
  (if (null (car db))
      nil
      (progn
        (reorder-student attr (car db))
        (pop-column attr (cdr db)))))

(defmacro reorder-plists (order-list db)
  "Main reorder plist fn NOTE: Clobber them in *grade-db*"
  `(loop for attr in (reverse ,order-list) do
        (pop-column attr ,db)))

(defmacro reset-plists ()
  "NOTE: Clobbers *grade-db*  Back to initialization form"
  `(reorder-plists (list :name :class :score :grade :attendance :pinyin) *grade-db*))

(defmacro get-student (name attr)
  "returns a single student's attribute - :grade, :class, :score, :pinyin, :name, anything"
  `(getf (car (select (where :name ,name))) ,attr))

(defun make-additional-grade (name assignment-grade)
  "Pushes a single additional grade to a studnet's name NOTE: main component of ADD-GRADES"
  (setf (get-student name :grade)
        (cons assignment-grade
              (get-student name :grade))))

(defun prompt-for-grade ()
  "The prompt for the ADD-GRADES func"
  (or (parse-integer (prompt-read "Assignment Grade") :junk-allowed t) 0))

(defun pinyin-add-grades ()
  "Lets you add in grades using pinyin name - just convenient"
  (loop (make-additional-grade-using-pinyin (prompt-read "Pinyin") (prompt-for-grade))
        (if (not (y-or-n-p "another? [y/n]: ")) (return))))

(defun add-grades ()
  "Loop to add multiple grades to database - Choose stu name, and grade to add in prompt"
  (loop (make-additional-grade (prompt-read "Name") (prompt-for-grade))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun prompt-for-student ()
  "Used in ADD-STUDENT-DB to create students dynamically"
  (make-student
   (prompt-read "Name")
   (or (parse-integer (prompt-read "Class Number") :junk-allowed t) 0)
   (or (parse-integer (prompt-read "Score") :junk-allowed t) 0)
   (or (parse-integer (prompt-read "Grade") :junk-allowed t) 0)
   (prompt-read "Pinyin")))

(defun add-student-db ()
  "Loop for pushing multiple new students into database"
  (loop (add-student (prompt-for-student))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))


(defmacro avg-grade (name)
  "returns the average of all numbers from :GRADE"
  `(let* ((grades (get-student ,name :grade))
          (len (length grades))
          (total (float (loop for i in grades sum i))))
     (if (= len 0) 0
         (/ total len))))

(defun update-all-grade-averages (&optional (db *grade-db*))
  "Sets the :SCORE for all students to an average of the :GRADE list"
  (if (not (null db))
      (progn
        (let ((student (first db)))
          (setf (getf student :score) (avg-grade (getf student :name))))
        (update-all-grade-averages (cdr db)))
      nil))

(defmacro get-class (class)
  "returns the students in a chosen class NOTE: DSL helper"
  `(select (where :class ,class)))

(defmacro random-student-from-class (class)
  "picks a student at random from the class"
  `(nth (random (length (get-class ,class))) (get-class ,class)))

(defmacro random-item-from-list (available)
  "returns an item (student) from a set of (available students) - used as helper for random-student"
  `(nth (random (length ,available)) ,available))

(defmacro present-students (class)
  "returns a list of the students who attended the most recent class (today)"
    `(remove nil (loop for student in (get-class ,class) collect
        (if (= (car (getf student :attendance)) 1)
            student
            nil))))

(defun random-student-present-name (class &optional (whole-student-list-p nil))
  "picks a student at random who is present today"
  (if whole-student-list-p
      (random-item-from-list (present-students class))
      (getf (random-item-from-list (present-students class)) :name)))

(defmacro get-class-average (class)
  "Returns the average for a class of students"
  `(/ (loop for students in (get-class ,class) sum (getf students :score)) (float (length (get-class ,class)))))

(defmacro fill-a-zero (name num)
  "Pushes a grade of zero into the student's grade list"
  `(let ((x (length (get-student ,name :grade))))
        (if (< x ,num) (make-additional-grade ,name 0) nil)))

(defun fill-grades (db)
  "Fills in grades with 0 to finish grading after all completed grades are
input"
  (let ((master-grade-length (length *master-grades*))
        (student-grade-length (length (getf (car db) :grade))))
    (if (not (null db))
        (if (>= master-grade-length student-grade-length)
            (progn
              (fill-a-zero (getf (car db) :name) master-grade-length)
              (if (> master-grade-length student-grade-length)
                  (fill-grades db)
                  (fill-grades (cdr db))))
            (fill-grades (cdr db)))
        nil)))

(defun trim-grade-lists (&optional (db *grade-db*))
  "If extra grades are added due to compilation not finishing, this will bring them back to *master-grade-list* length"
  (let* ((master-grade-length (length *master-grades*))
        (grades (getf (car db) :grade))
        (student-grade-length (length grades)))
    (if (not (null db))
        (if (<= master-grade-length student-grade-length)
            (progn
              (update (where :grade grades)
                      :grade (nthcdr (- student-grade-length master-grade-length) grades))
              (trim-grade-lists (cdr db)))
            (trim-grade-lists (cdr db)))
        nil)))

(defun resize-lists (&optional (db *grade-db*))
  "TODO: Should refactor to be more general - trims and expands"
  (let ((master-grade-length (length *master-grades*))
        (student-grades (getf (car db) :grade)))
    (if (not (null db))
        (cond ((= master-grade-length (length student-grades)) (resize-lists (cdr db)))
              ((< (+ 1 master-grade-length) (length student-grades)) (trim-grade-lists db))
              ((> master-grade-length (+ 1 (length student-grades)) (fill-grades db))))
        nil)))

(defun finalize-grade-list (&optional (db *grade-db*))
  "Helper to put the lists to the right size and update if necessary"
  (progn
    (resize-lists db)
    (update-all-grade-averages db)))

(defmacro recent-grade-change (name assignment-grade)
  "Changes the most recently entered grade - only used for updating last entered grade inputs"
  `(let ((grades (get-student ,name :grade)))
     (progn
       (setf (car grades) ,assignment-grade)
       (update (where :name ,name) :grade grades))))

(defmacro grade-change (how-many-ago name assignment-grade)
  "changes the nth grade of the student's to a different score"
  `(let ((grades (get-student ,name :grade)))
     (if (= ,how-many-ago 0) (recent-grade-change ,name ,assignment-grade)
     (progn
       (setf (nth ,how-many-ago grades) ,assignment-grade)
       (update (where :name ,name) :grade grades)))))

(defmacro current-date-p (date)
  `(if (string= ,date (current-date-string))
       t
       nil))

(defmacro add-date-for-class (date)
  `(if (current-date-p (car *class-dates*))
       nil
       (setq *class-dates* (cons ,date *class-dates*))))

(defun save-as-a-db (filename)
  "Saves DB to file NOTE: Clobbers"
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *grade-db* out))))

(defun save-as-a-csv (filename)
  "Creates CSV of grades, comments print more general data NOTE: Clobbers"
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (format out "Name,Eng Name,")
    (format out "~{~a~^,~}~%" *class-dates*)
    (dolist (student *grade-db*)
      (format out "~a," (getf student :grade))
      (format out "~a," (until-first-space (getf student :name)))
 ;     (format out "~a~%" (getf student :class)) ;NOTE: Inserts a break after (with ~%) may want to remove or toggle
      (format out "~{~a~^,~}~%" (getf student :attendance)))))

(defun save-grades (&optional (db-file *grade-db-file*))
  "NOTE: Clobbers, the general use saving function"
  (add-date-for-class (current-date-string))
  (save-as-a-db db-file)
  (let ((csv-file (make-pathname
                   :name (pathname-name (pathname db-file))
                   :type "csv")))
    (save-as-a-csv csv-file)))

(defun dump-student-db ()
  "Formatted text for reading the database"
  (dolist (student *grade-db*)
    (format t "*~{~a:~10t~a~%~}~%" student)))

(defmacro student-attends (name att-grade)
  "Adds att-grade to the attendence list"
  `(setf (get-student ,name :attendance)
        (cons ,att-grade (get-student ,name :attendance))))

(defmacro recent-attendance-change-to-attend (name)
  "set recent attendance to present (1) - sometimes students are late when taking roll"
  `(setf (get-student ,name :attendance)
             (cons 1 (cdr (get-student ,name :attendance)))))

(defmacro set-score (name score)
  "Sets a student's score section to a provided score"
  `(setf (get-student ,name :score) ,score))

(defun prompt-for-attendance (class)
  "This creates the prompt for take-attendence and saves the attendence as here:1 and absent:0"
  (dolist (student (select (where :class class)))
    (progn
      (format t "~a: " (getf student :name))
      (if (y-or-n-p)
          (student-attends (getf student :name) 1)
          (student-attends (getf student :name) 0)))))

(defmacro take-attendance (class)
  "runs through list of students prompting names and y/n for if they're present"
  `(progn
     (prompt-for-attendance ,class)
     (get-absences (where :class ,class))))
;     (if (not (eq (current-date-string) (first *classes-held*))) ;records date att taken
;         (cons (current-date-string *classes-held*))
;         nil)))

(defun get-absences (selector-fn &optional (with-silly-p nil))
  "returns the names of students absent from last recorded class"
  (let ((num-present 0)) ;counter for how many absent
    (progn
      (dolist (student (select selector-fn))
        (if (= (first (getf student :attendance)) 0) ;if most recent attendance is absent
            (progn
              (if with-silly-p
                  (progn
                    (format t "~a " (getf student :name)) ;get name
                    (format t "~a~%" (random-item-from-list *excuses*)))
                  nil))
              (setq num-present (+ 1 num-present)))
            nil))
      (format t "~a out of ~a students were present today" num-present (length (select selector-fn)))))

(defmacro pname (Ename Pname)
  "deprecated - appends a pinyin name to a 'student object'"
  `(nconc (car (select (where :name ,Ename))) '(:pinyin ,Pname)))

(defmacro get-grades (name)
  "Sets *grades* to a given student's grades NOTE: Deprecated, just a getter"
  `(get-student ,name :grade))

(defmacro get-attendance (name)
  "returns attendance list for the student NOTE: Deprecated, just a getter"
  `(get-student ,name :attendance))

(defmacro sum-grades (name)
  "No longer in use for averaging, just put in averaging"
  `(let ((x (get-student ,name :grade)))
    (loop for i in x sum i)))

(defmacro participation-grade-for-attendance (grade db)
  "push a grade into their grade list by attendance NOTE: Change CAR if grade has been entered"
  `(loop for student in ,db do
    (when (= 1 (car (getf student :attendance)))
      (make-additional-grade (getf student :name) ,grade))))

(defmacro average (grades)
  `(/ (loop for scores in ,grades sum scores)
      (length ,grades)))

(defmacro square (x)
  `(* ,x ,x))

(defmacro student-stdev (grades)
  `(/ (loop for scores in ,grades
            sum (square (- scores (average ,grades))))
      (length ,grades)))

(defmacro class-stdev (class)
  `(let ((avg (get-class-average ,class))
        (n (length (select-attribute :grade (select (where :class ,class))))))
     (/ (loop for student in (get-class ,class)
              sum (square (- (* (avg-grade student) (length (getf student :grade))) avg)))
        n)))

(defmacro z-score (student)
  `(/ (- (average (get-student ,student :grade))
         (get-class-average (get-student ,student :class)))
      (class-stdev (get-student ,student :class))))
