(defclass grade ()
  ((score
    :initarg :score
    :accessor score)
   (max-score
    :initarg :max-score
    :accessor max-score)
   (assignment-name
    :initform :assignment-name
    :accessor assignment-name)
   (grade-category
    :initform nil
    :accessor grade-category)))


(defmethod pass-p (grade &optional (percentage .7)) ;;percentage to tell if they pass
  (if (> (/ (score grade) (float (max-score grade))) percentage)
      t
      nil))

(defclass concept ()
  ((concept-number
    :initarg :concept-number
    :accessor cnum)
   (current-score
    :initarg :current-score
    :initform 0
    :accessor score)
   (possible-score
    :initarg :possible-score
    :initform 4
    :accessor possible-score)))

(defmethod (setf score) (new-score concept)
  "This doesn't work yet"
  (if (> new-score (possible-score concept))
      (format t "Input not valid")
      (setf (slot-value concept 'current-score) new-score)))
