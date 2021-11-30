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
    :initarg 0
    :accessor score)))
