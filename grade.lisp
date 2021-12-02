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
    :accessor current-score)
   (possible-score
    :initarg :possible-score
    :initform 4
    :accessor possible-score)))

(defmethod (setf current-score) (new-score (obj concept))
  "Ensures setting scores higher than current possible maximum scores doesn't happen"
  (if (> new-score (slot-value obj 'possible-score))
      (progn
        (format t "Reverting to maximum ~a" (slot-value obj 'possible-score))
        (setf (slot-value obj 'current-score) (slot-value obj 'possible-score)))
      (setf (slot-value obj 'current-score) new-score)))
