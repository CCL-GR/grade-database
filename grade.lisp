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

(defparameter *concept-names* (list "Graphing Proportional Relationships" "Constant of Proportionality" "Analyzing Proptional Relationship Graphs"
                                    "Constructing Relationship Graphs" "Straight Tax" "Simple Interest" "Added Costs" "Error Margins" "Sign Combination"
                                    "Integer Inverses" "Multiplication of Integers" "PEMDAS" "Multiple Representations of Rational Numbers" "Rational Operations"
                                    "Rational PEMDAS"))
(defclass concept ()
  ((concept-name
    :initarg :concept-name
    :accessor name)
   (concept-number
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

(defun generate-name (whose concept-number)
  "Creates a string representation of name and concept number to use for creating concept score names"
  (format nil "~a~a" whose concept-number))

;(defmacro create-concept (whose concept-number current-score possible-score)
; "This doesn't work because dynamically generating *x* needs to not use defvar, it's needs its own defvar"
; `(defvar `#(generate-name ,,whose ,,concept-number)
;    (make-instance
;     'concept :concept-name "a" :concept-number ,concept-number :current-score ,current-score :possible-score ,possible-score)))
