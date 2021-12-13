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

; Concept scores are for concept quizzes which have a max score of 4 in first test and 5 in next test
; These are quizzes designed around mastery learning.  *concept-names* are the titles of the topics in
; the quiz and each one is tested twice or more times for students to obtain full mastery of the material.
; At certain points throughout the year, the mastery topics are consolidated into a single grade, and the
; material starts over, at that point, *concept-names* will need to be rewritten or reentered.
; TODO: get *concept-names* to be read in from a file on load

(defparameter *concept-names* (list "Graphing Proportional Relationships" "Constant of Proportionality" "Analyzing Proptional Relationship Graphs"
                                    "Constructing Relationship Graphs" "Straight Tax" "Simple Interest" "Added Costs" "Error Margins" "Sign Combination"
                                    "Integer Inverses" "Multiplication of Integers" "PEMDAS" "Multiple Representations of Rational Numbers" "Rational Operations"
                                    "Rational PEMDAS" "Simplification and Reduction" "Balancing Equations" "Adding Linear Equations"
                                    "Simplifying Rational Linear Equations"))
(defvar *staged-concept* nil)
(defvar *staged-grade* nil)

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

(defmacro get-concept-name (cnum)
  `(nth ,cnum *concept-names*))

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
  ;(format t "~a~a" whose concept-number))  Use one of these for formating as text depending on future dev

(defun create-concept (score-set &optional (possible-score 4))
  "clobbers *staged-concept* and creates an object which obeys the grading rules of concept question quizzes"
  (progn
    (setq *staged-concept* (make-instance 'concept :concept-name (get-concept-name (car score-set))
                                                     :concept-number (car score-set)
                                                     :possible-score possible-score))
    (setf (current-score *staged-concept*) (second score-set))))

(defmacro concept->grade (concept)
  `(defvar *staged-grade* (make-instance 'grade :max-score 1
                                        :score (/ (float (current-score ,concept)) (float (possible-score ,concept))))))

(defmethod concept-p (num concept)
  "returns t if a concept number exists"
  (if (eq num (cnum concept))
       t
       nil))
