(in-package #:cl-user)

(defpackage #:simple-regexp
  (:use :cl)
  (:export seq alt star match))

(in-package #:simple-regexp)

;;; Regular Expressions
;;;
;;; We consider regular expressions constructed as follows
;;;
;;; <pattern> := <character>
;;;           | <predicate>
;;;           | <string>
;;;           | (seq <pattern> ...)
;;;           | (alt <pattern> ...)
;;;           | (star <pattern>)
;;;
;;; where <character> is a lisp character, <predicate> is a unary predicate
;;; defined on characters, <string> is a lisp string.
;;;
;;; The implementation below is based on the Thompson regexp -> nfa compiler,
;;; described here: https://swtch.com/~rsc/regexp/regexp1.html

(defun seq (pattern &rest patterns)
  "Match if all patterns match, in sequence."
  (reduce (lambda (a b)
            (list 'seq a b))
          (cons pattern patterns)))

(defun alt (pattern &rest patterns)
  "Match if any pattern matches."
  (reduce (lambda (a b)
            (list 'alt a b))
          (cons pattern patterns)))

(defun star (pattern)
  "Match if pattern matches zero or more times."
  (list '* pattern))

(defun valid-pattern-p (pattern)
  (or (characterp pattern)
      (stringp pattern)
      (functionp pattern)
      (and (listp pattern)
           (member (car pattern) '(seq alt repeat)))))

;;; FSM representation

(deftype state-type ()
  "The type of a state in our matcher."
  '(or
    character
    function
    (member :split :match)))

(defstruct state
  "A state in our matcher."
  type
  out
  out1
  last-idx)

(defun state (type &optional out out1)  
  (check-type type state-type)
  (make-state :type type :out out :out1 out1))

(defmethod print-object ((obj state) stream)
  (print-unreadable-object (obj stream :type t :identity t)
      (format stream "~A ~A"
              (state-type obj)
              (state-last-idx obj))))

(defstruct fragment
  "A fragment of a compiled regular expression, consisting of a starting state
and a set of (free) ending states."
  start
  ends)

(defun fragment (start ends)
  (make-fragment :start start :ends ends))

(defun patch (states target)
  "Update unassigned out-links in STATES to point to TARGET."
  (dolist (s states)
    (case (state-type s)
      (:match nil)
      (:split
       (setf (state-out s) (or (state-out s) target)
             (state-out1 s) (or (state-out1 s) target)))
      (otherwise
       (setf (state-out s) (or (state-out s) target))))))

(defun check-arity (regexp n)
  (assert (= n (length (rest regexp)))))

(defun compile-pattern (pattern)
  "Compile PATTERN, returning a starting STATE."
  (labels
      ((compile-to-fragment (pattern)
         (cond ((or (characterp pattern)
                    (functionp pattern))
                (let ((state (state pattern)))
                  (fragment state (list state))))
               ((stringp pattern)
                (compile-to-fragment
                 (apply #'seq (map 'list #'identity pattern))))
               (t
                (ecase (car pattern)
                  (seq
                   (check-arity pattern 2)
                   (destructuring-bind (f1 f2) (mapcar #'compile-to-fragment (rest pattern))
                     (patch (fragment-ends f1) (fragment-start f2))
                     (fragment (fragment-start f1) (fragment-ends f2))))
                  (alt
                   (check-arity pattern 2)
                   (destructuring-bind (f1 f2) (mapcar #'compile-to-fragment (rest pattern))
                     (fragment (state ':split (fragment-start f1) (fragment-start f2))
                               (append (fragment-ends f1) (fragment-ends f2)))))
                  (*
                   (check-arity pattern 1)
                   (let* ((f (compile-to-fragment (second pattern)))
                          (s (state ':split (fragment-start f))))
                     (patch (fragment-ends f) s)
                     (fragment s (list s)))))))))
    (let ((frag (compile-to-fragment pattern)))
      (patch (fragment-ends frag) (state ':match))
      (fragment-start frag))))

(defun match (pattern string)
  "Try to match STRING to PATTERN."
  (let (states
        next
        idx)
    (labels
        ((step-char (c)
           (setf states next
                 next nil)
           (loop :for state :in states
                 :for type := (state-type state)
                 :when (or (eq c type) ; characters
                           (and (functionp type) (funcall type c)))
                   :do (add-state (state-out state)))
           (incf idx))
         (add-state (s)
           (unless (or (null s)
                       (and idx (eql idx (state-last-idx s))))
             (setf (state-last-idx s) idx)
             (cond ((eq ':split (state-type s))
                    (add-state (state-out s))
                    (add-state (state-out1 s)))
                   (t
                    (push s next)))))
         (matchp ()
           (not
            (null
             (find-if (lambda (s) (eq ':match (state-type s)))
                      next)))))
      (add-state (compile-pattern pattern))
      (setf idx 0)
      (loop :for c :across string
            :do (step-char c)
            :finally (return (matchp))))))
