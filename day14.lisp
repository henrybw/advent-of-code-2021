(in-package #:advent-of-code-2021)

(defparameter *day* 14)

;; Returns a list of (POLYMER-TEMPLATE PAIR-INSERTIONS), where POLYMER-TEMPLATE
;; is a string, and PAIR-INSERTIONS is an association list of the form
;; (PAIR . INSERTION), e.g. ("AB" . "C").
(defmethod parse ((day (eql *day*)) (input stream))
  (let* ((lines (uiop:slurp-stream-lines input))
         (polymer-template (first lines))
         (pair-insertions (subseq lines 2)))
    (list polymer-template
          (mapcar (lambda (rule) (apply #'cons (split-by rule " -> ")))
                  pair-insertions))))

(defun step-grow (polymer rules)
  (labels ((insert (a b rules)
             (let* ((pair (format nil "~a~a" a b))
                    (rule (assoc-if (lambda (key) (string= key pair)) rules)))
               ;; second char will be appended as A in the loop's next ON clause
               (list (char pair 0) (when rule (cdr rule))))))
    (format nil "~{~a~}" (loop for (a b) on (coerce polymer 'list)
                               if b
                                 append (insert a b rules)
                               else
                                 collect a))))

;; Apply 10 steps of pair insertion to the polymer template and find the most
;; and least common elements in the result. What do you get if you take the
;; quantity of the most common element and subtract the quantity of the least
;; common element?
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (destructuring-bind (polymer rules) input
    (dotimes (i 10)
      (setf polymer (step-grow polymer rules)))
    (loop for element across (remove-duplicates polymer)
          maximize (count element polymer) into most
          minimize (count element polymer) into least
          finally (return (- most least)))))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 1588)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))))
