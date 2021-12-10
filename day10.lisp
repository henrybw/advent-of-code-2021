(in-package #:advent-of-code-2021)

(defparameter *day* 10)

(defmethod parse ((day (eql *day*)) (input stream))
  (loop for line in (uiop:slurp-stream-lines input)
        collect (loop for ch across line collect ch)))

(defun error-score (line)
  (let (token-stack)
    (dolist (token line)
      (if (find token "([{<")
          (push token token-stack)
          (let ((open-token (pop token-stack)))
            (cond ((and (char/= open-token #\() (char= token #\)))
                   (return-from error-score 3))
                  ((and (char/= open-token #\[) (char= token #\]))
                   (return-from error-score 57))
                  ((and (char/= open-token #\{) (char= token #\}))
                   (return-from error-score 1197))
                  ((and (char/= open-token #\<) (char= token #\>))
                   (return-from error-score 25137)))))))
  0)

;; Find the first illegal character in each corrupted line of the navigation
;; subsystem. What is the total syntax error score for those errors?
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (loop for line in input sum (error-score line)))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 26397)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))))
