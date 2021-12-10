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

(defun incomplete-score (line)
  (let (token-stack)
    (dolist (token line)
      (if (find token "([{<")
          (push token token-stack)
          (let ((open-token (pop token-stack)))
            (when (or (and (char/= open-token #\() (char= token #\)))
                      (and (char/= open-token #\[) (char= token #\]))
                      (and (char/= open-token #\{) (char= token #\}))
                      (and (char/= open-token #\<) (char= token #\>)))
              (return-from incomplete-score 0)))))
    (let ((score 0))
      (dolist (token token-stack)
        (setf score (+ (* score 5) (cond ((char= token #\() 1)
                                         ((char= token #\[) 2)
                                         ((char= token #\{) 3)
                                         ((char= token #\<) 4)
                                         (t 0)))))
      score)))

;; Find the completion string for each incomplete line, score the completion
;; strings, and sort the scores. What is the middle score?
(defmethod solve ((day (eql *day*)) (part (eql 2)) input)
  (let ((scores (sort (loop for line in input
                            for score = (incomplete-score line)
                            unless (= score 0)
                              collect score)
                      #'<)))
    (nth (floor (length scores) 2) scores)))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 26397))
  (assert (= (solve *day* 2 example) 288957)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))
    (format t "day~a-part2: ~a~%" *day* (solve *day* 2 input))))
