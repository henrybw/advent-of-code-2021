(in-package #:advent-of-code-2021)

(require :asdf)

;; The submarine has a bingo subsystem that automatically generates a random
;; order in which to draw numbers and a random set of boards.
(defmethod parse ((day (eql 4)) (input stream))
  (let ((numbers (mapcar #'parse-integer
                         (uiop:split-string (read-line input nil)
                                            :separator ",")))
        ;; each board is a blank line, followed by 5 lines with 5 numbers each
        (boards (loop for blank = (read-line input nil) while blank
                      collect (loop for i below 5
                                    for line = (read-line input nil) while line
                                    collect (with-input-from-string (row line)
                                              (loop for n = (read row nil nil)
                                                    while n collect n))))))
    (list numbers boards)))

(defun mark-board (target board state)
  (loop for board-row in board
        for row below (length board)
        do (loop for num in board-row
                 for col below (length board-row)
                 when (= num target)
                   do (setf (aref state row col) t))))

(defun winning-board-p (state)
  (destructuring-bind (rows cols) (array-dimensions state)
    (loop for row below rows
          do (loop for col below cols
                   always (aref state row col)
                   finally (return-from winning-board-p t)))
    (loop for col below cols
          do (loop for row below rows
                   always (aref state row col)
                   finally (return-from winning-board-p t)))))

(defun find-first-winner (numbers boards states)
  (dolist (target numbers)
    (let ((winner (loop for board in boards
                        for state in states
                        do (mark-board target board state)
                        when (winning-board-p state)
                          do (return (list target board state)))))
      (when winner
        (return winner)))))

(defun find-last-winner (numbers boards states)
  (let (last-winners won-boards)
    (dolist (target numbers)
      (loop for board in boards
            for state in states
            do (mark-board target board state))
      ;; multiple boards can win simultaneously
      (let ((winners (loop for board in boards
                           for state in states
                           unless (member board won-boards)
                             when (winning-board-p state)
                               collect (list target board state))))
        (dolist (winner winners)
          (destructuring-bind (target board state) winner
            (let ((state-copy (make-array '(5 5) :element-type 'boolean
                                                 :initial-element nil)))
              ;; copy the board state as it was when the win occurred
              (dotimes (i (array-total-size state))
                (setf (row-major-aref state-copy i) (row-major-aref state i)))
              (pushnew (list target board state-copy) last-winners)
              (pushnew board won-boards))))))
    (first last-winners)))

(defun sum-unmarked (board state)
  (loop for board-row in board
        for row below (length board)
        sum (loop for num in board-row
                  for col below (length board-row)
                  when (not (aref state row col))
                    sum num)
          into total
        finally (return total)))

;; The score of the winning board is calculated by finding the sum of all
;; unmarked numbers, then multiplying that sum by the number that was just
;; called when the board won.
(defun winning-board-score (which-winner input)
  (destructuring-bind (numbers boards) input
    (let ((states (loop for i below (length boards)
                        collect (make-array '(5 5) :element-type 'boolean
                                                   :initial-element nil))))
      (destructuring-bind (last-number board state)
          (funcall which-winner numbers boards states)
        (* last-number (sum-unmarked board state))))))

;; To guarantee victory against the giant squid, figure out which board will win
;; first. What will your final score be if you choose that board?
(defmethod solve ((day (eql 4)) (part (eql 1)) input)
  (winning-board-score #'find-first-winner input))

;; On the other hand, it might be wise to try a different strategy: let the
;; giant squid win. Figure out which board will win last. Once it wins, what
;; would its final score be?
(defmethod solve ((day (eql 4)) (part (eql 2)) input)
  (winning-board-score #'find-last-winner input))

(let ((example (parse 4 "example")))
  (assert (= (solve 4 1 example) 4512))
  (assert (= (solve 4 2 example) 1924)))

(let ((input (parse 4 "input")))
  (when input
    (format t "day4-part1: ~a~%" (solve 4 1 input))
    (format t "day4-part2: ~a~%" (solve 4 2 input))))
