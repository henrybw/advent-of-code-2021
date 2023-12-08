(asdf:defsystem #:advent-of-code-2021
  :description "Advent of Code 2021 solutions"
  :author "Henry Baba-Weiss <henry.babaweiss@gmail.com>"
  :license  "MIT"
  :serial t
  :depends-on (#:uiop #:alexandria #:minheap)
  :components ((:file "package")
               (:file "advent-of-code-2021")
               (:file "day1")
               (:file "day2")
               (:file "day3")
               (:file "day4")
               (:file "day5")
               (:file "day6")
               (:file "day7")
               (:file "day8")
               (:file "day9")
               (:file "day10")
               (:file "day11")
               (:file "day12")
               (:file "day13")
               (:file "day14")
               (:file "day15")))
