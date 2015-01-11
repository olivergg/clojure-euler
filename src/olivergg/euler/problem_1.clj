(ns olivergg.euler.problem_1
  (:require [olivergg.euler.common :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn summul
  "the sum of all the multiples of 3 or 5 below n"
  [n]
  (loop [iteration 1
         sum 0]
    (if (= iteration n)
      sum
      (recur (inc iteration)
             (+ sum (if (or  (zero? (mod iteration 3)) (zero? (mod iteration 5)))
                      iteration
                      0)
                )
             )
      )
    )
  )
(println (summul 1000))
;; 233168