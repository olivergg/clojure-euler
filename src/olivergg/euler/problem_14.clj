(ns olivergg.euler.problem_14
  (:require [olivergg.euler.common :refer :all]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 14
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn collatz
  [n]
  (loop [iter n, length 0]
    (cond
      (= 0 iter) length
      (= 1 iter) (inc length)
      (= 0 (mod iter 2)) (recur (/ iter 2) (inc length))
      :else (recur (+ 1 (* 3 iter)) (inc length))
      )
    )
  )

(time (loop [iter 0,
             maxlength 0,
             iterofmax 0]
        (if (= 1000000 iter)
          (list maxlength iterofmax)
          (let [collatzi (collatz iter)]
            (recur
              (inc iter)
              (max maxlength collatzi)
              (if (> collatzi maxlength) iter iterofmax))
            )
          )
        )
      )
; 837799 (length 525)
