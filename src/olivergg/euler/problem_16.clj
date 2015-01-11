(ns olivergg.euler.problem_16
  (:require [olivergg.euler.common :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 16
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; we assume there is no bigint, to 2^1000 is too big to be computed directly.
(defn dble
  "For a number x stored as a list of its decimal digits, compute 2x (in the same format)"
  [coll]
  (loop [c (reverse coll), remain 0, out ()]
    (let [[aa & rr] c]
      (if (empty? c)
        (if (pos? remain) (conj out remain) out)
        (do
          (def aa2 (* 2 aa))
          (def outval (mod aa2 10))
          (def nextremain (quot aa2 10))
          (recur rr nextremain (conj out (+ (if (>= aa2 10) outval aa2) remain)))
          )
        )
      )
    )
  )
; sum of 2¹⁰⁰⁰ digits
(reduce + (first (drop 999 (take 1000 (iterate dble (list 2))))))
; 1366