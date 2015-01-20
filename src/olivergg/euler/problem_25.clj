(ns olivergg.euler.problem_25
  (:require [olivergg.euler.common :refer :all]
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 25
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for large n, the nth fibo number is the closest integer to gold^n/sqrt(5)
;; the number of digits of a number x can be obtained from log10(x)
;;therefore, the number of digits of the nth fibo number is :

;; log10(  gold^n / sqrt(5) ) = n log10 (gold) - log10(sqrt(5))
;; we are looking for n such that n log10 (gold) - log10(sqrt(5)) +1 = 1000

(def gold (* 0.5 (+ 1 (Math/sqrt 5))))

(loop [n 1]
  (def cpt (+ (- (* n (Math/log10 gold)) (Math/log10 (Math/sqrt 5)) ) 1))
  (if (>= cpt 1000)
    n
    (recur (inc n))
    )
  )

;4782