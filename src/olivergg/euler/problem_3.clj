(ns olivergg.euler.problem_3
  (:require [olivergg.euler.common :refer :all]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(isprime 600851475143);; not prime

;; factorization using trial division would be too slow for such a great number.
(def n 60085147514)
(def a (rhofactor n))
(def b (rhofactor (/ n a)))
(def c (rhofactor (/ n a b)))
(def d (rhofactor (/ n a b c)))
;; (def e (rhofactor (/ n a b c d))) might be long
;; (println a b c d e)
(isprime 6857) ;; greatest prime factor
;; 6857
