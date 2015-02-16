(ns olivergg.euler.problem_33
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 33
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn badsimplify
  [num, denom]
  (def numlist (tobase10 num))
  (def denomlist (tobase10 denom))
  (let [[a b] numlist
        [c d] denomlist]
    (cond
      ; remove trivial cases
      (and (zero? (mod num 10)) (zero? (mod denom 10))) -1
      (and (pos? d) (= a c)) (/ b d)
      (and (pos? c) (= a d)) (/ b c)
      (and (pos? d) (= b c)) (/ a d)
      (and (pos? c) (= b d)) (/ a c)
      :else -1
      )
    )
  )


(assert (= -1 (badsimplify 30 70)))
(assert (= (/ 4 8) (badsimplify 49 98)))


(defn isdigitcancelling
  [num denom]
  (= (/ num denom) (badsimplify num denom))
  )


(denominator (reduce * (for [i (range 10 100)
                             j (range 10 i)
                             :when (isdigitcancelling j i)
                             ]
                         (/ j i)
                         )
                     )
             )
; 100