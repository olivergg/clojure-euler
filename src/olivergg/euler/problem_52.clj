(ns olivergg.euler.problem_52
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 52
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.
;
;Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

(defn contains-same-digit[n1,n2]
  (def n1list (tobase10 n1) )
  (def n2list (tobase10 n2) )
  (def n1set (into #{} n1list))
  (def n2set (into #{} n2list))
  (and 
    (= (count n1list) (count n2list))
    (= n1set n2set)
    )
  )

(assert (contains-same-digit 125874 251748))

(defn testf[n]
  (loop [iter 2
         out true]
    (if (or (= 6 iter) (not out))
      out
      (recur (inc iter) (and out (contains-same-digit n (* iter n))))
      )
    )
  )

(first (for [i (range 100000 1000000)
             :when (testf i)
             ]
         i
         )
       )


;; 142857

