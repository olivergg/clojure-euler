(ns olivergg.euler.problem_32
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 32
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def authdigits #{1 2 3 4 5 6 7 8 9})

(def tobase10-memo (memoize tobase10))

(defn ispandigital
  [mul1 mul2 prod]
  (def mul1list (tobase10-memo mul1))/
  (def mul1set (into #{} mul1list))
  (def mul2list (tobase10-memo mul2))
  (def mul2set (into #{} mul2list))
  (def prodlist (tobase10-memo prod))
  (def prodset (into #{} prodlist))
  (and
    (= (count mul1set) (count mul1list))
    (= (count mul2set) (count mul2list))
    (= (count prodset) (count prodlist))
    (= #{} (intersection mul1set mul2set))
    (= #{} (intersection mul1set prodset))
    (= #{} (intersection mul2set prodset))
    (= authdigits (union mul1set mul2set prodset))
    )
  )

; the product must be strictly less than 10000....otherwise it would have 5 digits, and
; its minimal value would be 12345 which is greater than the max value we can achieve with
; [][]x[][] or [][][]x[] ie 98*76 or 987*6
; we stil need to prove that the multiplier/multiplicand cannot be greater than 2345
; (* 1 2345) => not possible (same digits)
; (* 6 2345) (which is the smallest valid combination) more than 4 digits  => not possible
; (* 16 2345) (which is the smallest valid combination) more than 3 digits => not possible.
; etc..
(time (reduce + (into #{} (map (fn [x] (:prod x)) (for [i (range 1 2345)
                                                        j (range 1 2345)
                                                        :let [ij (* i j)]
                                                        :when (and (< ij 10000) (ispandigital i j ij))
                                                        ]
                                                    {:i i :j j :prod ij}
                                                    )
                               )
                      )
              )
      )

;45228
