(ns olivergg.euler.problem_27
  (:require [olivergg.euler.common :refer :all]
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 27
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn quadrform
  [a,b,n]
  (+ (* n n) (* a n) b)
  )

(defn nbofproducedprimes
  [a b]
  (loop [n 0]
    (def isprim (isprime (quadrform a b n)))
    (if (not isprim)
      n
      (recur (inc n))
      )
    )
  )

(def res (last (sort-by (fn [x] (x :nb))
                        (for [a (range -1000 1001)
                              b (range -1000 1001)
                              :let [nbofproducedp (nbofproducedprimes a b)]
                              ]
                          {:a a :b b :nb nbofproducedp}
                          )
                        )
               )
  )

(* (:a res) (:b res))

; -59231