(ns olivergg.euler.problem_26
  (:require [olivergg.euler.common :refer :all]
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 26
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn periodlength
  "Returns the period length of the decimal representation of inita/initb"
  [inita,initb]
  (loop [iter 0, a inita, b initb, restset #{}]
    (def Q (quot a b))
    (def r (mod a b))
    (if (contains? restset r)
      (do
        ;(println "last q " Q)
        ;(println "iter " iter)
        iter
        )
      (do
        ;(println "q" Q)
        (recur (inc iter) (* 10 r) b, (conj restset r))
        )
      )
    )
  )

(last (sort-by (fn[x] (x :len)) (for [d (range 1 1001)
      :let [plen (periodlength 1 d)]]
  {:d d :len plen }
  )))
; 983

