(ns olivergg.euler.problem_36
  (:require [olivergg.euler.common :refer :all]
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 36
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ispalind
  [coll]
  (let [[h1 h2 & t] coll
        [h & tail] coll
        ]
    (def l (last t))
    (cond
      (and (nil? h2) (nil? t)) true
      (nil? t) (= h1 h2)
      (= h1 l) (recur (drop-last tail))
      :else false
      )
    )
  )

(defn isbothpalind
  [n]
  (def nbase10 (tobase10 n))
  (def nbase2 (tobase2 n))
  (and (ispalind nbase10) (ispalind nbase2))
  )

(loop [iter 1, acc 0]
  (if (= 1000000 iter)
    acc
    (if (isbothpalind iter)
      (do
        (recur (inc iter) (+ acc iter)))
      (recur (inc iter) acc)
      )
    )
  )
; 872187