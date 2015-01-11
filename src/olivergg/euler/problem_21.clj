(ns olivergg.euler.problem_21
  (:require [olivergg.euler.common :refer :all]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 21
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(factor 10000)
(rhofactor 5000)

(def n 9999)

(defn divisor
  [n]
  (loop [iter 0
         maxdiv 1
         out #{1}
         ]
          (if (<= maxdiv (/ n 2))
            (do
              (def fac (rhofactor n))
              (recur (inc iter) (max fac maxdiv) (conj out fac))
              )
            (disj out n)
            )
          )
  )


(disj #{1 2 3} 3)

(sort (divisor 9999))

(loop [iter 220]
  (when (> iter 0)
    (println "iter " iter "sum of div " (reduce + (divisor iter)))
    (recur (dec iter))
    )
  )
