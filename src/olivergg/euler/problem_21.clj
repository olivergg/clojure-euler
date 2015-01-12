(ns olivergg.euler.problem_21
  (:require [olivergg.euler.common :refer :all]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 21
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn properdivisors
  [n]
  (loop [i 1, out #{1}]
    (def remain (mod n i))
    (def quoti (quot n i))
    (if (= i n)
      (disj out n)
      (if (zero? remain)
        (recur (inc i) (conj out remain quoti))
        (recur (inc i) out)
        )
      )
    )
  )

(defn computesumdiv-untiln
  [n]
  (loop [iter n, itertosum {}]
    (if (> iter 1)
      (do
        (def sumofdiv (reduce + (properdivisors iter)))
        (recur (dec iter) (conj itertosum {iter sumofdiv}))
        )
      itertosum
      )
    )
  )


(defn findamicable
  "find all amicable numbers below n"
  [n]
  (def itertosum (computesumdiv-untiln n))
  (loop [i n, out ()]
    (if (= 0 i)
      out
      (if (and
            (= i (itertosum (itertosum i))) ; amicable number
            (not= i (itertosum i)) ; but not a perfect number
            )
        (recur (dec i) (conj out i))
        (recur (dec i) out)
        )
      )
    )
  )

(def amicablenumbers (findamicable 10000))
;(220 284 1184 1210 2620 2924 5020 5564 6232 6368)
(reduce + amicablenumbers)
;31626

