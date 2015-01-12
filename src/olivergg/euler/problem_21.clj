(ns olivergg.euler.problem_21
  (:require [olivergg.euler.common :refer :all]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 21
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn divisor
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


(time (loop [iter 1000, itertosum {}, sumtoiter {}]
        (if (> iter 1)
          (do
            (def summm (reduce + (divisor iter)))
            ;(println "iter " iter "sum of div " summm)
            (recur (dec iter) (conj itertosum {iter summm}) (conj sumtoiter {summm iter}))
            )
          (do
            (println "itertosum " (sort itertosum))
            (println "sumtoiter " (sort sumtoiter))
            (loop [i 220]
              (if (= 0 i)
                (println "done")
                (do
                  (when (= i (itertosum (itertosum i)))
                    (println i)
                    )
                  (recur (dec i))
                  )

                )
              )

            )
          )
        )
      )





