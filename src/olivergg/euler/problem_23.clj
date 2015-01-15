(ns olivergg.euler.problem_23
  (:require [olivergg.euler.common :refer :all]
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 23
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def isabundant-memo (memoize isabundant))


(defn issumoftwoabundant
  [initn]
  (loop [iter 0]
    (if (> iter (quot initn 2))
      false
      (if (and (isabundant-memo iter) (isabundant-memo (- initn iter)))
        true
        (recur (inc iter))
        )
      )
    )
  )


(time (def outtoreduce (loop [n 28123, out ()]
                         (let [issumoftwoab (issumoftwoabundant n)]
                           (cond
                             (= 0 n) out
                             (false? issumoftwoab) (recur (dec n) (conj out n))
                             :else (recur (dec n) out)
                             )
                           )
                         )
        )
      )
(println (reduce + outtoreduce))
;4179871