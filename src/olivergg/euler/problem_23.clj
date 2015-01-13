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
  (loop [iter 0, n initn, out ()]
    (if (or (>= iter initn) (not (empty? out)))
      (do
        (println "iter " iter "n " n "n+iter" (+ (dec iter) (inc n)) "out " out)
        (not (empty? out))
        )
      (do
        (if (and (isabundant-memo iter) (isabundant-memo n))
          (recur (inc iter) (dec n) (conj out iter))
          (recur (inc iter) (dec n) out)
          )
        )
      )
    )
  )


(time (def outtoreduce (loop [n 28123, out ()]
                         (let [issumoftwoab (issumoftwoabundant n)]
                           (cond
                             (= 0 n) out
                             (false? issumoftwoab) (do
                                               (println "n " n)
                                               (recur (dec n) (conj out n))
                                               )
                             :else (do
                                     (println "n " n)
                                     (recur (dec n) out)
                                     )
                             )
                           )
                         )
        )
      )

(println (reduce + outtoreduce))

"Elapsed time: 154798.393372 msecs"j
;4179871