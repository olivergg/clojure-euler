(ns olivergg.euler.problem_48
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 48
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn modpow
  "From Applied Cryptography, Bruce Schneier, 2e, ISBN 0471117099"
  [initbase
   initexp
   m]
  (loop [exp initexp
         base initbase
         result 1]
    (if (pos? exp)
      (recur
        (bit-shift-right exp 1)
        (mod (*' base base) m)
        (if (pos? (bit-and exp 1))
          (mod (*' result base) m)
          result
          )
        )
      result
      )
    )
  )

(def m 10000000000) ; 10^10

(mod (reduce + (map (fn[x] (modpow x x m)) (range 1 1000))) m)
;9110846700




