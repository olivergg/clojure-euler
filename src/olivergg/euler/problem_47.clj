(ns olivergg.euler.problem_47
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 47
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hasKDistinctFactor
  [initn, K]
  (if (isprime initn)
    false
    (loop [n initn
           k 2
           acc #{}
           iter 0]
      (if (or (= 1 n) (> (count acc) K))
        (= K (count acc))
        (if (= 0 (mod n k))
          (recur (quot n k) k (conj acc k) (inc iter))
          (recur n (inc k) acc iter)
          )
        )
      )
    )
  )

(time (first (for [i (range 1 200000)
             :when (and
                     (hasKDistinctFactor i 4)
                     (hasKDistinctFactor (+ i 1) 4)
                     (hasKDistinctFactor (+ i 2) 4)
                     (hasKDistinctFactor (+ i 3) 4)
                      )
             ]
         i
         )
       ))
;134043