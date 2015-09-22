(ns olivergg.euler.problem_55
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 55
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



(def mymodpow-memo (memoize modpow))


(defn innerterm[a b n]
  (-> (- (mymodpow-memo a b (apply-pow 10 (inc n) )) (mymodpow-memo a b (apply-pow 10 n)))
      (/ (apply-pow 10 n))

  )
  )


(defn sumterm[a b]
  (->> (range 0 (* b (Math/log a)))
      (map (partial innerterm a b))
      (reduce +)
      )
  )



(assert (= 25 (sumterm 5 8)))



(time (apply max (for [i (range 1 101)
      j (range 1 101)
      :let [s (sumterm i j)]
      ]
  s
  )))




