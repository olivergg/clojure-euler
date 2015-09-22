(ns olivergg.euler.problem_55
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            [taoensso.timbre :as timbre]
            )
  )
(timbre/refer-timbre)
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

(def tentothepow (partial apply-pow 10))

(def tentothepow-memo (memoize tentothepow))

(profiling/defnp innerterm[a b n]
  (-> (- (mymodpow-memo a b (tentothepow-memo (inc n) )) (mymodpow-memo a b (tentothepow-memo n)))
                              (/ (tentothepow-memo n))

                              )
  )



(* 49E-6 1841770)

(defn sumterm[a b]
  (->> (range 0 (* b (Math/log a)))
       (map (partial innerterm a b))
       (reduce +)
       )
  )



(assert (= 25 (sumterm 5 8)))



(profile :info :Arithmetic
         (apply max
                (for [i (range 1 101)
                      j (range 1 101)
                      :when (and (not (= 0 (mod i 10)))
                                 (not (= 0 (mod j 10)))
                                 )
                      :let [s (sumterm i j)]
                      ]
                  s
                  )))






