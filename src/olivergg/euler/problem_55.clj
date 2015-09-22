(ns olivergg.euler.problem_55
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            [taoensso.timbre :refer :all]

            )
  )


(defn addreverse[n]
  (let [nlist (tobase10 n)
        reversenlist (reverse nlist)
        reversedn (frombase10 reversenlist)
        ]
    (+' n reversedn)
    )
  )


(defn is-lychrel[n]
  (loop [iter 1
         currentn n
         ]
    (def currentaddrev (addreverse currentn))
    (cond
     (= 50 iter) true
     (ispalindromic currentaddrev) false
     :else (recur (inc iter) currentaddrev)
     )
    )
  )



(is-lychrel 196)


(count (for [i (range 1 10001)
      :when (is-lychrel i)]
  i
  ))

; 249
