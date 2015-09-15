(ns olivergg.euler.problem_53
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 53
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;There are exactly ten ways of selecting three from five, 12345:
;
;123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
;
;In combinatorics, we use the notation, 5C3 = 10.
;
;In general,
;
;nCr =
;n!
;r!(n−r)!
;,where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
;It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.
;
;How many, not necessarily distinct, values of  nCr, for 1 ≤ n ≤ 100, are greater than one-million?


(defn cnk
  "binomial coefficients (For any set containing n elements, the number of distinct k-element subsets of it that can be formed) "
  [n,k]
  (if (>= n k)
    (quot (factorial n) (*' (factorial (-' n k)) (factorial k)))
    1)
  )

(assert (= 10 (cnk 5 (- 5 3))))
(assert (= 10 (cnk 5 3)))


(defn cnk-plus [n k] {:n n :k k :res (cnk n k)})

(defn get-first-above-million[n]
  (->> (range 0 (inc n))
      (map #(cnk-plus n %))
      (drop-while #(< (:res %) 1E6))
      (first)
      )
  )



(assert (nil? (get-first-above-million 22)))
(let [g (get-first-above-million 23)
      k (:k g)
      ]
  (assert (= 10 k))
  )


(defn get-width[n k]
  (->  (- n k)
       (- k)
       (+ 1)
       )
  )


(defn get-number-of-cnk-abovemillion[n]
  (let [getfirst (get-first-above-million n)
        k (:k getfirst)
        ]
    (get-width n k)
    )
  )


(->> (range 23 101)
     (map #(get-number-of-cnk-abovemillion %))
     (reduce +)
)

;; 4075
