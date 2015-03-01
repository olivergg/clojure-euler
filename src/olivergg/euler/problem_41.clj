(ns olivergg.euler.problem_41
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 41
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn genperm
  [n]
  (flatten (genpermut-apply (into (sorted-set) (range 1 (inc n)))
                            (fn[x] (frombase10 x))
                            )
           )
  )

(time (do
        ; hint : + is commutative
        (= 0 (mod (+ 9 8 7 6 5 4 3 2 1) 3)) ;true => all 9 digits pandigital are divisible by 3
        (= 0 (mod (+ 8 7 6 5 4 3 2 1) 3)) ;true => all 8 digits pandigital are divisible by 3
        (= 0 (mod (+ 7 6 5 4 3 2 1) 3))
        (count (filter (fn [x] (isprime x)) (genperm 7)))

        (apply max (filter (fn [x] (isprime x)) (genperm 7)))
        )
      )
;7652413