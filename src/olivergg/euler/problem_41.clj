(ns olivergg.euler.problem_41
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 41
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn genpermut-apply
  "Apply the function f to each permutation of the given set"
  ([someset f] (genpermut-apply someset () f))
  ([someset outx f]
   (cond
     (empty? someset) (f outx)
     :else (map (fn [x] (genpermut-apply (disj someset x) (conj outx x) f)) someset)
     )
    )
  )

(defn genperm
  [n]
  (flatten (genpermut-apply (into (sorted-set) (range 1 (inc n)))
                            (fn[x] (frombase10 x))
                            )
           )
  )

(time (do
        (count (filter (fn [x] (isprime x)) (genperm 9)))
        ;0
        (count (filter (fn [x] (isprime x)) (genperm 8)))
        ;0
        (count (filter (fn [x] (isprime x)) (genperm 7)))

        (last (sort (filter (fn [x] (isprime x)) (genperm 7))))
        )
      )
;7652413