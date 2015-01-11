(ns olivergg.euler.problem_2
  (:require [olivergg.euler.common :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fibbo
  "Append at the begining of the list, the sum of the two first elements of that list"
  [list]
  (def safeget (fnil identity 0))
  (let [[first second & rest] list
        safefirst (safeget first)
        safesecond (safeget second)
        ]
    (conj list (+ safefirst safesecond))
    )
  )


(fibbo '(2 1))
(fibbo (fibbo (fibbo '(2 1))))

(defn filtereven-less-fourmillion [x] (and (= 0 (mod x 2)) (< x 4000000)))
(reduce + (filter  filtereven-less-fourmillion (first (drop 56 (take 57 (iterate fibbo '(2 1)))))))


;; 4613732
