(ns olivergg.euler.problem_4
  (:require [olivergg.euler.common :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn str-reverse [s] (apply str (reverse s)))

(defn ispalindromic
  [n]
  (= (str n) (str-reverse (str n)))
  )

;; must be between 100 x 100 and  999 x 999
;; more likely to be closest to 1000000 so we start at the end (using negative index in the range).
(first (sort-by (fn [x] (- (:n x))) (for [i (range -1000 -1)
                                          j (range -1000 -1)
                                          :let [n (* (- i) (- j))]
                                          :when (and (ispalindromic n))]
                                      {:n n :i (- i) :j (- j)}))
       )

; {:n 906609, :i 993, :j 913}
