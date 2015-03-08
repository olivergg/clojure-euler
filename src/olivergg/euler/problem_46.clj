(ns olivergg.euler.problem_46
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 46
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn isdecomp
  [oddnum]
  (def primesbelow (generate-prime-numbers oddnum 1))
  (def twicesquares (map #(* 2 (* % %)) (range 1 (inc (int (Math/sqrt (/ oddnum 2)))))))
  (first (for [twsq twicesquares
               pri primesbelow
               :when (= oddnum (+ twsq pri))
               ]
           true
           )
         )
  )

(first (for [i (filter #(not (isprime %))  (range 7 Double/POSITIVE_INFINITY 2))
             :when (nil? (isdecomp i))]
         i
         )
       )

;5777