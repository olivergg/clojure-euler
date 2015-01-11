(ns olivergg.euler.problem_12
  (:require [olivergg.euler.common :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn countdiv
  "returns the number of divisors of n.
   if prime factorization of n is
   n = A^a x B^b x C^c x ...
   then, the number of divisors is (a+1)(b+1)(c+1)...
"
  [n]
  (if (< n 2)
    1
    (reduce * (map (fn [x] (inc (val x))) (frequencies (factor n))))
    )
  )

(defn tn
  "nth triangular number"
  [n]
  (/ (* n (- n 1)) 2)
  )

(time (first (for [n (range 10000 15000)
                   :let [tofn (tn n)
                         c (countdiv tofn)
                         ]
                   :when (> c 500)
                   ]
               (do
                 (println tofn c n)
                 tofn
                 )
               )
             )
      )
; 76576500 (the 12376th triangular number)

