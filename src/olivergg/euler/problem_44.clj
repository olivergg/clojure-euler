(ns olivergg.euler.problem_44
  (:require [olivergg.euler.common :refer :all]
            [olivergg.euler.problem_31 :refer :all]
            [clojure.set :refer :all]
            )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 44
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pn[n]   (quot (* n (- (* 3 n) 1) ) 2))

(defn ispentagonal
  [x]
  (if (pos? x)
    (do
      (def valtocheck (/ (+ 1 (Math/sqrt (+ 1 (* 24 x)))) 6))
      (== 0.0 (- valtocheck (long valtocheck)))
      )
    false
    )
  )

(assert (ispentagonal 22))
(assert (not (ispentagonal 6)))

(def rangetoiterate (map pn (range 1 10000)))

; still need to prove that D is minimal for the first encountered solution....
(first (for [i rangetoiterate
             j rangetoiterate
             :when (and (< j i) (ispentagonal (- i j)) (ispentagonal (+ i j)))
             ]
           {:i i :j j :D (Math/abs (- i j))}
         )
       )
;({:i 7042750, :j 1560090, :D 5482660})