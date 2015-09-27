(ns olivergg.euler.problem_57
  (:require [olivergg.euler.common :as common]


            )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 57
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn t[x]
  (+ 1 (/ 1 (+ 1 x)))
  )


(defn countdigits[n]
  (count (str n))
  )


(->> (iterate t 1)
     (take 1000)
     (drop 1)
     (map (fn[t] {:n (numerator t) :d (denominator t)}))
     (map (fn[x] {:cn (countdigits (:n x)) :cd (countdigits (:d x)) } ))
     (filter (fn[x] (> (:cn x) (:cd x))))
     (count)
     )

;153
