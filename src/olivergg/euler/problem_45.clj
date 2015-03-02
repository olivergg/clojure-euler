(ns olivergg.euler.problem_45
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 45
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pn[n]   (quot (* n (- (* 3 n) 1) ) 2))
(defn tn[n]   (quot (* n (+ n 1)) 2))
(defn hn[n]   (* n (- (* 2 n) 1)))

(assert (= (tn 285) (pn 165) (hn 143)))

(let [pntoiter (into #{} (map pn (range 1 105000)))
      tntoiter (into #{} (map tn (range 1 105000)))
      hntoiter (into #{} (map hn (range 1 105000)))
      ]
  (intersection pntoiter tntoiter hntoiter)
  )

;1533776805


