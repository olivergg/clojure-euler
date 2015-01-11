(ns olivergg.euler.problem_9
  (:require [olivergg.euler.common :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1 < a < b < c
; a²+b²=c²
; a+b+c = 1000
;
;
;l y a équivalence entre
;
;(i) \quad (x,y,z) est un triplet pythagoricien primitif avec x impair.
;(ii) Il existe  (p,q) \in\N^{*2}  avec p > q , p et q premiers entre eux et de parités différentes, tels que
;\quad x=p^2-q^2,\quad y=2pq\quad{\rm et}\quad z=p^2+q^2.

(time (first (for [q (range 1 1000)
                   p (range q 1000)
                   :let [pp (* p p)
                         qq (* q q)
                         a (- pp qq)
                         b (* 2 p q)
                         c (+ pp qq)
                         ]
                   :when (and (= 1000 (+ a b c)))
                   ]
               (do
                 (println "p "p "q " q "a " a "b " b "c " c "pgcd de p et q " (gcd q p))
                 {:a a :b b :c c :abc (* a b c)}
                 )
               )
             )
      )

;; {:a 200, :b 375, :c 425, :abc 31875000} multiple du triplet primitif (8,15,17) (facteur (gcd p q) = 5)

