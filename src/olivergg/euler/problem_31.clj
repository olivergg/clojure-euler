(ns olivergg.euler.problem_31
  (:require [olivergg.euler.common :refer :all]
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 31
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; according to number theory, if T is a set of positive integers then the number of partitions of n,
; all of whose parts belong to T, has generating function :
; Π ( 1  - x ^ t ) ^ (-1) ) for every t in T = Σ ( p(k,n) x^n ) for every natural integer n >= 0 where
; p(k,n) is the number of partitions of n into exactly k parts
; the solution is then the coefficient of the x^200 term in the following series :
; ((1/(1-x)) * (1/(1-x^2)) * (1/(1-x^5)) * (1/(1-x^10)) * (1/(1-x^20)) * (1/(1-x^50)) * (1/(1-x^100)) * (1/(1-x^200))
; let's remind that (1/(1-x)) = 1 + x + x^2 + x^3 + x^4 + ....
; let's define a polynomial P = a0 + a1 * X + a2 * X^2 + a3 * X^3 + ... by its coefficient (a0, a1, a2, a3, ....)


(defrecord Monomial [power value])

(defn mulpolExpand
  "Polynomial multiplication.
  The result is not a polynomial but a sequence of Monomial "
  [pol1, pol2]
  (for [i (range 0 (count pol1))
        j (range 0 (count pol2))
        :let [pol1i (pol1 i)
              pol2j (pol2 j)
              product (* pol1i pol2j)
              ]
        ]
    (->Monomial (+ i j) product)
    )
  )

(defn simplify
  "Simplify into a polynomial a sequence of monomials "
  [monomialSeq]
  (into [] (map
             ;group by power, sort by power the resulting map, and sum the values with the same power.
             (fn [[k monomialsOfPowerK]]
               (reduce + (map #(:value %) monomialsOfPowerK))
               )
             (into (sorted-map) (group-by #(:power %) monomialSeq))
             )
        )
  )

(defn mulpol
  "Polynomial multiplication. The result is also a polynomial"
 [pol1, pol2]
 (simplify (mulpolExpand pol1 pol2))
 )


(defn genvecforn
 "Generates a list of size n that starts with 1 followed by a sequence of 0.
  e.g. : n = 4 => '(1 0 0 0)
  "
  [n]
  (conj (repeat (- n 1) 0) 1)
  )




(defn expand-geomseries
  "Expand the series 1/1-x^n up to the given maximum degree maxpow
  e.g : 1/1-x^2 => n=2, maxpow=10 => [1 0 1 0 1 0 1 0 1 0 1]
  "
  [n,maxpow]
  (into [] (take (inc maxpow) (mapcat flatten (repeat (genvecforn n)))))
  )


(defn generatingfunc
  [authelems]
  (def maxelem (first authelems))
  (reduce mulpol (map (fn[x] (expand-geomseries x maxelem)) authelems))
  )

(defn getnbpart
  [authelems]
  ((generatingfunc authelems) (first authelems))
  )

(getnbpart (list 10 5 2 1))

(getnbpart (list 20 10 5 2 1))

(getnbpart (list 200 100 50 20 10 5 2 1))
;73682









