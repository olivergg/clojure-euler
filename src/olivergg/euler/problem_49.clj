(ns olivergg.euler.problem_49
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 49
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def primes (generate-prime-numbers 10000 1000))

(defn ord[n] (sort (str n)))

(def ord-memo (memoize ord))

(time (second (for [p1 primes
                    p2 (filter #(> % p1) primes)
                    p3 (filter #(and (> % p2) (= (- % p2) (- p2 p1))) primes)
                    :when
                    (and
                      (=
                        (ord-memo p1)
                        (ord-memo p2)
                        (ord-memo p3)
                        )
                      )]
                {:p1 p1 :p2 p2 :p3 p3}
                )))
; {:p1 2969, :p2 6299, :p3 9629}
; 296962999629