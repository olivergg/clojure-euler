(ns olivergg.euler.problem_50
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 50
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;The prime 41, can be written as the sum of six consecutive primes:
;
;41 = 2 + 3 + 5 + 7 + 11 + 13
;This is the longest sum of consecutive primes that adds to a prime below one-hundred.
;
;The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
;
;Which prime, below one-million, can be written as the sum of the most consecutive primes?


(defn cumulsum
  [initcoll]
  (loop [coll initcoll, cumulsum 0, length 0, out []]
    (cond
      (empty? coll) (subvec (conj out {:sum cumulsum :length length}) 1)
      :else (recur (rest coll) (+ cumulsum (first coll)) (inc length) (conj out {:sum cumulsum :length length}))
      )
    )
  )



(defn cumulsumprimes [start end]
  (cumulsum (filter #(isprime %) (range start end)))
  )


(defn findlongestsum
  [n]
  (loop [currentmaxsum 0
         currentmaxlength 0
         primestoiter (generate-prime-numbers 7 1)
         ]
        (if (or (empty? primestoiter) ;TODO : find another stop condition
                )
          {:sum currentmaxsum :length currentmaxlength}
          (do
            (def p (first primestoiter))
            (def cumulprimefromp (cumulsumprimes p n))
            (def listtosearch (filter #(and
                                        (< (:sum %) n)
                                        (>= (:sum %) currentmaxsum)
                                        (>= (:length %) currentmaxlength)
                                        (isprime (:sum %)))
                                      cumulprimefromp)
              )
            (def maxk (apply max-key :sum (if (empty? listtosearch) (list 0) listtosearch )))
            (def newmaxsum (:sum maxk))
            (def newmaxlength (:length maxk))
            (if (and (not (nil? newmaxsum))
                     (not (nil? newmaxlength))
                     (> newmaxlength currentmaxlength)
                     (isprime newmaxsum)
                     )
              (recur newmaxsum newmaxlength (rest primestoiter))
              (recur currentmaxsum currentmaxlength (rest primestoiter))
              )
            )
          )
        )
  )
(time (findlongestsum 1000000))
; {:sum 997651, :length 543}