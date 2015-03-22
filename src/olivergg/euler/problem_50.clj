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



;(filter #(and (isprime (:sum %)) (< (:sum %) 1000)) (cumulsumprimes 2 1000))
;;{:sum 281, :length 14}
;(filter #(and  (< (:sum %) 1000) (> (:sum %) 281) (>= (:length %) 15) (isprime (:sum %)))   (cumulsumprimes 3 1000))
;; {:sum 499, :length 17}
;(filter #(and  (< (:sum %) 1000) (> (:sum %) 499) (>= (:length %) 18) (isprime (:sum %)))   (cumulsumprimes 5 1000))
;; {:sum 563, :length 17}
;(filter #(and  (< (:sum %) 1000) (> (:sum %) 563) (>= (:length %) 18) (isprime (:sum %)))   (cumulsumprimes 7 1000))
;;{:sum 953, :length 21}
;
;(empty? (intersection
;          (into #{} (generate-prime-numbers 1000 953))
;          (into #{} (cumulsumprimes 2 100))
;          )
;        )
;;STOP
(cumulsumprimes 2 1000)
(generate-prime-numbers 1000 953)

;
;
;(filter #(and (isprime (:sum %)) (< (:sum %) 1000000)) (cumulsumprimes 2 1000000))
;;{:sum 281, :length 14}
;(filter #(and  (< (:sum %) 1000000) (>= (:sum %) 958577) (>= (:length %) 537) (isprime (:sum %)))   (cumulsumprimes 3 1000000))
;; continue
;(filter #(and  (< (:sum %) 1000000) (>= (:sum %) 958577) (>= (:length %) 537) (isprime (:sum %)))   (cumulsumprimes 5 1000000))
;; {:sum 978037, :length 539}
;(filter #(and  (< (:sum %) 1000000) (>= (:sum %) 978037) (>= (:length %) 540) (isprime (:sum %)))   (cumulsumprimes 7 1000000))
;;{:sum 997651, :length 543}
;
;(empty? (intersection
;          (into #{} (generate-prime-numbers 1000000 997651))
;          (into #{} (cumulsumprimes 2 1000000))
;          )
;        )
;
;; 997651



(defn findlongestsum
  [n]
  (loop [currentmaxsum 0
         currentmaxlength 0
         primestoiter (generate-prime-numbers 23 1)
         ;stopcond false
         ]
        (if (or (empty? primestoiter)
                ;stopcond
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
            (println (count listtosearch))
            (def maxk (apply max-key :sum (if (empty? listtosearch) (list 0) listtosearch )))
            (def newmaxsum (:sum maxk))
            (def newmaxlength (:length maxk))
            ;(def newstopcond (empty? (intersection
            ;          (into #{} (generate-prime-numbers n currentmaxsum))
            ;          (into #{} cumulprimefromp)
            ;          )
            ;        ))
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
(count (filter #(and (isprime (:sum %)) (< (:sum %) 1000000))  (cumulsumprimes 2 1000000)))

(time (findlongestsum 1000000))
; {:sum 997651, :length 543}