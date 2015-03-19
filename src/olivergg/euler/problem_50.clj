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
  (loop [coll initcoll, cumulsum 0, out []]
    (cond
      (empty? coll) (subvec (conj out cumulsum) 1)
      :else (recur (rest coll) (+ cumulsum (first coll)) (conj out cumulsum))
      )
    )
  )


(defn cumulsumbetween [start end]
  (cumulsum (filter #(isprime %) (range start end)))
  )


(cumulsumbetween 2 1000)
(drop 0 (map #(- % 0) (cumulsumbetween 2 1000)))
(cumulsumbetween 3 1000)
(drop 1 (map #(- % 2) (cumulsumbetween 2 1000)))
(cumulsumbetween 5 1000)
(drop 2 (map #(- % 5) (cumulsumbetween 2 1000)))
(cumulsumbetween 7 1000)
(drop 3 (map #(- % 10) (cumulsumbetween 2 1000)))



(defn findlastcurr
  [initcoll, someprimes, n]
  (loop [coll initcoll
         iter 0
         lastvalidelem -1]
    (cond
      (or (empty? coll) (>= iter n)) {:item lastvalidelem :idx (dec iter)}
      :else (do
              (recur (rest coll)
                     (if (and (isprime (first coll)) (< (first coll) n)) (inc iter) iter)
                     (if (and (isprime (first coll)) (< (first coll) n))  (first coll) lastvalidelem ))

              )

      )
    )
  )

(filter #(and (contains? initprimestoiter %) (< % 1000)) (cumulsumbetween 2 1000))
(findlastcurr  (cumulsumbetween 2 1000) (generate-prime-numbers 1000 2) 1000)


(defn findbelow
  [n]
  (def initprimestoiter (time (into (sorted-set 0) (generate-prime-numbers n 2))))
  (def initcsumbetween (time (into [0] (cumulsumbetween 2 n))))
  (loop [c 0 primestoiter initprimestoiter, maxsize 0, current 0]
    (def iter (first primestoiter))
    (cond
      (nil? iter) {:length maxsize :current (dbg current) :count (dbg c)}
      :else (do
              (def csumbetween  (drop c (map #(- % (get initcsumbetween c)) (subvec initcsumbetween 1))))
              ;(def curr (filter #(and (contains? initprimestoiter %) (< % n)) csumbetween))
              ;(def i  (time ((fnil identity 0) (first (indexes-of (last curr) csumbetween )))))
              ;(def i (count curr))
              ;(println csumbetween)
              ;(println (last curr))
              ;(def lll (time (last curr)))
              (def ff (findlastcurr csumbetween initprimestoiter n))
              (def i (:idx ff))
              ;(prn "c " c)
              ;(prn "---")
              (recur
                (inc c)
                (rest primestoiter)
                ;(max maxsize (count curr)) curr
                (if (> i maxsize)
                  i
                  maxsize
                  )
                (if (> i maxsize)
                  (:item ff)
                  current
                  )
                )
              )
      )
    )
  )


(assert (= 41 (:current (findbelow 100))))

(assert (= 953 (:current (findbelow 1000))))

;(findbelow 1000000)

