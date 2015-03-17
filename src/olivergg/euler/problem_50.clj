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

(for [i (filter #(isprime %) (range 1 20))]
  (println i)
  )


(def primes (filter #(isprime %) (range 1 Double/POSITIVE_INFINITY)))

(defn cumulsum
  [initcoll]
  (loop [coll initcoll, cumulsum 0, out []]
    (cond
      (empty? coll) (conj out cumulsum)
      :else (recur (rest coll) (+ cumulsum (first coll)) (conj out cumulsum))
      )
    )
  )


(defn cumulsumbetween [start end]
  (cumulsum (filter #(isprime %) (range start end)))
  )


(filter #(< % 100000) (cumulsumbetween 2 1000000))

(.indexOf (list 1 2 3) 3)


(defn bach[n]
  (/ (* (* n n ) (Math/log n) )2)
  )


(bach 4)
(map #(int (bach %)) (range 1 1000))


(def memoisprime (memoize isprime))


(defn findbelow
  [n]
  (def initprimestoiter (into (sorted-set) (generate-prime-numbers n 2)))
  (loop [primestoiter initprimestoiter, maxsize 0, current ()]
    (def iter (first primestoiter))
    (println "iter " iter "maxsize " maxsize "  current " current)
    (cond
      (nil? iter) {:length maxsize :current (last current)}
      :else
      (do
        (def csumbetween (into [] (cumulsumbetween iter n)))
        (def curr (filter #(and (contains? initprimestoiter %) (< % n)) csumbetween))
        (def i (.indexOf csumbetween (last curr)))
        (recur (rest primestoiter)
               ;(max maxsize (count curr)) curr
               (if (> i maxsize)
                 i
                 maxsize
                 )
               (if (> i maxsize)
                 curr
                 current
                 )
               )
        )
      )
    )
  )

(assert (= 41 (:current (findbelow 100))))

(assert (= 953 (:current (findbelow 1000))))

(time (findbelow 100000))

(loop [i 2 amax 0]
  (prn "i "i)
  (cond
    (= i 100) amax
    :else (recur (inc i)
                 (max amax
                      (apply max
                             (conj (filter (fn [x] (and
                                                     (isprime x)
                                                     (< x 100)
                                                     )
                                             )
                                           (cumulsumbetween i 100)
                                           )
                                   0
                                   )
                             )
                      )
                 )
    )
  )

(apply max (list))

(max (list 1 2 3))

(cumulsumbetween 2 100)

(filter (fn [x] (and (isprime (:out x)) (< (:out x) 100))) (cumulsumbetween 2 100))

(filter (fn [x] (and (isprime x) (< x 100))) (cumulsumbetween 5 100))

