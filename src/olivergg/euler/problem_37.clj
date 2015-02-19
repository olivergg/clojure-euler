(ns olivergg.euler.problem_37
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 37
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn removedigfromlefttoright
  [n]
  (def base10n (tobase10 n))
  (loop [coll base10n, out ()]
    (let [[head & tail] coll]
      (cond
        (nil? tail) out
        :else (recur tail (conj out (frombase10 tail)))
        )
      )
    )
  )

(defn removedigfromrighttoleft
  [n]
  (def base10n (tobase10 n))
  (loop [coll base10n, out ()]
    (cond
      (= 1 (count coll)) out
      :else
      (do
        (def dropped (drop-last coll))
        (recur dropped (conj out (frombase10 dropped)))
        )
      )
    )
  )

(defn removedig
  [n]
  (union
    #{n}
    (into #{} (removedigfromlefttoright n))
    (into #{} (removedigfromrighttoleft n))
    )
  )

(defn remainprime
  [n]
  (not (some (fn[x] (not (isprime x))) (removedig n)))
  )

(reduce + (loop [start 11
                 end 1000
                 c 0
                 out #{}
                 ]
            (def primes (generate-prime-numbers end start))
            (cond
              (= 11 c) out
              :else (do
                      (def filtered (into #{} (filter remainprime primes)))
                      (recur
                        (inc end)
                        (+ end 1000)
                        (+ c (count filtered))
                        (union out filtered)
                        )
                      )
              )
            )
        )

;748317
