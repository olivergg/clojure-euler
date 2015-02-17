(ns olivergg.euler.problem_35
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 35
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rotate
  [vecc]
  (conj (subvec vecc 1) (vecc 0))
  )

(defn allrotation
  [n]
  (def vecc (into [] (tobase10 n)))
  (def maxnbrot (count vecc))
  (loop [nbrot 0, v vecc, out #{}]
    (if (= nbrot maxnbrot)
      out
      (do
        (def rv (rotate v))
        (recur (inc nbrot) rv (conj out (frombase10 rv)))
        )
      )
    )
  )

(def primesbelow1E6 (into #{} (generate-prime-numbers 1000000)))
(count (filter (fn[x] (subset? (allrotation x) primesbelow1E6)) primesbelow1E6))

;55