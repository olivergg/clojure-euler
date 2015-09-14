(ns olivergg.euler.problem_51
  (:require [olivergg.euler.common :refer :all]
    [clojure.set :refer :all]
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 51
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine
; possible values: 13, 23, 43, 53, 73, and 83, are all prime.
;
; By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number
; is the first example having seven primes among the ten generated numbers,
; yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993.
; Consequently 56003, being the first member of this family, is the smallest prime with this property.
;
; Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits)
; with the same digit, is part of an eight prime value family.

(defn has-repeating-digits[n,dig, rep]
  (->> (tobase10 n)
    (group-by identity)
    (filter #(= dig (% 0)))
    (map (fn[s] (count (s 1))))
    (some #(= % rep))
    )
  )




(defn replace-digits[n,dig,rep]
  (->> (tobase10 n)
    (replace {dig rep})
    (frombase10)
    )
  )


(defn gen-family[n, dig]
  (for [i (range 0 10)
        :let [replaced (replace-digits n dig i)]
        :when (and (> (quot replaced 100000) 0) (isprime replaced))
        ]
    replaced
    )
  )


(time
  (do

    (def pri (generate-prime-numbers 999999 100000))


    (first (filter #(not-empty %) (for [dig (range 0 10)
                                        rep (range 1 6)
                                        ]
                                    (filter #(= 8 (count %))
                                      (map #(gen-family % dig)
                                        (filter #(has-repeating-digits % dig rep) pri)
                                        )
                                      )
                                    )
             )
      )
    )
  )

;; 121313






