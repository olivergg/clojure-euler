(ns olivergg.euler.problem_17
  (:require [olivergg.euler.common :refer :all]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 17
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def dico {1 "one"
           2 "two"
           3 "three"
           4 "four"
           5 "five"
           6 "six"
           7 "seven"
           8 "eight"
           9 "nine"
           10 "ten"
           11 "eleven"
           12 "twelve"
           13 "thirteen"
           14 "fourteen"
           15 "fifteen"
           16 "sixteen"
           17 "seventeen"
           18 "eighteen"
           19 "nineteen"
           20 "twenty"
           30 "thirty"
           40 "forty"
           50 "fifty"
           60 "sixty"
           70 "seventy"
           80 "eighty"
           90 "ninety"
           100 "hundred"
           1000 "thousand"
           })

(defn spellnumber
  [n]
  (def decade (* 10 (quot n 10)))
  (def cent (quot n 100))
  (def centremain (mod n 100))
  (cond
    (<= n 20) (dico n)
    (and (< n 100) (< n (+ decade 10))) (str (dico decade) " " (dico (mod n decade)))
    (and (< n 1000)) (str (dico cent) " " (dico 100) (if (pos? centremain)
                                                       (str " and " (spellnumber centremain))
                                                       "")
                          )
    (= n 1000) (str "one " (dico n))
    :else "N/A"
    )
  )
(defn remove-whitespace
  [s]
  (clojure.string/replace (clojure.string/trim s) #"\s{1,}" "")
  )

(def out (loop [iter 1, outstr ""]
           (if (= 1001 iter)
             outstr
             (recur (inc iter) (str outstr (spellnumber iter)))
             )
           )
  )
(count (remove-whitespace out))
; 21124

