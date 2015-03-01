(ns olivergg.euler.problem_43
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 43
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def divisors (generate-prime-numbers 17 2))

(defn subdivisiblity
  [initstr]
  (loop [astr initstr
         dvs divisors
         ]
    (cond
      (> (count astr ) 3) (do
                            ; (println astr)
                            (def parsedint (parse-int (subs astr 1 4)))
                            ;(println parsedint (dvs 0))
                            (if (= 0 (mod parsedint (dvs 0)))
                              (recur (apply str (rest astr)) (subvec dvs 1))
                              {:b false :res nil}
                              )
                            )
      :else {:b true :res (parse-int initstr)}
      )
    )
  )



(time
  (reduce
    (fn [t1 t2] {:res (+ (:res t1) (:res t2))})
    (filter
      (fn [y] (:b y))
      (flatten (genpermut-apply (into (sorted-set) (range 0 10))
                                (fn [x] (subdivisiblity (apply str x)))
                                )
               )
      )
    )
  )

;16695334890