(ns olivergg.euler.problem_30
  (:require [olivergg.euler.common :refer :all]
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 30
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;9474 = 9^4 + 4^4 + 7^4 + 4^4
(defn sumpowdigits [n,p]
  (reduce + (map (fn [x] (apply-pow x p)) (tobase10 n)))
  )


(assert (= 9474 (sumpowdigits 9474 4)))

; finding upper limit
(defn isupperlimit [n]
  (>= n (sumpowdigits n 5))
  )

(isupperlimit 99)
(isupperlimit 999)
(isupperlimit 9999)
(isupperlimit 99999)
(isupperlimit 999999)

(loop [iter 1]
  (if (= iter 999999)
    (println "done")
    (do
      (if (= iter (sumpowdigits iter 5))
        (println iter)
        )
      (recur (inc iter))
      )
    )
  )


(+ 4150
   4151
   54748
   92727
   93084
   194979)
