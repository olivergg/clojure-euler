(ns olivergg.euler.problem_34
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 34
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(reduce + (map (fn[x] (factorial x)) (tobase10 145)))

(defn isfactdigit
  [n]
  (def l (tobase10 n))
  (loop [coll l, currentval 0]
    (let [[a & rest] coll]
      (cond
        (nil? rest) (= (+ currentval (! a)) n)
        (and (not (nil? rest)) (> currentval n)) false
        :else (recur rest (+ currentval (! a)))
        )
      )
    )
  )

(assert (isfactdigit 145))

(- 9999999999 (* 10 (! 9)))
(- 999999999 (* 9 (! 9)))
(- 99999999 (* 8 (! 9)))
(- 9999999 (* 7 (! 9)))
(- 999999 (* 6 (! 9))) ;STOP
; upper limit is 9999999

(time (reduce + (for [i (range 3 9999999)
                      :when (isfactdigit i)
                      ]
                  i
                  )
              )
      )
;40730