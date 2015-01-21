(ns olivergg.euler.problem_28
  (:require [olivergg.euler.common :refer :all]
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 28
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn square [x] (* x x))

(assert (= 101 (+
                 1
                 (+ (square 3) (- (square 3) (* 1 2)) (- (square 3) (* 2 2)) (- (square 3) (* 3 2)))
                 (+ (square 5) (- (square 5) (* 1 4)) (- (square 5) (* 2 4)) (- (square 5) (* 3 4)))
                 )
           ))
(defn sumforodd [oddn]
  (def oddnsq (square oddn))
  (def oddnminusone (dec oddn))
  (if (= 1 oddn)
    1
    (+
      oddnsq
      (- oddnsq oddnminusone)
      (- oddnsq (* 2 oddnminusone))
      (- oddnsq (* 3 oddnminusone))
      )
    )
  )

(assert (= 101 (+ (sumforodd 1) (sumforodd 3) (sumforodd 5))))

(reduce + (map (fn[x] (sumforodd x)) (range 1 1002 2)))
;669171001