(ns olivergg.euler.problem_15
  (:require [olivergg.euler.common :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 15
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cnk
  "binomial coefficients (For any set containing n elements, the number of distinct k-element subsets of it that can be formed) "
  [n,k]
  (if (>= n k)
    (/ (factorial n) (* (factorial (- n k)) (factorial k)))
    1)
  )

;; Example on a 3x3 grid. Let's define a path as the following vector (a succession of direction r (right) or d (down)
; (r r r d d d)
; (r d r d r d)
; there must be as much r in the path as the horizontal dimension (3 in this case)
; there must be as much d in the path as the vertical dimension (3 in this case).
; we can see that this number is the number of distinct 3-element subsets of the set containing all the paths elements.
; namely this is (6
;                 3 )
;
(cnk 20 10)




;; code below is to bruteforce find all the elements...will not work with n > 10
;(defn nexx
;  [ijtuple]
;  (if (nil? ijtuple)
;    ()
;    (remove
;      (fn[x] (= x ijtuple))
;      (list
;        {:i (max 0 (dec (get ijtuple :i 0))) :j (:j ijtuple)}
;        {:i (:i ijtuple) :j (max 0 (dec (get ijtuple :j 0)))}
;        )
;      )
;    )
;  )
;(defn f [coll] (mapcat nexx coll))
;(def start {:i 4 :j 4})
;start
;(nexx start)
;(f (nexx start))
;(f (f (nexx start)))
;(f (f (f (nexx start))))
;;; the idea was to apply (f (f (f ...))) until {:i 0 :j 0} appears.

