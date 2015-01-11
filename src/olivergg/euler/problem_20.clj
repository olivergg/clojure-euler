(ns olivergg.euler.problem_20
  (:require [olivergg.euler.common :refer :all]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 20
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;// we assume there is no bigint, to 100! is too big to be computed directly.
;// hence, we reimplement artithmetic operation (* and +) on the list representation of a number.
;// 98 is represented by '(8 9), 7 by '(7 0) (we use a reverse notation to simplify list manipulation).

(defn sdiv
  [n,d]
  (list (quot n d) (mod n d))
  )


(defn decimalsplit
  [n]
  (sdiv n 10)
  )

(defn generate0 [c] (map (fn [x] 0) (range 0 c)))

(defn conjj
  [coll,x]
  (if (zero? x)
    coll
    (conj coll x)
    )
  )


(defn aline
  [initx, initcoll]

  (loop [x initx,remain 0, coll initcoll,out ()]
    (if (empty? coll)
      (reverse (conjj out remain))
      (do
        (def y (first coll))
        (def xy (+ (* x y) remain))
        (let [[xydec xyunit] (decimalsplit xy)]
          (recur x xydec (rest coll) (conj out xyunit))
          )
        )
      )
    )
  )

(defn thelines
  [initxcoll, initcoll]
  (loop [iter 0, xcoll initxcoll, coll initcoll, out ()]
    (if (empty? xcoll)
      out
      (do
        (def fi (first xcoll))
        (def zeros (generate0 iter))
        (def newout (if (= 0 iter)
                      (conj out (aline fi coll))
                      (conj out (concat zeros (aline fi coll)))
                      ))
        (recur (inc iter) (rest xcoll) coll newout)
        )
      )
    )
  )

(defn sumcoll
  [initcoll1, initcoll2]
  (loop [coll1 initcoll1, coll2 initcoll2, remain 0, out ()]
    (cond
      (and (empty? coll1) (empty? coll2)) (reverse (conjj out remain))
      :else (do
              (def fcol1 ((fnil identity 0) (first coll1)))
              (def fcol2 ((fnil identity 0) (first coll2)))
              (def decsplit (decimalsplit (+ fcol1 fcol2 remain)))
              (recur (rest coll1) (rest coll2) (first decsplit) (conj out (last decsplit)))
              )
      )
    )
  )


(defn productcoll
  [coll1, coll2]
  (reduce sumcoll (thelines coll1 coll2))
  )

(defn h [n] (reverse (decimalsplit n)))

(def numberstoreduce (map (fn [x] (h (- x))) (range -99 -1)))

(def outlist (reduce productcoll numberstoreduce))

(println (reduce + outlist))


; 648

;;; faster solution with bigint....
;(loop [f (factorial 99) sum 0]
;  (if (> f 0)
;    (recur (quot f 10) (+ sum (mod f 10)))
;    sum)