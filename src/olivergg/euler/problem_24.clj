(ns olivergg.euler.problem_24
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 24
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn firstofgroup
  [digits, start]
  (def digitssize (count digits))
  (def firstidx (+ 1
                   (* start (factorial (dec digitssize))
                      )
                   )
    )
  {:firstidx firstidx :item (conj (sort (disj digits start)) start)}
  )

(defn lastofgroup
  [digits, start]
  (def digitssize (count digits))
  (def lastidx (+ (factorial (dec digitssize))
              (* start (factorial (dec digitssize))
              )
    )
  )
  {:lastidx lastidx :item (conj (sort-by (fn[x] (- x)) (disj digits start)) start)}
  )

(def digitsset (sorted-set 0 1 2))
; 1 => 0 1 2
; 2 => 0 2 1
; 3 => 1 0 2
; 4 => 1 2 0
; 5 => 2 0 1
; 6 => 2 1 0

(defn findindex
  [initdigitset, tofound]
  (loop [set initdigitset, currentstart 0, currentindex 1]
    (println "set " set "currentstart " currentstart " currentindex " currentindex)
    (def g (lastofgroup set currentstart))
    (cond
      (>= tofound currentindex) (recur set (inc currentstart) (+ currentindex (:lastidx g)))
      (< tofound currentindex) (recur (disj set currentstart) 0 (+ currentindex (:lastidx g)))
      :else (println "found")
      )
    )
  )


(lastofgroup digitsset 1)


(defn findgroupat
  [digitsset, n]
  (loop [i 0]
    (def lg (lastofgroup digitsset i))
    (if (> (:lastidx lg) n)
      (mod (factorial (count digitsset)) (:lastidx lg))
      (recur (inc i))
      )
    ;(lastofgroup digitsset 1)
    ;(lastofgroup digitsset 2)
    )
  )
;(findindex digitsset 5)
(findgroupat digitsset 5)