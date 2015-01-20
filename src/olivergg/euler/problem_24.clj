(ns olivergg.euler.problem_24
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 24
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn findnthperm
  [initdigitset, initidx]
  (loop [currentset initdigitset, currentindex initidx, out ()]
    (if (= 1 (count currentset))
      (reverse (conj out (first currentset)))
      (do
        (def setsize (count currentset))
        (def groupsize (/ (factorial setsize) setsize))
        (def groupnum (int (Math/ceil (/ currentindex groupsize))))
        (def digittoconj (nth (vec currentset) (- groupnum 1)))
        (def indexoffirstelemofgroup (+ (* (- groupnum 1) groupsize) 1))
        (def newindex (+ (- currentindex indexoffirstelemofgroup) 1))
        (recur (disj currentset digittoconj) newindex (conj out digittoconj))
        )
      )
    )
  )

(findnthperm (sorted-set 0 1 2 3 4 5 6 7 8 9) 1000000)
;(2 7 8 3 9 1 5 4 6 0)