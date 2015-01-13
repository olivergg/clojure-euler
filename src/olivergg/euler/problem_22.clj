(ns olivergg.euler.problem_22
  (:require [olivergg.euler.common :refer :all]
            [clojure.java.io :refer :all :as io]
            [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 22
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn alphab-pos
  [c]
  (- (int c) 64)
  )

(assert (= (alphab-pos \C) 3))
(assert (= (alphab-pos \Z) 26))

(defn alphavalue
  [string]
  (reduce + (map (fn [x] (alphab-pos x)) string))
  )
(assert (= 53 (alphavalue "COLIN")))

(with-open [rdr (reader (io/resource "p022_names.txt"))]
  (doseq [line (line-seq rdr)]
    (def theline (str/split line #","))
    (def sortedline (sort theline))
    (def indexeddico (map-indexed
                       (fn [idx,word]
                         (* (inc idx) (alphavalue (str/replace word #"\"" ""))))
                       sortedline))
    (println (reduce + indexeddico))
    )
  )

;871198282