(ns olivergg.euler.problem_42
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            [clojure.java.io :refer :all :as io]
            [clojure.string :as str]
            )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 42
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tn [n]
  (/ (* n (inc n)) 2)
  )

(defn alphab-pos
  [c]
  (- (int c) 64)
  )

(defn alphavalue
  [string]
  (reduce + (map (fn [x] (alphab-pos x)) string))
  )
(assert (= 55 (alphavalue "SKY")))

(with-open [rdr (reader (io/resource "p042_words.txt"))]
  (doseq [line (line-seq rdr)]
    (def words (str/split line #","))
    (def alvals (map (fn[w] (alphavalue (str/replace w #"\"" ""))) words))
    (def maxval (reduce max alvals))
    (def maxn (int (Math/ceil (Math/sqrt (* maxval 2)))))
    (def tnnumbers (into #{} (map (fn[x] (tn x)) (range 1 (inc maxn)))))
    (println (count (filter (fn[i] (contains? tnnumbers i)) alvals)))
    )
  )

;162