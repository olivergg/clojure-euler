(ns olivergg.euler.problem_18
  (:require [olivergg.euler.common :refer :all]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 18
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;                    75
;                   95 64
;                 17 47 82
;                18 35 87 10
;               20 04 82 47 65
;              19 01 23 75 03 34
;             88 02 77 73 07 63 67
;           99 65 04 28 06 16 70 92
;          41 41 26 56 83 40 80 70 33
;        41 48 72 33 47 32 37 16 94 29
;      53 71 44 65 25 43 91 52 97 51 14
;     70 11 33 28 77 73 17 78 39 68 17 57
;    91 71 52 38 17 14 91 43 58 50 27 29 48
;   63 66 04 68 89 53 67 30 73 16 69 87 40 31
; 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

(def inittriangle [[75]
               [95 64]
               [17 47 82]
               [18 35 87 10]
               [20  4 82 47 65]
               [19  1 23 75  3 34]
               [88  2 77 73  7 63 67]
               [99 65  4 28  6 16 70 92]
               [41 41 26 56 83 40 80 70 33]
               [41 48 72 33 47 32 37 16 94 29]
               [53 71 44 65 25 43 91 52 97 51 14]
               [70 11 33 28 77 73 17 78 39 68 17 57]
               [91 71 52 38 17 14 91 43 58 50 27 29 48]
               [63 66  4 68 89 53 67 30 73 16 69 87 40 31]
               [ 4 62 98 27 23  9 70 98 73 93 38 53 60  4 23]
               ]
  )



(defn triij
  [triangle, i,j]
  (if (and (<= j i) (>= i 0) (>= j 0))
    ((triangle i) j)
    nil
    )
  )

(defn predecessor
  [triangle, i,j]
  (filter identity (list (triij triangle (dec i) j) (triij triangle (dec i) (dec j))))
  )

(defn bestpredecessor
  [triangle, i, j]
  (def pred (predecessor triangle i j))
  (let [ [first second & rest] pred]
    ((fnil max 0 0) first second)
    )
  )


(defn mapbestrow
  [triangle, row]
  (into [] (map-indexed (fn[x,val] (+ val (bestpredecessor triangle row x)) ) (triangle row)))
  )



(def maptolengthtriangle (loop [iter 0, triangle inittriangle]
                           (if (= 14 iter)
                             (do
                               (def newarray (mapbestrow triangle iter))
                               (assoc triangle iter newarray)
                               )
                             (do
                               (def newarray (mapbestrow triangle iter))
                               (def newtria (assoc triangle iter newarray))
                               (recur (inc iter) newtria)
                               )
                             )
                           )
  )

(apply max (last maptolengthtriangle))
; 1074
