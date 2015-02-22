(ns olivergg.euler.problem_39
  (:require [olivergg.euler.common :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 39
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn findpythagormul
  [inita initb initc targetn]
  (if (= (+ inita initb initc) targetn)
    {:a (min inita initb) :b (max initb inita) :c initc}
    (loop [a inita
           b initb
           c initc,
           multi 1
           out ()]
      (cond
        (= (+ a b c) targetn) out
        (> (+ a b c) targetn) nil
        :else (do
                (def newa (* multi inita))
                (def newb (* multi initb))
                (def newc (* multi initc))
                (recur newa newb newc (inc multi) {:a (min newa newb) :b (max newa newb) :c newc})
                )
        )
      )
    )
  )

(defn countsol
  [n]
  {:n n
   :c (count (into #{} (for [q (range 1 50)
                             p (range (inc q) 50)
                             :let [pp (* p p)
                                   qq (* q q)
                                   a (- pp qq)
                                   b (* 2 p q)
                                   c (+ pp qq)
                                   f (findpythagormul a b c n)
                                   ]
                             :when (and (<= (+ a b c) 1000) (not (nil? f)))
                             ]
                         f
                         )
                   )
             )
   }
  )
(time (reduce (fn[x,y] (if (> (:c x) (:c y)) x y)) (map countsol (range 121 1001))))
;{:n 840, :c 8}

