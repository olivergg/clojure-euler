(ns olivergg.euler.problem_38
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 38
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def authdigits #{1 2 3 4 5 6 7 8 9})


(defn isconcatprodpandigital
  "Returns a {:b true :prod XXXXX} map if the product of anint with (1,2,3,...,n) is 1 to 9 pandigital
  {:b false :prod nil} otherwise.
  "
  [anint, n]
  (def rr (range 1 (inc n)))
  (def mappedrr (map (fn[x] (* x anint) ) rr))
  (loop [[first & rest] mappedrr
         out #{}
         nbdigits 0
         proddig ()
         ]
    ;(println "first " first "rest " rest "out " out)
    (cond
      (not (nil? first)) (do
                           (def firstlist (tobase10 first))
                           (def firstset (into #{} firstlist))
                           ;(println "firstlist " firstlist)
                           (if (and
                                 (= (count firstlist) (count firstset))
                                 (= #{} (intersection firstset out))
                                 (< nbdigits 9)
                                 )
                             (recur rest
                                    (union out firstset)
                                    (+ nbdigits (count firstlist))
                                    (concat proddig firstlist)
                                    )
                             {:b false :prod nil}
                             )
                           )
      :else {:b (= out authdigits) :prod (frombase10 proddig)}
      )
    )
  )

(assert (:b (isconcatprodpandigital 9 5)))
(assert (:b (isconcatprodpandigital 192 3)))
(assert (not (:b (isconcatprodpandigital 192 4))))

(reduce (fn [x y]
          (if ((fnil > 0 0) (:prod x) (:prod y))
            x
            y
            )
          )
        (for [i (range 1 100000)
              j (range 1 10)
              :let [a (isconcatprodpandigital i j)
                    proda (:prod a)]
              :when (and (:b a)
                         (<= proda 987654321)
                         (> proda 918273645)
                         )
              ]
          {:i i :j j :prod proda}
          )
        )

;{:i 9327, :j 2, :prod 932718654}

