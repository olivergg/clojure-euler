(ns olivergg.euler.problem_40
  (:require [olivergg.euler.common :refer :all])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 40
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 0.123456789101112131415161718192021222324....

; let's define some groups :
;G1 = 123456789  => 9 1-digit numbers
;G2 = 10111213.... 99 => 90 2-digits numbers
;G3 = 100101102103..... 999 => 900 3-digits numbers
;G4 = 1000100110002..... 9999 => 9000 4-digits numbers
;G5 = 1000010001100002..... 99999 => 90000 5-digits numbers
;G6 = 100000100001100002..... 999999 => 900000 6-digits numbers
;Gn = xxxxxxxxxxxxxxxxxxxxxxxxxxxxx => n-digits numbers

(defn cumul
  "Create a cumul list from the given collection
  The nth item of this list is equals to the sum of the (n-1) previous items in the original collection.
  "
  [initcoll]
  (loop [[head & tail :as coll] initcoll
         out ()
         ]
    (cond
      (seq coll) (recur
                   tail
                   (conj out (+ ((fnil identity 0) (first out))
                                head)))
      :else (reverse out)
      )
    )
  )

; create the cumul map to compute the last idx of each group.7
; note : there is no need to go beyond the G6 group
(def lastidx (cumul
               (map (fn[x]
                      (* (inc x) (* 9 (apply-pow 10 x)))
                      )
                    (range 0 6)
                    )
               )
  )

; a groupdef is a sorted-map
(def groupdef (into (sorted-map)
                    (map-indexed (fn[groupkey,val]
                                   {(inc groupkey) val}
                                   )
                                 lastidx
                                 )
                    )
  )

(defn getgroup
  "Return the group of which the given integer n belongs. A groupedef must be provided (a sorted-map)"
  [n agroupdef]
  (loop [[f s & more :as gdef] (seq agroupdef)]
    (def lowerval (val f))
    (def upperval (val s))
    (cond
      (and (>= n lowerval) (<= n upperval)) s
      (seq gdef) (recur (rest gdef))
      :else nil
      )
    )
  )

(defn findvalatnth
  [n]
  (def groupofn (getgroup n groupdef))
  ;(println "group of n " groupofn)
  (def keyofgroup (key groupofn))
  ;(println "key of group "keyofgroup)
  (def groupstartindex (inc (groupdef (dec keyofgroup))))
  ;(println "groupstartindex " groupstartindex)
  (def indexinsidegroup (- n groupstartindex))
  ;(println "indexinsidegroup " indexinsidegroup)
  (def normalizedidx (quot indexinsidegroup keyofgroup))
  ;(println "normalizedidx "normalizedidx)
  (def normalizedremain (mod indexinsidegroup keyofgroup))
  ;(println "normalizedremain "normalizedremain)
  (def startofgroup (int (apply-pow 10 (dec keyofgroup))))
  ;(println "start of group " startofgroup)
  (def valatidx (+ startofgroup normalizedidx))
  ;(println "valatidx " valatidx)
  (def valatidxbase10 (tobase10 valatidx))
  ;(println "valatidxbase10 " valatidxbase10)
  (nth valatidxbase10 normalizedremain)
  )



(* 1
   (findvalatnth 10)
   (findvalatnth 100)
   (findvalatnth 1000)
   (findvalatnth 10000)
   (findvalatnth 100000)
   (findvalatnth 1000000))

; 210