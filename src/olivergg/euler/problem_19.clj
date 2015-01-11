(ns olivergg.euler.problem_19
  (:require [olivergg.euler.common :refer :all]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 19
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; january 7th 1900 was a sunday
;; january 6th 1901 was a sunday
(defn iscentury
  [year]
  (= 0 (mod year 100))
  )

(assert (= true (iscentury 1900)))
(defn isleapyear
  [year]
  (cond
    (= 0 (mod year 4)) (if (iscentury year) (= 0 (mod year 400)) true)
    :else false
    )
  )

(assert (= true (isleapyear 1996)))
(assert (= true (isleapyear 2000)))
(assert (= false (isleapyear 1900)))

(defn daysinmonth
  [n,year]
  (cond
    (or (= n 9) (= n 4) (= n 6) (= n 11)) 30
    (= n 2) (if (isleapyear year) 29 28)
    :else 31
    )
  )
(assert (= 31 (daysinmonth 1 1900)))
(assert (= 30 (daysinmonth 11 1900)))
(assert (= 28 (daysinmonth 2 1900)))
(assert (= 29 (daysinmonth 2 1996)))

(defn add
  "Add the given number of days to the given date. Returns a (day,month,year) list"
  [day month year nbdays]
  (def daysinm (daysinmonth month year))
  (def diff (- daysinm (+ day nbdays)))
  (def remain (- nbdays (- daysinm day) 1));; remove 1 to the remain, since the first day of the next month is counted.
  (if (>= diff 0)
    (list (+ day nbdays) month year)
    (do
      (def incmonth (inc month))
      (def alteredmonth (if (= incmonth 13) 1 incmonth))
      (def alteredyear (if (= incmonth 13) (inc year) year))
      (recur 1 alteredmonth alteredyear remain)
      )
    )
  )
(time (loop [iter 1, count 0]
        (def res (add 6 1 1901 (* 7 iter)))
        (if (= 2001 (last res))
          count
          (if (= 1 (first res));; first day of the month
            (recur (inc iter) (inc count))
            (recur (inc iter) count)
            )
          )
        ))
;171
