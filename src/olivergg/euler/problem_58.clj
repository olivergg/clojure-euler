(ns olivergg.euler.problem_58
  (:require [olivergg.euler.common :as common]
            [taoensso.timbre :as timbre]

            )
  )
(timbre/refer-timbre)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 58
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sq[x] (* x x))

(profiling/defnp corners[n]
  (let [
        width (+ (* 2 n) 1)
        c1 (sq width)
        c2 (+ (- c1 width) 1)
        c3 (+ (- c2 width) 1)
        c4 (+ (- c3 width) 1)
        primes (filter common/isprime (list c2 c3 c4))
        ]
    (count primes)
    )

  )

(def corners-memo (memoize corners))

(profiling/defnp countprimes [n]
  (loop [currentn n out 0]
    (if (= currentn 0) out
      (recur (dec currentn) (+ out (corners-memo currentn)))
      )
    )
  )


(defn ratioprimes [n]
  (let [width (+ (* 2 n) 1)
        countpri (countprimes n)
        totalnb (- (* 2 width) 1)
        ]

    (/ countpri totalnb)
    )
  )



(profile :info :Arithmetic (first  (for [i (drop 1 (range))
              :let [r (ratioprimes i)]
              :when (<= r 0.1)
              ]
                 (inc (* 2 i))
                 )
               )
)

;26241

;; still slow....


