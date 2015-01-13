(ns olivergg.euler.common)

(defmacro dbg
  "debug function call"
  [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn factorial
  [n]
  (loop [cnt n, acc 1]
    (if (= 0 cnt)
      acc
      (recur (dec' cnt) (*' acc cnt)))
    )
  )


(defn isprime
  "Check primality of an integer n"
  [^Long n]
  (cond
    ; we start with some obvious cases of primality
    (= n 1) false
    (= n 2) true
    (= n 3) true
    (= 0 (mod n 2)) false
    :else (loop [start 3
                 acc true]
            (cond
              (>= start (Math/sqrt n)) (and acc (not (zero? (mod n start))))
              :else (recur (+ start 2) (do
                                         ;                                          (when (zero? (mod n start))
                                         ;                                          (println "n =" n "is divisible by " start "which is prime ? " (isprime start) )
                                         ;                                          )
                                         (and acc (not (zero? (mod n start))))
                                         )
                           )
              )
            )
    )
  )

(defn generate-prime-numbers
  "Generate prime numbers between 2 and n"
  ([n] (generate-prime-numbers n 3))
  ([n start]
    (loop [iter start                                       ;start iteration at 3
           acc (if (= start 3) [2] [])                      ;start with the first prime number 2
           ]
      (cond
        (= n 1) ()
        (= n 2) acc
        (= n 3) (conj acc 3)
        (= iter n) (if (isprime iter) (conj acc iter) acc)
        (isprime iter) (recur (inc iter) (conj acc iter))
        :else (recur (inc iter) acc)
        )
      )
    )
  )

(defn nextprime
  [n]
  (loop [iter (inc n)]
    (if (isprime iter)
      iter
      (recur (inc iter))
      )
    )
  )

(defn factor
  "prime factorization by division trial"
  [n]
  (if (isprime n)
    (list n)
    (loop [x n,
           currentprime 2,
           outfactors ()]
      (cond
        (> currentprime (Math/sqrt n)) (do
                                         ;(println "end " currentprime "because square root of x " (Math/sqrt n) " is less than "currentprime )
                                         outfactors
                                         )
        (zero? (mod x currentprime))
        (do
          ;(println "current prime is " currentprime "and outfactors are " outfactors)
          (recur (quot x currentprime) currentprime (conj outfactors currentprime))
          )
        :else
        (recur n (nextprime currentprime) outfactors)
        )
      )
    )
  )






(defn gcd
  "greatest common divisor of a and b."
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn rhofactor
  [n]
  (let [g (fn [t] (mod (+' (*' t t) (rand-int 100)) n))
        initseed (+ 1 (rand-int 100))
        ]
    (if (= n 1)
      nil
      (loop [x initseed, y initseed, d 1]
        (if (= d 1)
          (do
            (let [x (g x) y (g (g y))]
              (recur x y (gcd (if (> x y) (- x y) (- y x)) n))
              )
            )
          (if (> d 1)
            d
            (println "failure d " d " n" n)
            )
          )
        )
      )
    )
  )

(defn properdivisors
  [n]
  (if (> n 0)
    (loop [i 1, out #{1}]
      (def remain (mod n i))
      (def quoti (quot n i))
      (if (= i n)
        (disj out n)
        (if (zero? remain)
          (recur (inc i) (conj out remain quoti))
          (recur (inc i) out)
          )
        )
      )
    #{}
    )
  )

(defn  sumproperdivisor
  [n]
  (reduce + (properdivisors n))
  )

(def sumproperdivisor-memo (memoize sumproperdivisor))

(defn isabundant
  [n]
  (> (sumproperdivisor-memo n) n)
  )

(defn isdeficient
  [n]
  (< (sumproperdivisor-memo n)  n)
  )

(defn isperfect
  [n]
  (= (sumproperdivisor-memo n)  n)
  )

(assert (isabundant 12))
(assert (isperfect 28))
(assert (isdeficient 13))
