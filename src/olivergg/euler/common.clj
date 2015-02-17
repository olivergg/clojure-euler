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
(defn ![n] (factorial n))


(defn isprime
  "Check primality of an integer n"
  [^Long n]
  (cond
    ; we start with some obvious cases of primality
    (= n 1) false
    (= n 2) true
    (= n 3) true
    (< n 0) false
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
  [initn]
  (loop [n initn k 2 acc ()]
    (if (= 1 n)
      acc
      (if (= 0 (mod n k))
        (recur (quot n k) k (conj acc k))
        (recur n (inc k) acc))
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

(defn countdiv
  "returns the number of divisors of n.
   if prime factorization of n is
   n = A^a x B^b x C^c x ...
   then, the number of divisors is (a+1)(b+1)(c+1)...
"
  [n]
  (if (< n 2)
    1
    (reduce * (map (fn [x] (inc (val x))) (frequencies (factor n))))
    )
  )


(defn apply-pow [base exp]
  (apply *' (repeat exp base)))



(defn sumproperdivisor
  "returns the sum of proper divisors of n.
  if prime factorization of n is
  n = A^a x B^b x C^c x ...
  then the sum of the proper divisors is (A^(a+1)-1 / A-1) x (B^(b+1)-1 / B -1) x ...
  "
  [n]
  (- (reduce * (map (fn [x]
                      (let [A (key x)
                            a (val x)]
                        (quot (- (apply-pow A (+ a 1)) 1) (- A 1)))
                      )
                    (frequencies (factor n))
                    )
             )
     n)
  )

(defn slowsumproperdivisor
  [n]
  (reduce + (properdivisors n))
  )


(assert (= (reduce + (properdivisors 100)) (sumproperdivisor 100)))
(assert (= (reduce + (properdivisors 28)) (sumproperdivisor 28)))
(assert (= (reduce + (properdivisors 28123)) (sumproperdivisor 28123)))


(def slowsumproperdivisor-memo (memoize slowsumproperdivisor))
(def sumproperdivisor-memo (memoize sumproperdivisor))

(defn isabundant
  [n]
  (and (> n 0) (> (sumproperdivisor-memo n) n))
  )

(assert (isabundant 12))


(defn tobase10
  [initn]
  (loop [n initn, out ()]
    (if (= n 0)
      out
      (recur (quot n 10) (conj out (mod n 10)))
      )
    )
  )

(defn frombase10
  [coll]
  (def size (count coll))
  (reduce + (map-indexed (fn[idx,val] (* val (apply-pow 10 (- (dec size) idx)))) coll))
  )

(assert (= 123 (frombase10 [1 2 3])))


(assert (= '(1 2 3) (tobase10 123)))