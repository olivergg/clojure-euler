(ns experiment.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(def x 3)


(defn factorial
  [n]
  (loop [cnt n, acc 1]
    (if (= 0 cnt)
      acc
      (recur (dec cnt) (* acc cnt)))
    )
  )


(factorial 6)

(if true
  (do (println "Success!")
    "abra cadabra")
  (do (println "Failure :(")
    "hocus pocus"))


(def toto ["test" "teooo", "oooo"])
(println toto)

(nil? nil)

(= 1 1)

(get-in {:a 0 :b {:c "boom"}} [:b :c])

(conj '(1 2 3 4) 5)


(def toto 4)
(identity (identity 'toto))


(defn too-enthusiastic
  "Return a cheer that might be a bit too enthusiastic"
  [name]
  (str "OH. MY. GOD! " name " YOU ARE MOST DEFINITELY LIKE THE BEST "
    "MAN SLASH WOMAN EVER I LOVE YOU AND WE SHOULD RUN AWAY TO SOMEWHERE"))

(too-enthusiastic "Zelda")



(defn x-chop
  "Describe the kind of chop you're inflicting on someone"
  ([name chop-type]
    (str "I " chop-type " chop " name "! Take that!"))
  ([name]
    (x-chop name "karate")))

(x-chop "Kayne West")


(defn codger-communication
  [whippersnapper]
  (str "Get off my lawn, " whippersnapper "!!!"))

(defn codger
  [& whippersnappers] ;; the ampersand indicates the "rest-param"
  (map codger-communication whippersnappers))

(codger "test" 4 "tutu")


(defn chooser
  [[first-choice second-choice third-choice & unimportant-choices]]
  (println (str "Your first choice is: " first-choice))
  (println (str "Your second choice is: " second-choice))
  (println (str "Your third choice is: " third-choice))
  (println (str "We're ignoring the rest of your choices. "
             "Here they are in case you need to cry over them: "
             (clojure.string/join ", " unimportant-choices))))


(chooser ["Marmalade", "Handsome Jack", "Pigpen", "Aquaman","A","B","C","D"])


(defn announce-treasure-location
  [{lat :lat lng :lng}]
  (println (str "Treasure lat: " lat))
  (println (str "Treasure lng: " lng)))




(. System currentTimeMillis)
(let [x 3 y 4] x y)

(if true 1 3)


(defn newton_epsilon [f eps x0]
  "Newton method"
  (let [df (fn [x] (/ (- (f (+ x eps)) (f x)) eps))]
    (loop [x x0
           iteration 0]
      (let [pas (/ (f x) (df x))]
        (if (< pas eps)
          (do
            (- x pas)
            (println "solution is" x "found after" iteration "iterations")
            )
          (recur (- x pas) (inc iteration))
          )
        )
      )
    )
  )

(time (newton_epsilon (fn [x] (- (- (* x x) x) 1))  1E-7 2  ))

(time (newton_epsilon (fn [x] (- (* x x) 2))  1E-7 2  ))
(time (Math/sqrt 2))



;;; END OF SANDBOX


;;; STARTING EULER PROJECT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn summul
  "the sum of all the multiples of 3 or 5 below n"
  [n]
  (loop [iteration 1
         sum 0]
    (if (= iteration n)
      sum
      (recur (inc iteration)
        (+ sum (if (or  (zero? (mod iteration 3)) (zero? (mod iteration 5)))
                 iteration
                 0)
          )
        )
      )
    )
  )
(summul 1000)
;; 233168

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fibbo
  "Append at the begining of the list, the sum of the two first elements of that list"
  [list]
  (def safeget (fnil identity 0))
  (let [[first second & rest] list
        safefirst (safeget first)
        safesecond (safeget second)
        ]
    (conj list (+ safefirst safesecond))
    )
  )


(fibbo '(2 1))
(fibbo (fibbo (fibbo '(2 1))))

(defn filtereven-less-fourmillion [x] (and (= 0 (mod x 2)) (< x 4000000)))
(reduce + (filter  filtereven-less-fourmillion (first (drop 56 (take 57 (iterate fibbo '(2 1)))))))


;; 4613732


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
              :else (recur (+ start 2)  (do
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
    (loop [iter start  ;start iteration at 3
           acc (if (= start 3) [2] []) ;start with the first prime number 2
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

(nextprime 995)


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
          (recur  (quot x currentprime) currentprime (conj outfactors currentprime))
          )
        :else
        (recur n (nextprime currentprime) outfactors)
        )
      )
    )
  )


(time (factor 60085147514))



(defn gcd
  "greatest common divisor of a and b."
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn rhofactor
  [n]
  (let [g (fn [t] (mod (+' (*' t t) (rand-int 100)) n) )
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
            (println "failure d "d " n" n)
            )
          )
        )
      )
    )
  )

(isprime 600851475143);; not prime


;; factorization using trial division would be too slow for such a great number.
(def n 60085147514)
(def a (rhofactor n))
(def b (rhofactor (/ n a)))
(def c (rhofactor (/ n a b)))
(def d (rhofactor (/ n a b c)))
;; (def e (rhofactor (/ n a b c d))) might be long
;; (println a b c d e)
(isprime 6857) ;; greatest prime factor
;; 6857

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn str-reverse [s] (apply str (reverse s)))

(defn ispalindromic
  [n]
  (= (str n) (str-reverse (str n)))
  )

;; must be between 100 x 100 and  999 x 999
;; more likely to be closest to 1000000 so we start at the end (using negative index in the range).
(first
  (for [i (range -1000 -100)
        j (range -1000 -100)
        :let [n (* (- i) (- j))]
        :when (ispalindromic n)]
    {:n n :i (- i) :j (- j)}))

; 580085 =  995 x 583


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(factor 2520)
(* 7 5 3 3 2 2 2)
;; PPCM classique
(* 19 17 13 11 7 5 3 3 2 2 2 2)
;; 232792560


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(-
  (java.lang.Math/pow (apply + (range 1 11)) 2)
  (apply + (map #(* % %)(range 1 11)))
  )

(-
  (int (java.lang.Math/pow (apply + (range 1 101)) 2))
  (apply + (map #(* % %)(range 1 101)))
  )
; 25164150

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(last (take 6 (generate-prime-numbers 100)))
(last (take 10001 (generate-prime-numbers 150000))) ;;kinda bruteforce...
;104743


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def big1000digits "73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450")

(def stringified10000digits (apply str (clojure.string/split-lines big1000digits)))

(defn parseint
  "parse a string to an integer (compatible with char)"
  [s]
  (Integer/parseInt (re-find #"\A-?\d+" s))
  )
(defn productstring
  [input]
  (loop [s input, out 1]
    (let [[h & t] s]
      (if (nil? t)
        (* out (parseint (str h)))
        (recur t (* out (parseint (str h))))
        )
      )
    )
  )

(require '[clojure.string :as str])
(defn findmax
  [inputstring]
  (loop [s inputstring, maxmap {:val 0 :ints ""}]
    (let [head (subs s 0 (min 13 (count s)))
          reststring (subs s 1 (count s))
          newval (productstring head)
          max (:val maxmap)
          ]
      (cond
        (empty? reststring) maxmap
        (and
          (> newval max)
          (= 13 (count head))) (recur reststring (conj maxmap {:val newval :ints  head}))
        :else (recur reststring maxmap)
        )
      )
    )
  )
(findmax stringified10000digits)

;23514624000 = 5x5x7x6x6x8x9x6x6x4x8x9x5



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1 < a < b < c
; a²+b²=c²
; a+b+c = 1000
;
;
;l y a équivalence entre
;
;(i) \quad (x,y,z) est un triplet pythagoricien primitif avec x impair.
;(ii) Il existe  (p,q) \in\N^{*2}  avec p > q , p et q premiers entre eux et de parités différentes, tels que
;\quad x=p^2-q^2,\quad y=2pq\quad{\rm et}\quad z=p^2+q^2.

(time (first (for [q (range 1 1000)
                   p (range q 1000)
                   :let [pp (* p p)
                         qq (* q q)
                         a (- pp qq)
                         b (* 2 p q)
                         c (+ pp qq)
                         ]
                   :when (and (= 1000 (+ a b c)))
                   ]
               (do
                 (println "p "p "q " q "a " a "b " b "c " c "pgcd de p et q " (gcd q p))
                 {:a a :b b :c c :abc (* a b c)}
                 )
               )
        )
  )

;; {:a 200, :b 375, :c 425, :abc 31875000} multiple du triplet primitif (8,15,17) (facteur (gcd p q) = 5)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reduce + (generate-prime-numbers 10))

; replace comment by time.
(comment (reduce + (generate-prime-numbers 2000000)))

;; 142913828922



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def mymatrix [[ 8  2 22 97 38 15  0 40  0 75  4  5  7 78 52 12 50 77 91  8]
               [49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48  4 56 62  0]
               [81 49 31 73 55 79 14 29 93 71 40 67 53 88 30  3 49 13 36 65]
               [52 70 95 23  4 60 11 42 69 24 68 56  1 32 56 71 37  2 36 91]
               [22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80]
               [24 47 32 60 99  3 45  2 44 75 33 53 78 36 84 20 35 17 12 50]
               [32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70]
               [67 26 20 68  2 62 12 20 95 63 94 39 63  8 40 91 66 49 94 21]
               [24 55 58  5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72]
               [21 36 23  9 75  0 76 44 20 45 35 14  0 61 33 97 34 31 33 95]
               [78 17 53 28 22 75 31 67 15 94  3 80  4 62 16 14  9 53 56 92]
               [16 39  5 42 96 35 31 47 55 58 88 24  0 17 54 24 36 29 85 57]
               [86 56  0 48 35 71 89  7  5 44 44 37 44 60 21 58 51 54 17 58]
               [19 80 81 68  5 94 47 69 28 73 92 13 86 52 17 77  4 89 55 40]
               [ 4 52  8 83 97 35 99 16  7 97 57 32 16 26 26 79 33 27 98 66]
               [88 36 68 87 57 62 20 72  3 46 33 67 46 55 12 32 63 93 53 69]
               [ 4 42 16 73 38 25 39 11 24 94 72 18  8 46 29 32 40 62 76 36]
               [20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74  4 36 16]
               [20 73 35 29 78 31 90  1 74 31 49 71 48 86 81 16 23 57  5 54]
               [ 1 70 54 71 83 51 54 69 16 92 33 48 61 43 52  1 89 19 67 48]])

(defn between
  "returns true if a <= x < b"
  [x,a,b]
  (and (>= x a) (< x b))
  )

(defn ij2
  "Returns the value at i,j or else 1 if i or j are out of range
i = index of rows, j index of column
"
  [v,i,j]
  (def n (count v))
  (if (and (between i 0 n) (between j 0 n))
    ((v i) j)
    1
    )
  )

(defn evalatij
  [v,i,j]
  (def up (* (ij2 v i j) (ij2 v (- i 1) j) (ij2 v (- i 2) j) (ij2 v (- i 3) j)))
  (def down (* (ij2 v i j) (ij2 v (+ i 1) j) (ij2 v (+ i 2) j) (ij2 v (+ i 3) j)))
  (def left (* (ij2 v i j) (ij2 v i (- j 1)) (ij2 v i (- j 2)) (ij2 v i (- j 3))))
  (def right (* (ij2 v i j) (ij2 v i (+ j 1)) (ij2 v i (+ j 2)) (ij2 v i (+ j 3))))
  (def diagtopleft (* (ij2 v i j) (ij2 v (- i 1) (- j 1)) (ij2 v (- i 2) (- j 2)) (ij2 v (- i 3) (- j 3))))
  (def diagbottomright (* (ij2 v i j) (ij2 v (+ i 1) (+ j 1)) (ij2 v (+ i 2) (+ j 2)) (ij2 v (+ i 3) (+ j 3))))
  (def diagtopright (* (ij2 v i j) (ij2 v (- i 1) (+ j 1)) (ij2 v (- i 2) (+ j 2)) (ij2 v (- i 3) (+ j 3))))
  (def diagbottomleft (* (ij2 v i j) (ij2 v (- i 1) (+ j 1)) (ij2 v (- i 2) (+ j 2)) (ij2 v (- i 3) (+ j 3))))
  (max up down left right diagtopleft diagtopright diagbottomleft diagbottomright)
  )

(defrecord MaxHolder [i j val]
  java.lang.Comparable
  (compareTo [this a] (compare  (:val this) (:val a) ))
  )
(defn mymax
  [a,b]
  (if (= 1 (compare a b))
    a
    b
    )
  )
(def msg1 (->MaxHolder 0 0 6))
(def msg2 (->MaxHolder 2 2 4))
(mymax msg2 msg1)

(defn matrixmaxby
  [matrix,maxfun]
  (def n (count matrix))
  (loop [i 0
         imax (->MaxHolder -1 -1 0)
         ]
    (if (= i n)
      imax
      (recur
        (inc i)
        (mymax
          imax
          (loop [j 0
                 jmax imax]
            (if (= j n)
              jmax
              (recur
                (inc j)
                (let [evalij (maxfun matrix i j)]
                  (if (>  evalij (:val jmax))
                    (->MaxHolder i j evalij)
                    jmax
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
(matrixmaxby mymatrix evalatij)
;; #experiment.core.MaxHolder{:i 15, :j 3, :val 70600674}
(evalatij mymatrix 15 3)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defn tn
  "nth triangular number"
  [n]
  (/ (* n (- n 1)) 2)
  )

(time (first (for [n (range 10000 15000)
                   :let [tofn (tn n)
                         c (countdiv tofn)
                         ]
                   :when (> c 500)
                   ]
               (do
                 (println tofn c n)
                 tofn
                 )
               )
        )
  )
; 76576500 (the 12376th triangular number)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 13
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def tableau [37107287533902102798797998220837590246510135740250
              46376937677490009712648124896970078050417018260538
              74324986199524741059474233309513058123726617309629
              91942213363574161572522430563301811072406154908250
              23067588207539346171171980310421047513778063246676
              89261670696623633820136378418383684178734361726757
              28112879812849979408065481931592621691275889832738
              44274228917432520321923589422876796487670272189318
              47451445736001306439091167216856844588711603153276
              70386486105843025439939619828917593665686757934951
              62176457141856560629502157223196586755079324193331
              64906352462741904929101432445813822663347944758178
              92575867718337217661963751590579239728245598838407
              58203565325359399008402633568948830189458628227828
              80181199384826282014278194139940567587151170094390
              35398664372827112653829987240784473053190104293586
              86515506006295864861532075273371959191420517255829
              71693888707715466499115593487603532921714970056938
              54370070576826684624621495650076471787294438377604
              53282654108756828443191190634694037855217779295145
              36123272525000296071075082563815656710885258350721
              45876576172410976447339110607218265236877223636045
              17423706905851860660448207621209813287860733969412
              81142660418086830619328460811191061556940512689692
              51934325451728388641918047049293215058642563049483
              62467221648435076201727918039944693004732956340691
              15732444386908125794514089057706229429197107928209
              55037687525678773091862540744969844508330393682126
              18336384825330154686196124348767681297534375946515
              80386287592878490201521685554828717201219257766954
              78182833757993103614740356856449095527097864797581
              16726320100436897842553539920931837441497806860984
              48403098129077791799088218795327364475675590848030
              87086987551392711854517078544161852424320693150332
              59959406895756536782107074926966537676326235447210
              69793950679652694742597709739166693763042633987085
              41052684708299085211399427365734116182760315001271
              65378607361501080857009149939512557028198746004375
              35829035317434717326932123578154982629742552737307
              94953759765105305946966067683156574377167401875275
              88902802571733229619176668713819931811048770190271
              25267680276078003013678680992525463401061632866526
              36270218540497705585629946580636237993140746255962
              24074486908231174977792365466257246923322810917141
              91430288197103288597806669760892938638285025333403
              34413065578016127815921815005561868836468420090470
              23053081172816430487623791969842487255036638784583
              11487696932154902810424020138335124462181441773470
              63783299490636259666498587618221225225512486764533
              67720186971698544312419572409913959008952310058822
              95548255300263520781532296796249481641953868218774
              76085327132285723110424803456124867697064507995236
              37774242535411291684276865538926205024910326572967
              23701913275725675285653248258265463092207058596522
              29798860272258331913126375147341994889534765745501
              18495701454879288984856827726077713721403798879715
              38298203783031473527721580348144513491373226651381
              34829543829199918180278916522431027392251122869539
              40957953066405232632538044100059654939159879593635
              29746152185502371307642255121183693803580388584903
              41698116222072977186158236678424689157993532961922
              62467957194401269043877107275048102390895523597457
              23189706772547915061505504953922979530901129967519
              86188088225875314529584099251203829009407770775672
              11306739708304724483816533873502340845647058077308
              82959174767140363198008187129011875491310547126581
              97623331044818386269515456334926366572897563400500
              42846280183517070527831839425882145521227251250327
              55121603546981200581762165212827652751691296897789
              32238195734329339946437501907836945765883352399886
              75506164965184775180738168837861091527357929701337
              62177842752192623401942399639168044983993173312731
              32924185707147349566916674687634660915035914677504
              99518671430235219628894890102423325116913619626622
              73267460800591547471830798392868535206946944540724
              76841822524674417161514036427982273348055556214818
              97142617910342598647204516893989422179826088076852
              87783646182799346313767754307809363333018982642090
              10848802521674670883215120185883543223812876952786
              71329612474782464538636993009049310363619763878039
              62184073572399794223406235393808339651327408011116
              66627891981488087797941876876144230030984490851411
              60661826293682836764744779239180335110989069790714
              85786944089552990653640447425576083659976645795096
              66024396409905389607120198219976047599490197230297
              64913982680032973156037120041377903785566085089252
              16730939319872750275468906903707539413042652315011
              94809377245048795150954100921645863754710598436791
              78639167021187492431995700641917969777599028300699
              15368713711936614952811305876380278410754449733078
              40789923115535562561142322423255033685442488917353
              44889911501440648020369068063960672322193204149535
              41503128880339536053299340368006977710650566631954
              81234880673210146739058568557934581403627822703280
              82616570773948327592232845941706525094512325230608
              22918802058777319719839450180888072429661980811197
              77158542502016545090413245809786882778948721859617
              72107838435069186155435662884062257473692284509516
              20849603980134001723930671666823555245252804609722
              53503534226472524250874054075591789781264330331690
              ])
(apply str (take 10 (str (reduce + tableau))))
;5537376230



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 14
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn collatz
  [n]
  (loop [iter n, length 0]
    (cond
      (= 0 iter) length
      (= 1 iter) (inc length)
      (= 0 (mod iter 2)) (recur (/ iter 2) (inc length))
      :else (recur (+ 1 (* 3 iter)) (inc length))
      )
    )
  )

(time (loop [iter 0,
             maxl 0,
             maxliter 0]
        (if (= 1000000 iter)
          (list maxl maxliter)
          (let [collatzi (collatz iter)]
            (recur
              (inc iter)
              (max maxl collatzi)
              (if (> collatzi maxl) iter maxliter))
            )
          )
        )
  )
; 837799 (length 525)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 15
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cnk 
  "binomial coefficients (For any set containing n elements, the number of distinct k-element subsets of it that can be formed) "
  [n,k]
  (if (>= n k)
    (/ (factorial n) (* (factorial (- n k)) (factorial k)))
    1)
  )

;; Example on a 3x3 grid. Let's define a path as the following vector (a succession of direction r (right) or d (down)
; (r r r d d d)
; (r d r d r d)
; there must be as much r in the path as the horizontal dimension (3 in this case)
; there must be as much d in the path as the vertical dimension (3 in this case).
; we can see that this number is the number of distinct 3-element subsets of the set containing all the paths elements.
; namely this is (6
;                 3 )
;
(cnk 20 10)




;; code below is to bruteforce find all the elements...will not work with n > 10
;(defn nexx
;  [ijtuple]
;  (if (nil? ijtuple)
;    ()
;    (remove
;      (fn[x] (= x ijtuple))
;      (list
;        {:i (max 0 (dec (get ijtuple :i 0))) :j (:j ijtuple)}
;        {:i (:i ijtuple) :j (max 0 (dec (get ijtuple :j 0)))}
;        )
;      )
;    )
;  )
;(defn f [coll] (mapcat nexx coll))
;(def start {:i 4 :j 4})
;start
;(nexx start)
;(f (nexx start))
;(f (f (nexx start)))
;(f (f (f (nexx start))))
;;; the idea was to apply (f (f (f ...))) until {:i 0 :j 0} appears.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 16
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dble
  "For a number x stored as a list of its decimal digits, compute 2x (in the same format)"
  [coll]
  (loop [c (reverse coll), remain 0, out ()]
    (let [[aa & rr] c]
      (if (empty? c)
        (if (pos? remain) (conj out remain) out)
        (do
          (def aa2 (* 2 aa))
          (def outval (mod aa2 10))
          (def nextremain (quot aa2 10))
          (recur rr nextremain (conj out (+ (if (>= aa2 10) outval aa2) remain)))
          )
        )
      )
    )
  )
; sum of 2¹⁰⁰⁰ digits
(reduce + (first (drop 999 (take 1000 (iterate dble (list 2))))))
; 1366


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 17
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def dico {1 "one" 
           2 "two"
           3 "three"
           4 "four"
           5 "five"
           6 "six"
           7 "seven"
           8 "eight"
           9 "nine"
           10 "ten"
           11 "eleven"
           12 "twelve"
           13 "thirteen"
           14 "fourteen"
           15 "fifteen"
           16 "sixteen"
           17 "seventeen"
           18 "eighteen"
           19 "nineteen"
           20 "twenty"
           30 "thirty"
           40 "forty"
           50 "fifty"
           60 "sixty"
           70 "seventy"
           80 "eighty"
           90 "ninety"
           100 "hundred"
           1000 "thousand"
           })

(defn spellnumber
  [n]
  (def decade (* 10 (quot n 10)))
  (def cent (quot n 100))
  (def centremain (mod n 100))
  (cond 
    (<= n 20) (dico n)
    (and (< n 100) (< n (+ decade 10))) (str (dico decade) " " (dico (mod n decade)))
    (and (< n 1000)) (str (dico cent) " " (dico 100) (if (pos? centremain) 
                                                       (str " and " (spellnumber centremain)) 
                                                       "")
                       )
    (= n 1000) (str "one " (dico n))
    :else "N/A"
    )
  )
(defn remove-whitespace
  [s]
  (clojure.string/replace (clojure.string/trim s) #"\s{1,}" "")  
  )

(def out (loop [iter 1, outstr ""]
           (if (= 1001 iter)
             outstr
             (recur (inc iter) (str outstr (spellnumber iter)))    
             )
           )
  )
(count (remove-whitespace out))
; 21124



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

(def triangle [[75]
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



(def maptolengthtriangle (loop [iter 0, tria triangle]
                           (if (= 14 iter)
                             (do
                               (def newarray (mapbestrow tria iter))    
                               (assoc tria iter newarray)
                               )
                             (do
                               (def newarray (mapbestrow tria iter))    
                               (def newtria (assoc tria iter newarray))
                               (recur (inc iter) newtria)
                               )
                             )
                           )
  )

(apply max (last maptolengthtriangle))

; 1074


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problem 20
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn sdiv
  [n,d]
  (list (quot n d) (mod n d))
  )


(defn decimalsplit
  [n]
  (sdiv n 10)
  )

(defn generate0 [c] (map (fn [x] 0) (range 0 c)))

(defn conjj
  [coll,x]
  (if (zero? x)
    coll
    (conj coll x)
    )
  )
  
  
(defn aline
  [initx, initcoll]
  
  (loop [x initx,remain 0, coll initcoll,out ()]
    (if (empty? coll)
      (reverse (conjj out remain))
      (do
        (def y (first coll))
        (def xy (+ (* x y) remain))
        (let [[xydec xyunit] (decimalsplit xy)]
          (recur x xydec (rest coll) (conj out xyunit))
          )
        )
      )
    )
  )

(defn thelines
  [initxcoll, initcoll]
  (loop [iter 0, xcoll initxcoll, coll initcoll, out ()]
    (if (empty? xcoll)
      out
      (do
        (def fi (first xcoll))
        (def zeros (generate0 iter))
        (def newout (if (= 0 iter)
                      (conj out (aline fi coll))
                      (conj out (concat zeros (aline fi coll)))
;                      (conj out (aline fi coll))
                      ))
        (recur (inc iter) (rest xcoll) coll newout)
        
        )
      )
    )
  )


(thelines '(0 1) '(2 3))

(first ())

(defn sumcoll
  [initcoll1, initcoll2]
  (loop [coll1 initcoll1, coll2 initcoll2, remain 0, out ()]
    (cond
      (and (empty? coll1) (empty? coll2)) (conjj out remain)
      :else (do
              (def fcol1 ((fnil identity 0) (first coll1)))
              (def fcol2 ((fnil identity 0) (first coll2)))
              (def decsplit (decimalsplit (+ fcol1 fcol2)))
              (recur (rest coll1) (rest coll2) (first decsplit) (conj out (last decsplit)))
              )
      )
    )
  )
  

(rest (rest (rest '(0 1 2))))
(sumcoll '(9 1 2) '(1 8))

(+ 219 81)

