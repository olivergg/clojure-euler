(ns olivergg.euler.sandbox
  (:require [olivergg.euler.common :refer :all])
  )




(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))



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

(defn phonepermut
  [initl]
  ;(def initl '(1 1 2 3))
  (def m {1 ["A" "B" "C"]
          2 ["D" "E" "F"]
          3 ["G" "H" "I"]
          4 ["J" "K" "L"]
          5 ["M" "N" "O"]
          6 ["P" "Q" "R"]
          7 ["S" "T" "U"]
          8 ["V" "W" "X"]
          9 ["Y" "Z" ""]})
  (def n (count initl))
  (def mappedl (map (fn [x] (m x)) initl))
  ; 3^n possibilités
  (def poss (apply-pow 3 n))


  (defn tobase3
    [initn]
    (loop [n initn, out ()]
      (if (= n 0)
        out
        (recur (quot n 3) (conj out (mod n 3)))
        )
      )
    )

  (defn paddleadingzero [i, size]
    (def listrep (tobase3 i))
    (def isize (count listrep))
    (def diffsize (max 0 (- size isize)))
    (if (> diffsize 0)
      (apply conj listrep (take diffsize (repeat 0)))
      listrep
      )
    )

  (defn getfrommappedl
    [initcollofvec, initlistofindex]
    (loop [collofvec initcollofvec, listofindex initlistofindex, out []]
      (if (empty? collofvec)
        out
        (do
          (def toadd ((first collofvec) (first listofindex)))
          (recur (rest collofvec) (rest listofindex) (conj out toadd))
          )
        )
      )
    )


  (loop [j 0]
    (if (= j poss)
      (println "stop")
      (do
        (println (getfrommappedl mappedl (tofun j, n)))
        (recur (inc j))
        )
      )
    )

  )

(phonepermut '(1 1 2))



(def initl '(1 1 2))
(def m {1 ["A" "B" "C"]
        2 ["D" "E" "F"]
        3 ["G" "H" "I"]
        4 ["J" "K" "L"]
        5 ["M" "N" "O"]
        6 ["P" "Q" "R"]
        7 ["S" "T" "U"]
        8 ["V" "W" "X"]
        9 ["Y" "Z" ""]})
(def n (count initl))
(def mappedl (map (fn [x] (m x)) initl))

;
;(defn f
;  [alist]
;  (g (first alist) (rest alist) "")
;  )
;
;(defn g
;  [anint, alist, astring]
;  (def lettergroup (m anint))
;  (for [i (range 0 3)]
;    (do
;      (def conc (str astring (lettergroup i)))
;      (if (empty? alist)
;        conc
;        (g (first alist) (rest alist) conc)
;        )
;      )
;    )
;  )
;
;(f initl)




(defn test3
  [alist, currentword]
  (if (empty? alist)
    (println "done")
    (do
      (def firstelem (first alist))
      (def lettergroup (m firstelem))
      (for [i (range 0 3)]
        (if (= (count alist) (count currentword))
          (do
            (println currentword)
            (test3 (rest alist) "")
            )
          (do
            (def conc (str currentword (lettergroup i)))
            (test3 alist conc)
            )
          )
        )
      )
    )
  )

(test3 initl "")