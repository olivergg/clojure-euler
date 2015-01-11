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
