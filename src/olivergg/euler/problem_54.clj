(ns olivergg.euler.problem_54
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            [taoensso.timbre :as timbre]
            [clojure.java.io :refer :all :as io]
            [clojure.string :as str]
            )
  )


(timbre/refer-timbre)


;; In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:

;; High Card: Highest value card.
;; One Pair: Two cards of the same value.
;; Two Pairs: Two different pairs.
;; Three of a Kind: Three cards of the same value.
;; Straight: All cards are consecutive values.
;; Flush: All cards of the same suit.
;; Full House: Three of a kind and a pair.
;; Four of a Kind: Four cards of the same value.
;; Straight Flush: All cards are consecutive values of same suit.
;; Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
;; The cards are valued in the order:
;; 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

;; If two players have the same ranked hands then the rank made up of the highest value wins; for example, a pair of eights beats a pair of fives (see example 1 below). But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared (see example 4 below); if the highest cards tie then the next highest cards are compared, and so on.

;; Consider the following five hands dealt to two players:

;; Hand	 	Player 1	 	Player 2	 	Winner
;; 1	 	5H 5C 6S 7S KD
;; Pair of Fives
;;  	2C 3S 8S 8D TD
;; Pair of Eights
;;  	Player 2
;; 2	 	5D 8C 9S JS AC
;; Highest card Ace
;;  	2C 5C 7D 8S QH
;; Highest card Queen
;;  	Player 1
;; 3	 	2D 9C AS AH AC
;; Three Aces
;;  	3D 6D 7D TD QD
;; Flush with Diamonds
;;  	Player 2
;; 4	 	4D 6S 9H QH QC
;; Pair of Queens
;; Highest card Nine
;;  	3D 6D 7H QD QS
;; Pair of Queens
;; Highest card Seven
;;  	Player 1
;; 5	 	2H 2D 4C 4D 4S
;; Full House
;; With Three Fours
;;  	3C 3D 3S 9S 9D
;; Full House
;; with Three Threes
;;  	Player 1
;; The file, poker.txt, contains one-thousand random hands dealt to two players. Each line of the file contains ten cards (separated by a single space): the first five are Player 1's cards and the last five are Player 2's cards. You can assume that all hands are valid (no invalid characters or repeated cards), each player's hand is in no specific order, and in each hand there is a clear winner.

;; How many hands does Player 1 win?



(def cardvals {"1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7
               "8" 8 "9" 9 "T" 10 "J" 11 "Q" 12 "K" 13 "A" 14 })

(def suitvals (list "S" "H" "D" "C"))

(def cardpattern #"([0-9TJQKA])([SHDC])")



(defmulti card (fn ([v s] [(class v) (class s)])))
(defmethod card [Long String]
  [v s] {:value v :suit s})
(defmethod card [String String]
  [v s] {:value (cardvals v) :suit s})



(card "4" "H")


(defn str-to-card
  "Transform a string representation of a card into a map that contains the value and the suit"
  [strcard]
  (let [groups (re-matches cardpattern strcard)
        v (cardvals (second groups))
        suit (get groups 2)
        ]
    (card v suit)
    )
  )


(defn strhand-to-hand[astr]
  (clojure.string/split astr #" ")
  )

(assert (= ["5H" "5C" "6S" "7S" "KD"] (strhand-to-hand "5H 5C 6S 7S KD")))


(defn c[strcard] (str-to-card strcard))


(defn cs
  "Convert a string that represents a list of cards in their string representation into a list of {:value X :suit Y}"
  [strhand]
  (map #(c %) (strhand-to-hand strhand))
  )


(let [card (c "6C")
      v (:value card)
      s (:suit card)
      ]
  (assert (= 6 v))
  (assert (= "C" s))
  )

(defn cardscomp
  [card1 card2]
  (let [card1val (:value card1)
        card2val (:value card2)
        ]

    (Integer/signum (- card1val card2val))
    )
  )


(defn cards>=
  "Check if card1 value is greater than the card2 one"
  [card1 card2]
  (>= (cardscomp card1 card2) 0)
  )






(assert (cards>= (c "JC") (c "TC")))


(defn sortcards
  "Sort a collection of card by increasing value"
  [coll]
  (sort-by #(:value %) coll)
  )



(sortcards (cs "JS QS AS KS TS"))
(defn is-flush[coll]
  (->> coll
       (map #(:suit %))
       (into #{})
       (count)
       (= 1)
       )
  )



(assert (is-flush (cs "6D 3D 7D TD QD")))
(assert (not (is-flush (cs "6D 3D 7D TD QS"))))

(defn is-straight[coll]
  (let [
        sortedcoll (sortcards coll)
        thevals (map #(:value %) sortedcoll)
        firstv (first thevals)
        thealteredvals (map #(- % firstv) thevals)
        ]
    (or (= '(0 1 2 3 4) thealteredvals)
        (= '(0 1 2 3 12) thealteredvals) ;; Baby Straight
        )
    )
  )



(cs "JS QS TS KS AS")

(assert (is-straight (cs "JS QS TS KS AS")))
(assert (is-straight (cs "AS 2S 3S 4D 5S")))
(assert (not (is-straight (cs "8S QS TS KS AS"))))
(assert (not (is-straight (cs "AS KS 3S 4D 5S"))))



(defn is-straight-flush[coll]
  (and
   (is-flush coll)
   (is-straight coll)
   )
  )


(defn is-royal-flush[hand]
  (and
   (is-straight-flush hand)
   (= 1 (->> hand
             (filter #(= 10 (:value %)))
             (count)
             )
      )
   )
  )

(assert (is-royal-flush (cs "JS QS TS KS AS")))
(assert (not (is-royal-flush (cs "JS QS TS KS AD"))))
(assert (not (is-royal-flush (cs "JS QS 9S KS AS"))))
(assert (not (is-royal-flush (cs "1S QS TS KS AS"))))




(defn is-n-of-a-kind[n, hand]
  (let [
        thevals (map #(:value %) hand)
        groupedbyval (group-by identity thevals)
        countval (into #{} (map #(count %) (vals groupedbyval)))
        ]
    (contains? countval n)
    ))

(defn is-four-of-a-kind[hand] (is-n-of-a-kind 4 hand))





(assert (is-four-of-a-kind (cs "JS JD JH JC AS")))
(assert (not (is-four-of-a-kind (cs "JS JD JH TC AS"))))



(defn is-three-of-a-kind[hand] (is-n-of-a-kind 3 hand))

(assert (is-three-of-a-kind (cs "5S 5D 5H QS KD")))
(assert (not (is-three-of-a-kind (cs "5S 5D 6H QS KD"))))



(defn is-pair[hand] (is-n-of-a-kind 2 hand))

(assert (is-pair (cs "5S 5D 4H QS KD")))





(defn is-full-house[hand]
  (and
   (is-three-of-a-kind hand)
   (is-pair hand)
   )
  )

(assert (is-full-house (cs "5S 5D 5H KS KD")))



(defn is-two-pairs[hand]
  (let [thevals (map #(:value %) hand)
        groupedbyval (group-by identity thevals)
        t (map #(count %) (vals groupedbyval))
        [a b c] (sort t)
        ]
    (= b c 2)
    )
  )

(assert (is-two-pairs (cs "7H 7S 2H 8D 8C")))
(assert (not (is-two-pairs (cs "7H 7S 2H 8D 6C"))))



(defn get-highest-value-card [hand]
  (last (sortcards hand))
  )
(get-highest-value-card (cs "7H 7S 2H 8D 6C"))

(defn get-highest-card-value [hand]
  (:value (get-highest-value-card hand))
  )

(assert (= (c "8D") (get-highest-value-card (cs "7H 7S 2H 8D 6C"))))
(assert (= (c "KS") (get-highest-value-card (cs "7H KS 2H 8D 6C"))))
(assert (= (c "AH") (get-highest-value-card (cs "7H KS AH 8D 6C"))))





(defn linetohands[aline]
  (let [allhands (into [] (cs aline))
        c (count allhands)
        half (/ c 2)
        h1 (subvec allhands 0 half)
        h2 (subvec allhands half c)
        ]
    [(sortcards h1) (sortcards h2)]
    )
  )


(let [ltoh (linetohands "8C TS KC 9H 4S 7D 2S 5D 3S AC")
      h1 (first ltoh)
      h2 (last ltoh)
      firstofh1 (first h1)
      firstofh2 (first h2)
      ]
  (assert (= 4 (:value firstofh1)))
  (assert (= 2 (:value firstofh2)))
  )







(defn get-pairval-and-remains
  "Get the value of the unique pair of the hand and the remaining hand"
  [hand]
  (let [
        groupedby (group-by #(:value %) hand)
        pairs (filter #(= 2 (count (val %))) groupedby)
        remains (flatten (vals (filter #(not= 2 (count (val %))) groupedby)))
        pairval (key (first pairs))
        ]
    {:cmpval pairval :remains remains}
    )
  )

(defn get-highestpairval-and-remains
  "Get the value of the highest pair of the hand (assuming there are two pairs in it) and the remaining hand"
  [hand]
  (let [
        groupedby (group-by #(:value %) hand)
        sortedpairs (sort-by #(key %) (filter #(= 2 (count (val %))) groupedby))
        pairval (key  (last sortedpairs))
        remains (flatten
                 (vals
                  (conj (filter #(not= 2 (count (val %)))
                                groupedby
                                )
                        (first sortedpairs)
                        )
                  )
                 )
        ]
    {:cmpval pairval :remains remains}
    )
  )

(defn get-threeofkindval-and-remains
  [hand]

  (let [
        groupedby (group-by #(:value %) hand)
        triplet (filter #(= 3 (count (val %))) groupedby)
        tripletval (key (first triplet))
        remains (flatten(vals(filter #(not= 3 (count (val %)))groupedby)))
        ]
    {:cmpval tripletval :remains remains}
    )

  )


(defn get-highest-value-and-remains[hand]
  (let [sortedcards (sortcards hand)
        highestval (last sortedcards)
        cmp (if (nil? highestval) 0 (:value highestval))
        ]
    {:cmpval cmp :remains (remove #(= % highestval) hand)}
    )
  )

(defn get-fourofkind-value-and-remains[hand]
  (let [
        groupedby (group-by #(:value %) hand)
        quartet (filter #(= 4 (count (val %))) groupedby)
        quartetval (key (first quartet))
        remains (flatten(vals(filter #(not= 4 (count (val %)))groupedby)))
        ]
    {:cmpval quartetval :remains remains}
    )
  )





(defn tie-break-on-functions[hand1 hand2 fvalremains ftiebreak]
  (let [temp1 (profiling/p :fvalremains (fvalremains hand1))
        remains1 (:remains temp1)
        cmpval1 (:cmpval temp1)

        temp2 (profiling/p :fvalremains (fvalremains hand2))
        remains2 (:remains temp2)
        cmpval2 (:cmpval temp2)

        ]
    ;(debug cmpval1 cmpval2)
    (cond
     (> cmpval1 cmpval2) 1
     (< cmpval1 cmpval2) 2
     (and (seq remains1) (seq remains2) (= cmpval1 cmpval2)) (ftiebreak remains1 remains2)
     :else 0
     )
    )
  )



(defn tie-break-on-highest-card-value
  [hand1 hand2]
  (tie-break-on-functions hand1 hand2 get-highest-value-and-remains tie-break-on-highest-card-value)
  )
(assert (= 0 (tie-break-on-highest-card-value (cs "AD TS") (cs "AH TS"))))
(assert (= 2 (tie-break-on-highest-card-value (cs "AD TS") (cs "AH QS"))))
(assert (= 1 (tie-break-on-highest-card-value (cs "AD TS") (cs "AH 3S"))))


(defn tie-break-on-pair
  [hand1 hand2]
  (tie-break-on-functions hand1 hand2 get-pairval-and-remains tie-break-on-highest-card-value)
  )


(defn tie-break-on-two-pairs
  [hand1 hand2]
  (tie-break-on-functions hand1 hand2 get-highestpairval-and-remains tie-break-on-pair)
  )


(defn tie-break-on-three-of-a-kind
  [hand1 hand2]
  (tie-break-on-functions hand1 hand2 get-threeofkindval-and-remains tie-break-on-highest-card-value)
  )

(defn tie-break-on-full-house
  [hand1 hand2]
  (tie-break-on-functions hand1 hand2 get-threeofkindval-and-remains tie-break-on-pair)
  )


(defn tie-break-on-four-of-a-kind
  [hand1 hand2]
  (tie-break-on-functions hand1 hand2 get-fourofkind-value-and-remains tie-break-on-highest-card-value)
  )

(defn get-rank[hand]
  (cond
   (is-royal-flush hand) 10
   (is-straight-flush hand) 9
   (is-four-of-a-kind hand) 8
   (is-full-house hand) 7
   (is-flush hand) 6
   (is-straight hand) 5
   (is-three-of-a-kind hand) 4
   (is-two-pairs hand) 3
   (is-pair hand) 2
   :else 1
   )
  )


(assert (= 2 (get-rank (cs "2C 3S 8S 8D TD"))))
(assert (= 7 (get-rank (cs "3C 3D 3S 9S 9D"))))




(defn tie-break[rank hand1 hand2]
  (cond
   (= 1 rank) (tie-break-on-highest-card-value hand1 hand2)
   (= 2 rank) (tie-break-on-pair hand1 hand2)
   (= 3 rank) (tie-break-on-two-pairs hand1 hand2)
   (= 4 rank) (tie-break-on-three-of-a-kind hand1 hand2)
   (= 5 rank) (tie-break-on-highest-card-value hand1 hand2)
   (= 6 rank) (tie-break-on-highest-card-value hand1 hand2)
   (= 7 rank) (tie-break-on-full-house hand1 hand2)
   (= 8 rank) (tie-break-on-four-of-a-kind hand1 hand2)
   (= 9 rank) (tie-break-on-highest-card-value hand1 hand2)
   (= 10 rank) 0
   :else 0
   )
  )

(defn get-winner[twohands]
  (let [[h1 h2] twohands
        rank1 (get-rank h1)
        rank2 (get-rank h2)
        ]
    (cond
     (> rank1 rank2) 1
     (> rank2 rank1) 2
     (= rank1 rank2) (tie-break rank1 h1 h2)
     )
    )
  )




(assert (= 2 (get-winner [(cs "AC TS 2C 5H 4S") (cs "7D AS 5D 3S TC")])))

(assert (= 1 (get-winner [(cs "6C TS TC 8H 4S") (cs "TD 7S 5D 3S TC")])))

(assert (= 2 (get-winner [(cs "5H 5C 6S 7S KD") (cs "2C 3S 8S 8D TD")])))

(assert (= 1 (get-winner [(cs "5D 8C 9S JS AC") (cs "2C 5C 7D 8S QH")])))

(assert (= 2 (get-winner [(cs "TS TH 9D 9S 3H") (cs "TS TH 9D 9S 4H")])))

(assert (= 1 (get-winner [(cs "TS TH 9D 9S 3H") (cs "TS TH 8D 8S 4H")])))

(assert (= 2 (get-winner [(cs "TS TH 9D 9S 3H") (cs "QS QH 8D 8S 4H")])))

(assert (= 1 (get-winner [(cs "TS TD TH 3D 2D") (cs "9S 9D 9H 2D 5S")])))

(assert (= 2 (get-winner [(cs "TS TD TH 3D 2D") (cs "TS TD TH 2D 4S")])))

(assert (= 1 (get-winner [(cs "TS TD TH 3D 5D") (cs "TS TD TH 3D 4S")])))

(assert (= 1 (get-winner [(cs "AS KD QH JH TH") (cs "KD QH JH TH 9D")])))

(assert (= 2 (get-winner [(cs "7S 6D 5H 4H 3H") (cs "KD QH JH TH 9D")])))

(assert (= 2 (get-winner [(cs "7S 7D 7H 4H 4D") (cs "KD KH KS 8H 8D")])))

(assert (= 2 (get-winner [(cs "KS KD KH 4H 4D") (cs "KD KH KS 8H 8D")])))

(assert (= 1 (get-winner [(cs "KS KD KH 4H 4D") (cs "1D 1H 1S 8H 8D")])))

(assert (= 1 (get-winner [(cs "KS KD KH KC 3D") (cs "QS QD QH QC 9S")])))

(assert (= 2 (get-winner [(cs "KS KD KH KC 3D") (cs "KS KD KH KC 9S")])))

(assert (= 2 (get-winner [(cs "KS KD KH KC 3D") (cs "AS AD AH AC 9S")])))



(with-open [rdr (reader (io/resource "p054_poker.txt"))]
  (count (filter #(= 1 %)
                 (for [line (line-seq rdr)]
                   (-> line
                        (linetohands)
                        (get-winner)
                        )
                   )
                 )
         )
  )

; 376
