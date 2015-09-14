(ns olivergg.euler.problem_54
  (:require [olivergg.euler.common :refer :all]
            [clojure.set :refer :all]
            )
  )

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

(defn c[strcard] (str-to-card strcard))



(let [card (str-to-card "6C")
      v (:value card)
      s (:suit card)
      ]
  (assert (= 6 v))
  (assert (= "C" s))
  )



(defn cards>=
  "Check if card1 value is greater than the card2 one"
  [card1 card2]
  (>= (cardscomp card1 card2) 0)
  )


(defn cardscomp
  [card1 card2]
  (let [card1val (:value card1)
        card2val (:value card2)
        ]

    (Integer/signum (- card1val card2val))
    )
  )




(assert (cards>= (c "JC") (c "TC")))


(defn sortcards
  "Sort a collection of card by increasing value"
  [coll]
  (sort-by #(:value %) coll)
  )



(sortcards [(c "JS")  (c "QS") (c "AS") (c "KS") (c "TS")])

(defn is-flush[coll]
  (->> coll
       (map #(:suit %))
       (into #{})
       (count)
       (= 1)
       )
  )



(assert (is-flush [(c "6D") (c "3D") (c "7D") (c "TD") (c "QD")]))
(assert (not (is-flush [(c "6D") (c "3D") (c "7D") (c "TD") (c "QS")])))

(defn is-straight[coll]
  (let [
        sortedcoll (sortcards coll)
        thevals (map #(:value (str-to-card %)) sortedcoll)
        firstv (first thevals)
        thealteredvals (map #(- % firstv) thevals)
        ]
    (= '(0 1 2 3 4) thealteredvals)
    )
  )



(assert (is-straight '( "JS" "QS" "TS" "KS" "AS")))
(assert (not (is-straight '( "8S" "QS" "TS" "KS" "AS"))))


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
             (filter #(= 10 (:value (str-to-card %))))
             (count)
             )
      )
   )
  )

(assert (is-royal-flush '( "JS" "QS" "TS" "KS" "AS")))
(assert (not (is-royal-flush '( "JS" "QS" "TS" "KS" "AD"))))
(assert (not (is-royal-flush '( "JS" "QS" "9S" "KS" "AS"))))
(assert (not (is-royal-flush '( "1S" "QS" "TS" "KS" "AS"))))




(defn is-n-of-a-kind[n, hand]
  (let [
        ;;sortedcoll (sortcards hand)
        thevals (map #(:value (str-to-card %)) hand)
        groupedbyval (group-by identity thevals)
        countval (into #{} (map #(count %) (vals groupedbyval)))
        ]
    (contains? countval n)
    ))

(defn is-four-of-a-kind[hand] (is-n-of-a-kind 4 hand))





(assert (is-four-of-a-kind '( "JS" "JD" "JH" "JC" "AS")))
(assert (not (is-four-of-a-kind '( "JS" "JD" "JH" "TC" "AS"))))



(defn is-three-of-a-kind[hand] (is-n-of-a-kind 3 hand))

(assert (is-three-of-a-kind '("5S" "5D" "5H" "QS" "KD")))
(assert (not (is-three-of-a-kind '("5S" "5D" "6H" "QS" "KD"))))



(defn is-pair[hand] (is-n-of-a-kind 2 hand))

(assert (is-pair '("5S" "5D" "4H" "QS" "KD")))





(defn is-full-house[hand]
  (and
   (is-three-of-a-kind hand)
   (is-pair hand)
   )
  )

(assert (is-full-house '("5S" "5D" "5H" "KS" "KD")))



(defn is-two-pairs[hand]
  (let [thevals (map #(:value (str-to-card %)) hand)
        groupedbyval (group-by identity thevals)
        t (map #(count %) (vals groupedbyval))
        [a b c] (sort t)
        ]
    (= b c 2)
    )
  )

(assert (is-two-pairs '("7H" "7S" "2H" "8D" "8C")))
(assert (not (is-two-pairs '("7H" "7S" "2H" "8D" "6C"))))



(defn get-highest-value-card [hand]
  (last (sortcards hand))
  )

(defn get-highest-card-value [hand]
  (:value (str-to-card (get-highest-value-card hand)))
  )

(assert (= "8D" (get-highest-value-card ["7H" "7S" "2H" "8D" "6C"])))
(assert (= "KS" (get-highest-value-card '("7H" "KS" "2H" "8D" "6C"))))
(assert (= "AH" (get-highest-value-card '("7H" "KS" "AH" "8D" "6C"))))




(defn strhand-to-hand[astr]
  (clojure.string/split astr #" ")
  )

(assert (= ["5H" "5C" "6S" "7S" "KD"] (strhand-to-hand "5H 5C 6S 7S KD")))


(defn linetohands[aline]
  (let [allhands (strhand-to-hand aline)
        c (count allhands)
        half (/ c 2)
        h1 (subvec allhands 0 half)
        h2 (subvec allhands half c)
        ]
    [h1 h2]
    )
  )

(assert (= [["8C" "TS" "KC" "9H" "4S"] ["7D" "2S" "5D" "3S" "AC"]] (linetohands "8C TS KC 9H 4S 7D 2S 5D 3S AC")))



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


(assert (= 2 (get-rank (strhand-to-hand "2C 3S 8S 8D TD"))))
(assert (= 7 (get-rank (strhand-to-hand "3C 3D 3S 9S 9D"))))



(defn tie-break-on-highest-card-value[hand1 hand2]
  (let [val1 (get-highest-card-value hand1)
        val2 (get-highest-card-value hand2)
        ]
    (if (> val1 val2) 1 2)
    )
  )


(defn compare-hands[hand1 hand2]
  (let [sortedhand1 (reverse (sortcards hand1))
        sortedhand2 (reverse (sortcards hand2))
        first1 (first sortedhand1)
        first2 (first sortedhand2)
        cmp (if (nil? first1) 0 (cardscomp first1 first2))
        ]
    (cond
     (and (empty? sortedhand1) (empty? sortedhand2)) 0
     (= 0 cmp) (recur (rest sortedhand1) (rest sortedhand2))
     (= 1 cmp) 1
     (= -1 cmp) 2
     )
    )
  )


(assert (= 0 (compare-hands ["AD" "TS"] ["AH" "TS"])))
(assert (= 2 (compare-hands ["AD" "TS"] ["AH" "QS"])))
(assert (= 1 (compare-hands ["AD" "TS"] ["AH" "3S"])))


(defn tie-break-on-pair[hand1 hand2]
  (let [
        thevals1 (map #(:value (str-to-card %)) (sortcards hand1))
        freq1 (group-by val (frequencies thevals1))
        pair1 (first (freq1 2))
        pairval1 (first pair1)

        thevals2 (map #(:value (str-to-card %)) (sortcards hand2))
        freq2 (group-by val (frequencies thevals2))
        pair2 (first (freq2 2))
        pairval2 (first pair2)
        ;;         pairA (first pairs)
        ;;         pairB (second pairs)

        ]
    1
    ;pairval1
    (cond
     (> pairval1 pairval2) 1
     (< pairval1 pairval2) 2
     (= pairval1 pairval2) 0;;; TODO : tie break sur card value sur le autres cartes

     ;;      (cardscomp


     )
    )
  )

(dissoc {3 4 2 1} 3)



(defn tie-break[rank hand1 hand2]
  (cond
   (= 1 rank) (tie-break-on-highest-card-value hand1 hand2)
   (= 2 rank) (tie-break-on-pair hand1 hand2)
   ;(= 3 rank) (tie-break-on-pairs hand1 hand2)
   :else 0
   )
  )

(defn get-winner[twohands]
  (let [[h1 h2] twohands
        rank1 (get-rank h1)
        rank2 (get-rank h2)
        ]
    ;[rank1 rank2]
    (cond
     (> rank1 rank2) 1
     (> rank2 rank1) 2
     (= rank1 rank2) (tie-break rank1 h1 h2)
     )
    )
  )



;(get-winner [["8C" "TS" "AC" "5H" "4S"] ["7D" "2S" "5D" "3S" "TC"]])
(get-winner [["6C" "TS" "TC" "8H" "4S"] ["TD" "7S" "5D" "3S" "TC"]])
;(get-winner [["8C" "TS" "TC" "8H" "4S"] ["7D" "7S" "5D" "TS" "TC"]])















