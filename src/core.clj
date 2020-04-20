(ns clojure-hello.hands
  (:require [clojure.math.combinatorics :as math]))

(defn n-of
  "Check if there is a n-combination of xs that satisfies pred.
  Returns the first satisfied combination, or nil if none is found."
  [n xs pred]
  (some #(when (pred %) %)
        (math/combinations xs n)))

(def four-of #(fn [xs] (n-of 4 xs %)))
(def three-of #(fn [xs] (n-of 3 xs %)))
(def two-of #(fn [xs] (n-of 2 xs %)))

(defn combine-preds
  "Returns a predicate that sequentially check if input satisfy all given predicates.
  Satisfied elements are removed from input and not available for later predicates.
  The predicate returns all unused inputs, or nil if check failed at some point.
  Note that the algorithm does not retry even if multiple combinations can satisfy
  a predicate, so ordering of predicates may change the result."
  [& preds]
  (fn [xs]
    (reduce (fn [xs pred]
              (if-let [comb (pred xs)]
                (remove (set comb) xs)
                (reduced nil)))
            xs
            preds)))

(defn same-suit
  "Returns true if all cards have the same suit."
  [cards]
  (apply = (map second cards)))

(defn same-rank
  "Returns true if all cards have the same rank."
  [cards]
  (apply = (map first cards)))

(defn has
  "Returns a predicate that check if some of the cards is the given rank."
  [rank]
  (fn [cards]
    (some (partial = rank)
          (map first cards))))

(defn consecutive
  "Returns true if ranks of the cards are consecutive.
  jack = 11, queen = 12, king = 13.
  Ace = 1 or 14 depending on the situation."
  [cards]
  (let [m {:ace (if ((has 2) cards) 1 14)
           :jack 11
           :queen 12
           :king 13}
        ranks (map first cards)
        numbers (map #(m % %) ranks)
        minimum (apply min numbers)
        maximum (apply max numbers)]
    (= (sort numbers)
       (range minimum (inc maximum)))))



(def royal-flush?
  (every-pred same-suit
              (has :ace)
              (has :king)
              (has :queen)
              (has :jack)
              (has 10)))

(def straight-flush?
  (every-pred consecutive
              same-suit))

(def four-of-a-kind?
  (four-of same-rank))

(def full-house?
  (combine-preds (three-of same-rank)
                 (two-of same-rank)))

(def flush?
  same-suit)

(def straight?
  consecutive)

(def three-of-a-kind?
  (three-of same-rank))

(def two-pairs?
  (combine-preds (two-of same-rank)
                 (two-of same-rank)))

(def pair?
  (two-of same-rank))

(defn score
  "Return the best scoring for a given poker hand of 5 cards."
  [cards]
  (cond (royal-flush? cards) "Royal Flush"
        (straight-flush? cards) "Straight Flush"
        (four-of-a-kind? cards) "Four of a kind"
        (full-house? cards) "Full house"
        (flush? cards) "Flush"
        (straight? cards) "Straight"
        (three-of-a-kind? cards) "Three of a kind"
        (two-pairs? cards) "Two pair"
        (pair? cards) "Pair"
        :else "High card"))
