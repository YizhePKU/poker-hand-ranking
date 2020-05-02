(ns poker-hand-ranking.core
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

(def input
  [[nil nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil nil]
   [nil nil nil nil 1   nil nil nil nil]
   [nil nil nil nil nil 8   nil nil nil]
   [nil nil nil nil nil nil 9   nil nil]
   [nil nil nil nil nil nil nil 8   nil]
   [nil nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil nil]
   [5   nil 4   nil nil nil nil nil nil]])

(def . nil)

(def input2
  [[. . . 9 . . . . 3]
   [. . . 5 . 8 6 . .]
   [1 6 7 . . . 5 . .]
   [5 . . 1 . . 2 4 .]
   [. . . . . . . . .]
   [. 4 9 . . 2 . . 5]
   [. . 3 . . . 4 6 2]
   [. . 4 2 . 1 . . .]
   [6 . . . . 9 . . .]])

(defn max-element-default-zero
  [xs]
  (if (seq xs)
    (apply max xs)
    0))

(defn no-dups?
  [xs]
  (-> xs
      frequencies
      (dissoc nil)
      vals
      max-element-default-zero
      (<= 1)))

(defn extract-squares
  [sudoku]
  (for [i (range 3)
        j (range 3)]
    (for [di (range 3)
          dj (range 3)]
      (get-in sudoku [(+ (* 3 i) di) (+ (* 3 j) dj)]))))

(defn valid-sudoku?
  [sudoku]
  (every? no-dups? (concat sudoku
                           (apply map vector sudoku)
                           (extract-squares sudoku))))

(defn index-of
  [xs v]
  (let [i (.indexOf xs v)]
    (if (neg? i)
      nil
      i)))

(defn first-space
  [sudoku]
  (first (remove #(get-in sudoku %)
                 (for [i (range 9)
                       j (range 9)]
                   [i j]))))

(defn values
  []
  (repeatedly #(inc (rand-int 9))))

(defn rand-value [] (inc (rand-int 9)))

(require '[clojure.pprint :refer [pprint]])

(defn solve
  [sudoku]
  (pprint sudoku)
  (let [space (first-space sudoku)]
    (if-not space
      sudoku
      (some identity
            (for [v (range 1 10)
                  :let [filled (assoc-in sudoku space v)]
                  :when (valid-sudoku? filled)]
              (solve filled))))))

(defn rand-choice
  [xs]
  {:pre [(not (nil? xs))
         (seq xs)]}
  ((vec xs) (rand-int (count xs))))

(defn solve-rand
  [sudoku]
  (pprint sudoku)
  (let [space (first-space sudoku)]
    (if-not space
      sudoku
      (let [choices (for [v (range 1 10)
                          :let [filled (assoc-in sudoku space v)]
                          :when (valid-sudoku? filled)]
                      filled)]
        (if (seq choices)
          (solve-rand (rand-choice choices))
          nil)))))

(defn try-solve-rand
  [sudoku]
  (some identity
        (repeatedly #(solve-rand sudoku))))
