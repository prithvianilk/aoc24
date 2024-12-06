(ns six
  (:require [clojure.string :as str]
            [input-reader]))

(defn char-at [grid x y] (nth (nth grid x) y))

(defn size [grid] [(count grid) (count (first grid))])

(defn valid-pos? [n m x y] (and (< x n) (>= x 0) (< y m) (>= y 0)))

(defn valid-step? [grid x y]
      (let [[n m] (size grid)]
        (and (valid-pos? n m x y) (not (= (char-at grid x y) "#")))
        ))

(defn guard-position [grid i j]
      (let [[n m] (size grid)]
        (cond
          (not (valid-pos? n m i j)) nil
          (= (char-at grid i j) "^") [i j]
          (< (inc j) m) (guard-position grid i (inc j))
          :else (guard-position grid (inc i) 0)
          )))

(defn distinct-positions
      ([grid]
       (distinct-positions grid (guard-position grid 0 0) "UP" #{}))

      ([grid pos dir positions]
       (let [[i j] pos]
         (case dir
           "UP"
           (cond
             (valid-step? grid (dec i) j)
             (distinct-positions grid [(dec i) j] dir (conj positions [(dec i) j]))

             (valid-step? grid i (inc j))
             (distinct-positions grid [i (inc j)] "RIGHT" (conj positions [i (inc j)]))

             :else positions)

           "DOWN"
           (cond
             (valid-step? grid (inc i) j)
             (distinct-positions grid [(inc i) j] dir (conj positions [(inc i) j]))

             (valid-step? grid i (dec j))
             (distinct-positions grid [i (dec j)] "LEFT" (conj positions [i (dec j)]))

             :else positions)

           "LEFT"
           (cond
             (valid-step? grid i (dec j))
             (distinct-positions grid [i (dec j)] dir (conj positions [i (dec j)]))

             (valid-step? grid (dec i) j)
             (distinct-positions grid [(dec i) j] "UP" (conj positions [(dec i) j]))

             :else positions)

           "RIGHT"
           (cond
             (valid-step? grid i (inc j))
             (distinct-positions grid [i (inc j)] dir (conj positions [i (inc j)]))

             (valid-step? grid (inc i) j)
             (distinct-positions grid [(inc i) j] "DOWN" (conj positions [(inc i) j]))

             :else positions)
           )))
      )

(defn solve-one [input-path]
      (->> (input-reader/read-lines input-path)
           (map #(str/split % #""))
           (distinct-positions)
           (count)
           (println)))