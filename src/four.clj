(ns four
  (:require [clojure.string :as str]
            [input-reader]))

(def directions [[0 1] [0 -1] [1 0] [-1 0] [-1 1] [1 1] [-1 -1] [1 -1]])

(defn char-at [grid x y] (nth (nth grid x) y))

(defn size [grid] [(count grid) (count (nth grid 0))])

(defn add-tuple-values [x y] [(+ (nth x 0) (nth y 0)) (+ (nth x 1) (nth y 1))])

(defn valid-pos? [n m x y] (and (< x n) (>= x 0) (< y m) (>= y 0)))

(defn xmas-count [grid x y dir prev]
      (let [[n m] (size grid)
            [i j] (add-tuple-values [x y] dir)]
        (cond
          (not (valid-pos? n m x y)) 0

          (and (= (char-at grid x y) "S") (= prev "A")) 1

          (not (valid-pos? n m i j)) 0

          (and (= (char-at grid x y) "X") (nil? prev) (= (char-at grid i j) "M"))
          (xmas-count grid i j dir "X")

          (and (= (char-at grid x y) "M") (= prev "X") (= (char-at grid i j) "A"))
          (xmas-count grid i j dir "M")

          (and (= (char-at grid x y) "A") (= prev "M") (= (char-at grid i j) "S"))
          (xmas-count grid i j dir "A")

          :else 0)))

(defn get-indices [grid]
      (let [[x y] (size grid)]
        (apply concat (map (fn [j] (map #(vector % j) (range x))) (range y)))))

(defn get-grid [input-path]
      (map #(str/split % #"") (input-reader/read-lines input-path)))

(defn solve-one [input-path]
      (let [grid (get-grid input-path)]
        (->> (get-indices grid)
             (filter #(= (char-at grid (nth % 0) (nth % 1)) "X"))
             (map (fn [index] (map #(vector (nth index 0) (nth index 1) %) directions)))
             (apply concat)
             (map #(xmas-count grid (nth % 0) (nth % 1) (nth % 2) nil))
             (reduce +))))

(four/solve-one "./problem-input/four/example_1.txt")
(four/solve-one "./problem-input/four/example_2.txt")
(four/solve-one "./problem-input/four/example_3.txt")
(four/solve-one "./problem-input/four/problem.txt")

