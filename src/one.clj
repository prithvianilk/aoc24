(ns one
  (:require [clojure.string :as str]))

(defn unzip [a b]
      (vector
        (conj (nth a 0) (Integer/parseInt (nth b 0)))
        (conj (nth a 1) (Integer/parseInt (nth b 1)))))

(defn read-input
      [input-path]
      (reduce unzip [[] []] (map #(str/split % #"   ") (str/split-lines (slurp input-path)))))

(defn distance-between-lists
      [x y]
      (reduce + (map (comp #(abs (- (nth % 0) (nth % 1))) vector)
                     (sort x) (sort y))))

(defn similarity-score
      [x y]
      (let [y-frequencies (frequencies y)]
        (reduce + (map #(* % (or (y-frequencies %) 0)) x))
        ))

(defn solve-first [input-path]
      (let
        [[x y] (read-input input-path)]
        (println (distance-between-lists x y))))

(defn solve-second [input-path]
      (let
        [[x y] (read-input input-path)]
        (println (similarity-score x y))))

(solve-first "./problem-input/one/example_1.txt")
(solve-first "./problem-input/one/problem.txt")
(solve-second "./problem-input/one/example_1.txt")
(solve-second "./problem-input/one/problem.txt")
