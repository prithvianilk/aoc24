(ns two
  (:require [clojure.string :as str]
            [input-reader]))

(defn diff-in-levels-at-least-1-and-at-most-3?
      [levels]
      (first
        (reduce
          (fn [a b]
            (if (false? (first a))
              [false b]
              (let [diff (abs (- (second a) b))]
                [(and (>= diff 1) (<= diff 3)) b]
                )))
          [true (inc (first levels))] levels)))

(defn report-safe? [levels]
      (and
        (or (= (sort levels) levels) (= (reverse (sort levels)) levels))
        (diff-in-levels-at-least-1-and-at-most-3? levels)))

(defn drop-nth [n coll]
      (keep-indexed #(if (not= %1 n) %2) coll))

(defn report-safe-with-dampener? [levels]
      (->> (range (count levels))
           (map #(drop-nth % levels))
           (filter report-safe?)
           (not-empty)))

(defn safe-report-count [reports] (count (filter report-safe? reports)))

(defn safe-report-with-dampener-count [reports] (count (filter report-safe-with-dampener? reports)))

(defn solve-one [input-path]
      (->> input-path
           (input-reader/read-lines)
           (map (comp #(map ^[String] Integer/parseInt %) #(str/split % #" ")))
           (safe-report-count)
           (println)))

(defn solve-two [input-path]
      (->> input-path
           (input-reader/read-lines)
           (map (comp #(map ^[String] Integer/parseInt %) #(str/split % #" ")))
           (safe-report-with-dampener-count)
           (println)))

(solve-one "./problem-input/two/example_1.txt")
(solve-one "./problem-input/two/problem.txt")
(solve-two "./problem-input/two/example_1.txt")
(solve-two "./problem-input/two/problem.txt")
