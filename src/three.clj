(ns three
  (:require [clojure.string :as str]
            [input-reader]))

(defn parse-mul [s]
      (-> s (str/replace "mul(" "") (str/replace ")" "") (str/split #",")))

(defn exec-mul [s]
      (reduce * (map ^[String] Integer/parseInt (parse-mul s))))

(defn get-sum-of-muls [line]
      (reduce + (map exec-mul (re-seq #"mul\([0-9]+,[0-9]+\)" line))))

(defn get-sum-of-muls-with-do
      [enabled coll]
      (if (empty? coll)
        0
        (let [word (first coll)]
          (cond
            (= word "do()") (get-sum-of-muls-with-do true (rest coll))
            (= word "don't()") (get-sum-of-muls-with-do false (rest coll))
            (true? enabled) (+ (exec-mul word) (get-sum-of-muls-with-do true (rest coll)))
            (false? enabled) (get-sum-of-muls-with-do false (rest coll))))
        ))

(defn solve-one [input-path]
      (->> input-path
           (input-reader/read-lines)
           (map get-sum-of-muls)
           (flatten)
           (reduce +)
           (println)))

(defn parse-line [line] (re-seq #"mul\([0-9]+,[0-9]+\)|don't\(\)|do\(\)" line))

(defn solve-two [input-path]
      (->> input-path
           (input-reader/read-lines)
           (map parse-line)
           (flatten)
           (get-sum-of-muls-with-do true)
           (println)))

;(three/solve-one "./problem-input/three/example_1.txt")
;(three/solve-one "./problem-input/three/problem.txt")
;(three/solve-two "./problem-input/three/example_2.txt")
;(three/solve-two "./problem-input/three/problem.txt")
