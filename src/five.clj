(ns five
  (:require [clojure.string :as str]
            [input-reader]))

(defn get-order [line] (map ^[String] Integer/parseInt (str/split line #"\|")))

(defn get-rules
      ([lines]
       (get-rules lines {}))

      ([lines rules]
       (if (= (first lines) "")
         {:rules rules :remaining-lines lines}
         (let [[left right] (get-order (first lines))]
           (get-rules (rest lines) (assoc rules left (conj (if (contains? rules left) (rules left) #{}) right))))
         ))
      )

(defn get-pages [lines]
      (->> lines
           (map #(str/split % #","))
           (map #(map ^[String] Integer/parseInt %))))

(defn get-rules-and-pages [input-path]
      (let [{rules :rules remaining-lines :remaining-lines}
            (get-rules (input-reader/read-lines input-path))]
        {:ordering-rules   rules
         :pages-to-produce (get-pages (rest remaining-lines))}))

(defn correctly-ordered?
      ([page ordering-rules] (correctly-ordered? (rest page) (first page) ordering-rules))

      ([page prev ordering-rules]
       (cond
         (empty? page) true
         (contains? (ordering-rules prev) (first page)) (correctly-ordered? (rest page) (first page) ordering-rules)
         :else false))
      )

(defn seq-middle [coll] (nth coll (/ (count coll) 2)))

(defn solve-one [input-path]
      (let [{ordering-rules :ordering-rules pages-to-produce :pages-to-produce}
            (get-rules-and-pages input-path)]
        (->> pages-to-produce
             (filter #(correctly-ordered? % ordering-rules))
             (map seq-middle)
             (reduce +))))

(defn get-correct-ordering
      ([page ordering-rules]
       (let [potential-correct-ordering (get-correct-ordering (rest page) (first page) ordering-rules)]
         (if (= potential-correct-ordering page)
           page
           (get-correct-ordering potential-correct-ordering ordering-rules))))

      ([page prev ordering-rules]
       (cond
         (empty? page) [prev]
         (contains? (ordering-rules prev) (first page)) (concat [prev] (get-correct-ordering (rest page) (first page) ordering-rules))
         :else (concat [(first page)] (get-correct-ordering (rest page) prev ordering-rules)))))

(defn solve-two [input-path]
      (let [{ordering-rules :ordering-rules pages-to-produce :pages-to-produce}
            (get-rules-and-pages input-path)]
        (->> pages-to-produce
             (filter #(not (correctly-ordered? % ordering-rules)))
             (map #(get-correct-ordering % ordering-rules))
             (map seq-middle)
             (reduce +))))

;(five/solve-one "./problem-input/five/example.txt")
;(five/solve-one "./problem-input/five/problem.txt")
;(five/solve-two "./problem-input/five/example.txt")
;(five/solve-two "./problem-input/five/problem.txt")
