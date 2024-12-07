(ns six
  (:require [input-reader]))

(defn char-at [grid x y] (str ((grid :values) [x y])))

(defn valid-pos?
      ([grid x y] (let [[n m] (grid :size)] (valid-pos? n m x y)))
      ([n m x y] (and (< x n) (>= x 0) (< y m) (>= y 0))))

(defn valid-step? [grid x y]
      (let [[n m] (:size grid)]
        (and (valid-pos? n m x y) (not (= (char-at grid x y) "#")))
        ))

(defn guard-position [grid i j]
      (let [[n m] (:size grid)]
        (cond
          (not (valid-pos? n m i j)) nil
          (= (char-at grid i j) (str "^")) [i j]
          (< (inc j) m) (guard-position grid i (inc j))
          :else (guard-position grid (inc i) 0)
          )))

(defn distinct-positions
      ([grid]
       (let [init-guard-position (guard-position grid 0 0)]
         (distinct-positions grid init-guard-position "UP" #{init-guard-position})))

      ([grid pos dir positions]
       (let [[i j] pos]
         (case dir
           "UP"
           (cond
             (not (valid-pos? grid (dec i) j)) positions

             (valid-step? grid (dec i) j)
             (distinct-positions grid [(dec i) j] dir (conj positions [(dec i) j]))

             (valid-step? grid i (inc j))
             (distinct-positions grid [i (inc j)] "RIGHT" (conj positions [i (inc j)])))

           "DOWN"
           (cond
             (not (valid-pos? grid (inc i) j)) positions

             (valid-step? grid (inc i) j)
             (distinct-positions grid [(inc i) j] dir (conj positions [(inc i) j]))

             (valid-step? grid i (dec j))
             (distinct-positions grid [i (dec j)] "LEFT" (conj positions [i (dec j)])))

           "LEFT"
           (cond
             (not (valid-pos? grid i (dec j))) positions

             (valid-step? grid i (dec j))
             (distinct-positions grid [i (dec j)] dir (conj positions [i (dec j)]))

             (valid-step? grid (dec i) j)
             (distinct-positions grid [(dec i) j] "UP" (conj positions [(dec i) j])))

           "RIGHT"
           (cond
             (not (valid-pos? grid i (inc j))) positions

             (valid-step? grid i (inc j))
             (distinct-positions grid [i (inc j)] dir (conj positions [i (inc j)]))

             (valid-step? grid (inc i) j)
             (distinct-positions grid [(inc i) j] "DOWN" (conj positions [(inc i) j])))
           )))
      )

(defn build-grid [lines]
      {:values (apply merge (flatten (map (fn [i] (map (fn [j] {(vector i j) (nth (nth lines i) j)}) (range (count (nth lines i))))) (range (count lines)))))
       :size   [(count lines) (count (first lines))]})

(defn solve-one [input-path]
      (->> (input-reader/read-lines input-path)
           (build-grid)
           (distinct-positions)
           (count)
           (println)))