(ns input-reader
  (:require [clojure.string :as str]))

(defn read-lines
      [input-path]
      (str/split-lines (slurp input-path)))
