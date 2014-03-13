(ns ^{:doc "Helper functions useful for many different problems"
      :author "Zenna Tavares"}
  clozen.zip
  (:require [clojure.zip :as zip]))

;; Zipper Helpers
(defn zip-loc-pos
  "What is the position of loc in seq" [zipper]
  (if-let [lefts (zip/lefts zipper)]
    (count lefts)
    0))

(defn base [loc]
  "For a given loc, return a seq of all the elements on its level"
  (concat (zip/lefts loc) (list (zip/node loc)) (zip/rights loc)))
