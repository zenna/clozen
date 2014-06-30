(ns ^{:doc "A line"
      :author "Zenna Tavares"}
  clozen.geometry.line
  (:require [clozen.vector :as zen-vec]
            [clozen.helpers :refer [count=]])
  (:import [clozen.vector VectorN]))

(defrecord Line
  [n-dims ^VectorN points])

(defn ->Line
  [points]
  {:pre [(apply count= (map zen-vec/num-dims points))]}
  (Line. (zen-vec/num-dims (first points)) points))