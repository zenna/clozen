(ns clozen.geometry.sphere
  (:require [clozen.helpers :refer [sqr]]
            [clozen.vector :refer [sqr-dist]])
  (:import [clozen.vector VectorN]))

(defrecord h-sphere
  [n-dims ^VectorN center])

;; Accessors ==================================================================
(defn num-dims [x]  (:n-dims x))
(defn bounds [x] (:bounds x))

;; Intersections ==============================================================
(defn intersect-hypersphere?
  "Do two hyperspheres intersect?
   They intersect if, and only if, the distance between their centers
   is between the sum and the difference of their radii.
   Avoids sqrt"
  [[c1 r1] [c2 r2]]
  (let [centre-diff (sqr-dist c1 c2)]
    (and (<= (sqr (- r1 r2)) centre-diff)
         (<= centre-diff (sqr (+ r1 r2))))))
