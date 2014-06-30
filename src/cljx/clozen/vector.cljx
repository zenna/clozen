(ns ^{:doc "Mathematical Vector"
      :author "Zenna Tavares"}
  clozen.vector
  (:require [clozen.helpers :refer [tolerant= sqr]]))

;; A High performance mathematical vector library

(defrecord VectorN
  [n-dims coords])

(defrecord Vector3
  [x y z])

(defrecord Vector2
  [x y])

;; Factories ==================================================================
(defn ->VectorN
  [dims coords]
  {:pre [(integer? dims)
         (vector? coords)]}
  (VectorN. dims coords))

(defn make-vector3
  [x y z]
  (->VectorN 3 [x y z]))

(defn unit-vectorn [n]
  (->VectorN n (vec (repeat n 1.0))))

(defn orig-vectorn [n]
  (->VectorN n (vec (repeat n 0.0))))

(def orig-vector3 (orig-vectorn 3))

(def orig-vector2 (orig-vectorn 2))

(defn std-uniform-vector [n]
  (->VectorN n (vec (repeatedly n rand))))

;; Accessors ==================================================================

(defn num-dims "Dimensionality of vector" [vectorn] (:n-dims vectorn))

(defn coords "Coordinates of vector" [vectorn] (:coords vectorn))

;; Fundamental Vector Operators ===============================================
(defn update-coords
  "Return vector with same dimensionality as vectorn, but with each element
   updated - (f elem)"
  [vectorn f]
  (->VectorN (num-dims vector) (mapv f (coords vectorn))))

(defn s*
  "Scalar Multiplication"
  [x vectorn]
  (->VectorN (num-dims vectorn) (mapv #(* x %) (coords vectorn))))

(defn v+
  "Vector Addition"
  [v1 v2]
  (->VectorN (num-dims v1) (mapv + (coords v1) (coords v2))))

(defn v-
  "Vector Subtraction v1 - v2"
  [v1 v2]
  (->VectorN (num-dims v1) (mapv - (coords v1) (coords v2))))

(defn add-scaled-v
  "v1 + s*v2"
  [v1 v2 s]
  (v+ v1 (s* s v2)))

(defn clear
  "Alias Zero vector"
  [vector]
  (orig-vector3))

;; Vector Products ============================================================
(defn component*
  "Component Product"
  [v1 v2]
  (->VectorN (num-dims v1) (mapv * (coords v1) (coords v2))))

(defn scalar*
  "Scalar Product"
  [v1 v2]
  (reduce + (mapv * (coords v1) (coords v2))))

(defn cross*
  "3D Cross Product"
  [v1 v2]
  {:pre [(= (num-dims v1) (num-dims v2) 3)]}
  (let [[x1 y1 z1] (coords v1)
        [x2 y2 z2] (coords v2)]
    (->VectorN 3
      [(- (* y1 z2) (* z1 y1))
       (- (* z1 x2) (* x1 z2))
       (- (* x1 y2) (* y1 x2))])))

(defn invert
  "Invert a vector"
  [vector3]
  (->Vector3 (- (:x vector3)) (- (:y vector3)) (- (:z vector3))))

(defn square-magnitude
  "Square Magnitude of vector"
  [vectorn]
  (reduce + (map * (:coords vectorn) (:coords vectorn))))

(defn magnitude
  "Magnitude of a vector"
  [vectorn]
  (Math/sqrt (square-magnitude vectorn)))

(defn sqr-dist
  "Squared Euclidean distance - typcially for effiency"
  [v1 v2]
  (reduce + (map (comp sqr -)  (coords v1) (coords v2))))

(defn dist
  "Euclidean distance"
  [v1 v2]
  (Math/sqrt (sqr-dist v1 v2)))

(defn normalise
  "Turns return unit vector"
  [vectorn]
  (let [l (magnitude vectorn)]
    (s* (/ 1 (magnitude vectorn)) vectorn)))

(defn orthonormal-basis
  "Get - orthonormal basis three mutually orthogonal by computing cross product
   between non-parallel v1 and v2"
  [v1 v2]
  (let [v1-norm (normalise v1)
        v3 (cross* v1-norm v2)
        v3-norm (normalise v3)]
    (if (tolerant= 0.0 (square-magnitude v3))
        (throw (#+cljs js/Error. Exception. "v1 and v2 parallel"))
        [v1-norm (cross* v3-norm v1-norm) v3-norm])))

;; Comment ===================================================================
(comment
  (def my-vector (std-uniform-vector 3))
  (magnitude my-vector)

  (coords my-vector)
  (s* 3 my-vector)

  (v+ my-vector my-vector)

  (def my-vector2 (std-uniform-vector 3))

  (normalise my-vector)
  (orthonormalBasis my-vector my-vector2)
  (+ 1 1)
  (orig-vectorn 4))
