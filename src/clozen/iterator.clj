(ns ^{:doc "Iterators"
      :author "Zenna Tavares"}
  clozen.iterator
  (:require [clozen.helpers :refer [repeat-until]])
  (:require [clojure.zip :as zip]))

;; Iterators
(defprotocol Iterator
  "This iterator is a purely functional version of an iterator ala C++
   The purpose is to abstract away the mechanics of iteration for better
   modularity.
   Suppose you have a pattern matching algorithm and you want to parameterise
   whether you are matching against every single node or just subgraphs.
   Iteration is done with (step iterator) which returns a new iterator,
   with some kind of pointer (this is a protocol so implementation details) to
   the next location.
   Since the iterator contains lots more information than you may want for
   any particular algorithm that uses it, we must use the realise function to
   get the actual value"
  (step [iterator])
  (end? [iterator])
  (update [iterator node])
  (root [iterator])
  (realise [iterator]))

(defrecord NodeIterator
  ^{:doc
    "Iterates through all nodes of an expression depth-first.
     e.g. (+ 3 4 (rand 0 1) (rand 3 4)) ->
          (+ 3 4 (rand 0 1) (rand 3 4)), +, 3, (rand 0 1), rand, 0, .."}
  [tree zipped-tree])

(defrecord SubtreeIterator
  ^{:doc
    "Iterates through all subtrees of an expression depth-first.
    e.g. (+ 3 4 (rand 0 1) (rand 3 4)) ->
         (+ 3 4 (rand 0 1) (rand 3 4)), (rand 0 1), (rand 3 4)"}
  [tree zipped-tree])

(defn node-iterator
  "Node iterator factory"
  [exp]
  (NodeIterator. exp (zip/zipper coll? seq (fn [_ c] c) exp)))

(defn subtree-iterator
  "Subtree iterator factory"
  [exp]
  (SubtreeIterator. exp (zip/zipper coll? seq (fn [_ c] c) exp)))

(def abstract-zip-iterator-impl
  "Abstract implementation for iterators built upon clojure/zip"
  {:step
    (fn [iterator]
      (assoc iterator :zipped-tree (zip/next (.zipped-tree iterator))))

   :end?
   (fn [iterator]
     (zip/end? (.zipped-tree iterator)))

   :update
   (fn [iterator node]
     (assoc iterator :zipped-tree (zip/replace (.zipped-tree iterator) node)))

   :root
   (fn [iterator]
     (zip/root (.zipped-tree iterator)))

   :realise
   (fn [iterator]
     (first (.zipped-tree iterator)))})

; Node iterator is the same as the abstract iterator
(extend NodeIterator
  Iterator
  abstract-zip-iterator-impl)

(extend SubtreeIterator
  Iterator
  (assoc abstract-zip-iterator-impl
    ; Keep doing zip/next until I get to a branch
    :step
    (fn [iterator]
      (let [zip-tree
            (repeat-until zip/next
                          (.zipped-tree iterator)
                          #(or (zip/end? %) (zip/branch? %)))]
        (assoc iterator :zipped-tree zip-tree)))))