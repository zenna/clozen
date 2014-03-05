(ns ^{:doc "Iterators"
      :author "Zenna Tavares"}
  clozen.iterator
  (:require [clozen.helpers :refer [repeat-until]])
  (:require [clojure.zip :as zip]))

;; Iterators
(defprotocol Itr
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
  (step [itr])
  (end? [itr])
  (update [itr node])
  (root [itr])
  (realise [itr]))

; There's more than one way to iterate through a collection
; - e.g. left->right vs right->left
; And there's more than one type of information that could be iterated over
; - e.g. nodes vs subtrees
; The design question is whether in each of these dimensions, different types
; should be distinguished by methods, or types
; Option one, both of these dimensions should be split on type, e.g. then I
; (defrecord NodeIterator) (defrecord SubtreeIterator)
; (defrecord SubtreeIterator-leaves-first)
; and the good news is that we can always just use step
; Bad news is that 1. I might end up with long iterator names
; If in some use case I need to go right until something and then do something else - I 
(defrecord SubtreeLeavesFirstItr
  ^{:doc
    "Iterates through all subtrees of an expression depth-first.
    e.g. (+ 3 4 (rand 0 1) (rand 3 4)) ->
         (+ 3 4 (rand 0 1) (rand 3 4)), (rand 0 1), (rand 3 4)"}
  [tree zipped-tree])

(defrecord NodeItr
  ^{:doc
    "Iterates through all nodes of an expression depth-first.
     e.g. (+ 3 4 (rand 0 1) (rand 3 4)) ->
          (+ 3 4 (rand 0 1) (rand 3 4)), +, 3, (rand 0 1), rand, 0, .."}
  [tree zipped-tree])

(defrecord SubtreeItr
  ^{:doc
    "Iterates through all subtrees of an expression depth-first.
    e.g. (+ 3 4 (rand 0 1) (rand 3 4)) ->
         (+ 3 4 (rand 0 1) (rand 3 4)), (rand 0 1), (rand 3 4)"}
  [tree zipped-tree])

(defn node-itr
  "Node iterator factory"
  [exp]
  (NodeItr. exp (zip/zipper coll? seq (fn [_ c] c) exp)))

(defn subtree-itr
  "Subtree iterator factory"
  [exp]
  (SubtreeItr. exp (zip/zipper coll? seq (fn [_ c] c) exp)))

(def abstract-zip-itr-impl
  "Abstract implementation for iterators built upon clojure/zip"
  {:step
    (fn [itr]
      (assoc itr :zipped-tree (zip/next (.zipped-tree itr))))

   :end?
   (fn [itr]
     (zip/end? (.zipped-tree itr)))

   :update
   (fn [itr node]
     (assoc itr :zipped-tree (zip/replace (.zipped-tree itr) node)))

   :root
   (fn [itr]
     (zip/root (.zipped-tree itr)))

   :realise
   (fn [itr]
     (first (.zipped-tree itr)))})

; Node iterator is the same as the abstract iterator
(extend NodeItr
  Itr
  abstract-zip-itr-impl)

(extend SubtreeItr
  Itr
  (assoc abstract-zip-itr-impl
    ; Keep doing zip/next until I get to a branch
    :step
    (fn [itr]
      (let [zip-tree
            (repeat-until zip/next
                          (.zipped-tree itr)
                          #(or (zip/end? %) (zip/branch? %)))]
        (assoc itr :zipped-tree zip-tree)))))

(extend SubtreeLeavesFirstItr
  Itr
  (assoc abstract-zip-itr-impl
    ; Keep doing zip/next until I get to a branch
    :step
    (fn [itr]
      (let [zip-tree
            (repeat-until zip/next
                          (.zipped-tree itr)
                          #(or (zip/end? %) (zip/branch? %)))]
        (assoc itr :zipped-tree zip-tree)))))