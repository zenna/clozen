(ns ^{:doc "Iterators"
      :author "Zenna Tavares"}
  clozen.iterator
  (:require [clozen.helpers :refer [repeat-until repeat-before-until]])
  (:require [clozen.debug :refer [dbg]])
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
; and the good news is that we can always just use (step itr)
; Bad news is that 1. I might end up with long iterator names
; 2. If some algorithm requires two types of iteration, I'll need two iterators.
; Option 2: is have multiple protocols.
; I choose Option 1 as it is simpler 
(defrecord SubtreeLeavesFirstItr
  ^{:doc
    "Iterates through all subtrees of an expression under constraint that
     no subtree will be seen before its children subtrees have been seen.
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

(defn go-to-next
  "For a leaves first subtree traversal, go to the next subtree.
   This is the parent of the next rightmost leaf"
  [zipped-tree]
  (loop [zipped-tree zipped-tree]
    (cond
      (zip/branch? zipped-tree)
      (recur (zip/down zipped-tree))

      (nil? (zip/right zipped-tree))
      (zip/up zipped-tree)

      :else
      (recur (zip/right zipped-tree)))))

(extend SubtreeLeavesFirstItr
  Itr
  (assoc abstract-zip-itr-impl
    :step
    (fn [itr]
      (let [zipped-tree (.zipped-tree itr)
            zip-tree-moved
            (cond
              (nil? (zip/up zipped-tree))
              (assoc zipped-tree 1 :end)
              
              (nil? (zip/right zipped-tree))
              (zip/up zipped-tree)

              :else
              (go-to-next (zip/right zipped-tree)))]
        (assoc itr :zipped-tree zip-tree-moved)))))

(defn subtree-leaves-first-itr
  "Subtree iterator factory"
  [exp]
  (SubtreeLeavesFirstItr. exp 
    (-> (zip/zipper coll? seq (fn [_ c] c) exp) zip/down go-to-next)))

(defn add-itr-constraint
  "Adds a constraint to the iterator
   Note it this is a predicate on the iterator, the actual value can be
   got at by using realise, e.g. for a seq constraint #(seq? (realise %))"
  [itr pred?]
  (reify Itr
    (end? [_] (end? itr))
    (update [_ node] (update itr node))
    (root [_] (root itr))
    (realise [_] (realise itr))
    (step [_] (add-itr-constraint
                (repeat-until step itr #(or (end? %) (pred? %)))
                pred?))))

(defn iterate-and-print
  [itr]
  (loop [itr itr]
    (when (not (end? itr))
      (println (realise itr))
      (recur (step itr)))))

(defn iterate-and-print-fn
  [itr f]
  (loop [itr itr]
    (when (not (end? itr))
      (println (realise itr) "-" (f itr))
      (recur (step itr)))))

(comment
  (def x (node-itr '(+ 1 2 (* 3 4) (+ 4 5 6) y (/ (plus a b) 6))))
  (def x (node-itr '(+ 1 2 3 (* 3 4))))
  (def y (add-itr-constraint x #(seq? (dbg (realise %)))))  
  (def z (add-itr-constraint y #(= (dbg (count (realise %))) 30)));#(= (count %) 2)))
  (iterate-and-print (step z))

  (def x (node-itr '(+ 3 2 (if (> x 3) 7 (if false 3 (+ 1 1))))))

  (iterate-and-print-fn x #(check-if (.zipped-tree (dbg realise %)))))