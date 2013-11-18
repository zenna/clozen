(ns ^{:doc "Helper functions useful for many different problems"
      :author "Zenna Tavares"}
  clozen.helpers
  (:require [clojure.set :refer :all]))

;; Number helpers

; NOTEST
(defn reciprocal
  "1/x"
  [x] (/ 1 x))

; NOTEST
(defn sum
  [coll]
  (reduce + coll))

; NOTEST
(defn mean
  [coll]
  (/ (reduce + coll)
      (count coll)))

; NOTEST
(defn NaN?
  "Test if this number is nan"
  [x]
  ; Nan is the only value for which equality is false
  (false? (== x x)))

; NOTEST
(defn sign
  "Sign of number"
  [x]
  (cond
    (pos? x) 'pos
    (neg? x) 'neg
    :else 'no-sign))

; NOTEST
(defn sqr
  "x^2"
  [x]
  (* x x))

;FIXME NEED BETTER ACCOUNT FOR FLOATING POINT PRECISION
; an equality to use for floating point arithmetic
; NOTEST
(defn tolerant=
  [x y]
  (< (* (- x y) (- x y)) 0.00001))

;; Collection helpers
; NOTEST
(defn between
  "Applies f(x,y) to list[n,n+1] to create a list of size count - 1"
  [f, coll]
  (if (= (count coll) 1)
    []
    (concat [(f (first coll) (second coll))] (between f (rest coll)))))

(defn consistent?
  [f coll]
  "is (f x) the same for all x in coll"
  (every? #(= (f %) (f (first coll))) coll))

(defn count=
  "Are these colls the same size?"
  [& args]
  (every? #(= (count (first %)) (count (second %)))
           (partition 2 1 args)))

(defn normalise
  [coll]
  (map #(/ % (sum coll)) coll))

; NOTEST
(defn lines
	"returns [(f0 v0),...,(fn vn)]"
	[funcs values]
	(if (= (count values) 0)
		[]
		(concat [((first funcs) (first values))]
			(lines (rest funcs) (rest values)))))

(defn counts
  "Count the elements of a collection"
  [coll]
  (loop [counts-map {} coll coll]
    (cond
      (empty? coll)
      counts-map

      (nil? (counts-map (first coll)))
      (recur (assoc counts-map (first coll) 1) (rest coll))

      :else
      (recur (update-in counts-map [(first coll)] inc) (rest coll)))))

(defn merge-sets
  "Merge sets"
  [sets]
  (reduce #(reduce conj %1 %2) sets))

; NOTEST
(defn unique-pairs
  "Returns a list of all the unique pairs of a collection"
  [coll]
  (loop [coll coll pairs []]
    (cond
    (= (count coll) 1)
    pairs

    :else
    (recur (rest coll) (concat pairs
      (for [x (rest coll)]
        [(first coll) x]))))))

;FIXME : currently returning only first
;NOTEST
(defn max-elements
  "Returns elements of coll where f(elements) is maximal"
  [f coll]
  (let [mapped-coll (map f coll)
        max-val (apply max mapped-coll)
        best-index (.indexOf mapped-coll max-val)]
    [(nth coll best-index)]))

(defn first-elements
  "Get the first elements of a set of sets, unless"
  [sets ignores in-ignore?]
  (loop [product-tuple [] sets sets]
    ; (println "sets " sets)
    (cond
      (or (nil? sets) (nil? (first sets)))
      product-tuple

      :else
      (if-let [set-op (remove #(in-ignore? product-tuple ignores %) (first sets))]
        (if (and (coll? set-op) (empty? set-op))
            product-tuple
            (recur (conj product-tuple (first set-op)) (next sets)))
        product-tuple))))

(defn in-ignore?
  "if I add elem to this build will it become a superset of any of the ignores"
  [build ignores elem]
  (some  #(clojure.set/superset? (conj (set build) elem) %) ignores))

(defn cartesian-product-ignore
  "All the ways to take one item from each sequence, except for banned
   Ignore is set of terms, where each term belongs to one of the sequences
   and I don't want these in my cart product"
  [ignores original-sets]
  (loop [cart-prod #{} sets original-sets]
    (let [firsts (first-elements sets ignores in-ignore?)]
      ; (print "firsts " firsts "-cart-prod " cart-prod " sets " sets "\n")
      (cond
        (zero? (count firsts))
        cart-prod

        (= (count sets) (count firsts))
        (recur (conj cart-prod firsts) (update-in sets [(dec (count sets))] next))

        :else
        (recur cart-prod (assoc
                           (update-in sets [(dec (count firsts))] next)
                           (count firsts)
                           (original-sets (count firsts))))))))

(defn zeros
  [n]
  (repeat n 0))

(defn first-index
  [coll value]
  (let [i (.indexOf coll value)]
    (if (= -1 i) 
        nil
        value)))

(defn first-index-pred
  [pred coll]
  (loop [coll coll i 0]
    (cond
      (empty? coll) nil
      (pred (first coll)) i
      :else
      (recur (rest coll) (inc i)))))
  
; NOTEST
; TODO- THIS IS INEFFICIENT< TRAVERSES ENTIRE LIST IN ALL CASES
(defn max-pred-index
  "What is index of the value of coll such that (pred coll) is true" 
  [pred coll]
  (loop [i 0 max-i nil coll coll]
    (cond
      (empty? coll)
      max-i

      (pred (first coll))
      (recur (inc i) i (rest coll))

      :else
      (recur (inc i) max-i (rest coll)))))

(defn max-pred
  "What is index of the value of coll such that (pred coll) is true" 
  [pred coll]
  (loop [coll coll]
    (cond
      (empty? coll)
      nil

      (pred (peek coll))
      (peek coll)
      
      :else
      (recur (pop coll)))))

(defn min-pred
  "What is index of the value of coll such that (pred coll) is true" 
  [pred coll]
  (loop [coll coll]
    (cond
      (empty? coll)
      nil

      (pred (first coll))
      (first coll)
      
      :else
      (recur (rest coll)))))

;; Map Helpers

; NOTEST
(defn extract
  "For list of maps, extract a key"
  [coll keyfn]
  (map (fn [m] (keyfn m)) coll))

; NOTEST
(defn extract-in
  "For list of maps, extract a key
  (extract-in [{:a {:b 'c}} {:a {:b 'd}}] [:a :b])
  => (c d)"
  [coll ks]
  (cond
    (list? coll) (map (fn [m] (get-in m ks)) coll)
    (vector? coll) (mapv (fn [m] (get-in m ks)) coll)))

(defn vec-remove
  "remove elem from coll"
  [coll pos]
  {:pre [(= clojure.lang.PersistentVector (type coll))]}
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

;; Stochastic functions
; NOTEST
(defn rand-bool
  "Return uniform over true,false"
  []
  (= (rand-int 2) 1))

(defn rand-vec-remove
  "Uniformly sample elem in coll and remove it"
  [coll]
  (let [to-take-i (rand-int (count coll))]
    [(nth coll to-take-i)
     (vec-remove coll to-take-i)]))

(defn flip
  [p]
  (< (rand) p))

(defn categorical
  "Categorical distribution"
  [coll weights]
  (let [sorted-coll (sort-by val < (zipmap coll weights))
        total-weight (sum (vals sorted-coll))
        rand-point (rand total-weight)]
    (loop [coll-loop sorted-coll accum-weight 0.0]
      (if (>= (+ accum-weight (val (first coll-loop)))
              rand-point)
          (key (first coll-loop))
          (recur (rest coll-loop)
                 (+ accum-weight (val (first coll-loop))))))))

; NOTEST
(defn rand-nth-reciprocal-categorical
  "Categorical distribution using reciprocal of weights"
  [coll weights]
  {:pre [(count= coll weights)]}
  (let [coll-weights (zipmap coll weights)
        clean-coll (filter #(not (NaN? (second %))) (seq coll-weights))
        ; clean-coll (filter #(not (NaN? (weight-key %1))) coll)
        zero-cost-coll (filter #(zero? (second %)) clean-coll)
        sample
        (if (empty? zero-cost-coll)
            (let [sorted-coll (sort-by val < clean-coll)
                  total-weight (sum (map #(reciprocal (val %1)) clean-coll))
                  ; ok (println "actual-total" total-weight)
                  rand-point (rand total-weight)]
                  (loop [clean-coll-loop sorted-coll accum-weight 0.0]
                    (if (>= (+ accum-weight (reciprocal (val (first clean-coll-loop))))
                            rand-point)
                      (key (first clean-coll-loop))
                      (recur (rest clean-coll-loop)
                             (+ accum-weight (reciprocal 
                                                 (val (first clean-coll-loop))))))))
            (key (rand-nth zero-cost-coll)))]
    sample))

; NOTEST
(def reciprocal-categorical rand-nth-reciprocal-categorical)



; NOTEST
(defn in? 
  "true if seq contains elm"
  [seq elm]  
  (some #(= elm %) seq))

; Returns map of key-list to element at that place .e.g {[1 2 1] 'y, ...}
; NOTEST
(defn coll-to-keys
  "Find nested keys to elements in map, ignore those where ignore-elem is true"
  [coll ignore-elem?]

  ((fn depth-first [coll all-keys base-keys pos]
          ; (println "eq" equation "all-keys" all-keys "base-keys" base-keys "pos" pos)
          (cond
            (empty? coll)
            all-keys

            ; Don't add if we want to ignore it
            (ignore-elem? (first coll) pos)
            (recur (rest coll) all-keys base-keys (inc pos))

            ; If it's a list add both the list AND recurse on the innards of the list
            (list? (first coll))
            (let [list-key {(conj base-keys pos) (first coll)}
                  inner-keys (depth-first (first coll) all-keys (conj base-keys pos) 0)]
              (recur (rest coll) (merge list-key inner-keys) base-keys (inc pos)))

            :else
            (recur (rest coll)
                   (merge all-keys {(conj base-keys pos) (first coll)})
                   base-keys
                   (inc pos))))

  coll {} [] 0))

; NOTEST
(defn pass
  "Apply a f to each element of coll and pass an object along while
   doing so.
   
   Use output of function eval as one input to a function
   (f init-ip (first coll) to yeild output
    then does (f output (second coll), and so on for all coll"
  [f init-ip coll]
  (loop [op init-ip coll coll]
  (if 
    (empty? coll) op
    (recur (f (first coll) op) (rest coll)))))

; NOTEST
(defn gen-until [f p]
  (let [x (f)]
    (if (p x) x (recur f p))))

; NOTEST
(defn walk-msg
  [f coll]
  "Performs a depth first, search on coll
  f:elm X message is applied to each nonlist element of coll
  and the message is passed along
  f must return {:elm element :msg message}"
  ((fn df-search
    [coll msg f-coll]
    (cond
      (empty? coll)
      {:msg msg :coll f-coll}

      (list? (first coll))
      (let [{in-coll :coll new-msg :msg} (df-search (first coll) msg '())]
        (recur (rest coll)
                new-msg
                (concat f-coll (list in-coll))))

      :else
      (let [{new-elm :elm new-msg :msg} (f (first coll) msg)]
        (recur (rest coll)
               new-msg
               (concat f-coll (list new-elm))))))

  coll {} '()))

; NOTEST
(defn replace-in-list [coll n x]
  (concat (take n coll) (list x) (nthnext coll (inc n))))

; NOTEST
(defn replace-in-sublist [coll ns x]
  (if (seq ns)
    (let [sublist (nth coll (first ns))]
      (replace-in-list coll
                       (first ns)
                       (replace-in-sublist sublist (rest ns) x)))
    x))

(defn vec-f
  "Appy a function element wise between a number of vectors"
  [f & args]
  (apply mapv `(~(fn [& args] (apply f args))
                    ~@args)))

; TODO TEST
(defn vec-scalar-f
  "scalar f (e.g. multiply division etc) of vector"
  [f v scalar]
  (map #(f %1 scalar) v))

; NOTEST
(defn empty-to-nil
  "If it's empty return nil, otherwise return the collection"
  [coll]
  (if (empty? coll)
      nil
      coll))

; NOTEST
(defn cons-conj
  [a b]
  "convenience function for
  a,b = [a,b]
  [a], b -> [a b]
  a, [b] -> [a b]
  (a),b -> (a,b)"
  (cond
    (and (coll? a) (coll? b)) (concat a b)
    (vector? a) (conj a b)
    (list? a) (concat a (list b))
    :else
    [a b]))

; NOTEST
(defn assoc-coll
  "assoc for collection of maps
   e.g. (assoc-coll [{:a 0 :b 1} {:c 2 :d 3}] :f 4)
   -> [{:a 0 :b 1 f: 4} {:c 2 :d 3 :f 4}]"
  [coll key val]
  (cond
    (vector? coll)
    (mapv #(assoc % key val) coll)
    :else
    (map #(assoc % key val) coll)))

; NOTEST
(defn remove-nth
  "Returns vector with nth value removed"
  [v i]
  (vec (concat (subvec v 0 i)
               (subvec v (inc i)))))

; NOTEST
(defn make-lambda-args
  "Make a function from an expression with some args"
  [expr args]
  (eval (list 'fn args expr)))

;; Syntactic Sugar Macros
(defmacro l
  [& params]
  {:pre [(odd? (count params))]}
  (let [args (drop-last params)
        body (last params)]
  `(let [~@args]
     ~body)))