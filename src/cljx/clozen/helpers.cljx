(ns ^{:doc "Helper functions useful for many different problems"
      :author "Zenna Tavares"}
  clozen.helpers
  (:require [clojure.set]))

;; Number helpers
(defn reciprocal
  "1/x"
  [x] (/ 1 x))

(defn sum
  [coll]
  (reduce + coll))

(defn mean
  [coll]
  (/ (reduce + coll)
      (count coll)))

(defn NaN?
  "Test if this number is nan"
  [x]
  ; Nan is the only value for which equality is false
  (false? (== x x)))

(defn sign
  "Sign of number"
  [x]
  (cond
    (pos? x) 'pos
    (neg? x) 'neg
    :else 'no-sign))

(defn sqr
  "x^2"
  [x]
  (* x x))

; NOTEST
(defn empty-to-nil
  "If it's empty return nil, otherwise return the collection"
  [coll]
  (if (empty? coll)
      nil
      coll))

(defn nil-to-false
  "Some functions return nil when I really want a predicate"
  [x]
  (if (nil? x)
      false
      x))

;FIXME NEED BETTER ACCOUNT FOR FLOATING POINT PRECISION
; an equality to use for floating point arithmetic
; NOTEST
(defn tolerant=
  [x y]
  (< (* (- x y) (- x y)) 0.00001))

;; Collection helpers
(defn between
  "Applies f(x,y) to list[n,n+1] to create a list of size count - 1"
  [f, coll]
  (if (= (count coll) 1)
    []
    (concat [(f (first coll) (second coll))] (between f (rest coll)))))

; TODO: Give this a better name
(defn consistent?
  "is (f x) the same for all x in coll"
  [f coll]
  (every? #(= (f %) (f (first coll))) coll))

(defn count=
  "Are these colls the same size?"
  [& args]
  (every? #(= (count (first %)) (count (second %)))
           (partition 2 1 args)))

(defn normalise
  "Normalise a vector of numbers to sum to 1"
  [coll]
  (map #(/ % (sum coll)) coll))

; TODO: Give this a better name
(defn lines
	"returns [(f0 v0),...,(fn vn)]"
	[funcs values]
	(if (= (count values) 0)
		[]
		(concat [((first funcs) (first values))]
			(lines (rest funcs) (rest values)))))

(defn counts
  "Counts, i.e. makes a histogram of a collection"
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
  "Merge, i.e. find union of set of sets"
  [sets]
  (reduce #(reduce conj %1 %2) sets))

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

(defn cartesian-product
  "All the ways to take one item from each sequence - Taken from math.combinatorics"
  [& seqs]
  (let [v-original-seqs (vec seqs)
	step
	(fn step [v-seqs]
	  (let [increment
		(fn [v-seqs]
		  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
		    (if (= i -1) nil
			(if-let [rst (next (v-seqs i))]
			  (assoc v-seqs i rst)
			  (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
	    (when v-seqs
	       (cons (map first v-seqs)
		     (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))

(defn cartesian-product-ignore-fn [ignore-sets colls]
  (cond (some empty? ignore-sets) () ; prune
        (empty? colls) (list ())     ; base case
        :else                        ; recursive case
        (for [e (first colls)
              sub-product (cartesian-product-ignore-fn (map (fn [s]
                                                           (disj s e))
                                                         ignore-sets)
                                                    (rest colls))]
          (cons e sub-product))))

(defn cartesian-product-ignore
  [ignore-sets colls]
  "Remove an item from the set if
   ignore-sets is vector of sets [#{a b c}#{a b c}]"
  (let [singleton-ignores (filter #(= (count %) 1) ignore-sets)]
    (if (zero? (count singleton-ignores))
        (cartesian-product-ignore-fn ignore-sets colls)
        (let [filtered-ignores (remove #(= (count %) 1) ignore-sets)
              filtered-colls
              (map (fn [elem]
                   (remove #((reduce clojure.set/union singleton-ignores) %)
                            elem))
                   colls)
              n-terms (apply * (map (comp double count) filtered-colls))]
        (println "n-terms after singleton cull" n-terms)
        (cartesian-product-ignore-fn filtered-ignores filtered-colls)))))

(defn zeros
  [n]
  (repeat n 0))

(defn first-index
  "Finds index of value in coll, nil if not found"
  [coll value]
  (let [i (.indexOf coll value)]
    (if (= -1 i)
        nil
        i)))

(defn first-index-pred
  "Finds index where (pred? value) is true, nil if not found"
  [pred coll]
  (loop [coll coll i 0]
    (cond
      (empty? coll) nil
      (pred (first coll)) i
      :else
      (recur (rest coll) (inc i)))))

(defn min-pred
  "For all values in coll where (pred value) is true, what is value with
   smallest index"
  [pred coll]
  (loop [coll coll]
    (cond
      (empty? coll)
      nil

      (pred (first coll))
      (first coll)

      :else
      (recur (rest coll)))))

; NOTEST
; TODO- THIS IS INEFFICIENT< TRAVERSES ENTIRE LIST IN ALL CASES
(defn max-pred-index
  "For all values in coll where (pred value) is true, what is highest index"
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
  "For all values in coll where (pred value) is true, what is value with
   largest index"
  [pred coll]
  (loop [coll coll]
    (cond
      (empty? coll)
      nil

      (pred (peek coll))
      (peek coll)

      :else
      (recur (pop coll)))))

(defn repeat-until
  "Keep doing (f x), where x is initially init-value until
  (pred (f x)) is satisfied, then return (f x).
   e.g. (repeat-until 3 inc (partial > 7)"
  [f init-value pred?]
  (loop [value (f init-value)]
    (if (pred? value)
        value
        (recur (f value)))))

(defn repeat-before-until
  "Keep doing (f x), where x is initially init-value until
  (pred (f x)) is satisfied, then return x.
   e.g. (repeat-until 3 inc (partial > 7)"
  [f init-value pred?]
  (loop [value init-value]
    (let [f-value (f value)]
      (if (pred? f-value)
          value
          (recur f-value)))))

(defn loop-until-fn
  "Loop through coll until (f elem) is not nil.
   then return (f elem).
   If no matches for any then return nil"
  [f coll]
  (loop [coll coll]
    (if (seq coll)
        (if-let [x (f (first coll))]
          x
          (recur (next coll)))
      nil)))

(defn in?
  "true if seq contains elm"
  [coll elm]
  (nil-to-false (some #(= elm %) coll)))

; NOTEST
(defn pass
  "Pass a message through a collection, where each element transforms it
   according to some function f.

   init-ip is the initial message, typically an empty collection.
   f is a binary function which is applied to elements of list and message.

   Use output of function eval as one input to a function
   (f (first coll) init-ip  to yeild output
    then does (f (second coll)  output, and so on for all coll"
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

(defn vec-remove
  "remove elem from coll"
  [coll pos]
  {:pre [(= clojure.lang.PersistentVector (type coll))]}
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn vec-scalar-f
  "scalar f (e.g. multiply division etc) of vector"
  [f v scalar]
  (map #(f %1 scalar) v))

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
(defn remove-nth
  "Returns vector with nth value removed"
  [v i]
  (vec (concat (subvec v 0 i)
               (subvec v (inc i)))))

(defn transposev
  "Transpsoe a matrix, which is an evenly sized vector of vectors"
  [matrix]
  (apply mapv vector matrix))

(defn split-with-pos
  "Split a collection (like core.split-with), but retain positions
   of elements in original list.
   (split-with-pos even? [3 1 4])
   => [[4] [3 1] [2][0 1]]"
  [pred? coll]
  (subvec
    (reduce
     (fn [[i & _ :as accum] elem]
       (if (pred? elem)
           (-> accum (update-in [1] #(conj % elem))
                     (update-in [3] #(conj % i))
                     (update-in [0] inc))
           (-> accum (update-in [2] #(conj % elem))
                     (update-in [4] #(conj % i))
                     (update-in [0] inc))))
     [0 [][][][]]
     coll)
     1))

(defn unsplit
  "Having split a vector using split-with-pos (or some other means)
   We might want to put things back together again in correct order

   (apply unsplit (split-with-pos odd? [492 1 8 2 0 3 5]))
   => [492 1 8 2 0 3 5]"
  [a b c d]
  {:pre [(count= a c)
         (count= b d)]}
  (let [res (vec (zeros (+ (count a) (count b))))
        f
        (fn [res pos v]
        (loop [res res v v pos pos]
          (if (seq pos)
              (recur (assoc res (first pos) (first v)) (next v) (next pos))
              res)))]
    (-> res (f c a) (f d b))))


;; ===========
;; Map Helpers
(defn extract
  "For list of maps, extract a key"
  [coll keyfn]
  (map (fn [m] (keyfn m)) coll))

(defn extract-in
  "For list of maps, extract a key
  (extract-in [{:a {:b 'c}} {:a {:b 'd}}] [:a :b])
  => (c d)"
  [coll ks]
  (cond
    (list? coll) (map (fn [m] (get-in m ks)) coll)
    (vector? coll) (mapv (fn [m] (get-in m ks)) coll)))

(defn zipmap-count
  "Like zipmap except instead of providing vals you provide a function
   which is applied to the count of the keys.

   Convenient for things like:
   (zipmap-c '[a b c] range) => {c 2, b 1, a 0}
   (zipmap-c '[a b c] zeros) => {c 0, b 0, a 0}"
  [coll f]
  (zipmap coll (f (count coll))))

(defn update-values [m f & args]
  "Apply a Function To Each Value of a Map"
  (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))

;; Stochastic functions
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

(defn reciprocal-categorical
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
#+clj (defn make-lambda-args
        "Make a function from an expression with some args"
        [expr args]
        (eval (list 'fn args expr)))

;; IO
;; (defn coll-to-file
;;   "Writes a collection to a file, one line for each elem"
;;   [coll fname]
;;   (doall
;;     (for [elem coll]
;;       (with-open [wrtr (writer fname :append true)]
;;           (if (coll? elem)
;;                (.write wrtr (clojure.string/join "," (doall elem)))
;;                (.write wrtr  (str elem)))
;;           (.write wrtr "\n")))))

;; String Helpers
(def VALID-CHARS
  "Vector of valid characters"
  (map char
      (concat (range 48 58) ; 0-9
              (range 66 91) ; A-Z
              (range 97 123)))) ; a-z

(defn random-char []
  "Generate a random character"
  (rand-nth VALID-CHARS))

(defn random-str [length]
  "Generate a random string"
  (apply str (take length (repeatedly random-char))))

(defn str-to-doubles
  [string]
  (mapv #(Double/parseDouble %)
        (clojure.string/split string #",")))
