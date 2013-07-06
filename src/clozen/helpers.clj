(ns ^{:doc "Helper functions useful for many different problems"
      :author "Zenna Tavares"}
  clozen.helpers)

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

;FIXME : currently returning only first
;NOTEST
(defn max-elements
  "Returns elements of coll where f(elements) is maximal"
  [f coll]
  (let [mapped-coll (map f coll)
        max-val (apply max mapped-coll)
        best-index (.indexOf mapped-coll max-val)]
    [(nth coll best-index)]))

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
  (map (fn [m] (get-in m ks)) coll))

;; Stochastic functions
; NOTEST
(defn rand-bool
  "Return uniform over true,false"
  []
  (= (rand-int 2) 1))

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
  "Use output of function eval as one input to a function
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

; NOTEST
(defn vec-f
    "Like merge-with but for vectors"
  [f v1 v2]
  (loop [index 0 merged-vec []]
    (if (= index (count v1))
      merged-vec
      (recur (inc index) (conj merged-vec (f (nth v1 index) (nth v2 index)))))))

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
