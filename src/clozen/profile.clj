(ns ^{:author "Zenna Tavares"
      :doc "Profiling Tools"}
  clozen.profile
  (:require [clozen.helpers :refer :all])
  (:require [taoensso.timbre :as timbre
                             :refer (trace debug info warn
                                     error fatal spy with-log-level)])
  (:require [taoensso.timbre.profiling :as profiling :refer (p o profile)]))

; Profiling Data
(defn powers-of-n
  "Lazy sequence of increasing powers of n"
  [n]
  (iterate (partial * n) 1))

(defn positive-numbers
  ([] (positive-numbers 1))
  ([n] (cons n (lazy-seq (positive-numbers (inc n))))))

(defn make-input
  [n-take & args]
  (let [x
        (for [arg args]
          (take n-take arg))]
    (apply map list x)))

(defn scaling
  "Scaling analysis: see how some property of the program (e.g. run time,
   size of output) varies with some input.

   Automatcally gathers coarse run time information, other properties are
   required then use the o macro as described in taoensso.timbre.profiling.

   f: the function to be tested
   input-gen: generates inputs for f, e.g. gen random vector
   inputs: sequence of inputs to input-gen, e.g. length of vector
   n-samples: number of samples for each input size

   Will return a map of type
   {input0 {metric1 {:count [sample0 sample1] :min [sample0 sample1] ...}
    ...}


   "
  [f input-gen inputs n-samples]
  (println "ip" inputs)
  (let [results (atom {})
        nested-merge
        (fn [a b]
          (merge-with 
            (partial merge-with
              (partial merge-with cons-conj)) b a))
        appender-fn
        (fn [{:keys [ap-config level prefix throwable message profile-stats object-stats orig-name] :as args}]
          ; (println "- " (keys args) "**" (vals args) "- \n")
          ; (println "\n" object-stats "\n")
          (swap! results (partial nested-merge {orig-name (merge profile-stats object-stats)})))]

    (timbre/set-config!
     [:appenders]
     {:my-appender {:doc       "Hello-world appender"
      :min-level :debug
      :enabled?  true
      :async?    false
      :limit-per-msecs nil ; No rate limit
      :fn appender-fn}})

    (doall
      (for [input inputs]
           (doall (repeatedly n-samples
                       #(profile :info input
                                 (p :whole (f (apply input-gen input))))))))
    @results))

;; Post Processing
(defn scale-indep-input
  "Extract data from scaling results map (for plotting perhaps)
   prof-keys is a list of list of keys, where each list of keys points to data
   within the nested results map.
   e..g [[:taoensso.timbre.profiling/whole :max]
         [:taoensso.timbre.profiling/whole :count]]"
  [results & prof-keys]
  (reduce-kv
    (fn [pass-map key val]
      (assoc pass-map key (mapv #(get-in val %) prof-keys)))
    {}
    results))

(defn flatten-scaling-data
  [data]
  "
   Data - from scale indep input"
  (mapv 
    (fn [i] (vec (reduce concat (mapv #(nth % i) (vals data)))))
    (range (count (first (vals data)))))) ; 0, 1,..,n-samples

(defn get-scaling-data
  [results sort-arg prof-keys]
  (let [sorted-map (sort-by #(nth (key %) sort-arg ) results)]
    [(mapv #(nth % sort-arg) (vec (keys sorted-map)))
    (mapv #(double (mean %))
          (extract-in (vec (vals sorted-map)) prof-keys))]))  

(comment
  (defn rand-vector
    [n]
    (vec (repeatedly n rand)))

  (scaling #(profile :info :sort-time sort) rand-vector
                                            (map vector
                                                 (take 20 (powers-of-n 2)) 10))

  (scale-indep-input r [:n-boxes :mean] [:expand :count]))