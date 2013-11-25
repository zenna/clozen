(ns ^{:author "Zenna Tavares"
      :doc "Comparative (bucket) Testing"}
  clozen.profile.bucket
  (:require [clozen.helpers :refer :all]))

(defonce* bucket-config
   "This map atom controls store the state for bucket tetsing"
   (atom {:forced-options {}
          :levels {}
          :seen-buckets []}))

(defn active-bucket? []
  (not (empty? (@bucket-config :levels))))

(defn enable-buckets!
  [& names]
  (swap! bucket-config #(merge % {:levels (zipmap-count names zeros)})))

(defn disable-buckets!
  [& names]
  (swap! bucket-config
        (fn [config]
          (update-in config [:levels]
                     #(apply (partial dissoc %) names)))))

(defn empty-seen-buckets! []
  (swap! bucket-config #(assoc % :seen-buckets [])))

(defn bucket-levels []
  (@bucket-config :levels))

(defn bucket-level
  [name]
  (get-in @bucket-config [:levels  name]))

; TODO, this should only add to seen once.
(defn update-seen-bucket!
  "Update config file with name of seen bucket

   When using (bucket-test ..) We store all the buckets we encounter in
   current-buckets of the config file.
   This allows bucket-test to decide which buckets to choose next."
  [name]
  (swap! bucket-config
        (fn [config] 
          (if (in? (config :seen-buckets) name) ; Don't add more than once
              config
              (update-in config [:seen-buckets] #(conj % name))))))

;TODO
(defn remove-seen-bucket!
  [name]
  (swap! bucket-config
        (fn [config] 
          (update-in config [:seen-buckets] #(vec (remove (partial = name) %))))))

(defn last-seen-bucket
  []
  (last (@bucket-config :seen-buckets)))

(defn increment-levels!
  []
  (swap! bucket-config
         (fn [config]
            (update-in config [:levels (last-seen-bucket)] inc))))

(defn buckets-left?
  []
  (empty? (@bucket-config :seen-buckets)))

(defn forced-option
  "Returns option name for a forced bucket or nil if not forced"
  [bucket-name]
  ((@bucket-config :forced-options) bucket-name))

(defn add-forced-options!
  "Add forced optiosn to config"
  [forced-options]
  (println "forced options" forced-options)
  (swap! bucket-config #(merge-with merge % {:forced-options forced-options})))

(defn remove-forced-options!
  "Add forced optiosn to config"
  []
  (swap! bucket-config #(assoc % :forced-options {})))

(defmacro ^{:private true} assert-args [fnname & pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  ~(str fnname " requires " (second pairs)))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args fnname more)))))

(defn named-bucket-fn
  "Designate what should be compared in Bucket testing

   Takes a list of arguments and will try all the alternatives for `whatever
   profiling is going on.

   If bucket is not enabled it will simply use the first argument as default
   "
  [bucket-name named-options]
  (assert-args
    (vector? named-options) "a vector for its binding"
    (even? (count named-options)) "an even number of forms in binding vector")
  (let [terms (take-nth 2 (next named-options))
        to-groups (partition 2 named-options)
        x named-options
        named-options (zipmap (map first to-groups) (map second to-groups))]
    `(if-let [bucket-pos# (bucket-level ~bucket-name)]
       (do
         (update-seen-bucket! ~bucket-name)
         (when (= bucket-pos# ~(dec (count terms)))
               (remove-seen-bucket! ~bucket-name))
         (case bucket-pos#
         ~@(reduce concat
             (for [i (range (count terms))]
               (list i (nth terms i))))))

       (if-let [forced-option# (forced-option ~bucket-name)]
         (~named-options forced-option#) ; Bucket is forced
         ~(nth terms 0))))) ; The bucket is disabled

(defmacro named-bucket
  [bucket-name named-options]
  (named-bucket-fn bucket-name named-options))

(defmacro bucket
  [bucket-name & terms] ; If no names given, give integer names by ordering
  (named-bucket-fn bucket-name (vec (interleave (range (count terms)) terms))))

(defn bucket-test-fn
  "Execute the bucket testing
   Returns a vector of all the return values"
  [bucket-names forced-options body]
  `(do
    (apply enable-buckets! ~bucket-names)
    (add-forced-options! ~forced-options)

    (try
      (loop [results# []]
        (let [result# (do ~@body)
              bucket-result# {:bucket (bucket-levels)
                             :result result#}]
          (cond
            (buckets-left?)
            (conj results# bucket-result#)

            :else
            (do
              (increment-levels!)
              (recur (conj results# bucket-result#))))))
      (finally
        (apply disable-buckets! ~bucket-names)
        (remove-forced-options!)
        (empty-seen-buckets!)))))

(defmacro bucket-test
  [bucket-names & body]
  (bucket-test-fn bucket-names {} body))

(defmacro bucket-test-force
  [bucket-names forced-options & body]
  (bucket-test-fn bucket-names forced-options body))

(comment
  (require '[clozen.profile.bucket :refer :all])
  (macroexpand '(named-bucket :coll-type [:normal-coll coll
                                          :vectorised-coll (vectorise coll)]))
  (macroexpand-1 '(bucket :coll-type coll (vectorise coll)))
  (macroexpand '(bucket-test [:vectorise] (vectorise (repeatedly 10 #(rand-int 10)))))

  (require '[taoensso.timbre :as timbre
                             :refer (trace debug info warn
                                     error fatal spy with-log-level)])
  (require '[taoensso.timbre.profiling :as profiling :refer (p o profile)])

  ; Using bucket with profile
  (defn vectorise
    [coll]
    (bucket :vectorise (vec coll) (vec (map inc coll))))

  (defn vectorise-named
    [coll]
    (named-bucket :vectorise [:just-vectorise (vec coll)
                              :and-increment (vec (map inc coll))]))
  
  (defn sort-list
    [coll]
    (p :sort (sort (bucket :coll-type coll (vectorise coll)))))


  ; Should be default behaviour, normal sort
  (sort-list (repeatedly 10 #(rand-int 10)))

  ; Will execute sorting twice, bucket testing coll-type
  (bucket-test [:coll-type] (sort-list (repeatedly 10 #(rand-int 10))))

  (bucket-test-force [] {:vectorise :and-increment} (vectorise-named '(1 2 3)))

  ; Will execute sorting three times, due to contained call to vectorise
  (bucket-test [:coll-type :vectorise]
               (sort-list (repeatedly 10 #(rand-int 10))))

  ; Compare performance of three versions
  (bucket-test [:coll-type :vectorise]
    (profile :info :sorting (sort-list (shuffle (range 10))))))  