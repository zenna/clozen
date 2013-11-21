(ns ^{:author "Zenna Tavares"
      :doc "Comparative Testing"}
  clozen.profile.bucket
  (:require [clozen.helpers :refer :all]))

;; TODO
; Integrate this with profile and scaling
; Profile stores its data in an object called pdata-stats
; I need to change profile such that it checks for 

(defonce* bucket-config
   "This map atom controls store the state for bucket tetsing"
   (atom {:levels {}
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

(defmacro bucket
  "Designate what should be compared in Bucket testing

   Takes a list of arguments and will try all the alternatives for `whatever
   profiling is going on.

   If bucket is not enabled it will simply use the first argument as default
   "
   [bucket-name & terms]
   `(if-let [bucket-pos# (bucket-level ~bucket-name)]
      (do
        (update-seen-bucket! ~bucket-name)
        (when (= bucket-pos# ~(dec (count terms)))
              (remove-seen-bucket! ~bucket-name))
        (case bucket-pos#
        ~@(reduce concat
            (for [i (range (count terms))]
              (list i (nth terms i))))))

      ~(first terms))) ; Bucket not enabled

(defmacro bucket-test
  "Execute the bucket testing
   Returns a vector of all the return values"
  [bucket-names & body]
  `(do
    (apply enable-buckets! ~bucket-names)
    
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
        (empty-seen-buckets!)))))

(comment
  (require '[clozen.profile.bucket :refer :all])

  (macroexpand '(sort (bucket :coll-type coll (vectorise coll))))
  (macroexpand '(bucket-test [:vectorise] (vectorise (repeatedly 10 #(rand-int 10)))))

  (require '[taoensso.timbre :as timbre
                             :refer (trace debug info warn
                                     error fatal spy with-log-level)])
  (require '[taoensso.timbre.profiling :as profiling :refer (p o profile)])

  ; Using bucket with profile
  (defn vectorise
    [coll]
    (bucket :vectorise (vec coll) (vec (map inc coll))))
  
  (defn sort-list
    [coll]
    (p :sort (sort (bucket :coll-type coll (vectorise coll)))))


  ; Should be default behaviour, normal sort
  (sort-list (repeatedly 10 #(rand-int 10)))

  ; Will execute sorting twice, bucket testing coll-type
  (bucket-test [:coll-type] (sort-list (repeatedly 10 #(rand-int 10))))

  ; Will execute sorting three times, due to contained call to vectorise
  (bucket-test [:coll-type :vectorise]
               (sort-list (repeatedly 10 #(rand-int 10))))

  ; Compare performance of three versions
  (bucket-test [:coll-type :vectorise]
    (profile :info :sorting (sort-list (shuffle (range 10))))))  