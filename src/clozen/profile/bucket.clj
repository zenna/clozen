(ns ^{:author "Zenna Tavares"
      :doc "Comparative Testing"}
  clozen.profile.bucket
  (:require [clozen.helpers :refer :all])
  (:require [taoensso.timbre :as timbre
                             :refer (trace debug info warn
                                     error fatal spy with-log-level)])
  (:require [taoensso.timbre.profiling :as profiling :refer (p o profile)]))


; I want a profiling system that will make it easy to
; 1. compare the performance of various versions of some code
; These versions may be - different functions in the source
; Different functions in the repository
; Different doing or not doing something

(utils/defonce* bucket-config
  "This map atom controls how bucket testing work.

   "
   (atom {:levels {}
          :seen-buckets []}))

; (defn bucket-enabled?
;   "Returns true iff bucket testing is enabled.
;    If bucket testing is not enabled, (bucket a b c d) will default to a"
;    [name]
;    (bucket-config :) name)

; (defn zipmap-f
;   "Like zipmap except instead of vals you provide
;    (f index key)

;    (zipmap-f '[a b c] #())"
;   [coll f]
;   (zipmap coll (map f (range (count coll)) coll)))

(defn zipmap-count
  "Like zipmap except instead of providing vals you provide a function
   which is applied to the count of the keys.

   Convenient for things like:
   (zipmap-c '[a b c] range) => {c 2, b 1, a 0}
   (zipmap-c '[a b c] zeros) => {c 0, b 0, a 0}"
  [coll f]
  (zipmap coll (f (count coll))))

(defn enable-buckets!
  [& names]
  (swap! bucket-config #(merge % {:levels (zipmap-count names zeros)})))

(defn disable-buckets!
  [& names]
  (swap! bucket-config
        (fn [config]
          (update-in config [:levels]
                     #(apply (partial dissoc %) names)))))

(defn empty-seen-buckets!
  []
  (swap! bucket-config #(assoc % :seen-buckets [])))

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
        (println @bucket-config)
        (update-seen-bucket! ~bucket-name)
        (println "pos" bucket-pos# "n terms" ~(count terms))
        (when (= bucket-pos# ~(dec (count terms)))
              (remove-seen-bucket! ~bucket-name))
        (case bucket-pos#
        ~@(reduce concat
            (for [i (range (count terms))]
              (list i (nth terms i))))))

      ~(first terms))) ; Bucket not enabled

(defmacro bucket-test
  "Execute the bucket testing

   ; Enable Testing for named bucket
   
   ; Dealing with recursion and nesting.
   Imagine I have a program which recursively calls sort-list many times.
   If I bucket test: I don't want on different iterations to use different versions.  What this means isthat only the bucket-tester can increment the
     counter.  To get the number of terms, possibly a macro could effect it, but I'm not sure that's possible.  Otherwise

     ; What about nested.
     So I've got multiple functions leading to the same nested function.
     What do I want in this case.
     on the one hand these seems similar to the recursion problem, and I should
     make sure only bucket-test can update the bucket being tested.

     On the other hand, if I have buckets within a bucket
     Then I want to test sort with coll, and when i choose vectorise I want
     to test with the two buckets within

    ; Disable Testing for named bucket

    ;Returns a map detailing the bucket choices made and the result
    {:bucket-choices
      [{:bucket-name :coll-type :bucket-number 0}
       {:bucket-name :vectorise :bucket number 1}]
     :result [1 2 3 4 5]}

     How will this tie in with profiling?
     Well profiling does not return any data, I used an atom with scaling
     to store that information for later use
     Bucket does not have access to this information so either I have to expose this information to bucket-test or I have to expose bucket-test to profiling and modify profiling.

     The simplest way seems to expose current bucket and let profiling add that
     to its pdata-stats.

     Hypothesis: Increment the most inner-bucket
     Q: How to get the bucket details out, i.e. which was chosen
     A: (bucket will createa list :current-bucket [])

     Q: What about mutually recurseive
     A: Well there's a question of what is desired.  Because you could argue it both ways.  If I choose bucket A then bucket B then bucket A then


    In summary this is what should happen:
    when 
    (bucket :name a b c)
    is encountered it should check the global atom for its name
    it will get data back of the form: You should be at level X.
    Originally all these Xs will bet set to zero by bucket-test.
    (bucket will evaluate its Xth argument)
    It will also write to the global thing that:
    It has N terms, and append to a list current bucket
    [:my-bucket]
    A bucket will do this only write to the current bucket list once.

    bucket-test will evaluate look at the current bucket and increment
    from the last downwards, the bucket ids

   "
   (defmacro bucket-test
  [bucket-names & body]
  `(do
    (apply enable-buckets! ~bucket-names)
    
    (try
      (loop [results# []]
        (let [result# (do ~@body)]
          (cond
            (buckets-left?)
            (conj results# result#)

            :else
            (do
              (increment-levels!)
              (recur (conj results# result#))))))
      (finally
        (apply disable-buckets! ~bucket-names)
        (empty-seen-buckets!)))))

(comment
  
  (macroexpand '(sort (bucket :coll-type coll (vectorise coll))))
  (macroexpand '(bucket-test [:vectorise] (vectorise (repeatedly 10 #(rand-int 10)))))

  ; Using bucket with profile
  (defn vectorise
    [coll]
    (bucket :vectorise (vec coll) (vec (map inc coll))))
  
  (defn sort-list
    [coll]
    (sort (bucket :coll-type coll (vectorise coll))))


  ; Should be default behaviour, normal sort
  (sort-list (repeatedly 10 #(rand-int 10)))

  ; Will execute sorting twice, bucket testing coll-type
  (bucket-test [:coll-type] (sort-list (repeatedly 10 #(rand-int 10))))

  ; Will execute sorting three times, due to contained call to vectorise
  (bucket-test [:coll-type :vectorise]
               (sort-list (repeatedly 10 #(rand-int 10))))

  ; Compare performance of three versions
  (bucket-test [:coll-type :vectorise]
    (profile :info :sorting (sort-list (shuffle (range 10000))))))