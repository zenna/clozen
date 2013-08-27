(ns ^{:author "Zenna Tavares"
      :doc "Profiling Tools"}
  relax.profile
  (:use clozen.helpers)
  (:require [taoensso.timbre :as timbre
                        :refer (trace debug info warn error fatal spy with-log-level)])
  (:require [taoensso.timbre.profiling :as profiling :refer (p profile)]))

; Profiling Data

(timbre/set-config!
 [:appenders :my-appender]
 {:doc       "Hello-world appender"
  :min-level :debug
  :enabled?  true
  :async?    false
  :limit-per-msecs nil ; No rate limit
  :fn (fn [{:keys [ap-config level prefix throwable message profile-stats] :as args}]

    ;Get the function name
    ; {:gen-random-boxes 
    ;   {[1 2] {:pia [1.0 2.0 3.0 :valid-ext [0.5 2.0 3.5]]}
    ;    [1 3] {:pia [1.0 2.0 3.0 :valid-ext [0.5 2.0 3.5]]}}}
        ; (when-not (:my-production-mode? ap-config)
          (println prefix "Hello world! ----" profile-stats "----\n\n" message))})

(defn powers-of-n
  [n]
  (iterate (partial * n) 1))

(defn scaling
  "Scaling performs scale testing on increased input size"
  [f input-gen inputs n-samples]
  (let [results (atom {})
        appender-fn
        (fn [{:keys [ap-config level prefix throwable message profile-stats] :as args}]
          (println "\n" profile-stats "\n")
          (swap! results 
                 (partial merge-with
                   (partial merge-with cons-conj))
                   profile-stats))]
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
           ; (println input)
           (doall (repeatedly n-samples
                       #(profile :info :Arithmetic
                                 (p :input (f (apply input-gen input))))))))
    results))