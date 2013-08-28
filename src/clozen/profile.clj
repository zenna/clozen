(ns ^{:author "Zenna Tavares"
      :doc "Profiling Tools"}
  clozen.profile
  (:use clozen.helpers)
  (:require [taoensso.timbre :as timbre
                        :refer (trace debug info warn error fatal spy with-log-level)])
  (:require [taoensso.timbre.profiling :as profiling :refer (p o profile)]))

; Profiling Data
(defn powers-of-n
  "Lazy sequence of increasing powers of n"
  [n]
  (iterate (partial * n) 1))

(defn scaling
  "Scaling performs scale testing on increased input size
  f: the function to be tested
  input-gen: generates inputs for f, e.g. gen random vector
  inputs: sequence of inputs to input-gen, e.g. length of vector
  n-samples: number of samples for each input size"
  [f input-gen inputs n-samples]
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
           ; (println input)
           (doall (repeatedly n-samples
                       #(profile :info input
                                 (p :whole (f (apply input-gen input))))))))
    @results))

(defn scale-indep-input
  "Extract data from results map (for plotting perhaps)"
  [results & prof-keys]
  (reduce-kv
    (fn [pass-map key val]
      (assoc pass-map key (mapv #(get-in val %) prof-keys)))
    {}
    results))

(defn flatten-input
  [data]
  "Data from scale indep input"
  (mapv 
    (fn [i] (vec (reduce concat (mapv #(nth % i) (vals data)))))
    (range (count (first (vals data))))))


;Possible questions
; How does the runtime scale with the number of dimensions/number of boxes
; How does runtime scale with number of output boxes
; How does number of calls to expand scale with number of output boxes

(comment
  (defn rand-vector
    [n]
    (vec (repeatedly n rand)))

  (scaling #(profile :info :sort-time sort) rand-vector (take 20 powers-of-n) 10)

  (scale-indep-input2 r [:n-boxes :mean] [:expand :count]))

; (defn -main []
;   (scaling sort rand-vector (mapv vector (take 5 (powers-of-n 2))) 2))

; (defn -main []
;   (profile :info :name (dotimes [n 10] (o :counts count (p :times (range (rand-int 20)))))))