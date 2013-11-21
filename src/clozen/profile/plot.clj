(ns ^{:author "Zenna Tavares"
      :doc "Plot Profiling Tools"}
  clozen.profile.plot
  (:require [clozen.helpers :refer :all]
            [clozen.profile.scaling :refer :all]
            [clojure.java.shell :only [sh]]))

(defn bucket-scaling-plot
  "A whole bunch of twisting the data around into a form suitable for
   matplotlib

   Outputs data in form
   [[bucket-name-0 inspects-name-0 [x-axis-vals] [y-axis-vals]
    [bucket-name-0 inspects-name-1 [x-axis-vals] [y-axis-vals]
    ...
    [bucket-name-1 inspects-name-0 [x-axis-vals] [y-axis-vals] ...]"
  [bucket-results & inspects]
  (for [{bucket :bucket result :result} bucket-results
               :let [profile-results
                    (apply (partial scale-indep-input result)
                           inspects)]]
    (mapv #(vector
          ;Name here
            ; Which bucket is this, which
            bucket
            (clojure.string/join " " (nth inspects %2))
            (vec (flatten (keys profile-results)))
            %1)
          (flatten-scaling-data profile-results)
          (range (count (flatten-scaling-data profile-results))))))

(comment
  (require '[clozen.profile.plot :refer :all])
  (require '[clozen.profile.scaling :refer :all])
  (require '[taoensso.timbre.profiling :as profiling :refer (p o profile)])
  (require '[clozen.profile.bucket :refer :all])

  (defn special-sort
    [coll]
    (bucket :sort (sort coll) (do (dotimes [_ 100000000] nil)
                                  (sort coll))))
  (defn rand-vector
    [n]
    (vec (repeatedly n rand)))


  (def bucket-data
    (bucket-test [:sort]
      (scaling special-sort
               rand-vector
               (map vector (take 3 (powers-of-n 2)))
               2)))

  (bucket-scaling-plot bucket-data [:taoensso.timbre.profiling/whole :max]
                                   [:taoensso.timbre.profiling/whole :count]))