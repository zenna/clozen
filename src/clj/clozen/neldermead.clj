(ns clozen.neldermead
  (:use clozen.helpers)
  (:require [incanter.distributions :as dist]))

;todo
; termination tests - start with just number of iterations
; convert model into cost function
; Figure out ms test

; point [p1 p3 ... pn]
; e.g. point = [10 20]
(defn init-simplex
  "Initialise a simplex anchored on a point"
  [point step-size]
  ; first construct a degenerate simplex, with all points identical
  (loop [simplex [point] dim 0]
    (if (> (count simplex) (count point))
      simplex
      (recur (conj simplex (update-in point [dim] #(+ %1 step-size)))
        (inc dim)))))

; Returns e.g. {:vertex [.2 .3 .4] :cost .35}
(defn nelder-mead
  [cost-func init-point num-iters]
  "Perform parameter optimsation with nelder-mead"
  ; Don't bother optimising if there are no parameters
  (cond (empty? init-point) {:vertex [] :cost (cost-func [])}

  :else
  (let [cost-func (memoize cost-func)
        step-size 20
        ;nelder-mead parameters
        alpha 1.0
        beta 0.5
        gamma 2.0
        delta 0.5
        simplex (init-simplex init-point step-size)]
    (loop [simplex-costs (mapv (fn [vertex] {:vertex vertex :cost (cost-func vertex)}) simplex)
           iter-num 0 translation "NOTHING"]
      
      ; Ordering: find worst, second worst and best point
      ; simplex-costs [{:vertex [.2 .3 .4] :cost .35} {...} ...]
      ; TODO: Check termination conditions first
      (if
        (or (>= iter-num num-iters)
            false)
        (first (vec (sort-by :cost simplex-costs)))
        (let [simplex-costs (vec (sort-by :cost simplex-costs))
              best-vertex (:vertex (nth simplex-costs 0))
              sec-worst-vertex (:vertex (nth simplex-costs (- (count simplex-costs) 2)))
              worst-vertex (:vertex (last simplex-costs))
              worst-index (dec (count simplex-costs))
              ; find centroid of the best side — opposite worst vertex 
              ; c = 1/n * vector sum of vertices
              centroid
                ; either i can destructure vertex-costs, to extract vectors
                ; or i can use original simplex
                (vec-scalar-f *
                  (reduce #(vec-f + %1 %2) (map #(:vertex %1) (remove-nth simplex-costs worst-index)))
                  (/ 1 (dec (count simplex-costs))))
              reflection-vertex
                (vec-f + centroid 
                  (vec-scalar-f * (vec-f - centroid worst-vertex) alpha))
              expansion-vertex
                (vec-f + centroid 
                       (vec-scalar-f * (vec-f - reflection-vertex centroid) gamma))
              out-contraction-vertex
                (vec-f + centroid 
                       (vec-scalar-f * (vec-f - reflection-vertex centroid) beta))
              in-contraction-vertex
                (vec-f + centroid 
                       (vec-scalar-f * (vec-f - worst-vertex centroid) beta))
              pvar (println "nd:Cost" (double (:cost (nth simplex-costs 0))) " ")
              ]
          ; (println translation)
          ; (println "simplex" simplex-costs)
          ; (println "refl" reflection-vertex (cost-func reflection-vertex))
          ; (println "exp" expansion-vertex)
          ; (println "out-c" out-contraction-vertex)
          ; (println "in-c" in-contraction-vertex)
          ; (println "worst" worst-vertex (cost-func worst-vertex))
          ; (println "best" best-vertex (cost-func best-vertex))
          ; (println "sec-worst-vertex" sec-worst-vertex (cost-func sec-worst-vertex))
          ; (println "cost" (cost-func best-vertex))

          ; Reflect: test if cost of reflection point is between best and second worst
          (cond
            (and (< (cost-func reflection-vertex) (cost-func sec-worst-vertex))
                  (<= (cost-func best-vertex) (cost-func reflection-vertex)))
            ; if so swap worst point for reflection point, terminate and recurse 
            (recur (assoc simplex-costs worst-index
              {:vertex reflection-vertex :cost (cost-func reflection-vertex)})
              (inc iter-num)
              "REFLECTED")

            ; Expand: If reflection point is better than best, compute expansion point
            (and (< (cost-func reflection-vertex) (cost-func best-vertex))
                 (< (cost-func expansion-vertex) (cost-func reflection-vertex)))
            
            (recur (assoc simplex-costs worst-index
              {:vertex expansion-vertex :cost (cost-func expansion-vertex)})
              (inc iter-num)
              "EXPANDED")

            ; Expand: If reflection point is better than best, compute expansion point
            (and (< (cost-func reflection-vertex) (cost-func best-vertex))
                 (>= (cost-func expansion-vertex) (cost-func reflection-vertex)))
            
            (recur (assoc simplex-costs worst-index
              {:vertex reflection-vertex :cost (cost-func reflection-vertex)})
              (inc iter-num)
              "EXPANDED2")

            ; Contract Outside: if fs <= fr < fh
            (and (>= (cost-func reflection-vertex) (cost-func sec-worst-vertex))
                 (< (cost-func reflection-vertex) (cost-func worst-vertex))
                 (<= (cost-func out-contraction-vertex) (cost-func reflection-vertex)))

            (recur (assoc simplex-costs worst-index
              {:vertex out-contraction-vertex :cost (cost-func out-contraction-vertex)})
              (inc iter-num)
              "OUTCONTRACTED")

            ; Contract Outside: if fs <= fr < fh
            (and (>= (cost-func reflection-vertex) (cost-func worst-vertex))
                 (< (cost-func in-contraction-vertex) (cost-func worst-vertex)))

            (recur (assoc simplex-costs worst-index
              {:vertex in-contraction-vertex :cost (cost-func in-contraction-vertex)})
              (inc iter-num)
              "IN CONTRACTED")

            ; Otherwise shrink!
            :else 
              (recur (conj
                (map (fn [vertex] (let [shrunk-vertex (vec-f + worst-vertex 
                         (vec-scalar-f * (vec-f - (:vertex vertex) worst-vertex) beta))]
                    {:vertex shrunk-vertex :cost (cost-func shrunk-vertex)}))
                  (remove-nth simplex-costs worst-index))
                (nth simplex-costs worst-index))
                (inc iter-num)
                "SHRANK"))))))))

  ; (defn model-to-cost-func)

(defn nelder-mead-random-restart
  [parameters cost-func num-iters]
  "Use nelder-mead with random init"
  (repeatedly)
  (nelder-mead parameters cost-func (repeatedly (count parameters) rand )))

(defn add-noise-to-mean
  "adds noise in both x y to a polygon"
  [x std]
  (dist/draw (dist/normal-distribution x std)))

(defn nelder-mead-noisy
  [cost-func init-point num-iters]
  "Perform parameter optimsation with nelder-mead with gaussian noise"
  (let [num-vals (count init-point)
        init-stds (vec (repeatedly num-vals #(* (rand) 35)))
        noisy-cost-f (fn [params]
                  (let [[real-params stds] (vec (split-at num-vals params))]
                  (mean
                  (repeatedly 1000 (fn [] 
                                  (cost-func (mapv #(add-noise-to-mean %1 %2)
                                                    real-params stds)))))))]
    (nelder-mead noisy-cost-f (vec (concat init-point init-stds)) num-iters)))  

; (def example-model
;   {:as-lambda
;   (fn [param-map indep-vars]
;     (+ (param-map 'p1) (Math/pow (param-map 'p2) (indep-vars 'x))))

;   :params ['p1 'p2]})

; (def example-model-linear
;   {:as-lambda
;   (fn [param-map indep-vars]
;     ; (println "MODEL!!" param-map indep-vars)
;     (+ (param-map 'p1) (* (param-map 'p2) (indep-vars 'x))))

;   :params ['p1 'p2]})

; (def data (gen-data-uniform line 1 100 10))
; ; (println "DATA!!!" data)

; (def example-cost (mean-sqr-error example-model-linear data))

; (defn -main []
;   ; (example-cost [1.5 10]))
;   (nelder-mead example-cost [5.0 5.0]))
; ; (require 'avalance.neldermead)
; ; (use 'clojure.tools.trace)
; ; (trace-ns 'avalance.neldermead)