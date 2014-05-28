(ns ^{:doc "Helper functions useful for many different problems"
      :author "Zenna Tavares"}
  clozen.helper-macros
  (:require [clojure.set :refer :all]
            [clojure.tools.macro :as macro]
            [clojure.java.io :refer :all]))

;; Syntactic Sugar Macros
(defmacro defonce* ; From taoensso.timbre.utils
  "Like `clojure.core/defonce` but supports optional docstring and attributes
  map for name symbol."
  {:arglists '([name expr])}
  [name & sigs]
  (let [[name [expr]] (macro/name-with-attributes name sigs)]
    `(clojure.core/defonce ~name ~expr)))
