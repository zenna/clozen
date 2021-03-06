(ns ^{:doc "Debug Helpers"
      :author "Zenna Tavares"}
  clozen.debug
  (:require [fipp.edn :refer (pprint) :rename {pprint fipp}]))

(defmacro dbg
  ([x] `(let [x# ~x] (println "dbg:" '~x "=") (fipp x#) x#))
  ([f x] `(let [x# ~x] (println "dbg-f:" '~x "=" (~f x#)) x#)))

(defmacro asrt [pred? x]
  `(let [x# ~x] (if (~pred? x#)
                    x#
                    (throw (Throwable. "Assertion failed, smelly face")))))