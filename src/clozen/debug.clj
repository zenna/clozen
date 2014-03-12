(ns ^{:doc "Debug Helpers"
      :author "Zenna Tavares"}
  clozen.debug)

(defmacro dbg
  ([x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))
  ([f x] `(let [x# ~x] (println "dbg-f:" '~x "=" (~f x#)) x#)))