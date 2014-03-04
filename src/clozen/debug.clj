(ns ^{:doc "Debug Helpers"
      :author "Zenna Tavares"}
  clozen.debug)

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))