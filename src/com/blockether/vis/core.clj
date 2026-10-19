(ns com.blockether.vis.core
  "vis - the binary entry.

   `clojure -M:vis` and the native-image build call `-main` through
   `:gen-class`. The commands live in `com.blockether.vis.internal.main`.
   Nothing under `com.blockether.vis.internal` requires this namespace.
   Extensions use the Python SDK `blockether.vis.extension`."
  (:gen-class)
  (:require [com.blockether.vis.internal.main :as binary]))

(defn -main
  "The binary entry; Python workers have their own entrypoint in the runtime."
  [& args]
  (apply binary/-main args))
