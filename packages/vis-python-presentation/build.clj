(ns build
  "Compiles the package's Java sources. Consumers reach `javac` through the
   `:deps/prep-lib` entry in deps.edn when they run `clojure -X:deps prep`."
  (:require [clojure.edn :as edn]
            [clojure.tools.build.api :as b]))

(def ^:private class-dir "target/classes")

(defn javac
  "Compile `src/java` into `target/classes` for Java 21, failing on any lint warning,
   and ship the NOTICE with the classes. The `:ensure` marker from deps.edn is written
   last, so an interrupted build compiles again on the next prep."
  [_]
  (b/delete {:path "target"})
  (b/javac {:basis (b/create-basis {:project "deps.edn"})
            :class-dir class-dir
            :javac-opts ["--release" "21" "-Xlint:all" "-Werror"]
            :src-dirs ["src/java"]})
  (b/copy-file {:src "NOTICE" :target (str class-dir "/META-INF/vis-python-presentation/NOTICE")})
  (b/write-file {:path (-> (slurp "deps.edn")
                           edn/read-string
                           :deps/prep-lib
                           :ensure)
                 :string ""}))
