(ns com.blockether.vis.internal.extension.native-preload
  "Build-time reachability for everything the engine loads DYNAMICALLY.

   The manifest resolves an entrypoint with `requiring-resolve`, a pack reaches
   its own optional half the same way (voice's ASR), and a
   native image contains only what the BUILDER saw. A namespace nobody loads
   while the image is built therefore has no class in the binary, and the first
   command that wants it dies with \"Could not locate ...__init.class on
   classpath\" - on a binary whose JVM run is perfectly healthy. Loading them
   here puts them in `clojure.lang.RT`'s namespace map, which IS part of the
   image heap, so the same resolve at run time finds them already loaded.

   NOTHING requires this namespace. `build.clj` names its `__init` class in
   `--initialize-at-build-time`, and that is what makes the builder load it -
   deliberately outside any other namespace's load, because an entrypoint that
   requires the engine back is a cyclic load anywhere else. A JVM run never
   loads this file, so the manifest stays as lazy as it always was.

   The list is DERIVED, never written down: the manifest's own entrypoints
   first, then every namespace this distribution compiled. Anything else is a
   dependency's build-time preload chain reaching them by accident - dropping
   one jar from the native classpath then silently unbinds a whole extension
   (see `native-classpath` in build.clj)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.extension.manifest :as manifest]))

(def ^:private compiled-package
  "The one package tree this distribution compiles: core and every internal
   namespace. A dependency's namespaces are reachable through their own code and
   are none of our business."
  ["com" "blockether" "vis"])

(defn- path->namespace
  "`com/blockether/vis/internal/provider/openai__init.class` -> the namespace symbol."
  [^String relative]
  (-> relative
      (str/replace #"__init\.class$" "")
      (str/replace java.io.File/separator ".")
      (str/replace "_" "-")
      symbol))

(defn- this-loader
  "The loader that loaded THIS namespace - the image builder's own class loader
   during a native build, where `java.class.path` names the BUILDER's jars and
   not the image's classpath at all."
  ^ClassLoader []
  (.getClassLoader (class this-loader)))

(defn compiled-namespaces
  "Every namespace this distribution AOT-compiled, read off the class path the
   image builder was handed. `<ns>__init.class` is the only class Clojure emits
   per namespace, so the tree IS the list and nothing has to be maintained."
  []
  (->> (enumeration-seq (.getResources (this-loader) (str/join "/" compiled-package)))
       (keep (fn [^java.net.URL url]
               (when (= "file" (.getProtocol url))
                 (let [tree
                       (io/file (.toURI url))

                       root
                       (nth (iterate #(.getParentFile ^java.io.File %) tree)
                            (count compiled-package))]

                   (when (.isDirectory tree) [root tree])))))
       (mapcat (fn [[^java.io.File root ^java.io.File tree]]
                 (->> (file-seq tree)
                      (filter #(and (.isFile ^java.io.File %)
                                    (str/ends-with? (.getName ^java.io.File %) "__init.class")))
                      (map #(path->namespace (str (.relativize (.toPath root)
                                                               (.toPath ^java.io.File %))))))))
       distinct
       sort))

(defn preload!
  "Load every namespace the image must keep. Any failure aborts the build: stepping
   over one produces an apparently successful image that cannot initialize the
   affected extension and usually poisons every namespace that requires it."
  []
  (let [targets (distinct (concat (map (comp symbol namespace) (manifest/initializers))
                                  (compiled-namespaces)))]
    (doseq [target targets]
      (try (require target)
           (catch Throwable t
             (throw
               (ex-info (str "Native-image could not preload " target) {:namespace target} t)))))
    (println "[vis] native-image preload:" (count targets) "namespaces loaded")))

(preload!)
