(ns build
  (:require [clojure.java.io :as io]
            [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'com.blockether/vis)

;; Single source of truth for the version string. Lives at the repo
;; root so every package + the runtime can agree without duplicating
;; the value. The `VERSION` env var (set by CI) overrides for releases;
;; the file gives non-release builds a deterministic `-SNAPSHOT` tag.
(def version
  (let [v (System/getenv "VERSION")]
    (if (and v (.startsWith v "v"))
      (subs v 1)
      (or v (str (.trim (slurp "VERSION")) "-SNAPSHOT")))))

(def class-dir "target/classes")
(def jar-file (format "target/%s.jar" (name lib)))
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"}))

(defn- write-version-resource!
  "Embed the resolved version string into the jar as
   `META-INF/vis/VERSION` so `com.blockether.vis.config/version` can
   read it via `(io/resource …)` at runtime."
  []
  (let [target (io/file class-dir "META-INF/vis/VERSION")]
    (io/make-parents target)
    (spit target version)))

(defn jar [_]
  (clean nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                :src-dirs ["src"]
                :pom-data [[:description "vis — RLM-inspired coding agent. Code-eval over tool-calls, O(1) context, secure-by-default SCI sandbox."]
                           [:url "https://github.com/Blockether/vis"]
                           [:licenses
                            [:license
                             [:name "Apache License, Version 2.0"]
                             [:url "https://www.apache.org/licenses/LICENSE-2.0"]]]]})
  (b/copy-dir {:src-dirs ["src"]
               :target-dir class-dir})
  (write-version-resource!)
  (b/jar {:class-dir class-dir
          :jar-file jar-file})
  (println "Built:" jar-file "version:" version))

(defn deploy [_]
  (jar nil)
  (dd/deploy {:installer :remote
              :artifact jar-file
              :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))

(defn install
  "Install the built jar into the local Maven repository (~/.m2)."
  [_]
  (jar nil)
  (dd/deploy {:installer :local
              :artifact jar-file
              :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))
