(ns build
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'com.blockether/vis)
(def version
  (let [v (System/getenv "VERSION")]
    (if (and v (.startsWith v "v"))
      (subs v 1)
      (or v (str (slurp "resources/VERSION") "-SNAPSHOT")))))

(def class-dir "target/classes")
(def jar-file (format "target/%s.jar" (name lib)))
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"}))

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
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
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
