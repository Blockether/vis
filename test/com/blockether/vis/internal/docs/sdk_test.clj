(ns com.blockether.vis.internal.docs.sdk-test
  "Compile the published Java example against the real JVM API without a gateway."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io ByteArrayOutputStream]
           [javax.tools ToolProvider]))

(defdescribe
  java-sdk-guide-compiles-test
  (it
    "java sdk guide compiles"
    (let [document
          (slurp (io/resource "vis-docs/jvm-sdk.md"))

          source
          (second (re-find #"(?s)```java\n// VisExample.java\n(.*?)\n```" document))

          directory
          (fs/create-temp-dir {:prefix "vis-java-guide-"})

          source-file
          (fs/file directory "VisExample.java")

          compiler
          (ToolProvider/getSystemJavaCompiler)]

      (try (expect (some? source) "The guide must keep its runnable Java example")
           (expect (some? compiler) "The JVM guide requires a JDK, not a JRE")
           (when (and source compiler)
             (spit source-file source)
             (with-open [errors (ByteArrayOutputStream.)]
               (expect (= {:exit 0 :diagnostics ""}
                          {:exit (.run compiler
                                       nil
                                       nil
                                       errors
                                       (into-array String
                                                   ["-classpath"
                                                    (System/getProperty "java.class.path") "-d"
                                                    (str directory) (str source-file)]))
                           :diagnostics (.toString errors "UTF-8")})))
             (doseq [[_ function-name] (re-seq #"api\(\"([^\"]+)\"\)" source)]
               (let [v (ns-resolve 'com.blockether.vis.core (symbol function-name))]
                 (expect (some? v) (str "Missing public API: " function-name))
                 (expect (not (:private (meta v))) function-name))))
           (finally (fs/delete-tree directory))))))

(defdescribe jvm-sdk-guide-published-dependencies-test
             (it "jvm sdk guide published dependencies"
                 (let [document
                       (slurp (io/resource "vis-docs/jvm-sdk.md"))

                       source
                       (second (re-find #"(?s)```edn\n(.*?)\n```" document))

                       dependencies
                       (when source (edn/read-string source))

                       library
                       (get-in dependencies [:deps 'com.blockether/vis])]

                   (expect (some? source) "The guide must keep its runnable deps.edn example")
                   (expect (= #{:mvn/version} (set (keys library))))
                   (expect (not (str/blank? (:mvn/version library))))
                   (expect (= (:mvn/repos (edn/read-string (slurp "deps.edn")))
                              (:mvn/repos dependencies))
                           "tools.deps does not inherit Maven repositories from dependencies"))))

(defdescribe
  native-build-guide-is-for-jvm-extension-authors-test
  (it "native build guide is for jvm extension authors"
      (let [document
            (slurp (io/resource "vis-docs/jvm-native-image.md"))

            site
            (edn/read-string (slurp (io/resource "vis-docs/site.edn")))

            sections
            (for [{:keys [section pages]}
                  (:nav site)

                  :when (some #(= "jvm-native-image" (:page %)) pages)]

              section)]

        (expect (= ["Extensions"] (vec sections)))
        (expect (str/starts-with? document "# Native builds for Java and Clojure extensions"))
        (expect (str/includes? document "not a drop-in JAR plugin system"))
        (expect (str/includes? document "You do **not** need a native build to use the Python SDK"))
        (expect (str/includes? document "resources/META-INF/vis/manifest.edn"))
        (expect (str/includes? document "clojure -M:test-native")))))
