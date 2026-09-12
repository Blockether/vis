(ns com.blockether.vis.internal.docs.sdk-test
  "Compile the published Java example against the real JVM API without a gateway."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [java.io ByteArrayOutputStream]
           [javax.tools ToolProvider]))

(deftest java-sdk-guide-compiles-test
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

    (try (is (some? source) "The guide must keep its runnable Java example")
         (is (some? compiler) "The JVM guide requires a JDK, not a JRE")
         (when (and source compiler)
           (spit source-file source)
           (with-open [errors (ByteArrayOutputStream.)]
             (is (= {:exit 0 :diagnostics ""}
                    {:exit (.run compiler
                                 nil
                                 nil
                                 errors
                                 (into-array String
                                             ["-classpath" (System/getProperty "java.class.path")
                                              "-d" (str directory) (str source-file)]))
                     :diagnostics (.toString errors "UTF-8")})))
           (doseq [[_ function-name] (re-seq #"api\(\"([^\"]+)\"\)" source)]
             (let [v (ns-resolve 'com.blockether.vis.core (symbol function-name))]
               (is (some? v) (str "Missing public API: " function-name))
               (is (not (:private (meta v))) function-name))))
         (finally (fs/delete-tree directory)))))

(deftest sdk-guides-task-order-test
  (doseq [[page headings] [["python-sdk"
                            ["## Install the SDK" "## Let your program own a private agent"
                             "## Connect to a gateway and run a task"
                             "## Give the agent your functions" "## Continue a conversation"
                             "## Show progress while a turn runs"
                             "## Handle failures and choose a lifecycle"]]
                           ["jvm-sdk"
                            ["## Prepare the JVM classpath" "## Connect from Java"
                             "## Call the same API from Clojure"
                             "## Package a JVM application or a native runtime"]]
                           ["gateway-service"
                            ["## Install the runtime" "## Start a local gateway"
                             "## Connect from another machine" "## Keep it running on Linux"]]
                           ["jvm-native-image"
                            ["## Add and test your JVM capability" "## Build and test the image"
                             "## Package and run your build" "## Native-image configuration"]]]]
    (let [document (slurp (io/resource (str "vis-docs/" page ".md")))
          positions (mapv #(str/index-of document %) headings)]

      (is (every? some? positions) (str page " retains task headings"))
      (when (every? some? positions)
        (is (apply < positions) (str page " presents tasks before advanced details"))))))

(deftest native-build-guide-is-for-jvm-extension-authors-test
  (let [document
        (slurp (io/resource "vis-docs/jvm-native-image.md"))

        site
        (edn/read-string (slurp (io/resource "vis-docs/site.edn")))

        sections
        (for [{:keys [section pages]}
              (:nav site)

              :when (some #(= "jvm-native-image" (:page %)) pages)]

          section)]

    (is (= ["Extensions"] (vec sections)))
    (is (str/starts-with? document "# Native builds for Java and Clojure extensions"))
    (is (str/includes? document "not a drop-in JAR plugin system"))
    (is (str/includes? document "You do **not** need a native build to use the Python SDK"))
    (is (str/includes? document "resources/META-INF/vis/manifest.edn"))
    (is (str/includes? document "clojure -M:test-native"))))
