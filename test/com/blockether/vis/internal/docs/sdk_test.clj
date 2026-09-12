(ns com.blockether.vis.internal.docs.sdk-test
  "Compile the published Java example against the real JVM API without a gateway."
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
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
