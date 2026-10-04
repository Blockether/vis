(ns com.blockether.vis.internal.docs.sdk-test
  "Check that the native build guide stays a guide for JVM extension authors."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]))

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
