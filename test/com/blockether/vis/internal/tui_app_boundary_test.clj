(ns com.blockether.vis.internal.tui-app-boundary-test
  "The terminal client is an independent gateway consumer, never an engine extension."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private app-root (io/file "apps/vis-tui"))
(def ^:private old-extension-root (io/file "extensions/channels/vis-channel-tui"))

(defn- clojure-sources
  [root]
  (when (.isDirectory ^java.io.File root)
    (->> (file-seq root)
         (filter #(and (.isFile ^java.io.File %)
                       (str/ends-with? (.getName ^java.io.File %) ".clj"))))))

(defdescribe
  independent-tui-app-test
  (it "ships as an app instead of an engine extension"
      (expect (.isFile (io/file app-root "deps.edn")))
      (expect (not (.exists old-extension-root)))
      (expect (not (str/includes? (slurp "resources/META-INF/vis/manifest.edn") "channel-tui")))
      (expect (not (str/includes? (slurp "deps.edn") "vis-channel-tui"))))
  (it "depends on the wire contract, not the Vis engine"
      (let [deps-file
            (io/file app-root "deps.edn")

            deps
            (when (.isFile deps-file) (:deps (edn/read-string (slurp deps-file))))]

        (expect (contains? deps 'com.blockether/vis-contract))
        (expect (not (contains? deps 'com.blockether/vis)))))
  (it "keeps every engine and extension namespace outside the app"
      (doseq [source
              (clojure-sources (io/file app-root "src"))

              forbidden
              ["com.blockether.vis.core" "com.blockether.vis.internal" "com.blockether.vis.ext"]]

        (expect (not (str/includes? (slurp source) forbidden))
                (str source " imports " forbidden)))))
