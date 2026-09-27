(ns com.blockether.vis.tui.packaging-test
  "The terminal app ships as an independent gateway client with its own native-image metadata."
  (:require [charred.api :as charred]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- clojure-sources
  [root]
  (->> (file-seq root)
       (filter #(and (.isFile ^java.io.File %)
                     (str/ends-with? (.getName ^java.io.File %) ".clj")))))

(defdescribe independent-app-test
             (it "depends on the wire contract, not the Vis engine"
                 (let [deps (:deps (edn/read-string (slurp "deps.edn")))]
                   (expect (contains? deps 'com.blockether/vis-contract))
                   (expect (not (contains? deps 'com.blockether/vis)))))
             (it "keeps every engine and extension namespace outside the app"
                 (let [sources (clojure-sources (io/file "src"))]
                   (expect (seq sources))
                   (doseq [source sources
                           forbidden ["com.blockether.vis.core" "com.blockether.vis.internal"
                                      "com.blockether.vis.ext"]]

                     (expect (not (str/includes? (slurp source) forbidden))
                             (str source " imports " forbidden))))))

(defdescribe
  native-resize-registration-test
  ;; UnixLikeTTYTerminal catches Throwable around this reflective registration.
  ;; Missing metadata leaves native terminals at their startup dimensions.
  (it "registers Lanterna's WINCH handler and its dynamic proxy"
      (let [resource
            (io/resource "META-INF/native-image/com.blockether/vis-tui/reachability-metadata.json")

            entries
            (get (charred/read-json (slurp resource)) "reflection")

            signal
            (filter #(= "sun.misc.Signal" (get % "type")) entries)

            members
            (set (for [entry
                       signal

                       method
                       (get entry "methods")]

                   [(get method "name") (vec (get method "parameterTypes"))]))]

        (expect (contains? members ["<init>" ["java.lang.String"]]))
        (expect (contains? members ["handle" ["sun.misc.Signal" "sun.misc.SignalHandler"]]))
        (expect (some #(true? (get % "allDeclaredMethods")) signal))
        (expect (some #(= {"proxy" ["sun.misc.SignalHandler"]} (get % "type")) entries)))))
