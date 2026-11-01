;; Guards the prep marker of packages/vis-tui-render, the Java passes of the TUI renderer.
(ns com.blockether.vis.tui.render-package-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files)
           (java.security MessageDigest)))

(def ^:private package-dir (io/file "../../packages/vis-tui-render"))

(defn- relative-path
  ^String [^File root ^File f]
  (str/replace (str (.relativize (.toPath root) (.toPath f))) File/separator "/"))

(defn- source-digest
  "The name of the package's prep marker: SHA-256 over every file under `src/java` in
   path order, each as its relative path, a NUL byte, its content and a NUL byte; the
   first 16 hex digits."
  []
  (let [root
        (io/file package-dir "src/java")

        digest
        (MessageDigest/getInstance "SHA-256")]

    (doseq [^File f (sort-by #(relative-path root %) (filter #(.isFile ^File %) (file-seq root)))]
      (.update digest (.getBytes (relative-path root f) StandardCharsets/UTF_8))
      (.update digest (byte-array 1))
      (.update digest (Files/readAllBytes (.toPath f)))
      (.update digest (byte-array 1)))
    (subs (format "%064x" (BigInteger. 1 (.digest digest))) 0 16)))

(defdescribe prep-marker-test
  (it "names the prep marker after the Java sources, so a changed source compiles again"
      (let [expected
            (str "target/prepared/" (source-digest))

            ensure
            (-> (io/file package-dir "deps.edn")
                slurp
                edn/read-string
                :deps/prep-lib
                :ensure)]

        (expect (= expected ensure)
                (str "Set :ensure in packages/vis-tui-render/deps.edn to \""
                     expected
                     "\"")))))
