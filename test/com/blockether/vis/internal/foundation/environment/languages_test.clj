(ns com.blockether.vis.internal.foundation.environment.languages-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.foundation.environment.languages :as languages]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- make-tmp-dir
  ^java.io.File []
  (let [path (Files/createTempDirectory "vis-env-test-" (into-array FileAttribute []))]
    (.toFile path)))

(defn- spit-rel
  [^java.io.File root rel content]
  (let [f (io/file root rel)]
    (.mkdirs (.getParentFile f))
    (spit f content)))

(defdescribe
  languages-scan-test
  (it "counts files per language and picks a primary"
      (let [root (make-tmp-dir)]
        (try (spit-rel root "src/main.clj" "(ns foo) (defn x [] 1)")
             (spit-rel root "src/util.clj" "(ns bar) (defn y [] 2)")
             (spit-rel root "README.md" "# Hello")
             (spit-rel root "deps.edn" "{:deps {}}")
             (spit-rel root "scripts/run.sh" "#!/bin/sh\necho hi")
             (let [scan (languages/scan root)
                   langs-by-name (into {} (map (juxt :language identity) (:languages scan)))]

               (expect (some? (langs-by-name "clojure")))
               (expect (= 2 (:files (langs-by-name "clojure"))))
               (expect (some? (langs-by-name "markdown")))
               (expect (some? (langs-by-name "edn")))
               (expect (some? (langs-by-name "shell")))
               (expect (some? (:primary scan)))
               (expect (string? (:primary scan)))
               (expect (false? (:truncated? scan)))
               (expect (pos? (long (:total-bytes scan))))
               (expect (= 5 (long (:total-files scan)))))
             (finally (when (.exists root)
                        (doseq [^java.io.File f (reverse (file-seq root))]
                          (.delete f)))))))
  (it "skips known non-source directories"
      (let [root (make-tmp-dir)]
        (try (spit-rel root "src/keep.clj" "(ns keep)")
             (spit-rel root "node_modules/skip.js" "skip")
             (spit-rel root "target/skip.clj" "skip")
             (spit-rel root ".git/config" "skip")
             (let [scan (languages/scan root)]
               (expect (= 1 (long (:total-files scan))))
               (expect (= "clojure" (:primary scan))))
             (finally (when (.exists root)
                        (doseq [^java.io.File f (reverse (file-seq root))]
                          (.delete f)))))))
  (it "excludes minified bundles and lockfiles, picks primary by file count"
      (let [root (make-tmp-dir)]
        (try
          ;; A handful of source .clj files
          (spit-rel root "src/a.clj" "(ns a)")
          (spit-rel root "src/b.clj" "(ns b)")
          (spit-rel root "src/c.clj" "(ns c)")
          ;; A single huge minified JS bundle and a lockfile -
          ;; would dominate by bytes if not excluded.
          (spit-rel root "docs/app.min.js" (apply str (repeat 50000 "abcdefgh")))
          (spit-rel root "package-lock.json" "{\"lockfileVersion\":3}")
          (let [scan (languages/scan root)
                langs-by-name (into {} (map (juxt :language identity) (:languages scan)))]

            (expect (= "clojure" (:primary scan)))
            (expect (nil? (langs-by-name "javascript")))
            (expect (nil? (langs-by-name "json"))))
          (finally (when (.exists root)
                     (doseq [^java.io.File f (reverse (file-seq root))]
                       (.delete f)))))))
  (it "handles an empty directory"
      (let [root (make-tmp-dir)]
        (try (let [scan (languages/scan root)]
               (expect (= 0 (long (:total-files scan))))
               (expect (nil? (:primary scan)))
               (expect (= [] (:languages scan))))
             (finally (.delete root))))))
