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
               (expect (pos? (:total-bytes scan)))
               (expect (= 5 (:total-files scan))))
             (finally (when (.exists root)
                        (doseq [^java.io.File f (reverse (file-seq root))]
                          (.delete f)))))))
  (it "skips known non-source directories"
      (let [root (make-tmp-dir)]
        (try (spit-rel root "src/keep.clj" "(ns keep)")
             (spit-rel root "node_modules/skip.js" "skip")
             (spit-rel root "target/skip.clj" "skip")
             (spit-rel root ".git/config" "skip")
             ;; User report 2026-10-30: the per-interpreter extension environment.
             (spit-rel root ".venv-0123456789ab/lib/skip.py" "skip = 1")
             (let [scan (languages/scan root)]
               (expect (= 1 (:total-files scan)))
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
               (expect (= 0 (:total-files scan)))
               (expect (nil? (:primary scan)))
               (expect (= [] (:languages scan))))
             (finally (.delete root)))))
  (it "reads a given git file list and never walks unlisted build output (#422)"
      (let [root (make-tmp-dir)]
        (try (spit-rel root "src/a.clj" "(ns a)")
             (spit-rel root "src/b.clj" "(ns b)")
             (spit-rel root "README.md" "# Hello")
             (doseq [i (range 3)]
               (spit-rel root (str "ios/DerivedData/gen" i ".py") "x = 1"))
             (spit-rel root "node_modules/skip.js" "skip")
             (let [listed ["src/a.clj" "src/b.clj" "README.md" "node_modules/skip.js" "gone.clj"]
                   scan (languages/scan root {:files listed})
                   langs-by-name (into {} (map (juxt :language identity) (:languages scan)))]

               (expect (= "clojure" (:primary scan)))
               (expect (= 3 (:total-files scan)))
               (expect (nil? (langs-by-name "python")))
               (expect (nil? (langs-by-name "javascript")))
               (expect (false? (:truncated? scan))))
             (finally (when (.exists root)
                        (doseq [^java.io.File f (reverse (file-seq root))]
                          (.delete f)))))))
  (it "gives the walk result when the list names every file"
      (let [root (make-tmp-dir)]
        (try (spit-rel root "src/a.clj" "(ns a)")
             (spit-rel root "web/app.ts" "export const a = 1")
             (spit-rel root "docs/guide.md" "# Guide")
             (spit-rel root "target/skip.clj" "skip")
             (let [listed ["docs/guide.md" "src/a.clj" "target/skip.clj" "web/app.ts"]
                   walked (languages/scan root)
                   given (languages/scan root {:files listed})]

               (expect (= (:languages walked) (:languages given)))
               (expect (= (:total-files walked) (:total-files given))))
             (finally (when (.exists root)
                        (doseq [^java.io.File f (reverse (file-seq root))]
                          (.delete f)))))))
  (it "stops at max-files with a given list"
      (let [root (make-tmp-dir)]
        (try (doseq [i (range 5)]
               (spit-rel root (str "src/f" i ".clj") "(ns f)"))
             (let [scan (languages/scan root
                                        {:files (mapv #(str "src/f" % ".clj") (range 5))
                                         :max-files 2})]
               (expect (true? (:truncated? scan)))
               (expect (= 2 (:total-files scan))))
             (finally (when (.exists root)
                        (doseq [^java.io.File f (reverse (file-seq root))]
                          (.delete f))))))))
