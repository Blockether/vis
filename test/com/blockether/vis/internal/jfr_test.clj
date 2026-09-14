(ns com.blockether.vis.internal.jfr-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.jfr :as jfr]
            [com.blockether.vis.internal.paths :as paths]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- with-logs
  [f]
  (let [dir (.toFile (Files/createTempDirectory "vis-jfr-test-" (make-array FileAttribute 0)))]
    (try (with-redefs [paths/logs-dir #(.getPath dir)]
           (f dir))
         (finally (doseq [^File file (reverse (file-seq dir))]
                    (Files/deleteIfExists (.toPath file)))))))

(defdescribe recording-path-test
             (it "places each role's recording under its UTC date"
                 (with-logs (fn [dir]
                              (let [^File file (#'jfr/recording-file "gateway")]
                                (expect (= dir (.getParentFile (.getParentFile file))))
                                (expect (some? (re-matches #"\d{4}-\d{2}-\d{2}"
                                                           (.getName (.getParentFile file)))))
                                (expect (some? (re-matches #"vis-gateway-\d+-\d{8}-\d{6}\.jfr"
                                                           (.getName file)))))))))

(defdescribe
  recording-retention-test
  (it
    "keeps the newest six recordings across dates and skips symlinks"
    (with-logs
      (fn [dir]
        (let [old
              (io/file dir "2026-09-01")

              new
              (io/file dir "2026-09-02")

              unrelated
              (io/file new "notes.txt")

              external
              (io/file dir "external")]

          (.mkdirs old)
          (.mkdirs new)
          (.mkdirs external)
          (spit unrelated "keep")
          (spit (io/file external "vis-external.jfr") "keep")
          (doseq [i (range 8)]
            (let [file (io/file (if (< i 4) old new) (str "vis-test-" i ".jfr"))]
              (spit file "recording")
              (.setLastModified file (+ 1000 i))))
          (Files/createSymbolicLink (.toPath (io/file new "vis-link.jfr"))
                                    (.toPath (io/file external "vis-external.jfr"))
                                    (make-array FileAttribute 0))
          (Files/createSymbolicLink (.toPath (io/file dir "2026-09-03"))
                                    (.toPath external)
                                    (make-array FileAttribute 0))
          (#'jfr/prune-old-recordings!)
          (expect (not (.exists (io/file old "vis-test-0.jfr"))))
          (expect (not (.exists (io/file old "vis-test-1.jfr"))))
          (doseq [i (range 2 8)]
            (expect (.isFile (io/file (if (< i 4) old new) (str "vis-test-" i ".jfr")))))
          (expect (.isFile unrelated))
          (expect (.isFile (io/file external "vis-external.jfr")))
          (expect (Files/isSymbolicLink (.toPath (io/file new "vis-link.jfr"))))
          (Files/delete (.toPath (io/file dir "2026-09-03")))
          (Files/delete (.toPath (io/file new "vis-link.jfr"))))))))
