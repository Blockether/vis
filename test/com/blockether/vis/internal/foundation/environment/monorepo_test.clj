(ns com.blockether.vis.internal.foundation.environment.monorepo-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.foundation.environment.monorepo :as monorepo]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- make-tmp-dir
  ^java.io.File []
  (let [path (Files/createTempDirectory "vis-env-monorepo-" (into-array FileAttribute []))]
    (.toFile path)))

(defn- spit-rel
  [^java.io.File root rel content]
  (let [f (io/file root rel)]
    (.mkdirs (.getParentFile f))
    (spit f content)))

(defn- cleanup
  [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))]
      (.delete f))))

(defdescribe
  monorepo-snapshot-test
  (it "detects polylith shape from extensions/ + packages/"
      (let [root (make-tmp-dir)]
        (try (spit-rel root "deps.edn" "{:deps {}}")
             (spit-rel root "packages/foo/deps.edn" "{:deps {}}")
             (spit-rel root "packages/bar/deps.edn" "{:deps {}}")
             (spit-rel root "extensions/common/baz/deps.edn" "{:deps {}}")
             (let [snap (monorepo/snapshot root)]
               (expect (= "polylith" (:shape snap)))
               (expect (= 3 (long (get-in snap [:totals :clojure])))))
             (finally (cleanup root)))))
  (it "detects submodules from .gitmodules at root"
      (let [root (make-tmp-dir)]
        (try (spit-rel root ".gitmodules" "[submodule \"foo\"]\n  path = foo\n  url = ./foo")
             (spit-rel root "package.json" "{}")
             (spit-rel root "vendor/foo/package.json" "{}")
             (let [snap (monorepo/snapshot root)]
               (expect (= "submodules" (:shape snap))))
             (finally (cleanup root)))))
  (it "returns :shape nil for a single-package repo"
      (let [root (make-tmp-dir)]
        (try (spit-rel root "deps.edn" "{:deps {}}")
             (spit-rel root "src/main.clj" "(ns main)")
             (let [snap (monorepo/snapshot root)]
               (expect (nil? (:shape snap)))
               (expect (or (nil? (:totals snap)) (empty? (:totals snap)))))
             (finally (cleanup root)))))
  (it "detects a workspace from multiple node manifests"
      (let [root (make-tmp-dir)]
        (try (spit-rel root "package.json" "{\"name\":\"root\"}")
             (spit-rel root "packages/a/package.json" "{\"name\":\"a\"}")
             (spit-rel root "packages/b/package.json" "{\"name\":\"b\"}")
             (let [snap (monorepo/snapshot root)]
               (expect (= 2 (long (get-in snap [:totals :node]))))
               (expect (some? (:shape snap))))
             (finally (cleanup root))))))
