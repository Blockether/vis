(ns com.blockether.vis.internal.foundation.environment.repositories-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.environment.repositories :as repositories]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- make-tmp-dir
  ^java.io.File []
  (let [path (Files/createTempDirectory "vis-env-repositories-" (into-array FileAttribute []))]
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

(defn- mark-repo! [^java.io.File dir] (.mkdirs (io/file dir ".git")))

(defdescribe repositories-snapshot-test
             (it "returns an empty repository list outside Git worktrees"
                 (let [root (make-tmp-dir)]
                   (try (spit-rel root "README.md" "not git")
                        (let [snap (repositories/snapshot root)]
                          (expect (= 0 (:count snap)))
                          (expect (empty? (:repositories snap))))
                        (finally (cleanup root))))))

(defdescribe
  repository-inventory-test
  (it "exposes a stable lightweight inventory through the Vis facade"
      (let [root (make-tmp-dir)]
        (try (mark-repo! (io/file root "services/zeta"))
             (mark-repo! (io/file root "services/alpha"))
             (repositories/refresh-inventory!)
             (let [inventory (vis/repository-inventory root)]
               (expect (= 2 (:count inventory)))
               (expect (= ["services/alpha" "services/zeta"]
                          (mapv :path (:repositories inventory))))
               (expect (false? (:truncated? inventory)))
               (expect (every? #(= #{:path :root} (set (keys %))) (:repositories inventory))))
             (finally (repositories/refresh-inventory!) (cleanup root)))))
  (it "discovers dot-prefixed repositories but skips known cache, vendor, and build roots"
      (let [root (make-tmp-dir)]
        (try (mark-repo! (io/file root "visible"))
             (mark-repo! (io/file root ".hidden/project"))
             (mark-repo! (io/file root ".cache/project"))
             (mark-repo! (io/file root "vendor/project"))
             (mark-repo! (io/file root "build/project"))
             ;; an iOS app's derived data holds a full clone of every resolved
             ;; package, and the same package again under a second build path
             (mark-repo! (io/file root "ios/App/DerivedData/SourcePackages/checkouts/capacitor"))
             (mark-repo! (io/file root "ios/DerivedData/SourcePackages/checkouts/capacitor"))
             (mark-repo! (io/file root "ios/App/Pods/SomePod"))
             (mark-repo! (io/file root ".build/checkouts/swift-log"))
             (repositories/refresh-inventory!)
             (expect (= [".hidden/project" "visible"]
                        (mapv :path (:repositories (repositories/inventory root)))))
             (finally (repositories/refresh-inventory!) (cleanup root)))))
  (it "keeps new repositories out of a cached inventory until refresh"
      (let [root (make-tmp-dir)]
        (try (mark-repo! (io/file root "alpha"))
             (repositories/refresh-inventory!)
             (expect (= ["alpha"] (mapv :path (:repositories (repositories/inventory root)))))
             (mark-repo! (io/file root "beta"))
             (expect (= ["alpha"] (mapv :path (:repositories (repositories/inventory root)))))
             (repositories/refresh-inventory!)
             (expect (= ["alpha" "beta"]
                        (mapv :path (:repositories (repositories/inventory root)))))
             (finally (repositories/refresh-inventory!) (cleanup root)))))
  (it "reports a bounded inventory as truncated"
      (let [root (make-tmp-dir)]
        (try (mark-repo! (io/file root "alpha"))
             (mark-repo! (io/file root "beta"))
             (repositories/refresh-inventory!)
             (let [inventory (repositories/inventory root {:max-repos 1})]
               (expect (= 1 (:count inventory)))
               (expect (true? (:truncated? inventory))))
             (finally (repositories/refresh-inventory!) (cleanup root))))))
