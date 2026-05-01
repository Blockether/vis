(ns com.blockether.vis.internal.extension.source-markers-test
  "Unit tests for the source-marker resolver. Covers file-classpath
   sources (the realistic case in this repo) and the empty / missing
   cases. Plan §5.5 + §6."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as sm]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe resolve-markers-test
  (it "resolves a single namespace to its source path + mtime + hash"
    (let [r (sm/resolve-markers ['com.blockether.vis.core])]
      (expect (= 1 (count (:source-paths r))))
      (let [p (first (:source-paths r))]
        (expect (str/ends-with? p "core.clj"))
        (expect (str/includes? p "/com/blockether/vis/")))
      (expect (pos? (:source-mtime-max r)))
      (expect (string? (:source-hash-sha256 r)))
      (expect (= 64 (count (:source-hash-sha256 r))))))

  (it "resolves multiple namespaces to a sorted path vec + aggregate hash"
    (let [r (sm/resolve-markers
              ['com.blockether.vis.ext.foundation.environment.core
               'com.blockether.vis.ext.foundation.environment.agents
               'com.blockether.vis.ext.foundation.environment.skills])]
      (expect (= 3 (count (:source-paths r))))
      ;; Paths are sorted alphabetically.
      (expect (= (:source-paths r) (sort (:source-paths r))))
      (expect (string? (:source-hash-sha256 r)))))

  (it "missing namespace → empty markers, no throw"
    (let [r (sm/resolve-markers ['this.namespace.does.not.exist.anywhere])]
      (expect (= [] (:source-paths r)))
      (expect (= -1 (:source-mtime-max r)))
      (expect (nil? (:source-hash-sha256 r)))))

  (it "empty input → empty markers"
    (let [r (sm/resolve-markers [])]
      (expect (= [] (:source-paths r)))
      (expect (= -1 (:source-mtime-max r)))
      (expect (nil? (:source-hash-sha256 r)))))

  (it "hash is stable across calls (deterministic)"
    (let [a (sm/resolve-markers ['com.blockether.vis.core])
          b (sm/resolve-markers ['com.blockether.vis.core])]
      (expect (= (:source-hash-sha256 a) (:source-hash-sha256 b))))))

(defdescribe resolve-markers-for-extension-test
  (it "uses :ext/namespace as fallback when no :nses field"
    (let [r (sm/resolve-markers-for-extension
              {:ext/namespace 'com.blockether.vis.core})]
      (expect (= 1 (count (:source-paths r))))))

  (it "respects an explicit :nses vec"
    (let [r (sm/resolve-markers-for-extension
              {:ext/namespace 'unused.fallback
               :nses ['com.blockether.vis.core
                      'com.blockether.vis.internal.extension]})]
      (expect (= 2 (count (:source-paths r))))))

  (it "missing :ext/namespace and :nses → empty"
    (let [r (sm/resolve-markers-for-extension {})]
      (expect (= [] (:source-paths r))))))
