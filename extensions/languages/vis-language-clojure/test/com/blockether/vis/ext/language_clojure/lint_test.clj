(ns com.blockether.vis.ext.language-clojure.lint-test
  "Tests for clj-kondo linting behind the `lint_code` language-surface verb:
   the low-level `lint/*` helpers and the `core/clj-lint-fn` facade adapter
   (code string / {:code} / {:path} / {:paths} / default src+test)."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.language-clojure.core :as core]
            [com.blockether.vis.ext.language-clojure.lint :as lint]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- tmp-dir
  ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-clj-lint-" (into-array FileAttribute []))))

(defn- cleanup
  [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))]
      (.delete f))))

;; ── low-level lint/* helpers ────────────────────────────────────────────────

(defdescribe lint-code-test
             ;; run-lint returns a STRING-keyed uniform map (crosses the strings-only
             ;; boundary as a tool `:result`).
             (it "returns the uniform clj-lint shape"
                 (let [r (lint/lint-code "(ns a) (defn h [x] x)")]
                   (expect (= "clj-lint" (get r "op")))
                   (expect (number? (get r "error")))
                   (expect (number? (get r "warning")))
                   (expect (number? (get r "info")))
                   (expect (vector? (get r "findings")))))
             (it "flags an unused binding as a warning"
                 (let [r
                       (lint/lint-code "(ns a) (defn h [a b] a)")

                       findings
                       (get r "findings")]

                   (expect (pos? (get r "warning")))
                   ;; the finding carries the uniform per-item shape
                   (expect (some #(= "unused-binding" (get % "type")) findings))
                   (let [f (first (filter #(= "unused-binding" (get % "type")) findings))]
                     (expect (= "warning" (get f "level")))
                     (expect (number? (get f "row")))
                     (expect (number? (get f "col")))
                     (expect (string? (get f "message"))))))
             (it "reports clean code with zero warnings and errors"
                 (let [r (lint/lint-code "(ns a) (defn h [x] x)")]
                   (expect (zero? (get r "error")))
                   (expect (zero? (get r "warning"))))))

(defdescribe lint-paths-test
             (it "lints a file on disk and reports its findings"
                 (let [dir
                       (tmp-dir)

                       f
                       (io/file dir "bad.clj")]

                   (try (spit f "(ns bad) (defn h [a b] a)")
                        (let [r (lint/lint-paths [(.getAbsolutePath f)])]
                          (expect (= "clj-lint" (get r "op")))
                          (expect (pos? (get r "warning")))
                          (expect (some #(= "unused-binding" (get % "type")) (get r "findings"))))
                        (finally (cleanup dir))))))

;; ── facade adapter: core/clj-lint-fn ────────────────────────────────────────

(defn- lint-result
  [env arg]
  (let [r (core/clj-lint-fn env arg)]
    (expect (:success? r))
    (get-in r [:result])))

(defdescribe
  clj-lint-fn-test
  (it "lints a raw code string argument"
      (let [res (lint-result {} "(ns a) (defn h [a b] a)")]
        (expect (= "clj-lint" (get res "op")))
        (expect (= "clojure" (get res "language")))
        (expect (pos? (get res "warning")))))
  ;; Model args are STRING-keyed (strings-only boundary); the tool reads only
  ;; string keys.
  (it "lints a {\"code\" ...} map"
      (let [res (lint-result {} {"code" "(ns a) (defn h [a b] a)"})]
        (expect (pos? (get res "warning")))))
  (it "lints a {\"path\" ...} file resolved under the workspace root"
      (let [dir (tmp-dir)]
        (try (let [src (io/file dir "src" "x.clj")]
               (.mkdirs (.getParentFile src))
               (spit src "(ns x) (defn h [a b] a)"))
             (let [res (lint-result {:workspace/root (.getAbsolutePath dir)} {"path" "src/x.clj"})]
               (expect (pos? (get res "warning")))
               (expect (some #(= "unused-binding" (get % "type")) (get res "findings"))))
             (finally (cleanup dir)))))
  (it "lints {\"paths\" [...]} resolved under the workspace root"
      (let [dir (tmp-dir)]
        (try (let [src (io/file dir "src" "y.clj")]
               (.mkdirs (.getParentFile src))
               (spit src "(ns y) (defn h [a b] a)"))
             (let [res (lint-result {:workspace/root (.getAbsolutePath dir)} {"paths" ["src"]})]
               (expect (pos? (get res "warning"))))
             (finally (cleanup dir)))))
  (it "with no arg, defaults to the workspace's src (and test) dirs"
      (let [dir (tmp-dir)]
        (try (let [src (io/file dir "src" "z.clj")]
               (.mkdirs (.getParentFile src))
               (spit src "(ns z) (defn h [a b] a)"))
             (let [res (lint-result {:workspace/root (.getAbsolutePath dir)} nil)]
               (expect (= "clj-lint" (get res "op")))
               (expect (pos? (get res "files")))
               (expect (pos? (get res "warning"))))
             (finally (cleanup dir))))))
