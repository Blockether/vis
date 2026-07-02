(ns com.blockether.vis.ext.language-clojure.lint-test
  "Tests for clj-kondo linting behind the `lint_code` language-surface verb:
   the low-level `lint/*` helpers and the `core/clj-lint-fn` facade adapter
   (code string / {:code} / {:path} / {:paths} / default src+test)."
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.ext.language-clojure.core :as core]
   [com.blockether.vis.ext.language-clojure.lint :as lint]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (java.nio.file Files)
   (java.nio.file.attribute FileAttribute)))

(defn- tmp-dir ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-clj-lint-" (into-array FileAttribute []))))

(defn- cleanup [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))] (.delete f))))

;; ── low-level lint/* helpers ────────────────────────────────────────────────

(defdescribe lint-code-test
  (it "returns the uniform :clj-lint shape"
    (let [r (lint/lint-code "(ns a) (defn h [x] x)")]
      (expect (= :clj-lint (:op r)))
      (expect (number? (:error r)))
      (expect (number? (:warning r)))
      (expect (number? (:info r)))
      (expect (vector? (:findings r)))))

  (it "flags an unused binding as a warning"
    (let [r        (lint/lint-code "(ns a) (defn h [a b] a)")
          findings (:findings r)]
      (expect (pos? (:warning r)))
      ;; the finding carries the uniform per-item shape
      (expect (some #(= "unused-binding" (:type %)) findings))
      (let [f (first (filter #(= "unused-binding" (:type %)) findings))]
        (expect (= "warning" (:level f)))
        (expect (number? (:row f)))
        (expect (number? (:col f)))
        (expect (string? (:message f))))))

  (it "reports clean code with zero warnings and errors"
    (let [r (lint/lint-code "(ns a) (defn h [x] x)")]
      (expect (zero? (:error r)))
      (expect (zero? (:warning r))))))

(defdescribe lint-paths-test
  (it "lints a file on disk and reports its findings"
    (let [dir (tmp-dir)
          f   (io/file dir "bad.clj")]
      (try
        (spit f "(ns bad) (defn h [a b] a)")
        (let [r (lint/lint-paths [(.getAbsolutePath f)])]
          (expect (= :clj-lint (:op r)))
          (expect (pos? (:warning r)))
          (expect (some #(= "unused-binding" (:type %)) (:findings r))))
        (finally (cleanup dir))))))

;; ── facade adapter: core/clj-lint-fn ────────────────────────────────────────

(defn- lint-result [env arg]
  (let [r (core/clj-lint-fn env arg)]
    (expect (:success? r))
    (get-in r [:result])))

(defdescribe clj-lint-fn-test
  (it "lints a raw code string argument"
    (let [res (lint-result {} "(ns a) (defn h [a b] a)")]
      (expect (= :clj-lint (:op res)))
      (expect (= "clojure" (:language res)))
      (expect (pos? (:warning res)))))

  (it "lints a {:code ...} map"
    (let [res (lint-result {} {:code "(ns a) (defn h [a b] a)"})]
      (expect (pos? (:warning res)))))

  (it "accepts stringified snake_case keys ({\"code\" ...})"
    (let [res (lint-result {} {"code" "(ns a) (defn h [a b] a)"})]
      (expect (pos? (:warning res)))))

  (it "lints a {:path ...} file resolved under :workspace/root"
    (let [dir (tmp-dir)]
      (try
        (let [src (io/file dir "src" "x.clj")]
          (.mkdirs (.getParentFile src))
          (spit src "(ns x) (defn h [a b] a)"))
        (let [res (lint-result {:workspace/root (.getAbsolutePath dir)}
                    {:path "src/x.clj"})]
          (expect (pos? (:warning res)))
          (expect (some #(= "unused-binding" (:type %)) (:findings res))))
        (finally (cleanup dir)))))

  (it "lints {:paths [...]} resolved under :workspace/root"
    (let [dir (tmp-dir)]
      (try
        (let [src (io/file dir "src" "y.clj")]
          (.mkdirs (.getParentFile src))
          (spit src "(ns y) (defn h [a b] a)"))
        (let [res (lint-result {:workspace/root (.getAbsolutePath dir)}
                    {:paths ["src"]})]
          (expect (pos? (:warning res))))
        (finally (cleanup dir)))))

  (it "with no arg, defaults to the workspace's src (and test) dirs"
    (let [dir (tmp-dir)]
      (try
        (let [src (io/file dir "src" "z.clj")]
          (.mkdirs (.getParentFile src))
          (spit src "(ns z) (defn h [a b] a)"))
        (let [res (lint-result {:workspace/root (.getAbsolutePath dir)} nil)]
          (expect (= :clj-lint (:op res)))
          (expect (pos? (:files res)))
          (expect (pos? (:warning res))))
        (finally (cleanup dir))))))
