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
  ;; Monorepo: a file under a NESTED project must be linted against THAT
  ;; project's .clj-kondo, not the workspace root's — clj-kondo's own run!
  ;; otherwise resolves config from the process CWD only.
  (it "honors a nested project's .clj-kondo config over the workspace root"
      (let [dir (tmp-dir)]
        (try
          ;; workspace-root config: unused-binding stays a warning
          (let [root-kondo (io/file dir ".clj-kondo")]
            (.mkdirs root-kondo)
            (spit (io/file root-kondo "config.edn")
                  "{:linters {:unused-binding {:level :warning}}}"))
          ;; nested project silences unused-binding
          (let [sub-kondo (io/file dir "sub" ".clj-kondo")
                src (io/file dir "sub" "src" "x.clj")]

            (.mkdirs sub-kondo)
            (spit (io/file sub-kondo "config.edn") "{:linters {:unused-binding {:level :off}}}")
            (.mkdirs (.getParentFile src))
            (spit src "(ns x) (defn h [a b] a)"))
          ;; nested config wins -> the unused binding is NOT reported
          (let [res (lint-result {:workspace/root (.getAbsolutePath dir)} {"path" "sub/src/x.clj"})]
            (expect (not (some #(= "unused-binding" (get % "type")) (get res "findings")))))
          ;; contrast: identical code under the ROOT config IS flagged
          (let [rsrc (io/file dir "src" "y.clj")]
            (.mkdirs (.getParentFile rsrc))
            (spit rsrc "(ns y) (defn h [a b] a)")
            (let [res (lint-result {:workspace/root (.getAbsolutePath dir)} {"path" "src/y.clj"})]
              (expect (some #(= "unused-binding" (get % "type")) (get res "findings")))))
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

;; ── provider tag + by-path grouping ───────────────────────────────────────

(defdescribe provider-and-grouping-test
             (it "tags each clj-kondo finding with \"provider\" \"clj-kondo\""
                 (let [findings (get (lint/lint-code "(ns a) (defn h [a b] a)") "findings")]
                   (expect (seq findings))
                   (expect (every? #(= "clj-kondo" (get % "provider")) findings))))
             (it "group-by-dir nests findings by directory then basename then level"
                 (let [grouped (lint/group-by-dir
                                 [{"file" "src/x.clj" "level" "warning" "type" "a"}
                                  {"file" "src/x.clj" "level" "error" "type" "b"}
                                  {"file" "test/y.clj" "level" "warning" "type" "c"}])]
                   (expect (= #{"src" "test"} (set (keys grouped))))
                   (expect (= 1 (count (get-in grouped ["src" "x.clj" "warning"]))))
                   (expect (= 1 (count (get-in grouped ["src" "x.clj" "error"]))))
                   (expect (= 1 (count (get-in grouped ["test" "y.clj" "warning"]))))
                   (expect (nil? (get-in grouped ["test" "y.clj" "error"]))))))

;; ── the :general provider through the facade (code strings AND paths) ─────────

(defdescribe
  general-provider-facade-test
  (it "runs BOTH providers over a code string and reports them"
      (let [res (lint-result {} "(ns demo) (defn r [x] (.length x))")]
        (expect (= ["clj-kondo" "general"] (get res "providers")))
        ;; the reflection warning comes from :general (clj-kondo does not flag it)
        (expect (some #(and (= "reflection" (get % "type")) (= "general" (get % "provider")))
                      (get res "findings")))
        (expect (pos? (get res "warning")))))
  (it "flags boxed math via the :general provider"
      (let [res (lint-result {} "(ns demo) (defn add [a b] (+ a b))")]
        (expect (some #(= "boxed-math" (get % "type")) (get res "findings")))))
  (it "exposes the by-dir grouped view nested directory → basename"
      (let [res
            (lint-result {} "(ns demo) (defn h [a b] (.length a))")

            grouped
            (get res "by-dir")]

        ;; <stdin> has no directory → grouped under "." then its basename;
        ;; clj-kondo (unused b) + general (reflection) mix under one file group
        (expect (contains? grouped "."))
        (expect (= #{"clj-kondo" "general"}
                   (set (map #(get % "provider") (get-in grouped ["." "<stdin>" "warning"])))))))
  (it "runs the :general provider on paths too (compiling each targeted file)"
      (let [dir (tmp-dir)]
        (try (let [src (io/file dir "src" "g.clj")]
               (.mkdirs (.getParentFile src))
               (spit src "(ns g) (defn h [a b] (.length a))"))
             (let [res (lint-result {:workspace/root (.getAbsolutePath dir)} {"path" "src/g.clj"})]
               (expect (= ["clj-kondo" "general"] (get res "providers")))
               (expect (some #(and (= "reflection" (get % "type")) (= "general" (get % "provider")))
                             (get res "findings"))))
             (finally (cleanup dir))))))
