(ns com.blockether.vis.lint-config-test
  "clj-kondo must read the SAME rules on a fresh checkout as it does here. The
   configs it imports from dependencies (`.clj-kondo/imports/`) are part of
   those rules — lazytest alone teaches it `deftest`, `defdescribe` and `it` —
   so they are tracked in the repo, never regenerated per machine."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private imports-dir
  "Where clj-kondo copies each dependency's exported config."
  ".clj-kondo/imports")

(def ^:private required-imports
  "Configs whose absence made `clojure -M:lint` report 289 unresolved symbols."
  ["io.github.noahtheduke/lazytest/config.edn"])

(defn- git-lines
  "Non-blank stdout lines. Exit 1 is an answer here (`check-ignore` says
   nothing matched), anything above it is a broken invocation."
  [& args]
  (let [{:keys [exit out err]} (apply shell/sh "git" args)]
    (when (> (int exit) 1) (throw (ex-info (str "git failed: " err) {:args (vec args)})))
    (->> (str/split-lines out)
         (remove str/blank?)
         vec)))

(defn- imported-files
  "Every imported config on disk, as repo-relative slash paths."
  []
  (->> (file-seq (io/file imports-dir))
       (filter (fn [^java.io.File f]
                 (.isFile f)))
       (map (fn [^java.io.File f]
              (str/replace (.getPath f) "\\" "/")))
       set))

(def ^:private in-repo? (and (.exists (io/file "deps.edn")) (.exists (io/file imports-dir))))

;; Regression: `.clj-kondo/imports/` was gitignored, so a fresh checkout (CI)
;; linted without the dependency configs and every lazytest test name came back
;; `Unresolved symbol` — 289 errors on a tree that was clean locally.
(defdescribe lint-config-test
             (it "tracks every imported clj-kondo config, so CI lints with our rules"
                 (when in-repo?
                   (let
                     [tracked
                      (set (git-lines "ls-files" imports-dir))

                      untracked
                      (sort (remove tracked (imported-files)))]

                     (expect (seq tracked)
                             (str imports-dir " is not tracked: a fresh checkout lints without it"))
                     (expect (empty? untracked)
                             (str "imported configs missing from git:\n" (str/join "\n" untracked)))
                     (doseq [required required-imports]
                       (expect (contains? tracked (str imports-dir "/" required))
                               (str required " must ship with the repo"))))))
             (it "keeps the imported configs out of every ignore rule"
                 (when in-repo?
                   (let [ignored (git-lines "check-ignore" "--" imports-dir)]
                     (expect (empty? ignored)
                             (str "ignored, so absent on CI: " (str/join ", " ignored)))))))
