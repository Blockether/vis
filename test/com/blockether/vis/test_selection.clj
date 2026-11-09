(ns com.blockether.vis.test-selection
  "Lazytest hook of the `:test` alias. On a developer machine it skips
   `release-bundle-test` when no file that it runs or reads has changed. CI and
   `VIS_TEST_ALL=1` run it. A `--namespace` or `--var` selection runs as given."
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [lazytest.hooks :as hooks]))

(def ^:private release-ns 'com.blockether.vis.release-bundle-test)

(def ^:private release-inputs
  "The launcher and build scripts, the workflows and the version file that the test
   reads, and the test itself. Any `deps.edn` also counts, for the package build."
  ["bin/" ".github/" "installer/" "build.clj" "VIS_VERSION"
   "test/com/blockether/vis/release_bundle_test.clj"])

(defn- git-lines
  [& args]
  (let [{:keys [exit out]} (apply sh/sh "git" args)]
    (when (zero? (long exit)) (remove str/blank? (str/split-lines out)))))

(defn- changed-paths
  "Paths that differ from the upstream branch: commits, staged and unstaged changes,
   and untracked files. Nil when Git cannot tell."
  []
  (when-let [diff (git-lines "diff" "--name-only" "@{upstream}")]
    (when-let [untracked (git-lines "ls-files" "--others" "--exclude-standard")]
      (concat diff untracked))))

(defn- release-input?
  [path]
  (boolean (or (= "deps.edn" (peek (str/split path #"/")))
               (some #(str/starts-with? path %) release-inputs))))

(defn skip-release?
  "True when the run may skip `release-bundle-test`. `changed` is nil when Git
   cannot tell what changed; then the test runs."
  [config env changed]
  (not (or (seq (:ns-filter config))
           (seq (:var-filter config))
           (not (str/blank? (get env "CI")))
           (= "1" (get env "VIS_TEST_ALL"))
           (nil? changed)
           (some release-input? changed))))

(hooks/defhook release-scripts
               "Skip `release-bundle-test` locally when its inputs are unchanged."
               (pre-test-run [config m]
                             (if (skip-release? config (System/getenv) (changed-paths))
                               (do (println (str "Skipped "
                                                 release-ns
                                                 ": no change under bin/, .github/, installer/, "
                                                 "build.clj, VIS_VERSION or deps.edn. "
                                                 "VIS_TEST_ALL=1 runs it."))
                                   (update m
                                           :children
                                           #(filterv (fn [s]
                                                       (not= release-ns (:doc s)))
                                              %)))
                               m)))
