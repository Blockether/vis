(ns com.blockether.vis.test-selection
  "Lazytest hook of the `:test` alias. On a developer machine it skips
   `release-bundle-test` when no file that it runs or reads has changed. CI and
   `VIS_TEST_ALL=1` run it. CI with `VIS_TEST_BASE`, the last green commit, skips it
   in the same way. `VIS_TEST_PART` runs one of the two parts of the suite,
   so two CI jobs share it. A `--namespace` or `--var` selection runs as given."
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
  "Paths that differ from the commit `base`, or from the upstream branch without one:
   commits, staged and unstaged changes, and untracked files. Nil when Git cannot tell."
  [base]
  (when-let [diff (git-lines "diff" "--name-only" (if (str/blank? base) "@{upstream}" base))]
    (when-let [untracked (git-lines "ls-files" "--others" "--exclude-standard")]
      (concat diff untracked))))

(defn- release-input?
  [path]
  (boolean (or (= "deps.edn" (peek (str/split path #"/")))
               (some #(str/starts-with? path %) release-inputs))))

(defn skip-release?
  "True when the run may skip `release-bundle-test`. `changed` is nil when Git
   cannot tell what changed; then the test runs. CI runs it, unless `VIS_TEST_BASE`
   names the commit that `changed` compares with."
  [config env changed]
  (not (or (seq (:ns-filter config))
           (seq (:var-filter config))
           (and (not (str/blank? (get env "CI"))) (str/blank? (get env "VIS_TEST_BASE")))
           (= "1" (get env "VIS_TEST_ALL"))
           (nil? changed)
           (some release-input? changed))))

(def ^:private python-prefix "com.blockether.vis.internal.python.")

(defn in-part?
  "True when the namespace `n` runs in `part`, the value of `VIS_TEST_PART`. The part
   `python` has the namespaces of the embedded Python, which take about half of the
   time of the suite. The part `other` has every other namespace. Without a part,
   every namespace runs."
  [part n]
  (let [python? (str/starts-with? (str n) python-prefix)]
    (case part
      (nil "")
      true

      "python"
      python?

      "other"
      (not python?)

      (throw (ex-info (str "VIS_TEST_PART must be python or other, not " (pr-str part))
                      {:part part})))))

(hooks/defhook part
               "Run only the namespaces of `VIS_TEST_PART`."
               (pre-test-run [config m]
                             (let [part (System/getenv "VIS_TEST_PART")]
                               (if (or (seq (:ns-filter config)) (seq (:var-filter config)))
                                 m
                                 (update m
                                         :children
                                         #(filterv (fn [s]
                                                     (in-part? part (:doc s)))
                                            %))))))

(hooks/defhook release-scripts
               "Skip `release-bundle-test` when its inputs are unchanged."
               (pre-test-run [config m]
                             (if (skip-release? config
                                                (System/getenv)
                                                (changed-paths (System/getenv "VIS_TEST_BASE")))
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
