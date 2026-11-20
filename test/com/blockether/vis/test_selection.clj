(ns com.blockether.vis.test-selection
  "Lazytest hook of the `:test` alias. It selects the test namespaces before Lazytest
   loads them, so a run loads only the namespaces that it runs. On a developer machine
   it leaves out `release-bundle-test` when no file that it runs or reads has changed.
   CI and `VIS_TEST_ALL=1` run it. CI with `VIS_TEST_BASE`, the last green commit,
   leaves it out in the same way. `VIS_TEST_PART` runs one part of the suite, or some
   parts with commas between them, so parallel CI jobs share it. A `--namespace` or
   `--var` selection runs as given."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.tools.namespace.find :as find]
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
  "True when the run may leave out `release-bundle-test`. `changed` is nil when Git
   cannot tell what changed; then the test runs. CI runs it, unless `VIS_TEST_BASE`
   names the commit that `changed` compares with."
  [env changed]
  (not (or (and (not (str/blank? (get env "CI"))) (str/blank? (get env "VIS_TEST_BASE")))
           (= "1" (get env "VIS_TEST_ALL"))
           (nil? changed)
           (some release-input? changed))))

(def ^:private parts
  "The named parts and the namespace prefixes of each, in match order. The times are of
   one hosted Linux runner. `python-cli` and `python-uv` start a JVM for each
   `vis-agent python` call (215 s and 185 s). `python-cli` is the shortest job, so it
   also has `release-bundle-test` (125 s), which most pushes to main leave out.
   `python-ext` is the extension worker (150 s). `python` is the rest of the embedded
   Python (190 s). `core` is the model loop, the tools and the gateway (260 s); the
   gateway answer check counts the contract tests, so they run with the gateway. The
   part `other` has every namespace that no named part has (305 s)."
  [["python-cli"
    ["com.blockether.vis.internal.python.cli-environment-test"
     "com.blockether.vis.release-bundle-test"]]
   ["python-uv" ["com.blockether.vis.internal.python.cli-shared-uv-test"]]
   ["python-ext" ["com.blockether.vis.internal.python.extensions-test"]]
   ["python" ["com.blockether.vis.internal.python."]]
   ["core"
    ["com.blockether.vis.internal.loop-test" "com.blockether.vis.internal.foundation."
     "com.blockether.vis.internal.gateway." "com.blockether.vis.contract."]]])

(def ^:private part-names (conj (mapv first parts) "other"))

(defn- part-of
  [n]
  (or (some (fn [[part prefixes]]
              (when (some #(str/starts-with? (str n) %) prefixes) part))
            parts)
      "other"))

(defn in-part?
  "True when the namespace `n` runs in `part`, the value of `VIS_TEST_PART`: one part
   name, or some names with commas between them. Without a part, every namespace runs."
  [part n]
  (or (str/blank? part)
      (let [wanted (str/split part #",")]
        (when-let [unknown (seq (remove (set part-names) wanted))]
          (throw (ex-info (str "VIS_TEST_PART must name parts of " (str/join ", " part-names)
                               ", not " (pr-str part))
                          {:part part :unknown unknown})))
        (boolean (some #{(part-of n)} wanted)))))

(defn selected
  "The namespaces of `all` that the run has: those of `part`, without
   `release-bundle-test` when `skip-release?`."
  [all part skip-release?]
  (into [] (comp (filter #(in-part? part %)) (remove #(and skip-release? (= release-ns %)))) all))

(hooks/defhook selection
               "Select the namespaces of the run before Lazytest loads them."
               (config
                 [config _]
                 (if (or (seq (:ns-filter config)) (seq (:var-filter config)))
                   config
                   (let [all
                         (into []
                               (mapcat #(find/find-namespaces-in-dir (io/file %)))
                               (or (:dirs config) ["test"]))

                         part
                         (System/getenv "VIS_TEST_PART")

                         skip?
                         (and (some #{release-ns} (selected all part false))
                              (skip-release? (System/getenv)
                                             (changed-paths (System/getenv "VIS_TEST_BASE"))))

                         run
                         (selected all part skip?)]

                     (when skip?
                       (println (str "Skipped "
                                     release-ns
                                     ": no change under bin/, .github/, installer/, "
                                     "build.clj, VIS_VERSION or deps.edn. "
                                     "VIS_TEST_ALL=1 runs it.")))
                     (if (= (count run) (count all)) config (assoc config :ns-filter (set run)))))))
