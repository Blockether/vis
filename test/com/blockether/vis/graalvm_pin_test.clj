(ns com.blockether.vis.graalvm-pin-test
  "The GraalVM build pin and every consumer must agree.

   `.graalvm-version` is the single source of truth, but nothing in a unit test
   run touches `native-image`, so a half-finished bump (`.sdkmanrc` moved, the
   file not — or the file moved past the lock) would otherwise only surface
   minutes into a release build, as an opaque native-image failure or an
   OutOfMemoryError. This is the cheap gate: it reads the same file the shell
   script, the CI action and build.clj read, and asserts the lock plus every
   mirror of it."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- parse-pin
  "`.graalvm-version` as a map. It is plain KEY=\"value\" precisely so both a
   POSIX shell and a parser this small can read it."
  [file]
  (into {}
        (keep (fn [line]
                (when-let [[_ k v] (re-matches #"\s*([A-Z0-9_]+)=\"?([^\"#]*)\"?\s*" line)]
                  [k (str/trim v)])))
        (str/split-lines (slurp file))))

(def ^:private pin-file (io/file ".graalvm-version"))

(defdescribe
  graalvm-pin-test
  (it "agrees with .sdkmanrc"
      (let [{:strs [GRAAL_SDKMAN_CANDIDATE]}
            (parse-pin pin-file)

            sdkmanrc
            (io/file ".sdkmanrc")]

        (when (.isFile sdkmanrc)
          (let [java-line (some->> (str/split-lines (slurp sdkmanrc))
                                   (some #(second (re-matches #"\s*java=(.*)" %)))
                                   str/trim)]
            (expect (= GRAAL_SDKMAN_CANDIDATE java-line))))))
  (it
    "keeps every GitHub build job on the locked pin"
    (let [{:strs [GRAAL_VERSION]}
          (parse-pin pin-file)

          action
          (io/file ".github/actions/setup-graalvm-25/action.yml")

          workflows
          (->> (.listFiles (io/file ".github/workflows"))
               (filter #(str/ends-with? (.getName %) ".yml"))
               sort)]

      ;; ONE installer for CI, and it reads the same single source of truth this
      ;; test does — including the lock, so a half-bumped pin dies in setup.
      (expect (.isFile action))
      (let [src (slurp action)]
        (expect (str/includes? src "${GITHUB_WORKSPACE}/.graalvm-version"))
        (expect (str/includes? src "GRAAL_PIN_LOCKED"))
        (expect (str/includes? src "GRAAL_MAX_VERSION")))
      (expect (seq workflows))
      (doseq [wf
              workflows

              :let [src
                    (slurp wf)]]

        ;; A build OR dependency prep can compile a git dependency. Every such JOB
        ;; must therefore run on the one pinned JDK; a setup elsewhere in the same
        ;; workflow cannot affect it.
        (doseq [job
                (re-seq #"(?ms)^  [a-zA-Z0-9_-]+:\n.*?(?=^  [a-zA-Z0-9_-]+:|\z)" src)

                :when (or (str/includes? job "clojure -T:build")
                          (str/includes? job "clojure -X:deps prep"))]

          (expect (str/includes? job "uses: ./.github/actions/setup-graalvm-25")
                  (str (.getName wf)
                       " prepares or builds on an unpinned JDK: "
                       (first (str/split-lines job)))))
        ;; And no workflow may name a GraalVM version other than the pin.
        (doseq [[_ v] (re-seq #"GraalVM (?:CE )?(\d+\.\d+\.\d+(?:\.\d+)?)" src)]
          (expect (= GRAAL_VERSION v)
                  (str (.getName wf) " mentions GraalVM " v ", pin is " GRAAL_VERSION)))))))
