(ns com.blockether.vis.graalvm-pin-test
  "The GraalVM pin is LOCKED at 25.1.3 and every consumer must agree with it.

   `.graalvm-version` is the single source of truth, but nothing in a unit test
   run touches `native-image`, so a half-finished bump (deps.edn moved, the file
   not — or the file moved past the lock) would otherwise only surface minutes
   into a release build, as an opaque Truffle mismatch or an OutOfMemoryError.
   This is the cheap gate: it reads the same file the shell script, the CI
   action and build.clj read, and asserts the lock plus every mirror of it."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
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

(defn- graalvm-dep-versions
  "Every `org.graalvm.*` :mvn/version in the root deps.edn. Truffle refuses a
   JDK whose built-in version differs from these jars."
  []
  (into []
        (keep (fn [[sym coord]]
                (when (str/includes? (str sym) "org.graalvm.") [sym (:mvn/version coord)])))
        (:deps (edn/read-string (slurp (io/file "deps.edn"))))))

(defdescribe
  graalvm-pin-test
  (it "is LOCKED at the maximum version — 25.1.3, nothing higher"
      (expect (.isFile pin-file))
      (let
        [{:strs [GRAAL_VERSION GRAAL_MAX_VERSION GRAAL_PIN_LOCKED GRAAL_EDITION]} (parse-pin
                                                                                    pin-file)]
        (expect (= "true" GRAAL_PIN_LOCKED))
        (expect (= "25.1.3" GRAAL_MAX_VERSION))
        (expect (= GRAAL_MAX_VERSION GRAAL_VERSION))
        ;; Community Edition, not Oracle: only CE's Classpath Exception keeps the
        ;; shipped binary redistributable (audit/README.md §4.1).
        (expect (= "GraalVM CE" GRAAL_EDITION))))
  (it "never names a 25.2.x artefact — that builder OOMs on this tree"
      (let
        [{:strs [GRAAL_VERSION GRAAL_TAG GRAAL_ASSET_VERSION GRAAL_SDKMAN_CANDIDATE
                 GRAAL_VENDOR_VERSION]}
         (parse-pin pin-file)]
        (expect (= "graal-25.1.3" GRAAL_TAG))
        (expect (= "25.1.3-graalce" GRAAL_SDKMAN_CANDIDATE))
        (expect (str/starts-with? GRAAL_VENDOR_VERSION (str "GraalVM CE " GRAAL_VERSION)))
        (doseq [v [GRAAL_TAG GRAAL_ASSET_VERSION GRAAL_SDKMAN_CANDIDATE GRAAL_VENDOR_VERSION]]
          (expect (not (str/includes? v "25.2"))))))
  (it "agrees with deps.edn's org.graalvm.* jars and .sdkmanrc"
      (let
        [{:strs [GRAAL_VERSION GRAAL_SDKMAN_CANDIDATE]}
         (parse-pin pin-file)

         deps
         (graalvm-dep-versions)]

        (expect (seq deps))
        (doseq [[sym v] deps]
          (expect (= GRAAL_VERSION v)
                  (str sym " pins " v ", .graalvm-version says " GRAAL_VERSION)))
        (let [sdkmanrc (io/file ".sdkmanrc")]
          (when (.isFile sdkmanrc)
            (let
              [java-line (some->> (str/split-lines (slurp sdkmanrc))
                                  (some #(second (re-matches #"\s*java=(.*)" %)))
                                  str/trim)]
              (expect (= GRAAL_SDKMAN_CANDIDATE java-line))))))))
