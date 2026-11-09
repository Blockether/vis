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

(defdescribe graalvm-pin-test
             (it "agrees with .sdkmanrc"
                 (let [{:strs [GRAAL_SDKMAN_CANDIDATE]}
                       (parse-pin pin-file)

                       sdkmanrc
                       (io/file ".sdkmanrc")]

                   (when (.isFile sdkmanrc)
                     (let [java-line (some->> (str/split-lines (slurp sdkmanrc))
                                              (some #(second (re-matches #"\s*java=(.*)" %)))
                                              str/trim)]
                       (expect (= GRAAL_SDKMAN_CANDIDATE java-line)))))))
