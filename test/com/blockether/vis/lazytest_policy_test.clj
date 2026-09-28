(ns com.blockether.vis.lazytest-policy-test
  "Vis tests use Lazytest's own API: `defdescribe`, `describe`, `it` and `expect`.
   The Lazytest runner never discovers `clojure.test` tests, and Lazytest's
   experimental interfaces imitate other frameworks and may change at any time."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private skipped-directories
  "Dependencies and build output, which hold no Vis source."
  #{"node_modules" "target"})

(defn- clojure-sources
  "Every Clojure file in the repository, outside hidden and skipped directories."
  []
  (letfn [(walk [^java.io.File directory]
            (mapcat (fn [^java.io.File file]
                      (let [file-name (.getName file)]
                        (cond (.isDirectory file) (when-not (or (str/starts-with? file-name ".")
                                                                (contains? skipped-directories
                                                                           file-name))
                                                    (walk file))
                              (re-find #"\.(clj[cs]?|bb)$" file-name) [file])))
                    (sort (.listFiles directory))))]
    (walk (io/file "."))))

(defn- code-only
  "`source` with each comment, string, regex body and character literal replaced by
   a space. A linear scan, so no string is too long to read."
  ^String [^String source]
  (let [n
        (.length source)

        out
        (StringBuilder. n)]

    (loop [i 0]
      (if (< i n)
        (let [c (.charAt source i)]
          (case c
            \;
            (do (.append out \space) (recur (long (or (str/index-of source "\n" i) n))))

            \"
            (do (.append out \space)
                (recur (loop [j (inc i)]
                         (cond (>= j n) j
                               (= \\ (.charAt source j)) (recur (+ j 2))
                               (= \" (.charAt source j)) (inc j)
                               :else (recur (inc j))))))

            \\
            (do (.append out \space)
                (recur (if (and (< (inc i) n) (Character/isLetter (.charAt source (inc i))))
                         (loop [j (+ i 2)]
                           (if (and (< j n) (Character/isLetterOrDigit (.charAt source j)))
                             (recur (inc j))
                             j))
                         (+ i 2))))

            (do (.append out c) (recur (inc i)))))
        (.toString out)))))

(defn- framework-references
  "Namespaces of `clojure.test` or a Lazytest experimental interface that the code
   in `source` refers to."
  [source]
  (->> (re-seq #"[^\s,()\[\]{}'`~@^#\"]+" (code-only source))
       (keep (fn [token]
               (let [namespace-part (first (str/split token #"/" 2))]
                 (when (or (= "clojure.test" namespace-part)
                           (str/starts-with? namespace-part "lazytest.experimental.interfaces"))
                   namespace-part))))
       distinct
       vec))

(defdescribe
  framework-references-test
  (it "finds clojure.test and Lazytest's experimental interfaces in code"
      (expect (= ["clojure.test"]
                 (framework-references "(ns a (:require [clojure.test :refer [deftest is]]))")))
      (expect (= ["clojure.test"] (framework-references "(clojure.test/is true)")))
      (expect (= ["lazytest.experimental.interfaces.clojure-test"]
                 (framework-references
                   "(require '[lazytest.experimental.interfaces.clojure-test :refer [is]])")))
      (expect (= ["lazytest.experimental.interfaces.xunit"]
                 (framework-references
                   "(:require [lazytest.experimental.interfaces.xunit :refer [defsuite]])"))))
  (it "reads a string of any length without exhausting the stack"
      (expect (= ["clojure.test"]
                 (framework-references
                   (str "(def s \"" (apply str (repeat 100000 "\\\"x")) "\") clojure.test/is")))))
  (it "reads a character literal as code, not as the start of a comment"
      (expect (= ["clojure.test"]
                 (framework-references "(case c \\; :semicolon clojure.test/is)"))))
  (it "ignores comments, strings, other namespaces and the division function"
      (expect (= [] (framework-references ";; not clojure.test\n(str \"clojure.test/is\")")))
      (expect (= [] (framework-references "(/ 10 (count clojure.string/blank?))")))
      (expect
        (= []
           (framework-references
             "(:require [clojure.test.check.generators :as gen] [lazytest.core :refer [it]])")))))

(defdescribe native-lazytest-only-test
             (it "every Clojure file writes its tests with lazytest.core"
                 (let [files
                       (clojure-sources)

                       offenders
                       (->> files
                            (keep (fn [^java.io.File file]
                                    (when-let [references (seq (framework-references (slurp file)))]
                                      (str (.getPath file) " (" (str/join ", " references) ")"))))
                            vec)]

                   (expect (some #(= "lazytest_policy_test.clj" (.getName ^java.io.File %)) files)
                           "the scan must reach the test directory")
                   (expect (= [] offenders)
                           (str
                             "Write tests with lazytest.core (defdescribe, describe, it, expect), "
                             "not clojure.test or a Lazytest experimental interface. Found in: "
                             (str/join ", " offenders))))))
