(ns com.blockether.vis.python.formatter-test
  "PythonFormatter produces what `ruff format` produces with its default settings. Each
   `fixtures/format/<name>.in.py` formats to `<name>.out.py`, which ruff 0.16.0 produced;
   together they cover calls, strings, comments, definitions, expressions, collections,
   statements, match, numbers, blank lines and a typical sandbox block."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it throws?]])
  (:import (com.blockether.vis.python PythonFormatter PythonFormatter$ParseException)
           (java.io File)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files)
           (java.security MessageDigest)))

(def ^:private fixture-dir (io/file "test/com/blockether/vis/python/fixtures/format"))

(def ^:private package-dir (io/file "packages/vis-python-presentation"))

(defn- golden-pairs
  "Every `<name>.in.py` fixture with the `<name>.out.py` ruff made of it, sorted by name."
  []
  (->> (.listFiles ^File fixture-dir)
       (keep
         (fn [^File f]
           (when-let [[_ stem] (re-matches #"(.+)\.in\.py" (.getName f))]
             {:name stem :in (slurp f) :out (slurp (io/file fixture-dir (str stem ".out.py")))})))
       (sort-by :name)))

(defn- relative-path
  ^String [^File root ^File f]
  (str/replace (str (.relativize (.toPath root) (.toPath f))) File/separator "/"))

(defn- source-digest
  "The name of the package's prep marker: SHA-256 over every file under `src/java` in
   path order, each as its relative path, a NUL byte, its content and a NUL byte; the
   first 16 hex digits."
  []
  (let [root
        (io/file package-dir "src/java")

        digest
        (MessageDigest/getInstance "SHA-256")]

    (doseq [^File f (sort-by #(relative-path root %) (filter #(.isFile ^File %) (file-seq root)))]
      (.update digest (.getBytes (relative-path root f) StandardCharsets/UTF_8))
      (.update digest (byte-array 1))
      (.update digest (Files/readAllBytes (.toPath f)))
      (.update digest (byte-array 1)))
    (subs (format "%064x" (BigInteger. 1 (.digest digest))) 0 16)))

(defdescribe
  format-test
  (it "formats every golden input the way ruff does"
      (expect (<= 10 (count (golden-pairs))))
      (doseq [{:keys [name in out]} (golden-pairs)]
        (expect (= out (PythonFormatter/format in)) name)))
  (it "leaves ruff's output unchanged"
      (doseq [{:keys [name out]} (golden-pairs)]
        (expect (= out (PythonFormatter/format out)) name)))
  (it "normalizes line endings, blank source and a byte order mark"
      (expect (= "x = 1\ny = 2\n" (PythonFormatter/format "x=1\r\ny=2\r\n")))
      (expect (= "x = 1\n" (PythonFormatter/format "x=1")))
      (expect (= "" (PythonFormatter/format "")))
      (expect (= "\n" (PythonFormatter/format "   \n\n")))
      (expect (= "x = 1\n" (PythonFormatter/format "\uFEFFx=1\n"))))
  (it "returns source that does not parse unchanged"
      (doseq [broken ["def f(:\n    pass\n" "x = (\n" "f'{'"]]
        (expect (= broken (PythonFormatter/format broken)))
        (expect (throws? PythonFormatter$ParseException #(PythonFormatter/formatOrThrow broken)))))
  (it "returns source with formatter suppression comments unchanged"
      (doseq [suppressed ["x = [1,2]  # fmt: skip\ny=3\n" "# fmt: off\nx=[1,2]\n# fmt: on\ny=3\n"
                          "# yapf: disable\nx=[1,2]\n"]]
        (expect (= suppressed (PythonFormatter/format suppressed))))))

(defdescribe prep-marker-test
             (it "names the prep marker after the Java sources, so a changed source compiles again"
                 (let [expected
                       (str "target/prepared/" (source-digest))

                       ensure
                       (-> (io/file package-dir "deps.edn")
                           slurp
                           edn/read-string
                           :deps/prep-lib
                           :ensure)]

                   (expect (= expected ensure)
                           (str "Set :ensure in packages/vis-python-presentation/deps.edn to \""
                                expected
                                "\"")))))
