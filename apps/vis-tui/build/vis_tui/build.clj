(ns vis-tui.build
  "Build the standalone vis-tui native executable."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.build.api :as b]))

(def ^:private class-dir "target/native-classes")

(def ^:private binary "target/vis-tui")

(def ^:private source-dirs ["src" "../../packages/vis-contract/src"])

(def ^:private resource-dirs ["resources" "../../packages/vis-contract/resources"])

(defn clean [_] (b/delete {:path "target"}))

(defn- repo-value
  [file key]
  (some->> (str/split-lines (slurp (str "../../" file)))
           (some #(second (re-matches (re-pattern (str key "=\"([^\"]+)\"")) %)))))

(defn- assert-graal!
  []
  (let [expected
        (repo-value ".graalvm-version" "GRAAL_VENDOR_VERSION")

        actual
        (System/getProperty "java.vendor.version")]

    (when-not (= expected actual)
      (throw (ex-info (str "vis-tui native build requires " expected "; found " actual)
                      {:expected expected :actual actual})))))

(defn- version [] (str/trim (slurp "../../VIS_VERSION")))

(defn- prepare!
  []
  (b/delete {:path class-dir})
  (b/copy-dir {:src-dirs (into source-dirs resource-dirs) :target-dir class-dir})
  (let [version-file (io/file class-dir "vis-tui" "VERSION")]
    (io/make-parents version-file)
    (spit version-file (version)))
  (let [basis (b/create-basis {:project "deps.edn" :aliases [:native]})]
    (b/compile-clj {:basis basis :src-dirs source-dirs :class-dir class-dir})
    basis))

(defn native
  [_]
  (assert-graal!)
  (let [basis
        (prepare!)

        jars
        (filter #(str/ends-with? % ".jar") (:classpath-roots basis))

        classpath
        (str/join java.io.File/pathSeparator (cons class-dir jars))

        extra
        (some-> (System/getenv "VIS_TUI_NATIVE_EXTRA_ARGS")
                str/trim
                not-empty
                (str/split #"\s+"))

        command
        (into ["native-image" "-cp" classpath "-o" binary "-H:IncludeResources=vis-tui/VERSION"
               "com.blockether.vis.tui.main"]
              extra)]

    (b/delete {:path binary})
    (println "native-image:" binary)
    (let [{:keys [exit]} (b/process {:command-args command})]
      (when-not (zero? exit) (throw (ex-info "vis-tui native-image build failed" {:exit exit}))))
    (println "-> built" binary)))
