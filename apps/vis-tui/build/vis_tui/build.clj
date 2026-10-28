(ns vis-tui.build
  "Build the standalone vis-tui native executable."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.build.api :as b]))

(def ^:private class-dir "target/native-classes")

(def ^:private binary "target/vis-tui")

(def ^:private source-dirs ["src" "../../packages/vis-contract/src"])

(def ^:private resource-dirs ["resources" "../../packages/vis-contract/resources"])

;; Java classes `clojure -X:deps prep` compiled; the image takes them as they are.
(def ^:private class-dirs ["../../packages/vis-python-presentation/target/classes"])

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

(defn- native-lib-token
  "Host-platform suffix of the imaging native artifact: darwin-arm64, linux-x64 and so on."
  []
  (let [os
        (str/lower-case (System/getProperty "os.name"))

        arch
        (str/lower-case (System/getProperty "os.arch"))

        a
        (cond (#{"aarch64" "arm64"} arch) "arm64"
              (#{"x86_64" "amd64" "x64"} arch) "x64"
              :else arch)]

    (str (if (str/includes? os "mac") "darwin" "linux") "-" a)))

(defn- imaging-native-jars
  "The host `imaging-native-<platform>` jars at the version of `com.blockether/imaging` in
   `basis`. A native image cannot download the library at run time, so without these jars
   every transcoded picture is blank. A missing jar fails the build."
  [basis]
  (let [artifact
        (symbol "com.blockether" (str "imaging-native-" (native-lib-token)))

        version
        (or (get-in basis [:libs 'com.blockether/imaging :mvn/version])
            (throw (ex-info "vis-tui native build requires com.blockether/imaging." {})))

        jars
        (->> (get-in (b/create-basis {:project nil
                                      :extra {:deps {artifact {:mvn/version version}}}})
                     [:libs artifact :paths])
             (filter #(str/ends-with? % ".jar"))
             vec)]

    (when (empty? jars)
      (throw (ex-info "vis-tui native build requires the imaging native library for its platform."
                      {:artifact artifact :version version})))
    jars))

(defn- prepare!
  []
  (b/delete {:path class-dir})
  (b/copy-dir {:src-dirs (concat source-dirs resource-dirs class-dirs) :target-dir class-dir})
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
        (concat (filter #(str/ends-with? % ".jar") (:classpath-roots basis))
                (imaging-native-jars basis))

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
