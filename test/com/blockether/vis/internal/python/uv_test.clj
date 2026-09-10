(ns com.blockether.vis.internal.python.uv-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- with-uv-fixture
  [script f]
  (let [dir
        (.toFile (Files/createTempDirectory "vis-uv-error" (make-array FileAttribute 0)))

        uv
        (io/file dir "uv")]

    (try (spit uv (str "#!/bin/sh\n" script))
         (.setExecutable uv true)
         (f dir (str uv))
         (finally (#'python-runtime/delete-tree! dir)))))

(defn- failure [f] (try (f) nil (catch Exception e e)))

(deftest installer-diagnostics-test
  ;; #183: preserve the installer verdict, not credentials or a generic replacement.
  (with-uv-fixture
    (str
      "printf '%s\n' 'UV_EXPORT_MARKER: stale lock' "
      "'https://user:fixture-password@gateway.example.com/simple?key=fixture-query#fixture-fragment' "
      "'https://fixture-token@gateway.example.com/simple' "
      "'Authorization: Bearer fixture-bearer' >&2\nexit 23\n")
    (fn [dir uv]
      (let [e
            (failure #(#'python-runtime/run-uv! dir [uv "export"]))

            data
            (ex-data e)

            rendered
            (str e (pr-str data))]

        (is (= :export (:phase data)))
        (is (= 23 (:exit data)))
        (is (false? (:timeout? data)))
        (is (str/includes? (str (:diagnostics data)) "UV_EXPORT_MARKER"))
        (is (str/includes? (str (.getMessage e)) "exit 23"))
        (doseq [secret ["fixture-password" "fixture-token" "fixture-query" "fixture-fragment"
                        "fixture-bearer"]]
          (is (not (str/includes? rendered secret))))))))

(deftest installer-output-is-bounded-test
  (with-uv-fixture (str
                     "i=0; while [ \"$i\" -lt 20000 ]; do printf '0123456789'; i=$((i+1)); done\n"
                     "printf '\nUV_TAIL_MARKER\n' >&2\nexit 7\n")
                   (fn [dir uv]
                     (let [e
                           (failure #(#'python-runtime/run-uv! dir [uv "pip" "install"]))

                           diagnostics
                           (:diagnostics (ex-data e))]

                       (is (= :install (:phase (ex-data e))))
                       (is (str/includes? (str diagnostics) "UV_TAIL_MARKER"))
                       (is (<= (count diagnostics) 16384))))))

(deftest installer-timeout-keeps-partial-output-test
  (with-uv-fixture "printf 'UV_PARTIAL_MARKER: waiting for registry' >&2\n/bin/sleep 30\n"
                   (fn [dir uv]
                     (let [e
                           (failure #(#'python-runtime/run-uv! dir [uv "lock"] {:timeout-ms 2000}))

                           data
                           (ex-data e)]

                       (is (= :lock (:phase data)))
                       (is (true? (:timeout? data)))
                       (is (str/includes? (str (:diagnostics data)) "UV_PARTIAL_MARKER")
                           (pr-str data))))))

(deftest automatic-preparation-preserves-safe-cause-test
  (with-uv-fixture "exit 0\n"
                   (fn [dir _]
                     (with-redefs-fn {#'python-runtime/run-uv!
                                      (fn [& _]
                                        (throw (ex-info "uv sync failed (exit 23): UV_CAUSE_MARKER"
                                                        {:phase :sync
                                                         :exit 23
                                                         :timeout? false
                                                         :diagnostics "UV_CAUSE_MARKER"})))}
                       (fn []
                         (let [e (failure #(python-runtime/ensure-project! dir))]
                           (is (= :sync (:phase (ex-data e))))
                           (is (= 23 (:exit (ex-data e))))
                           (is (str/includes? (str (.getMessage e)) "UV_CAUSE_MARKER"))
                           (is (some? (.getCause e)))))))))

(deftest installer-multiline-credential-redaction-test
  (let
    [e
     (python-runtime/installer-error
       "uv"
       :install
       {:exit 1
        :out
        "BUILD_ERROR_MARKER\n-----BEGIN PRIVATE KEY-----\nfixture-key-body\n-----END PRIVATE KEY-----\nBUILD_DETAIL_MARKER"})

     rendered
     (str e (pr-str (ex-data e)))]

    (is (not (str/includes? rendered "fixture-key-body")))
    (is (str/includes? rendered "BUILD_ERROR_MARKER"))
    (is (str/includes? rendered "BUILD_DETAIL_MARKER"))))

(deftest vis-index-reaches-uv-processes-test
  ;; #183: a Vis index must reach both explicit uv and automatic project preparation.
  (python-runtime/ensure-library!)
  (with-uv-fixture
    "printf '%s' \"${UV_DEFAULT_INDEX:-}\" > \"$0.index\"\nexit 0\n"
    (fn [dir uv]
      (let [yml
            (io/file dir "vis.yml")

            configure-index!
            @#'python-runtime/uv-index!]

        (with-redefs-fn {#'config/load-config-raw #(@#'config/read-yaml-config-map (str yml))
                         #'python-runtime/uv-index! (fn [^ProcessBuilder builder]
                                                      (doto (.environment builder)
                                                        (.remove "UV_DEFAULT_INDEX")
                                                        (.remove "UV_INDEX_URL"))
                                                      (configure-index! builder))
                         #'python-runtime/bundled-uv! (constantly uv)
                         #'python-runtime/project-packages (constantly dir)}
          (fn []
            (doseq [index ["https://gateway.example.com/simple"
                           "https://gateway.example.com/other/simple" nil]]
              (spit yml (if index (str "python:\n  index_url: " index "\n") "python: {}\n"))
              (doseq [invoke [#(python-runtime/uv-command! ["sync"])
                              #(python-runtime/ensure-project! dir)]]
                (invoke)
                (is (= (or index "") (slurp (io/file (str uv ".index")))))))))))))

(deftest uv-index-environment-precedence-test
  (doseq [[index inherited expected] [[nil {"UNRELATED" "kept"} {"UNRELATED" "kept"}]
                                      ["https://gateway.example.com/simple" {}
                                       {"UV_DEFAULT_INDEX" "https://gateway.example.com/simple"}]
                                      ["https://gateway.example.com/simple"
                                       {"UV_DEFAULT_INDEX" "https://gateway.example.com/explicit"}
                                       {"UV_DEFAULT_INDEX" "https://gateway.example.com/explicit"}]
                                      ["https://gateway.example.com/simple"
                                       {"UV_INDEX_URL" "https://gateway.example.com/legacy"}
                                       {"UV_INDEX_URL" "https://gateway.example.com/legacy"}]]]
    (let [builder (ProcessBuilder. ^java.util.List ["uv"])
          environment (.environment builder)]

      (.clear environment)
      (.putAll environment inherited)
      (with-redefs [config/load-config-raw (constantly
                                             (if index {"python" {"index_url" index}} {}))]
        (is (identical? builder (#'python-runtime/uv-index! builder)))
        (is (= expected (into {} environment)))))))

(deftest uv-index-invalid-config-does-not-launch-test
  ;; #183: never fall back to a public index when Vis's configured index is invalid.
  (python-runtime/ensure-library!)
  (with-uv-fixture "printf launched > \"$0.launched\"\n"
                   (fn [dir uv]
                     (doseq [index [nil "" " " 42 "--no-index"
                                    "https://user:fixture-password@gateway.example.com/simple"
                                    "https://gateway.example.com/simple?token=fixture"]]
                       (with-redefs-fn {#'config/load-config-raw (constantly {"python" {"index_url"
                                                                                        index}})
                                        #'python-runtime/bundled-uv! (constantly uv)}
                         (fn []
                           (doseq [invoke [#(python-runtime/uv-command! ["sync"])
                                           #(python-runtime/ensure-project! dir)]]
                             (let [e (failure invoke)]
                               (is (str/includes? (str (some-> e
                                                               .getMessage))
                                                  "python.index_url"))
                               (is (not (.exists (io/file (str uv ".launched")))))))))))))

(deftest uv-cli-forwards-argv-and-exit-test
  ;; #183: bundling uv must not replace its commands or reinterpret its options.
  (with-uv-fixture
    "printf '%s\n' \"$@\" > \"$0.args\"\nexit 23\n"
    (fn [_ uv]
      (with-redefs-fn {#'python-runtime/bundled-uv! (constantly uv)}
        (fn []
          (doseq [args [["--version"] ["sync" "--group" "dev" "--python" "chosen-python" "--frozen"]
                        ["run" "--no-sync" "python" "-c" "print('fixture')"]]]
            (let [result (try (python-runtime/uv-command! args) (catch Exception e e))
                  recorded (io/file (str uv ".args"))]

              (is (= 23 result))
              (is (= args (when (.isFile recorded) (str/split-lines (slurp recorded))))))))))))
