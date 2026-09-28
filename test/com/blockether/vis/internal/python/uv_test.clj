(ns com.blockether.vis.internal.python.uv-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [lazytest.core :refer [defdescribe expect it]])
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

(defdescribe
  installer-diagnostics-test
  (it
    "installer diagnostics"
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

          (expect (= :export (:phase data)))
          (expect (= 23 (:exit data)))
          (expect (false? (:timeout? data)))
          (expect (str/includes? (str (:diagnostics data)) "UV_EXPORT_MARKER"))
          (expect (str/includes? (str (.getMessage e)) "exit 23"))
          (doseq [secret ["fixture-password" "fixture-token" "fixture-query" "fixture-fragment"
                          "fixture-bearer"]]
            (expect (not (str/includes? rendered secret)))))))))

(defdescribe installer-output-is-bounded-test
             (it "installer output is bounded"
                 (with-uv-fixture
                   (str
                     "i=0; while [ \"$i\" -lt 20000 ]; do printf '0123456789'; i=$((i+1)); done\n"
                     "printf '\nUV_TAIL_MARKER\n' >&2\nexit 7\n")
                   (fn [dir uv]
                     (let [e
                           (failure #(#'python-runtime/run-uv! dir [uv "pip" "install"]))

                           diagnostics
                           (:diagnostics (ex-data e))]

                       (expect (= :install (:phase (ex-data e))))
                       (expect (str/includes? (str diagnostics) "UV_TAIL_MARKER"))
                       (expect (<= (count diagnostics) 16384)))))))

(defdescribe installer-timeout-keeps-partial-output-test
             (it "installer timeout keeps partial output"
                 (with-uv-fixture
                   "printf 'UV_PARTIAL_MARKER: waiting for registry' >&2\n/bin/sleep 30\n"
                   (fn [dir uv]
                     (let [e
                           (failure #(#'python-runtime/run-uv! dir [uv "lock"] {:timeout-ms 2000}))

                           data
                           (ex-data e)]

                       (expect (= :lock (:phase data)))
                       (expect (true? (:timeout? data)))
                       (expect (str/includes? (str (:diagnostics data)) "UV_PARTIAL_MARKER")
                               (pr-str data)))))))

(defdescribe automatic-preparation-preserves-safe-cause-test
             (it "automatic preparation preserves safe cause"
                 (with-uv-fixture
                   "exit 0\n"
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
                           (expect (= :sync (:phase (ex-data e))))
                           (expect (= 23 (:exit (ex-data e))))
                           (expect (str/includes? (str (.getMessage e)) "UV_CAUSE_MARKER"))
                           (expect (some? (.getCause e))))))))))

(defdescribe
  installer-multiline-credential-redaction-test
  (it
    "installer multiline credential redaction"
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

      (expect (not (str/includes? rendered "fixture-key-body")))
      (expect (str/includes? rendered "BUILD_ERROR_MARKER"))
      (expect (str/includes? rendered "BUILD_DETAIL_MARKER")))))

(defdescribe
  automatic-preparation-ignores-caller-project-environment-test
  (it "automatic preparation ignores caller project environment"
      (python-runtime/ensure-library!)
      (with-uv-fixture
        "printf '%s\n' \"${UV_PROJECT_ENVIRONMENT:-unset}\" >> \"$0.environments\"\nexit 0\n"
        (fn [dir uv]
          (let [environment-log
                (io/file (str uv ".environments"))

                configure-index!
                @#'python-runtime/uv-index!]

            (with-redefs-fn {#'python-runtime/uv-index! (fn [^ProcessBuilder builder]
                                                          (configure-index! builder)
                                                          (.put (.environment builder)
                                                                "UV_PROJECT_ENVIRONMENT"
                                                                "/foreign/extension-project"))
                             #'python-runtime/bundled-uv! (constantly uv)
                             #'python-runtime/project-packages (constantly dir)}
              (fn []
                (#'python-runtime/run-uv! dir [uv "sync"])
                (python-runtime/ensure-project! dir)
                (expect (= ["/foreign/extension-project" "unset"]
                           (str/split-lines (slurp environment-log)))))))))))

(defdescribe
  vis-index-reaches-uv-processes-test
  (it "vis index reaches uv processes"
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
                    (expect (= (or index "") (slurp (io/file (str uv ".index"))))))))))))))

(defdescribe uv-index-environment-precedence-test
             (it "uv index environment precedence"
                 (doseq [[index inherited expected]
                         [[nil {"UNRELATED" "kept"} {"UNRELATED" "kept"}]
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
                     (with-redefs [config/load-config-raw
                                   (constantly (if index {"python" {"index_url" index}} {}))]
                       (expect (identical? builder (#'python-runtime/uv-index! builder)))
                       (expect (= expected (into {} environment))))))))

(defdescribe uv-index-invalid-config-does-not-launch-test
             (it "uv index invalid config does not launch"
                 ;; #183: never fall back to a public index when Vis's configured index is invalid.
                 (python-runtime/ensure-library!)
                 (with-uv-fixture
                   "printf launched > \"$0.launched\"\n"
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
                               (expect (str/includes? (str (some-> e
                                                                   .getMessage))
                                                      "python.index_url"))
                               (expect (not (.exists (io/file (str uv ".launched"))))))))))))))

(defdescribe
  uv-cli-forwards-argv-and-exit-test
  (it "uv cli forwards argv and exit"
      ;; #183: bundling uv must not replace its commands or reinterpret its options.
      (with-uv-fixture
        "printf '%s\n' \"$@\" > \"$0.args\"\nexit 23\n"
        (fn [_ uv]
          (with-redefs-fn {#'python-runtime/bundled-uv! (constantly uv)}
            (fn []
              (doseq [args [["--version"]
                            ["sync" "--group" "dev" "--python" "chosen-python" "--frozen"]
                            ["run" "--no-sync" "python" "-c" "print('fixture')"]]]
                (let [result (try (python-runtime/uv-command! args) (catch Exception e e))
                      recorded (io/file (str uv ".args"))]

                  (expect (= 23 result))
                  (expect (= args
                             (when (.isFile recorded) (str/split-lines (slurp recorded)))))))))))))

(defdescribe
  shared-sync-options-test
  (it "shared sync options"
      (expect (= {:selection [["--locked"] ["--group" "dev"] ["--all-extras"]
                              ["--no-emit-package" "excluded"]]
                  :common [["--offline"]]
                  :location [["--project" "path with spaces"]]}
                 (#'python-runtime/shared-sync-args
                  ["--locked" "--group=dev" "--all-extras" "--no-install-package" "excluded"
                   "--offline" "--project=path with spaces"])))
      (doseq [args [["--group"] ["--group="] ["--group" "--offline"] ["--locked=yes"]
                    ["--target" "/elsewhere"] ["--python" "other"] ["--active"] ["--exact"]
                    ["--check"] ["--dry-run"] ["--script" "tool.py"] ["--output-file" "other"]]]
        (expect (some? (failure #(#'python-runtime/shared-sync-args args))) (pr-str args)))))

(defdescribe
  shared-sync-help-and-invalid-command-do-not-launch-test
  (it "shared sync help and invalid command do not launch"
      (with-redefs-fn {#'python-runtime/bundled-uv!
                       #(throw (ex-info "uv must not launch for help or invalid options" {}))}
        (fn []
          (with-open [bytes
                      (java.io.ByteArrayOutputStream.)

                      stream
                      (java.io.PrintStream. bytes)]

            (with-redefs [config/original-stdout stream]
              (expect (= 0 (python-runtime/uv-command! ["sync" "--help"] {:shared? true})))
              (expect (str/includes? (.toString bytes "UTF-8") "--all-groups"))
              (expect (str/includes? (.toString bytes "UTF-8") "unrelated"))))
          (expect (str/includes? (.getMessage (failure #(python-runtime/uv-command! ["run" "python"]
                                                                                    {:shared?
                                                                                     true})))
                                 "supports uv sync only"))))))

(defdescribe
  shared-sync-stages-and-cleanup-test
  (it
    "shared sync stages and cleanup"
    (python-runtime/ensure-library!)
    (doseq [[export-exit install-exit] [[0 0] [23 0] [0 17]]]
      (with-uv-fixture
        (str "printf '%s\n' \"$@\" > \"$0.$1.args\"\n"
             "printf '%s' \"${UV_DEFAULT_INDEX:-}\" > \"$0.$1.index\"\n"
             "case \"$1\" in\n"
             "  workspace) dirname \"$0\";;\n"
             "  export) exit "
             export-exit
             ";;\n"
             "  pip) exit "
             install-exit
             ";;\nesac\n")
        (fn [dir uv]
          (let [packages (str (io/file dir "shared"))
                configure-index! @#'python-runtime/uv-index!]

            (with-redefs-fn {#'python-runtime/bundled-uv! (constantly uv)
                             #'runtime/packages-dir (constantly packages)
                             #'config/load-config-raw
                             (constantly {"python" {"index_url"
                                                    "https://gateway.example.com/simple"}})
                             #'python-runtime/uv-index! (fn [^ProcessBuilder builder]
                                                          (doto (.environment builder)
                                                            (.remove "UV_DEFAULT_INDEX")
                                                            (.remove "UV_INDEX_URL"))
                                                          (configure-index! builder))}
              (fn []
                (expect (= (if (zero? export-exit) install-exit export-exit)
                           (python-runtime/uv-command! ["sync" "--locked" "--group" "dev"
                                                        "--offline"]
                                                       {:shared? true})))
                (let [export (str/split-lines (slurp (io/file (str uv ".export.args"))))
                      options (set export)
                      lock-file (io/file (second (drop-while #(not= "--output-file" %) export)))
                      install-file (io/file (str uv ".pip.args"))]

                  (expect (every? options ["--locked" "--group" "dev" "--offline" "pylock.toml"]))
                  (expect (= (.getCanonicalFile dir)
                             (.getCanonicalFile (.getParentFile lock-file))))
                  (expect (not (.exists lock-file)))
                  (expect (= (zero? export-exit) (.exists install-file)))
                  (doseq [phase (if (zero? export-exit)
                                  ["workspace" "export" "pip"]
                                  ["workspace" "export"])]
                    (expect (= "https://gateway.example.com/simple"
                               (slurp (io/file (str uv "." phase ".index")))))))
                (when (zero? export-exit)
                  (let [install (str/split-lines (slurp (io/file (str uv ".pip.args"))))]
                    (expect (= packages (second (drop-while #(not= "--target" %) install))))
                    (expect (not-any? #{"--exact" "sync" "--group" "--locked"} install))))))))))))

(defdescribe shared-sync-refuses-empty-workspace-path-test
             (it "shared sync refuses empty workspace path"
                 (python-runtime/ensure-library!)
                 (with-uv-fixture "exit 0\n"
                                  (fn [_ uv]
                                    (with-redefs-fn {#'python-runtime/bundled-uv! (constantly uv)}
                                      (fn []
                                        (expect (str/includes?
                                                  (.getMessage (failure #(python-runtime/uv-command!
                                                                           ["sync"]
                                                                           {:shared? true})))
                                                  "workspace directory"))))))))

(defdescribe extension-environment-probe-uses-workspace-test
             (it "extension environment probe uses workspace"
                 (with-uv-fixture
                   "exit 0\n"
                   (fn [dir _]
                     (let [member
                           (doto (io/file dir "member") .mkdir)

                           environment
                           (io/file dir ".venv")]

                       (spit (io/file dir "pyproject.toml")
                             "[tool.uv.workspace]\nmembers=['member']\n")
                       (spit (io/file member "pyproject.toml")
                             "[project]\nname='extension-member'\nversion='1.0.0'\n")
                       (expect (false? (python-runtime/project-environment-exists? member)))
                       (.mkdir (io/file member ".venv"))
                       (expect (false? (python-runtime/project-environment-exists? member)))
                       ;; An invalid environment must fail its readiness check, not select shared packages.
                       (spit environment "not an environment")
                       (expect (true? (python-runtime/project-environment-exists? member)))
                       (io/delete-file environment)
                       (.mkdir environment)
                       (expect (true? (python-runtime/project-environment-exists? member)))
                       (expect (not (.exists (io/file dir "uv.lock"))))
                       (expect (not (.exists (io/file member "uv.lock")))))))))

(defdescribe
  extension-environment-probe-keeps-errors-test
  (it "extension environment probe keeps errors"
      (doseq [[script message] [["exit 0\n" "workspace directory"]
                                ["echo WORKSPACE_FAILURE >&2; exit 23\n" "WORKSPACE_FAILURE"]]]
        (with-uv-fixture
          script
          (fn [dir uv]
            (with-redefs-fn {#'python-runtime/bundled-uv! (constantly uv)}
              (fn []
                (expect (str/includes?
                          (.getMessage (failure #(python-runtime/project-environment-exists? dir)))
                          message)))))))))
