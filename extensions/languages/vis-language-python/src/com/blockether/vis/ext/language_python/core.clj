(ns com.blockether.vis.ext.language-python.core
  "vis-language-python — a managed Python REPL exposed through the generic
   language facade (repl_start / repl_status / repl_stop / repl_eval). Activates
   only when the workspace looks like a Python project. The REPL is a subprocess
   on a project-aware interpreter (uv / poetry / .venv / python3), registered as
   a session resource so it shows in ctx + the footer and is stoppable by id."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.xml :as xml]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.language-python.interpreter :as interpreter]
            [com.blockether.vis.ext.language-python.repl-manager :as repl]
            [com.blockether.vis.ext.language-python.ruff :as pyruff]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.python-project :as pyproj]
            [com.blockether.vis.internal.python-test-runner :as ptr]
            [com.blockether.vis.internal.runtime-settings :as rt]
            [com.blockether.vis.internal.test-contract :as contract]))

;; Activation

(defn- workspace-has-python?
  [env]
  (let [root (some-> (:workspace/root env)
                     io/file)]
    (when (and root (.isDirectory root))
      (or (some #(.exists (io/file root %))
                ["pyproject.toml" "setup.py" "setup.cfg" "requirements.txt" "Pipfile" "uv.lock"])
          ;; bounded scan for a .py anywhere (lazy file-seq, capped)
          (boolean (some #(and (.isFile ^java.io.File %)
                               (str/ends-with? (.getName ^java.io.File %) ".py"))
                         (take 3000 (file-seq root))))))))

(defn- activation-fn [env] (boolean (workspace-has-python? env)))

;; Helpers

(defn- env-root
  ^String [env]
  (or (:workspace/root env)
      (throw (ex-info "python tool fired without :workspace/root in env"
                      {:type :py/no-workspace}))))

(defn- resolve-dir
  ^String [root dir]
  (let [d (paths/expand-home (str (or dir "")))]
    (.getCanonicalPath (cond (= "" d) (io/file root)
                             (.isAbsolute (io/file d)) (io/file d)
                             :else (io/file root d)))))

(defn- repl-resource-id
  [dir id]
  (let [id (some-> id
                   str
                   str/trim)]
    (if (seq id) id (str "pyrepl:" dir))))

(defn register-repl-resource!
  "Mirror a managed Python REPL into the session resource registry (ctx + footer
   + stop by id; no restart — stop, then start). No-op without a session or a live pid."
  [session dir result & [id]]
  ;; `result` is repl/start!'s STRING-keyed lifecycle map. The resource map is
  ;; the CENTRAL resources.clj DATA shape (keyword keys — ->data stringifies its
  ;; own keys + kind/status/owner/language enums), but `:detail` is passed
  ;; THROUGH verbatim, so it must already be STRING-keyed for the boundary.
  (when (and session (= "up" (get result "status")) (get result "pid"))
    (vis/register-resource! session
                            {:id (repl-resource-id dir id)
                             :kind :repl
                             :label (str "python REPL " (.getName (io/file dir)))
                             :status (or (get result "status") :up)
                             :detail {"cwd" dir "cmd" (get result "cmd")}
                             :pid (get result "pid")
                             :owner :ext/language-python
                             :language :python}
                            {:stop-fn (fn []
                                        (repl/stop! dir))})
    (vis/notify! (str "● python REPL up — " (.getName (io/file dir)))
                 :level :success
                 :ttl-ms 4000)))

;; Language-facade handlers

(defn py-start-repl-fn
  "REPL-lifecycle handler for Python. The facade's `repl_start` / `repl_status` /
   `repl_stop` verbs reach a pack as a positional `op` STRING plus opts
   `{dir, id, env}` — there is NO restart (stop, then start), and a `repl_start`
   for a REPL that is already running REUSES it, refusing only when this call
   named a different `env`. `op` arrives as a STRING from the model
   (strings-only boundary) — dispatch on it, no keyword minting."
  [env op opts]
  (let [root
        (env-root env)

        ;; A MISSING op must never spawn: every pack defaults to "status", the one
        ;; step with no side effect.
        op
        (if (string? op) op "status")

        id
        (or (get opts "id") (get opts "repl_id"))

        dir
        (resolve-dir root (get opts "cwd"))]

    (case op
      "status"
      (extension/success {:result (assoc (repl/status dir) "id" (repl-resource-id dir id))})

      "stop"
      (let [r (assoc (repl/stop! dir) "id" (repl-resource-id dir id))]
        (vis/unregister-resource! (:session-id env) (repl-resource-id dir id))
        (extension/success {:result r}))

      "start"
      (let [r (assoc (repl/start! dir
                                  (assoc (or opts {})
                                    "id" (repl-resource-id dir id)
                                    :session-id (:session-id env)))
                "id" (repl-resource-id dir id))]
        (register-repl-resource! (:session-id env) dir r id)
        (extension/success {:result r}))

      (throw (ex-info (str "python REPL lifecycle: unknown op " (pr-str op)
                           " — the verbs are repl_start / repl_status / repl_stop; there is no"
                           " repl_connect for Python, Vis owns the interpreter process.")
                      {:type :py/bad-args :got op})))))

(defn py-repl-eval-fn
  "repl_eval handler for Python. Accepts a code string or
   `{code, dir, timeout_ms}`. Requires a running REPL for the dir, then evaluates
   with globals persistent across calls."
  [env arg]
  (let [root
        (env-root env)

        code
        (cond (string? arg) arg
              (map? arg) (str (or (get arg "code") (get arg "source")))
              :else (throw (ex-info "repl_eval(python) expects a code string or {\"code\": ...}"
                                    {:type :py/bad-args :got arg})))

        dir
        (resolve-dir root (and (map? arg) (get arg "cwd")))

        tmo
        (and (map? arg) (get arg "timeout_ms"))]

    (when-not (= "up" (get (repl/status dir) "status"))
      ;; Home-homogenized: the message reads `~/vis`, matching the REPL ids in
      ;; session["resources"] — and `resolve-dir` expands `~` back, so the cwd
      ;; shown can be pasted straight into the retry call.
      (let [shown (paths/abbreviate-home (str dir))]
        (throw (ex-info (str "Python REPL is not up for "
                             shown
                             "; call repl_start(\"python\", {\"cwd\": "
                             (pr-str shown)
                             "}) first")
                        {:type :py/no-repl :dir dir}))))
    ;; Carry the evaluated code back on the result (string key) so the shared
    ;; repl_eval op-card can surface the FORM section — the render fn sees only
    ;; the result map, not the call args.
    (let [res (repl/eval! dir code tmo)]
      (extension/success {:result (cond-> res
                                    (map? res)
                                    (assoc "code" code))}))))

;; run_tests

(def ^:private output-char-cap
  "Chars of the pytest transcript carried back in `output`. A long run prints
   megabytes; what the cut drops is recoverable, because every fault also comes
   back structured in `failures`."
  8000)

(defn- clamp-output
  "`s` capped at `n` chars, cut in the MIDDLE behind a marker that NAMES how much
   went missing.

   Regression, issue #136: the cut was a tail slice behind a bare `…`, so a run
   with many failures came back with its summary line and no evidence at all —
   the whole `=== FAILURES ===` section was gone, and nothing said so. Both ends
   are load bearing: a collection error prints at the TOP, the verdict and the
   `short test summary info` at the BOTTOM."
  [^String s n]
  (let [len
        (count s)

        n
        (long n)]

    (if (<= len n)
      s
      ;; The marker itself is charged against the cap, so the result still fits.
      (let [kept
            (max 0 (- n 96))

            head-n
            (quot kept 3)

            tail-n
            (- kept head-n)]

        (str (subs s 0 head-n)
             "\n\n… " (- len kept)
             " characters omitted; every fault is listed in `failures` …\n\n"
             (subs s (- len tail-n)))))))

(defn- fault-headline
  "One-line headline for a fault body: the assertion line pytest marks with `E `
   when the body has one, else its first non-blank line. Capped — the full
   traceback stays in `output`."
  [s]
  (let [lines
        (remove str/blank? (str/split-lines (str s)))

        head
        (str/trim (str (or (last (filter #(re-find #"^E\s" %) lines)) (first lines) "")))]

    (if (> (count head) 400) (str (subs head 0 400) "…") head)))

(defn- junit-fault
  "One `<testcase>`'s `<failure>` / `<error>` child as the surface contract's
   `{ns test type message file line}`. `fault-type` is the contract's `\"fail\"`
   (a `<failure>`: an assertion came back false) or `\"error\"` (an `<error>`:
   the test threw), so ONE `failures` list carries both. `file` is resolved
   against the run's `cwd` when it lands there (pytest writes it relative to its
   own rootdir), and pytest's 0-based `line` becomes the contract's 1-based one."
  [^String dir case-attrs fault ^String fault-type]
  (let [rel
        (str (or (:file case-attrs) ""))

        resolved
        (when (seq rel)
          (let [^java.io.File f (io/file dir rel)]
            (if (.exists f) (.getCanonicalPath f) rel)))

        line
        (some-> (:line case-attrs)
                str
                parse-long)

        ns-name
        (str (or (:classname case-attrs) ""))

        attr-message
        (str/trim (str (or (:message (:attrs fault)) "")))]

    (cond-> {"test" (str (:name case-attrs))
             "type" fault-type
             "message" (fault-headline
                         (if (seq attr-message) attr-message (apply str (:content fault))))}
      (seq ns-name)
      (assoc "ns" ns-name)

      resolved
      (assoc "file" resolved)

      (and line (not (neg? (long line))))
      (assoc "line" (inc (long line))))))

(defn- junit-report
  "pytest's `--junitxml` report as `{:failures [...] :counts {...}}` — one fault
   list, each fault typed `\"fail\"` (a `<failure>`) or `\"error\"` (an `<error>`) —
   or nil when no readable report was written (a crash before pytest got there).

   Issue #136: pytest's summary line carries COUNTS and no node ids, so the
   project backend could report `1 failed` and nothing a model could open. The
   XML is the only machine-readable per-test record pytest offers, and its
   `<testsuite>` attributes also give counts for a run that printed no summary."
  [^String dir ^java.io.File xml]
  (when (.isFile xml)
    (try (let [root
               (with-open [in (io/input-stream xml)]
                 (xml/parse in))

               suites
               (if (= :testsuite (:tag root))
                 [root]
                 (filterv #(= :testsuite (:tag %)) (:content root)))

               cases
               (for [s
                     suites

                     c
                     (:content s)

                     :when (= :testcase (:tag c))]

                 c)

               faults
               (fn [tag fault-type]
                 (vec (for [c
                            cases

                            f
                            (:content c)

                            :when (= tag (:tag f))]

                        (junit-fault dir (:attrs c) f fault-type))))

               attr-sum
               (fn ^long [k]
                 (long (reduce +
                               0
                               (keep #(some-> (get-in % [:attrs k])
                                              str
                                              parse-long)
                                     suites))))

               tests
               (long (attr-sum :tests))

               failed
               (long (attr-sum :failures))

               errored
               (long (attr-sum :errors))

               skipped
               (long (attr-sum :skipped))]

           ;; pytest's own words are DISJOINT (`failures` beside `errors`); the
           ;; contract's `fail` holds every fault and `errored` is its erroring
           ;; SUBSET, so the sum happens HERE, where the two are known apart.
           {:failures (into (faults :failure "fail") (faults :error "error"))
            :counts {"pass" (max 0 (- tests failed errored skipped))
                     "fail" (+ failed errored)
                     "errored" errored
                     "skipped" skipped}})
         ;; A malformed or half-written report is not a reason to lose the run.
         (catch Exception _ nil))))

(defn- graalpy-faults
  "The hermetic backend's per-test records as surface-contract faults — one map
   per test whose `:outcome` is in `outcomes`, carrying the node id, its file,
   the assertion headline and its `\"type\"` (`:errored` -> `\"error\"`, anything
   else -> `\"fail\"`), so ONE `failures` list carries both kinds. Same reason as
   the project backend (issue #136): counts alone name nothing the reader can
   open."
  [tests outcomes]
  (vec (for [{:keys [nodeid outcome message file]}
             tests

             :when (contains? outcomes outcome)]

         (cond-> {"test" (or (second (str/split (str nodeid) #"::" 2)) (str nodeid))
                  "type" (if (= :errored outcome) "error" "fail")
                  "message" (fault-headline message)}
           (seq (str file))
           (assoc "file" (str file))))))

(defn- resolve-test-paths
  "What to hand a test runner: `{:paths [absolute-string] :names [string]}`.
   Honors `{paths}` — FILES, directories, or `<path>::<test-name>` NODE IDS
   (pytest's own grammar, and the ONE way every vis pack names a single test) —
   resolved against `dir` (the run's `cwd`), never the workspace root; else the
   project's own declared pytest `testpaths`; else `tests/` under `dir` when it
   exists, otherwise `dir`.
   A node id keeps its `::<test-name>` on the returned path — pytest selects
   with it — and only the PATH half has to exist. A PATHLESS `::<test-name>`
   names a test wherever it lives, so it comes back under `:names` for `-k`
   instead, leaving the default paths to say where to look.
   A named path that does not exist THROWS: silently running nothing reads as a
   false green."
  ([^String dir opts] (resolve-test-paths dir opts nil))
  ([^String dir opts testpaths]
   (let [ids
         (mapv contract/split-node-id (map str (get opts "paths")))

         located
         (filterv :path ids)

         names
         (into [] (comp (remove :path) (keep :var)) ids)

         abs
         (mapv (fn [{:keys [path var]}]
                 (str (resolve-dir dir path) (when var (str "::" var))))
               located)

         missing
         (vec (remove #(.exists (io/file ^String (first (str/split % #"::" 2)))) abs))]

     (when (seq missing)
       (throw (ex-info (str "run_tests(python) target does not exist: "
                            (str/join ", " missing)
                            " — relative paths resolve against the run's cwd")
                       {:type :py/bad-args :paths missing})))
     {:names names
      :paths (cond (seq abs) abs
                   (seq testpaths) (vec testpaths)
                   (.isDirectory (io/file dir "tests")) [(resolve-dir dir "tests")]
                   :else [(resolve-dir dir nil)])})))

(defn- graalpy-test
  "Hermetic backend: discover test_*.py / *_test.py under `paths` and run each in
   a trusted GraalPy context via the built-in stdlib-only pytest shim.
   `sys-path` carries the project's own declared import roots (a `src` layout),
   so a test importing the package under test sees it. Adds a `hint` to switch
   to the project interpreter when a failure smells like a missing third-party
   module the sandbox can't see."
  [paths sys-path]
  (let [res
        (ptr/test-python-extensions! {:dirs paths :sys-path sys-path})

        dep-smell?
        (boolean (some (fn [t]
                         (and (= :errored (:outcome t))
                              (re-find #"(?i)ModuleNotFoundError|No module named|ImportError"
                                       (str (:message t)))))
                       (:tests res)))]

    (cond-> {"runner" "graalpy"
             ;; In-process contexts, no shell — the surface contract's "repl" mode.
             "mode" "repl"
             "framework" "pytest"
             "tool" "graalpy"
             "files" (:files res)
             "is_pass" (boolean (:ok? res))
             "pass" (or (:passed res) 0)
             ;; Outcomes are disjoint here too: `fail` is every fault, `errored` the
             ;; subset of it that THREW — never a count added on top.
             "fail" (+ (long (or (:failed res) 0)) (long (or (:errored res) 0)))
             "errored" (or (:errored res) 0)
             "skipped" (or (:skipped res) 0)
             "failures" (graalpy-faults (:tests res) #{:failed :errored})
             "output" (ptr/render-test-report res)}
      (:error res)
      (assoc "error" (:error res))

      (seq sys-path)
      (assoc "sys_path" (vec sys-path))

      ;; Nothing was discovered: a run that executed no test is NOT a pass.
      (zero? (long (or (:files res) 0)))
      (assoc "is_pass"
        false "error"
        (str "No test file discovered under " (str/join ", " paths)
             " — looked for test_*.py / *_test.py (a directly named *.py file "
             "is taken as a test file). Nothing ran, so this is not a pass."))

      dep-smell?
      (assoc "hint"
        (str "Some tests failed to import modules under the stdlib-only GraalPy "
             "sandbox. Re-run with {\"environment\": \"project\"} to use the project's "
             "interpreter and installed dependencies.")))))

(defn- pytest-counts
  "Outcome counts read off pytest's own summary line, in the CONTRACT's words:
   `{\"pass\" n \"fail\" n \"errored\" n \"skipped\" n}`, where `fail` is every
   fault (pytest's `failed` PLUS its `error`s) and `errored` is the erroring
   subset of it. nil when that run printed no summary at all (a usage error, a
   crash), where the exit status is the only verdict there is.

   pytest NAMES only the outcomes that HAPPENED: an all-green run says `12 passed`
   and nothing else. Reading each absent word as UNKNOWN left an ordinary green
   run with no `total` and no `fail`, so its run_tests headline could report
   nothing but a duration. Once pytest reported ANY outcome, the words it left out
   are ZERO."
  [s]
  (let [n
        (fn [re]
          (some-> (second (re-find re (str s)))
                  parse-long))

        passed
        (n #"(?m)(\d+) passed")

        failed
        (n #"(?m)(\d+) failed")

        errored
        (n #"(?m)(\d+) error(?:ed|s)?\b")

        skipped
        (n #"(?m)(\d+) skipped")]

    (when (or passed failed errored skipped)
      {"pass" (or passed 0)
       "fail" (+ (long (or failed 0)) (long (or errored 0)))
       "errored" (or errored 0)
       "skipped" (or skipped 0)})))

(defn- project-test
  "Escape-hatch backend: shell the project interpreter's pytest (uv / poetry /
   .venv / python3 `-m pytest <paths>`) in `cwd` so installed deps are visible.

   The argv also asks pytest for a `--junitxml` report in a temp file (the
   `xunit1` family, the only one that still carries `file` / `line`), reads the
   per-test faults out of it and deletes it. That report is the ONLY
   machine-readable record pytest offers: without it the result could carry
   counts and not one node id (issue #136).

   `paths` are files, dirs or node ids (pytest reads `file.py::test_name`
   itself); `names` are the PATHLESS `::test_name` ids, joined into ONE `-k`
   expression because pytest keeps only the last `-k` flag it is given."
  [session-id ^String dir paths names]
  (let [^java.io.File junit
        (java.io.File/createTempFile "vis-pytest-" ".xml")

        cmd
        (cond-> (-> (interpreter/resolve-command dir)
                    (conj "-m" "pytest")
                    (into paths))
          (seq names)
          (conj "-k" (str/join " or " names))

          :always
          (conj "-o" "junit_family=xunit1" (str "--junitxml=" (.getPath junit))))

        launch
        (vis/session-process-launch session-id cmd)

        pb
        (doto (ProcessBuilder. ^java.util.List (:argv launch))
          (.directory (io/file dir))
          (.redirectErrorStream true))

        _env
        (let [^java.util.Map e (.environment ^ProcessBuilder pb)]
          (when (:replace-env? launch) (.clear e))
          (doseq [[k v] (:env launch)]
            (.put e ^String k ^String v)))

        p
        (.start pb)

        out
        (future (slurp (.getInputStream p)))

        done?
        (.waitFor p (long rt/RUN_TESTS_TIMEOUT_MS) java.util.concurrent.TimeUnit/MILLISECONDS)]

    (when-not done? (.destroyForcibly p))
    (try (let [s
               (str @out)

               report
               (junit-report dir junit)

               counts
               (or (pytest-counts s) (:counts report))]

           (cond-> (merge {"runner" "project"
                           "mode" "cli"
                           "framework" "pytest"
                           "tool" "pytest"
                           "command" (str/join " " cmd)
                           "cwd" dir
                           "exit" (when done? (.exitValue p))
                           "timed_out" (not done?)
                           "output" (clamp-output s output-char-cap)}
                          counts)
             (seq (:failures report))
             (assoc "failures" (:failures report))))
         (finally (.delete junit)))))

(defn- select-runner
  "Which backend a `run_tests` call uses, in precedence order: an explicit
   `environment` (`project`, else the sandbox), then `python.runner` from merged
   config, else the hermetic GraalPy sandbox. ONE word chooses it — the call says
   `environment`, config says `runner`, and neither spelling is accepted in the
   other's place."
  [opts]
  (let [environment
        (str/lower-case (str (or (get opts "environment") "")))

        configured
        (str/lower-case (str (or (interpreter/configured-runner) "graalpy")))]

    (cond (= "project" environment) "project"
          ;; An explicit environment that is not `project` is the sandbox,
          ;; whatever config says.
          (seq environment) "graalpy"
          (= "project" configured) "project"
          :else "graalpy")))

(defn py-test-fn
  "run_tests handler for Python. Two execution environments:
     - the DEFAULT is hermetic, stdlib-only GraalPy. It discovers `test_*.py` /
       `*_test.py` under `{paths}` (default: the project's declared pytest
       `testpaths`, else `tests/` if present, else the run's `cwd`) and runs
       each in a TRUSTED GraalPy context via the built-in pytest shim.
       `{paths}` entries may be FILES or dirs, resolve against `cwd`, must
       exist, and discovering nothing is NOT a pass. It runs whole FILES and
       has no test-name filter, so a `<path>::<test-name>` node id is REFUSED
       here (pointing at `{environment \"project\"}`) rather than quietly
       running every test in the file. The project's declared
       import roots (a `src` layout) are on `sys.path`; installed third-party
       deps are NOT visible. When that layout could not be READ, the result
       carries a `warning` instead of quietly claiming the project has none.
     - `{environment \"project\"}` shells the project interpreter's pytest
       (the argv pinned as `python.interpreter`, else `uv`/`poetry`/`.venv`/
       `python3` `-m pytest <paths>`) so installed test dependencies are
       visible. Node ids go straight through as pytest's own
       `file.py::test_name`, and a PATHLESS `::test_name` becomes `-k`.
   BOTH backends name their faults: every failing/erroring test comes back in
   ONE `failures` list as `{ns test type message file line}`, `type` telling a
   thrown `\"error\"` from a false-assertion `\"fail\"` (the project backend
   reads pytest's own `--junitxml` report), and `output` carries the transcript
   capped in the middle behind a marker that says how much it dropped.
   `python.runner` in merged config chooses the DEFAULT backend; an explicit
   `environment` argument still wins."
  [env arg]
  (let [root
        (env-root env)

        opts
        (if (map? arg) arg {})

        dir
        (resolve-dir root (get opts "cwd"))

        runner
        (select-runner opts)

        ;; The project interpreter reads the project's own config itself; only the
        ;; hermetic backend has to be taught the layout (one throwaway context).
        layout
        (when (= "graalpy" runner) (pyproj/project-layout dir))

        ;; The hermetic backend discovers and runs whole files; it has no
        ;; test-name filter, so a node id it CANNOT honor is refused by name
        ;; instead of running every test in the file and reporting that as the
        ;; selection the caller asked for.
        _
        (when (= "graalpy" runner)
          (when-let [named (seq (filter :var
                                        (map contract/split-node-id (map str (get opts "paths")))))]
            (throw (ex-info (str "run_tests(python) cannot select a single test in the hermetic"
                                 " sandbox: " (pr-str (mapv (fn [{:keys [path var]}]
                                                              (str path "::" var))
                                                            named))
                                 " — rerun with {\"environment\": \"project\"} so the project's own"
                                 " pytest reads the node id, or name the FILE to run all of it.")
                            {:type :py/bad-args :paths (mapv :path named)}))))

        {:keys [paths names]}
        (resolve-test-paths dir opts (:testpaths layout))]

    (extension/success {:result (cond-> (assoc (if (= "project" runner)
                                                 (project-test (:session-id env) dir paths names)
                                                 (graalpy-test paths (:import-roots layout)))
                                          "language" "python")
                                  (:warning layout)
                                  (assoc "warning" (:warning layout)))})))

;; Manifest

;; No :ext/prompt-fn — the foundation advertises repl_eval / repl through
;; the AUTO capability matrix; repl_eval's own result ({ok,out,value,data,type,
;; exc}; opaque values carry __type__/__attrs__/__opaque__) is self-documenting.

(def vis-extension
  (vis/extension
    {:ext/name "language-python"
     :ext/description
     "Python pack: in-process Ruff `format_code`/`lint_code` and managed uv/Poetry/venv/python3 `repl_start`/`repl_eval`; active in Python workspaces."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/activation-fn activation-fn
     :ext/language-tools [{:language "python"
                           :repl-eval-fn py-repl-eval-fn
                           :format-fn pyruff/py-format-fn
                           :lint-fn pyruff/py-lint-fn
                           :test-fn py-test-fn
                           :start-repl-fn (fn [env op opts]
                                            (py-start-repl-fn env op opts))}]
     :ext/kind "language"}))

(vis/register-extension! vis-extension)
