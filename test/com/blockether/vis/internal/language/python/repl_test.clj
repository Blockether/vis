(ns com.blockether.vis.internal.language.python.repl-test
  "Managed Python REPL: interpreter detection, subprocess lifecycle + persistent
   eval, and the language-facade wiring. The live-subprocess tests SKIP when no
   Python is on PATH so CI without Python stays green."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.language.python.core :as core]
            [com.blockether.vis.internal.language.python.interpreter :as interp]
            [com.blockether.vis.internal.language.python.repl-manager :as repl]
            [com.blockether.vis.internal.activity.presenter-test :as activity-fixture]
            [com.blockether.vis.internal.gateway.resources :as resources]
            [com.blockether.vis.internal.foundation.language-surface :as language-surface]
            [com.blockether.vis.internal.sandbox.jail :as process-jail]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- tmp-dir
  ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-py-ext-" (into-array FileAttribute []))))

(defn- cleanup
  [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))]
      (.delete f))))

(def ^:private on-path? @#'interp/on-path?)

(def ^:private test-session-id "python-pack-test")

(process-jail/register-session-jail! test-session-id
                                     (constantly {:roots-fn (constantly [(System/getProperty
                                                                           "java.io.tmpdir")])
                                                  :net-enabled? true
                                                  :disabled? true}))

(defn- has-python? [] (boolean (or (on-path? "python3") (on-path? "python"))))

;; ── interpreter detection (no subprocess) ────────────────────────────────────
(defdescribe
  interpreter-test
  (it "prefers a project-local .venv interpreter when present"
      (let [root
            (tmp-dir)

            py
            (io/file root ".venv" "bin" "python")]

        (try (.mkdirs (.getParentFile py))
             (spit py "#!/bin/sh\n")
             (.setExecutable py true)
             (expect (= [(.getAbsolutePath py)] (interp/detect-command (.getPath root))))
             (finally (cleanup root)))))
  (it "keeps the venv symlink instead of canonicalizing OUT of the venv"
      ;; `.venv/bin/python3` is a symlink into the base installation.
      ;; Canonicalizing it leaves the virtualenv, `pyvenv.cfg` is never
      ;; read, and the run dies with `No module named pytest`
      ;; (Blockether/vis#98).
      (let [root
            (tmp-dir)

            base
            (io/file root "base" "python3")

            py
            (io/file root ".venv" "bin" "python3")]

        (try (.mkdirs (.getParentFile base))
             (spit base "#!/bin/sh\n")
             (.mkdirs (.getParentFile py))
             (Files/createSymbolicLink (.toPath py) (.toPath base) (into-array FileAttribute []))
             (expect (= [(.getAbsolutePath py)] (interp/detect-command (.getPath root))))
             (finally (cleanup root)))))
  (it "falls back to a system interpreter with no project env"
      (let [root (tmp-dir)]
        (try (let [cmd (interp/detect-command (.getPath root))]
               (expect (= 1 (count cmd)))
               (expect (#{"python3" "python"} (first cmd))))
             (finally (cleanup root))))))

;; ── `python.runner` configured in merged config ─────────────────────────────
(defdescribe runner-config-test
             "The configured default test backend."
             (it "reads python.runner, ignoring anything that is not a backend"
                 (expect (= "project" (interp/pinned-runner {"python" {"runner" "Project"}})))
                 (expect (= "vispython" (interp/pinned-runner {"python" {"runner" "vispython"}})))
                 (expect (nil? (interp/pinned-runner {"python" {"runner" "pytest"}})))
                 (expect (nil? (interp/pinned-runner {})))))

;; ── uv detection reads TOML TABLE HEADERS, not substrings ────────────────────
;; `[tool.uvicorn]` used to satisfy a `str/includes? "[tool.uv"` check, so a
;; project that merely depends on uvicorn was launched under `uv run python`.
(def ^:private uv-project? @#'interp/uv-project?)

(defn- with-pyproject
  "Run `f` on a throwaway root holding `pyproject.toml` with `toml`."
  [^String toml f]
  (let [root (tmp-dir)]
    (try (spit (io/file root "pyproject.toml") toml) (f (.getPath root)) (finally (cleanup root)))))

(defdescribe uv-detection-test
             (it "does NOT mistake a [tool.uvicorn] table for a uv project"
                 (with-pyproject "[project]\nname = \"x\"\n\n[tool.uvicorn]\nport = 8000\n"
                                 (fn [root]
                                   (expect (false? (uv-project? root))))))
             (it "does NOT mistake [tool.uv-dynamic-versioning] for a uv project"
                 (with-pyproject "[tool.uv-dynamic-versioning]\nstyle = \"pep440\"\n"
                                 (fn [root]
                                   (expect (false? (uv-project? root))))))
             (it "ignores a commented-out [tool.uv] header"
                 (with-pyproject "[project]\nname = \"x\"\n# [tool.uv] we do not use uv\n"
                                 (fn [root]
                                   (expect (false? (uv-project? root))))))
             (it "ignores [tool.uv] inside a string value"
                 (with-pyproject "[project]\ndescription = \"see [tool.uv] docs\"\n"
                                 (fn [root]
                                   (expect (false? (uv-project? root))))))
             (it "ignores a header-looking line inside a multi-line string"
                 (with-pyproject "[project]\nreadme-text = \"\"\"\n[tool.uv]\n\"\"\"\n"
                                 (fn [root]
                                   (expect (false? (uv-project? root))))))
             (it "detects a real [tool.uv] table, trailing comment and all"
                 (with-pyproject "[tool.uv]  # uv config\nmanaged = true\n"
                                 (fn [root]
                                   (expect (true? (uv-project? root))))))
             (it "detects a quoted [tool.\"uv\"] header"
                 (with-pyproject "[tool.\"uv\"]\nmanaged = true\n"
                                 (fn [root]
                                   (expect (true? (uv-project? root))))))
             (it "detects a [tool.uv.sources] subtable"
                 (with-pyproject
                   "[project]\nname = \"x\"\n\n[tool.uv.sources]\npkg = { path = \"x\" }\n"
                   (fn [root]
                     (expect (true? (uv-project? root))))))
             (it "detects an [[tool.uv.index]] array of tables"
                 (with-pyproject "[[tool.uv.index]]\nname = \"pypi\"\n"
                                 (fn [root]
                                   (expect (true? (uv-project? root))))))
             (it "still trusts a uv.lock next to a uv-free pyproject"
                 (let [root (tmp-dir)]
                   (try (spit (io/file root "pyproject.toml") "[project]\nname = \"x\"\n")
                        (spit (io/file root "uv.lock") "")
                        (expect (true? (uv-project? (.getPath root))))
                        (finally (cleanup root)))))
             (it "treats an UNPARSABLE pyproject as not-uv rather than throwing"
                 (with-pyproject "[project\nname = "
                                 (fn [root]
                                   (expect (false? (uv-project? root))))))
             (it "reports nothing for a directory with no pyproject at all"
                 (let [root (tmp-dir)]
                   (try (expect (false? (uv-project? (.getPath root)))) (finally (cleanup root))))))

;; ── live REPL subprocess ─────────────────────────────────────────────────────
(defdescribe
  repl-lifecycle-test
  (it "starts, evaluates, persists globals across evals, captures output + errors, stops"
      (when (has-python?)
        (let [dir (.getPath (tmp-dir))]
          (try (expect (= "up" (get (repl/start! test-session-id dir {}) "status")))
               ;; last expression's value is captured (REPL semantics)
               (expect (= "2" (get (repl/eval! test-session-id dir "1+1" 10000) "value")))
               ;; globals PERSIST across separate evals — a real session
               (repl/eval! test-session-id dir "x = 21" 10000)
               (expect (= "42" (get (repl/eval! test-session-id dir "x*2" 10000) "value")))
               ;; stdout is captured, not leaked
               (let [r (repl/eval! test-session-id dir "print('hi')" 10000)]
                 (expect (= "hi\n" (get r "out")))
                 (expect (get r "ok")))
               ;; an exception is captured, not thrown into Clojure
               (let [r (repl/eval! test-session-id dir "1/0" 10000)]
                 (expect (false? (get r "ok")))
                 (expect (re-find #"ZeroDivisionError" (str (get r "exc")))))
               (let [up (repl/status test-session-id dir)]
                 (expect (= "up" (get up "status")))
                 ;; ONE status shape for every language: a key rides only where
                 ;; it MEANS something
                 (expect (true? (get up "running")))
                 (expect (get up "pid")))
               (repl/stop! test-session-id dir)
               (let [down (repl/status test-session-id dir)]
                 (expect (= "down" (get down "status")))
                 (expect (not (contains? down "running")))
                 (expect (not (contains? down "pid")))
                 (expect (not (contains? down "cmd"))))
               (finally (repl/stop! test-session-id dir))))))
  ;; Regression, issue #repl-consistency: a second `repl_start` KILLED the live
  ;; Python process and spawned a new one, so the session's globals vanished while
  ;; the call reported plain success — where Clojure answered "already-running".
  ;; A live REPL is reused in EVERY language now, and the env it was started with
  ;; is part of its identity.
  (it "reuses a live REPL and refuses a start naming a different env"
      (when (has-python?)
        (let [dir (.getPath (tmp-dir))]
          (try (let [first-start (repl/start! test-session-id dir {"env" {"VIS_REPL_MARK" "one"}})]
                 (expect (= "started" (get first-start "result")))
                 ;; the env rides by NAME + digest, never by value
                 (expect (= ["VIS_REPL_MARK"] (keys (get first-start "env"))))
                 (expect (not= "one" (get (get first-start "env") "VIS_REPL_MARK")))
                 ;; and it really reached the child
                 (expect (re-find #"one"
                                  (str (get (repl/eval! test-session-id
                                                        dir
                                                        "import os; os.environ['VIS_REPL_MARK']"
                                                        10000)
                                            "value"))))
                 (repl/eval! test-session-id dir "vis_mark = 7" 10000)
                 (let [same (repl/start! test-session-id dir {"env" {"VIS_REPL_MARK" "one"}})]
                   (expect (= "already-running" (get same "result")))
                   ;; SAME process — the state the session stands on survived
                   (expect (= "7" (get (repl/eval! test-session-id dir "vis_mark" 10000) "value"))))
                 (let [refused (try
                                 (repl/start! test-session-id dir {"env" {"VIS_REPL_MARK" "two"}})
                                 nil
                                 (catch clojure.lang.ExceptionInfo e e))]
                   (expect (some? refused))
                   (expect (= :py/repl-env-mismatch (:type (ex-data refused))))
                   (expect (= ["VIS_REPL_MARK"] (:env (ex-data refused))))
                   ;; the refusal names the KEY, never the value
                   (expect (not (str/includes? (.getMessage refused) "two")))
                   (expect (str/includes? (.getMessage refused) "repl_stop"))))
               (finally (repl/stop! test-session-id dir))))))
  ;; Regression, issue #123: a pinned `vis-agent python` command rejected `-u`,
  ;; but start still reported an unusable process as up and exposed its driver source.
  (it "fails startup when the child cannot complete the ping handshake"
      (let [dir (.getPath (tmp-dir))]
        (try (let [result (with-redefs [interp/detect-command
                                        (constantly ["sh" "-c" "printf 'not-json\\n'; sleep 30"])]
                            (repl/start! test-session-id dir {}))]
               (expect (= "failed" (get result "status")))
               (expect (re-find #"invalid response" (get result "message")))
               (expect (= "<vis python driver>" (last (get result "cmd"))))
               (expect (= :py/no-repl
                          (try (repl/eval! test-session-id dir "1" 1000)
                               nil
                               (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
             (finally (repl/stop! test-session-id dir)))))
  ;; Regression, issue #repl-consistency: a Python start that died read by
  ;; `error` / `stderr` / `exit_code` while Clojure's read by `message` /
  ;; `log_tail` / `exit`, so a failed start could not be read the same way twice.
  (it "reports a dead launch by the keys EVERY language uses"
      (let [dir (.getPath (tmp-dir))]
        (try (let [result (with-redefs [interp/detect-command
                                        (constantly ["sh" "-c" "echo boom 1>&2; exit 3"])]
                            (repl/start! test-session-id dir {}))]
               (expect (= "failed" (get result "result")))
               (expect (= "failed" (get result "status")))
               (expect (string? (get result "message")))
               (expect (= 3 (get result "exit")))
               (expect (= ["boom"] (get result "log_tail")))
               ;; and NEVER by the old per-language names
               (expect (nil? (get result "error")))
               (expect (nil? (get result "stderr")))
               (expect (nil? (get result "exit_code"))))
             (finally (repl/stop! test-session-id dir)))))
  (it "eval before start fails closed with a clear error"
      (let [dir (str (.getPath (tmp-dir)) "-never-started")]
        (expect (= :py/no-repl
                   (try (repl/eval! test-session-id dir "1" 1000)
                        nil
                        (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))))

;; ── language-facade wiring ───────────────────────────────────────────────────
(defdescribe
  repl-activity-test
  (it
    "retains real Python program, streams and result through the facade and Activity"
    (when (has-python?)
      (let [root
            (tmp-dir)

            dir
            (.getCanonicalPath root)

            env
            {:workspace/root dir
             :session-id test-session-id
             :jail-policy-fn (constantly {:roots-fn (constantly [dir]) :net-enabled? false})
             :extensions (atom [{:ext/name "python"
                                 :ext/language-tools [{:language "python"
                                                       :repl-eval-fn core/py-repl-eval-fn}]}])}

            code
            "import sys\nprint('hello')\nprint('warning', file=sys.stderr)\n{'answer': 42}"]

        (try (core/py-start-repl-fn env "start" nil)
             (let [result
                   (:result (language-surface/repl-eval env "python" {"code" code}))

                   projection
                   (activity-fixture/result-fixture [[:repl_eval "REPL" result nil]])

                   blocks
                   (get-in projection ["rows" 0 "presentation" "content"])]

               (expect (= "python" (get result "language")))
               (expect (= ["Program" "Stdout" "Stderr" "Result"]
                          (mapv #(get % "text") (filter #(= "heading" (get % "type")) blocks))))
               (expect (= [code "hello\n" "warning\n" "{'answer': 42}"]
                          (mapv #(get % "text") (filter #(= "code" (get % "type")) blocks)))))
             (finally (repl/stop! test-session-id dir) (cleanup root)))))))

(defdescribe
  facade-test
  (it "repl_eval requires explicit repl and then returns the value"
      (when (has-python?)
        (let [root
              (tmp-dir)

              dir
              (.getCanonicalPath root)

              env
              {:workspace/root (.getPath root) :session-id test-session-id}]

          (try (expect (= :py/no-repl
                          (try (core/py-repl-eval-fn env "3 * 7")
                               nil
                               (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
               (core/py-start-repl-fn env "start" nil)
               (let [r (core/py-repl-eval-fn env "3 * 7")]
                 (expect (:success? r))
                 (expect (= "21" (get-in r [:result "value"]))))
               (finally (repl/stop! test-session-id dir))))))
  (it "shows a home-relative, retryable cwd when no REPL is running"
      (let [home
            (System/getProperty "user.home")

            cwd
            "~/vis-python-not-running"

            env
            {:workspace/root home :session-id test-session-id}

            msg
            (try (core/py-repl-eval-fn env {"code" "1 + 1" "cwd" cwd})
                 nil
                 (catch clojure.lang.ExceptionInfo e (.getMessage e)))]

        (expect (str/includes? msg cwd))
        (expect (not (str/includes? msg home)))))
  (it "repl status/stop lifecycle ops route through the manager"
      (when (has-python?)
        (let [root
              (tmp-dir)

              dir
              (.getCanonicalPath root)

              env
              {:workspace/root (.getPath root) :session-id test-session-id}]

          (try
            (expect (:success? (core/py-start-repl-fn env "start" nil)))
            (expect (= "up" (get-in (core/py-start-repl-fn env "status" nil) [:result "status"])))
            (core/py-start-repl-fn env "stop" nil)
            (expect (= "down" (get-in (core/py-start-repl-fn env "status" nil) [:result "status"])))
            (finally (repl/stop! test-session-id dir)))))))

;; Regression, issue #208: sessions at one cwd must not share interpreter state
;; or let a lifecycle/resource cleanup stop another session's Python process.
(defn- with-session-repls
  [f]
  (let [root
        (tmp-dir)

        dir
        (.getCanonicalPath root)

        envs
        (mapv (fn [_]
                {:workspace/root dir :session-id (str (java.util.UUID/randomUUID))})
              (range 2))]

    (try (doseq [env envs]
           (process-jail/register-session-jail!
             (:session-id env)
             (constantly {:roots-fn (constantly [dir]) :net-enabled? false :disabled? true})))
         (apply f envs)
         (finally (doseq [env envs]
                    (resources/stop-all! (:session-id env))
                    (core/py-start-repl-fn env "stop" nil)
                    (process-jail/unregister-session-jail! (:session-id env)))
                  (cleanup root)))))

(defdescribe
  session-isolation-test
  (it
    "isolates discovery, globals and explicit stop at the same canonical cwd"
    (when (has-python?)
      (with-session-repls
        (fn [env-a env-b]
          (let [a (:result (core/py-start-repl-fn env-a "start" nil))]
            (expect (= "started" (get a "result")))
            (core/py-repl-eval-fn env-a "session_mark = 'A'")
            (expect (= "down"
                       (get-in (core/py-start-repl-fn env-b "status" nil) [:result "status"])))
            (expect (empty? (resources/list-resources (:session-id env-b))))
            (expect (= :py/no-repl
                       (try (core/py-repl-eval-fn env-b "session_mark")
                            nil
                            (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
            (expect (= "not-managed"
                       (get-in (core/py-start-repl-fn env-b "stop" nil) [:result "result"])))
            (expect (= "'A'"
                       (get-in (core/py-repl-eval-fn env-a "session_mark") [:result "value"])))
            (let [b (:result (core/py-start-repl-fn env-b "start" {"cwd" "."}))
                  reused (:result (core/py-start-repl-fn env-a "start" {"cwd" "."}))]

              (expect (= "started" (get b "result")))
              (expect (not= (get a "pid") (get b "pid")))
              (expect (= "already-running" (get reused "result")))
              (expect (= (get a "pid") (get reused "pid")))
              (expect (= (get a "pid")
                         (get (repl/status (:session-id env-a) (str (:workspace/root env-a) "/."))
                              "pid")))
              (expect (= (get a "id") (get reused "id")))
              (expect (= "False"
                         (get-in (core/py-repl-eval-fn env-b "'session_mark' in globals()")
                                 [:result "value"])))
              (core/py-repl-eval-fn env-b "session_mark = 'B'")
              (expect (= "'A'"
                         (get-in (core/py-repl-eval-fn env-a "session_mark") [:result "value"])))
              (expect (= "stopped"
                         (get-in (core/py-start-repl-fn env-a "stop" nil) [:result "result"])))
              (expect (empty? (resources/list-resources (:session-id env-a))))
              (expect (= (get b "pid")
                         (get-in (core/py-start-repl-fn env-b "status" nil) [:result "pid"])))
              (expect (= "'B'"
                         (get-in (core/py-repl-eval-fn env-b "session_mark")
                                 [:result "value"])))))))))
  (it
    "scopes resource stop and session teardown even when resource IDs match"
    (when (has-python?)
      (doseq [stop-mode [:resource :session]]
        (with-session-repls
          (fn [env-a env-b]
            (let [opts {"id" "shared-repl-name"}
                  a (:result (core/py-start-repl-fn env-a
                                                    "start"
                                                    (assoc opts "env" {"VIS_REPL_MARK" "A"})))
                  b (:result (core/py-start-repl-fn env-b
                                                    "start"
                                                    (assoc opts "env" {"VIS_REPL_MARK" "B"})))
                  sid-a (:session-id env-a)
                  sid-b (:session-id env-b)]

              (expect (not= (get a "pid") (get b "pid")))
              (expect (= (get a "pid")
                         (get (resources/get-resource sid-a "shared-repl-name") "pid")))
              (expect (= (get b "pid")
                         (get (resources/get-resource sid-b "shared-repl-name") "pid")))
              (core/py-repl-eval-fn env-b "session_mark = 208")
              (expect (= "'A'"
                         (get-in (core/py-repl-eval-fn env-a
                                                       "import os; os.environ['VIS_REPL_MARK']")
                                 [:result "value"])))
              (expect (= "'B'"
                         (get-in (core/py-repl-eval-fn env-b
                                                       "import os; os.environ['VIS_REPL_MARK']")
                                 [:result "value"])))
              (expect (= :stopped
                         (:result (case stop-mode
                                    :resource
                                    (resources/stop! sid-a "shared-repl-name")

                                    :session
                                    (first (resources/stop-all! sid-a))))))
              (expect (= "down"
                         (get-in (core/py-start-repl-fn env-a "status" opts) [:result "status"])))
              (expect (empty? (resources/list-resources sid-a)))
              (expect (= (get b "pid")
                         (get-in (core/py-start-repl-fn env-b "status" opts) [:result "pid"])))
              (expect (= ["shared-repl-name"]
                         (mapv #(get % "id") (resources/list-resources sid-b))))
              (expect (= "208"
                         (get-in (core/py-repl-eval-fn env-b "session_mark")
                                 [:result "value"]))))))))))

(defdescribe
  session-failure-isolation-test
  (it
    "keeps another session usable after failed startup or protocol teardown"
    (when (has-python?)
      (doseq [failure-mode [:handshake :closed :protocol]]
        (with-session-repls
          (fn [env-a env-b]
            (let [b (:result (core/py-start-repl-fn env-b "start" nil))]
              (core/py-repl-eval-fn env-b "session_mark = 208")
              (case failure-mode
                :handshake
                (let [failed (with-redefs [interp/detect-command
                                           (constantly ["sh" "-c"
                                                        "echo failed-start 1>&2; exit 3"])]
                               (:result (core/py-start-repl-fn env-a "start" nil)))]
                  (expect (= "failed" (get failed "result")))
                  (expect (= 3 (get failed "exit"))))

                (:closed :protocol)
                (do
                  (core/py-start-repl-fn env-a "start" nil)
                  (let
                    [error
                     (try
                       (core/py-repl-eval-fn
                         env-a
                         (case failure-mode
                           :closed
                           "import os; os._exit(0)"

                           :protocol
                           "import sys; sys.__stdout__.write('not-json\\n'); sys.__stdout__.flush()"))
                       nil
                       (catch clojure.lang.ExceptionInfo e (ex-data e)))]
                    (expect (= (case failure-mode
                                 :closed
                                 :py/closed

                                 :protocol
                                 :py/protocol-error)
                               (:type error)))
                    (expect (= (:session-id env-a) (:session-id error))))))
              (expect (= "down"
                         (get-in (core/py-start-repl-fn env-a "status" nil) [:result "status"])))
              (expect (= "not-managed"
                         (get-in (core/py-start-repl-fn env-a "stop" nil) [:result "result"])))
              (expect (= (get b "pid")
                         (get-in (core/py-start-repl-fn env-b "status" nil) [:result "pid"])))
              (expect (= "208"
                         (get-in (core/py-repl-eval-fn env-b "session_mark")
                                 [:result "value"]))))))))))

(def ^:private activation-fn @#'core/activation-fn)

(defdescribe activation-test
             (it "activates on a pyproject.toml workspace"
                 (let [root (tmp-dir)]
                   (try (spit (io/file root "pyproject.toml") "[project]\nname = \"x\"\n")
                        (expect (true? (activation-fn {:workspace/root (.getAbsolutePath root)})))
                        (finally (cleanup root)))))
             (it "activates on a loose .py file"
                 (let [root (tmp-dir)]
                   (try (spit (io/file root "script.py") "print(1)\n")
                        (expect (true? (activation-fn {:workspace/root (.getAbsolutePath root)})))
                        (finally (cleanup root)))))
             (it "stays dark on a non-Python workspace"
                 (let [root (tmp-dir)]
                   (try (spit (io/file root "README.md") "# nope\n")
                        (expect (false? (activation-fn {:workspace/root (.getAbsolutePath root)})))
                        (finally (cleanup root)))))
             (it "stays dark with no :workspace/root" (expect (false? (activation-fn {})))))

(defdescribe
  value-representation-test
  "Real Python objects come back as JSON-safe STRUCTURED data, not just a repr;
   objects that can't be serialized stay LIVE in the REPL and are described."
  (it "represents dicts / lists / sets as nested data"
      (when (has-python?)
        (let [dir (.getPath (tmp-dir))]
          (try (repl/start! test-session-id dir {})
               (expect (= {"a" 1 "b" [2 3]}
                          (get (repl/eval! test-session-id dir "{'a': 1, 'b': [2,3]}" 10000)
                               "data")))
               (expect (= [1 2 3]
                          (sort (get (repl/eval! test-session-id dir "{3,1,2}" 10000) "data"))))
               (expect (= "dict" (get (repl/eval! test-session-id dir "{}" 10000) "type")))
               (finally (repl/stop! test-session-id dir))))))
  (it "represents a dataclass / custom object as a field map tagged with __type__"
      (when (has-python?)
        (let [dir (.getPath (tmp-dir))]
          (try (repl/start! test-session-id dir {})
               (repl/eval!
                 test-session-id
                 dir
                 "from dataclasses import dataclass\n@dataclass\nclass P:\n    x: int\n    y: int"
                 10000)
               (expect (= {"x" 3 "y" 4 "__type__" "P"}
                          (get (repl/eval! test-session-id dir "P(3,4)" 10000) "data")))
               (finally (repl/stop! test-session-id dir))))))
  (it "an OPAQUE object stays LIVE + is described (type/repr/attrs), not lost"
      (when (has-python?)
        (let [dir (.getPath (tmp-dir))]
          (try (repl/start! test-session-id dir {})
               (let [d (get (repl/eval! test-session-id dir "(i for i in range(3))" 10000) "data")]
                 (expect (get d "__opaque__"))
                 (expect (= "generator" (get d "__type__")))
                 (expect (string? (get d "__repr__"))))
               ;; bind it, then keep using it across evals — globals persist
               (repl/eval! test-session-id dir "g = (i*i for i in range(4))" 10000)
               (expect (= "0" (get (repl/eval! test-session-id dir "next(g)" 10000) "value")))
               (expect (= "1" (get (repl/eval! test-session-id dir "next(g)" 10000) "value")))
               (expect (= "4" (get (repl/eval! test-session-id dir "next(g)" 10000) "value")))
               (finally (repl/stop! test-session-id dir)))))))
