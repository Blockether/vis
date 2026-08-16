(ns com.blockether.vis.ext.language-python.repl-test
  "Managed Python REPL: interpreter detection, subprocess lifecycle + persistent
   eval, and the language-facade wiring. The live-subprocess tests SKIP when no
   Python is on PATH so CI without Python stays green."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.language-python.core :as core]
            [com.blockether.vis.ext.language-python.interpreter :as interp]
            [com.blockether.vis.ext.language-python.repl-manager :as repl]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.process-jail :as process-jail]
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
      (let
        [root
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
      (let
        [root
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

;; ── `python.interpreter` / `python.runner` pinned in merged config ───────────
(defdescribe
  interpreter-pin-test
  "A workspace whose only sanctioned invocation is undetectable (`vis-agent
   python`, a wrapper script) pins it instead (Blockether/vis#98)."
  (it "takes a vector as the argv prefix, verbatim"
      (expect (= ["vis-agent" "python"]
                 (interp/pinned-command "/proj"
                                        {"python" {"interpreter" ["vis-agent" "python"]}}))))
  (it "takes a bare string as ONE argument, never word-split"
      (expect (= ["my python"]
                 (interp/pinned-command "/proj" {"python" {"interpreter" "my python"}}))))
  (it "resolves a path-like pin against the project dir"
      (expect (= ["/proj/.venv/bin/python"]
                 (interp/pinned-command "/proj" {"python" {"interpreter" [".venv/bin/python"]}}))))
  (it "expands ~ in a pinned path"
      (expect (= [(str (System/getProperty "user.home") "/bin/py")]
                 (interp/pinned-command "/proj" {"python" {"interpreter" "~/bin/py"}}))))
  (it "is nil without a pin, and for blank entries"
      (expect (nil? (interp/pinned-command "/proj" {})))
      (expect (nil? (interp/pinned-command "/proj" {"python" {"interpreter" ["" "  "]}}))))
  (it "prefers the pin over detection"
      (with-redefs
        [config/load-config-raw (constantly {"python" {"interpreter" ["vis-agent" "python"]}})]
        (expect (= ["vis-agent" "python"]
                   (interp/resolve-command (System/getProperty "java.io.tmpdir"))))))
  (it "reads python.runner, ignoring anything that is not a backend"
      (expect (= "project" (interp/pinned-runner {"python" {"runner" "Project"}})))
      (expect (= "graalpy" (interp/pinned-runner {"python" {"runner" "graalpy"}})))
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
          (try (expect (= "up" (get (repl/start! dir {:session-id test-session-id}) "status")))
               ;; last expression's value is captured (REPL semantics)
               (expect (= "2" (get (repl/eval! dir "1+1" 10000) "value")))
               ;; globals PERSIST across separate evals — a real session
               (repl/eval! dir "x = 21" 10000)
               (expect (= "42" (get (repl/eval! dir "x*2" 10000) "value")))
               ;; stdout is captured, not leaked
               (let [r (repl/eval! dir "print('hi')" 10000)]
                 (expect (= "hi\n" (get r "out")))
                 (expect (get r "ok")))
               ;; an exception is captured, not thrown into Clojure
               (let [r (repl/eval! dir "1/0" 10000)]
                 (expect (false? (get r "ok")))
                 (expect (re-find #"ZeroDivisionError" (str (get r "exc")))))
               (expect (= "up" (get (repl/status dir) "status")))
               (repl/stop! dir)
               (expect (= "down" (get (repl/status dir) "status")))
               (finally (repl/stop! dir))))))
  ;; Regression, issue #123: a pinned `vis-agent python` command rejected `-u`,
  ;; but start still reported an unusable process as up and exposed its driver source.
  (it "fails startup when the child cannot complete the ping handshake"
      (let [dir (.getPath (tmp-dir))]
        (try (let
               [result (with-redefs
                         [interp/resolve-command (constantly ["sh" "-c"
                                                              "printf 'not-json\\n'; sleep 30"])]
                         (repl/start! dir {:session-id test-session-id}))]
               (expect (= "failed" (get result "status")))
               (expect (re-find #"invalid response" (get result "error")))
               (expect (= "<vis python driver>" (last (get result "cmd"))))
               (expect (= :py/no-repl
                          (try (repl/eval! dir "1" 1000)
                               nil
                               (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
             (finally (repl/stop! dir)))))
  (it "eval before start fails closed with a clear error"
      (let [dir (str (.getPath (tmp-dir)) "-never-started")]
        (expect (= :py/no-repl
                   (try (repl/eval! dir "1" 1000)
                        nil
                        (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))))

;; ── language-facade wiring ───────────────────────────────────────────────────
(defdescribe
  facade-test
  (it "repl_eval requires explicit repl and then returns the value"
      (when (has-python?)
        (let
          [root
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
               (finally (repl/stop! dir))))))
  (it "shows a home-relative, retryable cwd when no REPL is running"
      (let
        [home
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
        (let
          [root
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
            (finally (repl/stop! dir)))))))

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
          (try (repl/start! dir {:session-id test-session-id})
               (expect (= {"a" 1 "b" [2 3]}
                          (get (repl/eval! dir "{'a': 1, 'b': [2,3]}" 10000) "data")))
               (expect (= [1 2 3] (sort (get (repl/eval! dir "{3,1,2}" 10000) "data"))))
               (expect (= "dict" (get (repl/eval! dir "{}" 10000) "type")))
               (finally (repl/stop! dir))))))
  (it "represents a dataclass / custom object as a field map tagged with __type__"
      (when (has-python?)
        (let [dir (.getPath (tmp-dir))]
          (try (repl/start! dir {:session-id test-session-id})
               (repl/eval!
                 dir
                 "from dataclasses import dataclass\n@dataclass\nclass P:\n    x: int\n    y: int"
                 10000)
               (expect (= {"x" 3 "y" 4 "__type__" "P"}
                          (get (repl/eval! dir "P(3,4)" 10000) "data")))
               (finally (repl/stop! dir))))))
  (it "an OPAQUE object stays LIVE + is described (type/repr/attrs), not lost"
      (when (has-python?)
        (let [dir (.getPath (tmp-dir))]
          (try (repl/start! dir {:session-id test-session-id})
               (let [d (get (repl/eval! dir "(i for i in range(3))" 10000) "data")]
                 (expect (get d "__opaque__"))
                 (expect (= "generator" (get d "__type__")))
                 (expect (string? (get d "__repr__"))))
               ;; bind it, then keep using it across evals — globals persist
               (repl/eval! dir "g = (i*i for i in range(4))" 10000)
               (expect (= "0" (get (repl/eval! dir "next(g)" 10000) "value")))
               (expect (= "1" (get (repl/eval! dir "next(g)" 10000) "value")))
               (expect (= "4" (get (repl/eval! dir "next(g)" 10000) "value")))
               (finally (repl/stop! dir)))))))
