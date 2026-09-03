(ns com.blockether.vis.internal.python-test-runner
  "Runs an extension author's Python tests (`test_*.py` / `*_test.py`) through
   the built-in `pytest`-compat shim, each in its own TRUSTED Python session
   (same trust level as the extension it covers). Tests import the extension's
   own package through the SAME `sys.path` sugar the loader gives `extension.py`,
   so an author ships real Python tests next to the code and runs them with the
   project's own tooling. Pure Python end to end — the shim is stdlib-only, and
   the test host refuses session live views so a test cannot publish artifacts.

   Split out of `python-extensions` (which owns loading/registration) so the
   runner is a single, testable responsibility. It depends on that namespace's
   trusted-context builder; the reverse `/test` wiring is resolved lazily there
   to avoid a require cycle.

   The source of truth for the outcome is the shim's PER-TEST record list
   (nodeid, outcome, message). Counts and pass/fail are DERIVED from those
   records on the host side — never a separate tally that could drift, and
   never scraped from stdout."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.env-python :as env]
            [com.blockether.vis.internal.python-extensions :as pyx]
            [com.blockether.vis.internal.python-worker :as pyext]
            [com.blockether.vis.internal.python-runtime :as python-runtime])
  (:import [java.io File]))

(set! *warn-on-reflection* true)

;; A test's outcome may fail with a longrepr that literally contains "9 passed";
;; joining records with control chars that CANNOT occur in a nodeid or a Python
;; traceback keeps the boundary string unambiguous. \u001e (RS) separates
;; records, \u001f (US) separates the three fields.
(def ^:private record-sep "\u001e")

(def ^:private field-sep "\u001f")

(defn- ensure-pytest!
  "Make the REAL `pytest` importable in `session`, installing it ONCE into the
   sandbox's own packages directory when the machine has never had it. Answers
   nil on success and the reason on failure — a missing test runner is a result,
   never a crash.

   The retry invalidates the import caches first: a path entry remembers the
   directory listing it saw, so a package pip wrote into a directory that was
   ALREADY on `sys.path` stays invisible for the life of the process. Measured
   on a machine that had never installed pytest — the install succeeded and the
   very next import still raised `ModuleNotFoundError`."
  [^String session]
  (try (pyext/exec! pyext/shared-key session "import pytest")
       nil
       (catch Throwable _
         (let [{:keys [exit out]} (python-runtime/pip-install! ["pytest"])]
           (if (zero? (long (or exit 1)))
             (try (pyext/exec! pyext/shared-key
                               session
                               "import importlib; importlib.invalidate_caches(); import pytest")
                  nil
                  (catch Throwable t (ex-message t)))
             (str "pytest could not be installed: " out))))))

(defn- walk-py
  "Every `*.py` under `d`, recursively, name-sorted."
  [^File d]
  (when (.isDirectory d)
    (mapcat (fn [^File f]
              (cond (.isDirectory f) (walk-py f)
                    (str/ends-with? (.getName f) ".py") [f]
                    :else nil))
            (sort-by #(.getName ^File %) (.listFiles d)))))

(defn- discover-tests
  "`[scan-dir test-file]` pairs across the given roots, deduped on the test file's
   canonical path.

   A DIRECTORY root contributes every `test_*.py` / `*_test.py` at any depth
   (top-level single-file siblings AND inside package extensions). A root that
   names a `*.py` FILE directly IS that test file, whatever it is called — an
   explicitly named target is honored (pytest behaves the same) instead of
   silently discovering nothing; its own directory is the scan root."
  [dirs]
  (->> (for [^File e
             (map io/file dirs)

             :when (.exists e)
             [^File d ^File f]
             (if (.isDirectory e)
               (for [^File f
                     (walk-py e)

                     :when (pyx/test-file? f)]

                 [e f])
               (when (str/ends-with? (.getName e) ".py")
                 [[(.getParentFile (.getCanonicalFile e)) e]]))]

         [d f])
       (reduce (fn [[seen acc] [_ ^File f :as pair]]
                 (let [p (.getCanonicalPath f)]
                   (if (seen p) [seen acc] [(conj seen p) (conj acc pair)])))
               [#{} []])
       second))

(defn- test-sys-path
  "sys.path entries for a test file, in order: `extra` (the project's own
   declared import roots, e.g. a `src` layout) first, then the test file's own
   dir and every ancestor up to and INCLUDING the extension scan dir — so
   `from mypkg.core import x` (package root), `from core import x` (same dir)
   and `import mypkg` (declared `src` root) all resolve regardless of how deep
   the test sits.

   The driver inserts these at `sys.path[0]` in order, so entries listed FIRST
   end up with the LOWEST precedence: the project roots never shadow a module
   sitting next to the test."
  ([^File scan-dir ^File test-file] (test-sys-path scan-dir test-file nil))
  ([^File scan-dir ^File test-file extra]
   (let [scan (.getCanonicalPath scan-dir)]
     (vec (distinct (concat (remove str/blank? (map str extra))
                            (loop [^File d (.getParentFile (.getCanonicalFile test-file))
                                   acc []]

                              (if (nil? d)
                                acc
                                (let [p (.getCanonicalPath d)
                                      acc (conj acc p)]

                                  (if (= p scan) acc (recur (.getParentFile d) acc)))))))))))

(def ^:private run-test-src
  "Python driver: prepend the bound `sys.path` roots, then
   run REAL `pytest` over the one test file with a collector plugin attached.
   Pytest's own output lands in `__vis_test_output__`, its exit code in
   `__vis_test_rc__`, and — the source of truth — the collector's PER-TEST record
   list in `__vis_test_report__` (records RS-joined, fields US-joined: nodeid,
   outcome, message).

   `sys.path` and `sys.modules` are PROCESS state, not session state: one
   embedded CPython serves every session, so a test module left behind makes
   pytest refuse the NEXT file of the same basename (`import file mismatch`).
   The driver restores both, which is what a separate interpreter per file used
   to do for free."
  (str "import sys as __vis_ts__, io as __vis_tio__\n" "__vis_path0__ = list(__vis_ts__.path)\n"
       "__vis_mods0__ = set(__vis_ts__.modules)\n" "for __vis_tp__ in __vis_test_paths__:\n"
       "    if __vis_tp__ and __vis_tp__ not in __vis_ts__.path:\n"
       "        __vis_ts__.path.insert(0, __vis_tp__)\n"
       "import pytest as __vis_pt__\n" "class __VisReports__:\n"
       "    def __init__(self):\n" "        self.records = []\n"
       "    def pytest_runtest_logreport(self, report):\n"
       ;; One record per test: the CALL phase is the verdict, and a setup or
       ;; teardown that fails is an error the call phase never reports.
       "        if report.when == 'call':\n"
       "            outcome = report.outcome\n" "        elif report.outcome != 'passed':\n"
       "            outcome = 'error'\n" "        else:\n"
       "            return\n" "        self.records.append((report.nodeid, outcome,"
       " str(report.longrepr) if report.longrepr else ''))\n" "__vis_col__ = __VisReports__()\n"
       "__vis_tbuf__ = __vis_tio__.StringIO()\n" "__vis_told__ = __vis_ts__.stdout\n"
       "__vis_terr__ = __vis_ts__.stderr\n" "__vis_ts__.stdout = __vis_tbuf__\n"
       "__vis_ts__.stderr = __vis_tbuf__\n" "try:\n"
       "    __vis_test_rc__ = int(__vis_pt__.main("
       "['-q', '-p', 'no:cacheprovider', __vis_test_file__],"
       " plugins=[__vis_col__]))\n" "finally:\n"
       "    __vis_ts__.stdout = __vis_told__\n" "    __vis_ts__.stderr = __vis_terr__\n"
       "    for __vis_m__ in [__vis_k__ for __vis_k__ in list(__vis_ts__.modules)"
       " if __vis_k__ not in __vis_mods0__]:\n"
       "        __vis_ts__.modules.pop(__vis_m__, None)\n"
       "    __vis_ts__.path[:] = __vis_path0__\n"
       "__vis_test_output__ = __vis_tbuf__.getvalue()\n" "__vis_test_report__ = chr(30).join("
       "str(__vis_nid__) + chr(31) + str(__vis_oc__) + chr(31) + str(__vis_msg__)"
       " for (__vis_nid__, __vis_oc__, __vis_msg__) in __vis_col__.records)\n"))

(defn- short-nodeid
  "pytest's nodeid is a path relative to whatever it picked as its rootdir, so
   the same test reads `foo_test.py::test_x` here and
   `../../../tmp/x/foo_test.py::test_x` from another working directory. Every
   record already carries its absolute `:file`, so keep the file's NAME and the
   test path after it."
  [nodeid]
  (let [[path & inner] (str/split (or nodeid "") #"::")]
    (str/join "::" (cons (last (str/split path #"/")) inner))))

(defn- parse-report
  "Parse the shim's serialized per-test record list into
   `[{:nodeid :outcome :message}]`. `error` → `:errored`. This is the ONE place
   an outcome becomes host data — counts are derived from it, so they cannot
   disagree with what actually ran."
  [^String s]
  (into []
        (comp (remove str/blank?)
              (map (fn [rec]
                     (let [[nodeid outcome message] (str/split rec (re-pattern field-sep) 3)]
                       {:nodeid (short-nodeid nodeid)
                        :outcome (if (= outcome "error") :errored (keyword outcome))
                        :message (or message "")}))))
        (str/split (or s "") (re-pattern record-sep))))

(defn- failing? [tests] (boolean (some (comp #{:failed :errored} :outcome) tests)))

(defn- run-test-file!
  "Run ONE test file in a fresh trusted session: bootstrap the `vis` module, make
   sure `pytest` is there, then drive `run-test-src`. `sys-path` is the extra
   import roots the project declares (a `src` layout), added below the test's
   own dirs. Returns `{:file :rc :ok? :output :tests}` where `:tests` is the
   per-test record list. Never throws — a broken test file is one `:errored`
   result, never a host crash."
  [sys-path ^File scan-dir ^File test-file]
  (let [path
        (.getCanonicalPath test-file)

        paths
        (test-sys-path scan-dir test-file sys-path)

        session
        (pyx/build-context (.getName test-file))]

    (try (pyx/bind-test-host! session (.getName test-file))
         (pyext/exec! pyext/shared-key session pyx/bootstrap-python)
         (when-let [missing (ensure-pytest! session)]
           (throw (ex-info missing {:file path})))
         ;; The two inputs cross as JSON the guest PARSES: pasting JSON straight
         ;; into Python source would keep its `\\/` escapes verbatim and break
         ;; every path in it.
         (pyext/exec! pyext/shared-key
                      session
                      (str "__vis_test_paths__ = "
                           (env/py-json-literal (vec paths))
                           "\n"
                           "__vis_test_file__ = "
                           (env/py-json-literal path)
                           "\n"))
         (pyext/exec! pyext/shared-key session run-test-src)
         (let [outcome
               (json/read-json (pyext/run pyext/shared-key
                                          session
                                          (str "{'report': __vis_test_report__,"
                                               " 'rc': __vis_test_rc__,"
                                               " 'output': __vis_test_output__}"))
                               :key-fn
                               identity)

               tests
               (parse-report (str (get outcome "report")))]

           {:file path
            :rc (int (get outcome "rc" -1))
            :ok? (not (failing? tests))
            :output (str (get outcome "output"))
            :tests tests})
         (catch Throwable t
           {:file path
            :rc -1
            :ok? false
            :output ""
            :tests [{:nodeid (.getName test-file) :outcome :errored :message (ex-message t)}]
            :error (ex-message t)})
         (finally (pyx/close-context! session)))))

(defn test-python-extensions!
  "Discover and run every Python test (`test_*.py` / `*_test.py`) across the
   extension dirs (default: `~/.vis/extensions` and `<cwd>/.vis/extensions`),
   each in its own TRUSTED session driving real `pytest`, which the sandbox
   installs on first use. Tests import the extension's own package through the
   `sys.path` sugar,
   exactly like `extension.py` does.

   Returns `{:files n :ok? bool :passed n :failed n :errored n :skipped n
   :tests [{:file :nodeid :outcome :message}] :results [{:file :ok? :tests …}]}`.
   Counts are DERIVED from `:tests` (the flat per-test list) — the single source
   of truth. Never throws: a file that blows up at import is one `:errored`
   result, not a crash.

   `:sys-path` adds extra import roots (the project's own declared `src` layout)
   below each test's own dirs, so a test that imports the package under test
   resolves it the way the project's packaging metadata says it should."
  ([] (test-python-extensions! nil))
  ([{:keys [dirs sys-path]}]
   (let [dirs
         (or dirs (pyx/default-extension-dirs))

         pairs
         (discover-tests dirs)

         results
         (mapv (fn [[d f]]
                 (run-test-file! sys-path d f))
               pairs)

         tests
         (vec (for [r
                    results

                    t
                    (:tests r)]

                (assoc t :file (:file r))))

         counts
         (frequencies (map :outcome tests))]

     (merge {:files (count results) :ok? (every? :ok? results) :tests tests :results results}
            (select-keys counts [:passed :failed :errored :skipped :xfailed :xpassed])))))

(defn- rel-name
  "Short display name for a test file: the last two path segments (package +
   file) so `.../my_ext/test_core.py` reads as `my_ext/test_core.py`."
  [^String path]
  (let [segs (str/split path #"/")]
    (str/join "/" (take-last 2 segs))))

(defn- first-line
  "First non-blank line of `s`, trimmed — the assertion detail's headline."
  [^String s]
  (->> (str/split-lines (or s ""))
       (map str/trim)
       (remove str/blank?)
       first))

(defn ^:no-doc render-test-report
  "Render a `test-python-extensions!` result into a human-readable report: a
   one-line summary (`✓/✗ N file(s): P passed, F failed, …`), then per FILE a
   `✓/✗ <file>` line, then per TEST a `✓/✗/s <nodeid>` line (failures carry the
   first line of the assertion detail). Pure — the renderer for `/test`."
  [{:keys [files passed failed errored skipped ok? results] :as res}]
  (cond (:error res) (str "✗ Python extension tests could not run: " (:error res))
        (zero? (long (or files 0))) "No Python extension tests found (test_*.py / *_test.py)."
        :else
        (let [summary
              (str (if ok? "✓" "✗")
                   " "
                   files
                   " file(s): "
                   (or passed 0)
                   " passed"
                   (when (pos? (long (or failed 0))) (str ", " failed " failed"))
                   (when (pos? (long (or errored 0))) (str ", " errored " errored"))
                   (when (pos? (long (or skipped 0))) (str ", " skipped " skipped")))

              mark
              (fn [outcome]
                (case outcome
                  :passed
                  "✓"

                  (:failed :errored)
                  "✗"

                  :skipped
                  "s"

                  :xfailed
                  "x"

                  :xpassed
                  "X"

                  "?"))

              file-block
              (fn [{:keys [file ok? tests error]}]
                (into
                  [(str "  " (if ok? "✓" "✗") " " (rel-name file) (when error (str " — " error)))]
                  (map (fn [{:keys [nodeid outcome message]}]
                         (str "      "
                              (mark outcome)
                              " "
                              nodeid
                              (when (and (#{:failed :errored} outcome) (seq (first-line message)))
                                (str " — " (first-line message))))))
                  tests))]

          (str/join "\n" (cons summary (mapcat file-block results))))))

(defn ^:no-doc run-and-report
  "Run every Python extension test and return `{:result <map> :report <string>}`
   for the `/test` slash command."
  [opts]
  (let [result (test-python-extensions! opts)]
    {:result result :report (render-test-report result)}))

(defn ^:no-doc test-slash
  "`/test` — run every Python extension test and report the outcome inline."
  [_ctx]
  (let [{:keys [result report]} (run-and-report nil)]
    {:slash/status (if (:ok? result) :ok :error)
     :slash/title (if (zero? (long (or (:files result) 0)))
                    "No Python extension tests found"
                    (str "Python extension tests: " (if (:ok? result) "all passed" "failures")))
     :slash/body report}))

