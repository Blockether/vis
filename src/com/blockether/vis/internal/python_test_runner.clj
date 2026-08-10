(ns com.blockether.vis.internal.python-test-runner
  "Runs an extension author's Python tests (`test_*.py` / `*_test.py`) through
   the built-in `pytest`-compat shim, each in its own GraalPy context.

   A test runs BESIDE the extension's code, never inside its trust. Test files
   are bytes nobody admitted: they reach this process as a PATH IN A TOOL CALL
   (`run_tests`, `/test`, `vis-agent extension test`), they are skipped by the
   loader's scan on purpose, and no `/reload` ever saw them. So the context is
   built with `:trust :none` — no subprocess boundary — and `vis.shell` /
   `vis.jailed_shell` refuse (`pyx/bind-untrusted-host!`). Everything an
   author's test legitimately needs stays bound.

   Tests import the extension's own package through the SAME `sys.path` sugar
   the loader gives `extension.py`, so an author ships real Python tests next to
   the code and runs them with the project's own tooling. Pure Python end to
   end — the shim is stdlib-only, no host bridge, no pip.

   Split out of `python-extensions` (which owns loading/registration) so the
   runner is a single, testable responsibility. It depends on that namespace's
   trusted-context builder; the reverse wiring (`/test` slash + `vis-agent extension test`
   CLI) is resolved lazily there to avoid a require cycle.

   The source of truth for the outcome is the shim's PER-TEST record list
   (nodeid, outcome, message). Counts and pass/fail are DERIVED from those
   records on the host side — never a separate tally that could drift, and
   never scraped from stdout."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.python-extensions :as pyx])
  (:import [java.io File]
           [org.graalvm.polyglot Context]))

(set! *warn-on-reflection* true)

;; A test's outcome may fail with a longrepr that literally contains "9 passed";
;; joining records with control chars that CANNOT occur in a nodeid or a Python
;; traceback keeps the boundary string unambiguous. \u001e (RS) separates
;; records, \u001f (US) separates the three fields.
(def ^:private record-sep "\u001e")

(def ^:private field-sep "\u001f")

(defn- pytest-shim-src
  "The `pytest`-compat shim's Python source, pulled from the live extension
   registry (`extension/sandbox-shims`, keyed by `:shim/name` \"pytest\") so we
   avoid a compile-time dependency on `shim-pytest` — which would cycle back
   through `vis.core`. nil when the shim extension isn't registered."
  []
  (some (fn [shim]
          (when (= "pytest" (:shim/name shim)) (extension/shim-src shim)))
        (extension/sandbox-shims)))

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
  (->> (for
         [^File e
          (map io/file dirs)

          :when (.exists e)
          [^File d ^File f]
          (if (.isDirectory e)
            (for
              [^File f
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
  "sys.path entries for a test file, NUL-joined into one string (a NUL can't
   occur in a path, and a plain string crosses the polyglot boundary cleanly —
   a host array is not iterable under `PolyglotAccess/NONE`): `extra` (the
   project's own declared import roots, e.g. a `src` layout) first, then the
   test file's own dir and every ancestor up to and INCLUDING the extension
   scan dir — so `from mypkg.core import x` (package root), `from core import x`
   (same dir) and `import mypkg` (declared `src` root) all resolve regardless of
   how deep the test sits.

   The driver inserts these at `sys.path[0]` in order, so entries listed FIRST
   end up with the LOWEST precedence: the project roots never shadow a module
   sitting next to the test."
  ([^File scan-dir ^File test-file] (test-sys-path scan-dir test-file nil))
  ([^File scan-dir ^File test-file extra]
   (let [scan (.getCanonicalPath scan-dir)]
     (str/join "\u0000"
               (distinct (concat (remove str/blank? (map str extra))
                                 (loop
                                   [^File d (.getParentFile (.getCanonicalFile test-file))
                                    acc []]

                                   (if (nil? d)
                                     acc
                                     (let
                                       [p (.getCanonicalPath d)
                                        acc (conj acc p)]

                                       (if (= p scan) acc (recur (.getParentFile d) acc)))))))))))

(def ^:private run-test-src
  "Python driver: prepend the bound `sys.path` roots (a NUL-joined string), exec
   the test source under the `<prog>` co_filename (so the shim's linecache-based
   assert introspection lights up) with `__file__` bound to the test file's real
   path (so `Path(__file__).parent` fixtures work), then run `pytest.main` over
   that module's own globals with stdout captured into `__vis_test_output__`,
   the exit code in `__vis_test_rc__`, and — the source of truth — the shim's
   PER-TEST record list serialized into `__vis_test_report__` (records RS-joined,
   fields US-joined: nodeid, outcome, message)."
  (str "import sys as __vis_ts__, io as __vis_tio__\n"
       "for __vis_tp__ in __vis_test_paths__.split(chr(0)):\n"
       "    if __vis_tp__ and __vis_tp__ not in __vis_ts__.path:\n"
       "        __vis_ts__.path.insert(0, __vis_tp__)\n"
       "__vis_test_ns__ = {'__vis_src__': __vis_src__, '__name__': '__vis_pytest__',"
       " '__file__': __vis_test_file__}\n"
       "exec(compile(__vis_src__, '<prog>', 'exec'), __vis_test_ns__)\n"
       "import pytest as __vis_pt__\n"
       "__vis_tbuf__ = __vis_tio__.StringIO()\n" "__vis_told__ = __vis_ts__.stdout\n"
       "__vis_ts__.stdout = __vis_tbuf__\n" "try:\n"
       "    __vis_test_rc__ = int(__vis_pt__.main(ns=__vis_test_ns__))\n" "finally:\n"
       "    __vis_ts__.stdout = __vis_told__\n" "__vis_test_output__ = __vis_tbuf__.getvalue()\n"
       "__vis_trep__ = getattr(__vis_pt__, '_vis_last_report', []) or []\n"
       "__vis_test_report__ = chr(30).join("
       "str(__vis_nid__) + chr(31) + str(__vis_oc__) + chr(31) + str(__vis_msg__)"
       " for (__vis_nid__, __vis_oc__, __vis_msg__) in __vis_trep__)\n"))

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
                       {:nodeid nodeid
                        :outcome (if (= outcome "error") :errored (keyword outcome))
                        :message (or message "")}))))
        (str/split (or s "") (re-pattern record-sep))))

(defn- failing? [tests] (boolean (some (comp #{:failed :errored} :outcome) tests)))

(defn- run-test-file!
  "Run ONE test file in a fresh UNTRUSTED context: bootstrap the `vis` module,
   install the pytest shim, then drive `run-test-src`. `sys-path` is the extra
   import roots the project declares (a `src` layout), added below the test's
   own dirs. Returns `{:file :rc :ok? :output :tests}` where `:tests` is the
   per-test record list. Never throws — a broken test file is one `:errored`
   result, never a host crash.

   The context carries no process boundary and no host shell: a test file is
   bytes that never passed the loader, and running one is not an admission."
  [^String shim-src sys-path ^File scan-dir ^File test-file]
  (let
    [path
     (.getCanonicalPath test-file)

     source
     (slurp test-file)

     paths
     (test-sys-path scan-dir test-file sys-path)

     ^Context ctx
     (pyx/build-context {:trust :none})]

    (try (pyx/bind-untrusted-host! ctx (.getName test-file))
         (locking ctx
           (.eval ctx "python" ^String pyx/bootstrap-python)
           (.eval ctx "python" shim-src)
           (let [g (.getBindings ctx "python")]
             (.putMember g "__vis_test_paths__" paths)
             (.putMember g "__vis_test_file__" ^String path)
             (.putMember g "__vis_src__" ^String source)
             (.eval ctx "python" ^String run-test-src)
             (let [tests (parse-report (.asString (.getMember g "__vis_test_report__")))]
               {:file path
                :rc (int (.asInt (.getMember g "__vis_test_rc__")))
                :ok? (not (failing? tests))
                :output (.asString (.getMember g "__vis_test_output__"))
                :tests tests})))
         (catch Throwable t
           {:file path
            :rc -1
            :ok? false
            :output ""
            :tests [{:nodeid (.getName test-file) :outcome :errored :message (ex-message t)}]
            :error (ex-message t)})
         (finally (try (.close ctx true) (catch Throwable _))))))

(defn test-python-extensions!
  "Discover and run every Python test (`test_*.py` / `*_test.py`) across the
   extension dirs (default: `~/.vis/extensions` and `<cwd>/.vis/extensions`),
   each in its own UNTRUSTED GraalPy context via the built-in `pytest`-compat
   shim: a test runs beside the extension's code, not inside its trust, so
   `vis.shell` and `subprocess` refuse. Tests import the extension's own package
   through the `sys.path` sugar, exactly like `extension.py` does.

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
   (let
     [dirs
      (or dirs (pyx/default-extension-dirs))

      shim-src
      (pytest-shim-src)

      pairs
      (discover-tests dirs)]

     (if (nil? shim-src)
       {:files 0 :ok? false :error "pytest shim not registered" :results [] :tests []}
       (let
         [results
          (mapv (fn [[d f]]
                  (run-test-file! shim-src sys-path d f))
                pairs)

          tests
          (vec (for
                 [r
                  results

                  t
                  (:tests r)]

                 (assoc t :file (:file r))))

          counts
          (frequencies (map :outcome tests))]

         (merge {:files (count results) :ok? (every? :ok? results) :tests tests :results results}
                (select-keys counts [:passed :failed :errored :skipped :xfailed :xpassed])))))))

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
   first line of the assertion detail). Pure — the shared renderer for the
   `/test` slash command and `vis-agent extension test`."
  [{:keys [files passed failed errored skipped ok? results] :as res}]
  (cond (:error res) (str "✗ Python extension tests could not run: " (:error res))
        (zero? (long (or files 0))) "No Python extension tests found (test_*.py / *_test.py)."
        :else
        (let
          [summary
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
             (into [(str "  " (if ok? "✓" "✗") " " (rel-name file) (when error (str " — " error)))]
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
  "Run every Python extension test and return `{:result <map> :report <string>}`.
   The one shared code path behind the `/test` slash command and `vis-agent extension test`."
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

(defn- failure-ex
  "The `:vis/user-error` ex-info to throw when a run FAILED, or nil when it's
   ok. Pure and testable — the exit signal without a `System/exit` (the
   top-level CLI maps `:vis/user-error` to a non-zero exit)."
  [result]
  (when-not (:ok? result)
    (ex-info (or (:error result)
                 (format "Python extension tests failed: %d failed, %d errored"
                         (long (or (:failed result) 0))
                         (long (or (:errored result) 0))))
             {:type :vis.ext-test/failures :vis/user-error true})))

(defn ^:no-doc test-cli!
  "`vis-agent extension test` — run every Python extension test, print a report, and signal
   a non-zero exit on failure by throwing a `:vis/user-error` ex-info. NEVER
   calls `System/exit`: pure and testable, and safe to call from anywhere
   without killing the host."
  [_parsed _residual]
  (let [{:keys [result report]} (run-and-report nil)]
    (println report)
    (when-let [e (failure-ex result)]
      (throw e))
    result))
