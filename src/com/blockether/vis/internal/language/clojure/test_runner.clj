(ns com.blockether.vis.internal.language.clojure.test-runner
  "Run a namespace's tests in the session's ALREADY-RUNNING nREPL (the fast inner
   loop) or -- the default, and whenever there is no such REPL -- by shelling the
   project's own test command in a clean JVM. Nothing here EVER starts a REPL.

   The in-REPL path is FRAMEWORK-AGNOSTIC: a ns whose vars carry clojure.test
   :test metadata runs through clojure.test/run-tests; otherwise it is treated
   as lazytest and run through lazytest.runner/run-tests. Either way the result
   is a uniform STRING-keyed map (crosses the strings-only boundary) with
   \"mode\" (repl or cli), \"framework\", \"ns\", \"total\", \"pass\", \"fail\" and
   \"failures\" [{\"ns\" \"test\" \"message\" \"file\" \"line\"} ...].

   run-form is the code EVALED on the target nREPL. It is a quoted form (not a
   call into this namespace) so it works against ANY project's nREPL, including
   hosts that do not have the vis-agent extension on their classpath."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [com.blockether.vis.internal.language.clojure.nrepl-client :as nrepl-client]
            [com.blockether.vis.internal.language.clojure.repl-manager :as repl-manager]
            [com.blockether.vis.internal.language.clojure.shadow-cljs :as shadow]
            [com.blockether.vis.contract.surface :as surface]
            [com.blockether.vis.contract.test-runner :as contract]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.config.runtime-settings :as rt]))

(def ^{:private true} run-form
  "Code evaled on the target nREPL. Loads each REQUESTED namespace FROM SOURCE
   (`require :reload`, or `load-file` when ns-files carries its path) and ONLY
   that namespace: `:reload` never touches its dependencies, so a production
   namespace the caller edited keeps the Vars the REPL already holds and the
   run measures stale code. Reloading those is the caller's move (`repl_eval`
   `(require 'my.prod.ns :reload)`, or a fresh REPL) — never `:reload-all`
   here, which would re-evaluate the whole graph under the test run. Selects
   tests by the lazytest-modeled selector map {:vars :include :exclude} at VAR
   granularity. :vars is what the `<path>::<test-name>` node ids in the CALL's
   :paths resolved to - {:ns <ns-or-nil> :name <name>} - and a name matches the
   test var itself (adds-test) or the SOURCE var it covers (adds), the same
   `-test` translation a SOURCE FILE gets. A non-empty :vars that matches
   NOTHING is an ERROR carrying only the searched namespace and test-var counts,
   never a silent 0/0 pass or an exhaustive list that wastes model context.
   ns-files is an optional map from namespace string to absolute test file path. The map used when the live nREPL was started without test paths on its classpath."
  (quote
    (fn [nsyms sel ns-files]
      (doseq [n nsyms]
        (if-let [path (get ns-files (str n))]
          (load-file path)
          (require n :reload)))
      (let [vars*
            (vec (:vars sel))

            inc*
            (set (:include sel))

            exc*
            (set (:exclude sel))

            tags-of
            (fn [v]
              (->> (meta v)
                   (keep (fn [[k v]]
                           (when (true? v) (name k))))
                   set))

            vname-of
            (fn [v]
              (name (:name (meta v))))

            nsname-of
            (fn [v]
              (str (ns-name (:ns (meta v)))))

            ;; A node id names either the TEST var (adds-test) or the SOURCE var it
            ;; covers (adds) - the same `-test` translation a SOURCE FILE gets, so
            ;; one convention answers `core.clj` and `core.clj::adds`.
            var-hit?
            (fn [nsn nm entry]
              (and (or (nil? (:ns entry)) (= (:ns entry) nsn))
                   (or (= (:name entry) nm) (= (str (:name entry) "-test") nm))))

            keep?
            (fn [v]
              (let [tags
                    (tags-of v)

                    nm
                    (vname-of v)

                    nsn
                    (nsname-of v)]

                (cond (some exc* tags) false
                      (and (seq vars*)
                           (not (some (fn [e]
                                        (var-hit? nsn nm e))
                                      vars*)))
                      false
                      (and (seq inc*) (not (some inc* tags))) false
                      :else true)))

            ;; A node id whose test name selects NOTHING is a caller mistake
            ;; (usually a stale or misspelled var name). Keep the error actionable
            ;; but bounded: listing every namespace and test var can consume an
            ;; entire tool result and adds no signal once the selector is known.
            var-miss
            (fn [framework all]
              (when (and (seq vars*) (empty? (filter keep? all)))
                {"framework" framework
                 "error" (str "no test var matched "
                              (pr-str (mapv (fn [e]
                                              (str (:ns e) "::" (:name e)))
                                            vars*))
                              " (searched "
                              (count all)
                              " test vars across "
                              (count nsyms)
                              (if (= 1 (count nsyms)) " namespace)" " namespaces)"))
                 "total" 0
                 "pass" 0
                 "fail" 0
                 "selected" 0
                 "skipped" (count all)
                 "failures" []}))

            all-ct
            (mapcat (fn [n]
                      (filter (fn [v]
                                (:test (meta v)))
                              (vals (ns-interns (the-ns n)))))
                    nsyms)

            lt?
            (fn [v]
              (let [m (meta v)]
                (or (= :lazytest/var (:type m)) (contains? m :lazytest/test))))

            all-lt
            (mapcat (fn [n]
                      (filter lt? (vals (ns-interns (the-ns n)))))
                    nsyms)

            out-writer
            (java.io.StringWriter.)

            result
            (binding [clojure.core/*out*
                      out-writer

                      clojure.core/*err*
                      out-writer]

              (if (seq all-ct)
                (or
                  (var-miss "clojure.test" all-ct)
                  (let [selected
                        (vec (filter keep? all-ct))

                        skipped
                        (- (count all-ct) (count selected))

                        fails
                        (atom [])

                        cnt
                        (atom {:pass 0 :fail 0 :error 0})]

                    (with-redefs [clojure.test/report
                                  (fn [m]
                                    (when (#{:fail :error :pass} (:type m))
                                      (swap! cnt update (:type m) (fnil inc 0)))
                                    (when (#{:fail :error} (:type m))
                                      (let [v0 (first clojure.test/*testing-vars*)
                                            vm (meta v0)
                                            thrown (when (= :error (:type m))
                                                     (let [a (:actual m)]
                                                       (when (instance? Throwable a) a)))]

                                        (swap! fails conj
                                          {"ns" (str (:ns vm))
                                           "test" (when v0 (str (:name vm)))
                                           "type" (name (:type m))
                                           "message" (if thrown
                                                       (str (.getName (class thrown))
                                                            (when-let [msg (.getMessage ^Throwable
                                                                                        thrown)]
                                                              (str ": " msg)))
                                                       (str (or (:message m) (:type m))))
                                           "expected" (pr-str (:expected m))
                                           ;; For an :error the raw :actual is the whole Throwable
                                           ;; (a giant #error map with stacktrace) — the class+message
                                           ;; above already carries the signal, so drop the dump.
                                           "actual" (if thrown "" (pr-str (:actual m)))
                                           ;; clojure.test pins :file/:line to the THROWING JVM frame
                                           ;; for errors (e.g. Numbers.java:190) — fall back to the
                                           ;; test var's own source location so the digest points
                                           ;; at the deftest, not clojure internals.
                                           "file" (if thrown (str (:file vm)) (str (:file m)))
                                           "line" (if thrown (:line vm) (:line m))}))))]
                      (clojure.test/test-vars selected))
                    (let [c
                          (clojure.core/deref cnt)

                          fs
                          (clojure.core/deref fails)]

                      {"framework" "clojure.test"
                       "total" (+ (:pass c) (:fail c) (:error c))
                       "pass" (:pass c)
                       "fail" (+ (:fail c) (:error c))
                       ;; The erroring SUBSET of "fail" — already inside it.
                       "errored" (:error c)
                       "selected" (count selected)
                       "skipped" skipped
                       "failures" fs})))
                (or
                  (var-miss "lazytest" all-lt)
                  (let [selected
                        (vec (filter keep? all-lt))

                        skipped
                        (- (count all-lt) (count selected))

                        lt-suite
                        (requiring-resolve (quote lazytest.suite/suite))

                        run-suite
                        (requiring-resolve (quote lazytest.runner/filter-and-run))

                        var->suite
                        (fn [v]
                          ;; A defdescribe var derefs to a THUNK that builds the
                          ;; suite; the older style stores it in :lazytest/test
                          ;; metadata. Mirror lazytest.runner's own extraction.
                          (let [m (meta v)]
                            (if (contains? m :lazytest/test)
                              (:lazytest/test m)
                              (let [x (deref v)]
                                (if (fn? x) (x) x)))))

                        run-var
                        (fn [v]
                          ;; lazytest.runner/run-test-var DROPS the ns-level
                          ;; :context that set-ns-context! attaches (only
                          ;; find-ns-suite reads it), so ns fixtures such as
                          ;; around-each never fire under per-var running.
                          ;; Rebuild the per-var run suite WITH the ns context so
                          ;; around-each / before-each wrappers apply. When a ns
                          ;; has no :context this is nil -> behaves exactly like
                          ;; run-test-var.
                          (let [tns (the-ns (symbol (namespace (symbol v))))]
                            (run-suite (lt-suite {:type :lazytest/run
                                                  :nses [tns]
                                                  :children [(var->suite v)]
                                                  :context (:context (meta tns))})
                                       {:output []})))

                        rseq
                        (requiring-resolve (quote lazytest.results/result-seq))

                        trees
                        (mapv (fn [v]
                                (run-var v))
                              selected)

                        results
                        (mapcat rseq trees)

                        leaves
                        (filter (fn [x]
                                  (#{:fail :error :pass} (:type x)))
                                results)

                        fails
                        (filter (fn [x]
                                  (#{:fail :error} (:type x)))
                                results)

                        ->fail
                        (fn [f]
                          {"ns" (str (:ns f))
                           "test" (str (:doc f))
                           "type" (name (:type f))
                           "message" (let [m (:message f)]
                                       (cond (seq (str m)) (str m)
                                             (:thrown f) (str (.getMessage (:thrown f)))
                                             :else (str "expected " (pr-str (:expected f))
                                                        " actual " (pr-str (:actual f)))))
                           "expected" (pr-str (:expected f))
                           "actual" (pr-str (:actual f))
                           "file" (str (:file f))
                           "line" (:line f)})]

                    {"framework" "lazytest"
                     "total" (count leaves)
                     "pass" (count (filter (fn [x]
                                             (= :pass (:type x)))
                                           results))
                     "fail" (count fails)
                     ;; The erroring SUBSET of "fail" — already inside it.
                     "errored" (count (filter (fn [x]
                                                (= :error (:type x)))
                                              results))
                     "selected" (count selected)
                     "skipped" skipped
                     "failures" (mapv ->fail fails)}))))]

        (assoc result "output" (clojure.core/str out-writer))))))

(defn build-eval-code
  "Self-contained Clojure source string that runs tests for ns-strs with sel.
   ns-files optionally maps namespace strings to absolute .clj paths to load
   when the target nREPL does not have test paths on the classpath.

   The printer vars are pinned (no length/level/meta/dup limits) so the emitted
   code is always COMPLETE and readable — a caller runtime that caps
   *print-level* / *print-length* would otherwise render deep sub-forms of
   run-form as `#` / `...` and produce an unreadable, unbalanced string.

   The emitted code RETURNS its result map pr-str'd to a STRING under those same
   pinned vars. A plain string value is immune to a truncating nREPL SESSION
   (a server whose *print-length* / *print-level* is set clips collections and
   nesting with `...` / `#`, but never a string's characters), so run-via-repl
   always gets parseable EDN back — it reads the value twice: once to unwrap the
   string literal, once to parse the map inside it."
  ([ns-strs sel] (build-eval-code ns-strs sel {}))
  ([ns-strs sel ns-files]
   (binding [*print-length*
             nil

             *print-level*
             nil

             *print-namespace-maps*
             false

             *print-meta*
             false

             *print-dup*
             false]

     (str
       "(binding [*print-length* nil *print-level* nil *print-namespace-maps* false *print-meta* false *print-dup* false] (pr-str ("
       (pr-str run-form)
       " (quote ["
       (str/join " " ns-strs)
       "]) "
       (pr-str sel)
       " "
       (pr-str ns-files)
       ")))"))))

(defn- strip-ansi
  "Strip ANSI escape sequences (colors / cursor controls) from a captured test
   run log, so channel previews (web + TUI) show plain text instead of raw
   `[32m`-style escape fragments. nil-safe."
  [s]
  (when s (str/replace s #"\u001b\[[0-9;]*[A-Za-z]" "")))

(defn- rel-fault-file
  "Rewrite a fault map's absolute \"file\" to one relative to workspace `root`, so
   digests read `test/foo_test.clj` instead of the machine-absolute
   `/Users/…/test/foo_test.clj` (load-file pins the compiled frame to the absolute
   path it was handed). Paths outside root and non-path sentinels pass through."
  [^java.io.File root fault]
  (let [raw
        (get fault "file")

        s
        (str raw)]

    (if (and root (not (str/blank? s)))
      (try (let [rp
                 (.toPath (.getCanonicalFile root))

                 fp
                 (.toPath (.getCanonicalFile (io/file s)))]

             (if (.startsWith fp rp) (assoc fault "file" (str (.relativize rp fp))) fault))
           (catch Throwable _ fault))
      fault)))

(def ^:private unlocated-files
  "Source-file SENTINELS a runtime uses for \"this frame has no source\": they are
   not paths, so a fault carrying one has no location at all."
  #{"Unknown" "Unknown Source" "NO_SOURCE_PATH" "NO_SOURCE_FILE"})

(defn- locate-fault
  "Drop a fault's location when the runtime could not resolve one, instead of
   passing its sentinel on. `StackTraceElement.getLineNumber` answers -1 for a
   frame it cannot place (-2 for a native one) and its file name is then a
   `Unknown` / `NO_SOURCE_PATH` sentinel — lazytest copies both onto the failure
   verbatim, and a NEGATIVE line violates the run_tests contract (`\"line\"` is a
   non-negative count), which used to blow up the whole result. nil is exactly
   what `failures->text` already renders as \"no location\"."
  [fault]
  (let [file
        (str (get fault "file"))

        line
        (get fault "line")]

    (assoc fault
      "file" (when-not (or (str/blank? file) (contains? unlocated-files file)) file)
      "line" (when (nat-int? line) line))))

(defn- normalize-faults
  "Make every fault's location in `parsed`'s \"failures\" honest: an unresolvable
   location is dropped (`locate-fault`) and a real absolute path is rewritten
   relative to workspace `root`. Idempotent — an already-relative path and an
   already-dropped location are left as-is."
  [root parsed]
  (let [root-file
        (io/file (str root))

        clean
        (comp (partial rel-fault-file root-file) locate-fault)]

    (if (seq (get parsed "failures")) (update parsed "failures" (partial mapv clean)) parsed)))

(defn- failures->text
  "Concise, framework-neutral digest of the structured failure/error maps a
   run-form result carries: one `✗ ns/test (file:line)` line per failure with its
   message (and expected/actual when they add signal). REPLACES each framework's own
   verbose per-namespace reporter tree, so a run's `output` stays a tight, ANSI-free
   summary instead of a `Ran N test cases … 0 failures` block repeated per
   defdescribe."
  [fails]
  (->> fails
       (map
         (fn [{:strs [ns test message expected actual file line]}]
           (let [keep?
                 (fn [x]
                   (and x (not (str/blank? (str x))) (not= "nil" (str x))))

                 loc
                 (when (keep? file) (str "  (" file (when line (str ":" line)) ")"))

                 head
                 (str "✗ " ns (when (keep? test) (str "/" test)) loc)

                 detail
                 (cond-> []
                   (keep? message)
                   (conj (str "    " message))

                   (keep? expected)
                   (conj (str "    expected: " expected))

                   (keep? actual)
                   (conj (str "    actual:   " actual)))]

             (str/join "\n" (cons head detail)))))
       (str/join "\n")))

(defn- compose-repl-output
  "Final `output` for a repl-mode result: the tests' OWN captured stdout (ANSI-
   stripped — empty on a quiet pass now the reporter is silenced) followed by a tidy
   `failures->text` digest when the run failed. Never the framework's per-namespace
   reporter tree, so a green run reads clean and a red one shows only what broke."
  [parsed]
  (let [cap
        (not-empty (str/trim (or (strip-ansi (str (get parsed "output"))) "")))

        digest
        (when-let [fails (seq (get parsed "failures"))]
          (failures->text fails))]

    (assoc parsed "output" (str/join "\n\n" (remove nil? [cap digest])))))

(defn- ns-of-file
  "The ns symbol a Clojure (test) file declares, as a string, or nil when it
   declares none.

   READS the form; never scans for it. `(ns ^{:clj-kondo/config …} foo.bar-test)`
   is an ordinary namespace, but a pattern anchored on `(ns` + a symbol meets the
   `^`, matches nothing and answers nil — and a nil name makes the WHOLE namespace
   invisible: it is missing from the workspace index, and naming its own file
   selects nothing, so `run_tests` refuses the path as if the tests were not
   there. Metadata belongs to the symbol, and `read` hands back the name with the
   metadata attached to it.

   The declaration is not always the first form (a leading comment form, a
   discard or a `set!` is legal), so a bounded prefix is read. `*read-eval*` is
   off: this parses source, it never runs it."
  [^java.io.File f]
  (try (with-open [r (java.io.PushbackReader. (io/reader f))]
         (binding [*read-eval* false]
           (let [opts {:eof ::eof :read-cond :allow :features #{:clj}}]
             (loop [read-so-far 0]
               (when (< read-so-far 16)
                 (let [form (read opts r)]
                   (cond (= ::eof form) nil
                         (and (seq? form) (= 'ns (first form)) (symbol? (second form)))
                         (str (second form))
                         :else (recur (inc read-so-far)))))))))
       (catch Throwable _ nil)))

(defn- source-ns->test-ns
  "Map a source namespace to its conventional test namespace: foo.bar ->
   foo.bar-test (an already-…-test ns is returned unchanged)."
  [ns-str]
  (when ns-str (if (str/ends-with? ns-str "-test") ns-str (str ns-str "-test"))))

(defn- source-ns->test-nses
  "Conventional test namespace candidates, nearest first: the source namespace's
   own `-test`, then each parent namespace's. The first candidate present in the
   test index owns a nested source file whose tests live in a parent suite."
  [ns-str]
  (when ns-str
    (let [parts (str/split ns-str #"\.")]
      (mapv (fn [n]
              (source-ns->test-ns (str/join "." (take n parts))))
            (range (count parts) 0 -1)))))

(def ^:private jvm-test-exts
  "Extensions a JVM Clojure namespace lives in. `.cljc` is loaded and run by
   `clojure -M:test` exactly like `.clj`, so `foo_test.cljc` IS a test file —
   skipping it makes a path the caller spelled CORRECTLY look like a location
   with no tests under it."
  [".clj" ".cljc"])

(def ^:private cljs-test-ext
  "The extension that NEVER loads on the JVM: `*_test.cljs` is a ClojureScript
   test, and the project's shadow-cljs build is the only runtime that can run it.
   Indexed exactly like a JVM test file — a path, a directory and a namespace
   name all still select it — because the FILE a namespace was read from is what
   says which runner the selection needs."
  ".cljs")

(def ^:private test-file-exts
  "Every extension a test namespace can be declared in, JVM and ClojureScript."
  (conj jvm-test-exts cljs-test-ext))

(defn- cljs-file?
  "A .cljs source requires a JS runtime; shared .cljc files can use either runtime."
  [^java.io.File f]
  (boolean (and f (str/ends-with? (.getName f) cljs-test-ext))))

(defn- clj-source-file?
  "True when `f` is a Clojure source file a test run can load."
  [^java.io.File f]
  (boolean (and (.isFile f)
                (some (fn [^String ext]
                        (str/ends-with? (.getName f) ext))
                      test-file-exts))))

(defn- test-source-file?
  "True when `f` is a TEST file: a `_test` source file in a Clojure extension."
  [^java.io.File f]
  (boolean (and (.isFile f)
                (some (fn [^String ext]
                        (str/ends-with? (.getName f) (str "_test" ext)))
                      test-file-exts))))

(defn- index-test-file
  "Keep one unambiguous file per namespace. Never overwrite a JVM/JS or nested
   project collision: callers can name a precise file/project instead."
  [index ns-str ^java.io.File file]
  (when-let [^java.io.File previous (get index ns-str)]
    (when (and file (not= (.getCanonicalPath previous) (.getCanonicalPath file)))
      (throw (ex-info (str "ambiguous test namespace "
                           ns-str
                           " in "
                           previous
                           " and "
                           file
                           "; select a specific file/project instead")
                      {:type :clj/bad-args}))))
  (assoc index ns-str (or file (get index ns-str))))

(defn- all-test-files
  "Index every test file under root by its declared ns string, built once per
   run so SOURCE paths can be resolved to their corresponding test namespace —
   and so a namespace NAME can be traced back to the file that decides which
   runtime runs it."
  [root test-file?]
  (reduce (fn [index ^java.io.File f]
            (if-let [ns-str (when (test-file? f) (ns-of-file f))]
              (index-test-file index ns-str f)
              index))
          {}
          (file-seq (io/file root))))

(defn- path->nses
  "Resolve ONE file/dir to `{:ns :file}` entries. A test file -> its own ns. A
   plain source file -> its nearest matching `*-test` ns: first its own, then
   each parent namespace, so a nested implementation file can resolve to the
   suite that owns its package. A directory -> every test file under it; a pure
   source dir maps each source ns the same way. `test-index` is a DELAY over
   {ns-str file} — naming test files never pays for the workspace walk a source
   file needs. Every entry carries the FILE it was read from, because that
   file's extension decides whether the namespace runs on the JVM or in
   shadow-cljs."
  [^java.io.File f test-index test-file?]
  (let [entry
        (fn [^java.io.File file]
          (when-let [n (ns-of-file file)]
            {:ns n :file file}))

        test-entry
        (fn [^java.io.File file]
          (when-let [src-ns (ns-of-file file)]
            (some (fn [tn]
                    (when-let [tf (get @test-index tn)]
                      {:ns tn :file tf}))
                  (source-ns->test-nses src-ns))))]

    (cond (test-file? f) (keep identity [(entry f)])
          (clj-source-file? f) (keep identity [(test-entry f)])
          (.isDirectory f) (let [test-files (filter test-file? (file-seq f))]
                             (if (seq test-files)
                               (keep entry test-files)
                               (->> (file-seq f)
                                    (filter clj-source-file?)
                                    (keep test-entry))))
          :else [])))

(defn- clj-file-name?
  "True when `s` names a Clojure FILE rather than a namespace — the one syntactic
   tell that separates `test/a/core_test.clj` from `a.core-test` when a path
   arrives under a namespace key."
  [s]
  (boolean (re-find #"\.clj[cs]?$" (str s))))

(defn- under-root
  "The FILE a request entry names: absolute as given, otherwise relative to
   `root`. The ONE place a caller's path becomes a location, so selection and
   the missing-path check can never disagree about which file was meant."
  ^java.io.File [root path]
  (let [f (io/file (str path))]
    (if (.isAbsolute f) f (io/file (str root) (str path)))))

(defn- deepest-existing
  "The nearest ancestor of `f` that is on disk (`f` itself when it is), or nil —
   the last segment of a misspelled path that was still right."
  ^java.io.File [^java.io.File f]
  (loop [f f]
    (cond (nil? f) nil
          (.exists f) f
          :else (recur (.getParentFile f)))))

(defn- missing-locations
  "The requested LOCATIONS that are not on disk, as
   `[{:path <as asked> :exists <deepest live ancestor, or nil>}]`. Only entries
   that NAME a location count: a namespace name has none — an unknown namespace
   is a require-time failure, not a bad path — but a path handed to a namespace
   key is still a path."
  [root path-entries ns-entries]
  (into []
        (comp (keep (fn [{:keys [path ns]}]
                      (when-let [named (or path (when (clj-file-name? ns) ns))]
                        [named (under-root root named)])))
              (remove (fn [[_ ^java.io.File f]]
                        (.exists f)))
              (map (fn [[named ^java.io.File f]]
                     {:path named
                      :exists (some-> (deepest-existing f)
                                      (.getPath))})))
        (concat path-entries ns-entries)))

(defn- ns->test-nses
  "Resolve ONE namespace NAME to the test namespaces it selects: a test namespace
   is itself, a SOURCE namespace becomes its `*-test` namespace — the same
   translation a source PATH gets. A name the workspace index does not know is
   passed through unchanged, because that index sees only the test FILES under
   root: a namespace that lives elsewhere on the test classpath still runs,
   and a misspelled one fails loudly at require time instead of quietly selecting
   nothing."
  [ns-str test-index]
  (let [tn (some #(when (contains? @test-index %) %) (source-ns->test-nses ns-str))]
    (cond (contains? @test-index ns-str) [ns-str]
          tn [tn]
          :else [ns-str])))

(defn- resolve-ns-entry
  "Resolve ONE namespace-selector entry `{:ns :var}` into `{:entries :var}`, each
   entry being `{:ns <ns-str> :file <file or nil>}`. The entry names a namespace
   (`a.core-test`), a namespace and ONE var in the spelling `clojure -M:test
   --var` takes (`a.core-test/adds-test`), or a PATH that arrived under a
   namespace key — read as the path it obviously is, rather than as a namespace
   that could never load. A name the workspace index does not know keeps a nil
   :file: it still runs, on the JVM path that has always required it."
  [root {ns-str :ns var-name :var} test-index test-file?]
  (let [with-files (fn [nses]
                     (mapv (fn [n]
                             {:ns n :file (get @test-index n)})
                           nses))]
    (if (nil? ns-str)
      {:entries [] :var var-name}
      (let [^java.io.File f (under-root root ns-str)]
        (cond (or (clj-file-name? ns-str) (.exists f))
              {:entries (vec (path->nses f test-index test-file?)) :var var-name}
              (and (nil? var-name) (str/includes? ns-str "/"))
              (let [[n v] (str/split ns-str #"/" 2)]
                {:entries (with-files (ns->test-nses n test-index)) :var (not-empty v)})
              :else {:entries (with-files (ns->test-nses ns-str test-index)) :var var-name})))))

(defn- resolve-selection
  "Resolve the CALL's selector entries into what a run needs: `:nses`, the test
   namespaces to load, `:vars`, the var filter to apply inside them
   (`{:ns <ns-or-nil> :name <test-name>}`), `:files`, the test file each
   NAMESPACE entry resolved to — a path entry already names its own location, a
   namespace entry does not, and the run must still be rooted at the project the
   tests live in — and `:ns-files`, the file EVERY selected namespace was read
   from (nil when the index does not know it), which is what says whether that
   namespace runs on the JVM or in shadow-cljs.
   `path-entries` are the node-id maps from `:paths` (`{:path :var}`, from
   `contract/split-node-id`); `ns-entries` are the namespace/var spellings
   (`{:ns :var}`, from `split-selector-entry`).
   Each entry is resolved ON ITS OWN, so `a_test.clj::x` and `b_test.clj::y`
   pair each name with its OWN file instead of cross-producting into both. An
   entry with no location (`::x`, or a bare `only` name) names a var wherever it
   lives — nil :ns, and no namespace of its own, so the other entries (or the
   whole-workspace default) decide where to look. Paths are relative to root or
   absolute; files AND directories are accepted, and SOURCE files/dirs map to
   their *_test namespaces."
  [root path-entries ns-entries test-file?]
  (let [test-index
        (delay (all-test-files root test-file?))

        ;; One entry's resolved namespaces plus the name it narrows to. A name with
        ;; no namespace of its own stays `{:ns nil}` — 'wherever it lives'.
        add
        (fn [acc entries var]
          (let [nses (mapv :ns entries)]
            (cond-> (-> acc
                        (update :nses into nses)
                        (update :ns-files
                                (fn [index]
                                  (reduce (fn [m {:keys [ns file]}]
                                            (index-test-file m ns file))
                                          index
                                          entries))))
              var
              (update :vars
                      into
                      (if (seq nses)
                        (map (fn [n]
                               {:ns n :name var})
                             nses)
                        [{:ns nil :name var}])))))

        acc
        (reduce (fn [acc {:keys [path var]}]
                  (let [entries (when path
                                  (vec (path->nses (under-root root path) test-index test-file?)))]
                    (when (and path (.exists (under-root root path)) (empty? entries))
                      (throw (ex-info (str "run_tests(clojure) found no test namespaces under "
                                           (pr-str path))
                                      {:type :clj/bad-args})))
                    (add acc entries var)))
                {:nses [] :vars [] :files [] :ns-files {}}
                path-entries)

        acc
        (reduce (fn [acc entry]
                  (let [{:keys [entries var]} (resolve-ns-entry root entry test-index test-file?)]
                    (-> (add acc entries var)
                        (update :files into (keep :file entries)))))
                acc
                ns-entries)]

    {:nses (vec (sort (distinct (:nses acc))))
     :vars (vec (distinct (:vars acc)))
     :files (vec (distinct (:files acc)))
     :ns-files (:ns-files acc)}))

(def ^:private namespace-selector-keys
  "Selector spellings whose value names a NAMESPACE. `clojure -M:test` takes
   `--namespace`, so this is what a model reaches for when it does not name a
   file; every one of them SELECTS instead of being refused, and
   `resolve-ns-entry` decides what each entry really names."
  ["ns" "nses" "namespace" "namespaces"])

(def ^:private var-selector-keys
  "Selector spellings whose value names a TEST VAR: `--var`'s `ns/var`, or a bare
   test name that narrows wherever it lives."
  ["var" "vars" "only"])

(defn- split-selector-entry
  "Split ONE namespace/var selector entry into `{:ns :var}`. `bare` says what a
   token carrying no separator means — `:ns` under a namespace key, `:var` under
   a var key. `a.core-test/adds-test` (the `--var` spelling) and
   `a.core-test::adds-test` (the node-id one) both split into both halves; a
   `.clj` entry keeps its slashes, because `resolve-ns-entry` reads it as the
   path it obviously is."
  [bare entry]
  (let [{:keys [path var]}
        (contract/split-node-id entry)

        [head v]
        (if (and path (nil? var) (not (clj-file-name? path)) (str/includes? path "/"))
          (let [[a b] (str/split path #"/" 2)]
            [(not-empty a) (not-empty b)])
          [path var])]

    (if (and (= :var bare) (nil? v)) {:ns nil :var head} {:ns head :var v})))

(defn- selector-entries
  "Every namespace/var selector entry a map arg carries, as `{:ns :var}` maps in
   key order."
  [arg]
  (vec (concat (for [k
                     namespace-selector-keys

                     e
                     (contract/->str-vec (get arg k))]

                 (split-selector-entry :ns e))
               (for [k
                     var-selector-keys

                     e
                     (contract/->str-vec (get arg k))]

                 (split-selector-entry :var e)))))

(defn- extra-aliases
  "The deps.edn aliases a run_tests call ADDS to the project's own `:test`:
   [\"bench\" \"dev\"], \"bench\" and \":bench\" all read as alias NAMES. A leading
   colon is accepted and dropped — an alias crosses the strings-only boundary as
   a name, spliced into `clojure -M:test:<name>` exactly like `repl_start`'s own
   `aliases`, and `:test` itself is never replaced."
  [arg]
  (->> (contract/->str-vec (get arg "aliases"))
       (mapv (fn [a]
               (str/replace a #"^:+" "")))
       (filterv (complement str/blank?))))

(defn- normalize-arg
  "Coerce the raw run_tests arg (a path string or an opts dict) into the
   canonical selector map
   `{:paths [{:path :var}] :ns-selectors [{:ns :var}] :include [str] :exclude [str]}`.
   The model arg is STRING-keyed (strings-only boundary); this is the
   external->internal seam that translates its keys into the keyword vocabulary
   the resolvers read, splitting each path entry on its `::` (see
   `contract/split-node-id`).

   PATHS are the primary spelling — one entry says WHERE and WHICH, and
   `clj-test-fn` resolves each path half to the test namespaces declared under
   it, so naming a SOURCE file runs its `*-test` namespace. A model that instead
   names a NAMESPACE is speaking `clojure -M:test --namespace` / `--var`, which
   is a real selection and not a mistake: `ns` / `nses` / `namespace` /
   `namespaces` and `var` / `vars` / `only` carry into `:ns-selectors` and run,
   and `path` is read alongside `paths`. `build` selects a ClojureScript build,
   including for .cljc tests. `aliases` adds classpath or selects an executable
   deps.edn runner for the clean-JVM command; its focus adapter follows that runner."
  [arg]
  (cond
    (or (string? arg) (symbol? arg)) (contract/normalize-selectors {:paths [(str arg)]})
    (map? arg) (assoc (contract/normalize-selectors {:paths (into
                                                              (contract/->str-vec (get arg "paths"))
                                                              (contract/->str-vec (get arg "path")))
                                                     :include (get arg "include")
                                                     :exclude (get arg "exclude")})
                 :ns-selectors (selector-entries arg)
                 :build (not-empty (str/trim (str (get arg "build"))))
                 :aliases (extra-aliases arg))
    :else
    (throw
      (ex-info
        "run_tests(clojure) expects a path string, or a dict with a \"paths\" key"
        {:type :clj/bad-args
         :got arg
         :examples
         ["run_tests(\"clojure\", \"test/com/example/thing_test.clj\")"
          "run_tests(\"clojure\", {\"paths\": [\"src/com/example/thing.clj\"]})"
          "run_tests(\"clojure\", {\"paths\": [\"test/com/example/thing_test.clj::adds-test\"]})"
          "run_tests(\"clojure\", {\"paths\": [\"::adds-test\"]})"
          "run_tests(\"clojure\", {\"ns\": \"com.example.thing-test\"})"
          "run_tests(\"clojure\", {\"paths\": [\"test\"], \"exclude\": [\"slow\"]})"]}))))

(defn- ns->source-relpath
  "The relative source path a namespace maps to, WITHOUT the extension
   (`a.core-test` -> `a/core_test`): the same namespace may live in a `.clj` or
   a `.cljc` file, so the caller tries every test extension."
  [ns-str]
  (-> ns-str
      (str/replace "." "/")
      (str/replace "-" "_")))

(defn- test-file-for
  "Find a test source file for ns-str under root, even when the live nREPL was
   started without test paths on its classpath."
  [root ns-str]
  (let [rels
        (mapv (partial str (ns->source-relpath ns-str)) jvm-test-exts)

        root-file
        (io/file root)]

    (some (fn [^java.io.File f]
            (let [p (.getPath f)]
              (when (and (.isFile f)
                         (some (fn [^String rel]
                                 (str/ends-with? p rel))
                               rels)
                         (str/includes? p
                                        (str java.io.File/separator "test" java.io.File/separator)))
                (.getAbsolutePath f))))
          (file-seq root-file))))

(defn- test-files-for
  [root ns-strs]
  (into {}
        (keep (fn [ns-str]
                (when-let [path (test-file-for root ns-str)]
                  [ns-str path])))
        ns-strs))

(defn- run-via-repl
  [root ns-strs sel port]
  (let [;; Cheap pre-flight: a single `describe` under a short timeout. A dead or
        ;; wedged nREPL is caught here in ~2s instead of blocking the whole test
        ;; eval for the multi-minute `default-test-timeout-ms` budget. `probe!`
        ;; never throws and reuses the same cached connection `eval!` warms, so
        ;; the healthy path pays only one fast round-trip. It can't recurse into
        ;; `eval!` from THIS layer (it does from inside `eval!`, which is why the
        ;; guard lives here at the entry point rather than in the client).
        probe (nrepl-client/probe! {:host "localhost" :port port :timeout-ms 2000})]
    (if (not= :up (:status probe))
      {"mode" "repl"
       "ns" (str/join " " ns-strs)
       "port" port
       "error" (str "nREPL at localhost:"
                    port
                    " is not ready to run tests (status "
                    (name (or (:status probe) :unknown))
                    ") — the server is down or unresponsive.")
       "repl_unusable" true}
      (let [ns-files (test-files-for root ns-strs)
            code (build-eval-code ns-strs sel ns-files)
            ns-disp (str/join " " ns-strs)]

        (try
          (let [r
                ;; This timeout is the real budget: a direct tool call has no outer
                ;; wall, and from a Python block `RUN_TESTS_FLOOR_SECS` floors the eval
                ;; watchdog above it. A slow / wedged nREPL therefore surfaces as a real
                ;; timeout ERROR (with nREPL err/tail), never an opaque harness kill.
                (nrepl-client/eval!
                  {:host "localhost" :port port :code code :timeout-ms rt/RUN_TESTS_TIMEOUT_MS})
                parsed (try (let [x (edn/read-string (get r "value"))]
                              (if (string? x) (edn/read-string x) x))
                            (catch Throwable _ nil))]

            (cond
              ;; nREPL never returned a result within the budget (eval! reports it and
              ;; evicts the connection). Surface a CLEAR timeout instead of the opaque
              ;; "could not parse test result" a nil value would otherwise produce.
              (get r "timed_out")
              {"mode" "repl"
               "ns" ns-disp
               "port" port
               "timed_out" true
               "error" (str "test run timed out after " rt/RUN_TESTS_TIMEOUT_MS
                            "ms — the nREPL never returned. The eval is likely wedged "
                            "(infinite loop, blocked I/O, or a deadlock in the code under "
                            "test); the connection was evicted so a retry reconnects fresh."
                            (when (seq (str (get r "err"))) (str " nREPL err: " (get r "err"))))
               "repl_wedged" true}
              (map? parsed) (-> parsed
                                (->> (normalize-faults root))
                                (compose-repl-output)
                                (assoc "mode" "repl"
                                       "ns" ns-disp
                                       "port" port))
              :else {"mode" "repl"
                     "ns" ns-disp
                     "port" port
                     "error" (str "could not parse test result"
                                  (when (seq (str (get r "err")))
                                    (str " - nREPL err " (get r "err"))))
                     "raw_value" (get r "value")}))
          ;; The probe passed but the server vanished before/while the eval ran
          ;; (a crash, an idle-reap, or a manual kill in the TOCTOU window between
          ;; probe and eval). Surface it as DATA — the same actionable "start a fresh
          ;; REPL and retry" the down-probe branch returns — instead of letting the raw
          ;; connect exception escape as a hard tool error and eat the turn.
          (catch clojure.lang.ExceptionInfo e
            (if (#{:clj/nrepl-connect-failed :clj/nrepl-io} (:type (ex-data e)))
              {"mode" "repl"
               "ns" ns-disp
               "port" port
               "error"
               (str
                 "nREPL at localhost:" port
                 " went down mid-run (" (.getMessage e)
                 ") — the server is no longer reachable. Stop it, "
                 "then start a fresh one (repl \"clojure\" \"stop\", then repl \"clojure\" \"start\"), and retry.")
               "repl_unusable" true}
              (throw e))))))))

(defn- cli-tail
  "Last 40 lines of a CLI test run's combined out+err, ANSI-stripped so the
   stored :output renders clean in every channel."
  [^String s]
  (let [lines (str/split-lines (strip-ansi (or s "")))]
    (str/join "\n" (take-last 40 lines))))

(defn- summary-counts
  "Read complete, anchored reporter summaries. Never splice counts from unrelated
   log lines, or let a later passing suite erase an earlier failure. cljs.test
   requires both failure and error counts; Lazytest can omit the error count."
  ([out] (summary-counts out false))
  ([out require-errors?]
   (let [out
         (strip-ansi (str out))

         conventional
         (re-seq #"(?m)^Ran (\d+) test[^\r\n]*\r?\n(\d+) failures?(?:, (\d+) errors?)?\.\r?$" out)

         kaocha
         (re-seq
           #"(?m)^(\d+) tests?, \d+ assertions?, ((?:\d+ (?:failures?|errors?)(?:, )?)+)\.\r?$"
           out)

         reports
         (concat (keep (fn [[_ cases fails errs]]
                         (when (or (not require-errors?) errs)
                           {:cases (parse-long cases)
                            :fails (parse-long fails)
                            :errs (if errs (parse-long errs) 0)}))
                       conventional)
                 (when-not require-errors?
                   (map (fn [[_ cases counts]]
                          (let [n (fn [pattern]
                                    (or (some-> (re-find pattern counts)
                                                second
                                                parse-long)
                                        0))]
                            {:cases (parse-long cases)
                             :fails (n #"(\d+) failures?")
                             :errs (n #"(\d+) errors?")}))
                        kaocha)))

         headers
         (count (re-seq #"(?m)^(?:Ran \d+ test|\d+ tests?, \d+ assertions?,)" out))]

     (when (and (seq reports) (= headers (count reports))) (apply merge-with + reports)))))

(defn- lazytest-selector-args
  "Translate resolved selectors into lazytest.main CLI flags.
   :vars — what the call's `<path>::<name>` node ids resolved to — becomes --var
   for precise targeting: an entry that carries its namespace targets that one
   var, a pathless `::name` entry is cross-producted over the selected nses.
   A node id may name the SOURCE var it covers, so the `-test` spelling is passed
   ALONGSIDE it: the repl path resolves that against LIVE vars, while a shelled
   runner can only be handed both (lazytest DROPS a --var that matches nothing,
   it does not fail). With no vars, --namespace filters at namespace level.
   --include and --exclude are always passed when present."
  [{:keys [nses vars include exclude]}]
  (vec
    (concat (if (seq vars)
              (mapcat (fn [{:keys [ns name]}]
                        (mapcat (fn [n]
                                  (mapcat (fn [nm]
                                            ["--var" (str n "/" nm)])
                                          (if (str/ends-with? (str name) "-test")
                                            [name]
                                            [name (str name "-test")])))
                                (if ns [ns] nses)))
                      vars)
              (mapcat (fn [ns]
                        ["--namespace" (str ns)])
                      nses))
            (mapcat (fn [t]
                      ["--include" (str t)])
                    include)
            (mapcat (fn [t]
                      ["--exclude" (str t)])
                    exclude))))

(defn- runner-entry
  "Classify an alias's executable entry point, not its name or dependencies.
   Unknown entry points can run a whole suite, but have no focus adapter."
  [{:keys [main-opts exec-fn]}]
  (let [main (some (fn [[flag arg]]
                     (when (and (#{"-m" "-e"} flag) (not (str/blank? arg)))
                       (if (= "-m" flag) arg "<expression>")))
                   (partition 2 1 main-opts))]
    (cond main {:mode "-M"
                :entry main
                :framework ({"lazytest.main" :lazytest "kaocha.runner" :kaocha} main)}
          (qualified-symbol? exec-fn)
          {:mode "-X" :entry (str exec-fn) :framework ({'kaocha.runner/exec-fn :kaocha} exec-fn)})))

(defn- focused?
  "An explicit selector, as opposed to the index discovered for an unfiltered run."
  [sel]
  (if (contains? sel :focused?)
    (:focused? sel)
    (boolean (some seq ((juxt :nses :vars :include :exclude) sel)))))

(defn- runner-selector-args
  "Translate focus using the detected runner's API. Kaocha -X takes EDN config,
   not main's CLI flags. Never silently broaden unsupported selector combinations."
  [{:keys [framework mode]} sel]
  (cond
    (not (focused? sel)) {:args []}
    (nil? framework) {:error "this runner has no supported focus adapter; no tests started"}
    (and (= :kaocha framework)
         (or (and (seq (:include sel)) (or (seq (:nses sel)) (seq (:vars sel))))
             (and (seq (:exclude sel))
                  (or (seq (:nses sel)) (seq (:vars sel)) (seq (:include sel))))))
    {:error
     "Kaocha combines focus/metadata filters differently; this selector combination is unsupported, so no tests started"}
    :else (let [pairs (partition 2 (lazytest-selector-args sel))]
            {:args (vec
                     (case framework
                       :lazytest
                       (mapcat identity pairs)

                       :kaocha
                       (if (= "-X" mode)
                         (mapcat (fn [[k entries]]
                                   [(str k) (pr-str (mapv (comp symbol second) entries))])
                                 (sort-by (comp str key)
                                          (group-by (fn [[flag _]]
                                                      ({"--namespace" :kaocha.filter/focus
                                                        "--var" :kaocha.filter/focus
                                                        "--include" :kaocha.filter/focus-meta
                                                        "--exclude" :kaocha.filter/skip-meta}
                                                       flag))
                                                    pairs)))
                         (mapcat (fn [[flag value]]
                                   [({"--namespace" "--focus"
                                      "--var" "--focus"
                                      "--include" "--focus-meta"
                                      "--exclude" "--skip-meta"}
                                     flag) value])
                                 pairs))))})))

(defn- inherited-runner-error
  "A one-shot operation cannot watch, or append selectors to inherited filters
   whose union/precedence may change the request. Leave such aliases untouched."
  [runner opts sel]
  (let [main
        (:main-opts opts)

        exec
        (:exec-args opts)

        flag?
        (fn [pattern]
          (some #(re-find pattern (str %)) main))

        filter-keys
        [:kaocha.filter/focus :kaocha.filter/skip :kaocha.filter/focus-meta
         :kaocha.filter/skip-meta]]

    (cond
      (or (flag? #"^(?:--watch|-w)(?:=true)?$") (and (= "-X" (:mode runner)) (:kaocha/watch? exec)))
      "the selected runner enables watch mode; run_tests requires a one-shot runner, so no tests started"
      (and (focused? sel)
           (or (flag?
                 #"^--(?:focus|focus-meta|skip|skip-meta|namespace|var|include|exclude)(?:=|$)")
               (and (= "-X" (:mode runner)) (some #(seq (get exec %)) filter-keys))))
      "the selected runner already has focus/metadata filters; use an unfiltered runner alias to honor this selection")))

(defn- deps-command
  "Discover a runnable deps.edn alias. Keep :test when declared for its classpath;
   prefer an explicitly supplied runner, then executable :test, then a unique
   recognized entry point. Never guess an unrelated -X build/deploy function."
  [root sel requested]
  (let [aliases
        (:aliases (edn/read-string (slurp (io/file root "deps.edn"))))

        requested
        (mapv keyword requested)

        unknown
        (remove #(contains? aliases %) requested)

        entries
        (into {}
              (keep (fn [[k v]]
                      (when-let [entry (runner-entry v)]
                        [k entry])))
              aliases)

        explicit
        (last (filter entries requested))

        candidates
        (sort-by str
                 (keep (fn [[k v]]
                         (when (:framework v) k))
                       entries))

        picked
        (or explicit
            (when (entries :test) :test)
            (when (= 1 (count candidates)) (first candidates)))]

    (cond
      (seq unknown) {:error (str "unknown deps.edn aliases: " (pr-str (vec unknown)))}
      (nil? picked)
      {:error
       (if (seq candidates)
         (str "multiple executable test runners "
              (pr-str (vec candidates))
              " — select one with aliases; no tests started")
         "deps.edn has no executable test runner (:main-opts or a supported :exec-fn); a classpath-only :test alias would open a REPL, so no tests started")}
      :else
      (let [active
            (vec (distinct (concat (when (contains? aliases :test) [:test])
                                   (when-not explicit [picked])
                                   requested)))

            combined
            (assoc (apply merge (map aliases active))
              :exec-args (apply merge
                           (keep #(when (map? (:exec-args %)) (:exec-args %))
                                 (map aliases active))))

            mode
            (:mode (entries picked))

            runner
            (runner-entry
              (if (= "-X" mode) (select-keys combined [:exec-fn]) (dissoc combined :exec-fn)))

            {:keys [args error]}
            (runner-selector-args runner sel)

            jflags
            (mapv #(str "-J" %) (repl-manager/inherited-jvm-opts (io/file root) active))]

        (cond (nil? runner)
              {:error "selected aliases override the executable runner options; no tests started"}
              (inherited-runner-error runner combined sel)
              {:error (inherited-runner-error runner combined sel)}
              error {:error (str (:entry runner) ": " error)}
              :else {:tool :clj
                     :framework (:framework runner)
                     :cmd (into (into ["clojure"] jflags) (cons (str mode (apply str active)) args))
                     :selectors? (boolean (:framework runner))})))))

(defn- cli-command-for
  "Choose the project's executable test command and translate supported focus.
   aliases add classpath or select a runner; a declared :test is retained, not
   invented. Unsupported focus is refused before any process starts."
  [root sel aliases]
  (let [present? (fn [n]
                   (.isFile (io/file root n)))]
    (cond (present? "deps.edn") (try (deps-command root sel aliases)
                                     (catch Exception e
                                       {:error (str "cannot read test runner configuration: "
                                                    (ex-message e))}))
          (present? "project.clj") {:tool :lein :cmd ["lein" "test"] :selectors? false}
          (present? "bb.edn") {:tool :bb :cmd ["bb" "test"] :selectors? false}
          :else nil)))

(defn- run-via-cli
  "Run the discovered command in a clean JVM. Exit zero is insufficient: require
   a nonempty test summary, preserve effective namespace focus, and report the
   executed test count as selected (the common numeric result contract)."
  [root norm]
  (let [sel
        (cond-> (select-keys norm [:nses :vars :include :exclude :focused?])
          (and (false? (:namespace-focus? norm)) (empty? (:vars norm)))
          (assoc :nses []))

        aliases
        (:aliases norm)

        {:keys [tool cmd error framework selectors?] :as plan}
        (cli-command-for root sel aliases)

        base
        (cond-> {"mode" "cli" "ns" (str/join " " (:nses sel)) "is_pass" false}
          tool
          (assoc "tool" (name tool))

          cmd
          (assoc "command" (str/join " " cmd))

          framework
          (assoc "framework" (name framework)))]

    (cond
      error (assoc base "error" error)
      (nil? plan) (assoc base
                    "error" (str "no nREPL reachable, and no deps.edn / project.clj / bb.edn in "
                                 root
                                 " to run tests via CLI"))
      (and (seq aliases) (not= :clj tool)) (assoc base
                                             "error" (str "aliases "
                                                          (pr-str (vec aliases))
                                                          " are deps.edn aliases, but "
                                                          root
                                                          " is a "
                                                          (name tool)
                                                          " project; no tests started"))
      (and (focused? sel) (not selectors?))
      (assoc base "error" "this runner has no supported focus adapter; no tests started")
      :else
      (let [res
            (try (apply shell/sh (concat cmd [:dir (str root)]))
                 (catch Throwable t {:exit -1 :out "" :err (ex-message t)}))

            out
            (str (:out res) (:err res))

            exit
            (long (or (:exit res) -1))

            {:keys [cases fails errs]}
            (summary-counts out)

            faults
            (+ (long (or fails 0)) (long (or errs 0)))

            ran?
            (and (some? cases) (or (some? fails) (some? errs)))

            ignored-focus?
            (and (= :kaocha framework) (str/includes? out "No tests found with metadata key"))]

        (cond-> (assoc base
                  "exit" exit
                  "output" (cli-tail out)
                  "is_pass" (boolean (and (zero? exit)
                                          ran?
                                          (pos? (long cases))
                                          (zero? faults)
                                          (not ignored-focus?))))
          (some? cases)
          (assoc "total"
            cases "selected"
            cases)

          ran?
          (assoc "fail"
            faults "errored"
            (long (or errs 0)))

          (and (zero? exit) (not ran?))
          (assoc "error"
            "test command exited 0 but printed no test summary — no verified test run (possibly a bare REPL or unsupported reporter)")

          (and (some? cases) (zero? (long cases)))
          (assoc "error"
            "test command ran 0 tests; check the selected namespaces and runner configuration")

          ignored-focus?
          (assoc "error"
            "Kaocha ignored an unmatched metadata filter; the requested focus was not verified"))))))

(defn- shadow-tail
  "shadow-cljs boots a JVM whose Unsafe/deprecation warnings are four lines of
   pure noise on EVERY run — dropped here so the tail the caller reads is the
   test report itself."
  [^String out]
  (cli-tail
    (str/join
      "\n"
      (remove (fn [line]
                (re-find
                  #"^WARNING: (A terminally deprecated|sun\.misc\.Unsafe|Please consider reporting)"
                  line))
        (str/split-lines (str out))))))

(defn- karma-summary-counts
  "Karma reports TOTAL or completed per-browser progress. Keep earlier failures,
   aggregate browsers/batches, and refuse partial or contradictory reports."
  [out]
  (let [out
        (strip-ansi (str out))

        n
        (fn [pattern s]
          (some-> (re-find pattern (str s))
                  second
                  parse-long))

        totals
        (mapv (fn [[_ tail]]
                (let [failed
                      (n #"(\d+) FAILED" tail)

                      passed
                      (n #"(\d+) SUCCESS" tail)]

                  {:cases (+ (long (or failed 0)) (long (or passed 0)))
                   :fails (or failed 0)
                   :complete? (or (some? failed) (some? passed))}))
              (re-seq #"(?m)^TOTAL: ([^\r\n]+)" out))

        browsers
        (reduce (fn [reports [_ browser executed expected tail]]
                  (let [cases
                        (parse-long executed)

                        failed
                        (n #"(\d+) FAILED" tail)]

                    (assoc reports
                      browser
                      {:cases cases
                       :fails (max (long (or failed 0)) (long (get-in reports [browser :fails] 0)))
                       :complete? (and (= (+ (long cases) (long (or (n #"(\d+) skipped" tail) 0)))
                                          (parse-long expected))
                                       (or (some? failed) (str/includes? tail "SUCCESS")))})))
                {}
                (re-seq #"(?m)^([^\r\n]+?): Executed (\d+) of (\d+)([^\r\n]*)" out))

        reports
        (if (seq totals) totals (vals browsers))

        failures
        (reduce + 0 (map :fails reports))]

    (when (and (seq reports)
               (every? :complete? reports)
               ;; TOTAL is authoritative for counts, but cannot erase a browser failure.
               (<= (reduce + 0 (map :fails (vals browsers))) failures))
      {:cases (reduce + 0 (map :cases reports)) :fails failures})))

(defn- run-via-shadow*
  "Use the project's own shadow launcher and build. Never silently broaden
   unsupported var/tag selectors or classpath aliases. A pass requires actual
   tests reported by the final execution step, including Karma's own reporter."
  [root nses norm output-root]
  (let
    [nses
     (if (focused? (assoc norm :nses nses)) nses [])

     unsupported
     (cond
       (seq (:aliases norm))
       "configure :deps {:aliases [...]} in shadow-cljs.edn; run_tests aliases cannot change the build's classpath"
       (some seq ((juxt :vars :include :exclude) norm))
       "the Vis shadow-cljs adapter currently supports namespace focus, not var or metadata selectors; no tests started")

     {:keys [error steps build target]}
     (if unsupported
       {:error unsupported}
       (shadow/run-steps root {:nses nses :build (:build norm) :output-root output-root}))

     base
     {"mode" "cli"
      "tool" "shadow-cljs"
      "framework" "cljs.test"
      "ns" (str/join " " nses)
      "is_pass" false}]

    (if error
      (assoc base "error" error)
      (let
        [ran
         (reduce
           (fn [acc {:keys [argv compile?]}]
             (let
               [res
                (try (apply shell/sh (concat argv [:dir (str root)]))
                     (catch Throwable t {:exit -1 :out "" :err (ex-message t)}))

                out
                (str (:out res) (:err res))

                acc
                (-> acc
                    (update :out str out)
                    (update :cmds conj (str/join " " argv))
                    (assoc :last-out out
                           :exit (long (or (:exit res) -1))))

                compiled?
                (or (not compile?)
                    (re-find (re-pattern (str "\\[:"
                                              (java.util.regex.Pattern/quote build)
                                              "\\] Build completed\\."))
                             (strip-ansi out)))

                acc
                (cond-> acc
                  (and (zero? (:exit acc)) (not compiled?))
                  (assoc :error
                    "shadow-cljs did not confirm compilation; nothing verified and no stale JavaScript executed"))]

               (if (and (zero? (long (:exit acc))) (not (:error acc))) acc (reduced acc))))
           {:out "" :exit 0 :cmds []}
           steps)

         exit
         (long (:exit ran))

         {:keys [cases fails errs]}
         (if (= :karma target)
           (karma-summary-counts (:last-out ran))
           (summary-counts (:last-out ran) true))

         faults
         (+ (long (or fails 0)) (long (or errs 0)))

         complete?
         (and (some? cases) (or (some? fails) (some? errs)))

         reported-nses
         (set (map second (re-seq #"(?m)^Testing ([^\r\n ]+)\r?$" (strip-ansi (:last-out ran)))))

         missing-nses
         (when (= :node-test target) (remove reported-nses nses))]

        (cond-> (assoc base
                  "build" build
                  "command" (str/join " && " (:cmds ran))
                  "exit" exit
                  "output" (shadow-tail (:out ran))
                  "is_pass" (boolean (and (zero? exit)
                                          (not (:error ran))
                                          (empty? missing-nses)
                                          complete?
                                          (pos? (long cases))
                                          (zero? faults))))
          (some? cases)
          (assoc "total"
            cases "selected"
            cases)

          (or fails errs)
          (assoc "fail" faults)

          errs
          (assoc "errored" errs)

          (seq missing-nses)
          (assoc "error"
            (str "Node did not report all requested namespaces: " (str/join ", " missing-nses)))

          (:error ran)
          (assoc "error" (:error ran))

          (and (zero? exit) (not (:error ran)) (not complete?))
          (assoc "error"
            "shadow-cljs exited 0 but printed no completed test summary — nothing verified; compilation or CLI help is not a test run")

          (and (some? cases) (zero? (long cases)))
          (assoc "error"
            (str "shadow-cljs ran 0 tests for build "
                 build
                 " — check the namespace selection and the configured source paths/classpath")))))))

(defn- run-via-shadow
  "Own one run's output directory through compilation and Node execution.
   Keep it under the project so Node still resolves project dependencies. Never
   traverse symlinks during cleanup or delete the user's watch output."
  [root nses norm]
  (let [dir (java.nio.file.Files/createTempDirectory
              (.toPath (io/file root))
              ".vis-shadow-run-"
              (make-array java.nio.file.attribute.FileAttribute 0))]
    (try (run-via-shadow* root nses norm (str dir))
         (finally (with-open [paths (java.nio.file.Files/walk
                                      dir
                                      (make-array java.nio.file.FileVisitOption 0))]
                    (doseq [^java.nio.file.Path path (reverse (vec (.toArray paths)))]
                      (java.nio.file.Files/deleteIfExists path)))))))

(defn- recover-if-unusable
  "Recovery seam for run_tests: what happens when the REUSED nREPL lets a run down.
   \"repl_unusable\" (down / gone mid-run) reruns the suite through the build tool's
   CLI in a clean JVM, so the caller still gets REAL results THIS turn instead of a
   'start a fresh REPL and retry' error that burns the turn. \"repl_wedged\" (hung past
   the timeout) is left CLI-less: the hang is likely the code under test, which a CLI
   run would only re-hang on. Nothing here starts or relaunches a REPL — reviving one
   is the caller's own `repl_start` call. The outcome is announced on :note so the result
   explains itself."
  [root norm result]
  (cond (get result "repl_unusable")
        (let [cli
              (run-via-cli root norm)

              why
              (get result "error")

              note
              (str "the reused nREPL was unusable"
                   (when why (str " (" why ")"))
                   " — ran the suite in a clean JVM via the build tool's CLI instead.")]

          (-> cli
              (assoc "recovered" true)
              (update "note"
                      (fn [n]
                        (if (seq (str n)) (str note " " n) note)))))
        (get result "repl_wedged")
        (update result
                "error"
                (fn [e]
                  (str e
                       " Stop it (repl_stop(\"clojure\")) — the next run then uses a clean JVM.")))
        :else result))

(defn- has-build-file?
  "True when `cwd` holds a Clojure build manifest (deps.edn / project.clj / bb.edn)."
  [^java.io.File dir]
  (boolean (some (fn [n]
                   (.isFile (io/file dir n)))
                 ["deps.edn" "project.clj" "bb.edn"])))

(defn- within-root?
  "True when `d` is `root-canon` itself or a directory nested under it."
  [^String root-canon ^java.io.File d]
  (let [dc (try (.getCanonicalPath d) (catch Throwable _ (.getPath d)))]
    (or (= dc root-canon) (str/starts-with? dc (str root-canon java.io.File/separator)))))

(defn- nearest-build-root
  "Closest ancestor directory of `start` (a File dir or file), at or below `root`,
   that holds a Clojure build manifest — i.e. the project the tests belong to, so a
   managed nREPL boots where its own deps.edn lives instead of the workspace root.
   Never escapes above `root`; falls back to `root` when none is found."
  ^java.io.File [^java.io.File root ^java.io.File start]
  (let [root-canon (try (.getCanonicalPath root) (catch Throwable _ (.getPath root)))]
    (loop [d (if (.isDirectory start) start (.getParentFile start))]
      (cond (or (nil? d) (not (within-root? root-canon d))) root
            (has-build-file? d) d
            :else (recur (.getParentFile d))))))

(defn- effective-test-root
  "The project root the requested tests belong to: the nearest build-file ancestor
   SHARED by every requested location, so a nested project runs against its OWN
   deps.edn. `locations` are absolute File dirs/files. Returns `root` when the
   locations disagree (a mixed run) or none is nested."
  ^java.io.File [^java.io.File root locations]
  (let [roots (distinct (map #(nearest-build-root root %) locations))]
    (if (= 1 (count roots)) (first roots) root)))

(defn- nearest-shadow-root
  "Closest ancestor directory of `start`, at or below `root`, that holds a
   `shadow-cljs.edn` — the project a ClojureScript run belongs to. A shadow
   project is often npm-only, with no deps.edn / project.clj / bb.edn anywhere in
   it, so the JVM build-file search would climb straight past it to a parent that
   cannot run the tests at all."
  ^java.io.File [^java.io.File root ^java.io.File start]
  (let [root-canon (try (.getCanonicalPath root) (catch Throwable _ (.getPath root)))]
    (loop [d (if (.isDirectory start) start (.getParentFile start))]
      (cond (or (nil? d) (not (within-root? root-canon d))) root
            (.isFile (io/file d "shadow-cljs.edn")) d
            :else (recur (.getParentFile d))))))

(defn- effective-shadow-root
  "The shadow-cljs project the requested ClojureScript tests belong to: the
   nearest `shadow-cljs.edn` ancestor SHARED by every selected test file.
   Refuse independent projects instead of falling back to an unrelated parent."
  ^java.io.File [^java.io.File root locations]
  (let [roots (distinct (map (fn [location]
                               (nearest-shadow-root root location))
                             locations))]
    (when (> (count roots) 1)
      (throw
        (ex-info
          "selection spans multiple shadow-cljs projects; run each project separately with cwd/paths"
          {:type :clj/bad-args})))
    (if (= 1 (count roots)) (first roots) root)))

(defn- note-unapplied-aliases
  "A reused nREPL cannot change its startup classpath. Say that explicitly when
   aliases were supplied; CLI runs have already applied or refused them."
  [aliases result]
  (if (or (empty? aliases) (= "cli" (get result "mode")))
    result
    (let [why
          (case (str (get result "mode"))
            "repl"
            (str "the run REUSED this session's nREPL, which carries only the aliases"
                 " `repl_start` booted it with — repl_stop(\"clojure\") first for a clean"
                 " JVM, or restart that REPL with these aliases")

            "this run did not invoke the deps.edn CLI")

          note
          (str "aliases " (pr-str (vec aliases)) " did NOT apply: " why ".")]

      (update result
              "note"
              (fn [n]
                (if (str/blank? (str n)) note (str note " " n)))))))

(defn clj-test-fn
  "Run tests selected by paths, namespaces or vars. Source paths map to their
   nearest *-test namespace; JS/shared files also recognize the configured
   shadow-cljs :ns-regexp/:namespaces instead of requiring a _test filename.
   Missing paths and explicit-but-empty selections fail instead of running all.

   The files and an optional build choose the runtime. .cljs cannot run on the
   JVM, .clj cannot run in JS, and .cljc can accompany either. An explicit build
   selects JS for shared tests. Mixed JVM/JS selections are refused, never
   silently trimmed. The nearest project manifest roots the command.

   No REPL is started: reuse this session's JVM REPL or shell an executable
   project runner. aliases add classpath or select that runner. Lazytest/Kaocha
   receive their own focus arguments; unsupported focus fails before launch.
   A reused REPL cannot apply aliases and says so. JS uses the project's own
   shadow launcher/config; no shadow dependency is loaded into Vis. JS var/tag
   selectors are refused instead of widened to namespace runs.

   An unfiltered CLI run leaves suite discovery to the project runner/build;
   local filename discovery does not override its configured suite. A reused
   REPL still uses the local index, loading only the test namespaces (callers
   must reload changed production dependencies or stop that REPL themselves).
   CLI verdicts require nonempty test summaries; selected is the executed test
   count, while ns/command identify effective focus. Language stays clojure."
  ([env arg]
   (let [;; An explicit `cwd` (the run_tests `cwd` param) roots the run — and thus
         ;; nREPL selection — at THAT project instead of the workspace root, so a
         ;; SIBLING / added-folder project runs against its OWN nREPL classpath
         ;; rather than booting the workspace-root REPL (whose classpath lacks it).
         req-dir
         (when (map? arg) (get arg "cwd"))

         root
         (let [wsroot (or (:workspace/root env)
                          (throw (ex-info "run_tests(clojure) fired without :workspace/root in env"
                                          {:type :clj/no-workspace})))]
           (if (str/blank? (str req-dir))
             wsroot
             (let [f (io/file (str req-dir))]
               (.getPath (if (.isAbsolute f) f (io/file wsroot (str req-dir)))))))

         {:keys [paths ns-selectors] :as norm}
         (normalize-arg arg)

         ;; Cache each nested project's config for this discovery pass. Only JS
         ;; and shared sources use shadow's namespace rules; JVM discovery stays
         ;; independent of the JavaScript toolchain.
         shadow-config
         (memoize (fn [dir]
                    (shadow/config dir)))

         test-file?
         (fn [^java.io.File file]
           (or (test-source-file? file)
               (and (clj-source-file? file)
                    (not (str/ends-with? (.getName file) ".clj"))
                    (when-let [ns-str (ns-of-file file)]
                      (shadow/test-namespace?
                        (shadow-config (.getPath (nearest-shadow-root (io/file root) file)))
                        (:build norm)
                        ns-str)))))

         resolved
         (resolve-selection root paths ns-selectors test-file?)

         ;; Locations the caller EXPLICITLY asked for — used ONLY to find the tests'
         ;; own project root. Empty for a bare "run everything" call (and for a
         ;; pathless `::name` id), which stays rooted at the workspace so it never
         ;; file-seqs per namespace.
         req-locations
         (into (vec (keep (fn [{:keys [path]}]
                            (when path (under-root root path)))
                          paths))
               (:files resolved))

         ;; No LOCATION at all = "run everything", so every *_test ns in the
         ;; workspace runs and a bare `::name` narrows inside it; a location that
         ;; resolves to nothing is explicit-but-empty and stays an error below. An
         ;; empty list [] counts as "not given" (empty? is total on nil), so [] and
         ;; nil behave identically here.
         selection
         (cond (or (some :path paths) (some :ns ns-selectors)) (select-keys resolved
                                                                            [:nses :ns-files])
               (:build norm) {:nses [] :ns-files {}}
               :else (let [index (all-test-files root test-file?)]
                       {:nses (vec (sort (keys index))) :ns-files index}))

         ;; An explicit build selects JS, including shared .cljc tests. Otherwise
         ;; only .clj is JVM-only; .cljc can accompany either runtime. Never drop
         ;; half a mixed selection and call the remaining half a passing run.
         cljs-nses
         (filterv (fn [n]
                    (cljs-file? (get (:ns-files selection) n)))
           (:nses selection))

         jvm-nses
         (filterv (fn [n]
                    (if-let [^java.io.File file (get (:ns-files selection) n)]
                      (str/ends-with? (.getName file) ".clj")
                      (not (:build norm))))
           (:nses selection))

         shadow-root
         (when (or (:build norm) (seq cljs-nses) (empty? jvm-nses))
           (effective-shadow-root (io/file root) (vals (:ns-files selection))))

         cljs?
         (or (:build norm)
             (seq cljs-nses)
             (and shadow-root (shadow-config (.getPath ^java.io.File shadow-root))))

         {:keys [nses] :as norm}
         (assoc norm
           :vars (:vars resolved)
           :nses (:nses selection)
           :namespace-focus? (boolean (or (some :path paths) (some :ns ns-selectors)))
           :focused?
           (boolean
             (or (seq paths) (seq ns-selectors) (seq (:include norm)) (seq (:exclude norm)))))

         sel
         (select-keys norm [:vars :include :exclude])

         ;; Root the run where the tests' OWN build file lives (nearest deps.edn /
         ;; project.clj / bb.edn at or below the workspace root; nearest
         ;; shadow-cljs.edn when the run is ClojureScript), so a nested project's
         ;; build file is honored. Falls back to the workspace root when the
         ;; request is at the top level or spans several projects.
         eff-root
         (cond cljs? (.getPath ^java.io.File shadow-root)
               (seq req-locations) (.getPath (effective-test-root (io/file root) req-locations))
               :else root)

         ;; REUSE, never spawn. `live-repl-for-dir` answers THIS session's REPL for the
         ;; project only while it ANSWERS, nil otherwise — run_tests starts nothing. With
         ;; no REPL up the suite runs in a clean JVM through the build tool's own test
         ;; command, which is also what a fresh session gets. A ClojureScript run never
         ;; asks: a JVM nREPL cannot load a `.cljs` namespace.
         port
         (when-not cljs? (:port (repl-manager/live-repl-for-dir (:session-id env) eff-root)))]

     ;; An explicit location that is NOT THERE is a misspelling, not an empty
     ;; suite: answering "no test namespaces under <path>" for a path that does
     ;; not exist sends the caller hunting for missing tests or a broken build
     ;; file instead of the wrong segment. Name the deepest part that DOES
     ;; exist and the next segment is the typo.
     (when-let [missing (seq (missing-locations root paths ns-selectors))]
       (throw (ex-info (str "run_tests(clojure) no such path: "
                            (str/join "; "
                                      (map (fn [{:keys [path exists]}]
                                             (str (pr-str path)
                                                  (when exists
                                                    (str " — exists up to " (pr-str exists)))))
                                           missing)))
                       {:type :clj/bad-args :got arg :missing (mapv :path missing)})))
     (when (and cljs? (seq jvm-nses))
       (throw
         (ex-info
           "selection contains JVM and ClojureScript tests; run each runtime separately with paths/ns (or a build without JVM paths)"
           {:type :clj/bad-args})))
     (when (and (empty? nses)
                (or (:namespace-focus? norm)
                    (seq (:vars norm))
                    (not (or cljs? (has-build-file? (io/file eff-root))))))
       (let [named (into (vec (keep :path paths)) (keep :ns ns-selectors))]
         (throw
           (ex-info
             (if (seq named)
               (str
                 "run_tests(clojure) found no test namespaces (*_test.clj / *_test.cljc / *_test.cljs) under "
                 (pr-str named))
               (str
                 "run_tests(clojure) found no test namespaces (*_test.clj / *_test.cljc / *_test.cljs) "
                 "anywhere under the workspace root"))
             {:type :clj/bad-args :got arg}))))
     (let [result
           (cond
             ;; ClojureScript: the project's shadow-cljs build, shelled. There is no
             ;; JVM path to fall back to.
             cljs? (run-via-shadow eff-root nses norm)
             ;; A REPL this session already keeps up for the project — the fast inner
             ;; loop. It reloads only the namespaces it RUNS, so production Vars the
             ;; caller edited stay as that REPL holds them (`repl_eval` `:reload`, or
             ;; stop the REPL and let the clean JVM run it).
             port (run-via-repl eff-root nses sel port)
             ;; The default: the build tool's own test command, in a clean JVM.
             :else (run-via-cli eff-root norm))

           result
           (recover-if-unusable eff-root norm result)

           result'
           (if (and (get result "error")
                    (str/includes? (get result "error") "Could not locate lazytest/core"))
             (run-via-cli eff-root norm)
             result)]

       (extension/success {:result (surface/check :test-fn
                                                  (assoc (note-unapplied-aliases (:aliases norm)
                                                                                 result')
                                                    "language" "clojure"))})))))
