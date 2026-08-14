(ns com.blockether.vis.ext.language-clojure.test-runner
  "Run a namespace's tests over the live nREPL (the fast inner loop) or, when no
   nREPL is reachable, by shelling clojure -M:test (the suite gate).

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
            [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
            [com.blockether.vis.ext.language-clojure.repl-manager :as repl-manager]
            [com.blockether.vis.internal.test-contract :as contract]
            [com.blockether.vis.internal.foundation.surface-contract :as surface]
            [com.blockether.vis.internal.extension :as extension]))

(def ^:private default-test-timeout-ms
  "Default budget for run_tests. The whole test run is parked OUTSIDE the
   native tool wall (see language-surface `run-tests`), so THIS is the real
   budget: an nREPL timeout surfaces as a structured test result instead of
   an opaque harness kill. Must stay well below the outside-wall wedge guard
   (MAX_EVAL_TIMEOUT_MS, 30 min)."
  290000)

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
      (let
        [vars*
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
           (let
             [tags
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
         (binding
           [clojure.core/*out*
            out-writer

            clojure.core/*err*
            out-writer]

           (if (seq all-ct)
             (or
               (var-miss "clojure.test" all-ct)
               (let
                 [selected
                  (vec (filter keep? all-ct))

                  skipped
                  (- (count all-ct) (count selected))

                  fails
                  (atom [])

                  cnt
                  (atom {:pass 0 :fail 0 :error 0})]

                 (with-redefs
                   [clojure.test/report
                    (fn [m]
                      (when (#{:fail :error :pass} (:type m))
                        (swap! cnt update (:type m) (fnil inc 0)))
                      (when (#{:fail :error} (:type m))
                        (let
                          [v0 (first clojure.test/*testing-vars*)
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
                                              (when-let [msg (.getMessage ^Throwable thrown)]
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
                 (let
                   [c
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
               (let
                 [selected
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
   (binding
     [*print-length*
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
  (let
    [raw
     (get fault "file")

     s
     (str raw)]

    (if (and root (not (str/blank? s)))
      (try (let
             [rp
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
   what `failures->text` and `group-faults-by-cwd` already render as \"no
   location\"."
  [fault]
  (let
    [file
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
  (let
    [root-file
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
           (let
             [keep?
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

(defn group-faults-by-cwd
  "Regroup the flat `failures` vector into the SAME directory-nested `by-cwd`
   shape lint and format expose, writing each file's directory ONCE:
   `{<dir> {<basename> {\"failures\" [...]}}}`.
   `<dir>` is the failing file's parent (`\".\"` when it has none, e.g. a bare JVM
   frame like `Numbers.java` or a missing file) and the inner key is the
   basename, so the long path prefix isn't repeated per file — the same
   character saving `lint/group-by-cwd` gives lint. A fault with no usable file
   lands under `\".\"`/`\"<unknown>\"`. Erroring tests are not grouped apart: each
   fault carries its own `\"type\"`."
  [failures]
  (reduce (fn [m fault]
            (let
              [raw
               (get fault "file")

               file
               (when-not (str/blank? (str raw)) (str raw))

               dir
               (if file (or (.getParent (java.io.File. ^String file)) ".") ".")

               base
               (if file (.getName (java.io.File. ^String file)) "<unknown>")]

              (update-in m [dir base "failures"] (fnil conj []) fault)))
          {}
          failures))

(defn- compose-repl-output
  "Final `output` for a repl-mode result: the tests' OWN captured stdout (ANSI-
   stripped — empty on a quiet pass now the reporter is silenced) followed by a tidy
   `failures->text` digest when the run failed. Never the framework's per-namespace
   reporter tree, so a green run reads clean and a red one shows only what broke."
  [parsed]
  (let
    [cap
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

(defn- all-test-files
  "Index every *_test.clj under root by its declared ns string, built once per
   run so SOURCE paths can be resolved to their corresponding test namespace."
  [root]
  (into {}
        (keep (fn [^java.io.File f]
                (when (and (.isFile f) (str/ends-with? (.getName f) "_test.clj"))
                  (when-let [ns (ns-of-file f)]
                    [ns f]))))
        (file-seq (io/file root))))

(defn- path->nses
  "Resolve ONE file/dir to test namespace strings. A *_test.clj file -> its own
   ns. A plain source .clj file -> its matching *-test ns (when that test file
   exists). A directory -> every *_test.clj under it; a pure source dir maps each
   source ns to its existing *-test ns. `test-index` is a DELAY over {ns-str file}
   — naming test files never pays for the workspace walk a source file needs."
  [^java.io.File f test-index]
  (let
    [test-ns (fn [src-ns]
               (let [tn (source-ns->test-ns src-ns)]
                 (when (contains? @test-index tn) tn)))]
    (cond (and (.isFile f) (str/ends-with? (.getName f) "_test.clj")) (keep identity
                                                                            [(ns-of-file f)])
          (and (.isFile f) (str/ends-with? (.getName f) ".clj")) (keep test-ns [(ns-of-file f)])
          (.isDirectory f)
          (let
            [test-files (filter (fn [^java.io.File x]
                                  (and (.isFile x) (str/ends-with? (.getName x) "_test.clj")))
                                (file-seq f))]
            (if (seq test-files)
              (keep ns-of-file test-files)
              (->> (file-seq f)
                   (filter (fn [^java.io.File x]
                             (and (.isFile x) (str/ends-with? (.getName x) ".clj"))))
                   (keep ns-of-file)
                   (keep test-ns))))
          :else [])))

(defn- resolve-selection
  "Resolve the CALL's node-id entries (`{:path :var}`, from
   `contract/split-node-id`) into what a run needs: `:nses`, the test namespaces
   to load, and `:vars`, the var filter to apply inside them
   (`{:ns <ns-or-nil> :name <test-name>}`).
   Each entry is resolved ON ITS OWN, so `a_test.clj::x` and `b_test.clj::y`
   pair each name with its OWN file instead of cross-producting into both. An
   entry with no path (`::x`) names a var wherever it lives — nil :ns, and no
   namespace of its own, so the other entries (or the whole-workspace default)
   decide where to look. Paths are relative to root or absolute; files AND
   directories are accepted, and SOURCE files/dirs map to their *_test
   namespaces."
  [root entries]
  (let
    [test-index
     (delay (all-test-files root))

     acc
     (reduce (fn [acc {:keys [path var]}]
               (let
                 [nses (when path
                         (let
                           [pf (io/file (str path))
                            f (if (.isAbsolute pf) pf (io/file root (str path)))]

                           (vec (path->nses f test-index))))]
                 (cond-> (update acc :nses into nses)
                   var
                   (update :vars
                           into
                           (if (seq nses)
                             (map (fn [n]
                                    {:ns n :name var})
                                  nses)
                             [{:ns nil :name var}])))))
             {:nses [] :vars []}
             entries)]

    {:nses (vec (sort (distinct (:nses acc)))) :vars (vec (distinct (:vars acc)))}))

(def ^:private removed-selector-replacements
  "What a caller should say instead of each dead selector spelling. PATHS are the
   only way to name what runs, so a location key becomes a path and a NAME key
   becomes a node id inside that path."
  {:paths (str "name the FILE or DIRECTORY instead. {\"paths\": [\"test/a/core_test.clj\"]}"
               " runs the namespace that file declares, a SOURCE file runs its *-test"
               " namespace, and a directory runs every *_test.clj under it.")
   :var (str "put the test name IN the path as a node id instead."
             " {\"paths\": [\"test/a/core_test.clj::adds-test\"]} runs that one var,"
             " \"src/a/core.clj::adds\" runs the *-test var covering it, and"
             " \"::adds-test\" finds it wherever it lives.")})

(def ^:private removed-selector-keys
  "Selector spellings that no longer exist, in message order, each paired with the
   replacement it wants. A key that quietly stopped selecting would silently turn
   ONE namespace's run into the whole workspace's — or ONE var's into its whole
   namespace — so every dead spelling is refused BY NAME."
  [["ns" :paths] ["namespace" :paths] ["namespaces" :paths] ["path" :paths] ["only" :var]])

(defn- refuse-removed-selectors!
  "Throw when `arg` carries a removed selector key, naming what replaced it."
  [arg]
  (when-let
    [removed (seq (filter (fn [[k _]]
                            (contains? arg k))
                          removed-selector-keys))]
    (throw (ex-info (str "run_tests(clojure) no longer takes " (str/join " / " (map first removed))
                         " — " (str/join " "
                                         (distinct (map (comp removed-selector-replacements second)
                                                        removed))))
                    {:type :clj/bad-args :got arg}))))

(defn- normalize-arg
  "Coerce the raw run_tests arg (a path string or an opts dict) into the
   canonical selector map via the shared test-contract:
   `{:paths [{:path :var}] :include [str] :exclude [str]}`. The model arg is
   STRING-keyed (strings-only boundary); this is the external->internal seam that
   translates its `\"paths\"/\"include\"/\"exclude\"` keys into the keyword
   vocabulary `normalize-selectors` reads, splitting each path entry on its
   `::` (see `contract/split-node-id`).

   PATHS are the only way in — a bare string is ONE entry, and no second key
   names a namespace or a test to disagree with it. `clj-test-fn` resolves each
   path half to the test namespaces declared under it, so naming a SOURCE file
   runs its `*-test` namespace, and the `::name` half narrows to ONE var."
  [arg]
  (when (map? arg) (refuse-removed-selectors! arg))
  (contract/normalize-selectors
    (cond
      (string? arg) {:paths [arg]}
      (symbol? arg) {:paths [(str arg)]}
      (map? arg)
      {:paths (get arg "paths") :include (get arg "include") :exclude (get arg "exclude")}
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
            "run_tests(\"clojure\", {\"paths\": [\"test\"], \"exclude\": [\"slow\"]})"]})))))

(defn- ns->source-relpath
  [ns-str]
  (str (-> ns-str
           (str/replace "." "/")
           (str/replace "-" "_"))
       ".clj"))

(defn- test-file-for
  "Find a test source file for ns-str under root, even when the live nREPL was
   started without test paths on its classpath."
  [root ns-str]
  (let
    [rel
     (ns->source-relpath ns-str)

     root-file
     (io/file root)]

    (some (fn [^java.io.File f]
            (let [p (.getPath f)]
              (when (and (.isFile f)
                         (str/ends-with? p rel)
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
  (let
    [;; Cheap pre-flight: a single `describe` under a short timeout. A dead or
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
      (let
        [ns-files (test-files-for root ns-strs)
         code (build-eval-code ns-strs sel ns-files)
         ns-disp (str/join " " ns-strs)]

        (try
          (let
            [r
             ;; The run is parked outside the native tool wall, so THIS timeout is
             ;; the real budget — a slow / wedged nREPL surfaces as a real timeout
             ;; ERROR (with nREPL err/tail) instead of an opaque harness kill.
             (nrepl-client/eval!
               {:host "localhost" :port port :code code :timeout-ms default-test-timeout-ms})
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
               "error" (str "test run timed out after " default-test-timeout-ms
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

(defn- lazytest-cli?
  "True when root's deps.edn :test alias mains lazytest.main, so selector flags\n   appended to `clojure -M:test` reach lazytest's own CLI parser. Guards the\n   pass-through: only deps.edn projects whose :test alias actually runs\n   lazytest.main share the contract vocabulary."
  [root]
  (try (let [f (io/file root "deps.edn")]
         (when (.isFile f)
           (let
             [edn (edn/read-string (slurp f))
              main-opts (get-in edn [:aliases :test :main-opts])]

             (boolean (some (fn* [p1__44725#] (= "lazytest.main" p1__44725#)) main-opts)))))
       (catch Throwable _ false)))

(defn- cli-command-for
  "Pick the CLI test command for `root` by build file, so the fallback is not
   hardcoded to `clojure -M:test`. Returns {:tool kw :cmd [strings] :selectors? bool}
   or nil when no known Clojure build manifest is present:
     deps.edn    -> clojure -M:test  (selectors passed through to lazytest.main
                    when the :test alias actually mains lazytest.main)
     project.clj -> lein test        (whole suite; selectors do NOT apply)
     bb.edn      -> bb test          (whole suite; selectors do NOT apply)
   `sel` is the resolved selector map {:nses :vars :include :exclude}."
  [root sel]
  (let
    [present?
     (fn [n]
       (.isFile (io/file root n)))

     ;; A NESTED project whose deps.edn declares no :jvm-opts for :test inherits
     ;; the workspace's, passed as -J flags so the CLI suite runs with the same
     ;; JVM options as the managed nREPL (native-access / preview / unsafe-memory).
     jflags
     (mapv #(str "-J" %) (repl-manager/inherited-jvm-opts (io/file root) [:test]))]

    (cond (present? "deps.edn")
          (if (lazytest-cli? root)
            {:tool :clj
             :cmd (into (into ["clojure"] jflags) (into ["-M:test"] (lazytest-selector-args sel)))
             :selectors? true}
            {:tool :clj :cmd (into (into ["clojure"] jflags) ["-M:test"]) :selectors? false})
          (present? "project.clj") {:tool :lein :cmd ["lein" "test"] :selectors? false}
          (present? "bb.edn") {:tool :bb :cmd ["bb" "test"] :selectors? false}
          :else nil)))

(defn- run-via-cli
  "Fallback when no nREPL is reachable: shell the build-tool's test command. For a
   deps.edn project whose :test alias mains lazytest.main, the normalized selectors
   are PASSED THROUGH as lazytest CLI flags (-n/-v/-i/-e) so cli mode honors them
   just like the repl path; otherwise the whole suite runs.
   The full shell command lives on :command, and the runner's own summary line is
   read into the COUNTS every mode shares — :total, :fail and its erroring subset
   :errored — instead of being retold as a sentence the caller has to parse.
   `norm` is the resolved selector map {:nses :vars :include :exclude}."
  [root norm]
  (let
    [ns-str
     (str/join " " (:nses norm))

     sel
     (select-keys norm [:nses :vars :include :exclude])]

    (if-let [{:keys [tool cmd]} (cli-command-for root sel)]
      (let
        [res (try (apply shell/sh (concat cmd [:dir (str root)]))
                  (catch Throwable t {:exit -1 :out "" :err (str (.getMessage t))}))
         out (str (:out res) (:err res))
         exit (long (or (:exit res) -1))
         ;; clojure.test and lazytest both close on "Ran N test…" plus an
         ;; "F failures, E errors." line — the only tally a shelled runner
         ;; offers, and the cli path reports it as the SAME counts the repl
         ;; path does.
         cases (some-> (re-find #"Ran (\d+) test" out)
                       second
                       parse-long)
         fails (some-> (re-find #"(\d+) failures?" out)
                       second
                       parse-long)
         errs (some-> (re-find #"(\d+) errors?" out)
                      second
                      parse-long)
         ;; A PASS demands a "Ran N test…" summary, not merely a 0 exit: a
         ;; deps.edn with no :test alias drops `clojure -M:test` into a bare
         ;; REPL that reads EOF and exits 0 having run ZERO tests. Counting
         ;; that as green silently hid whole suites (a real false green).
         ran? (some? cases)]

        ;; "is_pass" (exit-code verdict) is a DISTINCT key from the repl path's
        ;; "pass" (a count) — render-test-result reads both.
        (cond->
          {"mode" "cli"
           "ns" ns-str
           "tool" (name tool)
           "command" (str/join " " cmd)
           "exit" exit
           "is_pass" (and (zero? exit) ran?)
           "output" (cli-tail out)}
          cases
          (assoc "total" cases)

          (or fails errs)
          (assoc "fail" (+ (long (or fails 0)) (long (or errs 0))))

          errs
          (assoc "errored" errs)

          (and (zero? exit) (not ran?))
          (assoc "error"
            (str "test command exited 0 but printed no \"Ran N test…\" summary"
                 " — no tests actually ran (often a missing/misconfigured "
                 (name tool)
                 " :test alias, so `"
                 (str/join " " cmd)
                 "` fell"
                 " through to a bare REPL). Reported as NOT passing to avoid a"
                 " false green."))))
      {"mode" "cli"
       "ns" ns-str
       "error" (str "no nREPL reachable, and no deps.edn / project.clj / bb.edn in "
                    root
                    " to run tests via CLI")})))

(defn- relaunch-repl-async!
  "Best-effort background stop + fresh start of `session-id`'s managed nREPL for `cwd`
   on a daemon thread, so the NEXT eval/test hits a fresh server. Returns at once —
   the relaunch (deps resolve + JVM boot, up to ~2 min) never blocks the caller."
  [session-id dir]
  (when (and session-id dir)
    (doto (Thread. ^Runnable
                   (fn []
                     (try (repl-manager/stop! session-id dir)
                          (repl-manager/start! session-id dir nil)
                          (catch Throwable _ nil))))
      (.setDaemon true)
      (.setName "vis-clj-repl-recover")
      (.start)))
  nil)

(defn- recover-if-unusable
  "Auto-recovery seam for run_tests. When run-via-repl reports the nREPL was UNUSABLE
   for this run — down / gone mid-run (\"repl_unusable\") or wedged past the timeout
   (\"repl_wedged\") — don't just hand back a 'start a fresh REPL and retry' error and
   burn the turn. Stop and relaunch the nREPL in the BACKGROUND, and for an unusable (not
   merely wedged) server ALSO run the suite via the build-tool CLI so the caller still
   gets REAL results THIS turn. A wedged eval is left CLI-less: its hang is likely the
   code under test, which a CLI run would only re-hang on. The recovery is announced
   on :note so the outcome is self-explaining."
  [session-id root norm result]
  (cond (get result "repl_unusable")
        (do (relaunch-repl-async! session-id root)
            (let
              [cli
               (run-via-cli root norm)

               why
               (get result "error")

               note
               (str "nREPL was unusable"
                    (when why (str " (" why ")"))
                    " — ran the suite via CLI and relaunched a fresh nREPL in the background.")]

              (-> cli
                  (assoc "recovered" true)
                  (update "note"
                          (fn [n]
                            (if (seq (str n)) (str note " " n) note))))))
        (get result "repl_wedged")
        (do (relaunch-repl-async! session-id root)
            (update result
                    "error"
                    (fn [e]
                      (str e " Relaunching a fresh nREPL in the background."))))
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

(defn clj-test-fn
  "Run clojure tests. The arg names PATHS — files, directories, or
   `<path>::<test-name>` node ids — and nothing else: this is where a path
   becomes a test namespace and a name becomes ONE var. A *_test.clj file is read
   for the ns it declares, a SOURCE file maps to its `*-test` ns when that test
   file exists, and a directory is walked for both; the `::name` half then keeps
   just that var, matching the test var itself (`adds-test`) or the SOURCE var it
   covers (`adds`), and `::name` with no path finds it wherever it lives. When NO
   LOCATION is requested the whole workspace is scanned for *_test.clj and every
   test namespace runs — empty selectors mean 'run everything', not 'run
   nothing'. The one case that still errors is explicit-but-empty: a path was
   given yet no *_test.clj was found under it (a real 'nothing to run there', not
   a 'run all' intent), and likewise a `::name` that matched no var.
   The nREPL boots at the tests' OWN project root — the nearest deps.edn /
   project.clj / bb.edn at or below the workspace root — so a NESTED project runs
   against its own build file instead of the workspace root's classpath.
   The result :mode says which path ran; :language is always clojure so the result is self-describing
   across the language / framework / tool / mode axes."
  ([env arg]
   (let
     [;; An explicit `cwd` (the run_tests `cwd` param) roots the run — and thus
      ;; nREPL selection — at THAT project instead of the workspace root, so a
      ;; SIBLING / added-folder project runs against its OWN nREPL classpath
      ;; rather than booting the workspace-root REPL (whose classpath lacks it).
      req-dir
      (when (map? arg) (get arg "cwd"))

      root
      (let
        [wsroot (or (:workspace/root env)
                    (throw (ex-info "run_tests(clojure) fired without :workspace/root in env"
                                    {:type :clj/no-workspace})))]
        (if (str/blank? (str req-dir))
          wsroot
          (let [f (io/file (str req-dir))]
            (.getPath (if (.isAbsolute f) f (io/file wsroot (str req-dir)))))))

      {:keys [paths] :as norm}
      (normalize-arg arg)

      ;; Locations the caller EXPLICITLY asked for — used ONLY to find the tests'
      ;; own project root. Empty for a bare "run everything" call (and for a
      ;; pathless `::name` id), which stays rooted at the workspace so it never
      ;; file-seqs per namespace.
      req-locations
      (keep (fn [{:keys [path]}]
              (when path
                (let [pf (io/file (str path))]
                  (if (.isAbsolute pf) pf (io/file root (str path))))))
            paths)

      ;; The ONE translation: requested entries -> the test namespaces declared
      ;; under them (:nses) plus the var filter their `::name` halves name
      ;; (:vars). No LOCATION at all = "run everything", so every *_test ns in
      ;; the workspace runs and a bare `::name` narrows inside it; a path that
      ;; resolves to nothing is explicit-but-empty and stays an error below. An
      ;; empty list [] counts as "not given" (empty? is total on nil), so [] and
      ;; nil behave identically here.
      {:keys [nses] :as norm}
      (let [{:keys [nses vars]} (resolve-selection root paths)]
        (assoc norm
          :vars vars
          :nses (if (some :path paths) nses (sort (keys (all-test-files root))))))

      sel
      (select-keys norm [:vars :include :exclude])

      ;; Boot the nREPL where the tests' OWN build file lives (nearest deps.edn /
      ;; project.clj / bb.edn at or below the workspace root), so a nested
      ;; project's deps.edn is honored. Falls back to the workspace root when the
      ;; request is at the top level or spans several projects.
      eff-root
      (if (seq req-locations) (.getPath (effective-test-root (io/file root) req-locations)) root)

      ;; Autostart / reuse THIS session's nREPL. `ensure-repl-for-dir!` already
      ;; verifies liveness (wait-until-up) and stops+replaces a dead/wedged process,
      ;; so a keyword-keyed result carrying :port is a VERIFIED-up server. When it
      ;; can't hand back a live port it returns start!'s STRING-keyed lifecycle map
      ;; ("no-launcher"/"failed"/"starting"…) instead — the two cases are gated apart
      ;; below so a boot failure is surfaced, not swallowed into a bare CLI fallback.
      repl
      (repl-manager/ensure-repl-for-dir! (:session-id env) eff-root)

      port
      (:port repl)]

     (when (empty? nses)
       (throw
         (ex-info
           (if (some :path paths)
             (str "run_tests(clojure) found no *_test.clj namespaces under "
                  (pr-str (vec (keep :path paths))))
             "run_tests(clojure) found no *_test.clj namespaces anywhere under the workspace root")
           {:type :clj/bad-args :got arg})))
     (let
       [result
        (cond
          ;; nREPL is up and verified — the fast inner loop.
          port (run-via-repl eff-root nses sel port)
          ;; No launchable Clojure build file at all → the CLI suite is the
          ;; correct path (it shells the build tool's own test command).
          (= "no-launcher" (get repl "result")) (run-via-cli eff-root norm)
          ;; A build file EXISTS but the nREPL did NOT come up ("failed" /
          ;; still "starting" / wedged past its grace window). Do NOT silently
          ;; CLI-fall-back onto a project whose REPL just crashed — surface the
          ;; launcher's own story (result + message + log tail) so the boot
          ;; failure IS the reported error instead of a confusing CLI miss.
          (map? repl) (cond->
                        {"mode" "repl"
                         "ns" (str/join " " nses)
                         "port" (get repl "port")
                         "error" (str "nREPL for "
                                      eff-root
                                      " is not running (status "
                                      (get repl "result" "unknown")
                                      ") — "
                                      (get repl "message" "the server failed to start")
                                      ". Fix the boot error (see log_tail) and retry.")}
                        (get repl "log_tail")
                        (assoc "log_tail" (get repl "log_tail")))
          ;; Defensive last resort (nil / unexpected shape): the CLI suite.
          :else (run-via-cli eff-root norm))

        result
        (recover-if-unusable (:session-id env) eff-root norm result)

        result'
        (if (and (get result "error")
                 (str/includes? (get result "error") "Could not locate lazytest/core"))
          (run-via-cli eff-root norm)
          result)

        ;; Directory-nested view of the fault maps — the same `by-cwd` grouping
        ;; lint/format expose, so a 30-failure run writes each path prefix ONCE.
        ;; Only present when there's something to group.
        result''
        (let [failures (get result' "failures")]
          (if (seq failures) (assoc result' "by-cwd" (group-faults-by-cwd failures)) result'))]

       (extension/success {:result (surface/check :test-fn
                                                  (assoc result'' "language" "clojure"))})))))
