(ns com.blockether.vis.ext.language-clojure.test-runner
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
            [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
            [com.blockether.vis.ext.language-clojure.repl-manager :as repl-manager]
            [com.blockether.vis.internal.test-contract :as contract]
            [com.blockether.vis.internal.foundation.surface-contract :as surface]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.runtime-settings :as rt]))

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

(defn- clj-file-name?
  "True when `s` names a Clojure FILE rather than a namespace — the one syntactic
   tell that separates `test/a/core_test.clj` from `a.core-test` when a path
   arrives under a namespace key."
  [s]
  (boolean (re-find #"\.clj[cs]?$" (str s))))

(defn- ns->test-nses
  "Resolve ONE namespace NAME to the test namespaces it selects: a test namespace
   is itself, a SOURCE namespace becomes its `*-test` namespace — the same
   translation a source PATH gets. A name the workspace index does not know is
   passed through unchanged, because that index sees only `*_test.clj` files
   under root: a namespace that lives elsewhere on the test classpath still runs,
   and a misspelled one fails loudly at require time instead of quietly selecting
   nothing."
  [ns-str test-index]
  (let [tn (source-ns->test-ns ns-str)]
    (cond (contains? @test-index ns-str) [ns-str]
          (contains? @test-index tn) [tn]
          :else [ns-str])))

(defn- resolve-ns-entry
  "Resolve ONE namespace-selector entry `{:ns :var}` into `{:nses :var}`. The
   entry names a namespace (`a.core-test`), a namespace and ONE var in the
   spelling `clojure -M:test --var` takes (`a.core-test/adds-test`), or a PATH
   that arrived under a namespace key — read as the path it obviously is, rather
   than as a namespace that could never load."
  [root {ns-str :ns var-name :var} test-index]
  (if (nil? ns-str)
    {:nses [] :var var-name}
    (let
      [pf
       (io/file (str ns-str))

       ^java.io.File f
       (if (.isAbsolute pf) pf (io/file root (str ns-str)))]

      (cond (or (clj-file-name? ns-str) (.exists f)) {:nses (vec (path->nses f test-index))
                                                      :var var-name}
            (and (nil? var-name) (str/includes? ns-str "/")) (let [[n v] (str/split ns-str #"/" 2)]
                                                               {:nses (ns->test-nses n test-index)
                                                                :var (not-empty v)})
            :else {:nses (ns->test-nses ns-str test-index) :var var-name}))))

(defn- resolve-selection
  "Resolve the CALL's selector entries into what a run needs: `:nses`, the test
   namespaces to load, `:vars`, the var filter to apply inside them
   (`{:ns <ns-or-nil> :name <test-name>}`), and `:files`, the test file each
   NAMESPACE entry resolved to — a path entry already names its own location,
   a namespace entry does not, and the run must still be rooted at the project
   the tests live in.
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
  [root path-entries ns-entries]
  (let
    [test-index
     (delay (all-test-files root))

     ;; One entry's resolved namespaces plus the name it narrows to. A name with
     ;; no namespace of its own stays `{:ns nil}` — 'wherever it lives'.
     add
     (fn [acc nses var]
       (cond-> (update acc :nses into nses)
         var
         (update :vars
                 into
                 (if (seq nses)
                   (map (fn [n]
                          {:ns n :name var})
                        nses)
                   [{:ns nil :name var}]))))

     acc
     (reduce (fn [acc {:keys [path var]}]
               (let
                 [nses (when path
                         (let
                           [pf (io/file (str path))
                            f (if (.isAbsolute pf) pf (io/file root (str path)))]

                           (vec (path->nses f test-index))))]
                 (add acc nses var)))
             {:nses [] :vars [] :files []}
             path-entries)

     acc
     (reduce (fn [acc entry]
               (let [{:keys [nses var]} (resolve-ns-entry root entry test-index)]
                 (-> (add acc nses var)
                     (update :files into (keep @test-index nses)))))
             acc
             ns-entries)]

    {:nses (vec (sort (distinct (:nses acc))))
     :vars (vec (distinct (:vars acc)))
     :files (vec (distinct (:files acc)))}))

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
  (let
    [{:keys [path var]}
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
  (vec (concat (for
                 [k
                  namespace-selector-keys

                  e
                  (contract/->str-vec (get arg k))]

                 (split-selector-entry :ns e))
               (for
                 [k
                  var-selector-keys

                  e
                  (contract/->str-vec (get arg k))]

                 (split-selector-entry :var e)))))

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
   and `path` is read alongside `paths`."
  [arg]
  (cond
    (or (string? arg) (symbol? arg)) (contract/normalize-selectors {:paths [(str arg)]})
    (map? arg) (assoc (contract/normalize-selectors {:paths (into
                                                              (contract/->str-vec (get arg "paths"))
                                                              (contract/->str-vec (get arg "path")))
                                                     :include (get arg "include")
                                                     :exclude (get arg "exclude")})
                 :ns-selectors (selector-entries arg))
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

(defn- recover-if-unusable
  "Recovery seam for run_tests: what happens when the REUSED nREPL lets a run down.
   \"repl_unusable\" (down / gone mid-run) reruns the suite through the build tool's
   CLI in a clean JVM, so the caller still gets REAL results THIS turn instead of a
   'start a fresh REPL and retry' error that burns the turn. \"repl_wedged\" (hung past
   the timeout) is left CLI-less: the hang is likely the code under test, which a CLI
   run would only re-hang on. Nothing here starts or relaunches a REPL — reviving one
   is the caller's own `repl` call. The outcome is announced on :note so the result
   explains itself."
  [root norm result]
  (cond (get result "repl_unusable")
        (let
          [cli
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
        (update
          result
          "error"
          (fn [e]
            (str e " Stop it (repl(\"clojure\", \"stop\")) — the next run then uses a clean JVM.")))
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
   `<path>::<test-name>` node ids: this is where a path becomes a test namespace
   and a name becomes ONE var. A *_test.clj file is read for the ns it declares,
   a SOURCE file maps to its `*-test` ns when that test file exists, and a
   directory is walked for both; the `::name` half then keeps just that var,
   matching the test var itself (`adds-test`) or the SOURCE var it covers
   (`adds`), and `::name` with no path finds it wherever it lives.
   A NAMESPACE may be named instead — `ns` / `nses` / `namespace` / `namespaces`,
   and `var` / `vars` / `only` for a test name — because that is the vocabulary
   `clojure -M:test` itself takes; each entry gets the same translation (a source
   ns resolves to its `*-test` ns) and roots the run at the project its test file
   lives in, so the spelling a model reaches for RUNS instead of being refused.
   When NO LOCATION is requested the whole workspace is scanned for *_test.clj
   and every test namespace runs — empty selectors mean 'run everything', not
   'run nothing'. The one case that still errors is explicit-but-empty: a
   location was given yet no *_test.clj was found under it (a real 'nothing to
   run there', not a 'run all' intent), and likewise a `::name` that matched no
   var.
   The run is rooted at the tests' OWN project — the nearest deps.edn / project.clj /
   bb.edn at or below the workspace root — so a NESTED project is tested against its
   own build file. run_tests NEVER starts a REPL: it reuses THIS session's REPL for
   that project when one is already up, and otherwise shells the project's own test
   command in a clean JVM.
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

      {:keys [paths ns-selectors] :as norm}
      (normalize-arg arg)

      ;; The ONE translation: requested entries -> the test namespaces declared
      ;; under them (:nses), the var filter their `::name` / `ns/var` halves name
      ;; (:vars), and the test FILE a namespace entry resolved to (:files), which
      ;; is the only location a `{"ns": ...}` call carries.
      resolved
      (resolve-selection root paths ns-selectors)

      ;; Locations the caller EXPLICITLY asked for — used ONLY to find the tests'
      ;; own project root. Empty for a bare "run everything" call (and for a
      ;; pathless `::name` id), which stays rooted at the workspace so it never
      ;; file-seqs per namespace.
      req-locations
      (into (vec (keep (fn [{:keys [path]}]
                         (when path
                           (let [pf (io/file (str path))]
                             (if (.isAbsolute pf) pf (io/file root (str path))))))
                       paths))
            (:files resolved))

      ;; No LOCATION at all = "run everything", so every *_test ns in the
      ;; workspace runs and a bare `::name` narrows inside it; a location that
      ;; resolves to nothing is explicit-but-empty and stays an error below. An
      ;; empty list [] counts as "not given" (empty? is total on nil), so [] and
      ;; nil behave identically here.
      {:keys [nses] :as norm}
      (assoc norm
        :vars (:vars resolved)
        :nses (if (or (some :path paths) (some :ns ns-selectors))
                (:nses resolved)
                (sort (keys (all-test-files root)))))

      sel
      (select-keys norm [:vars :include :exclude])

      ;; Root the run where the tests' OWN build file lives (nearest deps.edn /
      ;; project.clj / bb.edn at or below the workspace root), so a nested
      ;; project's deps.edn is honored. Falls back to the workspace root when the
      ;; request is at the top level or spans several projects.
      eff-root
      (if (seq req-locations) (.getPath (effective-test-root (io/file root) req-locations)) root)

      ;; REUSE, never spawn. `live-repl-for-dir` answers THIS session's REPL for the
      ;; project only while it ANSWERS, nil otherwise — run_tests starts nothing. With
      ;; no REPL up the suite runs in a clean JVM through the build tool's own test
      ;; command, which is also what a fresh session gets.
      port
      (:port (repl-manager/live-repl-for-dir (:session-id env) eff-root))]

     (when (empty? nses)
       (let [named (into (vec (keep :path paths)) (keep :ns ns-selectors))]
         (throw
           (ex-info
             (if (seq named)
               (str "run_tests(clojure) found no *_test.clj namespaces under " (pr-str named))
               "run_tests(clojure) found no *_test.clj namespaces anywhere under the workspace root")
             {:type :clj/bad-args :got arg}))))
     (let
       [result
        (if port
          ;; A REPL this session already keeps up for the project — the fast inner
          ;; loop. It reloads only the namespaces it RUNS, so production Vars the
          ;; caller edited stay as that REPL holds them (`repl_eval` `:reload`, or
          ;; stop the REPL and let the clean JVM run it).
          (run-via-repl eff-root nses sel port)
          ;; The default: the build tool's own test command, in a clean JVM.
          (run-via-cli eff-root norm))

        result
        (recover-if-unusable eff-root norm result)

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
