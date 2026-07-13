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
   hosts that do not have the vis extension on their classpath."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
            [com.blockether.vis.ext.language-clojure.repl-manager :as repl-manager]
            [com.blockether.vis.internal.test-contract :as contract]
            [com.blockether.vis.internal.extension :as extension]))

(def ^{:private true} run-form
  "Code evaled on the target nREPL. Loads each requested namespace, selecting
   tests by the lazytest-modeled selector map {:only :include :exclude} at VAR
   granularity. ns-files is an optional map from namespace string to absolute test file path. The map used when the live nREPL was started without test paths on its classpath. ns-deps is an optional map from namespace string to a vector of PROJECT namespace strings the test directly requires; each is reloaded before the test so source edits are picked up."
  (quote
    (fn [nsyms sel ns-files ns-deps]
      (doseq [n nsyms]
        (doseq [d (get ns-deps (str n))]
          (try (require (symbol d) :reload) (catch Throwable _ nil)))
        (if-let [path (get ns-files (str n))]
          (load-file path)
          (require n :reload)))
      (let [only*
            (set (:only sel))

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

            keep?
            (fn [v]
              (let [tags
                    (tags-of v)

                    nm
                    (vname-of v)]

                (cond (some exc* tags) false
                      (and (seq only*) (not (only* nm))) false
                      (and (seq inc*) (not (some inc* tags))) false
                      :else true)))

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
                                    (let [v0 (first clojure.test/*testing-vars*)]
                                      (swap! fails conj
                                        {"ns" (str (:ns (meta v0)))
                                         "test" (when v0 (str (:name (meta v0))))
                                         "type" (name (:type m))
                                         "message" (str (or (:message m) (:type m)))
                                         "expected" (pr-str (:expected m))
                                         "actual" (pr-str (:actual m))
                                         "file" (str (:file m))
                                         "line" (:line m)}))))]
                    (clojure.test/test-vars selected))
                  (let [c
                        (clojure.core/deref cnt)

                        fs
                        (clojure.core/deref fails)]

                    {"framework" "clojure.test"
                     "total" (+ (:pass c) (:fail c) (:error c))
                     "pass" (:pass c)
                     "fail" (+ (:fail c) (:error c))
                     "selected" (count selected)
                     "skipped" skipped
                     "failures" fs
                     "errors" (vec (filter (fn [f]
                                             (= "error" (get f "type")))
                                           fs))}))
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
                                     {})))

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
                   "selected" (count selected)
                   "skipped" skipped
                   "failures" (mapv ->fail fails)
                   "errors" (mapv ->fail
                                  (filter (fn [x]
                                            (= :error (:type x)))
                                          results))})))]

        (assoc result "output" (clojure.core/str out-writer))))))

(defn build-eval-code
  "Self-contained Clojure source string that runs tests for ns-strs with sel.
   ns-files optionally maps namespace strings to absolute .clj paths to load
   when the target nREPL does not have test paths on the classpath. ns-deps
   maps each namespace string to the vector of PROJECT namespaces it directly
   requires, reloaded before the test so source edits are picked up.

   The printer vars are pinned (no length/level/meta/dup limits) so the emitted
   code is always COMPLETE and readable — a caller runtime that caps
   *print-level* / *print-length* would otherwise render deep sub-forms of
   run-form as `#` / `...` and produce an unreadable, unbalanced string."
  ([ns-strs sel] (build-eval-code ns-strs sel {} {}))
  ([ns-strs sel ns-files] (build-eval-code ns-strs sel ns-files {}))
  ([ns-strs sel ns-files ns-deps]
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

     (str "("
          (pr-str run-form)
          " (quote ["
          (str/join " " ns-strs)
          "]) "
          (pr-str sel)
          " "
          (pr-str ns-files)
          " "
          (pr-str ns-deps)
          ")"))))

(defn- strip-ansi
  "Strip ANSI escape sequences (colors / cursor controls) from a captured test
   run log, so channel previews (web + TUI) show plain text instead of raw
   `[32m`-style escape fragments. nil-safe."
  [s]
  (when s (str/replace s #"\u001b\[[0-9;]*[A-Za-z]" "")))

(defn- ns-of-file
  "Read the ns symbol declared in a Clojure (test) file as a string, or nil when
   the file has no parseable `(ns ...)` form."
  [^java.io.File f]
  (try (when-let [m (re-find #"\(ns\s+([A-Za-z0-9_.?!*+=<>$%&|-]+)" (slurp f))]
         (second m))
       (catch Throwable _ nil)))

(defn- ns-deps-of-file
  "Namespace strings DIRECTLY required by a test file's (ns ...) form (its
   `:require` / `:use` libspecs). These name the code under test that must be
   reloaded before the test so source edits are picked up on the live nREPL."
  [^java.io.File f]
  (try (let [ns-form
             (with-open [r (java.io.PushbackReader. (io/reader f))]
               (binding [*read-eval* false]
                 (loop []

                   (let [form (read {:eof ::eof :read-cond :allow} r)]
                     (cond (= form ::eof) nil
                           (and (seq? form) (= 'ns (first form))) form
                           :else (recur))))))

             spec->ns
             (fn [spec]
               (cond (symbol? spec) (str spec)
                     (or (vector? spec) (seq? spec)) (when (symbol? (first spec))
                                                       (str (first spec)))
                     :else nil))]

         (->> (rest ns-form)
              (filter seq?)
              (filter #(#{:require :use} (first %)))
              (mapcat rest)
              (keep spec->ns)
              distinct
              vec))
       (catch Throwable _ [])))

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
   source ns to its existing *-test ns. `test-index` is {ns-str file}."
  [^java.io.File f test-index]
  (let [test-ns (fn [src-ns]
                  (let [tn (source-ns->test-ns src-ns)]
                    (when (contains? test-index tn) tn)))]
    (cond (and (.isFile f) (str/ends-with? (.getName f) "_test.clj")) (keep identity
                                                                            [(ns-of-file f)])
          (and (.isFile f) (str/ends-with? (.getName f) ".clj")) (keep test-ns [(ns-of-file f)])
          (.isDirectory f)
          (let [test-files (filter (fn [^java.io.File x]
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

(defn- paths->test-nses
  "Discover test namespaces for `paths` (each relative to root or absolute).
   Files AND directories are accepted, and SOURCE files/dirs are mapped to their
   corresponding *_test namespaces. Returns a distinct, sorted vec of ns strings."
  [root paths]
  (let [test-index (all-test-files root)]
    (->> paths
         (mapcat (fn [p]
                   (let [pf (io/file (str p))
                         f (if (.isAbsolute pf) pf (io/file root (str p)))]

                     (path->nses f test-index))))
         distinct
         sort
         vec)))

(defn- normalize-arg
  "Coerce the raw clj_test arg (namespace string / symbol / opts dict) into the
   canonical selector map via the shared test-contract:
   `{:nses [str] :only [str] :include [str] :exclude [str]}`. The model arg is
   STRING-keyed (strings-only boundary); this is the external->internal seam that
   translates its `\"ns\"/\"namespace\"/\"namespaces\"/\"only\"/\"include\"/\"exclude\"`
   keys into the keyword vocabulary `normalize-selectors` reads. A dict may carry
   ns keys OR `\"paths\"/\"path\"` (resolved to namespaces upstream in clj-test-fn)."
  [arg]
  (contract/normalize-selectors
    (cond
      (string? arg) {:ns arg}
      (symbol? arg) {:ns (str arg)}
      (map? arg) {:ns (or (get arg "ns") (get arg "namespace") (get arg "namespaces"))
                  :only (get arg "only")
                  :include (get arg "include")
                  :exclude (get arg "exclude")}
      :else
      (throw
        (ex-info
          "clj_test expects a namespace string, or a dict with an \"ns\" / \"namespaces\" or \"paths\" key"
          {:type :clj/bad-args
           :got arg
           :examples
           ["clj_test(\"my.app.core-test\")"
            "clj_test({\"ns\": \"my.app.core-test\", \"only\": [\"adds\"]})"
            "clj_test({\"namespaces\": [\"a-test\", \"b-test\"], \"exclude\": [\"slow\"]})"
            "clj_test({\"paths\": [\"test\", \"extensions/.../test\"]})"]})))))

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
  (let [rel
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

(defn- reload-plan
  "For each requested test ns, the PROJECT namespaces it directly requires — the
   code under test. run-form reloads these before the test so source edits made
   since the nREPL loaded them are picked up (a plain (require ns :reload) only
   reloads the test file itself, leaving its source deps stale). Library
   namespaces are excluded: a dep counts only when a matching .clj source file
   lives under root."
  [root ns-strs ns-files]
  (let [src-rel-paths
        (->> (io/file root)
             file-seq
             (filter (fn [^java.io.File x]
                       (and (.isFile x) (str/ends-with? (.getName x) ".clj"))))
             (map (fn [^java.io.File x]
                    (str/replace (.getPath x) java.io.File/separator "/")))
             vec)

        project?
        (fn [dep]
          (let [rel (str "/" (ns->source-relpath dep))]
            (boolean (some #(str/ends-with? % rel) src-rel-paths))))]

    (into {}
          (keep (fn [ns-str]
                  (when-let [tf (or (get ns-files ns-str) (test-file-for root ns-str))]
                    (let [deps (->> (ns-deps-of-file (io/file tf))
                                    (filter project?)
                                    (remove #(= % ns-str))
                                    distinct
                                    vec)]
                      (when (seq deps) [ns-str deps])))))
          ns-strs)))

(defn- run-via-repl
  [root ns-strs sel port]
  (let [ns-files
        (test-files-for root ns-strs)

        ns-deps
        (reload-plan root ns-strs ns-files)

        code
        (build-eval-code ns-strs sel ns-files ns-deps)

        ns-disp
        (str/join " " ns-strs)

        r
        ;; Keep BELOW the run_tests tool budget (~120s) so a slow / wedged nREPL
        ;; surfaces as a real timeout ERROR (with nREPL err/tail) instead of an
        ;; opaque harness kill. It must never exceed the caller's tool budget.
        (nrepl-client/eval! {:host "localhost" :port port :code code :timeout-ms 110000})

        parsed
        (try (edn/read-string (get r "value")) (catch Throwable _ nil))]

    (if (map? parsed)
      (-> parsed
          (update "output" strip-ansi)
          (assoc "mode" "repl"
                 "ns" ns-disp
                 "port" port))
      {"mode" "repl"
       "ns" ns-disp
       "port" port
       "error" (str "could not parse test result"
                    (when (seq (str (get r "err"))) (str " - nREPL err " (get r "err"))))
       "raw_value" (get r "value")})))

(defn- cli-tail
  "Last 40 lines of a CLI test run's combined out+err, ANSI-stripped so the
   stored :output renders clean in every channel."
  [^String s]
  (let [lines (str/split-lines (strip-ansi (or s "")))]
    (str/join "\n" (take-last 40 lines))))
(defn- lazytest-selector-args
  "Translate normalized selectors into lazytest.main CLI flags.\n   When :only is specified, uses --var for precise targeting (cross-product\n   of nses x only names); otherwise uses --namespace for ns-level filtering.\n   --include and --exclude are always passed when present."
  [{:keys [nses only include exclude]}]
  (vec
    (concat (if (seq only)
              (mapcat (fn [[ns vname]]
                        [(str "--var") (str ns "/" vname)])
                      (for [ns
                            nses

                            vname
                            only]

                        [ns vname]))
              (mapcat (fn [ns]
                        [(str "--namespace") ns])
                      nses))
            (mapcat (fn [tag]
                      [(str "--include") tag])
                    include)
            (mapcat (fn [tag]
                      [(str "--exclude") tag])
                    exclude))))

(defn- lazytest-cli?
  "True when root's deps.edn :test alias mains lazytest.main, so selector flags\n   appended to `clojure -M:test` reach lazytest's own CLI parser. Guards the\n   pass-through: only deps.edn projects whose :test alias actually runs\n   lazytest.main share the contract vocabulary."
  [root]
  (try (let [f (io/file root "deps.edn")]
         (when (.isFile f)
           (let [edn (edn/read-string (slurp f))
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
   `sel` is the normalized selector map {:nses :only :include :exclude}."
  [root sel]
  (let [present?
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
   just like the repl path; otherwise the whole suite runs and a :note says so.
   The full shell command lives on :command; the :note stays a short human
   sentence so the RUN_TESTS headline doesn't re-list every namespace.
   `norm` is the canonical selector map {:nses :only :include :exclude}."
  [root norm]
  (let [ns-str
        (str/join " " (:nses norm))

        sel
        (select-keys norm [:nses :only :include :exclude])]

    (if-let [{:keys [tool cmd]} (cli-command-for root sel)]
      (let [res (try (apply shell/sh (concat cmd [:dir (str root)]))
                     (catch Throwable t {:exit -1 :out "" :err (str (.getMessage t))}))
            out (str (:out res) (:err res))
            cases (some-> (re-find #"Ran (\d+) test" out)
                          second)
            fails (some-> (re-find #"(\d+) failures?" out)
                          second)
            tally (when cases (str cases " cases" (when fails (str ", " fails " failures"))))]

        ;; "pass?" (exit-code verdict) is a DISTINCT key from the repl path's
        ;; "pass" (a count) — render-test-result reads both; keep the "?" so they
        ;; never collide.
        {"mode" "cli"
         "ns" ns-str
         "tool" (name tool)
         "command" (str/join " " cmd)
         "exit" (:exit res)
         "pass?" (zero? (or (:exit res) -1))
         ;; Surface just the RESULT — the tally is all the caller needs. The
         ;; runner mechanics (which build tool, live nREPL vs CLI, selector
         ;; pass-through) are internal plumbing, not something to narrate.
         "note" tally
         "output" (cli-tail out)})
      {"mode" "cli"
       "ns" ns-str
       "error" (str "no nREPL reachable, and no deps.edn / project.clj / bb.edn in "
                    root
                    " to run tests via CLI")})))

(defn- has-build-file?
  "True when `dir` holds a Clojure build manifest (deps.edn / project.clj / bb.edn)."
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
  "Run clojure tests. The arg may name namespaces directly (:ns) or point at
   directories/files via :paths / :path, which are walked for *_test.clj and
   resolved to namespaces. When NOTHING is requested (no :ns, no :paths) the
   whole workspace is scanned for *_test.clj and every test namespace runs —
   empty selectors mean 'run everything', not 'run nothing'. The one case that
   still errors is explicit-but-empty: a :paths was given yet no *_test.clj was
   found under them (a real 'nothing to run there', not a 'run all' intent).
   The nREPL boots at the tests' OWN project root — the nearest deps.edn /
   project.clj / bb.edn at or below the workspace root — so a NESTED project runs
   against its own build file instead of the workspace root's classpath.
   The result :mode says which path ran; :language is always clojure so the result is self-describing
   across the language / framework / tool / mode axes."
  ([env arg]
   (let [root
         (or (:workspace/root env)
             (throw (ex-info "clj_test fired without :workspace/root in env"
                             {:type :clj/no-workspace})))

         paths
         (when (map? arg) (or (get arg "paths") (get arg "path")))

         arg
         (if (and paths (not (or (get arg "ns") (get arg "namespace") (get arg "namespaces"))))
           (-> arg
               (dissoc "paths" "path")
               (assoc "ns" (paths->test-nses root paths)))
           arg)

         {:keys [nses] :as norm}
         (normalize-arg arg)

         ;; Locations the caller EXPLICITLY asked for (requested paths, else the
         ;; named test namespaces resolved to their files) — used ONLY to find the
         ;; tests' own project root. Empty for a bare "run everything" call, which
         ;; stays rooted at the workspace so it never file-seqs per namespace.
         req-locations
         (cond (seq paths) (map (fn [p]
                                  (let [pf (io/file (str p))]
                                    (if (.isAbsolute pf) pf (io/file root (str p)))))
                                paths)
               (seq nses) (keep #(some-> (test-file-for root %)
                                         io/file)
                                nses)
               :else nil)

         ;; Empty selectors = "run everything": when the caller named nothing
         ;; (no ns AND no paths), default to every *_test namespace under the
         ;; yielded no tests — that's an explicit-but-empty request and stays
         ;; an error below. An explicit empty list [] counts as "not given"
         ;; (empty? is total on nil), so [] and nil behave identically here.
         {:keys [nses] :as norm}
         (if (and (empty? nses) (empty? paths))
           (assoc norm :nses (sort (keys (all-test-files root))))
           norm)

         sel
         (select-keys norm [:only :include :exclude])

         ;; Boot the nREPL where the tests' OWN build file lives (nearest deps.edn /
         ;; project.clj / bb.edn at or below the workspace root), so a nested
         ;; project's deps.edn is honored. Falls back to the workspace root when the
         ;; request is at the top level or spans several projects.
         eff-root
         (if (seq req-locations) (.getPath (effective-test-root (io/file root) req-locations)) root)

         ;; Autostart (or reuse) THIS session's nREPL for the tests' project root —
         ;; the fast inner loop. nil only when there's no launchable build file, then
         ;; we fall back to the CLI suite gate below.
         port
         (some-> (repl-manager/ensure-repl-for-dir! (:session-id env) eff-root)
                 :port)]

     (when (empty? nses)
       (throw (ex-info
                (if paths
                  (str "clj_test found no *_test.clj namespaces under " (pr-str (vec paths)))
                  "clj_test found no *_test.clj namespaces anywhere under the workspace root")
                {:type :clj/bad-args :got arg})))
     (let [result
           (if port (run-via-repl eff-root nses sel port) (run-via-cli eff-root norm))

           result'
           (if (and (get result "error")
                    (str/includes? (get result "error") "Could not locate lazytest/core"))
             (run-via-cli eff-root norm)
             result)]

       (extension/success {:result (assoc result' "language" "clojure")})))))
