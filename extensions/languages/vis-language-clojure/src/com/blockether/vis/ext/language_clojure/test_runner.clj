(ns com.blockether.vis.ext.language-clojure.test-runner
  "Run a namespace's tests over the live nREPL (the fast inner loop) or, when no
   nREPL is reachable, by shelling clojure -M:test (the suite gate).

   The in-REPL path is FRAMEWORK-AGNOSTIC: a ns whose vars carry clojure.test
   :test metadata runs through clojure.test/run-tests; otherwise it is treated
   as lazytest and run through lazytest.runner/run-tests. Either way the result
   is a uniform map with :mode (repl or cli), :framework, :ns, :total, :pass,
   :fail and :failures [{:ns :test :message :file :line} ...].

   run-form is the code EVALED on the target nREPL. It is a quoted form (not a
   call into this namespace) so it works against ANY project's nREPL, including
   hosts that do not have the vis extension on their classpath."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.string :as str]
   [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
   [com.blockether.vis.ext.language-clojure.ports :as ports]
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
     (let [only* (set (:only sel))
           inc* (set (:include sel))
           exc* (set (:exclude sel))
           tags-of (fn [v]
                     (->> (meta v)
                          (keep (fn [[k v]]
                                  (when (true? v) (name k))))
                          set))
           vname-of (fn [v] (name (:name (meta v))))
           keep? (fn [v]
                   (let [tags (tags-of v)
                         nm (vname-of v)]
                     (cond
                       (some exc* tags) false
                       (and (seq only*) (not (only* nm))) false
                       (and (seq inc*) (not (some inc* tags))) false
                       :else true)))
           all-ct (mapcat (fn [n]
                            (filter (fn [v] (:test (meta v)))
                                    (vals (ns-interns (the-ns n)))))
                          nsyms)
           lt? (fn [v]
                 (let [m (meta v)]
                   (or (= :lazytest/var (:type m))
                       (contains? m :lazytest/test))))
           all-lt (mapcat (fn [n]
                            (filter lt? (vals (ns-interns (the-ns n)))))
                          nsyms)
           out-writer (java.io.StringWriter.)
           result (binding [clojure.core/*out* out-writer
                            clojure.core/*err* out-writer]
                    (if (seq all-ct)
                      (let [selected (vec (filter keep? all-ct))
                            skipped (- (count all-ct) (count selected))
                            fails (atom [])
                            cnt (atom {:pass 0 :fail 0 :error 0})]
                        (with-redefs [clojure.test/report
                                      (fn [m]
                                        (when (#{:fail :error :pass} (:type m))
                                          (swap! cnt update (:type m) (fnil inc 0)))
                                        (when (#{:fail :error} (:type m))
                                          (let [v0 (first clojure.test/*testing-vars*)]
                                            (swap! fails conj
                                                   {:ns (str (:ns (meta v0)))
                                                    :test (when v0 (str (:name (meta v0))))
                                                    :type (name (:type m))
                                                    :message (str (or (:message m) (:type m)))
                                                    :expected (pr-str (:expected m))
                                                    :actual (pr-str (:actual m))
                                                    :file (str (:file m))
                                                    :line (:line m)}))))]
                          (clojure.test/test-vars selected))
                        (let [c (clojure.core/deref cnt)
                              fs (clojure.core/deref fails)]
                          {:framework "clojure.test"
                           :total (+ (:pass c) (:fail c) (:error c))
                           :pass (:pass c)
                           :fail (+ (:fail c) (:error c))
                           :selected (count selected)
                           :skipped skipped
                           :failures fs
                           :errors (vec (filter (fn [f] (= "error" (:type f))) fs))}))
                      (let [selected (vec (filter keep? all-lt))
                            skipped (- (count all-lt) (count selected))
                            run-var (requiring-resolve (quote lazytest.runner/run-test-var))
                            rseq (requiring-resolve (quote lazytest.results/result-seq))
                            trees (mapv (fn [v] (run-var v)) selected)
                            results (mapcat rseq trees)
                            leaves (filter (fn [x] (#{:fail :error :pass} (:type x))) results)
                            fails (filter (fn [x] (#{:fail :error} (:type x))) results)
                            ->fail (fn [f]
                                     {:ns (str (:ns f))
                                      :test (str (:doc f))
                                      :type (name (:type f))
                                      :message (let [m (:message f)]
                                                 (cond
                                                   (seq (str m)) (str m)
                                                   (:thrown f) (str (.getMessage (:thrown f)))
                                                   :else (str "expected " (pr-str (:expected f))
                                                              " actual " (pr-str (:actual f)))))
                                      :expected (pr-str (:expected f))
                                      :actual (pr-str (:actual f))
                                      :file (str (:file f))
                                      :line (:line f)})]
                        {:framework "lazytest"
                         :total (count leaves)
                         :pass (count (filter (fn [x] (= :pass (:type x))) results))
                         :fail (count fails)
                         :selected (count selected)
                         :skipped skipped
                         :failures (mapv ->fail fails)
                         :errors (mapv ->fail (filter (fn [x] (= :error (:type x))) results))})))]
       (assoc result :output (clojure.core/str out-writer))))))

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
  ([ns-strs sel]
   (build-eval-code ns-strs sel {} {}))
  ([ns-strs sel ns-files]
   (build-eval-code ns-strs sel ns-files {}))
  ([ns-strs sel ns-files ns-deps]
   (binding [*print-length* nil
             *print-level* nil
             *print-namespace-maps* false
             *print-meta* false
             *print-dup* false]
     (str "(" (pr-str run-form) " (quote [" (str/join " " ns-strs) "]) "
          (pr-str sel) " " (pr-str ns-files) " " (pr-str ns-deps) ")"))))

(defn- strip-ansi
  "Strip ANSI escape sequences (colors / cursor controls) from a captured test
   run log, so channel previews (web + TUI) show plain text instead of raw
   `[32m`-style escape fragments. nil-safe."
  [s]
  (when s
    (str/replace s #"\u001b\[[0-9;]*[A-Za-z]" "")))

(defn- ns-of-file
  "Read the ns symbol declared in a Clojure (test) file as a string, or nil when
   the file has no parseable `(ns ...)` form."
  [^java.io.File f]
  (try
    (when-let [m (re-find #"\(ns\s+([A-Za-z0-9_.?!*+=<>$%&|-]+)" (slurp f))]
      (second m))
    (catch Throwable _ nil)))

(defn- ns-deps-of-file
  "Namespace strings DIRECTLY required by a test file's (ns ...) form (its
   `:require` / `:use` libspecs). These name the code under test that must be
   reloaded before the test so source edits are picked up on the live nREPL."
  [^java.io.File f]
  (try
    (let [ns-form (with-open [r (java.io.PushbackReader. (io/reader f))]
                    (binding [*read-eval* false]
                      (loop []
                        (let [form (read {:eof ::eof :read-cond :allow} r)]
                          (cond
                            (= form ::eof) nil
                            (and (seq? form) (= 'ns (first form))) form
                            :else (recur))))))
          spec->ns (fn [spec]
                     (cond
                       (symbol? spec) (str spec)
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
  (when ns-str
    (if (str/ends-with? ns-str "-test") ns-str (str ns-str "-test"))))

(defn- all-test-files
  "Index every *_test.clj under root by its declared ns string, built once per
   run so SOURCE paths can be resolved to their corresponding test namespace."
  [root]
  (into {}
        (keep (fn [^java.io.File f]
                (when (and (.isFile f)
                           (str/ends-with? (.getName f) "_test.clj"))
                  (when-let [ns (ns-of-file f)] [ns f]))))
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
    (cond
      (and (.isFile f) (str/ends-with? (.getName f) "_test.clj"))
      (keep identity [(ns-of-file f)])

      (and (.isFile f) (str/ends-with? (.getName f) ".clj"))
      (keep test-ns [(ns-of-file f)])

      (.isDirectory f)
      (let [test-files (filter (fn [^java.io.File x]
                                 (and (.isFile x)
                                      (str/ends-with? (.getName x) "_test.clj")))
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
                         f  (if (.isAbsolute pf) pf (io/file root (str p)))]
                     (path->nses f test-index))))
         distinct
         sort
         vec)))

(defn- normalize-arg
  "Coerce the raw clj_test arg (namespace string / symbol / opts dict) into the
   canonical selector map via the shared test-contract:
   `{:nses [str] :only [str] :include [str] :exclude [str]}`. A dict may carry
   :ns / :namespace / :namespaces (string or vector) OR :paths / :path (directories
   or files the caller has already resolved to namespaces upstream)."
  [arg]
  (contract/normalize-selectors
   (cond
     (string? arg) {:ns arg}
     (symbol? arg) {:ns (str arg)}
     (map? arg)    arg
     :else (throw (ex-info "clj_test expects a namespace string, or a dict with an :ns / :namespaces or :paths key"
                           {:type :clj/bad-args
                            :got arg
                            :examples ["clj_test(\"my.app.core-test\")"
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
  (let [rel (ns->source-relpath ns-str)
        root-file (io/file root)]
    (some (fn [^java.io.File f]
            (let [p (.getPath f)]
              (when (and (.isFile f)
                         (str/ends-with? p rel)
                         (str/includes? p (str java.io.File/separator "test" java.io.File/separator)))
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
  (let [src-rel-paths (->> (io/file root)
                           file-seq
                           (filter (fn [^java.io.File x]
                                     (and (.isFile x) (str/ends-with? (.getName x) ".clj"))))
                           (map (fn [^java.io.File x]
                                  (str/replace (.getPath x) java.io.File/separator "/")))
                           vec)
        project? (fn [dep]
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
  (let [ns-files (test-files-for root ns-strs)
        ns-deps (reload-plan root ns-strs ns-files)
        code (build-eval-code ns-strs sel ns-files ns-deps)
        ns-disp (str/join " " ns-strs)
        r (nrepl-client/eval! {:host "localhost" :port port :code code :timeout-ms 120000})
        parsed (try (edn/read-string (:value r)) (catch Throwable _ nil))]
    (if (map? parsed)
      (-> parsed
          (update :output strip-ansi)
          (assoc :mode "repl" :ns ns-disp :port port))
      {:mode "repl"
       :ns ns-disp
       :port port
       :error (str "could not parse test result"
                   (when (seq (str (:err r))) (str " - nREPL :err " (:err r))))
       :raw-value (:value r)})))

(defn- cli-tail
  "Last 40 lines of a CLI test run's combined out+err, ANSI-stripped so the
   stored :output renders clean in every channel."
  [^String s]
  (let [lines (str/split-lines (strip-ansi (or s "")))]
    (str/join "\n" (take-last 40 lines)))) (defn- lazytest-selector-args "Translate normalized selectors into lazytest.main CLI flags.\n   When :only is specified, uses --var for precise targeting (cross-product\n   of nses x only names); otherwise uses --namespace for ns-level filtering.\n   --include and --exclude are always passed when present." [{:keys [nses only include exclude]}] (vec (concat (if (seq only) (mapcat (fn [[ns vname]] [(str "--var") (str ns "/" vname)]) (for [ns nses vname only] [ns vname])) (mapcat (fn [ns] [(str "--namespace") ns]) nses)) (mapcat (fn [tag] [(str "--include") tag]) include) (mapcat (fn [tag] [(str "--exclude") tag]) exclude))))

(defn- lazytest-cli? "True when root's deps.edn :test alias mains lazytest.main, so selector flags\n   appended to `clojure -M:test` reach lazytest's own CLI parser. Guards the\n   pass-through: only deps.edn projects whose :test alias actually runs\n   lazytest.main share the contract vocabulary." [root] (try (let [f (io/file root "deps.edn")] (when (.isFile f) (let [edn (edn/read-string (slurp f)) main-opts (get-in edn [:aliases :test :main-opts])] (boolean (some (fn* [p1__44725#] (= "lazytest.main" p1__44725#)) main-opts))))) (catch Throwable _ false))) (defn- cli-command-for "Pick the CLI test command for `root` by build file, so the fallback is not\n   hardcoded to `clojure -M:test`. Returns {:tool kw :cmd [strings] :selectors? bool}\n   or nil when no known Clojure build manifest is present:\n     deps.edn    -> clojure -M:test  (selectors passed through to lazytest.main\n                    when the :test alias actually mains lazytest.main)\n     project.clj -> lein test        (whole suite; selectors do NOT apply)\n     bb.edn      -> bb test          (whole suite; selectors do NOT apply)\n   `sel` is the normalized selector map {:nses :only :include :exclude}." [root sel] (let [present? (fn [n] (.isFile (io/file root n)))] (cond (present? "deps.edn") (if (lazytest-cli? root) {:tool :clj, :cmd (into ["clojure" "-M:test"] (lazytest-selector-args sel)), :selectors? true} {:tool :clj, :cmd ["clojure" "-M:test"], :selectors? false}) (present? "project.clj") {:tool :lein, :cmd ["lein" "test"], :selectors? false} (present? "bb.edn") {:tool :bb, :cmd ["bb" "test"], :selectors? false} :else nil)))

(defn- run-via-cli "Fallback when no nREPL is reachable: shell the build-tool's test command. For a\n   deps.edn project whose :test alias mains lazytest.main, the normalized selectors\n   are PASSED THROUGH as lazytest CLI flags (-n/-v/-i/-e) so cli mode honors them\n   just like the repl path; otherwise the whole suite runs and a :note says so.\n   `norm` is the canonical selector map {:nses :only :include :exclude}." [root norm] (let [ns-str (str/join " " (:nses norm)) sel (select-keys norm [:nses :only :include :exclude]) has-sel? (boolean (some seq [(:only norm) (:include norm) (:exclude norm)]))] (if-let [{:keys [tool cmd selectors?]} (cli-command-for root sel)] (let [res (try (apply shell/sh (concat cmd [:dir (str root)])) (catch Throwable t {:exit -1, :out "", :err (str (.getMessage t))})) out (str (:out res) (:err res))] {:mode "cli", :ns ns-str, :tool (name tool), :command (str/join " " cmd), :exit (:exit res), :pass? (zero? (or (:exit res) -1)), :note (if selectors? (str "no nREPL reachable - ran via " (name tool) " (" (str/join " " cmd) "); selectors passed through to lazytest.main") (str "no nREPL reachable - ran the whole suite via " (name tool) " (" (str/join " " cmd) ")" (when has-sel? "; selectors do NOT apply to this build tool") "; start a REPL for a fast single-ns run")), :output (cli-tail out)}) {:mode "cli", :ns ns-str, :error (str "no nREPL reachable, and no deps.edn / project.clj / bb.edn in " root " to run tests via CLI")})))

(defn clj-test-fn
  "Run tests for one OR many namespaces with the lazytest-modeled selectors
   (only / include / exclude). Uses the live nREPL when a port is discoverable
   (fast, framework auto-detected); otherwise falls back to the build-tool CLI.
   If a dev nREPL lacks lazytest on its classpath, falls back to the project
   test CLI so the real deps.edn aliases/extension test paths stay on classpath.
   The arg may name namespaces directly (:ns) or point at directories/files via
   :paths / :path, which are walked for *_test.clj and resolved to namespaces.
   The result :mode says which path ran; :language is always clojure so the result is self-describing
   across the language / framework / tool / mode axes."
  ([env arg]
   (let [root (or (:workspace/root env)
                  (throw (ex-info "clj_test fired without :workspace/root in env"
                                  {:type :clj/no-workspace})))
         paths (when (map? arg) (or (:paths arg) (:path arg)))
         arg (if (and paths (not (or (:ns arg) (:namespace arg) (:namespaces arg))))
               (-> arg (dissoc :paths :path) (assoc :ns (paths->test-nses root paths)))
               arg)
         {:keys [nses] :as norm} (normalize-arg arg)
         sel (select-keys norm [:only :include :exclude])
         port (ports/find-default root)]
     (when (empty? nses)
       (throw (ex-info (if paths
                         (str "clj_test found no *_test.clj namespaces under " (pr-str (vec paths)))
                         "clj_test needs a namespace: a string like clj_test(\"my.app.core-test\"), or a dict {\"namespaces\": [\"a-test\" \"b-test\"]} / {\"ns\": \"my.app.core-test\"} / {\"paths\": [\"test\"]}")
                       {:type :clj/bad-args :got arg})))
     (let [result (if port
                    (run-via-repl root nses sel port)
                    (run-via-cli root norm))
           result' (if (and (:error result)
                            (str/includes? (:error result) "Could not locate lazytest/core"))
                     (run-via-cli root norm)
                     result)]
       (extension/success {:result (assoc result' :language "clojure")})))))
