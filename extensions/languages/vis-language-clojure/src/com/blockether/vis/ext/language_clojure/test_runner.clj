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

(def ^{:private true} run-form "Code evaled on the target nREPL: (require each ns :reload), SELECT tests by\n   the lazytest-modeled selector map {:only :include :exclude} at VAR\n   granularity, run them (clojure.test or lazytest, auto-detected per run), and\n   return the uniform result map. Self-contained - depends only on clojure.test\n   and lazytest being on the target classpath.\n\n   Selection (shared by both frameworks, mirrors lazytest precedence):\n     :only    keep only vars whose name is in this set\n     :include keep only vars carrying one of these metadata tags\n     :exclude drop vars carrying one of these tags (exclude OVERRIDES include)\n   A metadata TAG is a var-meta key whose value is literally true (so ^:slow /\n   ^:integration count; :test / :line / :lazytest/test do not).\n\n   The full run log (the framework's own printed report plus any error /\n   exception stacktraces written to *out* / *err*) is captured verbatim and\n   returned under :output, so the caller sees the errors and the logs, not just\n   the pass/fail counts. :errors is the erroring-test subset of :failures." (quote (fn [nsyms sel] (doseq [n nsyms] (require n :reload)) (let [only* (set (:only sel)) inc* (set (:include sel)) exc* (set (:exclude sel)) tags-of (fn [v] (->> (meta v) (keep (fn [kv] (when (true? (val kv)) (name (key kv))))) set)) vname-of (fn [v] (name (:name (meta v)))) keep? (fn [v] (let [tags (tags-of v) nm (vname-of v)] (cond (some exc* tags) false (and (seq only*) (not (only* nm))) false (and (seq inc*) (not (some inc* tags))) false :else true))) all-ct (mapcat (fn [n] (filter (fn [v] (:test (meta v))) (vals (ns-interns (the-ns n))))) nsyms) lt? (fn [v] (let [m (meta v)] (or (= :lazytest/var (:type m)) (contains? m :lazytest/test)))) all-lt (mapcat (fn [n] (filter lt? (vals (ns-interns (the-ns n))))) nsyms) out-writer (java.io.StringWriter.) result (binding [clojure.core/*out* out-writer clojure.core/*err* out-writer] (if (seq all-ct) (let [selected (vec (filter keep? all-ct)) skipped (- (count all-ct) (count selected)) fails (atom []) cnt (atom {:pass 0, :fail 0, :error 0})] (with-redefs [clojure.test/report (fn [m] (when (#{:fail :error :pass} (:type m)) (swap! cnt update (:type m) (fnil inc 0))) (when (#{:fail :error} (:type m)) (let [v0 (first clojure.test/*testing-vars*)] (swap! fails conj {:ns (str (:ns (meta v0))), :test (when v0 (str (:name (meta v0)))), :type (name (:type m)), :message (str (or (:message m) (:type m))), :expected (pr-str (:expected m)), :actual (pr-str (:actual m)), :file (str (:file m)), :line (:line m)}))))] (clojure.test/test-vars selected)) (let [c (clojure.core/deref cnt) fs (clojure.core/deref fails)] {:framework "clojure.test", :total (+ (:pass c) (:fail c) (:error c)), :pass (:pass c), :fail (+ (:fail c) (:error c)), :selected (count selected), :skipped skipped, :failures fs, :errors (vec (filter (fn [f] (= "error" (:type f))) fs))})) (let [selected (vec (filter keep? all-lt)) skipped (- (count all-lt) (count selected)) run-var (requiring-resolve (quote lazytest.runner/run-test-var)) rseq (requiring-resolve (quote lazytest.results/result-seq)) trees (mapv (fn [v] (run-var v)) selected) results (mapcat rseq trees) leaves (filter (fn [x] (#{:fail :error :pass} (:type x))) results) fails (filter (fn [x] (#{:fail :error} (:type x))) results) ->fail (fn [f] {:ns (str (:ns f)), :test (str (:doc f)), :type (name (:type f)), :message (let [m (:message f)] (cond (seq (str m)) (str m) (:thrown f) (str (.getMessage (:thrown f))) :else (str "expected " (pr-str (:expected f)) " actual " (pr-str (:actual f))))), :expected (pr-str (:expected f)), :actual (pr-str (:actual f)), :file (str (:file f)), :line (:line f)})] {:framework "lazytest", :total (count leaves), :pass (count (filter (fn [x] (= :pass (:type x))) results)), :fail (count fails), :selected (count selected), :skipped skipped, :failures (mapv ->fail fails), :errors (mapv ->fail (filter (fn [x] (= :error (:type x))) results))})))] (assoc result :output (clojure.core/str out-writer))))))

(defn build-eval-code "Self-contained Clojure source string that runs the tests for ns-strs (a vec of\n   namespace strings) with the selector map sel on the target nREPL, returning the\n   uniform result map (pr-str'd as the eval value)." [ns-strs sel] (str "(" (pr-str run-form) " (quote [" (str/join " " ns-strs) "]) " (pr-str sel) ")"))

(defn- strip-ansi
  "Strip ANSI escape sequences (colors / cursor controls) from a captured test
   run log, so channel previews (web + TUI) show plain text instead of raw
   `[32m`-style escape fragments. nil-safe."
  [s]
  (when s
    (str/replace s #"\u001b\[[0-9;]*[A-Za-z]" "")))

(defn- normalize-arg "Coerce the raw clj_test arg (namespace string / symbol / opts dict) into the\n   canonical selector map via the shared test-contract:\n   `{:nses [str] :only [str] :include [str] :exclude [str]}`." [arg] (contract/normalize-selectors (cond (string? arg) {:ns arg} (symbol? arg) {:ns (str arg)} (map? arg) arg :else (throw (ex-info "clj_test expects a namespace string or a dict with an ns key" {:type :clj/bad-args, :got arg, :examples ["clj_test(\"my.app.core-test\")" "clj_test({\"ns\": \"my.app.core-test\", \"only\": [\"adds\"]})" "clj_test({\"ns\": [\"a-test\", \"b-test\"], \"exclude\": [\"slow\"]})"]})))))

(defn- run-via-repl
  [ns-strs sel port]
  (let [code (build-eval-code ns-strs sel)
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

(defn clj-test-fn "Run tests for one OR many namespaces with the lazytest-modeled selectors\n   (only / include / exclude). Uses the live nREPL when a port is discoverable\n   (fast, framework auto-detected); otherwise falls back to the build-tool CLI.\n   For a deps.edn project whose :test alias mains lazytest.main, the selectors\n   are PASSED THROUGH to lazytest's CLI flags so cli mode honors them too;\n   otherwise the whole suite runs and a :note says so. The result :mode says\n   which path ran; :language is always \"clojure\" so the result is\n   self-describing across the language / framework / tool / mode axes." ([env arg] (let [root (or (:workspace/root env) (throw (ex-info "clj_test fired without :workspace/root in env" {:type :clj/no-workspace}))) {:keys [nses], :as norm} (normalize-arg arg) sel (select-keys norm [:only :include :exclude]) port (ports/find-default root)] (when (empty? nses) (throw (ex-info "clj_test needs a namespace, e.g. clj_test of my.app.core-test" {:type :clj/bad-args, :got arg}))) (extension/success {:result (assoc (if port (run-via-repl nses sel port) (run-via-cli root norm)) :language "clojure")}))))
