(ns com.blockether.vis.internal.python-extensions-test
  "Python extension host — load fixture `.py` files into trusted GraalPy
   contexts and assert on the registry + adapter contracts. Boots real
   GraalPy contexts (on the shared engine), no model in the loop."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.python-extensions :as pyx]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   [java.nio.file Files]
   [java.nio.file.attribute FileAttribute]))

;; =============================================================================
;; Harness
;; =============================================================================

(defn- temp-dir ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-pyext-test" (make-array FileAttribute 0))))

(defn- write-ext! [^java.io.File dir fname source]
  (let [f (io/file dir fname)]
    (spit f source)
    f))

(defn- with-loaded
  "Load `.py` sources (map of filename -> source) from a temp dir with
   state confined to a sibling temp dir, run `f` with the load result,
   then tear everything down so tests stay isolated."
  [sources f]
  (let [ext-dir (temp-dir)
        state-dir (temp-dir)]
    (doseq [[fname src] sources] (write-ext! ext-dir fname src))
    (binding [pyx/*state-dir* (str state-dir)]
      (try
        (f (pyx/reload-python-extensions! {:dirs [(str ext-dir)]})
          {:ext-dir ext-dir :state-dir state-dir})
        (finally
          (pyx/reload-python-extensions! {:dirs []}))))))

(defn- registered [ext-name]
  (some #(when (= ext-name (:ext/name %)) %) (extension/registered-extensions)))

(defn- symbol-fn [ext sym]
  (some #(when (= sym (:ext.symbol/symbol %)) (:ext.symbol/fn %))
    (get-in ext [:ext/engine :ext.engine/symbols])))

(def ^:private counter-py
  "\"\"\"Counter fixture: tools + state + slash + prompt.\"\"\"
import vis


def counter_bump(by):
    \"\"\"await counter_bump(by) -> {\\\"count\\\"} — bump the counter.\"\"\"
    n = vis.state.get(\"count\", 0) + by
    vis.state[\"count\"] = n
    return {\"count\": n}


def counter_read():
    \"\"\"await counter_read() -> {\\\"count\\\"} — read the counter.\"\"\"
    return {\"count\": vis.state.get(\"count\", 0)}


def counter_boom():
    \"\"\"await counter_boom() -> never — always raises.\"\"\"
    raise ValueError(\"kaboom\")


def _slash(ctx):
    return vis.ok(\"count is \" + str(vis.state.get(\"count\", 0)), data={\"args\": ctx[\"args\"]})


vis.extension(
    name=\"counter\",
    description=\"Counter fixture extension.\",
    version=\"0.1.0\",
    kind=\"integration\",
    alias=\"counter\",
    symbols=[
        vis.symbol(counter_bump, tag=\"mutation\"),
        vis.symbol(counter_read, tag=\"observation\"),
        vis.symbol(counter_boom, tag=\"observation\"),
    ],
    prompt=\"counter_ surface active.\",
    slash_commands=[vis.slash(\"count\", _slash, doc=\"Show the counter.\")],
)
")

;; =============================================================================
;; Loading + registry
;; =============================================================================

(defdescribe load-and-register-test
  (it "loads a file, registers the extension, and strips the alias prefix from symbol names"
    (with-loaded {"counter.py" counter-py}
      (fn [result _]
        (expect (= {:loaded 1 :failed 0 :changed? true} result))
        (let [ext (registered "counter")]
          (expect (some? ext))
          (expect (= 'counter (get-in ext [:ext/engine :ext.engine/alias])))
          (expect (= '[bump read boom]
                    (mapv :ext.symbol/symbol
                      (get-in ext [:ext/engine :ext.engine/symbols]))))
          ;; docstring became the model-facing doc; arglists carry the
          ;; real Python parameter names
          (let [bump (first (get-in ext [:ext/engine :ext.engine/symbols]))]
            (expect (str/includes? (:ext.symbol/doc bump) "bump the counter"))
            (expect (= ['[by]] (:ext.symbol/arglists bump)))
            (expect (= :mutation (:ext.symbol/tag bump))))))))

  (it "is idempotent: an unchanged scan is a no-op"
    (with-loaded {"counter.py" counter-py}
      (fn [_ {:keys [ext-dir]}]
        (let [again (pyx/load-python-extensions! {:dirs [(str ext-dir)]})]
          (expect (= false (:changed? again)))
          (expect (= 1 (:loaded again))))))))

;; =============================================================================
;; Tool adapter — envelope semantics
;; =============================================================================

(defdescribe tool-envelope-test
  (it "return value = success payload"
    (with-loaded {"counter.py" counter-py}
      (fn [_ _]
        (let [bump (symbol-fn (registered "counter") 'bump)
              result (bump 5)]
          (expect (extension/envelope-success? result))
          (expect (= 5 (get-in result [:result "count"])))))))

  (it "a raised Python exception = failure envelope with the Python message"
    (with-loaded {"counter.py" counter-py}
      (fn [_ _]
        (let [boom (symbol-fn (registered "counter") 'boom)
              result (boom)]
          (expect (extension/envelope-failure? result))
          (expect (str/includes? (get-in result [:error :message]) "kaboom")))))))

;; =============================================================================
;; State — durable across reloads
;; =============================================================================

(defdescribe state-durability-test
  (it "vis.state survives a full reload (fresh contexts, same EDN file)"
    (with-loaded {"counter.py" counter-py}
      (fn [_ {:keys [ext-dir]}]
        (let [bump (symbol-fn (registered "counter") 'bump)]
          (expect (= 7 (get-in (bump 7) [:result "count"])))
          ;; full teardown + fresh contexts
          (pyx/reload-python-extensions! {:dirs [(str ext-dir)]})
          (let [read (symbol-fn (registered "counter") 'read)]
            (expect (= 7 (get-in (read) [:result "count"])))))))))

;; =============================================================================
;; Prompt + slash
;; =============================================================================

(defdescribe prompt-and-slash-test
  (it "a string prompt normalizes into :ext/prompt-fn"
    (with-loaded {"counter.py" counter-py}
      (fn [_ _]
        (expect (= "counter_ surface active."
                  ((:ext/prompt-fn (registered "counter")) {}))))))

  (it "vis.slash run-fns receive the slim ctx and map vis.ok to :slash/*"
    (with-loaded {"counter.py" counter-py}
      (fn [_ _]
        (let [spec (first (:ext/slash-commands (registered "counter")))
              res ((:slash/run-fn spec) {:channel/id :tui
                                         :command/argv ["a" "b"]
                                         :command/raw "/count a b"})]
          (expect (= "count" (:slash/name spec)))
          (expect (= :ok (:slash/status res)))
          (expect (str/includes? (:slash/title res) "count is"))
          ;; :slash/data holds the Python-crossed dict — STRING keys
          (expect (= ["a" "b"] (get-in res [:slash/data "args"]))))))))

;; =============================================================================
;; Dynamic prompt + activation callables
;; =============================================================================

(def ^:private moods-py
  "\"\"\"Dynamic prompt/activation fixture.\"\"\"
import vis


def _prompt(env):
    return \"MOOD ON\" if vis.state.get(\"mood\", False) else None


def _active(env):
    return env[\"cwd\"] is not None


def _toggle(ctx):
    vis.state[\"mood\"] = not vis.state.get(\"mood\", False)
    return vis.ok(\"toggled\")


vis.extension(
    name=\"moods\",
    description=\"Dynamic prompt fixture.\",
    kind=\"fun\",
    activation=_active,
    prompt=_prompt,
    slash_commands=[vis.slash(\"mood\", _toggle, doc=\"Toggle mood.\")],
)
")

(defdescribe dynamic-callables-test
  (it "prompt callables are re-evaluated and may return None (no fragment)"
    (with-loaded {"moods.py" moods-py}
      (fn [_ _]
        (let [ext (registered "moods")]
          (expect (nil? ((:ext/prompt-fn ext) {})))
          ((:slash/run-fn (first (:ext/slash-commands ext)))
           {:channel/id :tui :command/argv [] :command/raw "/mood"})
          (expect (= "MOOD ON" ((:ext/prompt-fn ext) {})))))))

  (it "activation callables gate the extension per env"
    (with-loaded {"moods.py" moods-py}
      (fn [_ _]
        (expect (true? ((:ext/activation-fn (registered "moods")) {})))))))

;; =============================================================================
;; Op hooks — before(=guard) blocks, after observes
;; =============================================================================

(def ^:private guard-py
  "\"\"\"Guard fixture.\"\"\"
import vis


def _guard(call):
    for s in vis.strings_of(call[\"args\"]):
        if \".env\" in s:
            return vis.block(\"protected: \" + s)
    return None


vis.extension(
    name=\"guard\",
    description=\"Guard fixture extension.\",
    kind=\"guard\",
    op_hooks=[vis.op_hook([\"write\", \"patch\"], _guard, phase=\"before\")],
)
")

(defdescribe op-hook-test
  (it "'before' hooks compile to :around guards that can block with a failure envelope"
    (with-loaded {"guard.py" guard-py}
      (fn [_ _]
        (let [hooks (:ext/op-hooks (registered "guard"))
              write-hook (some #(when (= :write (:op %)) %) hooks)]
          (expect (= #{:write :patch} (set (map :op hooks))))
          (expect (every? #(= :around (:phase %)) hooks))
          ;; blocked: guard returns vis.block -> failure envelope, next never runs
          (let [ran? (atom false)
                res ((:fn write-hook) {} :write ["/x/.env" "data"]
                                      (fn [_] (reset! ran? true) :ran))]
            (expect (extension/envelope-failure? res))
            (expect (str/includes? (get-in res [:error :message]) "protected"))
            (expect (false? @ran?)))
          ;; allowed: guard returns None -> next runs with original args
          (expect (= :ran ((:fn write-hook) {} :write ["/x/ok.txt" "data"] (fn [_] :ran)))))))))

;; =============================================================================
;; Failure containment
;; =============================================================================

(defdescribe load-failure-test
  (it "a broken file is a recorded load failure, never a crash"
    (with-loaded {"broken.py" "import vis\nraise RuntimeError('nope at import')\n"}
      (fn [result _]
        (expect (= 0 (:loaded result)))
        (expect (= 1 (:failed result)))
        (expect (str/includes? (:error (first (pyx/load-failures))) "nope at import")))))

  (it "a file that never calls vis.extension() is a load failure"
    (with-loaded {"empty.py" "x = 1\n"}
      (fn [result _]
        (expect (= 1 (:failed result)))
        (expect (str/includes? (:error (first (pyx/load-failures)))
                  "never called vis.extension")))))

  (it "a tool without a docstring is rejected with a clear message"
    (with-loaded {"nodoc.py" (str "import vis\n"
                               "def nodoc_x():\n    return 1\n"
                               "vis.extension(name='nodoc', description='d', alias='nodoc',\n"
                               "              kind='x', symbols=[vis.symbol(nodoc_x)])\n")}
      (fn [result _]
        (expect (= 1 (:failed result)))
        (expect (str/includes? (:error (first (pyx/load-failures))) "docstring"))))))

;; =============================================================================
;; Reload + project-over-global precedence
;; =============================================================================

(defdescribe reload-test
  (it "editing a file and reloading swaps the registration"
    (with-loaded {"counter.py" counter-py}
      (fn [_ {:keys [ext-dir]}]
        (write-ext! ext-dir "counter.py"
          (str/replace counter-py "Counter fixture extension." "Counter v2."))
        (pyx/load-python-extensions! {:dirs [(str ext-dir)]})
        (expect (= "Counter v2." (:ext/description (registered "counter")))))))

  (it "change listeners see every (re)load and removal"
    (let [events (atom [])]
      (pyx/add-change-listener! ::test #(swap! events conj %))
      (try
        (with-loaded {"counter.py" counter-py}
          (fn [_ {:keys [ext-dir]}]
            ;; initial load: counter registered, nothing removed
            (let [{:keys [extensions removed]} (last @events)]
              (expect (= ["counter"] (mapv :ext/name extensions)))
              (expect (= [] removed)))
            ;; edit + reload: fresh registration, still nothing removed
            (write-ext! ext-dir "counter.py"
              (str/replace counter-py "Counter fixture extension." "Counter v2."))
            (pyx/load-python-extensions! {:dirs [(str ext-dir)]})
            (let [{:keys [extensions removed]} (last @events)]
              (expect (= "Counter v2." (:ext/description (first extensions))))
              (expect (= [] removed)))))
        ;; with-loaded's teardown scanned an empty dir set -> counter removed
        (let [{:keys [extensions removed]} (last @events)]
          (expect (= [] extensions))
          (expect (= ["counter"] removed)))
        (finally
          (pyx/remove-change-listener! ::test)))))

  (it "a later dir (project) wins over an earlier one (global) for the same extension name"
    (let [global (temp-dir)
          project (temp-dir)
          state (temp-dir)]
      (write-ext! global "counter.py" counter-py)
      (write-ext! project "counter.py"
        (str/replace counter-py "Counter fixture extension." "Project counter."))
      (binding [pyx/*state-dir* (str state)]
        (try
          (let [result (pyx/reload-python-extensions! {:dirs [(str global) (str project)]})]
            (expect (= 1 (:loaded result)))
            (expect (= "Project counter." (:ext/description (registered "counter")))))
          (finally
            (pyx/reload-python-extensions! {:dirs []})))))))
