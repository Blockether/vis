(ns com.blockether.vis.internal.python-extensions-test
  "Python extension host — load fixture `.py` files into trusted GraalPy
   contexts and assert on the registry + adapter contracts. Boots real
   GraalPy contexts (on the shared engine), no model in the loop."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.persistance :as ps]
            [com.blockether.vis.internal.python-extensions :as pyx]
            [com.blockether.vis.internal.registry :as registry]
            [com.blockether.vis.internal.python-test-runner :as runner]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

;; =============================================================================
;; Harness
;; =============================================================================

(defn- temp-dir
  ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-pyext-test" (make-array FileAttribute 0))))

(defn- write-ext!
  [^java.io.File dir fname source]
  (let [f (io/file dir fname)]
    (io/make-parents f)
    (spit f source)
    f))


(defn- with-loaded
  "Load `.py` sources (map of filename -> source) from a temp dir with
   `vis.state` confined to a throwaway in-memory DB, run `f` with the load
   result, then tear everything down so tests stay isolated."
  [sources f]
  (let
    [ext-dir
     (temp-dir)

     store
     (ps/db-create-connection! :memory)]

    (doseq [[fname src] sources]
      (write-ext! ext-dir fname src))
    (binding [pyx/*state-env* {:db-info store}]
      (try
        (f (pyx/reload-python-extensions! {:dirs [(str ext-dir)]}) {:ext-dir ext-dir :store store})
        (finally (pyx/reload-python-extensions! {:dirs []}) (ps/db-dispose-connection! store))))))

(defn- registered
  [ext-name]
  (some #(when (= ext-name (:ext/name %)) %) (extension/registered-extensions)))

(defn- symbol-fn
  [ext sym]
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
        vis.symbol(counter_boom, tag=\"observation\", is_hidden=True),
    ],
    prompt=\"counter_ surface active.\",
    slash_commands=[vis.slash(\"count\", _slash, doc=\"Show the counter.\")],
)
")

;; =============================================================================
;; Loading + registry
;; =============================================================================

(defdescribe
  load-and-register-test
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
                         (expect (= :mutation (:ext.symbol/tag bump))))
                       ;; is_hidden=True -> the :ext.symbol/hidden? predicate key
                       (let [boom (last (get-in ext [:ext/engine :ext.engine/symbols]))]
                         (expect (= 'boom (:ext.symbol/symbol boom)))
                         (expect (true? (:ext.symbol/hidden? boom))))))))
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
                                (let
                                  [bump
                                   (symbol-fn (registered "counter") 'bump)

                                   result
                                   (bump 5)]

                                  (expect (extension/envelope-success? result))
                                  (expect (= 5 (get-in result [:result "count"])))))))
             (it "a raised Python exception = failure envelope with the Python message"
                 (with-loaded {"counter.py" counter-py}
                              (fn [_ _]
                                (let
                                  [boom
                                   (symbol-fn (registered "counter") 'boom)

                                   result
                                   (boom)]

                                  (expect (extension/envelope-failure? result))
                                  (expect (str/includes? (get-in result [:error :message])
                                                         "kaboom")))))))

;; =============================================================================
;; State — durable across reloads
;; =============================================================================

(defdescribe state-durability-test
             (it "vis.state survives a full reload (fresh contexts, same DB)"
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
                 (with-loaded
                   {"counter.py" counter-py}
                   (fn [_ _]
                     (let
                       [spec
                        (first (:ext/slash-commands (registered "counter")))

                        res
                        ((:slash/run-fn spec)
                          {:channel/id :tui :command/argv ["a" "b"] :command/raw "/count a b"})]

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
;; Ctx contribution — vis.extension(ctx=...) folds into the session bag
;; =============================================================================

(def ^:private ctxer-py
  "\"\"\"Ctx-contribution fixture.\"\"\"
import vis


def _ctx(env):
    return {\"session_env\": {\"demo\": {\"cwd\": env[\"cwd\"], \"hits\": vis.state.get(\"hits\", 0)}}}


def _bad_ctx(env):
    return \"not a dict\"


vis.extension(
    name=\"ctxer\",
    description=\"Ctx fixture extension.\",
    kind=\"fun\",
    ctx=_ctx,
)
")

(defdescribe
  ctx-contribution-test
  (it "vis.extension(ctx=...) registers an :ext/ctx-fn that folds into the session bag"
      (with-loaded {"ctxer.py" ctxer-py}
                   (fn [_ _]
                     (let
                       [ext
                        (registered "ctxer")

                        contribution
                        ((:ext/ctx-fn ext) {:workspace/root "/p" :session-id "s1"})]

                       ;; STRING-keyed all the way down, ready to deep-merge into `session`
                       (expect (= 0 (get-in contribution ["session_env" "demo" "hits"])))
                       (expect (string? (get-in contribution ["session_env" "demo" "cwd"])))
                       ;; and it merges through the real aggregation path
                       (let [merged (extension/ctx-contributions {:workspace/root "/p"} [ext])]
                         (expect (= 0 (get-in merged ["session_env" "demo" "hits"]))))))))
  (it "a ctx fn that returns a non-map degrades to an empty contribution"
      (with-loaded {"badctx.py" (str/replace ctxer-py "ctx=_ctx" "ctx=_bad_ctx")}
                   (fn [_ _]
                     (expect (= {} ((:ext/ctx-fn (registered "ctxer")) {:workspace/root "/p"}))))))
  (it "a non-callable ctx= is rejected at load"
      (with-loaded {"badctx2.py"
                    (str "import vis\n"
                         "vis.extension(name='bc2', description='d', kind='x', ctx=42)\n")}
                   (fn [result _]
                     (expect (= 1 (:failed result)))
                     (expect (str/includes? (:error (first (pyx/load-failures))) "ctx="))))))

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

(defdescribe
  op-hook-test
  (it
    "'before' hooks compile to :around guards that can block with a failure envelope"
    (with-loaded
      {"guard.py" guard-py}
      (fn [_ _]
        (let
          [hooks
           (:ext/op-hooks (registered "guard"))

           write-hook
           (some #(when (= :write (:op %)) %) hooks)]

          (expect (= #{:write :patch} (set (map :op hooks))))
          (expect (every? #(= :around (:phase %)) hooks))
          ;; blocked: guard returns vis.block -> failure envelope, next never runs
          (let
            [ran?
             (atom false)

             res
             ((:fn write-hook)
               {}
               :write
               ["/x/.env" "data"]
               (fn [_]
                 (reset! ran? true)
                 :ran))]

            (expect (extension/envelope-failure? res))
            (expect (str/includes? (get-in res [:error :message]) "protected"))
            (expect (false? @ran?)))
          ;; allowed: guard returns None -> next runs with original args
          (expect (= :ran
                     ((:fn write-hook)
                       {}
                       :write
                       ["/x/ok.txt" "data"]
                       (fn [_]
                         :ran)))))))))

(def ^:private filter-py
  "import vis

def _req(r):
    if r['method'] == 'POST':
        return vis.block('no posting to ' + r['host'])
    return None

def _resp(r):
    if r['status'] == 403:
        return vis.block('upstream 403')
    return None

vis.extension(
    name='filt',
    description='Egress filter fixture.',
    kind='guard',
    network_filters=[vis.network_filter(_req), vis.network_filter(_resp)],
)
")

(defdescribe
  egress-filter-test
  (it "vis.network_filter registers host egress filters (request + response phases) that can block"
      (with-loaded
        {"filt.py" filter-py}
        (fn [_ _]
          (let
            [ext
             (registered "filt")

             rf
             (first (:ext/network-filters ext))

             pf
             (second (:ext/network-filters ext))]

            (expect (some? rf))
            (expect (some? pf))
            ;; request filter: POST blocked with the reason, GET allowed
            (let [d (rf {:phase :http :method "POST" :host "x.com" :path "/" :headers {}})]
              (expect (false? (:allow? d)))
              (expect (str/includes? (:reason d) "no posting to x.com")))
            (expect (:allow? (rf {:phase :http :method "GET" :host "x.com" :path "/" :headers {}})))
            ;; response filter: upstream 403 blocked, 200 allowed
            (expect
              (false?
                (:allow?
                  (pf {:phase :http-response :status 403 :host "x.com" :path "/" :headers {}}))))
            (expect
              (:allow?
                (pf {:phase :http-response :status 200 :host "x.com" :path "/" :headers {}}))))))))

;; =============================================================================
;; Failure containment
;; =============================================================================

(defdescribe load-failure-test
             (it "a broken file is a recorded load failure, never a crash"
                 (with-loaded {"broken.py" "import vis\nraise RuntimeError('nope at import')\n"}
                              (fn [result _]
                                (expect (= 0 (:loaded result)))
                                (expect (= 1 (:failed result)))
                                (expect (str/includes? (:error (first (pyx/load-failures)))
                                                       "nope at import")))))
             (it "a file that never calls vis.extension() is a load failure"
                 (with-loaded {"empty.py" "x = 1\n"}
                              (fn [result _]
                                (expect (= 1 (:failed result)))
                                (expect (str/includes? (:error (first (pyx/load-failures)))
                                                       "never called vis.extension")))))
             (it "a tool without a docstring is rejected with a clear message"
                 (with-loaded
                   {"nodoc.py" (str "import vis\n" "def nodoc_x():\n    return 1\n"
                                    "vis.extension(name='nodoc', description='d', alias='nodoc',\n"
                                    "              kind='x', symbols=[vis.symbol(nodoc_x)])\n")}
                   (fn [result _]
                     (expect (= 1 (:failed result)))
                     (expect (str/includes? (:error (first (pyx/load-failures))) "docstring"))))))

;; =============================================================================
;; Reload + project-over-global precedence
;; =============================================================================

(defdescribe
  reload-test
  (it "editing a file and reloading swaps the registration"
      (with-loaded {"counter.py" counter-py}
                   (fn [_ {:keys [ext-dir]}]
                     (write-ext!
                       ext-dir
                       "counter.py"
                       (str/replace counter-py "Counter fixture extension." "Counter v2."))
                     (pyx/load-python-extensions! {:dirs [(str ext-dir)]})
                     (expect (= "Counter v2." (:ext/description (registered "counter")))))))
  (it "a failed reload keeps the last-good module (never a stale old+dead mix) — #44"
      (with-loaded
        {"counter.py" counter-py}
        (fn [_ {:keys [ext-dir]}]
          (expect (= 0 (get-in ((symbol-fn (registered "counter") 'read)) [:result "count"])))
          (write-ext! ext-dir "counter.py" (str "BOOM = _vis_undefined_ + 1\n" counter-py))
          (let [result (pyx/load-python-extensions! {:dirs [(str ext-dir)]})]
            (expect (= 1 (:loaded result)))
            (expect (= 1 (:failed result)))
            (expect (str/includes? (:error (first (pyx/load-failures))) "_vis_undefined_"))
            (let [ext (registered "counter")]
              (expect (some? ext))
              (expect (= '[bump read boom]
                         (mapv :ext.symbol/symbol (get-in ext [:ext/engine :ext.engine/symbols]))))
              (expect (= 0 (get-in ((symbol-fn ext 'read)) [:result "count"]))))))))
  (it "change listeners see every (re)load and removal"
      (let [events (atom [])]
        (pyx/add-change-listener! ::test #(swap! events conj %))
        (try (with-loaded {"counter.py" counter-py}
                          (fn [_ {:keys [ext-dir]}]
                            ;; initial load: counter registered, nothing removed
                            (let [{:keys [extensions removed]} (last @events)]
                              (expect (= ["counter"] (mapv :ext/name extensions)))
                              (expect (= [] removed)))
                            ;; edit + reload: fresh registration, still nothing removed
                            (write-ext!
                              ext-dir
                              "counter.py"
                              (str/replace counter-py "Counter fixture extension." "Counter v2."))
                            (pyx/load-python-extensions! {:dirs [(str ext-dir)]})
                            (let [{:keys [extensions removed]} (last @events)]
                              (expect (= "Counter v2." (:ext/description (first extensions))))
                              (expect (= [] removed)))))
             ;; with-loaded's teardown scanned an empty dir set -> counter removed
             (let [{:keys [extensions removed]} (last @events)]
               (expect (= [] extensions))
               (expect (= ["counter"] removed)))
             (finally (pyx/remove-change-listener! ::test)))))
  (it "a later dir (project) wins over an earlier one (global) for the same extension name"
      (let
        [global
         (temp-dir)

         project
         (temp-dir)

         store
         (ps/db-create-connection! :memory)]

        (write-ext! global "counter.py" counter-py)
        (write-ext! project
                    "counter.py"
                    (str/replace counter-py "Counter fixture extension." "Project counter."))
        (binding [pyx/*state-env* {:db-info store}]
          (try (let [result (pyx/reload-python-extensions! {:dirs [(str global) (str project)]})]
                 (expect (= 1 (:loaded result)))
                 (expect (= "Project counter." (:ext/description (registered "counter")))))
               (finally (pyx/reload-python-extensions! {:dirs []})
                        (ps/db-dispose-connection! store)))))))

;; =============================================================================
;; Multi-file project — an extension imports a sibling package (sys.path sugar)
;; =============================================================================

(def ^:private pkgext-py
  "\"\"\"Package-backed fixture: imports a sibling package next to it.\"\"\"
import vis
from mypkg.core import add
from mypkg import VERSION


def pkg_add(a, b):
    \"\"\"await pkg_add(a, b) -> {\\\"sum\\\", \\\"version\\\"} — add via the sibling package.\"\"\"
    return {\"sum\": add(a, b), \"version\": VERSION}


vis.extension(
    name=\"pkgext\",
    description=\"Package-backed fixture extension.\",
    version=\"0.1.0\",
    kind=\"integration\",
    alias=\"pkg\",
    symbols=[vis.symbol(pkg_add, tag=\"observation\")],
)
")

(defdescribe
  package-import-test
  (it "a flat extension file imports a sibling package placed next to it — no manual sys.path"
      (with-loaded {"mypkg/__init__.py" "VERSION = \"1.2.3\"\n"
                    "mypkg/core.py" "def add(a, b):\n    return a + b\n"
                    "pkgext.py" pkgext-py}
                   (fn [result _]
                     ;; only the top-level pkgext.py is scanned as an extension;
                     ;; the package files under mypkg/ are NOT loaded as extensions
                     (expect (= {:loaded 1 :failed 0 :changed? true} result))
                     (let [ext (registered "pkgext")]
                       (expect (some? ext))
                       (let
                         [add (symbol-fn ext 'add)
                          res (add 2 3)]

                         (expect (extension/envelope-success? res))
                         (expect (= 5 (get-in res [:result "sum"])))
                         (expect (= "1.2.3" (get-in res [:result "version"])))))))))

;; =============================================================================
;; Package-extension convention — a subdir holding extension.py = ONE extension
;; =============================================================================

(defdescribe
  package-extension-convention-test
  (it "a subdir holding extension.py loads as ONE extension; its package/test files are not scanned"
      (with-loaded
        {"my_ext/mypkg/__init__.py" "VERSION = \"9.9\"\n"
         "my_ext/mypkg/core.py" "def add(a, b):\n    return a + b\n"
         "my_ext/extension.py"
         (str "import vis\n" "from mypkg.core import add\n"
              "def mx_add(a, b):\n"
              "    \"\"\"await mx_add(a, b) -> {\"sum\"} — add via the sibling package.\"\"\"\n"
              "    return {\"sum\": add(a, b)}\n"
              "vis.extension(name=\"myext\", description=\"d\", version=\"0.1.0\",\n"
              "              kind=\"integration\", alias=\"mx\",\n"
              "              symbols=[vis.symbol(mx_add, tag=\"observation\")])\n")
         "my_ext/test_core.py" "def test_ok():\n    assert 1 == 1\n"}
        (fn [result _]
          ;; the package dir contributes exactly ONE extension; the
          ;; modules under mypkg/ and the test file are NOT loaded
          (expect (= {:loaded 1 :failed 0 :changed? true} result))
          (let [ext (registered "myext")]
            (expect (some? ext))
            (let [add (symbol-fn ext 'add)]
              (expect (= 3 (get-in (add 1 2) [:result "sum"])))))))))

;; =============================================================================
;; Python-level self-tests — test_*.py / *_test.py run through the pytest shim
;; =============================================================================

(defdescribe
  python-self-test-test
  (it
    "runs test_*.py / *_test.py through the pytest shim, imports the sibling package, reports pass/fail"
    (let
      [ext-dir
       (temp-dir)

       store
       (ps/db-create-connection! :memory)]

      (write-ext! ext-dir "my_ext/mypkg/__init__.py" "VERSION = \"1.0\"\n")
      (write-ext! ext-dir "my_ext/mypkg/core.py" "def add(a, b):\n    return a + b\n")
      (write-ext! ext-dir
                  "my_ext/extension.py"
                  (str "import vis\n" "def noop():\n"
                       "    \"\"\"await noop() -> {} — nothing.\"\"\"\n" "    return {}\n"
                       "vis.extension(name=\"mx\", description=\"d\", kind=\"fun\", alias=\"mx\",\n"
                       "              symbols=[vis.symbol(noop, tag=\"observation\")])\n"))
      ;; a test INSIDE the package — imports mypkg via the sys.path sugar
      (write-ext! ext-dir
                  "my_ext/test_core.py"
                  (str "from mypkg.core import add\n"
                       "def test_add():\n    assert add(2, 3) == 5\n"))
      ;; a top-level test file with one passing + one failing case
      (write-ext! ext-dir
                  "foo_test.py"
                  (str "def test_pass():\n    assert 1 + 1 == 2\n"
                       "def test_fail():\n    assert 2 + 2 == 5\n"))
      (binding [pyx/*state-env* {:db-info store}]
        (try (let [res (runner/test-python-extensions! {:dirs [(str ext-dir)]})]
               (expect (= 2 (:files res)))
               (expect (= 2 (:passed res)))
               (expect (= 1 (:failed res)))
               (expect (false? (:ok? res)))
               (let
                 [by-name
                  (into {} (map (juxt #(last (str/split (:file %) #"/")) :ok?)) (:results res))]
                 ;; the package test resolves `from mypkg.core import add`
                 (expect (true? (get by-name "test_core.py")))
                 (expect (false? (get by-name "foo_test.py")))))
             (finally (ps/db-dispose-connection! store)))))))

;; =============================================================================
;; Structured counts — outcomes come from the shim, never scraped from stdout
;; =============================================================================

(defdescribe
  structured-counts-test
  (it "a failure whose assertion message contains '9 passed' must NOT inflate the pass count"
      (let
        [ext-dir
         (temp-dir)

         store
         (ps/db-create-connection! :memory)]

        ;; the failure detail literally says \"9 passed\" — a stdout regex would
        ;; miscount it as nine passes; the shim's structured outcomes cannot lie
        (write-ext! ext-dir
                    "liar_test.py"
                    "def test_only_fail():\n    assert False, \"9 passed items were expected\"\n")
        (binding [pyx/*state-env* {:db-info store}]
          (try (let [res (runner/test-python-extensions! {:dirs [(str ext-dir)]})]
                 (expect (= 1 (:files res)))
                 (expect (= 1 (:failed res)))
                 (expect (= 0 (get res :passed 0)))
                 (expect (false? (:ok? res))))
               (finally (ps/db-dispose-connection! store)))))))

;; =============================================================================
;; /test slash + `vis ext test` CLI — the user-facing surface for the runner
;; =============================================================================

(defdescribe cli-and-slash-wiring-test
             (it "the loader exposes a /test slash command and a `vis ext test` CLI command"
                 (with-loaded {"counter.py" counter-py}
                              (fn [_ _]
                                ;; Force a fresh registration so we read the
                                ;; CURRENT loader spec, not a stale one left by an
                                ;; earlier load in a reused REPL JVM (the
                                ;; `loader-registered?` defonce guard blocks re-runs).
                                (reset! @#'pyx/loader-registered? false)
                                (#'pyx/register-loader-extension!)
                                (let
                                  [loader
                                   (registered "python-extensions")

                                   slash
                                   (some #(when (= "test" (:slash/name %)) %)
                                         (:ext/slash-commands loader))

                                   cli
                                   (some #(when (= "test" (:cmd/name %)) %) (:ext/cli loader))]

                                  (expect (some? loader))
                                  (expect (some? slash))
                                  (expect (ifn? (:slash/run-fn slash)))
                                  (expect (some? cli))
                                  (expect (ifn? (:cmd/run-fn cli)))
                                  (expect (true? (:cmd/internal? cli))))))))

(defdescribe
  run-and-report-test
  (it "renders a friendly message when no tests are found"
      (expect (str/includes? (#'runner/render-test-report {:files 0 :ok? true :results []})
                             "No Python extension tests")))
  (it "the shared /test + `vis ext test` code path runs tests and renders a report"
      (let
        [ext-dir
         (temp-dir)

         store
         (ps/db-create-connection! :memory)]

        (write-ext! ext-dir
                    "foo_test.py"
                    (str "def test_pass():\n    assert 1 + 1 == 2\n"
                         "def test_fail():\n    assert 2 + 2 == 5\n"))
        (binding [pyx/*state-env* {:db-info store}]
          (try (let [{:keys [result report]} (#'runner/run-and-report {:dirs [(str ext-dir)]})]
                 (expect (= 1 (:files result)))
                 (expect (false? (:ok? result)))
                 (expect (str/includes? report "1 passed"))
                 (expect (str/includes? report "1 failed"))
                 (expect (str/includes? report "\u2717"))
                 (expect (str/includes? report "foo_test.py")))
               (finally (ps/db-dispose-connection! store)))))))

;; =============================================================================
;; Per-test granularity — the runner reports EACH test, not just a file verdict
;; =============================================================================

(defdescribe
  per-test-granularity-test
  (it "reports each test's nodeid + outcome (tagged with its file), not just a per-file aggregate"
      (let
        [ext-dir
         (temp-dir)

         store
         (ps/db-create-connection! :memory)]

        (write-ext! ext-dir
                    "foo_test.py"
                    (str "def test_alpha():\n    assert 1 + 1 == 2\n"
                         "def test_beta():\n    assert 2 + 2 == 5\n"))
        (binding [pyx/*state-env* {:db-info store}]
          (try (let
                 [res (runner/test-python-extensions! {:dirs [(str ext-dir)]})
                  by-id (into {} (map (juxt :nodeid :outcome)) (:tests res))]

                 (expect (= 2 (count (:tests res))))
                 (expect (= :passed (get by-id "test_alpha")))
                 (expect (= :failed (get by-id "test_beta")))
                 ;; every record carries the file it came from
                 (expect (every? :file (:tests res)))
                 (let [report (#'runner/render-test-report res)]
                   (expect (str/includes? report "test_alpha"))
                   (expect (str/includes? report "test_beta"))))
               (finally (ps/db-dispose-connection! store)))))))

;; =============================================================================
;; `vis ext test` exit signal — a :vis/user-error ex-info, NEVER System/exit
;; =============================================================================

(defdescribe
  cli-exit-signal-test
  (it "signals a failed run by throwing a :vis/user-error ex-info (mapped to a non-zero exit)"
      (let [ex (#'runner/failure-ex {:ok? false :failed 2 :errored 0})]
        (expect (instance? clojure.lang.ExceptionInfo ex))
        (expect (true? (:vis/user-error (ex-data ex))))
        (expect (str/includes? (ex-message ex) "2 failed"))))
  (it "produces no exit signal (nil) when every test passed"
      (expect (nil? (#'runner/failure-ex {:ok? true :passed 3})))))

;; =============================================================================
;; Providers — a `vis.provider(...)` registers a first-class provider descriptor
;; =============================================================================

(def ^:private provider-py
  "'''Acme provider fixture.'''
import vis


def _token():
    return {'token': 'sk-test-123', 'api_url': 'https://acme.test/v1'}


def _status():
    return {'is_authenticated': True, 'source': 'env-var', 'provider_id': 'acme'}


def _detect():
    return {'token': 'sk-test-123', 'source': 'env-var'}


_logout_calls = {'n': 0}


def _logout():
    _logout_calls['n'] += 1
    return 'logged-out'


def _limits():
    return {'provider_id': 'acme', 'status': 'ok',
            'dynamic': {'limits': [{'id': 'acme-daily', 'kind': 'tokens',
                                    'used': 10.0, 'limit': 100.0}]}}


# Strict 0-param refresh: the runtime calls (f rejected-token); the adapter
# must DROP the extra arg and still return the fresh token.
def _refresh():
    return {'token': 'sk-fresh-999', 'api_url': 'https://acme.test/v1'}


# 1-param refresh: must RECEIVE the rejected token the runtime threads in.
def _refresh_with_arg(rejected):
    return {'token': 'sk-fresh-abc', 'rejected_was': rejected}


def _auth(printer):
    printer('  Visit https://acme.test/device and enter code ABCD.')
    printer('  Then re-run.')
    return 'ok'


def _auth_prompt():
    return ['Acme OAuth: run `vis providers auth acme-oauth`.',
            'Or set ACME_TOKEN=... in the environment.']


def _enrich(provider, router_opts):
    # provider crosses in as a plain string-keyed dict (stringify-deep) so the
    # hook can read model names; return an enriched model list.
    return [{'name': m['name'], 'context': 262144, 'is_tool_call': True}
            for m in provider['models']]


_events = {'selected': None}


def _on_selected(event):
    # side-effect hook: capture the marshalled selection event.
    _events['selected'] = {'source': event['source'],
                           'provider_id': event['provider']['id']}


def seen_selected():
    '''Return the last on_selected event captured (test observation).'''
    return _events['selected']


vis.extension(
    name='provider-acme',
    description='Acme static-key provider fixture.',
    alias='acme',
    symbols=[vis.symbol(seen_selected)],
    providers=[
        vis.provider(
            id='acme',
            label='Acme AI',
            preset={'base-url': 'https://acme.test/v1',
                    'api_style': 'openai',
                    'default_models': ['acme-large', 'acme-small'],
                    'responses_path': '/responses',
                    'extra_body': {'temperature': 0.6, 'top_p': 0.95}},
            get_token_fn=_token,
            status_fn=_status,
            detect_fn=_detect,
            logout_fn=_logout,
            limits_fn=_limits,
            refresh_token_fn=_refresh,
            enrich_models_fn=_enrich,
            on_selected_fn=_on_selected,
        ),
        vis.provider(
            id='acme-oauth',
            label='Acme OAuth',
            refresh_token_fn=_refresh_with_arg,
            auth_fn=_auth,
            auth_prompt_fn=_auth_prompt,
        ),
    ],
)
")

(defdescribe
  provider-test
  (it
    "a vis.provider(...) registers a first-class provider descriptor (preset + every provider fn)"
    (with-loaded
      {"acme.py" provider-py}
      (fn [_ _]
        (let
          [ext
           (registered "provider-acme")

           entries
           (:ext/providers ext)

           p
           (registry/provider-by-id :acme)

           oauth
           (registry/provider-by-id :acme-oauth)]

          (expect (= 2 (count entries)))
          (expect (some? p))
          (expect (= :acme (:provider/id p)))
          (expect (= "Acme AI" (:provider/label p)))
          ;; preset: dash/underscore keys accepted, api-style -> keyword
          (let [preset (:provider/preset p)]
            (expect (= "https://acme.test/v1" (:base-url preset)))
            (expect (= :openai (:api-style preset)))
            (expect (= ["acme-large" "acme-small"] (:default-models preset)))
            ;; unknown preset keys pass through to svar: responses-path (dash-
            ;; normalized) + extra-body (nested API-literal keys kept verbatim).
            (expect (= "/responses" (:responses-path preset)))
            (expect (= {:temperature 0.6 :top_p 0.95} (:extra-body preset))))
          ;; get-token-fn marshals: snake_case -> kebab keys, string values
          (expect (= {:token "sk-test-123" :api-url "https://acme.test/v1"}
                     ((:provider/get-token-fn p))))
          ;; status-fn: kebab keys + enum values coerced to keywords
          (let [s ((:provider/status-fn p))]
            (expect (true? (:is-authenticated s)))
            (expect (= :env-var (:source s)))
            (expect (= :acme (:provider-id s))))
          ;; detect-fn works and coerces :source
          (let [d ((:provider/detect-fn p))]
            (expect (= "sk-test-123" (:token d)))
            (expect (= :env-var (:source d))))
          ;; limits-fn: nested dynamic limits round-trip
          (let [l ((:provider/limits-fn p))]
            (expect (= :acme (:provider-id l)))
            (expect (= :ok (:status l)))
            (expect (= 1 (count (get-in l [:dynamic :limits]))))
            (expect (= :tokens (get-in l [:dynamic :limits 0 :kind]))))
          ;; logout-fn is a real side-effecting call (returns nil view is fine)
          (expect (some? (:provider/logout-fn p)))
          ((:provider/logout-fn p))
          ;; refresh-token-fn, STRICT 0-param: runtime hands (f rejected);
          ;; the adapter drops the extra arg -> fresh token still returned.
          (expect (= {:token "sk-fresh-999" :api-url "https://acme.test/v1"}
                     ((:provider/refresh-token-fn p) "old-rejected-token")))
          (expect (= {:token "sk-fresh-999" :api-url "https://acme.test/v1"}
                     ((:provider/refresh-token-fn p))))
          ;; auth-fn: host hands in a print! collector; the Python fn calls it to
          ;; emit instruction lines, and its string return coerces to a keyword.
          (let
            [lines
             (atom [])

             collect
             #(swap! lines conj %)

             result
             ((:provider/auth-fn oauth) collect)]

            (expect (= :ok result))
            (expect (= ["  Visit https://acme.test/device and enter code ABCD." "  Then re-run."]
                       @lines)))
          ;; auth-prompt-fn: () -> guidance lines for the API-key dialog body
          (expect (= ["Acme OAuth: run `vis providers auth acme-oauth`."
                      "Or set ACME_TOKEN=... in the environment."]
                     ((:provider/auth-prompt-fn oauth))))
          ;; refresh-token-fn, 1-param: RECEIVES the rejected token.
          (expect (= {:token "sk-fresh-abc" :rejected-was "old-rejected-token"}
                     ((:provider/refresh-token-fn oauth) "old-rejected-token")))
          ;; enrich-models-fn: host provider + router-opts marshal INTO Python as
          ;; plain string-keyed dicts; the return keywordizes and the snake
          ;; `is_tool_call` becomes the `:tool-call?` key the router reads.
          (expect (= [{:name "acme-large" :context 262144 :tool-call? true}
                      {:name "acme-small" :context 262144 :tool-call? true}]
                     ((:provider/enrich-models-fn p)
                       {:id :acme :models [{:name "acme-large"} {:name "acme-small"}]}
                       {})))
          ;; on-selected-fn: the selection event marshals INTO Python (keyword
          ;; keys AND values stringified); the hook captures it and returns nil.
          (expect (nil? ((:provider/on-selected-fn p)
                          {:previous-provider {:id :openai}
                           :provider {:id :acme}
                           :config {:providers [{:id :acme}]}
                           :source :tui})))
          (let [seen (symbol-fn ext (clojure.core/symbol "seen_selected"))]
            (expect (= {"source" "tui" "provider_id" "acme"} (get-in (seen) [:result])))))))))

(defdescribe
  trusted-extension-boundary-test
  (it
    "intentionally permits real filesystem and subprocess access in an unconfined gateway"
    (if (= "1" (System/getenv "VIS_SEATBELT_ACTIVE"))
      (expect true)
      (with-open [ctx (pyx/build-context)]
        (let
          [ok
           (.asBoolean
             (.eval
               ctx
               "python"
               (str
                 "import os, subprocess, tempfile\n"
                 "p = os.path.join(os.path.expanduser('~'), '.vis-extension-trust-test')\n" "try:\n"
                 "    open(p, 'w').write('trusted')\n"
                 "    _ok = open(p).read() == 'trusted' and subprocess.run(['/usr/bin/true']).returncode == 0\n"
                 "finally:\n" "    try: os.unlink(p)\n"
                 "    except FileNotFoundError: pass\n" "_ok")))]
          (expect ok))))))
