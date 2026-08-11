(ns com.blockether.vis.internal.python-extensions-test
  "Python extension host — load fixture `.py` files into trusted GraalPy
   contexts and assert on the registry + adapter contracts. Boots real
   GraalPy contexts (on the shared engine), no model in the loop."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.channel-events :as channel-events]
            [com.blockether.vis.internal.egress-proxy :as egress]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.agents :as agents]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.human-input :as human-input]
            [com.blockether.vis.internal.persistance :as ps]
            [com.blockether.vis.internal.prompt-templates :as prompt-templates]
            [com.blockether.vis.internal.provider-limits :as provider-limits]
            [com.blockether.vis.internal.limits-format :as limits-format]
            [com.blockether.vis.internal.toggles :as toggles]
            [com.blockether.vis.internal.foundation.shell :as shell]
            [com.blockether.vis.internal.python-extensions :as pyx]
            [com.blockether.vis.internal.registry :as registry]
            [com.blockether.vis.internal.python-test-runner :as runner]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [org.graalvm.polyglot Context]))

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

(defonce ^:private live-load
  ;; The one extension set currently loaded in this JVM: {:sources :ext-dir
  ;; :result :fingerprint}. nil when nothing is loaded.
  (atom nil))

(defn- load-sources!
  "Write `sources` to a fresh temp dir and load them, replacing whatever was
   loaded before. Returns the new `live-load` value."
  [sources]
  (let [ext-dir (temp-dir)]
    (doseq [[fname src] sources]
      (write-ext! ext-dir fname src))
    (let [result (pyx/reload-python-extensions! {:dirs [(str ext-dir)]})]
      (reset! live-load
        {:sources sources :ext-dir ext-dir :result result :fingerprint @@#'pyx/last-fingerprint}))))

(defn- with-loaded
  "Load `.py` sources (map of filename -> source) from a temp dir with
   `vis.state` confined to a throwaway in-memory DB, and run `f` with the load
   result.

   An extension is its own GraalPy sandbox and re-executing the 28 KB
   `extension_bootstrap.py` into a fresh one costs ~100ms, so the SAME sources
   are not loaded twice in a row: the load survives between tests and only the
   store is thrown away. That is all the isolation these tests need, because
   `vis.state` resolves the store through `*current-environment*` at CALL time
   — a fresh in-memory DB is a fresh state map even in a reused module.

   A body that loads, reloads or edits extensions itself moves the loader's own
   fingerprint; that invalidates the cache, so the next test reloads."
  [sources f]
  (let
    [{:keys [ext-dir result fingerprint]}
     (let [cached @live-load]
       (if (and (= sources (:sources cached)) (= (:fingerprint cached) @@#'pyx/last-fingerprint))
         cached
         (load-sources! sources)))

     store
     (ps/db-create-connection! :memory)]

    (binding [extension/*current-environment* {:db-info store}]
      (try (f result {:ext-dir ext-dir :store store})
           (catch Throwable t
             (reset! live-load nil)
             (pyx/reload-python-extensions! {:dirs []})
             (throw t))
           (finally (when-not (= fingerprint @@#'pyx/last-fingerprint) (reset! live-load nil))
                    (ps/db-dispose-connection! store))))))

(defn- with-fresh-loaded
  "`with-loaded` for a test that OWNS the load: nothing before it is reused and
   nothing after it may reuse this one, and the teardown scan (the one that
   deregisters the extension) really runs. For a body that damages the sandbox
   on purpose — closing its `Context` — or that asserts on the unload itself."
  [sources f]
  (reset! live-load nil)
  (let
    [{:keys [ext-dir result]}
     (load-sources! sources)

     store
     (ps/db-create-connection! :memory)]

    (binding [extension/*current-environment* {:db-info store}]
      (try (f result {:ext-dir ext-dir :store store})
           (finally (reset! live-load nil)
                    (pyx/reload-python-extensions! {:dirs []})
                    (ps/db-dispose-connection! store))))))

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

(def ^:private kwargs-py
  "\"\"\"Kwargs fixture: keyword arguments must survive the sandbox fold.\"\"\"
import vis


def kw_probe(name, mode=\"plain\", is_deep=False):
    \"\"\"await kw_probe(name, mode, is_deep) -> {\\\"mode\\\"} — echo how the args arrived.\"\"\"
    return {\"name\": name, \"mode\": mode, \"is_deep\": is_deep}


def kw_mapping(payload):
    \"\"\"await kw_mapping(payload) -> {\\\"payload\\\"} — echo a mapping positional.\"\"\"
    return {\"payload\": payload}


vis.extension(
    name=\"kwargs\",
    description=\"Keyword-argument fixture extension.\",
    version=\"0.1.0\",
    kind=\"integration\",
    alias=\"kw\",
    symbols=[
        vis.symbol(kw_probe, tag=\"observation\"),
        vis.symbol(kw_mapping, tag=\"observation\"),
    ],
)
")

(defdescribe
  python-kwargs-test
  (it "keyword args folded into ONE trailing map are re-expanded onto the signature — #83"
      (with-loaded {"kwargs.py" kwargs-py}
                   (fn [_ _]
                     (let
                       [probe
                        (symbol-fn (registered "kwargs") 'probe)

                        result
                        ;; how the sandbox delivers probe(g, mode=deep, is_deep=True)
                        (probe "g" {"mode" "deep" "is_deep" true})]

                       (expect (extension/envelope-success? result))
                       (expect (= "g" (get-in result [:result "name"])))
                       (expect (= "deep" (get-in result [:result "mode"])))
                       (expect (true? (get-in result [:result "is_deep"])))))))
  (it "a plain positional call is untouched"
      (with-loaded {"kwargs.py" kwargs-py}
                   (fn [_ _]
                     (let [result ((symbol-fn (registered "kwargs") 'probe) "g" "deep")]
                       (expect (= "deep" (get-in result [:result "mode"])))
                       (expect (false? (get-in result [:result "is_deep"])))))))
  (it "a genuine mapping positional stays ONE argument"
      (with-loaded {"kwargs.py" kwargs-py}
                   (fn [_ _]
                     (let [result ((symbol-fn (registered "kwargs") 'mapping) {"a" 1 "b" 2})]
                       (expect (extension/envelope-success? result))
                       (expect (= 1 (get-in result [:result "payload" "a"])))
                       (expect (= 2 (get-in result [:result "payload" "b"]))))))))

;; =============================================================================
;; Declared host environment -- issue #129
;; =============================================================================

(def ^:private env-py
  "\"\"\"Declared host env allowlist fixture.\"\"\"
import os
import vis


def env_probe():
    \"\"\"await env_probe() -> {\"has_path\", \"has_unset\", \"host_path\", \"host_default\"} -- report declared env reachability.\"\"\"
    return {\"has_path\": os.environ.get(\"PATH\") is not None,
            \"has_unset\": os.environ.get(\"VIS_TEST_NEVER_SET_129\") is not None,
            \"host_path\": vis.host_env(\"PATH\") is not None,
            \"host_default\": vis.host_env(\"VIS_TEST_NEVER_SET_129\", \"fallback\")}


vis.extension(
    name=\"env-allowlist\",
    description=\"Declared env allowlist fixture.\",
    version=\"0.1.0\",
    kind=\"integration\",
    alias=\"env\",
    symbols=[vis.symbol(env_probe, tag=\"observation\")],
    env=[\"PATH\", \"VIS_TEST_NEVER_SET_129\"],
)
")

(def ^:private env-bad-py
  "\"\"\"Bad env= fixture -- not a list.\"\"\"
import vis


vis.extension(name=\"env-bad\", description=\"bad env fixture.\", env=\"PATH\")
")

;; Regression, issue #129: a Python extension could not read host env vars.
;; `vis.extension(...)` did not accept `env=`, so any extension declaring the
;; variables it needed raised at load and its provider was silently absent.
;; Now `env=` declares an allowlist the host resolves from the process
;; environment (`System/getenv`), and the declaration lands on `:ext/env` so
;; doctor/the TUI can surface a declared-but-unset variable instead of failing
;; silently.
(defdescribe
  declared-host-env-test
  (it "resolve-declared-env reads System/getenv and drops unset/malformed names"
      (let [path (System/getenv "PATH")]
        (expect (some? path))
        (expect (= path (get (pyx/resolve-declared-env ["PATH"]) "PATH")))
        (expect (= {} (pyx/resolve-declared-env ["VIS_TEST_NEVER_SET_129"])))
        (expect (= {"PATH" path} (pyx/resolve-declared-env ["9BAD" "" "BAD-NAME" "PATH"])))
        (expect (= {} (pyx/resolve-declared-env nil)))
        (expect (= {} (pyx/resolve-declared-env [])))))
  (it "a name declared only under `environment:` reaches an extension unasked"
      (let [previous @config/active-config]
        (try (reset! config/active-config {:environment {"VIS_TEST_ENV_BLOCK"
                                                         {"command" ["/bin/echo" "from-config"]}}})
             (expect (= "from-config" (get (pyx/resolve-declared-env []) "VIS_TEST_ENV_BLOCK")))
             (finally (reset! config/active-config previous)))))
  (it "accepts env=, injects declared vars, and registers :ext/env"
      (with-loaded {"env_allowlist.py" env-py}
                   (fn [result _]
                     (expect (= {:loaded 1 :failed 0 :changed? true} result))
                     (let [ext (registered "env-allowlist")]
                       (expect (some? ext))
                       (expect (= [{:name "PATH" :required? true}
                                   {:name "VIS_TEST_NEVER_SET_129" :required? true}]
                                  (:ext/env ext)))
                       (let
                         [probe (symbol-fn ext 'probe)
                          out (get-in (probe) [:result])]

                         (expect (true? (get out "has_path")))
                         (expect (false? (get out "has_unset")))
                         (expect (true? (get out "host_path")))
                         (expect (= "fallback" (get out "host_default"))))))))
  (it "rejects env= that is not a list of variable names"
      (with-loaded {"env_bad.py" env-bad-py}
                   (fn [result _]
                     (expect (= 1 (:failed result)))
                     (let [errs (map :error (pyx/load-failures))]
                       (expect (some #(str/includes? (str/lower-case %) "env") errs)))))))

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
    op_hooks=[vis.op_hook([\"struct_patch\", \"patch\"], _guard, phase=\"before\")],
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

           struct-hook
           (some #(when (= :struct_patch (:op %)) %) hooks)]

          (expect (= #{:struct_patch :patch} (set (map :op hooks))))
          (expect (every? #(= :around (:phase %)) hooks))
          ;; blocked: guard returns vis.block -> failure envelope, next never runs
          (let
            [ran?
             (atom false)

             res
             ((:fn struct-hook)
               {}
               :struct_patch
               ["/x/.env" "data"]
               (fn [_]
                 (reset! ran? true)
                 :ran))]

            (expect (extension/envelope-failure? res))
            (expect (str/includes? (get-in res [:error :message]) "protected"))
            (expect (false? @ran?)))
          ;; allowed: guard returns None -> next runs with original args
          (expect (= :ran
                     ((:fn struct-hook)
                       {}
                       :struct_patch
                       ["/x/ok.txt" "data"]
                       (fn [_]
                         :ran)))))))))

;; =============================================================================
;; Gate hooks — a Python extension guards the FILESYSTEM, not a tool's arguments
;; =============================================================================

(def ^:private fs-gate-py
  "\"\"\"Filesystem gate fixture.\"\"\"
import vis


def _fs_gate(access):
    if \"secrets\" in access[\"path\"] and access[\"operation\"].endswith(\"write\"):
        return vis.block(\"the vault is written through the vault tool\")
    return None


vis.extension(
    name=\"vault\",
    description=\"Vault fixture extension.\",
    kind=\"guard\",
    op_hooks=[vis.op_hook([\"fs_access\"], _fs_gate)],
)
")

(def ^:private broken-fs-gate-py
  "\"\"\"Filesystem gate that breaks.\"\"\"
import vis


def _fs_gate(access):
    raise RuntimeError(\"the rule table is unreadable\")


vis.extension(
    name=\"broken-vault\",
    description=\"Broken vault fixture extension.\",
    kind=\"guard\",
    op_hooks=[vis.op_hook([\"fs_access\"], _fs_gate)],
)
")

(defdescribe
  fs-access-gate-python-test
  "A gate is ASKED, never wrapped: the Python callable receives the gate's own
   string-keyed ctx and answers with `vis.block(reason)` or None. The op reaches
   `confine!`, so the rule binds `open(p, 'w')` and `write` alike — an argument
   guard could only ever see the tool."
  (it "compiles a Python fs_access hook into a :gate hook that refuses by sentence"
      (with-loaded
        {"vault.py" fs-gate-py}
        (fn [_ _]
          (let
            [hooks
             (:ext/op-hooks (registered "vault"))

             gate
             (first hooks)]

            (expect (= 1 (count hooks)))
            (expect (= :fs/access (:op gate)))
            (expect (= :gate (:phase gate)))
            ;; refused: the guest's own sentence crosses back, unchanged
            (expect (= {:reason "the vault is written through the vault tool"}
                       ((:fn gate) {} :fs/access {:operation "file-write" :path "/w/secrets/key"})))
            ;; a READ of the same path, and a write elsewhere, both pass
            (expect (nil?
                      ((:fn gate) {} :fs/access {:operation "file-read" :path "/w/secrets/key"})))
            (expect
              (nil? ((:fn gate) {} :fs/access {:operation "file-write" :path "/w/notes.txt"})))))))
  (it "fails CLOSED when the Python guard raises"
      (with-loaded {"broken_vault.py" broken-fs-gate-py}
                   (fn [_ _]
                     (let
                       [gate
                        (first (:ext/op-hooks (registered "broken-vault")))

                        refusal
                        ((:fn gate) {} :fs/access {:operation "file-read" :path "/w/notes.txt"})]

                       ;; The opposite of an ordinary hook, which logs and runs on: a
                       ;; boundary that opens when its guard breaks is not a boundary.
                       (expect (some? refusal))
                       (expect (str/includes? (:reason refusal) "fails closed"))
                       (expect (str/includes? (:reason refusal)
                                              "the rule table is unreadable")))))))

(defdescribe
  op-hook-payload-test
  "Op-hook payloads carry ORDINARY host data — a keyword op enum, keyword arg
   keys, keyword result values. Unstringified they died on the STRINGS-ONLY
   boundary INSIDE the hook, taking down the very call the hook only observed."
  (let [payload #'pyx/op-hook-payload]
    (it "a before-hook payload is strings-only and crosses the boundary intact"
        (let [p (payload :struct_patch [{:path "/x/a.clj" :is-dirty-ok true}])]
          (expect (= {"op" "struct_patch" "args" [{"path" "/x/a.clj" "is-dirty-ok" true}]} p))
          (expect (= p (ep/boundary-view p)))))
    (it "an after-hook payload stringifies the result too"
        (let [p (payload :grep ["needle"] {:status :ok :hits [{:path "a.clj"}]})]
          (expect
            (= {"op" "grep" "args" ["needle"] "result" {"status" "ok" "hits" [{"path" "a.clj"}]}}
               p))
          (expect (= p (ep/boundary-view p)))))))

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
        (try (with-fresh-loaded
               {"counter.py" counter-py}
               (fn [_ {:keys [ext-dir]}]
                 ;; initial load: counter registered, nothing removed
                 (let [{:keys [extensions removed]} (last @events)]
                   (expect (= ["counter"] (mapv :ext/name extensions)))
                   (expect (= [] removed)))
                 ;; edit + reload: fresh registration, still nothing removed
                 (write-ext! ext-dir
                             "counter.py"
                             (str/replace counter-py "Counter fixture extension." "Counter v2."))
                 (pyx/load-python-extensions! {:dirs [(str ext-dir)]})
                 (let [{:keys [extensions removed]} (last @events)]
                   (expect (= "Counter v2." (:ext/description (first extensions))))
                   (expect (= [] removed)))))
             ;; with-fresh-loaded's teardown scanned an empty dir set -> counter removed
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
        (binding [extension/*current-environment* {:db-info store}]
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
      (binding [extension/*current-environment* {:db-info store}]
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
        (binding [extension/*current-environment* {:db-info store}]
          (try (let [res (runner/test-python-extensions! {:dirs [(str ext-dir)]})]
                 (expect (= 1 (:files res)))
                 (expect (= 1 (:failed res)))
                 (expect (= 0 (get res :passed 0)))
                 (expect (false? (:ok? res))))
               (finally (ps/db-dispose-connection! store)))))))

;; =============================================================================
;; /test slash + `vis extension test` CLI — the user-facing surface for the runner
;; =============================================================================

(defdescribe cli-and-slash-wiring-test
             (it "the loader exposes a /test slash command and a `vis extension test` CLI command"
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
  (it "the shared /test + `vis extension test` code path runs tests and renders a report"
      (let
        [ext-dir
         (temp-dir)

         store
         (ps/db-create-connection! :memory)]

        (write-ext! ext-dir
                    "foo_test.py"
                    (str "def test_pass():\n    assert 1 + 1 == 2\n"
                         "def test_fail():\n    assert 2 + 2 == 5\n"))
        (binding [extension/*current-environment* {:db-info store}]
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
        (binding [extension/*current-environment* {:db-info store}]
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
;; `vis extension test` exit signal — a :vis/user-error ex-info, NEVER System/exit
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
            'dynamic': {'limits': [{'id': 'acme-daily', 'label': 'Daily tokens',
                                    'scope': 'account', 'kind': 'tokens',
                                    'precision': 'exact', 'source': 'provider-api',
                                    'is_unlimited': False,
                                    'used': 25.49, 'limit': 100.0}]}}


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
    return ['Acme OAuth: run `vis-agent providers auth acme-oauth`.',
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
            preset={'base_url': 'https://acme.test/v1',
                    'api_style': 'openai',
                    'default_models': ['acme-large', 'acme-small'],
                    'responses_path': '/responses',
                    'llm_headers': {'X-Initiator': 'agent'},
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
          ;; preset: one snake_case spelling per key, api-style -> keyword
          (let [preset (:provider/preset p)]
            (expect (= "https://acme.test/v1" (:base-url preset)))
            (expect (= :openai (:api-style preset)))
            (expect (= ["acme-large" "acme-small"] (:default-models preset)))
            ;; An UNDECLARED preset key is named through `wire/engine-key` and
            ;; its VALUE is never entered: `extra_body`/`llm_headers` are the
            ;; author's own API payloads and reach svar exactly as written —
            ;; string-keyed, like `runtime-settings/AGENT_INITIATOR_HEADERS`.
            (expect (= "/responses" (:responses-path preset)))
            (expect (= {"X-Initiator" "agent"} (:llm-headers preset)))
            (expect (= {"temperature" 0.6 "top_p" 0.95} (:extra-body preset))))
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
          (expect (= ["Acme OAuth: run `vis-agent providers auth acme-oauth`."
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

;; Regression, issue #113: ordinary extension process calls from process-level
;; provider callbacks were redirected into the session-only jail and returned nil.
(defn- jail-wait
  "`sh.wait(secs)` for a jailed-shell handle — the HOST's own wait op through the
   jailed entry point, so the test waits exactly as an extension does and no copy
   of the poll loop lives here."
  [started]
  (:result (shell/jailed-shell nil {"op" "wait" "id" (get started "id") "seconds" 20})))

(def ^:private shell-provider-py
  "The ONE shelling-provider fixture. Both process-boundary cases load it — the
   session jail and the user's `shell` toggle — so the two cannot drift apart."
  "import vis\ndef detect():\n    result = vis.shell({'command': 'printf regular-shell'}).wait(20)\n    return {'token': result['stdout'], 'source': 'shell'}\nvis.extension(name='shell-provider', description='shell provider', providers=[vis.provider(id='shell-provider', label='Shell provider', detect_fn=detect)])")

(defdescribe
  python-extension-process-boundary-test
  (it
    "lets a provider callback spawn a native subprocess outside any session"
    (with-loaded
      {"process_provider.py"
       "import subprocess\nimport vis\ndef detect():\n    result = subprocess.run(['/bin/sh', '-c', 'printf extension-native'], capture_output=True, text=True, check=True)\n    return {'token': result.stdout, 'source': 'subprocess'}\nvis.extension(name='process-provider', description='process provider', providers=[vis.provider(id='process-provider', label='Process provider', detect_fn=detect)])"}
      (fn [_ _]
        (expect (= {:token "extension-native" :source :subprocess}
                   ((:provider/detect-fn (registry/provider-by-id :process-provider))))))))
  (it "keeps vis.shell unrestricted even while the invoking session jail is enabled"
      (with-loaded {"shell_provider.py" shell-provider-py}
                   (fn [_ _]
                     (let
                       [detect
                        (:provider/detect-fn (registry/provider-by-id :shell-provider))

                        env
                        {:session-id "jailed-session"
                         :security-policy {:sandbox true}
                         :jail-policy-fn
                         (fn []
                           (throw (ex-info "the regular extension shell touched the jail" {})))}]

                       (expect (= {:token "regular-shell" :source :shell} (detect)))
                       (binding [extension/*current-environment* env]
                         (expect (= {:token "regular-shell" :source :shell} (detect))))))))
  ;; Regression, toggle audit: the user-owned `shell` toggle unbinds the MODEL's
  ;; `shell` tool in the sandbox. An installed extension is a separate trust
  ;; layer with its own process door (`vis.shell`, and plain `subprocess` in its
  ;; own context), so flipping the toggle must not silently break an extension
  ;; that shells out.
  (it "keeps an extension's vis.shell working while the user's shell toggle is OFF"
      (with-loaded {"shell_provider.py" shell-provider-py}
                   (fn [_ _]
                     (let
                       [detect
                        (:provider/detect-fn (registry/provider-by-id :shell-provider))

                        before
                        (toggles/enabled? "shell")]

                       (try (toggles/set-enabled! "shell" false)
                            (expect (false? (toggles/enabled? "shell")))
                            (expect (= {:token "regular-shell" :source :shell} (detect)))
                            (finally (toggles/set-enabled! "shell" before)))))))
  (it
    "reads and validates the latest merged config for every jailed_shell spawn"
    (let
      [latest-root
       (temp-dir)

       _
       (spit (io/file latest-root "latest.txt") "latest")

       configs
       (atom [{"workspace" {"filesystem" [{"id" "latest-root"
                                           "path" (.getCanonicalPath latest-root)}]}
               "jail" {"enabled" true "filesystem" {"allow" ["latest-root"]}}}
              {"this_key_is_invalid" true}])

       loads
       (atom 0)]

      (with-redefs
        [config/load-config-raw (fn []
                                  (swap! loads inc)
                                  (let [value (first @configs)]
                                    (swap! configs subvec 1)
                                    value))]
        (let
          [first-run (:result (shell/jailed-shell nil
                                                  {"cwd" (.getCanonicalPath latest-root)
                                                   "command"
                                                   "test -r latest.txt && printf latest-policy"}))
           second-run (try (:result (shell/jailed-shell nil
                                                        {"cwd" (.getCanonicalPath latest-root)
                                                         "command" "printf must-not-run"}))
                           (catch Throwable t {"note" (ex-message t)}))]

          (expect (= "latest-policy" (get (jail-wait first-run) "stdout")))
          ;; The SECOND spawn re-reads config and finds it invalid, so it refuses.
          (expect (not= "must-not-run" (get second-run "stdout")))
          (expect (re-find #"Invalid Vis configuration" (str (get second-run "note"))))
          (expect (= 2 @loads))))))
  (it
    "keeps the latest and session-snapshot jail APIs distinct"
    (with-loaded
      {"jail.py"
       "import vis\ndef latest():\n    \"Use the latest jail.\"\n    return vis.jailed_shell({'command':'echo latest'})['stdout']\ndef session():\n    \"Use the session jail.\"\n    return vis.jailed_shell_session({'command':'echo session'}).wait(20)['stdout']\nvis.extension(name='jail', description='jail', alias='j', symbols=[vis.symbol(latest), vis.symbol(session)])"}
      (fn [_ _]
        (let
          [ext
           (registered "jail")

           latest
           (symbol-fn ext 'latest)

           session
           (symbol-fn ext 'session)

           seen
           (atom [])

           env
           {:session-id "session-1" :jail-policy-fn (constantly {:disabled? true})}]

          (with-redefs
            [shell/jailed-shell (fn [actual-env opts]
                                  (swap! seen conj [actual-env opts])
                                  {"stdout" "latest"})]
            (expect (= "latest" (:result (latest))))
            (expect (try (shell/session-jailed-shell nil {"command" "echo refused"})
                         false
                         (catch Throwable t
                           (str/includes?
                             (str t)
                             "jailed_shell_session is available only while handling a session"))))
            (binding [extension/*current-environment* env]
              (expect (= "session\n" (:result (session)))))
            ;; The session API did not cross the latest-config host callback.
            (expect (= [{"command" "echo latest"}] (mapv second @seen))))))))
  (it
    "unwraps the host tool ENVELOPE so a shelling extension crosses the boundary"
    ;; Regression, issue #96: `jailed-shell` returns an `extension/success`
    ;; envelope whose KEYWORD keys `->py` rejects, so every extension that
    ;; shelled out died with "STRINGS-ONLY boundary violation: non-string-key
    ;; :result" — blaming the extension for the framework's own payload.
    (with-loaded
      {"jail.py"
       "import vis\ndef run():\n    \"Shell out.\"\n    r = vis.jailed_shell({'command': 'echo hi'})\n    return [r['stdout'], r['stage'], sorted(r.keys())]\nvis.extension(name='jail', description='jail', alias='j', symbols=[vis.symbol(run)])"}
      (fn [_ _]
        (let
          [run
           (symbol-fn (registered "jail") 'run)

           env
           {:session-id "session-1" :jail-policy-fn (constantly {:disabled? true})}]

          (with-redefs
            [shell/jailed-shell (fn [_env opts]
                                  (extension/success {:result {"stdout" (get opts "command")
                                                               "stage" :run}
                                                      :op :shell
                                                      :metadata {:duration-ms 1}}))]
            (binding [extension/*current-environment* env]
              ;; Python sees the UNWRAPPED, deep-stringified `:result` only.
              (expect (= ["echo hi" "run" ["stage" "stdout"]] (:result (run))))))))))
  (it
    "hands an extension the SAME live handle the sandbox gets, not a raw dict"
    ;; Regression, handle audit: `__VisShell__` was applied only in the model's
    ;; sandbox, so an extension's only continuation was hand-authoring the
    ;; `{"op": "logs", "id": …}` grammar that the handle object replaced.
    (with-loaded
      {"jail.py"
       "import vis\ndef run():\n    \"Shell out.\"\n    sh = vis.jailed_shell({'command': 'echo hi', 'id': 'h'})\n    return [sh['id'], sh.logs(offset=0)['stage'], sh.wait(5)['stage'], sh.type('y')['stage'], sh.stop()['stage']]\nvis.extension(name='jail', description='jail', alias='j', symbols=[vis.symbol(run)])"}
      (fn [_ _]
        (let
          [run
           (symbol-fn (registered "jail") 'run)

           env
           {:session-id "session-1" :jail-policy-fn (constantly {:disabled? true})}]

          (with-redefs
            [shell/jailed-shell (fn [_env opts]
                                  (extension/success {:result {"id" (get opts "id")
                                                               "stage" (or (get opts "op") "run")}
                                                      :op :shell
                                                      :metadata {:duration-ms 1}}))]
            (binding [extension/*current-environment* env]
              ;; Every op reaches the ONE dispatch grammar, and every answer is
              ;; itself a handle.
              (expect (= ["h" "logs" "wait" "send" "stop"] (:result (run))))))))))
  (it
    "raises a failing host tool envelope instead of handing Python the envelope"
    (with-loaded
      {"jail.py"
       "import vis\ndef run():\n    \"Shell out.\"\n    return vis.jailed_shell({'command': 'nope'})\nvis.extension(name='jail', description='jail', alias='j', symbols=[vis.symbol(run)])"}
      (fn [_ _]
        (let
          [run
           (symbol-fn (registered "jail") 'run)

           env
           {:session-id "session-1" :jail-policy-fn (constantly {:disabled? true})}]

          (with-redefs
            [shell/jailed-shell (fn [_env _opts]
                                  (extension/failure {:error {:message "jail refused the command"}
                                                      :op :shell}))]
            (binding [extension/*current-environment* env]
              ;; The failure raises in the extension frame, so the symbol call
              ;; fails with the host's reason — never a bogus success carrying a
              ;; keyword-keyed envelope as its payload.
              (let [r (run)]
                (expect (false? (:success? r)))
                (expect (= "jail refused the command" (:message (:error r))))))))))))

(it
  "rejects a Python set so serial command ordering cannot be lost"
  (with-loaded
    {"jail.py"
     "import vis\ndef run():\n    return vis.jailed_shell({'echo first', 'echo second'})\nvis.extension(name='jail', description='jail', alias='j', symbols=[vis.symbol(run)])"}
    (fn [_ _]
      (let
        [run
         (symbol-fn (registered "jail") 'run)

         env
         {:session-id "session-1" :jail-policy-fn (constantly {:disabled? true})}]

        (expect (try (binding [extension/*current-environment* env]
                       (run))
                     false
                     (catch Throwable _ true)))))))

(defdescribe
  net-probe-report-test
  "The in-sandbox `network_probe` host callback: guard-only report over the
   gateway policy + registered filters. Pure — no socket, no egress."
  (let
    [pol
     (egress/compile-policy {:allowed-domains ["example.com"]
                             :rules [{:host "example.com" :access "read-only"}]})

     report
     (fn [method target & [headers-json body]]
       ;; redef INSIDE the thunk — lazytest runs `it` bodies after
       ;; the surrounding form, so a `with-redefs` wrapping the `it`s
       ;; would already be unwound.
       (with-redefs [pyx/session-network-policy (constantly pol)]
         (pyx/net-probe-report method target (or headers-json "") (or body ""))))]

    (it "allows a GET to an allowed host and sees a registered gateway filter"
        (try (egress/register-network-filter! ::npr
                                              (fn [_ctx]
                                                nil))
             (let [s (report "GET" "https://example.com/data")]
               (expect (re-find #"\"allow\":true" s)) ; tier1 allow
               (expect (re-find #"npr" s))            ; the registered filter shows up
               (expect (re-find #"\"phase\":\"http\"" s)))
             (finally (egress/unregister-network-filters-for-owner! ::npr))))
    (it "denies POST at tier-1 for a read-only host"
        (let [s (report "POST" "https://example.com/data")]
          (expect (re-find #"\"allow\":false" s))
          (expect (re-find #"not allowed for host example.com" s))))
    (it "denies a host outside the allow-list"
        (let [s (report nil "https://google.com/")]
          (expect (re-find #"host not permitted: google.com" s))))
    (it "preserves the query string in the probed ctx path"
        (let [s (report "GET" "https://example.com/get?token=abc")]
          (expect (re-find #"\?token=abc" s))))
    (it "feeds headers + body into the ctx so a filter can simulate rules on them"
        (try (egress/register-network-filter!
               ::npr-hb
               (fn [ctx]
                 (cond (= "Bearer leaked" (get (:headers ctx) "authorization"))
                       {:allow? false :reason "token exfil in header"}
                       (and (:body ctx) (clojure.string/includes? (:body ctx) "SECRET"))
                       {:allow? false :reason "secret in body"}
                       :else nil)))
             (let
               [s (report "GET" "https://example.com/data"
                          "{\"authorization\":\"Bearer leaked\"}" "")]
               (expect (re-find #"\"authorization\":\"Bearer leaked\"" s)) ; echoed back
               (expect (re-find #"token exfil in header" s)))
             (let [s (report "GET" "https://example.com/data" "{}" "has a SECRET inside")]
               (expect (re-find #"\"body\":\"has a SECRET inside\"" s))
               (expect (re-find #"secret in body" s)))
             (let [s (report "GET" "https://example.com/data" "{}" "clean")]
               (expect (re-find #"\"allow\":true" s)))
             (finally (egress/unregister-network-filters-for-owner! ::npr-hb))))
    (it "reports a parse error for a blank target"
        (let [s (report nil "   ")]
          (expect (re-find #"\"error\"" s))))))

;; =============================================================================
;; /reload re-hydrates feature toggles
;; =============================================================================

(defdescribe
  reload-slash-toggles-test
  "`/reload` is the ONE user-facing re-read of `vis.yml`. Toggles used to be
   hydrated only at process start (gateway `install-toggle-persistence!`, TUI
   `screen/run-chat!`), so `shell: false` in the YAML kept the tool live until a
   restart while `/reload` reported success."
  (it
    "the shell toggle edited in vis.yml applies after /reload — #64"
    (let
      [_
       shell/vis-extension

       before
       (toggles/enabled? "shell")]

      (toggles/set-value! "shell" true)
      (expect (true? (toggles/enabled? "shell")))
      (try (with-redefs
             [pyx/reload-python-extensions!
              (fn [& _]
                {:loaded 0 :failed 0})

              config/reload-config!
              (constantly {})

              config/current-config
              (constantly {})

              config/load-config-raw
              (constantly {"toggles" {"shell" false}})

              extension/run-reload-hooks!
              (constantly {})

              agents/reload!
              (constantly nil)

              prompt-templates/reload!
              (constantly [])]

             (let [res ((var pyx/reload-slash) {:channel/id :tui :command/argv []})]
               (expect (= :ok (:slash/status res)))))
           (expect (false? (toggles/enabled? "shell")))
           (finally (toggles/set-value! "shell" before))))))

;; =============================================================================
;; Human input — `vis.ask` blocks the extension until a channel answers
;; =============================================================================

(defn- answer-pending!
  "Wait for a human-input request titled `title` to show up, then run `answer-fn`
   on its id. Runs off-thread: `vis.ask` parks the calling thread.

   Mounts a no-op listener on the default channels first: a request that reaches
   no channel at all is refused as undeliverable, and a bare JVM has none — so
   without this the seam under test would never open a dialog to answer."
  [title answer-fn]
  (doseq [chan [:tui :app]]
    (channel-events/add-channel-event-listener! chan
                                                ::answering
                                                (fn [_])))
  (future (try (loop [n 0]
                 (if-let [req (first (filter #(= title (:title %)) (human-input/pending-requests)))]
                   (answer-fn (:id req))
                   (when (< n 400) (Thread/sleep 25) (recur (inc n)))))
               (finally (doseq [chan [:tui :app]]
                          (channel-events/remove-channel-event-listener! chan ::answering))))))

(def ^:private asker-py
  "
import vis

def ask_key():
    'Ask for deploy details.'
    answer = vis.ask(
        'Deploy',
        [{'name': 'env', 'label': 'Target', 'description': 'Where this deploy lands.',
          'type': 'select', 'options': ['staging', 'prod'], 'is_required': True},
         {'name': 'token', 'label': 'Deploy token', 'type': 'password'},
         {'name': 'dry', 'type': 'checkbox', 'default': True}],
        description='Pick a target',
        timeout_ms=20000,
    )
    return {'ok': bool(answer),
            'reason': answer.reason,
            'env': answer['env'],
            'dry': answer['dry'],
            'is_handle': answer['token'].startswith('vis-secret:'),
            'token': answer.reveal('token'),
            'forgotten': vis.forget(answer['token'])}

def ask_cancelled():
    'Ask for confirmation.'
    answer = vis.ask('Confirm', [{'name': 'yes', 'type': 'checkbox'}], timeout_ms=20000)
    return {'ok': bool(answer), 'reason': answer.reason, 'values': answer.values}

def ask_camel_case():
    'Ask with a camelCase field key.'
    try:
        vis.ask('Typo', [{'name': 'env', 'isRequired': True}], timeout_ms=5000)
    except BaseException as e:
        return {'raised': True, 'message': str(e)}
    return {'raised': False, 'message': ''}

def ask_validated():
    'Ask with Python validators.'
    def an_email(text):
        if '@' not in text:
            return 'must be an email address'

    answer = vis.ask(
        'Sign up',
        [{'name': 'email', 'label': 'Email', 'validate': an_email},
         {'name': 'again', 'label': 'Repeat it',
          'validate': lambda text, values:
              None if text == values['email'] else 'the two do not match'}],
        timeout_ms=20000,
    )
    return {'ok': bool(answer), 'email': answer['email']}

vis.extension(name='asker', description='asker', alias='a',
              symbols=[vis.symbol(ask_key), vis.symbol(ask_cancelled),
                       vis.symbol(ask_camel_case), vis.symbol(ask_validated)])
")

(defdescribe
  python-human-input-test
  (it
    "vis.ask pauses the extension, then returns typed values with the password kept opaque"
    (with-loaded
      {"asker.py" asker-py}
      (fn [_ _]
        (let
          [ask-key
           (symbol-fn (registered "asker") 'ask_key)

           drawn
           (atom nil)

           answered
           (answer-pending! "Deploy"
                            (fn [id]
                              (reset! drawn (human-input/pending-request id))
                              (human-input/submit! id {"env" "prod" "token" "hunter2"})))

           result
           (:result (binding [extension/*current-environment* {:session-id "sess-ask"}]
                      (ask-key)))]

          (expect (= {:is-accepted true} @answered))
          ;; The snake_case string spec a Python extension writes is
          ;; exactly what the dialog draws — name, label, description.
          (let [[env] (:fields @drawn)]
            (expect (= "env" (:name env)))
            (expect (= "Target" (:label env)))
            (expect (= "Where this deploy lands." (:description env)))
            (expect (true? (:is-required env))))
          (expect (= {"ok" true
                      "reason" "submitted"
                      "env" "prod"
                      "dry" true
                      "is_handle" true
                      "token" "hunter2"
                      "forgotten" true}
                     result))
          (expect (empty? (human-input/pending-requests)))))))
  (it
    "runs a Python validator on confirmation only, refusing with its own message"
    (with-loaded
      {"asker.py" asker-py}
      (fn [_ _]
        (let
          [ask-validated
           (symbol-fn (registered "asker") 'ask_validated)

           drawn
           (atom nil)

           refused
           (atom nil)

           answered
           (answer-pending! "Sign up"
                            (fn [id]
                              (reset! drawn (human-input/pending-request id))
                              ;; Both fields agree, so only the one-argument
                              ;; validator refuses this answer.
                              (reset! refused (human-input/submit! id
                                                                   {"email" "nope" "again" "nope"}))
                              (human-input/submit! id {"email" "a@b.c" "again" "a@b.c"})))

           result
           (:result (binding [extension/*current-environment* {:session-id "sess-ask"}]
                      (ask-validated)))]

          ;; The validator is a Python callable, invoked by the engine from the
          ;; submitting thread while `vis.ask` parks the extension's own thread
          ;; inside the host call — GraalPy releases the GIL for it.
          (expect (= {:is-accepted false :errors {"email" "must be an email address"}} @refused))
          ;; A refusal keeps the request open, so the next confirmation answers it.
          (expect (= {:is-accepted true} @answered))
          ;; And the function itself never reaches a channel.
          (expect (every? #(not (contains? % :validate)) (:fields @drawn)))
          (expect (= {"ok" true "email" "a@b.c"} result))
          (expect (empty? (human-input/pending-requests)))))))
  ;; Regression, issue #104: `vis.ask` raised from an extension SYMBOL was
  ;; reported to reach nobody — no dialog on any channel and not one
  ;; `human_input.request` in the gateway journal — on the theory that a symbol
  ;; invoked from a `python_execution` block has no ambient session binding, so
  ;; the request had to name its session through an undocumented `session_id=`
  ;; kwarg. Called the way the sandbox actually binds it (`wrap-extension` over
  ;; the live session env) the ask names its session with no kwarg at all, on
  ;; BOTH channels — and the session id is the only thing the gateway bridge
  ;; needs to turn the request into a session event.
  (it
    "an extension symbol's vis.ask names its session on every channel it reaches"
    (with-loaded
      {"asker.py" asker-py}
      (fn [_ {:keys [store]}]
        (let
          [seen
           (atom [])

           ask-key
           (get (extension/wrap-extension (registered "asker")
                                          {:session-id "sid-104" :db-info store})
                'ask_key)]

          (channel-events/add-channel-event-listener! :tui ::issue-104 #(swap! seen conj [:tui %]))
          (channel-events/add-channel-event-listener! :app ::issue-104 #(swap! seen conj [:app %]))
          (try (let
                 [answered (answer-pending!
                             "Deploy"
                             #(human-input/submit! % {"env" "prod" "token" "hunter2"}))]
                 (ask-key)
                 (expect (= {:is-accepted true} (deref answered 10000 ::never))))
               (let [opened (filterv #(= :human-input/request (:op (second %))) @seen)]
                 (expect (= [:tui :app] (mapv first opened)))
                 (expect (= ["sid-104" "sid-104"]
                            (mapv #(get-in (second %) [:request :session-id]) opened))))
               (finally (channel-events/remove-channel-event-listener! :tui ::issue-104)
                        (channel-events/remove-channel-event-listener! :app ::issue-104)))))))
  (it "a cancelled request returns a falsey answer instead of raising"
      (with-loaded {"asker.py" asker-py}
                   (fn [_ _]
                     (let
                       [ask-cancelled
                        (symbol-fn (registered "asker") 'ask_cancelled)

                        answered
                        (answer-pending! "Confirm" #(human-input/cancel! % "dismissed"))

                        result
                        (:result (binding [extension/*current-environment* {:session-id "sess-ask"}]
                                   (ask-cancelled)))]

                       (expect (true? @answered))
                       (expect (= {"ok" false "reason" "dismissed" "values" {}} result))))))
  (it "refuses a camelCase field key instead of leaving the field optional"
      ;; A key the engine did not recognise used to be dropped in silence, so
      ;; `isRequired` opened a dialog whose mandatory field was optional. The
      ;; request must never open at all.
      (with-loaded {"asker.py" asker-py}
                   (fn [_ _]
                     (let [result (:result ((symbol-fn (registered "asker") 'ask_camel_case)))]
                       (expect (true? (get result "raised")))
                       (expect (some? (re-find #"is_required" (str (get result "message")))))
                       (expect (empty? (human-input/pending-requests))))))))

;; =============================================================================
;; Hook callbacks run inside the caller's session env (issue #101)
;; =============================================================================

(def ^:private hook-asker-py
  "
import vis

def hook_prompt(env):
    answer = vis.ask('HookAsk', [{'name': 'go', 'type': 'checkbox'}], timeout_ms=20000)
    return 'hook:' + str(answer.reason)

vis.extension(name='hookasker', description='hookasker', alias='hk', prompt=hook_prompt)
")

(defdescribe python-hook-environment-test
             (it "a hook callback's vis.ask names the session the hook was invoked for"
                 ;; Regression, issue #101: a hook callback's `vis.ask` raised a
                 ;; session-less request that never reached the caller's session.
                 ;; Hook adapters (prompt/activation/ctx/slash/op) are HANDED the live env,
                 ;; but used to drop it: the Python callable ran with no
                 ;; `extension/*current-environment*`, so `vis.ask` raised a session-less
                 ;; request that the gateway bridge discards and `vis.shell` refused to run
                 ;; at all. The env has to reach the callable.
                 (with-loaded {"hookasker.py" hook-asker-py}
                              (fn [_ _]
                                (let
                                  [prompt-fn
                                   (:ext/prompt-fn (registered "hookasker"))

                                   drawn
                                   (atom nil)

                                   _
                                   (answer-pending! "HookAsk"
                                                    (fn [id]
                                                      (reset! drawn (human-input/pending-request
                                                                      id))
                                                      (human-input/submit! id {"go" true})))

                                   result
                                   (prompt-fn {:session-id "sess-hook"})]

                                  (expect (= "hook:submitted" result))
                                  (expect (= "sess-hook" (:session-id @drawn)))
                                  (expect (empty? (human-input/pending-requests))))))))

;; =============================================================================
;; Torn-down contexts heal instead of dying (issues #102, #103)
;; =============================================================================

(def ^:private rebuilder-py
  "import vis

def ping():
    '''await ping() -> str — answer pong.'''
    return 'pong'

def boom():
    '''await boom() -> str — always raises.'''
    raise ValueError('kaboom')

vis.extension(name='rebuilder', description='rebuilder', alias='rb',
              symbols=[vis.symbol(ping), vis.symbol(boom)])
")

(defdescribe python-extension-context-heal-test
             (it "a symbol captured before a reload keeps working after the rebuild"
                 ;; Sandbox bindings and cached session env rows capture the symbol fn ONCE,
                 ;; over the context that was alive then. A `/reload` builds new contexts and
                 ;; closes the old ones without re-binding anything, so every captured symbol
                 ;; died with "Context execution was cancelled" until the session restarted.
                 (with-fresh-loaded {"rebuilder.py" rebuilder-py}
                                    (fn [_ {:keys [ext-dir]}]
                                      (let [captured (symbol-fn (registered "rebuilder") 'ping)]
                                        (expect (= "pong" (:result (captured))))
                                        (pyx/reload-python-extensions! {:dirs [(str ext-dir)]})
                                        (expect (= "pong" (:result (captured))))))))
             (it "a symbol whose context was torn down rebuilds that context and answers"
                 ;; Nothing reloaded — the context itself is gone (host teardown, cancel).
                 ;; The loader's fingerprint gate would call a reload a no-op here, so the
                 ;; failing call has to rebuild its own file.
                 (with-fresh-loaded
                   {"rebuilder.py" rebuilder-py}
                   (fn [_ _]
                     (let
                       [captured
                        (symbol-fn (registered "rebuilder") 'ping)

                        ^Context dead
                        (:context (first (vals @@#'pyx/loaded)))]

                       (.close dead true)
                       (expect (= "pong" (:result (captured))))
                       ;; the heal re-registered a LIVE context, so the freshly resolved
                       ;; symbol goes straight through
                       (expect (not (identical? dead (:context (first (vals @@#'pyx/loaded))))))
                       (expect (= "pong"
                                  (:result ((symbol-fn (registered "rebuilder") 'ping)))))))))
             (it "context-dead? asks the context itself, it never matches error text"
                 ;; Liveness is a QUESTION for GraalVM, not a string to parse: a cheap
                 ;; `asValue` handshake returns on a live context and throws on a cancelled
                 ;; or closed one. A raised Python exception leaves the context alive.
                 (with-fresh-loaded
                   {"rebuilder.py" rebuilder-py}
                   (fn [_ _]
                     (let [^Context live (:context (first (vals @@#'pyx/loaded)))]
                       (expect (false? (#'pyx/context-dead? live)))
                       (expect (false? (:success? ((symbol-fn (registered "rebuilder") 'boom)))))
                       (expect (false? (#'pyx/context-dead? live)))
                       (.close live true)
                       (expect (true? (#'pyx/context-dead? live)))
                       (expect (true? (#'pyx/context-dead? nil)))))))
             (it "an ordinary Python error stays a failure and never rebuilds the context"
                 (with-fresh-loaded
                   {"rebuilder.py" rebuilder-py}
                   (fn [_ _]
                     (let
                       [before
                        (:context (first (vals @@#'pyx/loaded)))

                        res
                        ((symbol-fn (registered "rebuilder") 'boom))]

                       (expect (false? (:success? res)))
                       (expect (str/includes? (get-in res [:error :message]) "kaboom"))
                       (expect (identical? before (:context (first (vals @@#'pyx/loaded))))))))))

;; =============================================================================
;; Human-input form builders on the `vis` module
;; =============================================================================

(def ^:private forms-py
  "'''Form builder fixture: composes a request and checks it without asking.'''
import vis


def _at_least_8(secret):
    return None if len(secret) >= 8 else 'at least 8 characters'


def forms_report():
    '''await forms_report() -> {'ok', ...} - build a form and check it.'''
    good = vis.column(
        vis.heading('Target'),
        vis.paragraph('Staging pages nobody.'),
        vis.row(
            vis.select(
                'env',
                [vis.option('staging', 'Staging'), vis.option('prod')],
                is_required=True,
                default='prod',
            ),
            vis.slider('canary', min=0, max=100, step=5, default=10),
        ),
        vis.plaintext('who', label='Deployer'),
        vis.multiline('note'),
        vis.multiselect('regions', ['eu', 'us']),
        vis.otp('code', min_length=6, max_length=6),
        vis.checkbox('ack', is_required=True),
        vis.password('token'),
    )
    return {
        'ok': vis.check('Deploy', [good], submit_label='Ship it'),
        'kind': good['type'] + ':' + good['direction'],
        'ink': good['fields'][0],
        'slider_type': good['fields'][2]['fields'][1]['type'],
        'bad_option': vis.check('Deploy', [vis.select('env', [])]),
        'bad_track': vis.check('Deploy', [vis.slider('canary', min=5, max=2)]),
        'bad_names': vis.check('Deploy', [vis.plaintext('who'), vis.password('who')]),
        'bad_title': vis.check('', [vis.plaintext('who')]),
        'bad_key': vis.check('Deploy', [vis.plaintext('who', required=True)]),
        'validated': vis.check('Deploy', [
            vis.plaintext('who', validate=lambda text: None if text else 'who?'),
            vis.password('token', validate=[_at_least_8, lambda value, values: None]),
        ]),
        'bad_validator': vis.check('Deploy', [vis.plaintext('who', validate=lambda: None)]),
        'not_a_validator': vis.check('Deploy', [vis.plaintext('who', validate='nope')]),
    }


vis.extension(
    name='forms',
    description='Form builder fixture extension.',
    version='0.1.0',
    kind='integration',
    alias='forms',
    symbols=[vis.symbol(forms_report, tag='observation')],
)
")

(defdescribe
  human-input-builders-test
  (it
    "gives Python extensions the same form builders, checked by the engine itself"
    (with-loaded
      {"forms.py" forms-py}
      (fn [_ _]
        (let
          [ext
           (registered "forms")

           res
           (:result ((symbol-fn ext 'report)))]

          ;; the builders compose plain wire data: a group, and nameless ink
          (expect (= "group:column" (get res "kind")))
          (expect (= {"type" "heading" "text" "Target"} (get res "ink")))
          ;; `slider` is spelled so it never shadows the `range` builtin
          (expect (= "range" (get res "slider_type")))
          ;; a valid request is answered with None, never an exception
          (expect (nil? (get res "ok")))
          ;; and every mistake comes back as the engine's own one-line reason
          (expect (= "Invalid human-input field env: select needs at least one option"
                     (get res "bad_option")))
          (expect (= "Invalid human-input field canary: :max must be greater than :min"
                     (get res "bad_track")))
          (expect (= "Invalid human-input request: field names must be distinct"
                     (get res "bad_names")))
          (expect (str/includes? (get res "bad_title") "non-blank :title"))
          (expect (str/includes? (get res "bad_key") "unknown field key"))
          ;; a validator is CODE: it never crosses the wire, so only its
          ;; SHAPE is judged, and it is judged where it was written
          (expect (nil? (get res "validated")))
          (expect (= (str "a validate function takes the value, or the value "
                          "and every value - this one takes neither")
                     (get res "bad_validator")))
          (expect (str/includes? (get res "not_a_validator")
                                 "validate is a function, or a list of functions")))))))

;; Regression, issue #118: a Python provider could never publish live account
;; usage. Its `limits_fn` row came back with `:unlimited?` instead of the host
;; schema's `:is-unlimited`, so every row failed `::limit-row`, the whole report
;; was replaced by an invalid-report error, and the TUI footer showed
;; "limits: error (Provider limits fn returned an invalid report)".
(defdescribe python-provider-limits-test
             (it "a Python limits_fn yields a valid report the footer can render"
                 (with-loaded {"acme.py" provider-py}
                              (fn [_ _]
                                (provider-limits/flush-limits-cache!)
                                (let
                                  [report
                                   (provider-limits/provider-limits :acme)

                                   row
                                   (first (get-in report [:dynamic :limits]))]

                                  (expect (= :ok (:status report)))
                                  (expect (nil? (:error report)))
                                  ;; the host-schema boolean survives the Python boundary verbatim
                                  (expect (false? (:is-unlimited row)))
                                  (expect (nil? (:unlimited? row)))
                                  (expect (= :account (:scope row)))
                                  (expect (= :provider-api (:source row)))
                                  (expect (= 25.49 (:used row)))
                                  ;; and the footer's own formatter renders it
                                  (let [summary (limits-format/dynamic-summary report)]
                                    (expect (some? summary))
                                    (expect (str/includes? summary "Daily tokens"))))))))

;; Regression, issue #118 (same defect, general form): a Python provider's `is_*`
;; keys crossed under a hand-written `:<foo>?` rule guarded by a two-entry
;; allow-list, so every key the list did not name arrived as `:<foo>?` — a name
;; no host schema reads. That is how `:unlimited?` silenced the TUI footer, and
;; any NEW `is_*` key was one more silent miss. Keys now take `wire/engine-key`,
;; the single inverse of the gateway's `wire-key`, with no allow-list to forget.
(def ^:private provider-is-keys-py
  "'''Provider fixture pinning `is_*` key spelling across the boundary.'''
import vis


def _status():
    return {'is_authenticated': True, 'is_stale': False, 'source': 'env-var'}


vis.extension(
    name='provider-iskeys',
    description='is_* key spelling fixture.',
    providers=[
        vis.provider(
            id='iskeys',
            label='Is Keys',
            preset={'base_url': 'https://iskeys.test/v1', 'is_hidden': True},
            status_fn=_status,
        ),
    ],
)
")

(defdescribe python-provider-is-key-test
             (it "every `is_*` key crosses as `:is-*` — one mechanical inverse, no allow-list"
                 (with-loaded {"iskeys.py" provider-is-keys-py}
                              (fn [_ _]
                                (let
                                  [p
                                   (registry/provider-by-id :iskeys)

                                   status
                                   ((:provider/status-fn p))]

                                  (expect (true? (:is-authenticated status)))
                                  ;; the key no allow-list ever named
                                  (expect (false? (:is-stale status)))
                                  (expect (nil? (:stale? status)))
                                  (expect (true? (:is-hidden (:provider/preset p))))
                                  (expect (nil? (:hidden? (:provider/preset p)))))))))

;; =============================================================================
;; Freshness — only a process start and `/reload` put new bytes live
;; =============================================================================

(def ^:private sidecar-py
  "import vis

def peek():
    '''await peek() -> str — the sidecar module's version, read at call time.'''
    import sidecar_impl
    return sidecar_impl.VERSION

vis.extension(name='sidecar', description='sidecar', alias='sd',
              symbols=[vis.symbol(peek)])
")

(def ^:private sidecar-impl-py "VERSION = 'v1'\n")

(defdescribe
  extension-freshness-test
  (it "an implicit load never adopts an edited file - only /reload does"
      ;; `ensure-python-extensions-loaded!` is what a session env cache miss, an
      ;; env recycle and a `sub_loop` child env call. None of them is a human
      ;; act, so none of them may execute bytes that appeared after the last
      ;; load: the running process keeps serving the version it admitted.
      (with-fresh-loaded
        {"counter.py" counter-py}
        (fn [_ {:keys [ext-dir]}]
          (write-ext! ext-dir
                      "counter.py"
                      (str/replace counter-py "Counter fixture extension." "Counter v2."))
          (let [result (pyx/ensure-python-extensions-loaded! {:dirs [(str ext-dir)]})]
            (expect (false? (:changed? result)))
            (expect (= 1 (:loaded result)))
            (expect (= "Counter fixture extension." (:ext/description (registered "counter")))))
          (pyx/reload-python-extensions! {:dirs [(str ext-dir)]})
          (expect (= "Counter v2." (:ext/description (registered "counter")))))))
  (it "a process that has loaded nothing yet loads what is on disk"
      (let
        [ext-dir
         (temp-dir)

         store
         (ps/db-create-connection! :memory)]

        (write-ext! ext-dir "counter.py" counter-py)
        (reset! live-load nil)
        (binding [extension/*current-environment* {:db-info store}]
          (try
            ;; a freshly started process: nothing loaded here yet
            (reset! @#'pyx/last-fingerprint nil)
            (let [result (pyx/ensure-python-extensions-loaded! {:dirs [(str ext-dir)]})]
              (expect (true? (:changed? result)))
              (expect (= "Counter fixture extension." (:ext/description (registered "counter")))))
            (finally (reset! live-load nil)
                     (pyx/reload-python-extensions! {:dirs []})
                     (ps/db-dispose-connection! store))))))
  (it "a torn-down context never heals into edited bytes"
      ;; The heal path re-executes the extension with no human act in the chain,
      ;; so it may only re-run the bytes this process loaded.
      (with-fresh-loaded
        {"rebuilder.py" rebuilder-py}
        (fn [_ {:keys [ext-dir]}]
          (let
            [captured
             (symbol-fn (registered "rebuilder") 'ping)

             ^Context dead
             (:context (first (vals @@#'pyx/loaded)))]

            (write-ext! ext-dir "rebuilder.py" (str/replace rebuilder-py "'pong'" "'edited'"))
            (.close dead true)
            (let [res (captured)]
              (expect (not= "edited" (:result res)))
              (expect (not= "pong" (:result res))))
            (expect (identical? dead (:context (first (vals @@#'pyx/loaded)))))
            (pyx/reload-python-extensions! {:dirs [(str ext-dir)]})
            (expect (= "edited" (:result ((symbol-fn (registered "rebuilder") 'ping)))))))))
  (it "a module the extension imports lazily is frozen with its entry file"
      ;; Regression: identity was the ENTRY file's sha and `sys.path` pointed at
      ;; the LIVE directory, so `import sidecar_impl` inside a symbol read the
      ;; disk when the CALL ran. Editing a sidecar module - never the entry -
      ;; put new bytes in the trusted context with no `/reload` in the chain.
      (with-fresh-loaded
        {"sidecar/sidecar_impl.py" sidecar-impl-py "sidecar/extension.py" sidecar-py}
        (fn [_ {:keys [ext-dir]}]
          (write-ext! ext-dir "sidecar/sidecar_impl.py" (str/replace sidecar-impl-py "v1" "v2"))
          (expect (false? (:changed? (pyx/ensure-python-extensions-loaded! {:dirs [(str
                                                                                     ext-dir)]}))))
          (expect (= "v1" (:result ((symbol-fn (registered "sidecar") 'peek)))))
          (pyx/reload-python-extensions! {:dirs [(str ext-dir)]})
          (expect (= "v2" (:result ((symbol-fn (registered "sidecar") 'peek))))))))
  (it "a heal re-runs the frozen tree, never a sidecar module edited since"
      ;; A heal is the one re-execution with no human act behind it, so what it
      ;; proves unchanged is the WHOLE import root - an entry-only check let an
      ;; edited sidecar module ride into the rebuilt trusted context.
      (with-fresh-loaded
        {"sidecar/sidecar_impl.py" sidecar-impl-py "sidecar/extension.py" sidecar-py}
        (fn [_ {:keys [ext-dir]}]
          (let
            [captured
             (symbol-fn (registered "sidecar") 'peek)

             ^Context dead
             (:context (first (vals @@#'pyx/loaded)))]

            (write-ext! ext-dir "sidecar/sidecar_impl.py" (str/replace sidecar-impl-py "v1" "v2"))
            (.close dead true)
            (expect (not= "v2" (:result (captured))))
            (expect (identical? dead (:context (first (vals @@#'pyx/loaded)))))))))
  (it "a torn-down context heals from the frozen tree when nothing changed"
      (with-fresh-loaded
        {"sidecar/sidecar_impl.py" sidecar-impl-py "sidecar/extension.py" sidecar-py}
        (fn [_ _]
          (let
            [captured
             (symbol-fn (registered "sidecar") 'peek)

             ^Context dead
             (:context (first (vals @@#'pyx/loaded)))]

            (.close dead true)
            (expect (= "v1" (:result (captured))))
            (expect (not (identical? dead (:context (first (vals @@#'pyx/loaded)))))))))))

(defdescribe
  frozen-import-root-test
  (it "a symlinked module is frozen with the extension it belongs to"
      ;; Regression: the frozen copy's destination came from the CANONICAL file,
      ;; so a symlink pointing out of the import root produced a `../…` relative
      ;; path — `io/copy` wrote it OUTSIDE the snapshot (truncating whatever it
      ;; landed on) and the module the extension imports was missing from the
      ;; tree the load handed to `sys.path`. Linking a package one is working on
      ;; into `~/.vis/extensions` is ordinary extension development.
      (let
        [ext-dir
         (temp-dir)

         away
         (temp-dir)

         impl
         (io/file away "sidecar_impl.py")

         store
         (ps/db-create-connection! :memory)]

        (write-ext! ext-dir "sidecar/extension.py" sidecar-py)
        (spit impl sidecar-impl-py)
        (Files/createSymbolicLink (.toPath (io/file ext-dir "sidecar" "sidecar_impl.py"))
                                  (.toPath impl)
                                  (make-array FileAttribute 0))
        (reset! live-load nil)
        (binding [extension/*current-environment* {:db-info store}]
          (try (pyx/reload-python-extensions! {:dirs [(str ext-dir)]})
               (expect (= "v1" (:result ((symbol-fn (registered "sidecar") 'peek)))))
               (let [snap (io/file (:snapshot (first (vals @@#'pyx/loaded))))]
                 (expect (.isFile (io/file snap "sidecar_impl.py"))))
               (expect (= sidecar-impl-py (slurp impl)))
               (finally (reset! live-load nil)
                        (pyx/reload-python-extensions! {:dirs []})
                        (ps/db-dispose-connection! store)))))))
