(ns com.blockether.vis.internal.python.extensions-test
  "Python extension host — load fixture `.py` files into trusted CPython
   contexts and assert on the registry + adapter contracts. Boots real
   Python sessions (on the shared engine), no model in the loop."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [com.blockether.svar.core :as svar]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.channel.events :as channel-events]
            [com.blockether.vis.contract.activity :as activity-contract]
            [com.blockether.vis.internal.activity.core :as activity]
            [com.blockether.vis.internal.sandbox.egress-proxy :as egress]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.context.agents :as agents]
            [com.blockether.vis.internal.context.prompt :as prompt-context]
            [com.blockether.vis.internal.foundation.harness.discovery :as discovery]
            [com.blockether.vis.internal.foundation.harness.core :as harness]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.runtime-settings :as rt]
            [com.blockether.vis.internal.gateway.state :as gateway-state]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [com.blockether.vis.internal.view.core :as human-input]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.context.prompt-templates :as prompt-templates]
            [com.blockether.vis.internal.provider.auth :as pauth]
            [com.blockether.vis.internal.provider.limits :as provider-limits]
            [com.blockether.vis.internal.provider.service :as providers]
            [com.blockether.vis.internal.provider.limits-format :as limits-format]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.foundation.shell :as shell]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [com.blockether.vis.internal.python.worker :as worker]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.python.test-runner :as runner]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.sun.net.httpserver HttpServer HttpHandler HttpExchange]
           [java.io ByteArrayOutputStream]
           [java.net InetSocketAddress]
           [java.nio.charset StandardCharsets]
           [java.util.zip ZipEntry ZipOutputStream]
           [java.lang ProcessHandle]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

;; Harness

(defdescribe bootstrap-python-test
             ;; Regression, td-f2732d: JSON slash escaping made the embedded API invalid Python.
             (it "embeds division operators without JSON-only slash escapes"
                 (expect (str/includes? pyx/bootstrap-python " / 1000"))
                 (expect (not (str/includes? pyx/bootstrap-python "\\/")))))

(defn- temp-dir
  "A throwaway extension directory INSIDE the working directory, which is what
   the sandbox confines the interpreter to.

   The embedded interpreter's filesystem policy is PROCESS state, not session
   state: once any session is confined, the system temp folder is outside every
   later session's roots too, so an extension written there is one the loader
   genuinely may not read."
  ^java.io.File []
  (let [target (doto (java.io.File. "target") .mkdirs)]
    (.getAbsoluteFile (.toFile (Files/createTempDirectory (.toPath target)
                                                          "vis-pyext-test"
                                                          (make-array FileAttribute 0))))))

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

   An extension is its own Python sandbox and re-executing the 28 KB
   `extension_bootstrap.py` into a fresh one costs ~100ms, so the SAME sources
   are not loaded twice in a row: the load survives between tests and only the
   store is thrown away. That is all the isolation these tests need, because
   `vis.state` resolves the store through `*current-environment*` at CALL time
   — a fresh in-memory DB is a fresh state map even in a reused module.

   A body that loads, reloads or edits extensions itself moves the loader's own
   fingerprint; that invalidates the cache, so the next test reloads."
  [sources f]
  (let [{:keys [ext-dir result fingerprint]}
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
  (let [{:keys [ext-dir result]}
        (load-sources! sources)

        store
        (ps/db-create-connection! :memory)]

    (binding [extension/*current-environment* {:db-info store}]
      (try (f result {:ext-dir ext-dir :store store})
           (finally (reset! live-load nil)
                    (pyx/reload-python-extensions! {:dirs []})
                    (ps/db-dispose-connection! store))))))

(defn- with-shared-packages
  "Give both test workers one isolated package directory, never the user's installation."
  [f]
  (let [home
        (temp-dir)

        packages
        (.getCanonicalFile (io/file home ".vis/python/packages"))]

    (.mkdirs packages)
    (worker/stop-worker! worker/shared-key)
    (try (with-redefs [runtime/packages-dir (constantly (str packages))]
           (f packages))
         (finally (worker/stop-worker! worker/shared-key)
                  (doseq [file (reverse (file-seq home))]
                    (io/delete-file file true))))))

(defn- registered
  [ext-name]
  (some #(when (= ext-name (:ext/name %)) %) (extension/registered-extensions)))

(defn- symbol-fn
  [ext sym]
  (some #(when (= sym (:ext.symbol/symbol %)) (:ext.symbol/fn %))
        (get-in ext [:ext/engine :ext.engine/symbols])))

(defdescribe
  dependency-errors-keep-safe-diagnostics-test
  (it
    "keeps pip failure output and thrown causes without credentials (#183)"
    (doseq [throws? [false true]]
      (let
        [dir (temp-dir)
         output
         "PIP_MARKER: no matching wheel at https://user:fixture-secret@gateway.example.com/simple?auth=fixture-query"]

        (try (with-redefs-fn {#'pyx/freeze-root! (fn [_]
                                                   {:dir dir})
                              #'python-runtime/pip-install!
                              (fn [& _]
                                (if throws?
                                  (throw (ex-info output {:exit 19 :timeout? true}))
                                  {:exit 19 :out output :command ["private raw command"]}))}
               (fn []
                 (let [error (try (#'pyx/prepare-root! {:roots [] :dependencies ["fixture"]})
                                  nil
                                  (catch Exception e e))
                       text (str error (pr-str (ex-data error)))]

                   (expect (= 19 (:exit (ex-data error))))
                   (expect (= throws? (:timeout? (ex-data error))))
                   (expect (str/includes? text "PIP_MARKER"))
                   (doseq [secret ["fixture-secret" "fixture-query" "private raw command"]]
                     (expect (not (str/includes? text secret)))))))
             (finally (doseq [f (reverse (file-seq dir))]
                        (io/delete-file f true))))))))

(defdescribe
  cached-registration-helper-reload-test
  ;; Issue #176: characterize imported registration and call-time initialization.
  ;; The reported duplicate registration has not been reproduced with this fixture.
  (it
    "keeps namespace discovery and repeated calls working across sessions and reload"
    (with-fresh-loaded
      {"entry.py" "from registration_helper import register\nregister()\n"
       "other.py"
       "import blockether.vis.extension as vis\nvis.register(vis.Extension(name='other-registration', description='Independent registration'))\n"
       "registration_helper/__init__.py"
       "import blockether.vis.extension as vis\nclass Probe:\n    def ready(self):\n        'Return the registration owner.'\n        return vis._registration['spec']['name']\ndef register():\n    vis.register(vis.Extension(name='helper-reload', alias='helper_reload', description='Reload fixture', symbols=[vis.Symbol(Probe(), name='probe')]))\n"}
      (fn [result {:keys [ext-dir]}]
        (expect (= 2 (:loaded result)) (pr-str (pyx/load-failures)))
        (letfn
          [(with-session [f]
             (let [made
                   (ep/create-python-context {}
                                             (constantly [(.getCanonicalPath ^java.io.File
                                                                             ext-dir)])
                                             {:worker? true
                                              :jail-enabled? true
                                              :enabled? false
                                              :allowed-domains []
                                              :denied-domains []
                                              :exclude-domains []}
                                             nil)

                   ctx
                   (:python-context made)]

               (try (f {:python-context ctx :extensions (atom []) :active-extensions (atom [])})
                    (finally (ep/dispose-python-context! ctx)))))
           (check-session [env]
             (let [ext (registered "helper-reload")]
               (reset! (:extensions env) [ext])
               (lp/sync-active-extension-symbols! env [ext])
               (let
                 [out
                  (ep/run-python-block
                    (:python-context env)
                    "assert 'Return the registration owner' in doc('probe.ready')\nfor _ in range(3):\n    assert await probe.ready() == 'helper-reload'\nprint('ready')")]
                 (expect (nil? (:error out)) (pr-str out))
                 (expect (= "ready" (str/trim (:stdout out)))))))]
          (with-session (fn [first-env]
                          (with-session (fn [second-env]
                                          (doseq [_ (range 2)]
                                            (doseq [env [first-env second-env]]
                                              (check-session env))
                                            (let [reloaded (pyx/reload-python-extensions!
                                                             {:dirs [(str ext-dir)]})]
                                              (expect (= 0 (:failed reloaded))
                                                      (pr-str (pyx/load-failures)))
                                              (expect (= 2 (:loaded reloaded)))))
                                          (doseq [env [first-env second-env]]
                                            (check-session env)))))))))))

(defdescribe
  python-sdk-domain-isolation-test
  (it
    "keeps the package root inert and binds each extension to its own SDK module"
    (let [source
          (fn [name symbol]
            (str "import blockether.vis as package\n"
                 "import blockether.vis.extension as vis\n"
                 "def probe(value):\n"
                 "    \"Read this extension's state and API identity.\"\n"
                 "    if value is not None:\n"
                 "        vis.state['owner'] = value\n"
                 "    return [vis.__name__, vis._registration['spec']['name'], "
                 "vis.state.get('owner'), hasattr(package, '_host'), hasattr(package, 'state')]\n"
                 "vis.register(vis.Extension(name="
                 (pr-str name)
                 ", alias="
                 (pr-str symbol)
                 ", description='SDK binding', "
                 "symbols=[vis.Symbol(probe, name="
                 (pr-str symbol)
                 ")]))\n"))]
      (with-loaded {"first.py" (source "sdk-first" "sdk_first")
                    "second.py" (source "sdk-second" "sdk_second")}
                   (fn [result _]
                     (expect (= 2 (:loaded result)))
                     (expect (zero? (:failed result)))
                     (let [first-probe (symbol-fn (registered "sdk-first") 'sdk_first)
                           second-probe (symbol-fn (registered "sdk-second") 'sdk_second)]

                       (expect (= ["blockether.vis.extension" "sdk-first" "first" false false]
                                  (:result (first-probe "first"))))
                       (expect (= ["blockether.vis.extension" "sdk-second" "second" false false]
                                  (:result (second-probe "second"))))
                       (expect (= ["blockether.vis.extension" "sdk-first" "first" false false]
                                  (:result (first-probe nil))))))))))

(def ^:private counter-py
  "\"\"\"Counter fixture: tools + state + slash + prompt.\"\"\"
import blockether.vis.extension as vis


def counter_bump(by):
    \"\"\"await counter_bump(by) -> {\\\"count\\\"} — bump the counter.\"\"\"
    n = vis.state.get(\"count\", 0) + by
    vis.state[\"count\"] = n
    return {\"count\": n}


def counter_read():
    \"\"\"await counter_read() -> {\\\"count\\\"} — read the counter.\"\"\"
    vis.publish_activity(vis.ActivityPresentation(\"Counter\", \"Ready\", (vis.ActivityMarkdown(\"**Counter** ready\"),)))
    return {\"count\": vis.state.get(\"count\", 0)}


def counter_boom():
    \"\"\"await counter_boom() -> never — always raises.\"\"\"
    raise ValueError(\"kaboom\")


def _slash(ctx):
    return vis.ok(\"count is \" + str(vis.state.get(\"count\", 0)), data={\"args\": ctx[\"args\"]})


vis.register(vis.Extension(
    name=\"counter\",
    description=\"Counter fixture extension.\",
    version=\"0.1.0\",
    kind=\"integration\",
    alias=\"counter\",
    symbols=[
        vis.Symbol(counter_bump, tag=\"mutation\"),
        vis.Symbol(counter_read, tag=\"observation\", activity=vis.Activity(presenter=\"tests\", label=\"checking counter\", render=lambda phase, **_: vis.ActivityPresentation('Counter', 'checking counter', (vis.ActivityText(phase),)))),
        vis.Symbol(counter_boom, tag=\"observation\", is_hidden=True),
    ],
    prompt=\"counter_ surface active.\",
    slash_commands=[vis.SlashCommand(\"count\", _slash, doc=\"Show the counter.\")],
))
")

(defdescribe
  sdk-activity-lifecycle-test
  (it
    "uses declared presentation but engine-owned success and failure, through the published runtime"
    (with-loaded
      {"counter.py" counter-py}
      (fn [_ _]
        (expect (= [] (pyx/load-failures)))
        (let [ext
              (registered "counter")

              entries
              (get-in ext [:ext/engine :ext.engine/symbols])

              read-entry
              (second entries)

              boom-entry
              (last entries)

              events
              (atom [])]

          (binding [extension/*tool-event-sink* #(swap! events conj %)]
            (expect (= {"count" 0 "op" "counter_counter_read"}
                       (extension/invoke-symbol-wrapper ext read-entry [] {})))
            (expect (try (extension/invoke-symbol-wrapper ext boom-entry [] {})
                         false
                         (catch Exception _ true))))
          (let [projection (-> @events
                               activity/replay
                               activity/presentation)]
            (expect (= 7 (count @events)))
            (expect (= ["start" "**Counter** ready" "success"]
                       (mapv #(get-in % [:presentation "content" 0 "text"])
                             (filter #(= :content (:phase %)) @events))))
            (expect (= {"headline" "Counter"
                        "summary" "checking counter"
                        "content" [{"type" "text" "text" "success"}]}
                       (get-in projection [:rows 0 :presentation])))
            (expect (= "failed" (:state projection)))
            (expect (= {:running 0 :succeeded 1 :failed 1 :cancelled 0} (:counts projection)))
            (expect (= "tests" (get-in projection [:rows 0 :presenter])))
            (expect (= "observation" (get-in projection [:rows 0 :signal])))
            (expect (= "checking counter" (get-in projection [:rows 0 :summary])))
            (expect (activity-contract/valid-projection? projection))))))))

;; Loading + registry

(defdescribe load-and-register-test
             (it "loads a file and keeps each flat function's exact declared public name"
                 (with-loaded {"counter.py" counter-py}
                              (fn [result _]
                                (expect (= {:loaded 1 :failed 0 :changed? true} result))
                                (let [ext (registered "counter")]
                                  (expect (some? ext))
                                  (expect (= 'counter (get-in ext [:ext/engine :ext.engine/alias])))
                                  (expect (= '[counter_bump counter_read counter_boom]
                                             (mapv :ext.symbol/symbol
                                                   (get-in ext [:ext/engine :ext.engine/symbols]))))
                                  ;; docstring became the model-facing doc; arglists carry the
                                  ;; real Python parameter names
                                  (let [bump (first (get-in ext [:ext/engine :ext.engine/symbols]))]
                                    (expect (str/includes? (:ext.symbol/doc bump)
                                                           "bump the counter"))
                                    (expect (= ['[by]] (:ext.symbol/arglists bump)))
                                    (expect (= :mutation (:ext.symbol/tag bump))))
                                  ;; is_hidden=True -> the :ext.symbol/hidden? predicate key
                                  (let [boom (last (get-in ext [:ext/engine :ext.engine/symbols]))]
                                    (expect (= 'counter_boom (:ext.symbol/symbol boom)))
                                    (expect (true? (:ext.symbol/hidden? boom))))))))
             (it "is idempotent: an unchanged scan is a no-op"
                 (with-loaded {"counter.py" counter-py}
                              (fn [_ {:keys [ext-dir]}]
                                (let [again (pyx/load-python-extensions! {:dirs [(str ext-dir)]})]
                                  (expect (= false (:changed? again)))
                                  (expect (= 1 (:loaded again))))))))

;; Regression, user report: an extension reading Python's conventional `__file__`
;; global failed during loading before it could call `vis.Extension(...)`.
(defdescribe
  extension-entry-module-globals-test
  (it
    "binds the canonical entry file and conventional module globals before evaluation"
    (with-loaded
      {"entry_globals.py"
       (str
         "import os
import blockether.vis.extension as vis
"
         "ENTRY_FILE = os.path.realpath(__file__)
"
         "CONVENTIONAL = {name: name in globals() for name in "
         "('__name__', '__file__', '__cached__', '__loader__', '__package__', '__spec__', '__builtins__')}
"
         "def entry_metadata():
"
         "    \"Return entry-module metadata.\"
"
         "    return {'file': ENTRY_FILE, 'globals': CONVENTIONAL}
"
         "vis.register(vis.Extension(name='entry-globals', description='entry globals', alias='entry', "
         "symbols=[vis.Symbol(entry_metadata)]))
")}
      (fn [result {:keys [ext-dir]}]
        (expect (= {:loaded 1 :failed 0 :changed? true} result))
        (let [metadata (:result ((symbol-fn (registered "entry-globals") 'entry_metadata)))]
          (expect (= (.getCanonicalPath (io/file ext-dir "entry_globals.py"))
                     (get metadata "file")))
          (expect (every? true? (vals (get metadata "globals")))))))))

(def ^:private object-namespace-py
  "\"\"\"Object namespace fixture.\"\"\"
import blockether.vis.extension as vis

from dataclasses import dataclass

@dataclass(frozen=True, slots=True)
class BuildStatus:
    state: str

class Jenkins:
    def poll(self, job, number=None, wait=0):
        \"\"\"Poll one Jenkins build.\"\"\"
        return BuildStatus(f'{job}:{number}:{wait}')

    @vis.method(tag='mutation')
    def deploy_status(self, job, number=None):
        \"\"\"Inspect deployment stages for one build.\"\"\"
        return {'job': job, 'number': number}

    def _credential(self):
        \"\"\"Must not cross the namespace boundary.\"\"\"
        return 'secret'

jenkins = Jenkins()
vis.register(vis.Extension(
    name='glms', description='Object namespace fixture.', kind='integration', alias='glms',
    symbols=[vis.Symbol(jenkins, name='glms_jenkins')],
))
")

(defdescribe
  python-object-activity-test
  (it "keeps typed runtime values intact while redacting only Activity arguments and results"
      (with-loaded {"object_namespace.py" object-namespace-py}
                   (fn [_ _]
                     (doseq [job ["job" "password=fixture-runtime-secret"]]
                       (let [ext (registered "glms")
                             entry (first (get-in ext [:ext/engine :ext.engine/symbols]))
                             events (atom [])
                             value (binding [extension/*tool-event-sink* #(swap! events conj %)]
                                     (extension/invoke-symbol-wrapper ext entry [job 4 0] {}))
                             projection (-> @events
                                            activity/replay
                                            activity/presentation)]

                         (expect (= "BuildStatus" (get value "__vis_object__")))
                         (expect (= (str job ":4:0") (get-in value ["__vis_attrs__" "state"])))
                         (expect (= 2 (count @events)))
                         (expect (= (pr-str {"state"
                                             (if (= job "job") "job:4:0" "password=[REDACTED]")})
                                    (get-in projection [:rows 0 :result-summary])))
                         (expect (not (str/includes? (pr-str projection) "__vis_")))
                         (expect (not (str/includes? (pr-str projection) "fixture-runtime-secret")))
                         (expect (activity-contract/valid-projection? projection))))))))

(defdescribe python-object-namespace-test
             ;; Regression, issue #166: object integrations had to flatten every method into a
             ;; separately prefixed function, or expose a raw object that bypassed tool execution.
             (it "exports only public methods under the exact declared sandbox namespace"
                 (with-loaded
                   {"object_namespace.py" object-namespace-py}
                   (fn [result _]
                     (expect (= {:loaded 1 :failed 0 :changed? true} result))
                     (let [ext
                           (registered "glms")

                           entries
                           (get-in ext [:ext/engine :ext.engine/symbols])

                           poll
                           (symbol-fn ext 'glms_jenkins.poll)

                           deploy
                           (symbol-fn ext 'glms_jenkins.deploy_status)]

                       (expect (= '[glms_jenkins.poll glms_jenkins.deploy_status]
                                  (mapv :ext.symbol/symbol entries)))
                       (expect (= [:observation :mutation] (mapv :ext.symbol/tag entries)))
                       (expect (= [['[job number wait]] ['[job number]]]
                                  (mapv :ext.symbol/arglists entries)))
                       (expect (nil? (symbol-fn ext 'glms_jenkins._credential)))
                       (let [value (:result (poll "typed" nil 0))]
                         (expect (= "BuildStatus" (get value "__vis_object__")))
                         (expect (= "typed:None:0" (get-in value ["__vis_attrs__" "state"]))))
                       (expect (= "build" (get-in (deploy "build" 42) [:result "job"]))))))))

(def ^:private recursive-object-namespace-py
  "import blockether.vis.extension as vis

class IssueNamespace:
    def find_issue(self, query):
        \"\"\"Find one issue.\"\"\"
        return {'query': query}

    @vis.method(tag='mutation')
    def create_issue(self, title):
        \"\"\"Create one issue.\"\"\"
        return {'title': title}

    def _token(self):
        return 'private'

class VisNamespace:
    def __init__(self):
        self.issues = IssueNamespace()

class UberworkspaceNamespace:
    def __init__(self):
        self.vis = VisNamespace()

    def ping(self):
        \"\"\"Check the workspace.\"\"\"
        return 'pong'

vis.register(vis.Extension(
    name='uberworkspace', description='Recursive object namespace fixture.',
    kind='integration', alias='uberworkspace',
    symbols=[vis.Symbol(UberworkspaceNamespace(), name='uberworkspace')],
))
")

;; Regression, issue #171: object namespaces stopped after one level, forcing
;; extension authors to flatten nested capability objects into method names.
(defdescribe
  python-recursive-object-namespace-test
  (it
    "exports nested methods as managed dotted tools with readable namespace objects"
    (with-loaded
      {"recursive_object_namespace.py" recursive-object-namespace-py}
      (fn [result _]
        (expect (= {:loaded 1 :failed 0 :changed? true} result))
        (let [ext
              (registered "uberworkspace")

              entries
              (get-in ext [:ext/engine :ext.engine/symbols])

              made
              (ep/create-python-context {} nil {:worker? true} nil)

              ctx
              (:python-context made)

              env
              {:python-context ctx :extensions (atom [ext]) :active-extensions (atom [])}]

          (expect (= '[uberworkspace.ping uberworkspace.vis.issues.find_issue
                       uberworkspace.vis.issues.create_issue]
                     (mapv :ext.symbol/symbol entries)))
          (expect (= [:observation :observation :mutation] (mapv :ext.symbol/tag entries)))
          (expect (nil? (symbol-fn ext 'uberworkspace.vis.issues._token)))
          (try (lp/sync-active-extension-symbols! env [ext])
               (let
                 [answer
                  (ep/run-python-block
                    ctx
                    (str/join
                      "\n"
                      ["print(repr(uberworkspace))" "print(repr(uberworkspace.vis.issues))"
                       "print(await uberworkspace.ping())"
                       "print((await uberworkspace.vis.issues.find_issue('171'))['query'])"
                       "print((await uberworkspace.vis.issues.create_issue('nested'))['title'])"
                       "print([item.name for item in apropos(r'^uberworkspace\\.')])"
                       "print('Find one issue.' in doc('uberworkspace.vis.issues.find_issue'))"]))

                  out
                  (or (:stdout answer) "")]

                 (expect (nil? (:error answer)))
                 (expect (str/includes? out "<vis namespace 'uberworkspace': ping, vis>"))
                 (expect (str/includes?
                           out
                           "<vis namespace 'uberworkspace.vis.issues': create_issue, find_issue>"))
                 (expect (str/includes? out "pong\n171\nnested\n"))
                 (expect (str/includes? out "uberworkspace.vis.issues.find_issue"))
                 (expect (str/ends-with? (str/trim out) "True")))
               (lp/sync-active-extension-symbols! env [])
               (expect (= {:stdout "False\n"}
                          (ep/run-python-block ctx "print('uberworkspace' in globals())")))
               (finally (ep/dispose-python-context! ctx))))))))

(def ^:private invalid-recursive-object-namespace-py
  "import blockether.vis.extension as vis

class Leaf:
    def run(self):
        \"\"\"Run the leaf.\"\"\"
        return True

cycle = Leaf()
cycle.child = cycle
shared = Leaf()
class Repeated:
    def __init__(self):
        self.first = shared
        self.second = shared
class Unsupported:
    def __init__(self):
        self.version = 171

errors = []
for name, value in [('cycle', cycle), ('repeated', Repeated()), ('unsupported', Unsupported())]:
    try:
        vis.Symbol(value, name=name)
    except ValueError as error:
        errors.append(str(error))
raise RuntimeError(' | '.join(errors))
")

;; Regression, issue #171: recursive discovery could otherwise loop forever,
;; expose one object twice, or silently discard a public data attribute.
(defdescribe python-recursive-object-namespace-validation-test
             (it "rejects cycles, repeated references, and unsupported public values by path"
                 (with-loaded {"invalid_recursive_object_namespace.py"
                               invalid-recursive-object-namespace-py}
                              (fn [result _]
                                (let [error (:error (first (pyx/load-failures)))]
                                  (expect (= 1 (:failed result)))
                                  (expect (str/includes? error "cycle.child"))
                                  (expect (str/includes? error "repeated.second"))
                                  (expect (str/includes? error "repeated.first"))
                                  (expect (str/includes? error "unsupported.version"))
                                  (expect (str/includes? error "int")))))))

(defdescribe
  python-object-namespace-sandbox-test
  ;; Regression, issue #166: namespaced methods could not be called through the
  ;; sandbox's deferred tool machinery or discovered as dotted public symbols.
  (it
    "calls, introspects, types, and removes only declared namespace methods"
    (with-loaded
      {"object_namespace.py" object-namespace-py}
      (fn [_ _]
        (let [ext
              (registered "glms")

              made
              (ep/create-python-context {} nil {:worker? true} nil)

              ctx
              (:python-context made)

              env
              {:python-context ctx :extensions (atom [ext]) :active-extensions (atom [])}]

          (try (lp/sync-active-extension-symbols! env [ext])
               (let [result
                     (ep/run-python-block
                       ctx
                       (str "import inspect\n" "status = await glms_jenkins.poll('job', number=4)\n"
                            "try:\n    glms_jenkins._credential\n    private = 'leaked'\n"
                            "except AttributeError:\n    private = 'safe'\n"
                            "print(type(status).__name__, status.state, private, "
                            "str(inspect.signature(glms_jenkins.poll)), "
                            "dir(glms_jenkins), "
                            "'Poll one Jenkins build.' in (glms_jenkins.poll.__doc__ or ''))"))

                     out
                     (or (:stdout result) "")]

                 (expect (nil? (:error result)))
                 (expect (str/includes? out "BuildStatus job:4:0 safe"))
                 (expect (str/includes? out "(job, number=None, wait=Ellipsis)"))
                 (expect (str/includes? out "['deploy_status', 'poll']"))
                 (expect (str/ends-with? (str/trim out) "True"))
                 (lp/sync-active-extension-symbols! env [])
                 (expect (= {:stdout "False\n"}
                            (ep/run-python-block ctx "print('glms_jenkins' in globals())"))))
               (finally (ep/dispose-python-context! ctx))))))))

(def ^:private trusted-process-py
  "import ctypes
import os
import sys
import types
import blockether.vis.extension as vis


def session_probe(path):
    \"\"\"Read through the trusted host API and convert a local native value.\"\"\"
    marker = sys.modules.setdefault('vis_session_probe', types.SimpleNamespace(value=0))
    marker.value += 2
    number = ctypes.c_int(42)
    native = ctypes.cast(ctypes.pointer(number), ctypes.POINTER(ctypes.c_int)).contents.value
    text = vis.fs.read_text(path)
    return {'pid': os.getpid(), 'value': marker.value, 'text': text, 'native': native}


vis.register(vis.Extension(
    name='trusted-process',
    description='Trusted extension process probe.',
    alias='trusted_process',
    symbols=[vis.Symbol(session_probe)],
))
")

(defdescribe
  python-extension-session-interpreter-test
  (it
    "keeps trusted native execution and host permissions outside the owning sandbox"
    (with-loaded
      {"trusted_process.py" trusted-process-py}
      (fn [_ _]
        (let [sandbox-root
              (temp-dir)

              outside-root
              (temp-dir)

              outside
              (io/file outside-root "trusted.txt")

              _
              (spit outside "trusted")

              ext
              (registered "trusted-process")

              registered-before
              ext

              made
              (ep/create-python-context {}
                                        (fn []
                                          [(.getCanonicalPath sandbox-root)])
                                        {:worker? true
                                         :jail-enabled? true
                                         :enabled? false
                                         :allowed-domains []
                                         :denied-domains []
                                         :exclude-domains []}
                                        nil)

              ctx
              (:python-context made)

              env
              {:python-context ctx
               :session-id "trusted-process-session"
               :extensions (atom [ext])
               :active-extensions (atom [])}]

          (try
            (lp/sync-active-extension-symbols! env [ext])
            (let
              [result
               (ep/run-python-block
                 ctx
                 (str
                   "import os, sys, types\n"
                   "sandbox_pid = os.getpid()\n"
                   "sys.modules['vis_session_probe'] = types.SimpleNamespace(value=40)\n"
                   "try:\n"
                   "    open("
                   (pr-str (.getCanonicalPath outside))
                   ", encoding='utf-8').read()\n"
                   "    sandbox_access = 'open'\n"
                   "except Exception:\n"
                   "    sandbox_access = 'refused'\n"
                   "answer = await session_probe("
                   (pr-str (.getCanonicalPath outside))
                   ")\n"
                   "second = await session_probe("
                   (pr-str (.getCanonicalPath outside))
                   ")\n"
                   "print(sandbox_pid, answer['pid'], answer['value'], answer['text'], sandbox_access, "
                   "answer['native'], second['value'], sys.modules['vis_session_probe'].value)"))

               words
               (str/split (str/trim (or (:stdout result) "")) #"\s+")]

              (expect (nil? (:error result)) (pr-str result))
              (expect (not= (first words) (second words)))
              (expect (= ["2" "trusted" "refused" "42" "4" "40"] (subvec (vec words) 2)))
              (expect (identical? registered-before (registered "trusted-process")))
              (let [local-ctx
                    (:context (get @@#'pyx/session-contexts [ctx "trusted-process"]))

                    workers
                    @@#'worker/workers]

                (expect (some? local-ctx))
                (expect (not (contains? @(get-in workers [ctx :peer :host-sessions]) local-ctx)))
                (expect (contains?
                          @(get-in workers [(worker/extension-worker-key ctx) :peer :host-sessions])
                          local-ctx))))
            (finally (ep/dispose-python-context! ctx)))
          (expect (not (worker/worker-live? (worker/extension-worker-key ctx))))
          (expect (not (contains? @@#'pyx/session-contexts [ctx "trusted-process"]))))))))

;; Tool adapter — envelope semantics

(defdescribe tool-envelope-test
             (it "return value = success payload"
                 (with-loaded {"counter.py" counter-py}
                              (fn [_ _]
                                (let [bump
                                      (symbol-fn (registered "counter") 'counter_bump)

                                      result
                                      (bump 5)]

                                  (expect (extension/envelope-success? result))
                                  (expect (= 5 (get-in result [:result "count"])))))))
             (it "a raised Python exception = failure envelope with the Python message"
                 (with-loaded {"counter.py" counter-py}
                              (fn [_ _]
                                (let [boom
                                      (symbol-fn (registered "counter") 'counter_boom)

                                      result
                                      (boom)]

                                  (expect (extension/envelope-failure? result))
                                  (expect (str/includes? (get-in result [:error :message])
                                                         "kaboom")))))))

(def ^:private kwargs-py
  "\"\"\"Kwargs fixture: keyword arguments must survive the sandbox fold.\"\"\"
import blockether.vis.extension as vis


def kw_probe(name, mode=\"plain\", is_deep=False):
    \"\"\"await kw_probe(name, mode, is_deep) -> {\\\"mode\\\"} — echo how the args arrived.\"\"\"
    return {\"name\": name, \"mode\": mode, \"is_deep\": is_deep}


def kw_mapping(payload):
    \"\"\"await kw_mapping(payload) -> {\\\"payload\\\"} — echo a mapping positional.\"\"\"
    return {\"payload\": payload}


vis.register(vis.Extension(
    name=\"kwargs\",
    description=\"Keyword-argument fixture extension.\",
    version=\"0.1.0\",
    kind=\"integration\",
    alias=\"kw\",
    symbols=[
        vis.Symbol(kw_probe, tag=\"observation\"),
        vis.Symbol(kw_mapping, tag=\"observation\"),
    ],
))
")

(defdescribe
  python-kwargs-test
  (it "keyword args folded into ONE trailing map are re-expanded onto the signature — #83"
      (with-loaded {"kwargs.py" kwargs-py}
                   (fn [_ _]
                     (let [probe
                           (symbol-fn (registered "kwargs") 'kw_probe)

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
                     (let [result ((symbol-fn (registered "kwargs") 'kw_probe) "g" "deep")]
                       (expect (= "deep" (get-in result [:result "mode"])))
                       (expect (false? (get-in result [:result "is_deep"])))))))
  (it "a genuine mapping positional stays ONE argument"
      (with-loaded {"kwargs.py" kwargs-py}
                   (fn [_ _]
                     (let [result ((symbol-fn (registered "kwargs") 'kw_mapping) {"a" 1 "b" 2})]
                       (expect (extension/envelope-success? result))
                       (expect (= 1 (get-in result [:result "payload" "a"])))
                       (expect (= 2 (get-in result [:result "payload" "b"]))))))))

;; Declared host environment -- issue #129

(def ^:private env-py
  "\"\"\"Declared host env allowlist fixture.\"\"\"
import os
import blockether.vis.extension as vis


def env_probe():
    \"\"\"await env_probe() -> {\"has_path\", \"has_unset\", \"host_path\", \"host_default\"} -- report declared env reachability.\"\"\"
    return {\"has_path\": os.environ.get(\"PATH\") is not None,
            \"has_unset\": os.environ.get(\"VIS_TEST_NEVER_SET_129\") is not None,
            \"host_path\": vis.host_env(\"PATH\") is not None,
            \"host_default\": vis.host_env(\"VIS_TEST_NEVER_SET_129\", \"fallback\")}


vis.register(vis.Extension(
    name=\"env-allowlist\",
    description=\"Declared env allowlist fixture.\",
    version=\"0.1.0\",
    kind=\"integration\",
    alias=\"env\",
    symbols=[vis.Symbol(env_probe, tag=\"observation\")],
    env=[\"PATH\", \"VIS_TEST_NEVER_SET_129\"],
))
")

(def ^:private env-bad-py
  "\"\"\"Bad env= fixture -- not a list.\"\"\"
import blockether.vis.extension as vis


vis.register(vis.Extension(name=\"env-bad\", description=\"bad env fixture.\", env=\"PATH\"))
")

;; Regression, issue #129: a Python extension could not read host env vars.
;; `vis.Extension(...)` did not accept `env=`, so any extension declaring the
;; variables it needed raised at load and its provider was silently absent.
;; Now `env=` declares an allowlist the host resolves from the process
;; environment (`System/getenv`), and the declaration lands on `:ext/env` so
;; doctor/the TUI can surface a declared-but-unset variable instead of failing
;; silently.
(defdescribe
  declared-host-env-test
  (it "resolve-declared-env reads the process environment and drops unset/malformed names"
      ;; Local config and dotenv values must never reach assertion diagnostics.
      (with-redefs [config/current-config (constantly {})]
        (binding [config/*extension-dotenv-path* nil
                  config/*extension-dotenv-local-path* nil
                  config/*extension-getenv* {"PATH" "fixture-path"}]

          (expect (= {"PATH" "fixture-path"} (pyx/resolve-declared-env ["PATH"])))
          (expect (= {} (pyx/resolve-declared-env ["VIS_TEST_NEVER_SET_129"])))
          (expect (= {"PATH" "fixture-path"}
                     (pyx/resolve-declared-env ["9BAD" "" "BAD-NAME" "PATH"])))
          (expect (= {} (pyx/resolve-declared-env nil)))
          (expect (= {} (pyx/resolve-declared-env []))))))
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
                       (let [probe (symbol-fn ext 'env_probe)
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

;; State — durable across reloads

(defdescribe state-durability-test
             (it "vis.state survives a full reload (fresh contexts, same DB)"
                 (with-loaded {"counter.py" counter-py}
                              (fn [_ {:keys [ext-dir]}]
                                (let [bump (symbol-fn (registered "counter") 'counter_bump)]
                                  (expect (= 7 (get-in (bump 7) [:result "count"])))
                                  ;; full teardown + fresh contexts
                                  (pyx/reload-python-extensions! {:dirs [(str ext-dir)]})
                                  (let [read (symbol-fn (registered "counter") 'counter_read)]
                                    (expect (= 7 (get-in (read) [:result "count"])))))))))

(def ^:private state-mapping-py
  "\"\"\"vis.state as a whole mapping fixture.\"\"\"
import blockether.vis.extension as vis


def state_probe():
    \"\"\"await state_probe() -> {...} — drive vis.state through the mapping surface.\"\"\"
    vis.state.update({\"repo\": \"acme/widgets\", \"count\": 2})
    vis.state.setdefault(\"count\", 99)
    vis.state.setdefault(\"branch\", \"main\")
    vis.state[\"ghost\"] = None
    probe = {
        \"keys\": sorted(vis.state),
        \"len\": len(vis.state),
        \"values\": sorted(str(v) for v in vis.state.values()),
        \"popped\": vis.state.pop(\"count\"),
        \"default\": vis.state.pop(\"count\", \"gone\"),
        \"equals\": vis.state == {\"repo\": \"acme/widgets\", \"branch\": \"main\"},
        \"raised\": False,
    }
    try:
        del vis.state[\"count\"]
    except KeyError:
        probe[\"raised\"] = True
    vis.state.clear()
    probe[\"cleared\"] = list(vis.state)
    return probe


vis.register(vis.Extension(
    name=\"state-mapping\",
    description=\"vis.state mapping fixture.\",
    version=\"0.1.0\",
    kind=\"integration\",
    alias=\"state\",
    symbols=[vis.Symbol(state_probe, tag=\"mutation\")],
))
")

;; Regression: `vis.state` answered five methods, so an extension could not `pop`
;; (AttributeError) and `list(vis.state)` fell through to the old sequence protocol
;; — it asked the host for the key `0`.
(defdescribe state-mapping-test
             (it "vis.state drives the whole mapping surface across the host boundary"
                 (with-loaded {"state_mapping.py" state-mapping-py}
                              (fn [_ _]
                                (let [probe
                                      (symbol-fn (registered "state-mapping") 'state_probe)

                                      out
                                      (:result (probe))]

                                  ;; a key written as None is no key: no host tells a
                                  ;; stored null from one nobody ever wrote
                                  (expect (= ["branch" "count" "repo"] (get out "keys")))
                                  (expect (= 3 (get out "len")))
                                  (expect (= ["2" "acme/widgets" "main"] (get out "values")))
                                  (expect (= 2 (get out "popped")))
                                  (expect (= "gone" (get out "default")))
                                  (expect (true? (get out "equals")))
                                  (expect (true? (get out "raised")))
                                  (expect (= [] (get out "cleared"))))))))

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
                     (let [spec
                           (first (:ext/slash-commands (registered "counter")))

                           res
                           ((:slash/run-fn spec)
                             {:channel/id :tui :command/argv ["a" "b"] :command/raw "/count a b"})]

                       (expect (= "count" (:slash/name spec)))
                       (expect (= :ok (:slash/status res)))
                       (expect (str/includes? (:slash/title res) "count is"))
                       ;; :slash/data holds the Python-crossed dict — STRING keys
                       (expect (= ["a" "b"] (get-in res [:slash/data "args"]))))))))

;; Dynamic prompt + activation callables

(def ^:private moods-py
  "\"\"\"Dynamic prompt/activation fixture.\"\"\"
import blockether.vis.extension as vis


def _prompt(env):
    return \"MOOD ON\" if vis.state.get(\"mood\", False) else None


def _active(env):
    return env[\"cwd\"] is not None


def _toggle(ctx):
    vis.state[\"mood\"] = not vis.state.get(\"mood\", False)
    return vis.ok(\"toggled\")


vis.register(vis.Extension(
    name=\"moods\",
    description=\"Dynamic prompt fixture.\",
    kind=\"fun\",
    activation=_active,
    prompt=_prompt,
    slash_commands=[vis.SlashCommand(\"mood\", _toggle, doc=\"Toggle mood.\")],
))
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

;; Ctx contribution — vis.Extension(ctx=...) folds into the session bag

(def ^:private ctxer-py
  "\"\"\"Ctx-contribution fixture.\"\"\"
import blockether.vis.extension as vis


def _ctx(env):
    return {\"session_env\": {\"demo\": {\"cwd\": env[\"cwd\"], \"hits\": vis.state.get(\"hits\", 0)}}}


def _bad_ctx(env):
    return \"not a dict\"


vis.register(vis.Extension(
    name=\"ctxer\",
    description=\"Ctx fixture extension.\",
    kind=\"fun\",
    ctx=_ctx,
))
")

(defdescribe
  ctx-contribution-test
  (it
    "vis.register(vis.Extension(ctx=...)) registers an :ext/ctx-fn that folds into the session bag"
    (with-loaded {"ctxer.py" ctxer-py}
                 (fn [_ _]
                   (let [ext
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
      (with-loaded
        {"badctx2.py"
         (str "import blockether.vis.extension as vis\n"
              "vis.register(vis.Extension(name='bc2', description='d', kind='x', ctx=42))\n")}
        (fn [result _]
          (expect (= 1 (:failed result)))
          (expect (str/includes? (:error (first (pyx/load-failures))) "ctx must be a callable"))))))

;; Op hooks — before(=guard) blocks, after observes

(def ^:private guard-py
  "\"\"\"Guard fixture.\"\"\"
import blockether.vis.extension as vis


def _guard(call):
    for s in vis.strings_of(call[\"args\"]):
        if \".env\" in s:
            return vis.block(\"protected: \" + s)
    return None


vis.register(vis.Extension(
    name=\"guard\",
    description=\"Guard fixture extension.\",
    kind=\"guard\",
    op_hooks=[vis.OpHook([\"fs\", \"patch\"], _guard, phase=\"before\")],
))
")

(defdescribe
  op-hook-test
  (it
    "'before' hooks compile to :around guards that can block with a failure envelope"
    (with-loaded
      {"guard.py" guard-py}
      (fn [_ _]
        (let [hooks
              (:ext/op-hooks (registered "guard"))

              fs-hook
              (some #(when (= :fs (:op %)) %) hooks)]

          (expect (= #{:fs :patch} (set (map :op hooks))))
          (expect (every? #(= :around (:phase %)) hooks))
          ;; blocked: guard returns vis.block -> failure envelope, next never runs
          (let [ran?
                (atom false)

                res
                ((:fn fs-hook)
                  {}
                  :fs
                  ["/x/.env" "data"]
                  (fn [_]
                    (reset! ran? true)
                    :ran))]

            (expect (extension/envelope-failure? res))
            (expect (str/includes? (get-in res [:error :message]) "protected"))
            (expect (false? @ran?)))
          ;; allowed: guard returns None -> next runs with original args
          (expect (= :ran
                     ((:fn fs-hook)
                       {}
                       :fs
                       ["/x/ok.txt" "data"]
                       (fn [_]
                         :ran)))))))))

;; Gate hooks — a Python extension guards the FILESYSTEM, not a tool's arguments

(def ^:private fs-gate-py
  "\"\"\"Filesystem gate fixture.\"\"\"
import blockether.vis.extension as vis


def _fs_gate(access):
    if \"secrets\" in access[\"path\"] and access[\"operation\"].endswith(\"write\"):
        return vis.block(\"the vault is written through the vault tool\")
    return None


vis.register(vis.Extension(
    name=\"vault\",
    description=\"Vault fixture extension.\",
    kind=\"guard\",
    op_hooks=[vis.OpHook([\"fs_access\"], _fs_gate)],
))
")

(def ^:private broken-fs-gate-py
  "\"\"\"Filesystem gate that breaks.\"\"\"
import blockether.vis.extension as vis


def _fs_gate(access):
    raise RuntimeError(\"the rule table is unreadable\")


vis.register(vis.Extension(
    name=\"broken-vault\",
    description=\"Broken vault fixture extension.\",
    kind=\"guard\",
    op_hooks=[vis.OpHook([\"fs_access\"], _fs_gate)],
))
")

(defdescribe
  fs-access-gate-python-test
  "A gate is ASKED, never wrapped: the Python callable receives the gate's own
   string-keyed ctx and answers with `vis.block(reason)` or None. The compiled
   hook is what the host file tools consult before they touch a path."
  (it "compiles a Python fs_access hook into a :gate hook that refuses by sentence"
      (with-loaded
        {"vault.py" fs-gate-py}
        (fn [_ _]
          (let [hooks
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
                     (let [gate
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
        (let [p (payload :patch [{:path "/x/a.clj" :edits [{:from "12:a1b" :replace "x"}]}])]
          (expect (= {"op" "patch"
                      "args" [{"path" "/x/a.clj" "edits" [{"from" "12:a1b" "replace" "x"}]}]}
                     p))
          (expect (= p (ep/boundary-view p)))))
    (it "an after-hook payload stringifies the result too"
        (let [p (payload :grep ["needle"] {:status :ok :hits [{:path "a.clj"}]})]
          (expect
            (= {"op" "grep" "args" ["needle"] "result" {"status" "ok" "hits" [{"path" "a.clj"}]}}
               p))
          (expect (= p (ep/boundary-view p)))))))

(def ^:private filter-py
  "import blockether.vis.extension as vis

def _req(r):
    if r['method'] == 'POST':
        return vis.block('no posting to ' + r['host'])
    return None

def _resp(r):
    if r['status'] == 403:
        return vis.block('upstream 403')
    return None

vis.register(vis.Extension(
    name='filt',
    description='Egress filter fixture.',
    kind='guard',
    network_filters=[vis.NetworkFilter(_req), vis.NetworkFilter(_resp)],
))
")

(defdescribe
  egress-filter-test
  (it "vis.network_filter registers host egress filters (request + response phases) that can block"
      (with-loaded
        {"filt.py" filter-py}
        (fn [_ _]
          (let [ext
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

;; Failure containment

(defdescribe
  load-failure-test
  (it "a broken file is a recorded load failure, never a crash"
      (with-loaded
        {"broken.py"
         "import blockether.vis.extension as vis\nraise RuntimeError('nope at import')\n"}
        (fn [result _]
          (expect (= 0 (:loaded result)))
          (expect (= 1 (:failed result)))
          (expect (str/includes? (:error (first (pyx/load-failures))) "nope at import")))))
  (it "a file that never calls vis.register(vis.Extension()) is a load failure"
      (with-loaded {"empty.py" "x = 1\n"}
                   (fn [result _]
                     (expect (= 1 (:failed result)))
                     (expect (str/includes? (:error (first (pyx/load-failures)))
                                            "never called vis.register")))))
  (it "a tool without a docstring is rejected with a clear message"
      (with-loaded {"nodoc.py"
                    (str
                      "import blockether.vis.extension as vis\n" "def nodoc_x():\n    return 1\n"
                      "vis.register(vis.Extension(name='nodoc', description='d', alias='nodoc',\n"
                      "              kind='x', symbols=[vis.Symbol(nodoc_x)]))\n")}
                   (fn [result _]
                     (expect (= 1 (:failed result)))
                     (expect (str/includes? (:error (first (pyx/load-failures))) "docstring"))))))

;; Regression, issue #152: a preset's `api_style` was keywordized verbatim, so a
;; near-miss spelling like `openai_responses` reached svar as a dialect its `case`
;; does not know and silently meant `/chat/completions` — a Responses endpoint
;; served on the chat wire, which is how a Responses-minted tool-call id ended up
;; replayed to a chat provider.
(defdescribe
  provider-preset-dialect-test
  (it "an accepted alias normalizes to svar's own api-style"
      (with-loaded
        {"dialect.py"
         (str "import blockether.vis.extension as vis\n"
              "vis.register(vis.Extension(name='dialect', description='d',\n"
              "              providers=[vis.Provider(id='dialect', label='Dialect',\n"
              "                  preset=vis.ProviderPreset(base_url='https://dialect.test/v1',\n"
              "                          api_style='openai_responses'))]))\n")}
        (fn [_ _]
          (expect (= :openai-compatible-responses
                     (:api-style (:provider/preset (registry/provider-by-id :dialect))))))))
  (it "an api_style naming no wire dialect is refused, never keywordized"
      (with-loaded
        {"baddialect.py"
         (str "import blockether.vis.extension as vis\n"
              "vis.register(vis.Extension(name='baddialect', description='d',\n"
              "              providers=[vis.Provider(id='baddialect', label='Bad',\n"
              "                  preset=vis.ProviderPreset(base_url='https://bad.test/v1',\n"
              "                          api_style='openai-responses-v2'))]))\n")}
        (fn [result _]
          (expect (= 1 (:failed result)))
          (expect (str/includes? (:error (first (pyx/load-failures))) "names no wire dialect")))))
  (it "a credential the extension issues may declare the wire dialect too"
      ;; The dialect is a property of the ENDPOINT, and a managed provider's endpoint
      ;; only exists once the credential is issued — so `get_token_fn` takes the same
      ;; vocabulary as the preset does.
      (with-loaded
        {"tokendialect.py"
         (str "import blockether.vis.extension as vis\n" "def _token():\n"
              "    return vis.ProviderCredential('k', api_url='https://issued.test/v1',\n"
              "            api_style='openai_responses', responses_path='/responses')\n"
              "vis.register(vis.Extension(name='tokendialect', description='d',\n"
              "              providers=[vis.Provider(id='tokendialect', label='Issued',\n"
              "                  preset=vis.ProviderPreset(base_url='https://issued.test/v1'),\n"
              "                  get_token_fn=_token)]))\n")}
        (fn [_ _]
          (expect (= :openai-compatible-responses
                     (:api-style ((:provider/get-token-fn (registry/provider-by-id
                                                            :tokendialect))))))))))

;; The default belongs to the transport, not credentials: a token-only callback
;; must preserve a preset's custom path. Exercise the pinned Svar through HTTP.
(defdescribe
  provider-responses-path-test
  (it
    "routes Responses requests without a path and preserves explicit overrides"
    (doseq [[preset-path credential-path configured-path expected-path]
            [[nil nil nil "/responses"] ["responses" nil nil "/responses"]
             ["/custom/responses" nil nil "/custom/responses"]
             ["/preset/responses" "/credential/responses" nil "/credential/responses"]
             ["/preset/responses" "/credential/responses" "/configured/responses"
              "/configured/responses"]]

            stream?
            [false true]]

      (with-loaded
        {"responsesfixture.py"
         (str "import blockether.vis.extension as vis\n"
              "def credential():\n"
              "    return vis.ProviderCredential('fixture'"
              (when credential-path (str ", responses_path=" (pr-str credential-path)))
              ")\n"
              "vis.register(vis.Extension(name='responsesfixture', description='d',\n"
              "    providers=[vis.Provider(id='responsesfixture', label='Responses fixture',\n"
              "        preset=vis.ProviderPreset(base_url='https://gateway.example.com/v1',\n"
              "            api_style='openai-responses'"
              (when preset-path (str ", responses_path=" (pr-str preset-path)))
              "), get_token_fn=credential)]))\n")}
        (fn [loaded _]
          (expect (= 1 (:loaded loaded)))
          (let [provider
                (config/->svar-provider
                  (with-redefs [config/load-config-raw
                                (constantly {"providers"
                                             [(cond-> {"id" "responsesfixture"
                                                       "models" [{"name" "fixture-model"}]}
                                                configured-path
                                                (assoc "responses_path" configured-path))]})]
                    (first (:providers (config/load-config)))))

                router
                (svar/make-router [provider])

                requests
                (atom [])

                reply
                {:id "response-fixture"
                 :output
                 [{:type "message" :role "assistant" :content [{:type "output_text" :text "ok"}]}]
                 :usage {:input_tokens 1 :output_tokens 1 :total_tokens 2}}]

            (expect (= :openai-compatible-responses (:api-style provider)))
            (expect (= (or configured-path credential-path preset-path) (:responses-path provider)))
            (with-redefs [http/post (fn [url opts]
                                      (swap! requests conj
                                        {:url url :body (json/parse-string (:body opts) true)})
                                      {:status 200
                                       :headers {}
                                       :body (if (= :stream (:as opts))
                                               (io/input-stream (.getBytes
                                                                  (str "data: "
                                                                       (json/generate-string
                                                                         {:type "response.completed"
                                                                          :response reply})
                                                                       "\n\ndata: [DONE]\n\n")
                                                                  StandardCharsets/UTF_8))
                                               (json/generate-string reply))})]
              (expect (= "ok"
                         (:content (svar/ask-code! router
                                                   (cond-> {:routing {:provider :responsesfixture
                                                                      :model "fixture-model"}
                                                            :messages [{:role "user"
                                                                        :content "hello"}]
                                                            :tools []}
                                                     stream?
                                                     (assoc :on-chunk
                                                       (fn [_])))))))
              (expect (= 1 (count @requests)))
              (let [{:keys [url body]} (first @requests)]
                (expect (= (str "https://gateway.example.com/v1" expected-path) url))
                (expect (= "fixture-model" (:model body)))
                (expect (pos-int? (:max_output_tokens body)))
                (expect (not (contains? body :max_tokens)))
                (expect (seq (:input body)))
                (expect (not (contains? body :messages)))))))))))

;; Reload + project-over-global precedence

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
          (expect (= 0
                     (get-in ((symbol-fn (registered "counter") 'counter_read)) [:result "count"])))
          (write-ext! ext-dir "counter.py" (str "BOOM = _vis_undefined_ + 1\n" counter-py))
          (let [result (pyx/load-python-extensions! {:dirs [(str ext-dir)]})]
            (expect (= 1 (:loaded result)))
            (expect (= 1 (:failed result)))
            (expect (str/includes? (:error (first (pyx/load-failures))) "_vis_undefined_"))
            (let [ext (registered "counter")]
              (expect (some? ext))
              (expect (= '[counter_bump counter_read counter_boom]
                         (mapv :ext.symbol/symbol (get-in ext [:ext/engine :ext.engine/symbols]))))
              (expect (= 0 (get-in ((symbol-fn ext 'counter_read)) [:result "count"]))))))))
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
      (let [global
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

;; Multi-file project — an extension imports a sibling package (sys.path sugar)

(def ^:private pkgext-py
  "\"\"\"Package-backed fixture: imports a sibling package next to it.\"\"\"
import blockether.vis.extension as vis
from mypkg.core import add
from mypkg import VERSION


def pkg_add(a, b):
    \"\"\"await pkg_add(a, b) -> {\\\"sum\\\", \\\"version\\\"} — add via the sibling package.\"\"\"
    return {\"sum\": add(a, b), \"version\": VERSION}


vis.register(vis.Extension(
    name=\"pkgext\",
    description=\"Package-backed fixture extension.\",
    version=\"0.1.0\",
    kind=\"integration\",
    alias=\"pkg\",
    symbols=[vis.Symbol(pkg_add, tag=\"observation\")],
))
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
                       (let [add (symbol-fn ext 'pkg_add)
                             res (add 2 3)]

                         (expect (extension/envelope-success? res))
                         (expect (= 5 (get-in res [:result "sum"])))
                         (expect (= "1.2.3" (get-in res [:result "version"])))))))))

(defdescribe
  entrypoint-import-collision-test
  ;; Issue #176: preserve import precedence and explain the entrypoint rename.
  (it
    "reports a colliding entrypoint and keeps a renamed bridge working across reloads"
    (with-shared-packages
      (fn [packages]
        (write-ext! packages "issue176_demo/__init__.py" "")
        (write-ext! packages "issue176_demo/core.py" "VALUE = 'OK'\n")
        (doseq [[entry collision?] [["issue176_demo.py" true] ["issue176_bridge.py" false]]]
          (let [source (str "import importlib\nimport blockether.vis.extension as vis\n"
                            "def ping():\n" "    'Return the implementation value.'\n"
                            "    return importlib.import_module('issue176_demo.core').VALUE\n"
                            "vis.register(vis.Extension(name='issue176-collision', "
                            "description='Import collision fixture', alias='issue176_demo', "
                            "symbols=[vis.Symbol(ping, tag='observation')]))\n")]
            (with-fresh-loaded
              {entry source}
              (fn [result {:keys [ext-dir]}]
                (expect (= 1 (:loaded result)) (pr-str (pyx/load-failures)))
                (expect (= 0 (:failed result)))
                (dotimes [generation 3]
                  (dotimes [_ 2]
                    (let [out ((symbol-fn (registered "issue176-collision") 'ping))]
                      (if collision?
                        (do (expect (false? (:success? out)) (pr-str out))
                            (doseq [fragment
                                    ["extension 'issue176-collision' is already registered"
                                     "If this happened during an import"
                                     "may be shadowing a package or module with the same name"
                                     "Rename the entrypoint" "demo.py -> demo_bridge.py"
                                     "public alias can stay unchanged"]]
                              (expect (str/includes? (str (get-in out [:error :message])) fragment)
                                      (pr-str out))))
                        (do (expect (extension/envelope-success? out) (pr-str out))
                            (expect (= "OK" (:result out)) (pr-str out))))))
                  (when (< generation 2)
                    (write-ext! ext-dir entry (str source "# Reload " generation "\n"))
                    (expect (= {:loaded 1 :failed 0 :changed? true}
                               (pyx/reload-python-extensions! {:dirs [(str ext-dir)]})))))))))))))

;; Characterization of the split implementation/dependency layout, before adding
;; a declaration format. These assertions describe today's missing bootstrap steps,
;; not the desired final behavior. No private index or network install is needed.
(defdescribe
  split-project-extension-bootstrap-test
  (it
    "separates missing implementation, missing dependency, and successful registration"
    (with-fresh-loaded
      {"einmal/vis_einmal_fixture/__init__.py"
       "from vis_einmal_dependency_fixture import VALUE\ndef answer():\n    return VALUE\n"
       "einmal/pyproject.toml"
       (str "[project]\nname = \"vis-einmal-fixture\"\nversion = \"0.0.1\"\n"
            "dependencies = [\"vis-einmal-dependency-fixture==0.0.1\"]\n"
            "[tool.uv.sources]\nvis-einmal-dependency-fixture = {path = \"../dependency\"}\n")
       "dependency/vis_einmal_dependency_fixture/__init__.py" "VALUE = 42\n"
       ".vis/extensions/einmal.py"
       (str "import blockether.vis.extension as vis\n"
            "from vis_einmal_fixture import answer\n" "def einmal_answer():\n"
            "    \"\"\"Return the separately implemented fixture value.\"\"\"\n"
            "    return answer()\n"
            "vis.register(vis.Extension(name=\"einmal-fixture\", description=\"Fixture\", "
            "alias=\"einmal-fixture\", symbols=[vis.Symbol(einmal_answer)]))\n")}
      (fn [_ {:keys [ext-dir]}]
        (let [entries
              (io/file ext-dir ".vis/extensions")

              reload!
              #(pyx/reload-python-extensions! {:dirs [(str entries)]})]

          ;; CLI source_paths are not extension import roots.
          (with-redefs [config/load-config-raw
                        (constantly {"python" {"source_paths" [(str (io/file ext-dir "einmal"))]}})]
            (expect (= {:loaded 0 :failed 1 :changed? true} (reload!)))
            (expect (nil? (registered "einmal-fixture")))
            (expect (str/includes? (:error (first (pyx/load-failures)))
                                   "No module named 'vis_einmal_fixture'")))
          ;; The existing linked-package convention makes the implementation
          ;; importable, but does not install its pyproject/uv dependencies.
          (Files/createSymbolicLink (.toPath (io/file entries "vis_einmal_fixture"))
                                    (.toPath (io/file ext-dir "einmal/vis_einmal_fixture"))
                                    (make-array FileAttribute 0))
          (expect (= {:loaded 0 :failed 1 :changed? true} (reload!)))
          (expect (nil? (registered "einmal-fixture")))
          (expect (str/includes? (:error (first (pyx/load-failures)))
                                 "No module named 'vis_einmal_dependency_fixture'"))
          ;; Supply the dependency locally to isolate registration from package
          ;; installation. This is a fixture, not a proposed installer.
          (Files/createSymbolicLink (.toPath (io/file entries "vis_einmal_dependency_fixture"))
                                    (.toPath (io/file ext-dir
                                                      "dependency/vis_einmal_dependency_fixture"))
                                    (make-array FileAttribute 0))
          (expect (= {:loaded 1 :failed 0 :changed? true} (reload!)))
          (expect (empty? (pyx/load-failures)))
          (let [ext
                (registered "einmal-fixture")

                result
                ((symbol-fn ext 'einmal_answer))]

            (expect (some? ext))
            (expect (extension/envelope-success? result))
            (expect (= 42 (:result result)))))))))

(def ^:private split-extension-script
  "# /// script\n# dependencies = [\"vis-einmal-dependency-fixture==0.0.1\"]\n# [tool.vis]\n# source_paths = [\"../../einmal\"]\n# ///\nimport blockether.vis.extension as vis\nfrom vis_einmal_fixture import answer\n\ndef einmal_answer():\n    \"\"\"Return the fixture value.\"\"\"\n    return answer()\n\nvis.register(vis.Extension(name=\"einmal-declared\", description=\"Fixture\",\n                           alias=\"einmal-declared\", symbols=[vis.Symbol(einmal_answer)]))\n")

(defdescribe
  declared-extension-bootstrap-test
  (it
    "installs before imports, freezes external sources, and retains last-good on install failure"
    (with-shared-packages
      (fn [_]
        (let [installs
              (atom [])

              fail?
              (atom false)]

          (with-redefs [python-runtime/pip-install!
                        (fn [opts specs]
                          (swap! installs conj specs)
                          (if @fail?
                            {:exit 1
                             :out "INSTALL_FAILURE_MARKER: no matching wheel; token=fixture-secret"}
                            (do (write-ext! (io/file (:target opts))
                                            "vis_einmal_dependency_fixture.py"
                                            "VALUE = 42\n")
                                {:exit 0})))]
            (with-fresh-loaded
              {"einmal/vis_einmal_fixture/__init__.py"
               "def answer():\n    from vis_einmal_dependency_fixture import VALUE\n    return VALUE\n"
               ".vis/extensions/einmal.py" split-extension-script}
              (fn [_ {:keys [ext-dir]}]
                (let [opts {:dirs [(str (io/file ext-dir ".vis/extensions"))]}
                      invoke #(:result ((symbol-fn (registered "einmal-declared") 'einmal_answer)))]

                  (expect (= {:loaded 1 :failed 0 :changed? true}
                             (pyx/reload-python-extensions! opts)))
                  (expect (= [["vis-einmal-dependency-fixture==0.0.1"]] @installs))
                  (expect (= 42 (invoke)))
                  (expect (false? (:changed? (pyx/load-python-extensions! opts))))
                  (expect (= 1 (count @installs)))
                  (write-ext! ext-dir
                              "einmal/vis_einmal_fixture/__init__.py"
                              "def answer():\n    return 43\n")
                  (expect (= 42 (invoke)))
                  (expect (= 0 (:failed (pyx/reload-python-extensions! opts))))
                  (expect (= 43 (invoke)))
                  (reset! fail? true)
                  (expect (= {:loaded 1 :failed 1 :changed? true}
                             (pyx/reload-python-extensions! opts)))
                  (expect (= 43 (invoke)))
                  ;; #183: retain useful installer errors, redact only the credential.
                  (let [error (:error (first (pyx/load-failures)))]
                    (expect (str/includes? error "INSTALL_FAILURE_MARKER"))
                    (expect (not (str/includes? error "fixture-secret")))))))))))))

(defdescribe
  extension-metadata-validation-test
  (it "rejects malformed or unsupported declarations before registration or installation"
      (doseq
        [block
         ["# /// script\n# dependencies = [\n# ///\n" "# /// script\n# dependencies = []\n"
          "# /// script\n# dependencies = []\n# ///\n# /// script\n# ///\n"
          "# /// script\n# dependencies = 'six'\n# ///\n"
          "# /// script\n# dependencies = [42]\n# ///\n"
          "# /// script\n# dependencies = ['--no-index']\n# ///\n"
          "# /// script\n# dependencies = ['six @ https://gateway.example.com/six.whl']\n# ///\n"
          "# /// script\n# requires-python = '<2'\n# ///\n"
          "# /// script\n# requires-python = 42\n# ///\n"
          "# /// script\n# [tool.vis]\n# source_paths = 'src'\n# ///\n"
          "# /// script\n# [tool.vis]\n# source_paths = ['missing']\n# ///\n"
          "# /// script\n# [tool.vis]\n# source_paths = ['..']\n# ///\n"
          "# /// script\n# [tool.uv.sources]\n# six = {path = '../six'}\n# ///\n"
          "# /// script\n# [tool.vis]\n# project = 42\n# ///\n"
          "# /// script\n# [tool.vis]\n# project = ''\n# ///\n"
          "# /// script\n# [tool.vis]\n# project = 'missing'\n# ///\n"
          "# /// script\n# dependencies = ['six']\n# [tool.vis]\n# project = '.'\n# ///\n"]]
        (let [installs (atom 0)]
          (with-redefs [python-runtime/pip-install! (fn [& _]
                                                      (swap! installs inc))]
            (with-fresh-loaded {"bad.py" (str block counter-py)}
                               (fn [result _]
                                 (expect (= 0 (:loaded result)))
                                 (expect (= 1 (:failed result)))
                                 (expect (nil? (registered "counter")))
                                 (expect (zero? @installs)))))))))

(defdescribe
  extension-source-collision-test
  (it "rejects conflicting files from external roots instead of overwriting one"
      (with-fresh-loaded
        {"impl/shared.py" "VALUE = 2\n"
         ".vis/extensions/shared.py" "VALUE = 1\n"
         ".vis/extensions/entry.py"
         (str "# /// script\n# [tool.vis]\n# source_paths = ['../../impl']\n# ///\n" counter-py)}
        (fn [_ {:keys [ext-dir]}]
          (pyx/reload-python-extensions! {:dirs [(str (io/file ext-dir ".vis/extensions"))]})
          (expect (nil? (registered "counter")))
          (expect (some #(str/includes? (:error %) "conflicting relative file names")
                        (pyx/load-failures)))))))

(defdescribe
  extension-index-wheel-registration-test
  (it
    "downloads a wheel from vis.yml's index and calls the tool in its trusted worker"
    (doseq [mode
            [:pip :uv-default :uv-index :uv-path]

            :let [uv?
                  (not= :pip mode)

                  local?
                  (= :uv-path mode)]]

      (with-shared-packages
        (fn [packages]
          (let
            [wheel-name
             "vis_einmal_dependency_fixture-0.0.1-py3-none-any.whl"

             dist
             "vis_einmal_dependency_fixture-0.0.1.dist-info/"

             bytes
             (ByteArrayOutputStream.)

             _
             (with-open [zip (ZipOutputStream. bytes)]
               (doseq
                 [[path text]
                  {"vis_einmal_dependency_fixture.py" "VALUE = 42\n"
                   (str dist "METADATA")
                   "Metadata-Version: 2.1\nName: vis-einmal-dependency-fixture\nVersion: 0.0.1\n"
                   (str dist "WHEEL")
                   "Wheel-Version: 1.0\nGenerator: vis-test\nRoot-Is-Purelib: true\nTag: py3-none-any\n"
                   (str dist "RECORD") ""}]
                 (.putNextEntry zip (ZipEntry. path))
                 (.write zip (.getBytes ^String text StandardCharsets/UTF_8))
                 (.closeEntry zip)))

             wheel
             (.toByteArray bytes)

             requests
             (atom [])

             installs
             (atom 0)

             install!
             runtime/pip-install!

             server
             (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]

            (.createContext server
                            "/"
                            (reify
                              HttpHandler
                                (handle [_ exchange]
                                  (let [^HttpExchange exchange
                                        exchange

                                        path
                                        (.getPath (.getRequestURI exchange))

                                        is-wheel
                                        (= path (str "/files/" wheel-name))

                                        body
                                        (if is-wheel
                                          wheel
                                          (.getBytes
                                            (str "<a href='/files/" wheel-name "'>fixture</a>")
                                            StandardCharsets/UTF_8))]

                                    (swap! requests conj path)
                                    (.set (.getResponseHeaders exchange)
                                          "Content-Type"
                                          (if is-wheel "application/octet-stream" "text/html"))
                                    (.sendResponseHeaders exchange 200 (alength ^bytes body))
                                    (with-open [out (.getResponseBody exchange)]
                                      (.write out ^bytes body))
                                    (.close exchange)))))
            (.start server)
            (try
              (with-redefs [runtime/pip-install! (fn [opts specs]
                                                   (swap! installs inc)
                                                   (install! opts
                                                             (into (vec specs)
                                                                   ["--isolated" "--proxy" ""
                                                                    "--retries" "0" "--timeout" "3"
                                                                    "--no-cache-dir"])))]
                (with-fresh-loaded
                  {"einmal/vis_einmal_fixture/__init__.py"
                   "import vis_einmal_dependency_fixture as dep\ndef answer():\n    return {'value': dep.VALUE, 'path': dep.__file__}\n"
                   ".vis/extensions/einmal.py"
                   (if uv?
                     (-> split-extension-script
                         (str/replace "# dependencies = [\"vis-einmal-dependency-fixture==0.0.1\"]"
                                      "# dependencies = []")
                         (str/replace "# [tool.vis]" "# [tool.vis]\n# project = '../../einmal'"))
                     split-extension-script)
                   "vis.yml" (str "python:\n  index_url: http://127.0.0.1:"
                                  (.getPort (.getAddress server))
                                  "/simple\n")}
                  (fn [_ {:keys [ext-dir]}]
                    (with-redefs [config/load-config-raw #(@#'config/read-yaml-config-map
                                                            (str (io/file ext-dir "vis.yml")))]
                      (when uv?
                        (when local?
                          (write-ext!
                            ext-dir
                            "dependency/pyproject.toml"
                            (str
                              "[project]\nname = 'vis-einmal-dependency-fixture'\nversion = '0.0.1'\n"
                              "[build-system]\nrequires = []\nbuild-backend = 'fixture_backend'\nbackend-path = ['.']\n"))
                          (with-open [out (io/output-stream
                                            (io/file ext-dir "dependency" wheel-name))]
                            (.write out ^bytes wheel))
                          (write-ext!
                            ext-dir
                            "dependency/fixture_backend.py"
                            (str
                              "from pathlib import Path\nimport shutil\n"
                              "def build_wheel(wheel_directory, config_settings=None, metadata_directory=None):\n"
                              "    name = '" wheel-name
                              "'\n"
                              "    shutil.copyfile(Path(__file__).with_name(name), Path(wheel_directory) / name)\n"
                              "    return name\n"
                              "def build_editable(wheel_directory, config_settings=None, metadata_directory=None):\n"
                              "    from zipfile import ZipFile\n"
                              "    root = Path(__file__).parent\n"
                              "    name = '" wheel-name
                              "'\n"
                              "    with ZipFile(root / name) as source, ZipFile(Path(wheel_directory) / name, 'w') as target:\n"
                              "        for entry in source.namelist():\n"
                              "            if not entry.endswith('.py'):\n                target.writestr(entry, source.read(entry))\n"
                              "        target.writestr('fixture.pth', str(root) + '\\n')\n"
                              "    return name\n"))
                          (write-ext! ext-dir
                                      "dependency/vis_einmal_dependency_fixture.py"
                                      "VALUE = 42\n"))
                        (write-ext!
                          ext-dir
                          "einmal/pyproject.toml"
                          (str
                            "[project]\nname = 'fixture-project'\nversion = '0.0.1'\n"
                            "requires-python = '>=3.12'\n"
                            "dependencies = ['vis-einmal-dependency-fixture==0.0.1']\n"
                            (cond
                              local?
                              "[tool.uv.sources]\nvis-einmal-dependency-fixture = {path = '../dependency', editable = true}\n"
                              (= :uv-index mode)
                              (str
                                "[tool.uv.sources]\nvis-einmal-dependency-fixture = {index = 'fixture'}\n"
                                "[[tool.uv.index]]\nname = 'fixture'\nurl = '"
                                (get-in (config/load-config-raw) ["python" "index_url"])
                                "'\nexplicit = true\n")
                              :else (str "[[tool.uv.index]]\nurl = '"
                                         (get-in (config/load-config-raw) ["python" "index_url"])
                                         "'\ndefault = true\n"))))
                        (let [p (.start
                                  (doto (ProcessBuilder.
                                          ^java.util.List
                                          [(runtime/uv-executable) "lock" "--project"
                                           (str (io/file ext-dir "einmal")) "--python"
                                           (com.blockether.vispython.Interpreter/pythonExecutable)
                                           "--default-index"
                                           (get-in (config/load-config-raw) ["python" "index_url"])
                                           "--no-cache" "--no-python-downloads"])
                                    (.redirectErrorStream true)))
                              output (slurp (.getInputStream p))]

                          (expect (= 0 (.waitFor p)) output)))
                      (when uv?
                        (expect (= {:loaded 0 :failed 1 :changed? true}
                                   (pyx/reload-python-extensions!
                                     {:dirs [(str (io/file ext-dir ".vis/extensions"))]})))
                        (expect (zero?
                                  (python-runtime/uv-command!
                                    ["sync" "--project" (str (io/file ext-dir "einmal")) "--locked"
                                     "--python"
                                     (com.blockether.vispython.Interpreter/pythonExecutable)]))))
                      (expect (= {:loaded 1 :failed 0 :changed? true}
                                 (with-redefs [python-runtime/ensure-project!
                                               (fn [& _]
                                                 (throw (ex-info "Loader must not sync" {})))]
                                   (pyx/reload-python-extensions!
                                     {:dirs [(str (io/file ext-dir ".vis/extensions"))]}))))
                      (when-not local?
                        (expect (some #{"/simple/vis-einmal-dependency-fixture/"} @requests))
                        (expect (some #{(str "/files/" wheel-name)} @requests)))
                      (when uv? (expect (.isDirectory (io/file ext-dir "einmal/.venv"))))
                      (let [ext (registered "einmal-declared")
                            made (ep/create-python-context {}
                                                           (fn []
                                                             [(.getCanonicalPath ext-dir)])
                                                           {:worker? true
                                                            :jail-enabled? true
                                                            :enabled? false
                                                            :allowed-domains []
                                                            :denied-domains []
                                                            :exclude-domains []}
                                                           nil)
                            ctx (:python-context made)
                            env {:python-context ctx
                                 :session-id "declared-extension-worker"
                                 :extensions (atom [ext])
                                 :active-extensions (atom [])}]

                        (try
                          (lp/sync-active-extension-symbols! env [ext])
                          (let
                            [result
                             (ep/run-python-block
                               ctx
                               (str
                                 "from pathlib import Path\n"
                                 (if uv?
                                   "import importlib.util; assert importlib.util.find_spec('vis_einmal_dependency_fixture') is None\n"
                                   "import vis_einmal_dependency_fixture as dep\n")
                                 "result = await einmal_answer()\n"
                                 (when-not uv? "assert result['path'] == dep.__file__\n")
                                 "assert Path(result['path']).parent == Path("
                                 (pr-str (str (cond local? (io/file ext-dir "dependency")
                                                    uv? (python-runtime/prepared-project
                                                          (io/file ext-dir "einmal"))
                                                    :else packages)))
                                 ")\n" "print(result['value'])"))]
                            (expect (nil? (:error result)))
                            (expect (= "42" (str/trim (:stdout result)))))
                          (expect (= (if uv? 0 1) @installs))
                          (when uv?
                            (let [probe (ep/run-python-block
                                          ctx
                                          "assert '__vis_pip_install__' not in globals()")]
                              (expect (nil? (:error probe))))
                            (let [lock-file (io/file ext-dir "einmal/uv.lock")
                                  lock-before (slurp lock-file)]

                              (spit
                                (io/file ext-dir "einmal/pyproject.toml")
                                "\n[project.optional-dependencies]\nstale = ['absent-fixture==1']\n"
                                :append
                                true)
                              (expect (= {:loaded 1 :failed 1 :changed? true}
                                         (pyx/reload-python-extensions!
                                           {:dirs [(str (io/file ext-dir ".vis/extensions"))]})))
                              (expect (= lock-before (slurp lock-file)))
                              (expect (identical? ext (registered "einmal-declared")))
                              (expect (= 42
                                         (get-in ((symbol-fn ext 'einmal_answer))
                                                 [:result "value"])))))
                          (finally (ep/dispose-python-context! ctx))))))))
              (finally (.stop server 0)))))))))

;; Package-extension convention — a subdir holding extension.py = ONE extension

(defdescribe
  package-extension-convention-test
  (it "a subdir holding extension.py loads as ONE extension; its package/test files are not scanned"
      (with-loaded
        {"my_ext/mypkg/__init__.py" "VERSION = \"9.9\"\n"
         "my_ext/mypkg/core.py" "def add(a, b):\n    return a + b\n"
         "my_ext/extension.py"
         (str "import blockether.vis.extension as vis\n" "from mypkg.core import add\n"
              "def mx_add(a, b):\n"
              "    \"\"\"await mx_add(a, b) -> {\"sum\"} — add via the sibling package.\"\"\"\n"
              "    return {\"sum\": add(a, b)}\n"
              "vis.register(vis.Extension(name=\"myext\", description=\"d\", version=\"0.1.0\",\n"
              "              kind=\"integration\", alias=\"mx\",\n"
              "              symbols=[vis.Symbol(mx_add, tag=\"observation\")]))\n")
         "my_ext/test_core.py" "def test_ok():\n    assert 1 == 1\n"}
        (fn [result _]
          ;; the package dir contributes exactly ONE extension; the
          ;; modules under mypkg/ and the test file are NOT loaded
          (expect (= {:loaded 1 :failed 0 :changed? true} result))
          (let [ext (registered "myext")]
            (expect (some? ext))
            (let [add (symbol-fn ext 'mx_add)]
              (expect (= 3 (get-in (add 1 2) [:result "sum"])))))))))

;; Python-level self-tests — test_*.py / *_test.py run through the pytest shim

(defdescribe
  python-self-test-test
  (it
    "runs test_*.py / *_test.py through the pytest shim, imports the sibling package, reports pass/fail"
    (let [ext-dir
          (temp-dir)

          store
          (ps/db-create-connection! :memory)]

      (write-ext! ext-dir "my_ext/mypkg/__init__.py" "VERSION = \"1.0\"\n")
      (write-ext! ext-dir "my_ext/mypkg/core.py" "def add(a, b):\n    return a + b\n")
      (write-ext!
        ext-dir
        "my_ext/extension.py"
        (str
          "import blockether.vis.extension as vis\n" "def noop():\n"
          "    \"\"\"await noop() -> {} — nothing.\"\"\"\n" "    return {}\n"
          "vis.register(vis.Extension(name=\"mx\", description=\"d\", kind=\"fun\", alias=\"mx\",\n"
          "              symbols=[vis.Symbol(noop, tag=\"observation\")]))\n"))
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
               (let [by-name
                     (into {} (map (juxt #(last (str/split (:file %) #"/")) :ok?)) (:results res))]
                 ;; the package test resolves `from mypkg.core import add`
                 (expect (true? (get by-name "test_core.py")))
                 (expect (false? (get by-name "foo_test.py")))))
             (finally (ps/db-dispose-connection! store)))))))

;; Structured counts — outcomes come from the shim, never scraped from stdout

(defdescribe
  structured-counts-test
  (it "a failure whose assertion message contains '9 passed' must NOT inflate the pass count"
      (let [ext-dir
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

;; /test is the user-facing surface for the Python extension runner.

(defdescribe slash-test-wiring-test
             (it "exposes /test without contributing an extension CLI command"
                 (with-loaded {"counter.py" counter-py}
                              (fn [_ _]
                                ;; Force a fresh registration so we read the
                                ;; CURRENT loader spec, not a stale one left by an
                                ;; earlier load in a reused REPL JVM (the
                                ;; `loader-registered?` defonce guard blocks re-runs).
                                (reset! @#'pyx/loader-registered? false)
                                (#'pyx/register-loader-extension!)
                                (let [loader
                                      (registered "python-extensions")

                                      slash
                                      (some #(when (= "test" (:slash/name %)) %)
                                            (:ext/slash-commands loader))]

                                  (expect (some? loader))
                                  (expect (some? slash))
                                  (expect (ifn? (:slash/run-fn slash)))
                                  (expect (empty? (:ext/cli loader))))))))

(defdescribe
  run-and-report-test
  (it "renders a friendly message when no tests are found"
      (expect (str/includes? (#'runner/render-test-report {:files 0 :ok? true :results []})
                             "No Python extension tests")))
  (it "the /test code path runs tests and renders a report"
      (let [ext-dir
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

;; Per-test granularity — the runner reports EACH test, not just a file verdict

(defdescribe
  per-test-granularity-test
  (it "reports each test's nodeid + outcome (tagged with its file), not just a per-file aggregate"
      (let [ext-dir
            (temp-dir)

            store
            (ps/db-create-connection! :memory)]

        (write-ext! ext-dir
                    "foo_test.py"
                    (str "def test_alpha():\n    assert 1 + 1 == 2\n"
                         "def test_beta():\n    assert 2 + 2 == 5\n"))
        (binding [extension/*current-environment* {:db-info store}]
          (try (let [res (runner/test-python-extensions! {:dirs [(str ext-dir)]})
                     by-id (into {} (map (juxt :nodeid :outcome)) (:tests res))]

                 (expect (= 2 (count (:tests res))))
                 ;; pytest's own nodeid: the file it was collected from, then
                 ;; the test - never a bare function name.
                 (expect (= :passed (get by-id "foo_test.py::test_alpha")))
                 (expect (= :failed (get by-id "foo_test.py::test_beta")))
                 ;; every record carries the file it came from
                 (expect (every? :file (:tests res)))
                 (let [report (#'runner/render-test-report res)]
                   (expect (str/includes? report "test_alpha"))
                   (expect (str/includes? report "test_beta"))))
               (finally (ps/db-dispose-connection! store)))))))

;; Providers — a `vis.Provider(...)` registers a first-class provider descriptor

(def ^:private provider-py
  "'''Acme provider fixture.'''
import blockether.vis.extension as vis


def _token():
    return vis.ProviderCredential('sk-test-123', api_url='https://acme.test/v1')


def _status():
    return vis.ProviderStatus(True, source='env-var', provider_id='acme')


def _detect():
    return vis.ProviderCredential('sk-test-123', source='env-var')


_logout_calls = {'n': 0}


def _logout():
    _logout_calls['n'] += 1
    return 'logged-out'


def _limits():
    return vis.ProviderLimits(provider_id='acme', limits=[
        vis.ProviderLimit('acme-daily', 'Daily tokens', scope='account', kind='tokens',
            precision='exact', source='provider-api', used=25.49, limit=100.0)])


# Strict 0-param refresh: the runtime calls (f rejected-token); the adapter
# must DROP the extra arg and still return the fresh token.
def _refresh():
    return vis.ProviderCredential('sk-fresh-999', api_url='https://acme.test/v1')


# 1-param refresh: must RECEIVE the rejected token the runtime threads in.
def _refresh_with_arg(rejected):
    return vis.ProviderCredential('sk-fresh-' + rejected)


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
    return [vis.ProviderModel(m['name'], context=262144, is_tool_call=True)
            for m in provider['models']]


_events = {'selected': None}


def _on_selected(event):
    # side-effect hook: capture the marshalled selection event.
    _events['selected'] = {'source': event['source'],
                           'provider_id': event['provider']['id']}


def seen_selected():
    '''Return the last on_selected event captured (test observation).'''
    return _events['selected']


vis.register(vis.Extension(
    name='provider-acme',
    description='Acme static-key provider fixture.',
    alias='acme',
    symbols=[vis.Symbol(seen_selected)],
    providers=[
        vis.Provider(
            id='acme',
            label='Acme AI',
            preset=vis.ProviderPreset(base_url='https://acme.test/v1',
                    api_style='openai',
                    default_models=['acme-large', 'acme-small'],
                    responses_path='/responses',
                    llm_headers={'X-Initiator': 'agent'},
                    extra_body={'temperature': 0.6, 'top_p': 0.95}),
            get_token_fn=_token,
            status_fn=_status,
            detect_fn=_detect,
            logout_fn=_logout,
            limits_fn=_limits,
            refresh_token_fn=_refresh,
            enrich_models_fn=_enrich,
            on_selected_fn=_on_selected,
        ),
        vis.Provider(
            id='acme-oauth',
            label='Acme OAuth',
            refresh_token_fn=_refresh_with_arg,
            auth_fn=_auth,
            auth_prompt_fn=_auth_prompt,
        ),
    ],
))
")

(defdescribe
  provider-test
  (it
    "a vis.Provider(...) registers a first-class provider descriptor (preset + every provider fn)"
    (with-loaded
      {"acme.py" provider-py}
      (fn [_ _]
        (let [ext
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
          ;; preset: one snake_case spelling per key, and `api_style` resolves to
          ;; svar's OWN dialect keyword (`openai` -> `:openai-compatible-chat`) —
          ;; the value svar's `case` dispatches on, not the author's spelling.
          (let [preset (:provider/preset p)]
            (expect (= "https://acme.test/v1" (:base-url preset)))
            (expect (= :openai-compatible-chat (:api-style preset)))
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
          (let [lines
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
          (expect (= {:token "sk-fresh-old-rejected-token"}
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
  python-provider-refresh-once-test
  (it
    "does not retry a refresh callback after its body raises"
    (with-loaded
      {"refresh_once.py"
       "import blockether.vis.extension as vis\ndef refresh(rejected=None):\n    vis.state['refresh_calls'] = vis.state.get('refresh_calls', 0) + 1\n    raise RuntimeError('fixture refresh failure')\ndef calls():\n    '''Read the number of refresh attempts.'''\n    return vis.state.get('refresh_calls', 0)\nvis.register(vis.Extension(name='refresh-once', description='Refresh regression', alias='once', symbols=[vis.Symbol(calls)], providers=[vis.Provider(id='refresh-once', label='Refresh once', refresh_token_fn=refresh)]))\n"}
      (fn [_ _]
        (let [provider (registry/provider-by-id :refresh-once)]
          (expect (nil? ((:provider/refresh-token-fn provider) "fixture-rejected")))
          (expect (= 1 (get-in ((symbol-fn (registered "refresh-once") 'calls)) [:result]))))))))

;; Regression: a MANAGED provider declared by a PYTHON extension was a flag that
;; never crossed the host boundary - `is_managed=True` in `vis.Provider(...)`
;; reached the entry decoder and every credential seam below still saw an
;; ordinary API-key provider: a key band, an `Add provider` row, a startable flow.
(def ^:private managed-provider-py
  "'''Corp gateway fixture - the runtime issues the credential.'''
import blockether.vis.extension as vis


def _token():
    return vis.ProviderCredential('issued-by-runtime', api_url='https://corp.test/v1')


vis.register(vis.Extension(
    name='provider-corp',
    description='Managed corporate gateway fixture.',
    providers=[
        vis.Provider(
            id='corp-managed',
            label='Corp Gateway',
            is_managed=True,
            preset=vis.ProviderPreset(base_url='https://corp.test/v1',
                    api_style='openai', default_models=['corp-large']),
            get_token_fn=_token,
        ),
        vis.Provider(
            id='corp-byok',
            label='Corp BYOK',
            preset=vis.ProviderPreset(base_url='https://corp.test/v1',
                    api_style='openai', default_models=['corp-large']),
        ),
    ],
))
")

(defdescribe
  python-managed-provider-test
  (it "carries is_managed=True from blockether.vis.Provider(...) to every credential seam"
      (with-loaded {"corp.py" managed-provider-py}
                   (fn [_ _]
                     ;; The DECLARED flag reaches the registry entry; an undeclared one stays
                     ;; absent rather than arriving as `false` by truthiness.
                     (expect (true? (:provider/is-managed (registry/provider-by-id :corp-managed))))
                     (expect (nil? (:provider/is-managed (registry/provider-by-id :corp-byok))))
                     (expect (true? (providers/managed? :corp-managed)))
                     (expect (= :managed (providers/auth-kind :corp-managed)))
                     (expect (= :api-key (providers/auth-kind :corp-byok)))
                     ;; No channel can ever post a key for it: the flow refuses to start.
                     (let [refusal (pauth/start-auth! :corp-managed)]
                       (expect (= false (:ok? refusal)))
                       (expect (= :auth-managed (:error refusal)))
                       (expect (= false (pauth/supported? :corp-managed))))
                     ;; And nobody has to add it: absent from the `Add provider` picker, and
                     ;; already standing in the fleet with no configuration at all.
                     (with-redefs [providers/configured-providers
                                   (constantly [])

                                   providers/configured-providers-cached
                                   (constantly [])]

                       (let [offered
                             (into #{} (map :id) (providers/available-presets))

                             bound
                             (into #{} (map :id) (providers/authenticated-preset-providers))]

                         (expect (contains? offered :corp-byok))
                         (expect (not (contains? offered :corp-managed)))
                         (expect (contains? bound :corp-managed))
                         (expect (not (contains? bound :corp-byok)))))))))

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
  "import blockether.vis.extension as vis\ndef detect():\n    handle = vis.shell({'command': 'printf regular-shell'})\n    result = handle.wait(30)\n    for _ in range(3):\n        if not result.get('timed_out'):\n            break\n        result = handle.wait(30)\n    return vis.ProviderCredential(result['out'], source='shell')\nvis.register(vis.Extension(name='shell-provider', description='shell provider', providers=[vis.Provider(id='shell-provider', label='Shell provider', detect_fn=detect)]))")

(def ^:private popen-probe-py
  "Issue #142's own reproduction as an extension: start a child, hand the pid
   the handle carries straight back to the host."
  "import subprocess\nimport blockether.vis.extension as vis\ndef probe():\n    '''await probe() -> {'pid'} — start a child and report the handle it got.'''\n    child = subprocess.Popen(['/bin/sleep', '39'])\n    return {'pid': child.pid, 'poll': child.poll()}\nvis.register(vis.Extension(name='popen-probe', description='popen probe', alias='popen', symbols=[vis.Symbol(probe, tag='observation')]))")

(defdescribe
  python-extension-process-boundary-test
  (it
    "lets a provider callback spawn a native subprocess outside any session"
    (with-loaded
      {"process_provider.py"
       "import subprocess\nimport blockether.vis.extension as vis\ndef detect():\n    result = subprocess.run(['/bin/sh', '-c', 'printf extension-native'], capture_output=True, text=True, check=True)\n    return vis.ProviderCredential(result.stdout, source='subprocess')\nvis.register(vis.Extension(name='process-provider', description='process provider', providers=[vis.Provider(id='process-provider', label='Process provider', detect_fn=detect)]))"}
      (fn [_ _]
        (expect (= {:token "extension-native" :source :subprocess}
                   ((:provider/detect-fn (registry/provider-by-id :process-provider))))))))
  ;; Regression, issue #142: through this same loader, `Popen(...)` started a real
  ;; child and handed the extension a handle whose pid was `1` — the interpreter's
  ;; own first child SLOT, and 1 is init — so nothing could ps, lsof or supervise
  ;; the child the extension had just started.
  (it "hands a tool the real OS pid of the child its Popen started"
      (with-loaded
        {"popen_probe.py" popen-probe-py}
        (fn [_ _]
          (let [result
                ((symbol-fn (registered "popen-probe") 'probe))

                pid
                (long (get-in result [:result "pid"]))

                found
                (ProcessHandle/of pid)]

            (try (expect (extension/envelope-success? result))
                 (expect (nil? (get-in result [:result "poll"]))
                         "the child was running when the tool returned")
                 (expect (.isPresent found) "the pid names a process the host can see")
                 (expect (str/includes? (str (.orElse (.command (.info ^ProcessHandle (.get found)))
                                                      ""))
                                        "sleep")
                         "and it is the child the extension started")
                 (finally (when (.isPresent found) (.destroy ^ProcessHandle (.get found)))))))))
  (it "keeps vis.shell unrestricted even while the invoking session jail is enabled"
      (with-loaded
        {"shell_provider.py" shell-provider-py}
        (fn [_ _]
          (let [detect
                (:provider/detect-fn (registry/provider-by-id :shell-provider))

                env
                {:session-id "jailed-session"
                 :security-policy {:jail-enabled true}
                 :jail-policy-fn (fn []
                                   (throw (ex-info "the regular extension shell touched the jail"
                                                   {})))}]

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
                     (let [detect
                           (:provider/detect-fn (registry/provider-by-id :shell-provider))

                           before
                           (toggles/enabled? "shell")]

                       (try (toggles/set-enabled! "shell" false)
                            (expect (false? (toggles/enabled? "shell")))
                            (expect (= {:token "regular-shell" :source :shell} (detect)))
                            (finally (toggles/set-enabled! "shell" before)))))))
  (it
    "reads and validates the latest merged config for every jailed_shell spawn"
    (let [latest-root
          (temp-dir)

          _
          (spit (io/file latest-root "latest.txt") "latest")

          configs
          (atom [{"workspace" {"filesystem" [{"id" "latest-root"
                                              "path" (.getCanonicalPath latest-root)}]}
                  "jail" {"enabled" true "filesystem" {"allow" ["latest-root"]}}}
                 {"this_key_is_invalid" true}])

          loads
          (atom 0)

          test-thread
          (Thread/currentThread)

          load-config
          config/load-config-raw]

      (with-redefs [config/load-config-raw (fn []
                                             (if (identical? test-thread (Thread/currentThread))
                                               (do (swap! loads inc)
                                                   (let [value (first @configs)]
                                                     (swap! configs subvec 1)
                                                     value))
                                               (load-config)))]
        (let [first-run (:result (shell/jailed-shell
                                   nil
                                   {"cwd" (.getCanonicalPath latest-root)
                                    "command" "test -r latest.txt && printf latest-policy"}))
              second-run (try (:result (shell/jailed-shell nil
                                                           {"cwd" (.getCanonicalPath latest-root)
                                                            "command" "printf must-not-run"}))
                              (catch Throwable t {"note" (ex-message t)}))]

          ;; Background readers must not consume this test's scripted spawn policies.
          (let [reader (future (try (config/load-config-raw) (catch Throwable _ nil)))]
            (try (expect (not= ::timeout (deref reader 5000 ::timeout)))
                 (finally (future-cancel reader))))
          ;; What this test is about is the policy snapshot read by each spawn, not
          ;; platform-specific enforcer diagnostics.
          (expect (str/ends-with? (str (get (jail-wait first-run) "out")) "latest-policy"))
          ;; The SECOND spawn re-reads config and finds it invalid, so it refuses.
          (expect (not= "must-not-run" (get second-run "out")))
          (expect (re-find #"Invalid Vis configuration" (str (get second-run "note"))))
          (expect (= 2 @loads))))))
  (it
    "keeps the latest and session-snapshot jail APIs distinct"
    (with-loaded
      {"jail.py"
       "import blockether.vis.extension as vis\ndef latest():\n    \"Use the latest jail.\"\n    return vis.jailed_shell({'command':'echo latest'})['out']\ndef session():\n    \"Use the session jail.\"\n    return vis.jailed_shell_session({'command':'echo session'}).wait(20)['out']\nvis.register(vis.Extension(name='jail', description='jail', alias='j', symbols=[vis.Symbol(latest), vis.Symbol(session)]))"}
      (fn [_ _]
        (let [ext
              (registered "jail")

              latest
              (symbol-fn ext 'latest)

              session
              (symbol-fn ext 'session)

              seen
              (atom [])

              env
              {:session-id "session-1" :jail-policy-fn (constantly {:disabled? true})}]

          (with-redefs [shell/jailed-shell (fn [actual-env opts]
                                             (swap! seen conj [actual-env opts])
                                             {"out" "latest"})]
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
       "import blockether.vis.extension as vis\ndef run():\n    \"Shell out.\"\n    r = vis.jailed_shell({'command': 'echo hi'})\n    return [r['out'], r['stage'], sorted(r.keys())]\nvis.register(vis.Extension(name='jail', description='jail', alias='j', symbols=[vis.Symbol(run)]))"}
      (fn [_ _]
        (let [run
              (symbol-fn (registered "jail") 'run)

              env
              {:session-id "session-1" :jail-policy-fn (constantly {:disabled? true})}]

          (with-redefs [shell/jailed-shell (fn [_env opts]
                                             (extension/success {:result {"out" (get opts "command")
                                                                          "stage" :run}
                                                                 :op :shell
                                                                 :metadata {:duration-ms 1}}))]
            (binding [extension/*current-environment* env]
              ;; Python sees the UNWRAPPED, deep-stringified `:result` only.
              (expect (= ["echo hi" "run" ["out" "stage"]] (:result (run))))))))))
  (it
    "hands an extension the SAME live handle the sandbox gets, not a raw dict"
    ;; Regression, handle audit: `__VisShell__` was applied only in the model's
    ;; sandbox, so an extension's only continuation was hand-authoring the
    ;; `{"op": "logs", "id": …}` grammar that the handle object replaced.
    (with-loaded
      {"jail.py"
       "import blockether.vis.extension as vis\ndef run():\n    \"Shell out.\"\n    sh = vis.jailed_shell({'command': 'echo hi', 'id': 'h'})\n    return [sh['id'], sh.logs(offset=0)['stage'], sh.wait(5)['stage'], sh.type('y')['stage'], sh.stop()['stage']]\nvis.register(vis.Extension(name='jail', description='jail', alias='j', symbols=[vis.Symbol(run)]))"}
      (fn [_ _]
        (let [run
              (symbol-fn (registered "jail") 'run)

              env
              {:session-id "session-1" :jail-policy-fn (constantly {:disabled? true})}]

          (with-redefs [shell/jailed-shell (fn [_env opts]
                                             (extension/success
                                               {:result {"id" (get opts "id")
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
       "import blockether.vis.extension as vis\ndef run():\n    \"Shell out.\"\n    return vis.jailed_shell({'command': 'nope'})\nvis.register(vis.Extension(name='jail', description='jail', alias='j', symbols=[vis.Symbol(run)]))"}
      (fn [_ _]
        (let [run
              (symbol-fn (registered "jail") 'run)

              env
              {:session-id "session-1" :jail-policy-fn (constantly {:disabled? true})}]

          (with-redefs [shell/jailed-shell (fn [_env _opts]
                                             (extension/failure {:error {:message
                                                                         "jail refused the command"}
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
     "import blockether.vis.extension as vis\ndef run():\n    return vis.jailed_shell({'echo first', 'echo second'})\nvis.register(vis.Extension(name='jail', description='jail', alias='j', symbols=[vis.Symbol(run)]))"}
    (fn [_ _]
      (let [run
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
  (let [pol
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
             (let [s (report "GET" "https://example.com/data"
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

;; /reload re-hydrates feature toggles

(defdescribe
  reload-slash-toggles-test
  "`/reload` is the ONE user-facing re-read of `vis.yml`. Toggles used to be
   hydrated only at process start (gateway `install-toggle-persistence!`, TUI
   `screen/run-chat!`), so `shell: false` in the YAML kept the tool live until a
   restart while `/reload` reported success."
  (it
    "the shell toggle edited in vis.yml applies after /reload — #64"
    (let [before
          (toggles/enabled? "shell")

          calls
          (atom [])]

      (toggles/set-value! "shell" true)
      (expect (true? (toggles/enabled? "shell")))
      (try (with-redefs [pyx/reload-python-extensions!
                         (fn [opts]
                           (swap! calls conj opts)
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
               (expect (= [{:sync-projects? false}] @calls))
               ;; #178: only explicit --sync authorizes preparation, even with shell off.
               (expect (false? (toggles/enabled? "shell")))
               (expect (= :error
                          (:slash/status (#'pyx/reload-slash {:command/argv ["--unknown"]}))))
               (expect (= 1 (count @calls)))
               (expect (= :ok (:slash/status (#'pyx/reload-slash {:command/argv ["--sync"]}))))
               (expect (= [{:sync-projects? false} {:sync-projects? true}] @calls))
               (expect (= :ok (:slash/status res)))))
           (expect (false? (toggles/enabled? "shell")))
           (finally (toggles/set-value! "shell" before))))))

;; Input Views — `vis.ask` blocks the extension until a channel answers

(defn- answer-pending!
  "Wait for an input View titled `title` to show up, then run `answer-fn`
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
import blockether.vis.extension as vis

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

vis.register(vis.Extension(name='asker', description='asker', alias='a',
              symbols=[vis.Symbol(ask_key), vis.Symbol(ask_cancelled),
                       vis.Symbol(ask_camel_case), vis.Symbol(ask_validated)]))
")

(defdescribe
  extension-reload-dialog-isolation-test
  ;; #192: reload in B must not revoke A's pending dialog callback or cleanup.
  (it
    "keeps both session workers alive through reload and secret cleanup"
    (with-fresh-loaded
      {"asker.py" asker-py}
      (fn [_ {:keys [ext-dir]}]
        (let [a
              (:python-context (ep/create-python-context {} nil {:worker? true} nil))

              b
              (:python-context (ep/create-python-context {} nil {:worker? true} nil))

              invoke
              (fn [ctx sym]
                (binding [extension/*current-environment* {:python-context ctx :session-id ctx}]
                  ((symbol-fn (registered "asker") sym))))

              pending
              (promise)

              continue
              (promise)

              answer
              (answer-pending! "Deploy"
                               (fn [id]
                                 (deliver pending id)
                                 (deref continue 15000 nil)
                                 (human-input/submit! id
                                                      {"env" "staging" "token" "synthetic-192"})))

              running
              (future (invoke a 'ask_key))]

          (try (expect (string? (deref pending 10000 nil)))
               (let [a-row
                     (get @@#'pyx/session-contexts [a "asker"])

                     a-worker
                     (worker/extension-worker-key a)]

                 (expect (some? (:context a-row)))
                 (binding [extension/*current-environment* {:python-context b :session-id b}]
                   (expect (= 0 (:failed (pyx/reload-python-extensions! {:dirs [(str ext-dir)]})))))
                 (expect (worker/worker-live? a-worker))
                 (expect (= a-row (get @@#'pyx/session-contexts [a "asker"])))
                 (expect (contains? @(get-in @@#'worker/workers [a-worker :peer :host-sessions])
                                    (:context a-row)))
                 (deliver continue true)
                 (expect (= {:is-accepted true} (deref answer 10000 ::timeout)))
                 (let [result (deref running 10000 ::timeout)]
                   (expect (= "synthetic-192" (get-in result [:result "token"])))
                   (expect (true? (get-in result [:result "forgotten"]))))
                 (expect (empty? (human-input/pending-requests))))
               (finally (deliver continue true)
                        (future-cancel running)
                        (ep/dispose-python-context! a)
                        (ep/dispose-python-context! b))))))))

(defdescribe extension-cancelled-caller-lease-test
             ;; #192: cancellation of the host caller is not completion of guest execution.
             (it "defers retirement until the outstanding worker reply settles"
                 (let [ctx
                       (pyx/build-context "cancelled-caller-192")

                       reply
                       (promise)]

                   (try (locking @#'pyx/session-contexts (#'pyx/acquire-context! ctx))
                        (pyx/close-context! ctx)
                        (let [released (with-redefs [worker/pending-replies (constantly [reply])]
                                         (#'pyx/release-settled-context! ctx))]
                          (expect (contains? @@#'pyx/live-contexts ctx))
                          (expect (contains? @(get-in @@#'worker/workers
                                                      [worker/shared-key :peer :host-sessions])
                                             ctx))
                          (expect (not (realized? released)))
                          (deliver reply {"value" nil})
                          (expect (not= ::timeout (deref released 10000 ::timeout)))
                          (expect (not (contains? @@#'pyx/live-contexts ctx))))
                        (finally (deliver reply {"value" nil}) (pyx/close-context! ctx))))))

(def ^:private reload-dialog-py
  "import os
import blockether.vis.extension as vis

def probe():
    'Return worker identity and admitted helper version.'
    import reload_helper_192
    return {'pid': os.getpid(), 'version': reload_helper_192.version}

def wait_probe(mode, timeout_ms):
    'Wait for synthetic input, then import admitted code and clean up.'
    answer = vis.ask('Reload lifecycle', [{'name': 'token', 'type': 'password'}], timeout_ms=timeout_ms)
    if not answer:
        return {'reason': answer.reason}
    handle = answer['token']
    try:
        matched = answer.reveal('token') == 'synthetic-192'
        if mode == 'failure':
            raise ValueError('primary-192')
        result = probe()
        result['matched'] = matched
        return result
    finally:
        vis.forget(handle)

vis.register(vis.Extension(name='reload-lifecycle', description='Reload lifecycle fixture.',
    alias='reload_lifecycle', symbols=[vis.Symbol(probe), vis.Symbol(wait_probe)]))
")

(defdescribe
  extension-reload-lifecycle-paths-test
  ;; #192: exercise real workers, dialog outcomes, lazy imports and secret cleanup.
  (doseq [mode [:submit :cancel :timeout :failure :teardown]]
    (it
      (str "drains the old session generation through " (name mode))
      (with-fresh-loaded
        {"extension.py" reload-dialog-py "reload_helper_192/__init__.py" "version = 1\n"}
        (fn [loaded {:keys [ext-dir]}]
          (expect (= 0 (:failed loaded)) (pr-str (pyx/load-failures)))
          (let [a (:python-context (ep/create-python-context {} nil {:worker? true} nil))
                b (:python-context (ep/create-python-context {} nil {:worker? true} nil))
                invoke (fn [ctx sym & args]
                         (binding [extension/*current-environment* {:python-context ctx
                                                                    :session-id ctx}]
                           (apply (symbol-fn (registered "reload-lifecycle") sym) args)))
                b-before (:result (invoke b 'probe))
                pending (promise)
                proceed (promise)
                answer (answer-pending! "Reload lifecycle"
                                        (fn [id]
                                          (deliver pending id)
                                          (deref proceed 15000 nil)
                                          (case mode
                                            :cancel
                                            (human-input/cancel! id)

                                            (:timeout :teardown)
                                            nil

                                            (human-input/submit! id {"token" "synthetic-192"}))))
                running (future
                          (invoke a 'wait_probe (name mode) (if (= mode :timeout) 3000 20000)))
                secret-count (count @@#'human-input/secrets)]

            (try (expect (string? (deref pending 10000 nil)))
                 (let [old-ctx (:context (get @@#'pyx/session-contexts [a "reload-lifecycle"]))
                       snapshot (get-in @@#'pyx/context-lifecycle [old-ctx :snapshot])]

                   (write-ext! ext-dir "reload_helper_192/__init__.py" "version = 2\n")
                   (binding [extension/*current-environment* {:python-context
                                                              (if (= mode :submit) a b)}]
                     (expect (= 0
                                (:failed (pyx/reload-python-extensions! {:dirs [(str ext-dir)]})))))
                   (let [b-after (:result (invoke b 'probe))]
                     (expect (= 2 (get b-after "version")))
                     (expect (= (get b-before "pid") (get b-after "pid"))))
                   (expect (.isDirectory (io/file snapshot)))
                   (expect (contains? @@#'pyx/live-contexts old-ctx))
                   (when (= mode :teardown) (pyx/close-session-contexts! a))
                   (deliver proceed true)
                   (deref answer 10000 ::timeout)
                   (let [result (deref running 10000 ::timeout)]
                     (case mode
                       :submit
                       (do (expect (= 1 (get-in result [:result "version"])))
                           (expect (true? (get-in result [:result "matched"])))
                           (expect (not= (get b-before "pid") (get-in result [:result "pid"]))))

                       :cancel
                       (expect (= "cancelled" (get-in result [:result "reason"])))

                       :timeout
                       (expect (= "timeout" (get-in result [:result "reason"])))

                       :teardown
                       (expect (str/includes? (pr-str result) "session lifecycle cancellation"))

                       :failure
                       (expect (str/includes? (pr-str result) "primary-192"))))
                   (loop [attempt 0]
                     (when (and (seq (human-input/pending-requests)) (< attempt 100))
                       (Thread/sleep 10)
                       (recur (inc attempt))))
                   (expect (empty? (human-input/pending-requests)))
                   (expect (= secret-count (count @@#'human-input/secrets)))
                   (when-not (= mode :teardown)
                     (expect (= 2 (get-in (invoke a 'probe) [:result "version"]))))
                   (expect (not (contains? @@#'pyx/live-contexts old-ctx)))
                   (expect (not (.exists (io/file snapshot)))))
                 (finally (deliver proceed true)
                          (ep/dispose-python-context! a)
                          (ep/dispose-python-context! b)
                          (future-cancel running)))))))))

(defdescribe
  python-human-input-test
  (it
    "vis.ask pauses the extension, then returns typed values with the password kept opaque"
    (with-loaded
      {"asker.py" asker-py}
      (fn [_ _]
        (let [ask-key
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
        (let [ask-validated
              (symbol-fn (registered "asker") 'ask_validated)

              drawn
              (atom nil)

              refused
              (atom nil)

              answered
              (answer-pending!
                "Sign up"
                (fn [id]
                  (reset! drawn (human-input/pending-request id))
                  ;; Both fields agree, so only the one-argument
                  ;; validator refuses this answer.
                  (reset! refused (human-input/submit! id {"email" "nope" "again" "nope"}))
                  (human-input/submit! id {"email" "a@b.c" "again" "a@b.c"})))

              result
              (:result (binding [extension/*current-environment* {:session-id "sess-ask"}]
                         (ask-validated)))]

          ;; The validator is a Python callable, invoked by the engine from the
          ;; submitting thread while `vis.ask` parks the extension's own thread
          ;; inside the host call — CPython releases the GIL for it.
          (expect (= {:is-accepted false :errors {"email" "must be an email address"}} @refused))
          ;; A refusal keeps the request open, so the next confirmation answers it.
          (expect (= {:is-accepted true} @answered))
          ;; And the function itself never reaches a channel.
          (expect (every? #(not (contains? % :validate)) (:fields @drawn)))
          (expect (= {"ok" true "email" "a@b.c"} result))
          (expect (empty? (human-input/pending-requests)))))))
  ;; Regression, issue #104: `vis.ask` raised from an extension SYMBOL was
  ;; reported to reach nobody — no dialog on any channel and not one
  ;; `view.open` in the gateway journal — on the theory that a symbol
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
        (let [seen
              (atom [])

              ask-key
              (get (extension/wrap-extension (registered "asker")
                                             {:session-id "sid-104" :db-info store})
                   'ask_key)]

          (channel-events/add-channel-event-listener! :tui ::issue-104 #(swap! seen conj [:tui %]))
          (channel-events/add-channel-event-listener! :app ::issue-104 #(swap! seen conj [:app %]))
          (try (let [answered (answer-pending!
                                "Deploy"
                                #(human-input/submit! % {"env" "prod" "token" "hunter2"}))]
                 (ask-key)
                 (expect (= {:is-accepted true} (deref answered 10000 ::never))))
               (let [opened (filterv #(= :view/open (:op (second %))) @seen)]
                 (expect (= [:tui :app] (mapv first opened)))
                 (expect (= ["sid-104" "sid-104"]
                            (mapv #(get-in (second %) [:view :session-id]) opened))))
               (finally (channel-events/remove-channel-event-listener! :tui ::issue-104)
                        (channel-events/remove-channel-event-listener! :app ::issue-104)))))))
  (it "a cancelled request returns a falsey answer instead of raising"
      (with-loaded {"asker.py" asker-py}
                   (fn [_ _]
                     (let [ask-cancelled
                           (symbol-fn (registered "asker") 'ask_cancelled)

                           answered
                           (answer-pending! "Confirm" #(human-input/cancel! % "dismissed"))

                           result
                           (:result (binding [extension/*current-environment* {:session-id
                                                                               "sess-ask"}]
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

;; Hook callbacks run inside the caller's session env (issue #101)

(def ^:private hook-asker-py
  "
import blockether.vis.extension as vis

def hook_prompt(env):
    answer = vis.ask('HookAsk', [{'name': 'go', 'type': 'checkbox'}], timeout_ms=20000)
    return 'hook:' + str(answer.reason)

vis.register(vis.Extension(name='hookasker', description='hookasker', alias='hk', prompt=hook_prompt))
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
                                (let [prompt-fn
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

;; Torn-down contexts heal instead of dying (issues #102, #103)

(def ^:private rebuilder-py
  "import blockether.vis.extension as vis

def ping():
    '''await ping() -> str — answer pong.'''
    return 'pong'

def boom():
    '''await boom() -> str — always raises.'''
    raise ValueError('kaboom')

vis.register(vis.Extension(name='rebuilder', description='rebuilder', alias='rb',
              symbols=[vis.Symbol(ping), vis.Symbol(boom)]))
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
                     (let [captured
                           (symbol-fn (registered "rebuilder") 'ping)

                           dead
                           (:context (first (vals @@#'pyx/loaded)))]

                       (pyx/close-context! dead)
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
                     (let [live (:context (first (vals @@#'pyx/loaded)))]
                       (expect (false? (#'pyx/context-dead? live)))
                       (expect (false? (:success? ((symbol-fn (registered "rebuilder") 'boom)))))
                       (expect (false? (#'pyx/context-dead? live)))
                       (pyx/close-context! live)
                       (expect (true? (#'pyx/context-dead? live)))
                       (expect (true? (#'pyx/context-dead? nil)))))))
             (it "an ordinary Python error stays a failure and never rebuilds the context"
                 (with-fresh-loaded
                   {"rebuilder.py" rebuilder-py}
                   (fn [_ _]
                     (let [before
                           (:context (first (vals @@#'pyx/loaded)))

                           res
                           ((symbol-fn (registered "rebuilder") 'boom))]

                       (expect (false? (:success? res)))
                       (expect (str/includes? (get-in res [:error :message]) "kaboom"))
                       (expect (identical? before (:context (first (vals @@#'pyx/loaded))))))))))

;; Human-input form builders on the `vis` module

(def ^:private forms-py
  "'''Form builder fixture: composes a request and checks it without asking.'''
import blockether.vis.extension as vis


def _refusal(title, fields, **options):
    '''The one line `vis.ask` refuses this form with, or None when it takes it.

    The engine's refusal crosses as a foreign exception - which is NOT an
    `Exception` - reading `<host.class.Name>: <message>`, while a builder refuses
    a shape it never had with a plain Python error. The author reads the message
    either way.
    '''
    try:
        vis.ask(title, fields, **options)
    except BaseException as exc:
        head, separator, rest = str(exc).partition(': ')
        return rest if separator and '.' in head and ' ' not in head else str(exc)
    return None


def forms_report():
    '''await forms_report() -> {'kind', ...} - build a form, let the engine judge it.'''
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
        'kind': good['type'] + ':' + good['direction'],
        'ink': good['fields'][0],
        'slider_type': good['fields'][2]['fields'][1]['type'],
        'bad_option': _refusal('Deploy', [vis.select('env', [])]),
        'bad_track': _refusal('Deploy', [vis.slider('canary', min=5, max=2)]),
        'bad_names': _refusal('Deploy', [vis.plaintext('who'), vis.password('who')]),
        'bad_title': _refusal('', [vis.plaintext('who')]),
        'bad_key': _refusal('Deploy', [vis.plaintext('who', required=True)]),
        'bad_validator': _refusal('Deploy', [vis.plaintext('who', validate=lambda: None)]),
        'not_a_validator': _refusal('Deploy', [vis.plaintext('who', validate='nope')]),
    }


vis.register(vis.Extension(
    name='forms',
    description='Form builder fixture extension.',
    version='0.1.0',
    kind='integration',
    alias='forms',
    symbols=[vis.Symbol(forms_report, tag='observation')],
))
")

(defdescribe
  human-input-builders-test
  (it
    "gives Python extensions the same form builders, and lets the engine refuse a bad one"
    (with-loaded {"forms.py" forms-py}
                 (fn [_ _]
                   (let [ext
                         (registered "forms")

                         res
                         (:result ((symbol-fn ext 'forms_report)))]

                     ;; the builders compose plain wire data: a group, and nameless ink
                     (expect (= "group:column" (get res "kind")))
                     (expect (= {"type" "heading" "text" "Target"} (get res "ink")))
                     ;; `slider` is spelled so it never shadows the `range` builtin
                     (expect (= "range" (get res "slider_type")))
                     ;; and every mistake is RAISED by `vis.ask` itself, carrying the
                     ;; engine's own one-line reason — the line the human never had to see
                     (expect (= "Invalid input View field env: select needs at least one option"
                                (get res "bad_option")))
                     (expect (= "Invalid input View field canary: :max must be greater than :min"
                                (get res "bad_track")))
                     (expect (= "Invalid input View request: field names must be distinct"
                                (get res "bad_names")))
                     (expect (str/includes? (get res "bad_title") "non-blank :title"))
                     (expect (str/includes? (get res "bad_key") "unknown field key"))
                     ;; a validator is CODE: it never crosses the wire, so only its
                     ;; SHAPE is judged, and it is judged where it was written
                     (expect (= (str "a validate function takes the value, or the value "
                                     "and every value - this one takes neither")
                                (get res "bad_validator")))
                     (expect (str/includes? (get res "not_a_validator")
                                            "validate is a function, or a list of functions")))))))

;; Regression, issue #118: a Python provider could never publish live account
;; usage. Its `limits_fn` row came back with `:unlimited?` instead of the host
;; schema's `:is-unlimited`, so every row failed `contract.provider/limit-row`, the
;; whole report was replaced by an invalid-report error, and the TUI footer showed
;; "limits: error (Provider limits fn returned an invalid report)".
(defdescribe python-provider-limits-test
             (it "a Python limits_fn yields a valid report the footer can render"
                 (with-loaded {"acme.py" provider-py}
                              (fn [_ _]
                                (provider-limits/flush-limits-cache!)
                                (let [report
                                      (provider-limits/provider-limits :acme)

                                      row
                                      (first (get-in report [:dynamic :limits]))]

                                  (expect (= :ok (:status report)))
                                  (expect (nil? (:error report)))
                                  ;; the canonical boolean survives the Python boundary verbatim
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
import blockether.vis.extension as vis


def _status():
    return vis.ProviderStatus(True, source='env-var', extra={'is_stale': False})


vis.register(vis.Extension(
    name='provider-iskeys',
    description='is_* key spelling fixture.',
    providers=[
        vis.Provider(
            id='iskeys',
            label='Is Keys',
            preset=vis.ProviderPreset(base_url='https://iskeys.test/v1', is_hidden=True),
            status_fn=_status,
        ),
    ],
))
")

(defdescribe python-provider-is-key-test
             (it "every `is_*` key crosses as `:is-*` — one mechanical inverse, no allow-list"
                 (with-loaded {"iskeys.py" provider-is-keys-py}
                              (fn [_ _]
                                (let [p
                                      (registry/provider-by-id :iskeys)

                                      status
                                      ((:provider/status-fn p))]

                                  (expect (true? (:is-authenticated status)))
                                  ;; the key no allow-list ever named
                                  (expect (false? (:is-stale status)))
                                  (expect (nil? (:stale? status)))
                                  (expect (true? (:is-hidden (:provider/preset p))))
                                  (expect (nil? (:hidden? (:provider/preset p)))))))))

;; Freshness — only a process start and `/reload` put new bytes live

(def ^:private sidecar-py
  "import blockether.vis.extension as vis

def peek():
    '''await peek() -> str — the sidecar module's version, read at call time.'''
    import sidecar_impl
    return sidecar_impl.VERSION

vis.register(vis.Extension(name='sidecar', description='sidecar', alias='sd',
              symbols=[vis.Symbol(peek)]))
")

(def ^:private sidecar-impl-py "VERSION = 'v1'\n")

(defdescribe
  extension-freshness-test
  (it "an implicit load never adopts an edited file - only /reload does"
      ;; `ensure-python-extensions-loaded!` is what a session env cache miss, an
      ;; env recycle and a child env call. None of them is a human
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
      (let [ext-dir
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
  ;; Regression, Vis session ae259fdd-2712-4591-8f12-e1cdff30b208: the slash
  ;; request and first agent environment could construct CPython concurrently.
  (it
    "serializes concurrent first-load requests"
    (let [fingerprint
          @#'pyx/last-fingerprint

          previous
          @fingerprint

          calls
          (atom 0)

          worker-count
          8

          ready
          (java.util.concurrent.CountDownLatch. worker-count)

          start
          (java.util.concurrent.CountDownLatch. 1)]

      (try (reset! fingerprint nil)
           (with-redefs [pyx/load-python-extensions! (fn [_]
                                                       (swap! calls inc)
                                                       (Thread/sleep 100)
                                                       (reset! fingerprint [::loaded])
                                                       {:loaded 1 :failed 0 :changed? true})]
             (let [workers (mapv (fn [_]
                                   (future (.countDown ready)
                                           (.await start)
                                           (pyx/ensure-python-extensions-loaded! {:dirs []})))
                                 (range worker-count))]
               (expect (.await ready 5 java.util.concurrent.TimeUnit/SECONDS))
               (.countDown start)
               (expect (every? map? (mapv #(deref % 5000 ::timeout) workers)))
               (expect (= 1 @calls))))
           (finally (reset! fingerprint previous)))))
  (it "a torn-down context never heals into edited bytes"
      ;; The heal path re-executes the extension with no human act in the chain,
      ;; so it may only re-run the bytes this process loaded.
      (with-fresh-loaded
        {"rebuilder.py" rebuilder-py}
        (fn [_ {:keys [ext-dir]}]
          (let [captured
                (symbol-fn (registered "rebuilder") 'ping)

                dead
                (:context (first (vals @@#'pyx/loaded)))]

            (write-ext! ext-dir "rebuilder.py" (str/replace rebuilder-py "'pong'" "'edited'"))
            (pyx/close-context! dead)
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
          (let [captured
                (symbol-fn (registered "sidecar") 'peek)

                dead
                (:context (first (vals @@#'pyx/loaded)))]

            (write-ext! ext-dir "sidecar/sidecar_impl.py" (str/replace sidecar-impl-py "v1" "v2"))
            (pyx/close-context! dead)
            (expect (not= "v2" (:result (captured))))
            (expect (identical? dead (:context (first (vals @@#'pyx/loaded)))))))))
  (it "a torn-down context heals from the frozen tree when nothing changed"
      (with-fresh-loaded
        {"sidecar/sidecar_impl.py" sidecar-impl-py "sidecar/extension.py" sidecar-py}
        (fn [_ _]
          (let [captured
                (symbol-fn (registered "sidecar") 'peek)

                dead
                (:context (first (vals @@#'pyx/loaded)))]

            (pyx/close-context! dead)
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
      (let [ext-dir
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

(def ^:private fs-door-py
  "\"\"\"Filesystem door fixture.\"\"\"
import blockether.vis.extension as vis


def fsdoor_probe(path, text):
    \"\"\"await fsdoor_probe(path, text) -> dict — write and read back through the host's door.\"\"\"
    vis.fs.mkdir(path.rsplit(\"/\", 1)[0])
    vis.fs.write(path, text)
    vis.fs.copy(path, path + \".copy\")
    vis.fs.move(path + \".copy\", path + \".moved\")
    return {
        \"exists\": vis.fs.exists(path),
        \"read\": vis.fs.read_text(path),
        \"moved\": vis.fs.read_text(path + \".moved\"),
        \"listed\": sorted(vis.fs.list(path.rsplit(\"/\", 1)[0])),
        \"removed\": vis.fs.remove(path),
        \"gone\": vis.fs.exists(path),
    }


vis.register(vis.Extension(
    name=\"fsdoor\",
    description=\"Reads and writes through the host's door.\",
    kind=\"integration\",
    alias=\"fsdoor\",
    symbols=[vis.Symbol(fsdoor_probe, tag=\"mutation\")],
))
")

(defdescribe
  filesystem-door-test
  ;; The extension's own filesystem, performed by the HOST. Under a jail the
  ;; interpreter refuses the extension's own files to Python — they are not the
  ;; session's roots — so the door is how a trusted extension still reaches them,
  ;; exactly as `shell` is how it still reaches a process. What makes it a door
  ;; and not a widening is that a sandbox block cannot borrow it: it is bound in
  ;; extension namespaces only, and the host authorizes on the session the
  ;; interpreter names.
  (it "writes, reads, lists and removes a path the session never named"
      (with-loaded
        {"fsdoor.py" fs-door-py}
        (fn [_ _]
          (let [outside
                (str (System/getProperty "java.io.tmpdir") "/vis-fs-door-" (System/nanoTime))

                target
                (str outside "/note.txt")

                ext
                (registered "fsdoor")

                answer
                (:result ((symbol-fn ext 'fsdoor_probe) target "written by the door"))]

            (expect (= true (get answer "exists")))
            (expect (= "written by the door" (get answer "read")))
            (expect (= "written by the door" (get answer "moved")))
            (expect (= ["note.txt" "note.txt.moved"] (get answer "listed")))
            (expect (= true (get answer "removed")))
            (expect (= false (get answer "gone"))))))))

(defdescribe
  repository-sdk-check-extension-test
  (it
    "loads sdk.check through the real host and returns typed ordered gate evidence"
    (foundation/register!)
    (with-loaded
      {"sdk_checks.py"
       (str
         (slurp (io/file ".vis/extensions/sdk_checks.py"))
         "\n_run_process = _run\n"
         "def _run(name, argv, cwd, env, timeout_s):\n"
         "    return _run_process(name, ['/bin/sh', '-c', 'printf fixture; exit 1'], cwd, env, timeout_s)\n")}
      (fn [loaded _]
        (expect (= 1 (:loaded loaded)))
        (expect (= 0 (:failed loaded)))
        (let [ext
              (registered "sdk")

              check
              (symbol-fn ext 'sdk.check)

              reply
              (check (.getCanonicalPath (io/file ".")))

              result
              (:result reply)

              attrs
              (get result "__vis_attrs__")

              first-step
              (first (get attrs "steps"))]

          (expect (:success? reply) (get-in reply [:error :message]))
          (expect (= "CheckReport" (get result "__vis_object__")))
          (expect (= false (get attrs "is_engine_checked")))
          (expect (= false (get attrs "is_pass")))
          (expect (= "CheckResult" (get first-step "__vis_object__")))
          (expect (= "lint" (get-in first-step ["__vis_attrs__" "name"])))
          (expect (= "fixture" (get-in first-step ["__vis_attrs__" "output_tail"])))
          (expect (= 1 (get-in first-step ["__vis_attrs__" "exit_code"]))))))))

(defdescribe
  pyproject-package-reload-test
  (it
    "prepares a pyproject package automatically and reloads source with last-good fallback"
    (let [prepares
          (atom 0)

          fail?
          (atom false)]

      (with-redefs [python-runtime/ensure-project!
                    (fn [_]
                      (swap! prepares inc)
                      (when @fail? (throw (ex-info "fixture preparation failure" {})))
                      (io/file (runtime/packages-dir)))]
        (with-fresh-loaded
          {"greeter/pyproject.toml"
           (str
             "[project]\nname='vis-greeter'\nversion='1.0.0'\n"
             "description='Greeting tools'\nrequires-python='>=3.11'\n"
             "dependencies=['vis-agent>=0.1.0']\n[tool.vis]\ncategory='tools'\nsource_paths=['src']\n")
           "greeter/extension.py"
           (str "import blockether.vis.extension as vis\nfrom center_greeter import hello\n"
                "vis.register(vis.Extension(name='vis-greeter', description='Greeting tools', "
                "alias='greeter', symbols=[vis.Symbol(hello)]))\n")
           "greeter/src/center_greeter.py"
           "def hello():\n    \"Return the greeting.\"\n    return 'one'\n"}
          (fn [result {:keys [ext-dir]}]
            (expect (= 0 (:failed result)))
            (expect (= 1 @prepares))
            (expect (= "1.0.0" (:ext/version (registered "vis-greeter"))))
            (expect (= "tools" (:ext/kind (registered "vis-greeter"))))
            (let [invoke #(:result ((symbol-fn (registered "vis-greeter") 'hello)))
                  opts {:dirs [(str ext-dir)]}]

              (expect (= "one" (invoke)))
              (write-ext! ext-dir
                          "greeter/src/center_greeter.py"
                          "def hello():\n    \"Return the greeting.\"\n    return 'two'\n")
              (expect (= "one" (invoke)))
              (expect (= 0 (:failed (pyx/reload-python-extensions! opts))))
              (expect (= "two" (invoke)))
              ;; Regression #178: retained tools must be explicitly marked stale.
              (reset! fail? true)
              (write-ext! ext-dir
                          "greeter/src/center_greeter.py"
                          "def hello():\n    return 'three'\n")
              (expect (= 1 (:failed (pyx/reload-python-extensions! opts))))
              (let [failure (first (pyx/load-failures))
                    prompt (:ext/prompt-fn (registered "python-extensions"))]

                (expect (true? (:stale? failure)))
                (expect (= "vis-greeter" (:extension failure)))
                (expect (not= (:loaded-fingerprint failure) (:requested-fingerprint failure)))
                (let [loader (registered "python-extensions")
                      context (#'prompt-context/turn-system-context-block {} [loader])
                      checks (#'pyx/doctor-fn {})]

                  (expect ((:ext/activation-fn loader) {}))
                  (expect (str/includes? context "tools and docs are stale"))
                  (expect (some #(and (= :error (:level %))
                                      (str/includes? (:message %) "tools and docs are stale"))
                                checks))
                  (expect (not-any? #(= :info (:level %)) checks)))
                (expect (and prompt (str/includes? (prompt {}) "stale"))))
              (expect (= "two" (invoke))))))))))

(defdescribe package-staging-is-not-scanned-test
             (it "ignores an incomplete hidden install directory"
                 (with-fresh-loaded {".install-incomplete/extension.py"
                                     "raise RuntimeError('not admitted')
"}
                                    (fn [result _]
                                      (expect (= 0 (:failed result)))
                                      (expect (= 0 (:loaded result)))))))

(defdescribe
  python-symbol-contract-test
  ;; Issues #176 and #179: wrapped typed metadata survives registration and reload.
  (it
    "uses one inert contract for keyword-only docs and callable inspection across reloads"
    (with-fresh-loaded
      {"contract_helpers/__init__.py"
       (str "from functools import wraps\n"
            "def wrapped(fn):\n    @wraps(fn)\n    def call(*args, **kwargs):\n"
            "        return fn(*args, **kwargs)\n    return call\n")
       "contract_tools.py"
       (str
         "from __future__ import annotations\n" "from dataclasses import dataclass\n"
         "from typing import Annotated\n" "from contract_helpers import wrapped\n"
         "import blockether.vis.extension as vis\n" "@dataclass(frozen=True)\n"
         "class Result:\n" "    \"A greeting result.\"\n"
         "    text: Annotated[str, 'Greeting text.']\n"
         "@dataclass(frozen=True)\nclass Results:\n    items: tuple[Result, ...]\n"
         "    opaque: object = None\n    missing: MissingResult = None\n"
         "class Greeter:\n    @vis.method(tag='mutation')\n    @wrapped\n"
         "    def hello(self, name: str, /, *, loud: bool = False, note: str | None = None) -> Results:\n"
         "        \"Greet one person without changing state.\"\n"
         "        return Results((Result(name.upper() if loud else name),))\n"
         "vis.register(vis.Extension(name='contract-tools', description='Contract tools', alias='greet', symbols=[vis.Symbol(Greeter(), name='greet')]))\n")}
      (fn [result {:keys [ext-dir]}]
        (expect (= 1 (:loaded result)))
        (let [made
              (ep/create-python-context {} nil {:worker? true} nil)

              ctx
              (:python-context made)

              env
              {:python-context ctx :extensions (atom []) :active-extensions (atom [])}]

          (try
            (dotimes [iteration 2]
              (let [ext (registered "contract-tools")
                    entry (first (get-in ext [:ext/engine :ext.engine/symbols]))]

                (expect (= "greet.hello" (get-in entry [:ext.symbol/contract "name"])))
                (reset! (:extensions env) [ext])
                (lp/sync-active-extension-symbols! env [ext])
                (let
                  [answer
                   (ep/run-python-block
                     ctx
                     (str
                       "import json, inspect, typing\n"
                       "assert greet.hello.__annotations__ == {}\n"
                       "assert typing.get_type_hints(greet.hello) == {}\n"
                       "assert getattr(greet.hello, '__signature__', None) is None\n"
                       "assert greet.hello.contract['tag'] == 'mutation'\n"
                       "assert greet.hello.contract['returns']['fields'][0]['type']['variadic'] is True\n"
                       "assert 'tuple[Result, ...]' in doc('greet.hello')\n"
                       "assert greet.hello.contract['returns']['fields'][1]['type']['kind'] == 'opaque'\n"
                       "assert greet.hello.contract['returns']['fields'][2]['type']['kind'] == 'unresolved'\n"
                       "assert 'opaque: object (opaque)' in doc('greet.hello')\n"
                       "assert 'missing: MissingResult (unresolved)' in doc('greet.hello')\n"
                       "assert greet.hello.contract['name'] == 'greet.hello'\n"
                       "assert greet.hello.contract['parameters'][1]['kind'] == 'keyword_only'\n"
                       "assert 'loud: bool' in doc('greet.hello')\n"
                       "assert "
                       (pr-str (if (zero? iteration) "Greeting text." "Reloaded greeting text."))
                       " == greet.hello.contract['returns']['fields'][0]['type']['arguments'][0]['fields'][0]['type']['description']\n"
                       "assert "
                       (pr-str (if (zero? iteration) "Greeting text." "Reloaded greeting text."))
                       " in doc('greet.hello')\n"
                       "assert str(inspect.signature(greet.hello)) == '(name, /, *, loud=Ellipsis, note=None)'\n"
                       "assert inspect.signature(greet.hello).return_annotation is inspect.Signature.empty\n"
                       "assert all(p.annotation is inspect.Parameter.empty for p in inspect.signature(greet.hello).parameters.values())\n"
                       "value = await greet.hello('Ada', loud=True)\n"
                       "assert value.items[0].text == 'ADA'\n"
                       "assert (await greet.hello('Ada')).items[0].text == 'Ada'\n"
                       "try:\n    value.items[0].text = 'changed'\n"
                       "except AttributeError:\n    pass\n"
                       "else:\n    raise AssertionError('mutable nested result')\n"
                       "print(json.dumps(greet.hello.contract['returns']['fields'][0]['name']))"))]
                  (expect (nil? (:error answer)) (pr-str answer))
                  (expect (str/includes? (or (:stdout answer) "") "items"))))
              (when (zero? iteration)
                (write-ext! ext-dir
                            "contract_tools.py"
                            (str/replace (slurp (io/file ext-dir "contract_tools.py"))
                                         "Greeting text."
                                         "Reloaded greeting text."))
                (expect (= 1 (:loaded (pyx/reload-python-extensions! {:dirs [(str ext-dir)]}))))))
            (finally (ep/dispose-python-context! ctx))))))))

(defdescribe
  package-owned-skills-test
  ;; Issue #176: package procedures follow the same successful reload as code.
  (it
    "discovers qualified skills with resources and retains last-good on failure"
    (with-redefs [python-runtime/ensure-project! (fn [_]
                                                   (io/file (runtime/packages-dir)))]
      (with-fresh-loaded
        {"skillpack/pyproject.toml"
         (str "[project]\nname='vis-skillpack'\nversion='1.0.0'\n"
              "description='Skill package'\nrequires-python='>=3.11'\n"
              "dependencies=['vis-agent>=0.1.0']\n[tool.vis]\ncategory='workflows'\n"
              "skills=['skills/review']\n")
         "skillpack/extension.py"
         "import blockether.vis.extension as vis\nvis.register(vis.Extension(name='vis-skillpack', description='Skill package'))\n"
         "skillpack/skills/review/SKILL.md"
         "---\nname: review\ndescription: Review when requested.\n---\nRead v1.\n"
         "skillpack/skills/review/references/checklist.md" "Checklist v1."}
        (fn [result {:keys [ext-dir]}]
          (expect (= 1 (:loaded result)))
          (let [find-skill #(some (fn [s]
                                    (when (= "vis-skillpack/review" (:name s)) s))
                                  (discovery/skills))
                first-skill (find-skill)
                entry (io/file ext-dir "skillpack/extension.py")
                skill-file (io/file ext-dir "skillpack/skills/review/SKILL.md")
                ctx (:python-context (ep/create-python-context {} nil {:worker? true} nil))
                check-discovery (fn [body description]
                                  (let [answer (ep/run-python-block
                                                 ctx
                                                 (str "hits = apropos(r'^vis-skillpack/review$')\n"
                                                      (if body
                                                        (str "assert len(hits) == 1, repr(hits)\n"
                                                             "assert hits[0].type == 'skill'\n"
                                                             "assert hits[0].body == "
                                                             (json/generate-string description)
                                                             "\n"
                                                             "assert "
                                                             (json/generate-string body)
                                                             " in doc(hits[0])\n")
                                                        "assert not hits, repr(hits)\n")
                                                      "print('discovery verified')"))]
                                    (expect (nil? (:error answer)) (pr-str answer))
                                    (expect (str/includes? (or (:stdout answer) "")
                                                           "discovery verified"))))]

            (try (check-discovery "Read v1." "Review when requested.")
                 (expect (= "1.0.0" (get-in first-skill [:package :version])))
                 (expect (= ["references/checklist.md"] (:resources first-skill)))
                 (expect (= "Checklist v1."
                            (slurp (io/file (:dir first-skill) "references/checklist.md"))))
                 (expect (nil? (:project-root first-skill)))
                 (spit skill-file
                       "---\nname: review\ndescription: Review revised inputs.\n---\nRead v2.\n")
                 (expect (= (:body first-skill) (:body (find-skill))))
                 (check-discovery "Read v1." "Review when requested.")
                 (let [code (slurp entry)]
                   (spit entry "raise RuntimeError('fixture load failure')\n")
                   (expect (= 1 (:failed (pyx/reload-python-extensions! {:dirs [(str ext-dir)]}))))
                   (expect (= (:body first-skill) (:body (find-skill))))
                   (check-discovery "Read v1." "Review when requested.")
                   (spit entry code))
                 (expect (= 1 (:loaded (pyx/reload-python-extensions! {:dirs [(str ext-dir)]}))))
                 (expect (= "Read v2.\n" (:body (find-skill))))
                 (check-discovery "Read v2." "Review revised inputs.")
                 (pyx/reload-python-extensions! {:dirs []})
                 (expect (nil? (find-skill)))
                 (check-discovery nil nil)
                 (finally (ep/dispose-python-context! ctx)))))))))

(defdescribe
  authoring-example-test
  ;; #176: load the documented package itself, not a second copy of its snippets.
  (it "loads the one-file tutorial and calls its documented defaults in the sandbox"
      (let [source (second (re-find
                             #"(?s)```python\n# \.vis/extensions/greeting_tools\.py\n(.*?)\n```"
                             (slurp (io/resource "vis-docs/extending.md"))))]
        (expect (some? source))
        (with-fresh-loaded
          {"greeting_tools.py" source}
          (fn [result _]
            (expect (= 1 (:loaded result)) (pr-str result))
            (let [ext (registered "greeting")
                  ctx (:python-context (ep/create-python-context {} nil {:worker? true} nil))
                  env {:python-context ctx :extensions (atom [ext]) :active-extensions (atom [])}]

              (try (lp/sync-active-extension-symbols! env [ext])
                   (let [answer (ep/run-python-block
                                  ctx
                                  (str
                                    "hits = apropos(r'^hello$')\n"
                                    "assert len(hits) == 1, repr(hits)\n"
                                    "assert 'uppercase defaults to False' in doc(hits[0])\n"
                                    "assert hello.contract['parameters'][1]['has_default']\n"
                                    "assert await hello('Ada') == 'Hello, Ada!'\n"
                                    "assert await hello('Ada', uppercase=True) == 'HELLO, ADA!'\n"
                                    "print('tutorial verified')"))]
                     (expect (nil? (:error answer)) (pr-str answer))
                     (expect (str/includes? (or (:stdout answer) "") "tutorial verified")))
                   (finally (ep/dispose-python-context! ctx))))))))
  (it
    "exposes its contract, result, documentation and packaged skill in a real session"
    (let [example
          (io/file "packages/vis-agent/examples/greeter")

          files
          ["pyproject.toml" "extension.py" "src/vis_greeter/__init__.py" "skills/greeting/SKILL.md"
           "skills/greeting/references/style.md"]

          sources
          (into {}
                (map (fn [path]
                       [(str "greeter/" path) (slurp (io/file example path))]))
                files)]

      (with-redefs [python-runtime/ensure-project! (fn [_]
                                                     (io/file (runtime/packages-dir)))]
        (with-fresh-loaded
          sources
          (fn [result _]
            (expect (= 1 (:loaded result)) (pr-str result))
            (let [ext (registered "vis-greeter")
                  ctx (:python-context (ep/create-python-context {} nil {:worker? true} nil))
                  env {:python-context ctx :extensions (atom [ext]) :active-extensions (atom [])}]

              (try
                (lp/sync-active-extension-symbols! env [ext])
                (let
                  [answer
                   (ep/run-python-block
                     ctx
                     (str
                       "hits = apropos(r'^(?:greet[.]hello|vis-greeter/greeting|extension-(?:design|packages|development|api|troubleshooting))$')\n"
                       "assert len(hits) == 7, repr(hits)\n"
                       "by_name = {hit.name: hit for hit in hits}\n"
                       "tool_hit = by_name['greet.hello']\n"
                       "assert tool_hit.type == 'tool', repr(tool_hit)\n"
                       "assert tool_hit.body.startswith('Greet one person.'), repr(tool_hit)\n"
                       "assert 'Parameters:' not in tool_hit.body\n"
                       "assert 'Unicode code points' in doc(tool_hit)\n"
                       "assert 'uppercase: bool' in doc(tool_hit)\n"
                       "assert 'uppercase defaults to False' in doc(tool_hit)\n"
                       "assert doc(tool_hit) == doc('greet.hello')\n"
                       "assert all(0 < len(hit.body) <= 100 and '\\n' not in hit.body for hit in hits)\n"
                       "for name in ('extension-design', 'extension-packages', 'extension-development', 'extension-api', 'extension-troubleshooting'):\n"
                       "    hit = by_name[name]\n" "    assert hit.type == 'doc', repr(hit)\n"
                       "    page = doc(hit)\n" "    assert page.startswith('# '), page[:100]\n"
                       "    assert hit.body.casefold() != name.replace('-', ' '), repr(hit)\n"
                       "    assert '## See also' in page\n"
                       "skill_hit = by_name['vis-greeter/greeting']\n"
                       "assert skill_hit.type == 'skill', repr(skill_hit)\n"
                       "assert skill_hit.body.startswith('Use when the user requests a greeting'), repr(skill_hit)\n"
                       "assert 'vis-greeter@1.0.0' in doc(skill_hit)\n"
                       "assert 'references/style.md' in doc(skill_hit)\n"
                       "assert 'Do not send it to another service' in doc(skill_hit)\n"
                       "assert greet.hello.contract['name'] == 'greet.hello'\n"
                       "assert 'Unicode code points' in doc('greet.hello')\n"
                       "assert (await greet.hello('Ada')).text == 'Hello, Ada!'\n"
                       "assert (await greet.hello('Ada', uppercase=True)).text == 'HELLO, ADA!'\n"
                       "skill = doc('vis-greeter/greeting')\n"
                       "assert 'vis-greeter@1.0.0' in skill\n"
                       "assert 'references/style.md' in skill\n" "print('example verified')"))
                   template (some #(when (= "skill:vis-greeter/greeting" (:name %)) %)
                                  (#'harness/skill-template-entries))]

                  (expect (nil? (:error answer)) (pr-str answer))
                  (expect (str/includes? (or (:stdout answer) "") "example verified"))
                  (expect (some? template))
                  (expect (nil? (:project-root template)))
                  (expect (str/includes? ((:expand-fn template) {} "for Ada")
                                         "references/style.md")))
                (finally (ep/dispose-python-context! ctx))))))))))

(defn- with-watchdog-extension
  "Run a test with a real sandbox and a separate trusted extension worker."
  [f]
  (with-loaded
    {"watchdog_probe.py"
     (slurp "e2e/scenarios/extension-watchdog/files/.vis/extensions/watchdog_probe.py")}
    (fn [loaded _]
      (expect (zero? (:failed loaded)) (pr-str (pyx/load-failures)))
      (let [sandbox-root
            (temp-dir)

            ctx
            (:python-context (ep/create-python-context {}
                                                       (fn []
                                                         [(.getCanonicalPath sandbox-root)])
                                                       {:worker? true
                                                        :jail-enabled? true
                                                        :enabled? false
                                                        :allowed-domains []
                                                        :denied-domains []
                                                        :exclude-domains []}
                                                       nil))

            ext
            (registered "watchdog-probe")

            env
            {:python-context ctx
             :session-id (str "watchdog-" (random-uuid))
             :extensions (atom [ext])
             :active-extensions (atom [])}]

        (try (lp/sync-active-extension-symbols! env [ext])
             ;; Worker startup is not what these deliberately short budgets measure.
             (let [warm (ep/run-python-block ctx "await watchdog_probe.poll(0)")]
               (expect (nil? (:error warm)) (pr-str warm)))
             (f ctx env)
             (finally (ep/dispose-python-context! ctx)))))))

(defn- watchdog-block
  [ctx env code & {:keys [tool-event-fn]}]
  (with-redefs [rt/MIN_EVAL_TIMEOUT_MS 1000]
    (binding [rt/*eval-timeout-ms* 1000]
      ;; A source-code timeout heuristic must not hide this regression.
      (expect (= 1000 (rt/eval-timeout-ms-for-code 1000 code)))
      (#'lp/run-python-code ctx code :env env :tool-event-fn tool-event-fn))))

(defdescribe
  python-extension-execution-budget-test
  ;; Issue #187 and session 633cdc58-89fe-4b3d-88ec-caa624d118ed:
  ;; reproduce silent calls and helpers, never replay historical deployments.
  (it "lets a silent extension outlive execution and grants a full budget on return"
      (with-watchdog-extension
        (fn [ctx env]
          (let [result (watchdog-block ctx
                                       env
                                       (str "import time\n"
                                            "time.sleep(0.65)\n"
                                            "answer = await watchdog_probe.poll(1.3)\n"
                                            "time.sleep(0.65)\n"
                                            "print(answer.count, answer.timed_out, 'resumed')\n"))]
            (expect (nil? (:error result)) (pr-str result))
            (expect (not (:timeout? result)))
            (expect (str/includes? (or (:stdout result) "") "2 False resumed"))
            (expect (not (worker/retired? ctx)))))))
  (it "resolves a previous-block helper and computed duration without replaying calls"
      (with-watchdog-extension
        (fn [ctx env]
          (let [defined
                (watchdog-block ctx
                                env
                                (str "async def wait_release_progress():\n"
                                     "    duration = sum([0.7, 0.6])\n"
                                     "    return await watchdog_probe.poll(duration)\n"))

                result
                (watchdog-block ctx
                                env
                                (str "import time\n" "for _ in range(3):\n"
                                     "    time.sleep(0.6)\n"
                                     "    observation = await wait_release_progress()\n"
                                     "    print(observation.count)\n" "time.sleep(0.6)\n"))]

            (expect (nil? (:error defined)) (pr-str defined))
            (expect (nil? (:error result)) (pr-str result))
            (expect (= "2\n3\n4" (str/trim (or (:stdout result) ""))))))))
  (it "keeps overlapping calls parked until the last one finishes"
      (with-watchdog-extension
        (fn [ctx env]
          (let [result
                (watchdog-block
                  ctx
                  env
                  (str
                    "import time\n"
                    "time.sleep(0.6)\n"
                    "answers = await gather(watchdog_probe.poll(0.2), watchdog_probe.poll(1.6))\n"
                    "time.sleep(0.6)\n" "print(sorted(answer.count for answer in answers))\n"))]
            (expect (nil? (:error result)) (pr-str result))
            (expect (str/includes? (or (:stdout result) "") "[2, 3]"))))))
  (it "restores the budget after a handled extension exception"
      (with-watchdog-extension
        (fn [ctx env]
          (let [result (watchdog-block ctx
                                       env
                                       (str "import time\n" "time.sleep(0.6)\n"
                                            "try:\n" "    await watchdog_probe.fail(1.3)\n"
                                            "except Exception:\n" "    print('handled')\n"
                                            "time.sleep(0.6)\n" "print('continued')\n"))]
            (expect (nil? (:error result)) (pr-str result))
            (expect (str/includes? (or (:stdout result) "") "handled\ncontinued"))))))
  (it "preserves an extension-owned timeout as data and continues the same block"
      (with-watchdog-extension
        (fn [ctx env]
          (let [result (watchdog-block ctx
                                       env
                                       (str "import time\n" "time.sleep(0.6)\n"
                                            "observation = await watchdog_probe.deadline(1.3)\n"
                                            "assert observation.timed_out\n"
                                            "time.sleep(0.6)\n"
                                            "print('operation timed out; execution resumed')\n"))]
            (expect (nil? (:error result)) (pr-str result))
            (expect (not (:timeout? result)))
            (expect (str/includes? (or (:stdout result) "") "execution resumed"))))))
  (it "stops runaway Python after a returned extension and preserves the context"
      (with-watchdog-extension
        (fn [ctx env]
          (let [result
                (watchdog-block ctx env "await watchdog_probe.poll(1.3)\nwhile True:\n    pass\n")

                next-result
                (watchdog-block ctx env "print((await watchdog_probe.poll(0)).count)\n")]

            (expect (true? (:timeout? result)))
            (expect (not (worker/retired? ctx)))
            (expect (nil? (:error next-result)) (pr-str next-result))
            (expect (= "3" (str/trim (or (:stdout next-result) "")))))))))

(defdescribe
  python-extension-watchdog-lifecycle-test
  ;; Issue #187: parking the execution clock must not hide cancellation or death.
  (it "still lets the user cancel an in-flight extension"
      (with-watchdog-extension
        (fn [ctx env]
          (let [token
                (cancellation/cancellation-token)

                started
                (promise)

                stopper
                (future (when (= true (deref started 5000 :timeout))
                          (Thread/sleep 200)
                          (cancellation/cancel! token :client-cancel-turn)))]

            (try (let [result (watchdog-block
                                ctx
                                (assoc env :cancel-token token)
                                "await watchdog_probe.poll(30)\nprint('must not resume')\n"
                                :tool-event-fn
                                #(when (= :start (:phase %)) (deliver started true)))]
                   (expect (cancellation/cancelled? token))
                   (expect (some? (:error result)))
                   (expect (not (:timeout? result)))
                   (expect (not (str/includes? (or (:stdout result) "") "must not resume"))))
                 (finally (cancellation/cancel! token :test-cleanup) (deref stopper 5000 nil)))))))
  (it "surfaces a terminated extension worker instead of waiting forever"
      (with-watchdog-extension
        (fn [ctx env]
          (let [started
                (promise)

                stopper
                (future (when (= true (deref started 5000 :timeout))
                          (Thread/sleep 200)
                          (worker/stop-worker! (worker/extension-worker-key ctx))))]

            (try (let [result (watchdog-block ctx
                                              env
                                              "await watchdog_probe.poll(30)\n"
                                              :tool-event-fn
                                              #(when (= :start (:phase %)) (deliver started true)))]
                   (expect (some? (:error result)))
                   (expect (not (:timeout? result)))
                   (expect (not (worker/worker-live? (worker/extension-worker-key ctx)))))
                 (finally (deref stopper 5000 nil)))))))
  (it "does not classify a silent real extension as a stalled turn"
      (with-watchdog-extension
        (fn [ctx env]
          (let [started
                (promise)

                checked
                (promise)

                inspector
                (future (when (= true (deref started 5000 :timeout))
                          (Thread/sleep 1100)
                          (deliver
                            checked
                            (#'gateway-state/turn-stall-decision
                             (#'gateway-state/advance-turn-stall-state
                              {:started? true :first-output-timeout-ms 10 :stall-timeout-ms 10}
                              {:phase :tool-start}
                              0)
                             900000))))]

            (try (let [result (watchdog-block ctx
                                              env
                                              "print((await watchdog_probe.poll(1.6)).count)\n"
                                              :tool-event-fn
                                              #(when (= :start (:phase %)) (deliver started true)))]
                   (expect (nil? (:error result)) (pr-str result))
                   (expect (= false (:tripped? (deref checked 3000 nil)))))
                 (finally (deref inspector 5000 nil))))))))

(defdescribe
  python-extension-watchdog-cross-validation-test
  ;; Issue #187: exercise different worker paths and verify a disabled fix is detected.
  (it "detects the original timeout when dispatch parking is deliberately disabled"
      (with-watchdog-extension
        (fn [ctx env]
          (with-redefs [rt/park-blocking-wall (fn [thunk]
                                                (thunk))]
            (let [result (watchdog-block ctx env "await watchdog_probe.poll(1.6)\n")]
              (expect (true? (:timeout? result)) (pr-str result))
              (expect (some? (:error result))))))))
  (it "parks an asynchronous extension method through its await"
      (with-watchdog-extension
        (fn [ctx env]
          (let [result (watchdog-block ctx
                                       env
                                       (str "import time\ntime.sleep(0.6)\n"
                                            "observation = await watchdog_probe.poll_async(1.6)\n"
                                            "time.sleep(0.6)\nprint(observation.count)\n"))]
            (expect (nil? (:error result)) (pr-str result))
            (expect (= "2" (str/trim (or (:stdout result) ""))))))))
  (it "keeps a sibling call parked when a concurrent operation fails"
      (with-watchdog-extension
        (fn [ctx env]
          (let [events
                (atom [])

                result
                (watchdog-block
                  ctx
                  env
                  (str "async def recover():\n" "    try:\n        await watchdog_probe.fail(0.2)\n"
                       "    except Exception:\n        return 'handled'\n"
                       "answers = await gather(recover(), watchdog_probe.poll(1.6))\n"
                       "import time\ntime.sleep(0.6)\n" "print(answers[0], answers[1].count)\n")
                  :tool-event-fn
                  #(swap! events conj %))

                starts
                (filter #(= :start (:phase %)) @events)

                terminals
                (filter #(= :terminal (:phase %)) @events)]

            (expect (nil? (:error result)) (pr-str result))
            (expect (= "handled 2" (str/trim (or (:stdout result) ""))))
            (expect (= 2 (count starts)))
            (expect (= (set (map :invocation-id starts)) (set (map :invocation-id terminals))))))))
  (it "restores execution after an operation-owned TimeoutError"
      (with-watchdog-extension
        (fn [ctx env]
          (let [result
                (watchdog-block ctx
                                env
                                (str "import time\ntime.sleep(0.6)\n"
                                     "try:\n    await watchdog_probe.expire(1.6)\n"
                                     "except Exception as exc:\n"
                                     "    assert 'extension observation expired' in str(exc)\n"
                                     "    print('operation expired')\n"
                                     "time.sleep(0.6)\nprint('resumed')\n"))

                runaway
                (watchdog-block ctx env "while True:\n    pass\n")]

            (expect (nil? (:error result)) (pr-str result))
            (expect (not (:timeout? result)))
            (expect (= "operation expired\nresumed" (str/trim (or (:stdout result) ""))))
            (expect (true? (:timeout? runaway)))
            (expect (not (worker/retired? ctx)))))))
  (it "retires an uncooperative extension after cancellation instead of leaking work"
      (with-watchdog-extension
        (fn [ctx env]
          (let [token
                (cancellation/cancellation-token)

                retired
                (atom false)

                started
                (promise)

                stopper
                (future (when (= true (deref started 5000 :timeout))
                          (Thread/sleep 200)
                          (cancellation/cancel! token :client-cancel-turn)))]

            (try (let [cancelled (watchdog-block ctx
                                                 (assoc env
                                                   :cancel-token token
                                                   :python-context-retired-atom retired)
                                                 "await watchdog_probe.poll(30)\n"
                                                 :tool-event-fn
                                                 #(when (= :start (:phase %))
                                                    (deliver started true)))]
                   (expect (some? (:error cancelled)))
                   (expect (not (:timeout? cancelled)))
                   (expect (loop [remaining 100]
                             (cond (and @retired (worker/retired? ctx)) true
                                   (zero? remaining) false
                                   :else (do (Thread/sleep 50) (recur (dec remaining)))))))
                 (finally (cancellation/cancel! token :test-cleanup) (deref stopper 5000 nil)))))))
  (it "restores the watchdog and context after cancelling a cooperative extension"
      (with-watchdog-extension
        (fn [ctx env]
          (let [token
                (cancellation/cancellation-token)

                started
                (promise)

                stopper
                (future (when (= true (deref started 5000 :timeout))
                          (Thread/sleep 200)
                          (cancellation/cancel! token :client-cancel-turn)))]

            (try (let [cancelled
                       (watchdog-block ctx
                                       (assoc env :cancel-token token)
                                       "await watchdog_probe.poll_cooperative(30)\n"
                                       :tool-event-fn
                                       #(when (= :start (:phase %)) (deliver started true)))

                       runaway
                       (watchdog-block ctx env "while True:\n    pass\n")

                       resumed
                       (watchdog-block ctx env "print('context reusable')\n")]

                   (expect (some? (:error cancelled)))
                   (expect (not (:timeout? cancelled)))
                   (expect (true? (:timeout? runaway)) (pr-str runaway))
                   (expect (nil? (:error resumed)) (pr-str resumed))
                   (expect (= "context reusable" (str/trim (or (:stdout resumed) "")))))
                 (finally (cancellation/cancel! token :test-cleanup) (deref stopper 5000 nil)))))))
  (it "keeps concurrent async calls parked until both complete"
      (with-watchdog-extension
        (fn [ctx env]
          (let [result (watchdog-block ctx
                                       env
                                       (str
                                         "answers = await gather(watchdog_probe.poll_async(0.2), "
                                         "watchdog_probe.poll_async(1.6))\n"
                                         "import time\ntime.sleep(0.6)\n"
                                         "print(sorted(answer.count for answer in answers))\n"))]
            (expect (nil? (:error result)) (pr-str result))
            (expect (= "[2, 3]" (str/trim (or (:stdout result) ""))))))))
  (it "restores execution after an asynchronous extension raises"
      (with-watchdog-extension
        (fn [ctx env]
          (let [result (watchdog-block
                         ctx
                         env
                         (str "import time\ntime.sleep(0.6)\n"
                              "try:\n    await watchdog_probe.fail_async(1.6)\n"
                              "except Exception as exc:\n"
                              "    assert 'asynchronous extension observation failed' in str(exc)\n"
                              "    print('handled')\n" "time.sleep(0.6)\nprint('resumed')\n"))]
            (expect (nil? (:error result)) (pr-str result))
            (expect (= "handled\nresumed" (str/trim (or (:stdout result) "")))))))))
