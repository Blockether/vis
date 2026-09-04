(ns com.blockether.vis.test-python-context
  "Long-lived sandbox SESSIONS for the test JVM, keyed instead of per-test.

   A sandbox is a namespace inside the ONE interpreter the process starts, so what
   it costs is the seeding — the runtime, the tools, their contracts, the policy —
   not an engine. That is milliseconds rather than seconds, but a suite that built
   one per test still paid it hundreds of times, and the sharing below is what
   keeps the suite's Python cheap.

   `shared` is THE sandbox for tests that merely USE Python: the door suites,
   which import a module and assert on what it renders. `shared-with!` is that
   same session with extra host callables installed on it (exactly what
   `create-python-context`'s `custom-bindings` does, only later), so a test that
   needs a stub tool does not need its own. Give stubs distinct names: the session
   is shared, and so are its globals.

   A namespace that ABUSES the sandbox on purpose — the engine contract suites
   run `from math import *`, shadow tool names and delete engine internals —
   takes its own with `(context ::ctx)`, keyed by a namespaced keyword, so its
   debris stays inside it. `from json import *` in one namespace is exactly what
   redefined `_format` under the tabulate door in another.

   A test that needs real ISOLATION — its own filesystem roots, its own network
   policy, its own stdin, a genuinely empty global namespace, or one that WIPES
   or monkeypatches the sandbox (`globals().clear()`, `requests.request = fake`)
   — must use neither: that damage outlives even the test that follows it in the
   same namespace. Use [[with-own]], which disposes the session on the way out.

   Two things a session cannot own, because the runtime holds them for the whole
   PROCESS: the filesystem roots and the network capability. `with-own` with a
   `roots-fn` re-confines the interpreter and does not put the previous roots
   back, so a namespace that mixes rooted and unrooted sandboxes states the roots
   it needs in each one.

   ONE NAMESPACE, ONE REGISTRATION. `shared-with!` installs a stub that CLOSES
   OVER test state (the `(atom [])` a test asserts on) into a process-global
   sandbox. Register the same namespace twice in one JVM — `-M:test --dir test
   --namespace x` re-adds a directory the `:test` alias already passes, which
   runs everything under it twice — and the second registration overwrites the
   binding with a stub closing over the SECOND atom. The first pass then calls
   a live tool, gets a real result back, and asserts on an atom nobody wrote to:
   the test fails while the code it covers is provably fine. Run a single
   namespace as `-M:test --namespace <ns>`, with no `--dir`."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.env-python :as env-python]))

(defn new-context
  "A fresh sandbox result map, with `create-python-context`'s arguments in its own
   order: bindings, then roots, then network, then stdin — so a call site states
   only what it cares about."
  ([] (new-context {} nil nil))
  ([bindings] (new-context bindings nil nil))
  ([bindings roots-fn] (new-context bindings roots-fn nil))
  ([bindings roots-fn network-opts] (new-context bindings roots-fn network-opts nil))
  ([bindings roots-fn network-opts stdin]
   (env-python/create-python-context bindings roots-fn network-opts stdin)))

(defonce ^:private contexts (atom {}))

(defn context
  "The long-lived sandbox session for `k`, built on first use. Key it with a
   namespaced keyword (`::ctx`) so two suites cannot collide by accident."
  ^String [k]
  (or (get @contexts k)
      (get (swap! contexts (fn [m]
                             (cond-> m
                               (not (contains? m k))
                               (assoc k (:python-context (new-context))))))
           k)))

(defn context-with!
  "`context` for `k` with `bindings` (symbol -> host value) installed on it."
  ^String [k bindings]
  (let [ctx (context k)]
    (doseq [[sym v] bindings]
      (env-python/set-python-binding! ctx sym v))
    ctx))

(defn ev
  "The value of guest `code` in `session`, as Clojure data.

   Statements run and a trailing expression's value comes back — the same shape
   a block has — and it crosses as JSON, so what a test asserts on is strings,
   numbers, vectors and maps keyed by KEYWORD: the guest's snake_case dict keys
   arrive verbatim as `:snake_case`, which is what the suites were written
   against. A guest exception is NOT swallowed:
   it arrives as the exception the runtime raises, because a test that silently
   read `nil` for a crash would pass for the wrong reason."
  [^String session ^String code]
  (let [text (runtime/run session code)]
    (when-not (str/blank? text) (json/read-json text :key-fn keyword))))

(defmacro with-own
  "Bind `sym` to a sandbox built for THIS test alone, and dispose it when the body
   returns.

   For a test that cannot share: one that wipes the sandbox
   (`globals().clear()`, `del __vis_run_async__`), asserts on a COLD session, or
   needs its own roots / network policy / stdin. Those reasons are real; this
   does not take them away. It bounds how LONG the sandbox lives.

   That is the number that matters. A session built in a `let` at suite level
   exists from namespace load until the JVM exits, and every one of them holds
   its own globals plus every host closure its tools captured.

   `args` are [[new-context]]'s: bindings, roots-fn, network-opts."
  [[sym & args] & body]
  `(let [r#
         (new-context ~@args)

         ~sym
         (:python-context r#)]

     (try ~@body (finally (env-python/dispose-python-context! (:python-context r#))))))

(defmacro with-own-env
  "[[with-own]] for a test that needs the whole `create-python-context` RESULT —
   `:initial-ns-keys`, `:sandbox-ns` — and not just the session. `binding` is
   destructured against that map; the sandbox is disposed the same way."
  [[binding & args] & body]
  `(let [r#
         (new-context ~@args)

         ~binding
         r#]

     (try ~@body (finally (env-python/dispose-python-context! (:python-context r#))))))

(defn shared
  "The sandbox shared by every suite that only USES Python."
  ^String []
  (context ::shared))

(defn shared-with!
  "`shared` with `bindings` (symbol -> host value) installed on it."
  ^String [bindings]
  (context-with! ::shared bindings))
