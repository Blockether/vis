(ns com.blockether.vis.test-python-context
  "Long-lived GraalPy sandboxes for the test JVM, keyed instead of per-test.

   Building a `Context` costs ~8s the first time (Truffle engine bootstrap) and
   ~150-300ms every time after that, so a suite that opened one per test — or
   even one per namespace — spent whole seconds doing nothing but re-entering
   the same interpreter.

   `shared` is THE sandbox for tests that merely USE Python: the shim suites,
   which import a module and assert on what it renders. `shared-with!` is that
   same context with extra host callables installed on it (exactly what
   `create-python-context`'s `custom-bindings` does, only later), so a test that
   needs a stub tool does not need its own interpreter. Give stubs distinct
   names: the sandbox is shared, and so are its globals.

   A namespace that ABUSES the sandbox on purpose — the engine contract suites
   run `from math import *`, shadow tool names and delete engine internals —
   takes its own with `(context ::ctx)`, keyed by a namespaced keyword, so its
   debris stays inside it. `from json import *` in one namespace is exactly what
   redefined `_format` under the tabulate shim in another.

   A test that needs real ISOLATION — its own filesystem roots, its own network
   policy, its own stdin, a genuinely empty global namespace, or one that WIPES
   or monkeypatches the sandbox (`globals().clear()`, `requests.request = fake`)
   — must use neither: that damage outlives even the test that follows it in the
   same namespace. Call `env-python/create-python-context` directly and own the
   lifecycle.

   Nothing here closes a context: they live as long as the JVM does, and a
   `Context.close` on a sandbox with live guest threads is itself slow."
  (:require [com.blockether.vis.internal.env-python :as env-python])
  (:import [org.graalvm.polyglot Context]))

(defonce ^:private contexts (atom {}))

(defn context
  "The long-lived sandbox `Context` for `k`, built on first use. Key it with a
   namespaced keyword (`::ctx`) so two suites cannot collide by accident."
  ^Context [k]
  (or (get @contexts k)
      (get (swap! contexts (fn [m]
                             (cond-> m
                               (not (contains? m k))
                               (assoc k (:python-context (env-python/create-python-context {}))))))
           k)))

(defn context-with!
  "`context` for `k` with `bindings` (symbol -> host value) installed on it."
  ^Context [k bindings]
  (let [ctx (context k)]
    (doseq [[sym v] bindings]
      (env-python/set-python-binding! ctx sym v))
    ctx))

(defn shared
  "The sandbox shared by every suite that only USES Python."
  ^Context []
  (context ::shared))

(defn shared-with!
  "`shared` with `bindings` (symbol -> host value) installed on it."
  ^Context [bindings]
  (context-with! ::shared bindings))
