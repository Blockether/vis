(ns com.blockether.vis.test-python-context
  "ONE GraalPy sandbox for the whole test JVM.

   Building a `Context` costs ~8s the first time (Truffle engine bootstrap) and
   ~150-300ms every time after that, so a suite that opened one per test — or
   even one per namespace — spent whole seconds doing nothing but re-entering
   the same interpreter. Tests that only need *a* sandbox share this one.

   `shared` is that context. `shared-with!` is the same context with extra host
   callables installed on it (exactly what `create-python-context`'s
   `custom-bindings` does, only later), so a test that needs a stub tool does
   not need its own interpreter. Give stubs distinct names: the sandbox is
   shared, and so are its globals.

   A test that needs real ISOLATION — its own filesystem roots, its own network
   policy, its own stdin, a genuinely empty global namespace, or one that WIPES
   or monkeypatches the sandbox (`globals().clear()`, `requests.request = fake`)
   — must NOT use this: that damage outlives the test and lands on whatever runs
   next. Call `env-python/create-python-context` directly and own the lifecycle.

   Nothing here closes the context: it lives as long as the JVM does, and a
   `Context.close` on a sandbox with live guest threads is itself slow."
  (:require [com.blockether.vis.internal.env-python :as env-python])
  (:import [org.graalvm.polyglot Context]))

(def ^:private shared* (delay (:python-context (env-python/create-python-context {}))))

(defn shared "The process-wide test sandbox `Context`." ^Context [] @shared*)

(defn shared-with!
  "The process-wide test sandbox with `bindings` (symbol -> host value)
   installed on it. Returns the same `Context` every time."
  ^Context [bindings]
  (let [ctx (shared)]
    (doseq [[sym v] bindings]
      (env-python/set-python-binding! ctx sym v))
    ctx))
