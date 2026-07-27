(ns com.blockether.vis.internal.cancellation
  "Cancellation token - leaf module.

   The cancellation token is a tiny two-atom record that lets a UI
   thread (TUI, REPL caller) cooperatively abort an
   in-flight `turn!` AND interrupt the worker future hosting the
   blocking provider call. The cooperative side is checked at every
   iteration boundary; the future side hard-cancels any HTTP call
   that has already started.

   Public API:

     `(cancellation-token)`       - fresh token
     `(cancellation-atom token)`  - cooperative flag atom (pass to `turn!`)
     `(cancellation-set-future! token fut)` - register the worker future
     `(cancel! token)`            - set flag + interrupt registered future
     `(cancelled? token)`         - true once `cancel!` has been called
     `(cancellation? throwable)`  - true if exception was caused by `cancel!`

   This namespace has zero side effects at load time and depends only
   on Java interop - channels and the runtime can require it
   directly without pulling in the rest of the SDK."
  (:refer-clojure))

(defn virtual-threads-available?
  "True when this JVM exposes Java virtual-thread APIs. Reflection keeps
   source compatible with older runtimes."
  []
  (try (.getMethod Thread "startVirtualThread" (into-array Class [Runnable]))
       true
       (catch Throwable _ false)))

(defn worker-runtime
  "Runtime probe for worker execution. `:worker-helper` is stable metadata for
   diagnostics; `:virtual-threads?` reports whether new worker tasks will use
   Java virtual threads."
  []
  {:worker-helper :vis/worker-future :virtual-threads? (virtual-threads-available?)})

(defn worker-future
  "Run `f` on a cancellable worker Future. Uses a virtual thread when the JVM
   supports it, otherwise falls back to a named daemon platform thread.

   The returned value implements java.util.concurrent.Future plus Clojure
   deref/realized? protocols so legacy `future` call sites can migrate without
   losing timeout/cancellation behavior."
  ([f] (worker-future "vis-worker" f))
  ([name f]
   (let [task
         (java.util.concurrent.FutureTask. ^java.util.concurrent.Callable
                                           (reify
                                             java.util.concurrent.Callable
                                               (call [_] (f))))

         _runner
         (if (virtual-threads-available?)
           (let [m (.getMethod Thread "startVirtualThread" (into-array Class [Runnable]))]
             (.invoke m nil (object-array [task])))
           (doto (Thread. ^Runnable task (str name)) (.setDaemon true) (.start)))]

     (reify
       java.util.concurrent.Future
         (cancel [_ may-interrupt-if-running] (.cancel task may-interrupt-if-running))
         (isCancelled [_] (.isCancelled task))
         (isDone [_] (.isDone task))
         (get [_] (.get task))
         (get [_ timeout unit] (.get task timeout unit))
       clojure.lang.IDeref
         (deref [_] (.get task))
       clojure.lang.IBlockingDeref
         (deref [_ timeout-ms timeout-val]
           (try (.get task timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
                (catch java.util.concurrent.TimeoutException _ timeout-val)))
       clojure.lang.IPending
         (isRealized [_] (.isDone task))))))

(defn cancellation-token
  "Construct a fresh cancellation token.

   The token bundles two things every cancellable boundary needs:
     - `::flag`      — cooperative boolean atom, polled at iteration
                        boundaries by callers that can return
                        gracefully.
     - `::callbacks` — vec of `[id thunk]` pairs run by `cancel!` so
                        any number of in-flight workers (provider
                        HTTP call, Python eval future, voice recorder)
                        can register their own hard-cancel hook.

   `cancellation-set-future!` (legacy single-future API) is kept for
   call sites that have not migrated yet; it now routes through the
   callback registry too so behaviour stays identical."
  []
  {::flag (atom false) ::callbacks (atom [])})

(defn cancellation-atom
  "Cooperative flag atom — read with `@` at iteration boundaries when
   the consumer can return without external help."
  [token]
  (::flag token))

(defn cancelled?
  "True once `cancel!` has been called on this token."
  [token]
  (boolean (some-> token
                   ::flag
                   deref)))

(defn on-cancel!
  "Register a no-arg `thunk` to fire the moment `cancel!` is invoked
   on `token`. Returns a `dispose!` thunk the caller MUST invoke when
   the cancellable work finishes normally — otherwise callbacks
   accumulate for the token's lifetime.

   If `cancel!` has already fired on this token, `thunk` runs
   synchronously here and `dispose!` is a no-op. This matches the
   contract every consumer wants: registering AFTER cancellation
   must still cancel, not silently swallow the request.

   Replaces the atom-watch pattern earlier eval boundaries hand-rolled:
   one shared callback list, no per-consumer add-watch / remove-watch
   plumbing, no risk of leaving a watch on the flag after the worker
   finishes."
  [token thunk]
  (when (and token (fn? thunk))
    (if (cancelled? token)
      (do (try (thunk) (catch Throwable _ nil)) (constantly nil))
      (let [cb-id
            (Object.)

            ; identity key; survives equal-by-value collisions
            callbacks
            (::callbacks token)]

        (swap! callbacks conj [cb-id thunk])
        (fn dispose! []
          (swap! callbacks (fn [cbs]
                             (vec (remove #(identical? cb-id (first %)) cbs)))))))))

(defn cancellation-set-future!
  "Register a worker `Future` so `cancel!` interrupts it. Thin
   convenience over `on-cancel!`: wraps `.cancel(true)` in a thunk
   and discards the returned `dispose!` (the future's own completion
   makes the second cancel a no-op).

   Returns the future for convenient threading."
  [token ^java.util.concurrent.Future fut]
  (when (and token fut)
    (on-cancel! token
                (fn []
                  (try (.cancel fut true) (catch Throwable _ nil)))))
  fut)

(defn cancel!
  "Abort the in-flight turn. Flips the cooperative flag AND runs every
   registered `on-cancel!` callback. Each callback is wrapped in its
   own `try`/`catch` so one bad consumer cannot starve the rest.
   Safe to call multiple times — on the second call the flag is
   already set, and the callback list has typically drained because
   workers disposed themselves."
  [token]
  (when token
    (try (reset! (::flag token) true) (catch Throwable _ nil))
    (doseq [[_ thunk] @(::callbacks token)]
      (try (thunk) (catch Throwable _ nil))))
  nil)

(defn ^:private cancellation-cause?
  "Walk an exception's cause chain looking for an `InterruptedException`.
   Some HTTP libs wrap thread interruption in a runtime exception, so
   the obvious `(instance? InterruptedException e)` check misses them."
  [^Throwable e]
  (loop [t e]
    (cond (nil? t) false
          (instance? InterruptedException t) true
          (instance? java.util.concurrent.CancellationException t) true
          :else (recur (.getCause t)))))

(defn cancellation?
  "True if the given throwable was caused by a `cancel!` call. Channels
   should treat this as a normal (cancelled) outcome rather than an
   error and avoid showing stack traces."
  [^Throwable e]
  (cancellation-cause? e))
