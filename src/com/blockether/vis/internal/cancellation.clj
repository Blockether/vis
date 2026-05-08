(ns com.blockether.vis.internal.cancellation
  "Cancellation token - leaf module.

   The cancellation token is a tiny two-atom record that lets a UI
   thread (TUI, Telegram bot, REPL caller) cooperatively abort an
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
  (try
    (.getMethod Thread "startVirtualThread" (into-array Class [Runnable]))
    true
    (catch Throwable _ false)))

(defn worker-runtime
  "Runtime probe for worker execution. `:worker-helper` is stable metadata for
   diagnostics; `:virtual-threads?` reports whether new worker tasks will use
   Java virtual threads."
  []
  {:worker-helper :vis/worker-future
   :virtual-threads? (virtual-threads-available?)})

(defn worker-future
  "Run `f` on a cancellable worker Future. Uses a virtual thread when the JVM
   supports it, otherwise falls back to a named daemon platform thread.

   The returned value implements java.util.concurrent.Future plus Clojure
   deref/realized? protocols so legacy `future` call sites can migrate without
   losing timeout/cancellation behavior."
  ([f] (worker-future "vis-worker" f))
  ([name f]
   (let [task (java.util.concurrent.FutureTask.
                ^java.util.concurrent.Callable
                (reify java.util.concurrent.Callable
                  (call [_] (f))))
         _runner (if (virtual-threads-available?)
                   (let [m (.getMethod Thread "startVirtualThread" (into-array Class [Runnable]))]
                     (.invoke m nil (object-array [task])))
                   (doto (Thread. ^Runnable task (str name))
                     (.setDaemon true)
                     (.start)))]
     (reify
       java.util.concurrent.Future
       (cancel [_ may-interrupt-if-running]
         (.cancel task may-interrupt-if-running))
       (isCancelled [_] (.isCancelled task))
       (isDone [_] (.isDone task))
       (get [_] (.get task))
       (get [_ timeout unit] (.get task timeout unit))

       clojure.lang.IDeref
       (deref [_] (.get task))

       clojure.lang.IBlockingDeref
       (deref [_ timeout-ms timeout-val]
         (try
           (.get task timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
           (catch java.util.concurrent.TimeoutException _ timeout-val)))

       clojure.lang.IPending
       (isRealized [_] (.isDone task))))))

(defn cancellation-token
  "Construct a fresh cancellation token. The token wraps a cooperative
   flag (`:cancel-atom` of `turn!`) and a worker `Future` reference so
   a hard `(.cancel f true)` can interrupt a blocking provider call."
  []
  {::flag   (atom false)
   ::future (atom nil)})

(defn cancellation-atom
  "Cooperative flag atom - pass under `:cancel-atom` to `turn!`."
  [token]
  (::flag token))

(defn cancellation-set-future!
  "Register the worker `Future` so a hard cancel can interrupt it.
   Returns the future for convenient threading."
  [token fut]
  (reset! (::future token) fut)
  fut)

(defn cancel!
  "Abort the in-flight turn. Sets the cooperative flag AND interrupts
   the registered future (if any). Safe to call multiple times. Safe
   to call when no future has been registered yet - only the flag is
   flipped in that case, and the cooperative path will still pick it
   up at the next iteration boundary."
  [token]
  (when token
    (try (reset! (::flag token) true) (catch Throwable _ nil))
    (when-let [^java.util.concurrent.Future f (some-> (::future token) deref)]
      (try (.cancel f true) (catch Throwable _ nil))))
  nil)

(defn cancelled?
  "True once `cancel!` has been called on this token."
  [token]
  (boolean (some-> token ::flag deref)))

(defn ^:private cancellation-cause?
  "Walk an exception's cause chain looking for an `InterruptedException`.
   Some HTTP libs wrap thread interruption in a runtime exception, so
   the obvious `(instance? InterruptedException e)` check misses them."
  [^Throwable e]
  (loop [t e]
    (cond
      (nil? t)                            false
      (instance? InterruptedException t)         true
      (instance? java.util.concurrent.CancellationException t) true
      :else                               (recur (.getCause t)))))

(defn cancellation?
  "True if the given throwable was caused by a `cancel!` call. Channels
   should treat this as a normal (cancelled) outcome rather than an
   error and avoid showing stack traces."
  [^Throwable e]
  (cancellation-cause? e))
