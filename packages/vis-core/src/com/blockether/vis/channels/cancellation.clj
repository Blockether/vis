(ns com.blockether.vis.channels.cancellation
  "Shared cancellation primitive used by every channel that runs an
   async query (TUI today; web/telegram/CLI tomorrow).

   Two layers, in order of speed:

   1. Cooperative flag — an `(atom false)` honored by the RLM iteration
      loop at iteration boundaries (`:cancel-atom` opt of `query!`).
      Fast and clean once the loop reaches its next check.

   2. Hard interrupt — the worker `Future` running the query. Calling
      `(.cancel f true)` sends `Thread.interrupt()` so the blocking
      HTTP wait inside `llm/ask!` returns immediately instead of
      sitting on the socket until the provider answers.

   Channels glue these together with a single token map:

       (let [tok (cancellation/make)]
         (cancellation/set-future! tok                          ;
           (future                                              ;
             (chat/query! conv text                             ;
               {:cancel-atom (cancellation/cancel-atom tok)}))) ;
         ;; user pressed Esc / Stop / /cancel:
         (cancellation/cancel! tok))

   Always close over a single token per turn — never share tokens
   across turns; create a fresh one per `query!`.")

(defn make
  "Construct a fresh cancellation token."
  []
  {::flag   (atom false)
   ::future (atom nil)})

(defn cancel-atom
  "The cooperative flag atom — pass under `:cancel-atom` to `query!`."
  [token]
  (::flag token))

(defn set-future!
  "Register the worker `Future` so a hard cancel can interrupt it.
   Returns the future for convenient threading. Idempotent: replaces
   any prior registration on the same token."
  [token fut]
  (reset! (::future token) fut)
  fut)

(defn cancel!
  "Abort the in-flight query. Sets the cooperative flag AND interrupts
   the registered future (if any). Safe to call multiple times. Safe
   to call when no future has been registered yet — only the flag is
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

(defn ^:private interrupt-in-cause-chain?
  "Walk an exception's cause chain looking for an `InterruptedException`.
   Some HTTP libs wrap thread interruption in a runtime exception, so
   the obvious `(instance? InterruptedException e)` check misses them."
  [^Throwable e]
  (loop [t e]
    (cond
      (nil? t)                            false
      (instance? InterruptedException t)  true
      :else                               (recur (.getCause t)))))

(defn cancellation?
  "True if the given throwable was caused by a `cancel!` call. Channels
   should treat this as a normal (cancelled) outcome rather than an
   error and avoid showing stack traces."
  [^Throwable e]
  (or (instance? java.util.concurrent.CancellationException e)
    (interrupt-in-cause-chain? e)))
