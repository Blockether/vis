(ns com.blockether.vis.internal.provider.oauth
  "Single-flight OAuth token refresh, shared by every provider.

   A token endpoint that ROTATES the refresh_token (Anthropic, OpenAI Codex, MCP
   servers) answers HTTP 400 `invalid_grant` when two exchanges spend one refresh
   token. A 401 storm (each iteration's retry plus the limits poll) caused exactly
   that race and killed whole turns. So refresh is serialized PER CREDENTIAL STORE,
   never globally: each `make-file-refresher` owns its lock, and an optional OS file
   lock serializes processes. Under the lock, a caller reuses a result that another
   caller just saved, so a burst of N 401s runs ONE exchange.

   USE: a file-backed rotating store calls `make-file-refresher`. It returns a 0/1-arg
   fn for `:provider/get-token-fn` / `:provider/refresh-token-fn`. A store with its own
   cache (GitHub Copilot) calls `single-flight!` and `log-refresh!` itself."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel]))

(def default-reuse-window-ms
  "A refresh persisted/produced within this window is REUSED instead of
   re-run. Wide enough to absorb a 401 burst (every iteration's retry +
   the limits poll), short enough that a genuinely-stale token still
   refreshes promptly."
  15000)

(defn auth-json-str
  "Encode provider auth state with the canonical total JSON codec."
  ^String [auth-state]
  (wire/json-str auth-state))

(defn new-lock
  "A fresh monitor object. One per credential store, owned by the refresher
   built around it — never share a lock across providers."
  []
  (Object.))

(defn fresh-within?
  "True when `saved-at-ms` (epoch millis, or nil) is within `window-ms` of
   now — i.e. another caller persisted these creds this recently, so they
   are safe to reuse rather than running another (rotating) exchange."
  ([saved-at-ms] (fresh-within? saved-at-ms default-reuse-window-ms))
  ([saved-at-ms window-ms]
   (and (number? saved-at-ms) (< (- (util/now-ms) (long saved-at-ms)) (long window-ms)))))

(defn single-flight!
  "Serialize `refresh!` on `lock`. Once the lock is held, call `reuse`
   first: if it returns a non-nil result, another caller already refreshed
   while this thread waited for the lock, so return that WITHOUT a second
   exchange; otherwise run `refresh!` and return its result. `reuse` and
   `refresh!` are 0-arg fns; `reuse` returns the reusable token map or nil."
  [lock reuse refresh!]
  (locking lock (or (reuse) (refresh!))))

(defn- error-chain
  "`error` and each of its causes, outermost first."
  [error]
  (take-while some? (iterate ex-cause error)))

(defn- invalid-grant?
  "True when `error` or one of its causes is an HTTP 400/401 `invalid_grant`
   answer of a token endpoint. The body is JSON text or an already-parsed map."
  [error]
  (boolean (some (fn [e]
                   (let [{:keys [status body]} (ex-data e)]
                     (and (contains? #{400 401} status)
                          (= "invalid_grant"
                             (cond (map? body) (or (get body "error") (get body :error))
                                   (string? body) (try (:error
                                                         (json/read-json body :key-fn keyword))
                                                       (catch Exception parse-error
                                                         (tel/log! {:level :debug
                                                                    :id ::token-error-body-not-json
                                                                    :data {:status status}
                                                                    :error parse-error})
                                                         nil)))))))
                 (error-chain error))))

(defn- token-fingerprint
  "The first 8 hex digits of the SHA-256 of `token`, or nil for a blank one. A log
   can follow one rotation chain across processes with it, and it reveals nothing
   of the token."
  [token]
  (when-not (str/blank? token) (subs (util/sha256-hex token) 0 8)))

(defn log-refresh!
  "Log ONE refresh exchange of credential store `store` as `::token-refresh`.
   `outcome` is :refreshed, :superseded (refused, but another writer replaced the
   credential), :rejected (a sign-in is needed), :failed, :persist-failed (the
   rotated token is lost) or :no-refresh-token. `locked` is false when the exchange
   ran without its file lock. The record holds no token, body or error message:
   only the HTTP status, the error type and class, and token fingerprints."
  [{:keys [store outcome trigger started-ms error refresh-token next-refresh-token locked]}]
  (let [chain
        (error-chain error)

        data
        (into {}
              (remove (comp nil? val))
              {:store (some-> store
                              name)
               :outcome outcome
               :trigger trigger
               :pid (paths/process-id)
               :role (paths/process-role)
               :locked locked
               :duration-ms (some->> started-ms
                                     (- (util/now-ms)))
               :status (some (comp :status ex-data) chain)
               :error-type (some (comp :type ex-data) chain)
               :error-class (some-> error
                                    class
                                    .getName)
               :refresh-token-sha (token-fingerprint refresh-token)
               :next-refresh-token-sha (token-fingerprint next-refresh-token)})]

    (tel/log! {:level (case outcome
                        (:refreshed :superseded)
                        :info

                        :persist-failed
                        :error

                        :warn)
               :id ::token-refresh
               :data data}
              (str "OAuth token refresh of " (:store data) ": " (name outcome)))))

(def default-file-lock-timeout-ms
  "How long to wait for the CROSS-PROCESS advisory lock before giving up and
   running unlocked. A peer's own exchange takes a second or two, so this is
   generous; the point is that a peer that STALLED (or died holding the lock on a
   filesystem that leaks it) must never freeze provider auth in this JVM
   forever — the caller reaches here holding the in-process monitor, so a wedged
   acquisition blocks EVERY refresh here, not just this one."
  20000)

(defn- try-lock!
  "ONE non-blocking attempt at the OS advisory lock on `ch`. Returns the
   `FileLock`, or nil when another PROCESS currently holds it. Seam: tests
   simulate a stalled peer holder by redefining this to always return nil."
  [^java.nio.channels.FileChannel ch]
  (.tryLock ch))

(defn- close-quietly!
  "Close `c`, swallowing any failure — used on paths that already carry an
   outcome (a throw, or the normal `finally`) that must not be masked."
  [^java.io.Closeable c]
  (when c (try (.close c) (catch Throwable _ nil))))

(defn- acquire-file-lock!
  "Poll `try-lock!` until granted or `timeout-ms` elapses. nil on timeout, on an
   overlapping lock already held by THIS JVM, or on any locking failure
   (unsupported filesystem, IO error) — every one of which means \"run unlocked\".

   An INTERRUPT is the one input that is not degraded: the caller is being
   cancelled, so the wait aborts, the interrupt flag is RESTORED (`Thread/sleep`
   clears it) and `InterruptedException` propagates for the caller to close over."
  [ch timeout-ms]
  (try (util/poll (+ (util/now-ms) (long timeout-ms))
                  50
                  #(let [fl (try (try-lock! ch) (catch Throwable _ ::failed))] (cond (= ::failed fl)
                                                                                     (reduced nil)
                                                                                     (some? fl)
                                                                                     (reduced fl))))
       (catch InterruptedException e (.interrupt (Thread/currentThread)) (throw e))))

(defn call-with-file-lock
  "Run `f` while holding an OS advisory lock on `lock-path`, serializing the
   critical section ACROSS processes (e.g. two vis JVMs — a source-runtime client and
   its gateway daemon — sharing one credential file). The caller must already
   hold the in-process monitor, since a single JVM cannot hold two overlapping
   locks on the same file. `f` gets true when it holds the lock, false when it
   runs unlocked, and nil for a blank `lock-path`.

   `f` runs EXACTLY ONCE, always, and its exception propagates untouched — a
   retry here would re-run a ROTATING token exchange with an already-spent
   refresh token. Any locking failure (unsupported filesystem, IO error,
   overlapping lock), or a peer still holding the lock after `timeout-ms`,
   degrades to running `f` UNLOCKED rather than blocking authentication.

   The single exception is an INTERRUPT while waiting for a peer: the caller is
   being cancelled, so `f` does NOT run and `InterruptedException` propagates
   with the flag restored. The lock file handle is closed on EVERY exit path,
   including that one — an fd left to the GC would also leave the advisory lock
   held for an unbounded time."
  ([lock-path f] (call-with-file-lock lock-path default-file-lock-timeout-ms f))
  ([lock-path timeout-ms f]
   (if (str/blank? lock-path)
     (f nil)
     (let [ch (try (io/make-parents (io/file lock-path))
                   (.getChannel (java.io.RandomAccessFile. (io/file lock-path) "rw"))
                   (catch Throwable _ nil))]
       (if (nil? ch)
         (f false)
         (let [fl (try (acquire-file-lock! ch timeout-ms)
                       (catch Throwable t (close-quietly! ch) (throw t)))]
           (try (f (some? fl))
                (finally (when fl
                           (try (.release ^java.nio.channels.FileLock fl) (catch Throwable _ nil)))
                         (close-quietly! ch)))))))))

(def ^:private reauthentication-message
  "Your sign-in has expired or was revoked. Select Connect to sign in again.")

(defn reauthentication-status
  "Return the saved rejection verdict, or nil when sign-in is not required."
  [credentials]
  (when (true? (:reauth-required credentials))
    {:is-authenticated false :auth-state :rejected :error reauthentication-message}))

(defn require-usable-credentials!
  "Reject a saved invalid grant before token reuse or another refresh exchange."
  [credentials]
  (when (reauthentication-status credentials)
    (throw (ex-info reauthentication-message {:type :provider/reauthentication-required})))
  credentials)

(defn reauthentication-error?
  "True when a refresh grant requires a new sign-in."
  [error]
  (= :provider/reauthentication-required (:type (ex-data error))))

(defn make-file-refresher
  "Build a 0/1-arg single-flight refresh fn for a FILE-backed credential
   store whose token endpoint ROTATES the refresh_token (Anthropic,
   OpenAI Codex, MCP servers). Owns its own lock; reuses usable saved creds
   instead of running a second (rotating) exchange — UNLESS the reusable token
   equals the one just rejected (pass that token when calling the returned fn on
   401 recovery), so a server-rotated but locally-fresh token is re-exchanged
   rather than handed back dead. Every exchange is logged under `:store` through
   [[log-refresh!]]; a reuse is not.

   opts — all required unless noted:
     :store         keyword or string              the store name in the log
     :load          (fn [] -> creds-map|nil)       read persisted creds
     :saved-at      (fn [creds] -> epoch-ms|nil)   persist timestamp; required
                                                   only without `:usable?`
     :refresh-token (fn [creds] -> string|nil)     the rotating refresh token
     :exchange!     (fn [refresh-token] -> creds)  the rotating HTTP call
     :persist!      (fn [creds] -> creds)          save + STAMP saved-at
     :->token       (fn [creds] -> token-map)      runtime provider-token shape
     :no-token!     (fn [] -> throws)              when no refresh token on file
     :usable?       (optional) (fn [creds] -> bool) reuse test; default: saved
                    within `:reuse-window-ms`
     :on-rejected!  (optional) (fn [creds]) after a refused grant, before the
                    sign-in error; default: persist `:reauth-required true`
     :reuse-window-ms (optional) override the default window
     :lock-path       (optional) file path for an OS advisory lock that
                      serializes the rotating exchange ACROSS processes
                      (two vis JVMs sharing one credential file). Without it,
                      only in-process callers serialize and two processes can
                      still race the rotation into HTTP 400 `invalid_grant`.

   Returns the provider-token map produced by `:->token`."
  [{:keys [store load saved-at usable? refresh-token exchange! persist! on-rejected! ->token
           no-token! reuse-window-ms lock-path]}]
  (let [window
        (or reuse-window-ms default-reuse-window-ms)

        usable?
        (or usable? #(fresh-within? (saved-at %) window))

        on-rejected!
        (or on-rejected! #(persist! (assoc % :reauth-required true)))

        lock
        (new-lock)

        reuse
        (fn [rejected]
          (let [creds (require-usable-credentials! (load))]
            (when (and creds (usable? creds))
              (let [tok (->token creds)]
                ;; Never hand back the exact token the server just
                ;; rejected (401): a locally-fresh but server-rotated
                ;; token would otherwise be REUSED and 401 again. When it
                ;; differs, reuse still collapses the 401 storm.
                (when-not (= rejected (:token tok)) tok)))))

        refresh!
        (fn [rejected locked?]
          (let [creds
                (require-usable-credentials! (load))

                rt
                (refresh-token creds)

                entry
                {:store store
                 :trigger (if rejected :rejected-token :expired)
                 :started-ms (util/now-ms)
                 :refresh-token rt
                 :locked locked?}

                log!
                (fn [outcome extra]
                  (log-refresh! (merge entry {:outcome outcome} extra)))

                ;; [:exchanged fresh-creds] or [:superseded current-creds], or throws.
                exchange-step
                (fn []
                  (try [:exchanged (exchange! rt)]
                       (catch Throwable e
                         (when-not (invalid-grant? e) (log! :failed {:error e}) (throw e))
                         (let [current (load)]
                           ;; A sign-in can replace the file while the exchange is pending.
                           (when (= rt (refresh-token current))
                             (log! :rejected {:error e})
                             (on-rejected! current)
                             (require-usable-credentials! (assoc current :reauth-required true)))
                           (log! :superseded {:error e})
                           [:superseded current]))))]

            (when (str/blank? rt) (log! :no-refresh-token nil) (no-token!))
            (let [[step fresh] (exchange-step)]
              (->token (case step
                         :superseded
                         (do (when (str/blank? (refresh-token fresh)) (no-token!))
                             (require-usable-credentials! fresh))

                         :exchanged
                         (let [saved (try (persist! fresh)
                                          (catch Throwable e
                                            (log! :persist-failed
                                                  {:error e
                                                   :next-refresh-token (refresh-token fresh)})
                                            (throw e)))]
                           (log! :refreshed {:next-refresh-token (refresh-token saved)})
                           saved))))))

        ;; In-process monitor serializes threads in THIS JVM; the file lock
        ;; then serializes across JVMs. Order matters: hold the monitor first
        ;; so only one thread per JVM ever reaches the (non-reentrant) file
        ;; lock. After the winner persists and releases, the next process's
        ;; `reuse` reads the freshly-stamped file and reuses instead of
        ;; running a second rotating exchange.
        run
        (fn [rejected]
          (locking lock
            (call-with-file-lock lock-path #(or (reuse rejected) (refresh! rejected %)))))]

    (fn ([] (run nil)) ([rejected] (run rejected)))))
