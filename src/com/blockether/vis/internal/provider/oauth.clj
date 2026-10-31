(ns com.blockether.vis.internal.provider.oauth
  "Unified OAuth token-refresh facade, shared by every provider.

   WHY THIS EXISTS — Providers whose token endpoint ROTATES the
   refresh_token on every exchange (Anthropic, OpenAI Codex) must never run
   two refresh exchanges at once: the second reuses an already-rotated
   refresh token and the server answers HTTP 400 `invalid_grant`. Under a
   401 \"storm\" — the turn loop's per-iteration retry PLUS a usage/limits
   poll, all sharing one credential — that race killed whole turns (~10 refreshes/min: one lost the
   rotation race and the turn died). Providers that mint a short-lived token from a STABLE
   credential (GitHub Copilot) don't 400, but still benefit: concurrent
   401s otherwise stampede the exchange endpoint with redundant calls.

   THE MODEL — refresh is serialized PER CREDENTIAL STORE, never globally.
   Each `make-file-refresher` / `refresher` call mints its OWN lock, so a
   refresh to Anthropic and a refresh to Codex run fully in parallel; only
   two refreshes to the SAME store (e.g. two sessions both hitting
   Anthropic) serialize — which is the whole point. Once the lock is held,
   a caller REUSES a result another thread just produced (creds persisted
   within `default-reuse-window-ms`, or an already-valid cache) so a burst
   of N concurrent 401s collapses into ONE exchange.

   USE — file-backed rotating stores (Anthropic, Codex): `make-file-refresher`.
   Cache-backed / bespoke stores (Copilot): `refresher` with custom
   reuse/refresh fns. Both return a 0-arg fn yielding the provider-token
   map, owning their own lock; drop them straight into
   `:provider/get-token-fn` / `:provider/refresh-token-fn`."
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
   `refresh!` are 0-arg fns; `reuse` returns the reusable token map or nil.

   Low-level core — prefer `make-file-refresher` / `refresher`."
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
                                                       (catch Exception _ nil)))))))
                 (error-chain error))))

(defn- token-fingerprint
  "The first 8 hex digits of the SHA-256 of `token`, or nil for a blank one. A log
   can follow one rotation chain across processes with it, and it reveals nothing
   of the token."
  [token]
  (when-not (str/blank? token) (subs (util/sha256-hex token) 0 8)))

(defn log-refresh!
  "Log ONE token refresh exchange of credential store `store`, and return nil.

   `outcome` is one of:
     :refreshed        the exchange succeeded and the new credential was saved
     :rejected         the endpoint refused the refresh grant; a sign-in is needed
     :superseded       refused, but another writer replaced the credential meanwhile
     :failed           the exchange failed for another reason
     :persist-failed   the exchange succeeded but saving failed: the rotated
                       refresh token is lost, so the next refresh is refused
     :no-refresh-token nothing to exchange

   `trigger` is :rejected-token when a request was just refused, else :expired.
   The record names this process (pid and role) and the duration since
   `started-ms`. It never holds a token, a response body or an error message:
   only the HTTP status, the error type and class of `error`, and the
   [[token-fingerprint]] of `refresh-token` (sent) and `next-refresh-token`
   (received; on :persist-failed it names the lost token)."
  [{:keys [store outcome trigger started-ms error refresh-token next-refresh-token]}]
  (let [status
        (some #(:status (ex-data %)) (error-chain error))

        error-type
        (some #(:type (ex-data %)) (error-chain error))

        sent
        (token-fingerprint refresh-token)

        saved
        (token-fingerprint next-refresh-token)]

    (tel/log! {:level (case outcome
                        (:refreshed :superseded)
                        :info

                        :persist-failed
                        :error

                        :warn)
               :id ::token-refresh
               :data (cond-> {:store (if (keyword? store) (name store) (str store))
                              :outcome outcome
                              :trigger trigger
                              :pid (paths/process-id)
                              :role (paths/process-role)
                              :duration-ms (when started-ms (- (util/now-ms) (long started-ms)))}
                       status
                       (assoc :status status)

                       error-type
                       (assoc :error-type error-type)

                       error
                       (assoc :error-class (.getName (class error)))

                       sent
                       (assoc :refresh-token-sha sent)

                       saved
                       (assoc :next-refresh-token-sha saved))}
              (str "OAuth token refresh of " (if (keyword? store) (name store) store)
                   ": " (name outcome)))
    nil))

(defn refresher
  "Build a single-flight refresh fn that OWNS a fresh lock. `refresh!` is as in
   `single-flight!`; `reuse` takes ONE arg — the REJECTED token (the access
   token the server just 401'd, or nil) — and returns the reusable token map or
   nil. For stores that aren't a plain rotating file — e.g. an in-memory cache
   (GitHub Copilot). Each `refresh!` run is logged under `store` through
   [[log-refresh!]]; a reuse is not. To log another outcome for a refusal,
   `refresh!` throws an `ex-info` whose data (or a cause's data) holds
   `::outcome`, for example `{::oauth/outcome :no-refresh-token}`. The outcome
   `:none` logs nothing: use it when no exchange ran because nothing is on file.

   The returned fn is 0/1-arity: call it WITH the rejected token on 401 recovery
   so `reuse` never hands back the very token that was just rejected; the 0-arity
   form (nil rejected) keeps plain freshness reuse."
  [store reuse refresh!]
  (let [lock
        (new-lock)

        logged
        (fn [rejected]
          (let [entry {:store store
                       :trigger (if rejected :rejected-token :expired)
                       :started-ms (util/now-ms)}]
            (try (let [result (refresh!)]
                   (log-refresh! (assoc entry :outcome :refreshed))
                   result)
                 (catch Throwable e
                   (let [outcome (or (some #(::outcome (ex-data %)) (error-chain e))
                                     (if (invalid-grant? e) :rejected :failed))]
                     (when-not (= :none outcome)
                       (log-refresh! (assoc entry
                                       :outcome outcome
                                       :error e))))
                   (throw e)))))]

    (fn ([] (single-flight! lock #(reuse nil) #(logged nil))) ([rejected] (single-flight! lock
                                                                            #(reuse rejected)
                                                                            #(logged rejected))))))

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

(defn- warn-unlocked!
  "Log that a refresh runs WITHOUT the cross-process lock on `lock-path`, so a
   lost rotation race can be traced to it. `reason` is :unopenable or :not-acquired."
  [lock-path reason]
  (tel/log!
    {:level :warn
     :id ::refresh-unlocked
     :data
     {:lock-path (str lock-path) :reason reason :pid (paths/process-id) :role (paths/process-role)}}
    "OAuth token refresh runs without the cross-process lock"))

(defn call-with-file-lock
  "Run `f` while holding an OS advisory lock on `lock-path`, serializing the
   critical section ACROSS processes (e.g. two vis JVMs — a source-runtime client and
   its gateway daemon — sharing one credential file). The caller must already
   hold the in-process monitor, since a single JVM cannot hold two overlapping
   locks on the same file.

   `f` runs EXACTLY ONCE, always, and its exception propagates untouched — a
   retry here would re-run a ROTATING token exchange with an already-spent
   refresh token. Blank/nil `lock-path`, any locking failure (unsupported
   filesystem, IO error, overlapping lock), or a peer still holding the lock
   after `timeout-ms` degrades to running `f` UNLOCKED rather than blocking
   authentication.

   The single exception is an INTERRUPT while waiting for a peer: the caller is
   being cancelled, so `f` does NOT run and `InterruptedException` propagates
   with the flag restored. The lock file handle is closed on EVERY exit path,
   including that one — an fd left to the GC would also leave the advisory lock
   held for an unbounded time."
  ([lock-path f] (call-with-file-lock lock-path default-file-lock-timeout-ms f))
  ([lock-path timeout-ms f]
   (if (str/blank? lock-path)
     (f)
     (let [ch (try (io/make-parents (io/file lock-path))
                   (.getChannel (java.io.RandomAccessFile. (io/file lock-path) "rw"))
                   (catch Throwable _ nil))]
       (if (nil? ch)
         (do (warn-unlocked! lock-path :unopenable) (f))
         (let [fl (try (acquire-file-lock! ch timeout-ms)
                       (catch Throwable t (close-quietly! ch) (throw t)))]
           (when-not fl (warn-unlocked! lock-path :not-acquired))
           (try (f)
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
   OpenAI Codex). Owns its own lock; reuses creds persisted within the
   reuse window instead of running a second (rotating) exchange — UNLESS the
   reusable token equals the one just rejected (pass that token when calling the
   returned fn on 401 recovery), so a server-rotated but locally-fresh token is
   re-exchanged rather than handed back dead. Every exchange is logged under
   `:store` through [[log-refresh!]]; a reuse is not.

   opts — all required unless noted:
     :store         keyword or string              the store name in the log
     :load          (fn [] -> creds-map|nil)       read persisted creds
     :saved-at      (fn [creds] -> epoch-ms|nil)   persist timestamp
                                                   (usually the keyword
                                                   `:saved-at-ms`/`:saved-at`)
     :refresh-token (fn [creds] -> string|nil)     the rotating refresh token
     :exchange!     (fn [refresh-token] -> creds)  the rotating HTTP call
     :persist!      (fn [creds] -> creds)          save + STAMP saved-at
     :->token       (fn [creds] -> token-map)      runtime provider-token shape
     :no-token!     (fn [] -> throws)              when no refresh token on file
     :reuse-window-ms (optional) override the default window
     :lock-path       (optional) file path for an OS advisory lock that
                      serializes the rotating exchange ACROSS processes
                      (two vis JVMs sharing one credential file). Without it,
                      only in-process callers serialize and two processes can
                      still race the rotation into HTTP 400 `invalid_grant`.

   Returns the provider-token map produced by `:->token`."
  [{:keys [store load saved-at refresh-token exchange! persist! ->token no-token! reuse-window-ms
           lock-path]}]
  (let [window
        (or reuse-window-ms default-reuse-window-ms)

        lock
        (new-lock)

        reuse
        (fn [rejected]
          (let [creds (require-usable-credentials! (load))]
            (when (and creds (fresh-within? (saved-at creds) window))
              (let [tok (->token creds)]
                ;; Never hand back the exact token the server just
                ;; rejected (401): a locally-fresh but server-rotated
                ;; token would otherwise be REUSED and 401 again. When it
                ;; differs, reuse still collapses the 401 storm.
                (when-not (= rejected (:token tok)) tok)))))

        refresh!
        (fn [rejected]
          (let [creds
                (require-usable-credentials! (load))

                rt
                (refresh-token creds)

                entry
                {:store store
                 :trigger (if rejected :rejected-token :expired)
                 :started-ms (util/now-ms)
                 :refresh-token rt}

                log!
                (fn [outcome & {:as more}]
                  (log-refresh! (merge entry {:outcome outcome} more)))]

            (when (str/blank? rt) (log! :no-refresh-token) (no-token!))
            (let [exchanged (try {:fresh (exchange! rt)}
                                 (catch Throwable e
                                   (if-not (invalid-grant? e)
                                     (do (log! :failed :error e) (throw e))
                                     (let [current (load)]
                                       ;; A sign-in can replace the file while the exchange is pending.
                                       (if (= rt (refresh-token current))
                                         (let [rejected (assoc current :reauth-required true)]
                                           (log! :rejected :error e)
                                           (persist! rejected)
                                           (require-usable-credentials! rejected))
                                         (do (log! :superseded :error e)
                                             (when (str/blank? (refresh-token current)) (no-token!))
                                             {:token (->token (require-usable-credentials!
                                                                current))}))))))]
              (if (contains? exchanged :token)
                (:token exchanged)
                (let [saved (try (persist! (:fresh exchanged))
                                 (catch Throwable e
                                   (log! :persist-failed
                                         :error e
                                         :next-refresh-token (refresh-token (:fresh exchanged)))
                                   (throw e)))]
                  (log! :refreshed :next-refresh-token (refresh-token saved))
                  (->token saved))))))

        ;; In-process monitor serializes threads in THIS JVM; the file lock
        ;; then serializes across JVMs. Order matters: hold the monitor first
        ;; so only one thread per JVM ever reaches the (non-reentrant) file
        ;; lock. After the winner persists and releases, the next process's
        ;; `reuse` reads the freshly-stamped file and reuses instead of
        ;; running a second rotating exchange.
        run
        (fn [rejected]
          (locking lock
            (call-with-file-lock lock-path #(or (reuse rejected) (refresh! rejected)))))]

    (fn ([] (run nil)) ([rejected] (run rejected)))))
