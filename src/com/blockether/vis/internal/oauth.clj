(ns com.blockether.vis.internal.oauth
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
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def default-reuse-window-ms
  "A refresh persisted/produced within this window is REUSED instead of
   re-run. Wide enough to absorb a 401 burst (every iteration's retry +
   the limits poll), short enough that a genuinely-stale token still
   refreshes promptly."
  15000)

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
   (boolean (and (number? saved-at-ms)
                 (< (- (System/currentTimeMillis) (long saved-at-ms)) (long window-ms))))))

(defn single-flight!
  "Serialize `refresh!` on `lock`. Once the lock is held, call `reuse`
   first: if it returns a non-nil result, another caller already refreshed
   while this thread waited for the lock, so return that WITHOUT a second
   exchange; otherwise run `refresh!` and return its result. `reuse` and
   `refresh!` are 0-arg fns; `reuse` returns the reusable token map or nil.

   Low-level core — prefer `make-file-refresher` / `refresher`."
  [lock reuse refresh!]
  (locking lock (or (reuse) (refresh!))))

(defn refresher
  "Build a single-flight refresh fn that OWNS a fresh lock. `refresh!` is as in
   `single-flight!`; `reuse` takes ONE arg — the REJECTED token (the access
   token the server just 401'd, or nil) — and returns the reusable token map or
   nil. For stores that aren't a plain rotating file — e.g. an in-memory cache
   (GitHub Copilot).

   The returned fn is 0/1-arity: call it WITH the rejected token on 401 recovery
   so `reuse` never hands back the very token that was just rejected; the 0-arity
   form (nil rejected) keeps plain freshness reuse."
  [reuse refresh!]
  (let [lock (new-lock)]
    (fn ([] (single-flight! lock #(reuse nil) refresh!))
      ([rejected] (single-flight! lock #(reuse rejected) refresh!)))))

(defn call-with-file-lock
  "Run `f` while holding an OS advisory lock on `lock-path`, serializing the
   critical section ACROSS processes (e.g. two vis JVMs — a `--jvm` client and
   its gateway daemon — sharing one credential file). The caller must already
   hold the in-process monitor, since a single JVM cannot hold two overlapping
   locks on the same file. Blank/nil `lock-path`, or any locking failure
   (unsupported filesystem, IO error, overlapping lock), degrades to running
   `f` UNLOCKED rather than blocking authentication."
  [lock-path f]
  (if (str/blank? lock-path)
    (f)
    (let [lf (io/file lock-path)]
      (try (io/make-parents lf)
           (with-open [ch (.getChannel (java.io.RandomAccessFile. lf "rw"))]
             (let [fl (.lock ch)]
               (try (f) (finally (.release fl)))))
           (catch java.nio.channels.OverlappingFileLockException _ (f))
           (catch Throwable _ (f))))))

(defn make-file-refresher
  "Build a 0/1-arg single-flight refresh fn for a FILE-backed credential
   store whose token endpoint ROTATES the refresh_token (Anthropic,
   OpenAI Codex). Owns its own lock; reuses creds persisted within the
   reuse window instead of running a second (rotating) exchange — UNLESS the
   reusable token equals the one just rejected (pass that token when calling the
   returned fn on 401 recovery), so a server-rotated but locally-fresh token is
   re-exchanged rather than handed back dead.

   opts — all required unless noted:
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
  [{:keys [load saved-at refresh-token exchange! persist! ->token no-token! reuse-window-ms
           lock-path]}]
  (let
    [window
     (or reuse-window-ms default-reuse-window-ms)

     lock
     (new-lock)

     reuse
     (fn [rejected]
       (let [creds (load)]
         (when (and creds (fresh-within? (saved-at creds) window))
           (let [tok (->token creds)]
             ;; Never hand back the exact token the server just
             ;; rejected (401): a locally-fresh but server-rotated
             ;; token would otherwise be REUSED and 401 again. When it
             ;; differs, reuse still collapses the 401 storm.
             (when-not (= rejected (:token tok)) tok)))))

     refresh!
     (fn []
       (let
         [creds
          (load)

          rt
          (refresh-token creds)]

         (when (str/blank? rt) (no-token!))
         (-> (exchange! rt)
             persist!
             ->token)))

     ;; In-process monitor serializes threads in THIS JVM; the file lock
     ;; then serializes across JVMs. Order matters: hold the monitor first
     ;; so only one thread per JVM ever reaches the (non-reentrant) file
     ;; lock. After the winner persists and releases, the next process's
     ;; `reuse` reads the freshly-stamped file and reuses instead of
     ;; running a second rotating exchange.
     run
     (fn [rejected]
       (locking lock (call-with-file-lock lock-path #(or (reuse rejected) (refresh!)))))]

    (fn ([] (run nil)) ([rejected] (run rejected)))))
