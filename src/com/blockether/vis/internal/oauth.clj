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
  (:require [clojure.string :as str]))

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
  "Build a 0-arg single-flight refresh fn that OWNS a fresh lock. `reuse`
   and `refresh!` are as in `single-flight!`. For stores that aren't a
   plain rotating file — e.g. an in-memory cache (GitHub Copilot)."
  [reuse refresh!]
  (let [lock (new-lock)]
    (fn []
      (single-flight! lock reuse refresh!))))

(defn make-file-refresher
  "Build a 0-arg single-flight refresh fn for a FILE-backed credential
   store whose token endpoint ROTATES the refresh_token (Anthropic,
   OpenAI Codex). Owns its own lock; reuses creds persisted within the
   reuse window instead of running a second (rotating) exchange.

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

   Returns the provider-token map produced by `:->token`."
  [{:keys [load saved-at refresh-token exchange! persist! ->token no-token! reuse-window-ms]}]
  (let [window (or reuse-window-ms default-reuse-window-ms)]
    (refresher (fn []
                 (let [creds (load)]
                   (when (and creds (fresh-within? (saved-at creds) window)) (->token creds))))
               (fn []
                 (let [creds (load)
                       rt (refresh-token creds)]

                   (when (str/blank? rt) (no-token!))
                   (-> (exchange! rt)
                       persist!
                       ->token))))))
