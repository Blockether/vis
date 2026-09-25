(ns com.blockether.vis.internal.provider.auth-health
  "Process-wide credential health of model providers.

   Owns the forced-refresh circuit breaker, the post-refresh propagation marker,
   the escalating authentication cooldown and the single-flight interactive
   re-authentication. The iteration loop decides how a turn recovers from a
   rejected credential; this namespace records what happened to each provider and
   answers whether its credential may be refreshed or routed to.

   The state is process-wide on purpose: router builds, every session's turns and
   the gateway `/metrics` endpoint share one breaker and one cooldown, and
   concurrent turns share one interactive authentication per provider."
  (:require [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.provider.limits :as provider-limits]
            [com.blockether.vis.internal.util :as util]))

(def AUTH_REFRESH_WINDOW_MS
  "Rolling window (ms) for the forced-OAuth-refresh circuit breaker."
  60000)

(def AUTH_REFRESH_WINDOW_MAX
  "Max forced OAuth refreshes for one provider inside `AUTH_REFRESH_WINDOW_MS`
   before the breaker trips. Legitimate rotation refreshes at most a handful of
   times a minute; more than this is a flap, not real rotation."
  6)

(def ^:private AUTH_PROPAGATION_WINDOW_MS
  "How long (ms) after a FORCED OAuth refresh a subsequent auth 401 reads as
   PROPAGATION LAG (retry the same freshly-minted token with backoff) rather
   than a dead credential (re-mint). Comfortably exceeds the loop's full post-refresh
   backoff sequence (`MAX_AUTH_REFRESH_RETRIES` retries of
   `auth-propagation-backoff-ms`, ~11s) so the whole settling burst stays
   classified as lag; the marker is cleared on the first accepted request so it
   never lingers into a later genuine rotation."
  30000)

(defonce ^:private refresh-events
  ;; provider-id -> vector of epoch-ms timestamps of recent forced refreshes.
  (atom {}))

(defonce ^:private last-refreshed
  ;; provider-id -> {:at <epoch-ms of the last FORCED refresh>}. A recency
  ;; marker: a fresh auth 401 within AUTH_PROPAGATION_WINDOW_MS of it reads as
  ;; PROPAGATION LAG (back off, retry the SAME token), not a dead credential.
  ;; Cleared on the first accepted request by `note-ok!`.
  (atom {}))

(def AUTH_COOLDOWN_MS
  "BASE window (ms) a provider stays EXCLUDED from routing after its credentials were
   rejected and the turn had to rescue itself on another provider.

   The rescue route itself is per-ITERATION state, so without a process-wide
   cooldown the very next iteration re-probes the dead credential: every single
   iteration then pays a 401 round-trip, a fallback log line and a visible
   progress chunk until the user re-authenticates."
  300000)

(def ^:private AUTH_COOLDOWN_MAX_MS
  "Ceiling for the escalating auth cooldown, and how long a lapsed strike record is
   kept before the streak is forgotten. A credential nobody has repaired for hours
   must not buy a fresh 401 + refresh + fallback dance every five minutes for the
   rest of the day (issue #154)."
  3600000)

(defn- cooldown-window-ms
  "Window (ms) for the Nth UNBROKEN credential rejection: the base window doubled per
   strike, capped at [[AUTH_COOLDOWN_MAX_MS]]. One accepted request clears the streak
   (`note-ok!`), so a re-authenticated provider never serves an
   escalated window — only a provider that keeps rejecting is probed ever less often."
  ^long [strikes]
  (let [n (max 0 (dec (long (or strikes 1))))]
    (min (long AUTH_COOLDOWN_MAX_MS) (bit-shift-left (long AUTH_COOLDOWN_MS) (min 8 n)))))

(defonce ^:private cooldowns
  ;; provider-id -> {:until <epoch-ms>, :since <epoch-ms>, :hits <long>, :strikes <long>}.
  ;; Opened by `note-failure!` when auth recovery is exhausted and the
  ;; turn falls back to another provider; closed by `note-ok!` as soon
  ;; as the provider accepts a request again (fresh login / rotated key). `:hits` counts
  ;; the fallbacks inside the CURRENT window, `:strikes` the unbroken rejections across
  ;; windows — the escalation reads the latter, so a lapsed window that fails again does
  ;; not restart at the base window.
  (atom {}))

(defn note-failure!
  "Open (or extend) the auth cooldown for `pid` after a fallback. Returns true only
   for the FIRST trip of a cooldown window so the caller can log the escape once at
   :warn and keep the repeats at :debug."
  [pid]
  (boolean
    (when pid
      (let [now
            (util/now-ms)

            after
            (swap! cooldowns (fn [m]
                               (let [prev
                                     (get m pid)

                                     live?
                                     (and prev (> (long (:until prev)) now))

                                     strikes
                                     (inc (long (or (:strikes prev) 0)))]

                                 (assoc m
                                   pid {:until (+ now (cooldown-window-ms strikes))
                                        :since (if prev (:since prev) now)
                                        :strikes strikes
                                        :hits (if live? (inc (long (:hits prev))) 1)}))))]

        (= 1 (long (:hits (get after pid))))))))

(defn- clear-cooldown!
  "Close the auth cooldown for `pid`; called once the provider accepts a request.
   Drops the strike streak with it, so a repaired credential starts from the base
   window again. Returns true when a cooldown was actually cleared."
  [pid]
  (boolean (when (and pid (contains? @cooldowns pid)) (swap! cooldowns dissoc pid) true)))

(defn cooled
  "Set of providers whose credentials are still inside their auth cooldown. Prunes
   records whose window lapsed more than [[AUTH_COOLDOWN_MAX_MS]] ago on the way, so
   the map cannot grow without bound while a recent streak still outlives its own
   window and keeps the escalation honest."
  []
  (let [now
        (util/now-ms)

        live
        (swap! cooldowns (fn [m]
                           (into {}
                                 (filter (fn [[_ v]]
                                           (> (+ (long (:until v)) (long AUTH_COOLDOWN_MAX_MS))
                                              now)))
                                 m)))]

    (set (keep (fn [[k v]]
                 (when (> (long (:until v)) now) k))
               live))))

(defn cooldown-metrics
  "Observability snapshot of the per-provider auth cooldown: the BASE window, the
   ceiling it escalates to, and the providers still excluded — each with the epoch-ms
   the exclusion lifts, how many fallbacks landed inside the window and how many
   unbroken strikes set its length."
  []
  (let [cooled (cooled)]
    {:cooldown-ms AUTH_COOLDOWN_MS
     :cooldown-max-ms AUTH_COOLDOWN_MAX_MS
     :cooled-providers cooled
     :cooldowns (select-keys @cooldowns cooled)}))

(defn refresh-allowed?
  "Circuit breaker for forced OAuth refreshes. Atomically prunes timestamps older
   than the rolling window for `pid`, records this attempt ONLY when it is
   GRANTED, and returns true while the provider is still under the per-window
   budget. When it returns false the breaker is OPEN: the caller must NOT refresh
   and must recover without touching the token endpoint, so the user
   re-authenticates once instead of the daemon flapping it.

   Recording only GRANTED refreshes is what lets the breaker CLOSE again. An
   earlier version stamped every call, denials included, so a fleet of tabs still
   retrying inside the window kept re-arming the breaker they had just tripped:
   the window never drained and the process stayed in permanent auth fallback
   until restart, even once the on-file token was healthy again."
  [pid]
  (let [now
        (util/now-ms)

        cutoff
        (- now (long AUTH_REFRESH_WINDOW_MS))

        live
        (fn [ts]
          (filterv #(> (long %) cutoff) (or ts [])))

        [before after]
        (swap-vals! refresh-events
                    update
                    pid
                    (fn [ts]
                      (let [kept (live ts)]
                        (cond-> kept
                          (< (long (count kept)) (long AUTH_REFRESH_WINDOW_MAX))
                          (conj now)))))]

    (> (long (count (get after pid))) (long (count (live (get before pid)))))))

(defn refresh-metrics
  "Observability snapshot of the OAuth-refresh circuit breaker. Returns the
   rolling window, the trip threshold, the per-provider count of forced
   refreshes still inside the window, and the set of providers at the refresh
   limit (breaker OPEN). Surfaced by the gateway `/metrics` endpoint so an
   auth-refresh flap is visible at a glance instead of needing a `vis.log`
   grep."
  []
  (let [cutoff
        (- (util/now-ms) (long AUTH_REFRESH_WINDOW_MS))

        in-window
        (into {}
              (for [[pid ts]
                    @refresh-events

                    :let [n
                          (long (count (filter #(> (long %) cutoff) ts)))]
                    :when (pos? n)]

                [pid n]))]

    {:window-ms AUTH_REFRESH_WINDOW_MS
     :max-per-window AUTH_REFRESH_WINDOW_MAX
     :refreshes-in-window in-window
     :breaker-open (into #{}
                         (keep (fn [[pid n]]
                                 (when (>= (long n) (long AUTH_REFRESH_WINDOW_MAX)) pid)))
                         in-window)}))

(defn note-refreshed!
  "Stamp a FORCED refresh of `pid` now, opening its propagation window."
  [pid]
  (swap! last-refreshed assoc pid {:at (util/now-ms)})
  nil)

(defn forget-refresh!
  "Drop the propagation marker of `pid`, so a rejection during interactive
   re-authentication is not read as lag of an earlier refresh."
  [pid]
  (swap! last-refreshed dissoc pid)
  nil)

(defn propagation-lag?
  "True while the last FORCED refresh of `pid` is younger than
   [[AUTH_PROPAGATION_WINDOW_MS]]: a new authentication rejection then reads as
   propagation lag of the freshly minted token, not as a dead credential."
  [pid]
  (boolean (when-let [{:keys [at]} (get @last-refreshed pid)]
             (< (- (util/now-ms) (long at)) (long AUTH_PROPAGATION_WINDOW_MS)))))

(defn note-ok!
  "Record that `pid` ACCEPTED a request: close its propagation window and its auth
   cooldown, so a later rotation is a fresh rejection and a re-authenticated
   provider re-enters routing at once. Returns true when a cooldown was cleared."
  [pid]
  (when (contains? @last-refreshed pid) (swap! last-refreshed dissoc pid))
  (clear-cooldown! pid))

(defn managed?
  "True when `provider` is managed and can both sign in interactively and read its token."
  [provider]
  (boolean (and (:provider/is-managed provider)
                (:provider/auth-fn provider)
                (:provider/get-token-fn provider))))

(defonce ^:private auth-flights
  ;; provider-id -> promise carrying one first-use authentication result. The map only
  ;; contains live attempts; the leader removes its own promise after delivery.
  (atom {}))

(defn usable-token?
  "True when `envelope` carries a non-blank token other than the `rejected` one."
  [envelope rejected]
  (and (util/non-blank-string? (:token envelope)) (not= rejected (:token envelope))))

(defn- managed-auth-failure
  [pid message cause]
  (ex-info (str "Authentication for " (name pid) " " message)
           {:type :provider/authentication-failed :provider pid}
           cause))

(defn reauthenticate!
  "Run at most one interactive authentication for `pid`; concurrent turns await the
   same result. Re-read storage inside the flight to adopt a peer credential, but
   never accept the token rejected by the request that triggered recovery."
  [pid provider rejected]
  (let [candidate
        (promise)

        [_ flights]
        (swap-vals! auth-flights
                    (fn [current]
                      (if (contains? current pid) current (assoc current pid candidate))))

        flight
        (get flights pid)

        leader?
        (identical? candidate flight)]

    (when leader?
      (let [get-token-fn
            (:provider/get-token-fn provider)

            auth-fn
            (:provider/auth-fn provider)

            outcome
            (try (let [before
                       (try (get-token-fn) (catch Throwable _ nil))

                       envelope
                       (if (usable-token? before rejected)
                         before
                         (do (auth-fn (constantly nil)) (get-token-fn)))]

                   (if (usable-token? envelope rejected)
                     (do (provider-limits/auth-changed! pid) {:value envelope})
                     {:error (managed-auth-failure
                               pid
                               "was cancelled or did not produce a usable credential."
                               nil)}))
                 (catch Throwable t
                   {:error (if (= :provider/authentication-failed (:type (ex-data t)))
                             t
                             (managed-auth-failure
                               pid
                               (str "failed: " (or (ex-message t) "unknown authentication error"))
                               t))}))]

        (deliver candidate outcome)
        (swap! auth-flights (fn [current]
                              (if (identical? candidate (get current pid))
                                (dissoc current pid)
                                current)))))
    (let [{:keys [value error]} @flight]
      (if error (throw error) value))))

(defn ensure-authenticated!
  "Resolve `pid` immediately before a real provider request. A managed provider with
   an auth function authenticates only when its token lookup has no usable credential;
   startup, status probes, and picker rendering never call this function."
  [pid]
  (let [provider
        (registry/provider-by-id pid)

        get-token-fn
        (:provider/get-token-fn provider)]

    (when (managed? provider)
      (let [envelope (try (get-token-fn) (catch Throwable _ nil))]
        (if (usable-token? envelope nil) envelope (reauthenticate! pid provider nil))))))
