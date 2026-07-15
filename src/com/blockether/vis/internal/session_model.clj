(ns com.blockether.vis.internal.session-model
  "Persistent, channel-NEUTRAL per-session model preference.

   ONE source of truth — `session_soul.llm_pref_provider` + `llm_pref_model`
   in the DB — for every channel (web gateway + TUI), so a session routes
   through the same PROVIDER + MODEL wherever it's opened and the choice
   survives restarts. Provider + model (not just a model name) mirrors how a
   turn records its route and disambiguates a model name shared by >1 provider.

   The engine reads it at turn start (`prepare-turn-context` in loop.clj) as
   the default route when the caller passes none; `router-for-model` hoists the
   chosen model (the provider follows, since it's the one carrying that model).

   DEBOUNCED WRITE-BACK: `set-model!` updates an in-memory value IMMEDIATELY
   (footer + engine see it at once) and coalesces the DB write, so cycling the
   model (TUI Ctrl+T) many times in a row produces a SINGLE write. Reads prefer
   the pending in-memory value, falling back to the DB.

   Values are `{:provider <id-string-or-nil> :model <name>}` or nil. Keyed by
   the session-soul id (the gateway's `sid` and the engine env's `:session-id`)."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.persistance :as persistance])
  (:import (java.util.concurrent Executors
                                 ScheduledExecutorService
                                 ScheduledFuture
                                 ThreadFactory
                                 TimeUnit)))

(def ^:private debounce-ms 600)

;; sid-string -> {:db-info <handle> :provider <id-or-nil> :model <name-or-nil>}.
;; Authoritative for reads until its debounced flush lands, then removed.
(defonce ^:private pending (atom {}))
(defonce ^:private flush-futures (atom {})) ; sid-string -> ScheduledFuture

(defonce ^:private scheduler
  (Executors/newSingleThreadScheduledExecutor
    (reify
      ThreadFactory
        (newThread [_ r]
          (doto (Thread. ^Runnable r "vis-session-model-flush") (.setDaemon true))))))

;; Short-TTL cache for per-frame DISPLAY readers (the TUI footer renders every
;; frame; the codebase avoids per-paint DB reads). Pending always wins over it.
(defonce ^:private display-cache (atom {})) ; sid-string -> {:v val :at ms}
(def ^:private display-ttl-ms 1500)

(defn- db-read [db-info sid] (persistance/db-get-session-model-pref db-info sid)) ; {:provider :model} or nil

(defn- pending->val
  "The {:provider :model} for a pending entry, or nil when its model is blank
   (a cleared / router-default preference)."
  [{:keys [provider model]}]
  (when model {:provider provider :model model}))

(defn- flush-one!
  [k]
  (when-let [{:keys [db-info provider model]} (get @pending k)]
    (try (persistance/db-set-session-model-pref! db-info k provider model)
         (finally (swap! pending dissoc k) (swap! flush-futures dissoc k)))))

(defn model-of
  "The preference for session `sid` as `{:provider :model}`, or nil for the
   router default. Prefers the immediate in-memory value, else the DB. Use on
   the routing path (engine, gateway)."
  [db-info sid]
  (when (and db-info sid)
    (let [k (str sid)]
      (if (contains? @pending k) (pending->val (get @pending k)) (db-read db-info sid)))))

(defn model-of-cached
  "Like `model-of` but DISPLAY-oriented: when no pending value exists, a recent
   DB value is served from a tiny TTL cache so callers can read it every frame
   without a DB hit."
  [db-info sid]
  (when (and db-info sid)
    (let [k (str sid)]
      (if (contains? @pending k)
        (pending->val (get @pending k))
        (let [now (System/currentTimeMillis)
              c (get @display-cache k)
              at (long (or (:at c) 0))
              c (get @display-cache k)]

          (if (and c (< (- now at) (long display-ttl-ms)))
            (:v c)
            (let [v (db-read db-info sid)]
              (swap! display-cache assoc k {:v v :at now})
              v)))))))

(defn set-model!
  "Set (or clear, with blank model) the PROVIDER + MODEL preference for session
   `sid`. Takes effect IMMEDIATELY for reads; the DB write is debounced so
   rapid cycling coalesces to one write. Returns `{:provider :model}` (or nil)."
  [db-info sid provider model]
  (when (and db-info sid)
    (let [model
          (some-> model
                  str
                  str/trim
                  not-empty)

          provider
          (some-> provider
                  str
                  str/trim
                  not-empty)

          k
          (str sid)]

      (swap! pending assoc k {:db-info db-info :provider provider :model model})
      (swap! display-cache dissoc k)
      (when-let [^ScheduledFuture old (get @flush-futures k)]
        (.cancel old false))
      (let [^ScheduledExecutorService s
            scheduler

            f
            (.schedule s
                       ^Runnable
                       (fn []
                         (flush-one! k))
                       (long debounce-ms)
                       TimeUnit/MILLISECONDS)]

        (swap! flush-futures assoc k f))
      (when model {:provider provider :model model}))))
