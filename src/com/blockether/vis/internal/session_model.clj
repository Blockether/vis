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
            [com.blockether.vis.internal.persistance :as persistance]
            [taoensso.telemere :as tel])
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

(defn pending-pref
  "The UNFLUSHED in-memory preference for `sid` as `[pending? value]`.

   Callers that already hold the session's persisted `llm_pref_*` columns (the
   gateway soul reads the whole `session_soul` row anyway) use this to honour a
   just-made pick during its debounce window WITHOUT paying for a DB read. The
   two-element answer distinguishes \"nothing pending\" from \"pending CLEAR\"
   (back to the router default), which a bare nil cannot."
  [sid]
  (let [k
        (str sid)

        p
        @pending]

    (if (contains? p k) [true (pending->val (get p k))] [false nil])))

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

;; ── Change listeners ────────────────────────────────────────────────────────
;; The store is shared, but every attached surface keeps its OWN display copy (the
;; TUI footer chip, the companion header, the web rail). A pick set by the ENGINE —
;; the auth rescue in loop.clj repointing a session off a dead credential — has to
;; reach those surfaces the same way a picker change does, so EVERY writer funnels
;; through `set-model!` and `set-model!` broadcasts. Keeping the listener here (and
;; not an `append-event!` call in the gateway) is what makes that true for writers
;; that never touch the gateway facade.

(defonce ^:private model-listeners
  ;; #{listener-fn ...} - fns of [sid {:provider :model :reason}], fired on EVERY
  ;; session's pick change.
  (atom #{}))

(defn add-model-listener!
  "Register `listener-fn` to observe pick changes across ALL sessions. It is invoked
   with the session id and `{:provider :model :reason}` — provider/model blank when the
   override was cleared, `:reason` naming why a NON-manual writer moved the pick.

   Returns the listener fn so callers can pass it to `remove-model-listener!` later."
  [listener-fn]
  (swap! model-listeners conj listener-fn)
  listener-fn)

(defn remove-model-listener!
  "Deregister a previously added model listener. Idempotent."
  [listener-fn]
  (swap! model-listeners disj listener-fn)
  nil)

(defn- broadcast-model-change!
  "Fire every registered listener with `sid` and the new pick. Listeners that throw are
   swallowed and logged — a misbehaving channel must never reject a model change."
  [sid pick]
  (doseq [f @model-listeners]
    (try (f sid pick)
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::model-listener-failed
                      :data {:session-id (str sid) :error (ex-message t)}}
                     (str "Session-model listener threw: " (ex-message t)))))))
(defn set-model!
  "Set (or clear, with blank model) the PROVIDER + MODEL preference for session
   `sid`. Takes effect IMMEDIATELY for reads; the DB write is debounced so
   rapid cycling coalesces to one write. Returns `{:provider :model}` (or nil).

   `reason` names why a writer that is NOT the human moved the pick (the engine's
   `:authentication-fallback` rescue); it rides the broadcast so a surface can say
   why the chip changed under the user's hands. nil for a manual pick. An
   idempotent write neither schedules storage nor announces a change."
  ([db-info sid provider model] (set-model! db-info sid provider model nil))
  ([db-info sid provider model reason]
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

           result
           (when model {:provider provider :model model})

           before
           (model-of db-info sid)

           k
           (str sid)]

       (if (= before result)
         result
         (do (swap! pending assoc k {:db-info db-info :provider provider :model model})
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
             ;; Broadcast AFTER the in-memory value is live, so a listener that re-reads
             ;; the pick sees the new one.
             (broadcast-model-change! sid {:provider provider :model model :reason reason})
             result))))))

(defn record-switch!
  "Persist one manual model-preference transition for the session-usage routing
   ledger. This is deliberately synchronous and best-effort: preference writes
   remain debounced, while losing audit telemetry must never reject a picker
   change. Returns the created sidecar row, or nil when nothing changed/failed."
  [db-info sid from to source]
  (when (and db-info sid (not= from to))
    (try (persistance/db-create-extension-aggregate! db-info
                                                     {:extension-id "vis"
                                                      :aggregate-key (str (random-uuid))
                                                      :kind :session-model-switch
                                                      :session-soul-id sid
                                                      :content {:from from :to to :source source}})
         (catch Throwable _ nil))))
