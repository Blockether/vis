(ns com.blockether.vis.internal.session-model
  "Persistent, channel-NEUTRAL per-session model preference.

   ONE source of truth — `session_soul.model_pref` in the DB — for every
   channel (web gateway + TUI), so a session routes through the same model
   wherever it's opened and the choice survives process restarts. The engine
   reads it at turn start (`prepare-turn-context` in loop.clj) as the default
   `:model` when the caller passes none; `router-for-model` then hoists the
   chosen model with the rest of the fleet kept behind it as fallback.

   DEBOUNCED WRITE-BACK: a `set-model!` updates an in-memory value IMMEDIATELY
   (so the footer + engine see the new choice at once) and coalesces the DB
   write — cycling the model (TUI Ctrl+T) many times in a row produces a
   SINGLE write after the user settles, not one per keypress. Reads prefer the
   pending in-memory value, falling back to the DB.

   Keyed by the session-soul id — the same id the gateway's `sid` and the
   engine env's `:session-id` carry."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.persistance :as persistance])
  (:import
   (java.util.concurrent Executors ScheduledExecutorService ScheduledFuture
                         ThreadFactory TimeUnit)))

(def ^:private debounce-ms 600)

;; sid-string -> {:db-info <handle> :model <name-or-nil>}. Authoritative for
;; reads until its debounced flush lands, then removed (reads fall to the DB).
(defonce ^:private pending (atom {}))
(defonce ^:private flush-futures (atom {})) ; sid-string -> ScheduledFuture

(defonce ^:private scheduler
  (Executors/newSingleThreadScheduledExecutor
    (reify ThreadFactory
      (newThread [_ r]
        (doto (Thread. ^Runnable r "vis-session-model-flush") (.setDaemon true))))))

;; Short-TTL cache for per-frame DISPLAY readers (the TUI footer renders every
;; frame; the codebase avoids per-paint DB reads). Pending always wins over it.
(defonce ^:private display-cache (atom {})) ; sid-string -> {:v val :at ms}
(def ^:private display-ttl-ms 1500)

(defn- db-read [db-info sid]
  (some-> (persistance/db-get-session-model-pref db-info sid) str not-empty))

(defn- flush-one! [k]
  (when-let [{:keys [db-info model]} (get @pending k)]
    (try
      (persistance/db-set-session-model-pref! db-info k model)
      (finally
        (swap! pending dissoc k)
        (swap! flush-futures dissoc k)))))

(defn model-of
  "The model preferred for session `sid`, or nil for the router default.
   Prefers the immediate in-memory value, else the DB. Use on the routing
   path (engine, gateway)."
  [db-info sid]
  (when (and db-info sid)
    (let [k (str sid)]
      (if (contains? @pending k)
        (:model (get @pending k))
        (db-read db-info sid)))))

(defn model-of-cached
  "Like `model-of` but DISPLAY-oriented: when no pending value exists, a recent
   DB value is served from a tiny TTL cache so callers can read it every frame
   without a DB hit."
  [db-info sid]
  (when (and db-info sid)
    (let [k (str sid)]
      (if (contains? @pending k)
        (:model (get @pending k))
        (let [now (System/currentTimeMillis)
              c   (get @display-cache k)]
          (if (and c (< (- now (long (:at c))) display-ttl-ms))
            (:v c)
            (let [v (db-read db-info sid)]
              (swap! display-cache assoc k {:v v :at now})
              v)))))))

(defn set-model!
  "Set (or clear, with nil/blank) the model preference for session `sid`.
   Takes effect IMMEDIATELY for reads; the DB write is debounced
   (`debounce-ms`) so rapid cycling coalesces to one write. Returns the
   normalized model (or nil)."
  [db-info sid model]
  (when (and db-info sid)
    (let [model (some-> model str str/trim not-empty)
          k     (str sid)]
      (swap! pending assoc k {:db-info db-info :model model})
      (swap! display-cache dissoc k)
      (when-let [^ScheduledFuture old (get @flush-futures k)]
        (.cancel old false))
      (let [^ScheduledExecutorService s scheduler
            f (.schedule s ^Runnable (fn [] (flush-one! k))
                (long debounce-ms) TimeUnit/MILLISECONDS)]
        (swap! flush-futures assoc k f))
      model)))
