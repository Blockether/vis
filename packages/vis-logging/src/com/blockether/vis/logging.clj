(ns com.blockether.vis.logging
  "Telemere setup: console handler + persistence-backed `log` handler.

   The persistence handler reads context from telemere's `*ctx*` dynamic
   var — callers bind `:db-info`, `:conversation-soul-id`, `:query-id`,
   `:iteration-id` into `*ctx*` and every `tel/log!` / `tel/event!`
   inside that scope auto-lands in the right DB rows with the right FK
   scope.

   ── Why dynaload? ─────────────────────────────────────────────────

   This package depends on `borkdude/dynaload` and NOT on any
   `vis-persistance` jar. The persistence facade fn is resolved on
   first use through `borkdude.dynaload/dynaload`, so:

   1. An embedder that only wants console logging can pull in
      vis-logging without inheriting the JDBC stack.
   2. When vis-persistance + a backend (e.g. vis-persistance-sqlite)
      are on the classpath AND have been required by the host
      application, the handler picks them up automatically.
   3. Hot-reload of `persistance.core` (e.g. backend swap) is
      transparent — `dynaload` returns a `LazyVar` that re-resolves
      on every call after first deref."
  (:require
   [borkdude.dynaload :as dl]
   [charred.api :as json]
   [taoensso.telemere :as tel]))

;; =============================================================================
;; Dynaload bindings — resolved on first call, no compile-time dep
;; =============================================================================

;; `dynaload` returns a LazyVar. It's IFn — call directly. The `:default`
;; sentinel keeps the handler from blowing up when no persistence jar
;; is present; the handler short-circuits in that case (see `handler:db`).
;; The facade's `log!` is the only entry point we need — it already
;; performs id/keyword normalization through `persistance.base`.
(def ^:private persistance-log!
  (dl/dynaload 'com.blockether.vis.persistance.core/log!
    {:default ::no-persistance}))

(defn- persistance-available?
  "True iff the persistence facade resolved successfully."
  []
  (not= ::no-persistance @persistance-log!))

;; =============================================================================
;; Signal → log entry
;; =============================================================================

(defn- signal->entry
  "Transform a telemere signal into the entry map accepted by the
   persistence facade's `log!`. The facade fills in `:id`/`:created_at`
   and converts ids/keywords through `persistance.base`, so this fn
   only carries the semantic payload."
  [signal]
  (let [ctx   (or (:ctx signal) {})
        level (or (:level signal) :info)
        event (or (some-> (:id signal) str)
                (some-> (:ns signal) str)
                "unknown")
        data  (try
                (json/write-json-str
                  (cond-> {}
                    (:msg_ signal)  (assoc :msg (force (:msg_ signal)))
                    (:data signal)  (assoc :data (:data signal))
                    (:ns signal)    (assoc :ns (str (:ns signal)))
                    (:error signal) (assoc :error (str (:error signal)))))
                (catch Throwable _ nil))]
    (cond-> {:level level
             :event event
             :data  data}
      (:conversation-soul-id ctx) (assoc :conversation-soul-id (:conversation-soul-id ctx))
      (:query-id ctx)             (assoc :query-soul-id (:query-id ctx))
      (:iteration-id ctx)         (assoc :iteration-id (:iteration-id ctx)))))

;; =============================================================================
;; Handler
;; =============================================================================

(defn handler:db
  "Telemere handler that persists every signal through the
   `com.blockether.vis.persistance.core/log!` facade.

   The handler reads `:db-info` from the signal's telemere context
   (`*ctx*`). When `:db-info` is absent (no DB connection active in
   scope), the signal is silently dropped — the console handler still
   prints it. When the persistence facade isn't on the classpath at
   all, every signal is silently dropped (see `persistance-available?`).

   Usage:
     (tel/add-handler! :db (handler:db))

     (tel/with-ctx+ {:db-info db-info :conversation-soul-id conv-id}
       (tel/log! :info \"something happened\"))"
  ([] (handler:db nil))
  ([_opts]
   (fn handler
     ([signal]
      (when-let [db-info (get-in signal [:ctx :db-info])]
        (when (persistance-available?)
          (try
            (persistance-log! db-info (signal->entry signal))
            (catch Throwable _ nil)))))
     ([] nil))))

(defn setup!
  "Initialize telemere: console + persistence handler. Call once at
   startup."
  []
  ;; Console handler (human-readable)
  (tel/add-handler! :console
    (tel/handler:console {:output-fn (tel/format-signal-fn {})})
    {:min-level :info})

  ;; Persistence handler (auto-routes through whichever backend is loaded)
  (tel/add-handler! :db
    (handler:db)
    {:async {:mode :dropping :buffer-size 2048 :n-threads 1}
     :min-level :info}))
