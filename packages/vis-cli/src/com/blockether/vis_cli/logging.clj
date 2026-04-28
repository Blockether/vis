(ns com.blockether.vis-cli.logging
  "Persistence-backed Telemere handler.

   Installs a `:db` handler that persists every Telemere signal
   through the `com.blockether.vis-persistance.core/log!` facade,
   scoped by ids carried in Telemere's `*ctx*` dynamic var. Callers
   bind `:db-info`, `:conversation-soul-id`, `:query-id`,
   `:iteration-id` into `*ctx*` (via `tel/with-ctx+`) and every
   `tel/log!` / `tel/event!` inside that scope lands in the right DB
   rows with the right FK scope.

   Console + file handlers are NOT this namespace's job — vis-cli's
   `configure-logging!` owns those. This namespace owns ONE thing:
   `setup-db-handler!` (and its underlying `handler:db` factory)."
  (:require
   [charred.api :as json]
   [com.blockether.vis-persistance.core :as persistance]
   [taoensso.telemere :as tel]))

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
   `com.blockether.vis-persistance.core/log!` facade.

   The handler reads `:db-info` from the signal's telemere context
   (`*ctx*`). When `:db-info` is absent (no DB connection active in
   scope), the signal is silently dropped — the console handler still
   prints it.

   Usage:
     (tel/add-handler! :db (handler:db))

     (tel/with-ctx+ {:db-info db-info :conversation-soul-id conv-id}
       (tel/log! :info \"something happened\"))"
  ([] (handler:db nil))
  ([_opts]
   (fn handler
     ([signal]
      (when-let [db-info (get-in signal [:ctx :db-info])]
        (try
          (persistance/log! db-info (signal->entry signal))
          (catch Throwable _ nil))))
     ([] nil))))

(defn setup-db-handler!
  "Install the `:db` Telemere handler. Idempotent — reusing the same
   handler key replaces the previous registration. Call once at
   process startup, after the persistence backend is loaded
   (otherwise the handler will silently drop signals because no
   backend is registered with the facade yet).

   The handler is asynchronous (dropping mode, 2048-entry buffer,
   single drain thread) so a slow DB write never back-pressures the
   call site that emitted the signal."
  []
  (tel/add-handler! :db
    (handler:db)
    {:async     {:mode :dropping :buffer-size 2048 :n-threads 1}
     :min-level :info}))
