(ns com.blockether.vis.logging
  "Telemere setup: console handler + persistence-backed `log` handler.

   The persistence handler reads context from telemere's `*ctx*` dynamic
   var — callers bind `:db-info`, `:conversation-soul-id`, `:query-id`,
   `:iteration-id` into `*ctx*` and every `tel/log!` / `tel/event!`
   inside that scope lands in the right DB rows with the right FK scope."
  (:require
   [charred.api :as json]
   [com.blockether.vis.persistance.core :as persistance]
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
   `com.blockether.vis.persistance.core/log!` facade.

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
