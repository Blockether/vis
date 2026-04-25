(ns com.blockether.vis.logging
  "Telemere setup: console handler + SQLite DB handler.

   The DB handler automatically persists every signal to the `log` table.
   It reads context from telemere's `*ctx*` dynamic var — callers bind
   `:db-info`, `:conversation-soul-id`, `:query-id`, `:iteration-id`
   into `*ctx*` and every `tel/log!` / `tel/event!` inside that scope
   auto-lands in the right DB rows with the right FK scope."
  (:require
   [charred.api :as json]
   [honey.sql :as sql]
   [next.jdbc :as jdbc]
   [taoensso.telemere :as tel]
   [com.blockether.vis.persistance.base :as base]))

(defn- signal->db-row
  "Transform a telemere signal map into a `log` table row."
  [signal]
  (let [ctx   (or (:ctx signal) {})
        level (or (:level signal) :info)
        event (or (some-> (:id signal) str)
                (some-> (:ns signal) str)
                "unknown")]
    (cond-> {:id         (str (java.util.UUID/randomUUID))
             :level      (base/->kw level)
             :event      event
             :data       (try
                           (json/write-json-str
                             (cond-> {}
                               (:msg_ signal)  (assoc :msg (force (:msg_ signal)))
                               (:data signal)  (assoc :data (:data signal))
                               (:ns signal)    (assoc :ns (str (:ns signal)))
                               (:error signal) (assoc :error (str (:error signal)))))
                           (catch Throwable _ nil))
             :created_at (or (some-> (:inst signal) inst-ms)
                           (base/now-ms))}
      (:conversation-soul-id ctx) (assoc :conversation_soul_id (base/->id (:conversation-soul-id ctx)))
      (:query-id ctx)             (assoc :query_soul_id (base/->id (:query-id ctx)))
      (:iteration-id ctx)         (assoc :iteration_id (base/->id (:iteration-id ctx))))))

(defn handler:sqlite-db
  "Telemere handler that persists signals to the SQLite `log` table.

   The handler reads `:db-info` from the signal's telemere context (`*ctx*`).
   When `:db-info` is absent (no DB session active), the signal is silently
   dropped — console handler still prints it.

   Usage:
     (tel/add-handler! :db (handler:sqlite-db))

   Then in your code:
     (tel/with-ctx+ {:db-info db-info :conversation-soul-id conv-id}
       (tel/log! :info \"something happened\"))"
  ([] (handler:sqlite-db nil))
  ([_opts]
   (fn handler
     ([signal]
      (when-let [db-info (get-in signal [:ctx :db-info])]
        (when-let [ds (:datasource db-info)]
          (try
            (let [row  (signal->db-row signal)
                  stmt (sql/format {:insert-into :log :values [row]})]
              (jdbc/execute! ds stmt))
            (catch Throwable _ nil)))))
     ([] nil))))

(defn setup!
  "Initialize telemere: console + DB handler. Call once at startup."
  []
  ;; Console handler (human-readable)
  (tel/add-handler! :console
    (tel/handler:console {:output-fn (tel/format-signal-fn {})})
    {:min-level :info})

  ;; DB handler (auto-persist to SQLite log table)
  (tel/add-handler! :db
    (handler:sqlite-db)
    {:async {:mode :dropping :buffer-size 2048 :n-threads 1}
     :min-level :info}))
