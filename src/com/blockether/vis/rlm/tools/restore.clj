(ns com.blockether.vis.rlm.tools.restore
  "Restore tools — fetch persisted data vars from prior iterations and
   re-bind them into the current SCI sandbox. `make-restore-var-fn` is the
   raw fetch; `make-restore-vars-fn` wraps it for batched calls and
   collects per-symbol errors into the returned map."
  (:require
   [com.blockether.vis.rlm.persistence.db :as db]))

(defn make-restore-var-fn
  "Creates restore-var for fetching the latest persisted data var from prior iterations."
  [db-info conversation-ref]
  (fn restore-var
    ([sym]
     (restore-var sym {}))
    ([sym opts]
     (if db-info
       (let [sym (if (symbol? sym) sym (symbol (str sym)))
             registry (db/db-latest-var-registry db-info conversation-ref
                        (select-keys (or opts {}) [:max-scan-queries]))]
         (if-let [{:keys [value]} (get registry sym)]
           value
           (throw (ex-info (str "No restorable var found for " sym)
                    {:type :rlm/restore-var-missing :symbol sym}))))
       (throw (ex-info "No DB available for restore-var" {:type :rlm/no-db}))))))

(defn make-restore-vars-fn
  "Creates restore-vars for batch fetching latest persisted data vars."
  [restore-var-fn]
  (fn restore-vars
    ([syms]
     (restore-vars syms {}))
    ([syms opts]
     (into {}
       (map (fn [sym]
              (try
                [sym (restore-var-fn sym opts)]
                (catch Exception e
                  [sym {:error {:type (:type (ex-data e))
                                :symbol sym
                                :message (ex-message e)}}]))))
       syms))))
