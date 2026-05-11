(ns com.blockether.vis.internal.history-restore
  "Common helpers for displaying persisted conversation history.

   Persistence may store an iteration block result as `{:vis/ref :expr}` when
   the live return value is runtime-only (for example a SCI Var returned by a
   `(def ...)` form). The actual var value can still be present in expression
   state. These helpers reconnect that durable value to history renderers so
   every channel can show what the user saw, instead of a runtime-ref
   placeholder."
  (:require [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis.internal.prompt :as prompt]))

(defn runtime-ref?
  "True when `v` is the persistence sentinel for a runtime-only value."
  [v]
  (and (map? v) (= :expr (:vis/ref v))))

(defn def-symbol-name
  "Return the var name defined by a simple `(def ...)`/`(defonce ...)` form."
  [code]
  (when (string? code)
    (some-> (re-find #"^\s*\(\s*(?:clojure.core/)?(?:def|defonce)\s+([^\s\)]+)" code)
      second)))

(defn restored-var-values
  "Map persisted var name string -> durable value for a conversation.

   Runtime-ref values are omitted because they cannot improve a history
   display. Callers can use this once per resume/render pass, then feed the
   resulting map to `restored-def-result`."
  [db-info conversation-id]
  (into {}
    (keep (fn [{:keys [name result]}]
            (when (and name (not (runtime-ref? result)))
              [(str name) result])))
    (persistance/db-restore-blocks db-info conversation-id)))

(defn restored-def-result
  "Return a bounded display string for `code` when it is a `(def ...)` whose
   value exists in `restored-values`; otherwise nil."
  [restored-values code]
  (when-let [name (def-symbol-name code)]
    (when (contains? restored-values name)
      (prompt/safe-pr-str (get restored-values name)))))
