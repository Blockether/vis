(ns com.blockether.vis.tui.file-picker
  "Gateway-backed file suggestions for the active session."
  (:require [com.blockether.vis.tui.client :as client]))

(def ^:dynamic *session-id* nil)
(defonce ^:private warm? (atom false))

(defn index-warm? [] @warm?)
(defn prewarm-index! [] (reset! warm? true))

(defn fuzzy-file-rows
  [query {:keys [limit] :or {limit 20}}]
  (if-not *session-id*
    []
    (let [rows (client/suggest-files *session-id* query)]
      (reset! warm? true)
      (vec (take limit rows)))))
