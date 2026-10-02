(ns com.blockether.vis.tui.settings-model
  "Typed, owner-specific drafts. Gateway writes are separate from terminal preferences."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]))

(defn rows [catalog] (mapcat #(get % "toggles") (get catalog "groups")))

(defn setting-value [row] (if (= "boolean" (get row "type")) (get row "enabled") (get row "value")))

(defn start [catalog] {:base catalog :changes {} :latest nil :error nil})

(defn dirty? [draft] (boolean (seq (:changes draft))))

(defn receive
  [draft catalog]
  (if (and (dirty? draft) (not= (get-in draft [:base "revision"]) (get catalog "revision")))
    (assoc draft :latest catalog)
    (if (dirty? draft) draft (start catalog))))

(defn stage
  [draft id action value]
  (let [row
        (first (filter #(= id (get % "id")) (rows (:base draft))))

        unchanged?
        (if (= action "inherit")
          (not (get row "is_override"))
          (and (get row "is_override") (= value (get row "own_value"))))]

    (when-not (and row (#{"value" "inherit"} action))
      (throw (ex-info "Load this setting before editing it" {:id id})))
    (-> draft
        (assoc :error nil)
        (update :changes
                #(if unchanged?
                   (dissoc % id)
                   (assoc %
                     id (cond-> {"id" id "action" action}
                          (= action "value")
                          (assoc "value" value))))))))

(defn preview-row
  [draft row]
  (if-let [change (get (:changes draft) (get row "id"))]
    (let [inherit? (= "inherit" (get change "action"))
          value (if inherit? (get row "inherited_value") (get change "value"))]

      (assoc row
        (if (= "boolean" (get row "type")) "enabled" "value") value
        "source" (if inherit? (get row "inherited_source") (get-in draft [:base "scope"]))
        "is_override" (not inherit?)
        "own_value" (when-not inherit? value)
        "pending" true))
    row))

(defn preview
  [draft]
  (update (:base draft)
          "groups"
          #(mapv (fn [group]
                   (update group
                           "toggles"
                           (fn [items]
                             (mapv (partial preview-row draft) items))))
                 %)))

(defn changes [draft] (mapv val (sort-by key (:changes draft))))

(defn permission-change?
  [draft]
  (boolean (some #(or (str/starts-with? % "jail_") (= % "workspace_filesystem"))
                 (keys (:changes draft)))))

(defn rebase
  [draft]
  (if-let [catalog (:latest draft)]
    (let [available (set (map #(get % "id") (rows catalog)))]
      (when-not (every? available (keys (:changes draft)))
        (throw
          (ex-info
            "A drafted setting is no longer available. Keep this draft for review or discard it."
            {})))
      (assoc draft
        :base catalog
        :latest nil
        :error nil))
    draft))

(defn review-lines
  [draft]
  (let [by-id (into {} (map (juxt #(get % "id") identity)) (rows (:base draft)))]
    (vec (mapcat (fn [[id change]]
                   (let [row (get by-id id)]
                     [(str (get row "label") " · " (get change "action"))
                      (str "Before: " (wire/json-str (setting-value row)))
                      (str "After: " (wire/json-str (setting-value (preview-row draft row)))) ""]))
                 (sort-by key (:changes draft))))))

(defn config-property
  [definition property]
  (get-in (document/schema-document "config") ["$defs" definition "properties" property]))

(defn- transferable?
  [row]
  (or (#{"boolean" "enum" "number"} (get row "type"))
      (#{"agent_name" "workspace_filesystem" "jail_filesystem" "jail_network" "jail_deny_exec"
         "jail_environment" "jail_enabled" "jail_keychain"}
       (get row "id"))))

(defn export-profile
  [name catalog]
  {"version" 1
   "name" name
   "changes" (vec (keep (fn [row]
                          (when (and (transferable? row) (get row "is_override"))
                            {"id" (get row "id") "action" "value" "value" (get row "own_value")}))
                        (rows catalog)))})

(defn parse-profile
  [text catalog]
  (let [schema (get-in (document/schema-document "gateway") ["$defs" "settings_profile"])]
    (when (> (alength (.getBytes ^String text "UTF-8")) (long (get schema "x-vis-max-bytes")))
      (throw (ex-info "Settings profile is too large" {})))
    (let [profile (wire/parse-json text)
          items (get profile "changes")
          by-id (into {} (map (juxt #(get % "id") identity)) (rows catalog))]

      (when-not (and (document/valid-json? "gateway" "settings_profile" profile)
                     (= (count items) (count (distinct (map #(get % "id") items)))))
        (throw (ex-info "Use a version 1 settings profile with distinct setting ids" {})))
      (doseq [{:strs [id action value]} items]
        (let [row (get by-id id)
              type (get row "type")]

          (when-not (and row
                         (transferable? row)
                         (or (= action "inherit")
                             (case type
                               "boolean"
                               (boolean? value)

                               "enum"
                               (some #{value} (get row "choices"))

                               "number"
                               (number? value)

                               "string"
                               (string? value)

                               "array"
                               (vector? value)

                               "object"
                               (map? value)

                               false)))
            (throw (ex-info (str "Unsupported profile setting: " id) {:id id})))))
      profile)))
