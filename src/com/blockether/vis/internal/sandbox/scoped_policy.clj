(ns com.blockether.vis.internal.sandbox.scoped-policy
  "Scoped access settings. Local policy can narrow, never expand, the host ceiling."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.config.validation :as validation]
            [com.blockether.vis.internal.persistance.core :as store]
            [com.blockether.vis.internal.sandbox.policy :as policy]
            [com.blockether.vis.internal.workspace.core :as workspace]))

(def fields
  [{:id "workspace_filesystem"
    :section ["workspace"]
    :key "filesystem"
    :label "Workspace paths"
    :default []
    :type "array"
    :editor "paths"}
   {:id "jail_enabled"
    :section ["jail"]
    :key "enabled"
    :label "Process jail"
    :default false
    :type "boolean"
    :editor "switch"}
   {:id "jail_filesystem"
    :section ["jail"]
    :key "filesystem"
    :label "Filesystem access"
    :default {}
    :type "object"
    :editor "filesystem"}
   {:id "jail_network"
    :section ["jail"]
    :key "network"
    :label "Network access"
    :default {}
    :type "object"
    :editor "network"}
   {:id "jail_deny_exec"
    :section ["jail"]
    :key "deny_exec"
    :label "Denied executables"
    :default []
    :type "array"
    :editor "list"}
   {:id "jail_keychain"
    :section ["jail"]
    :key "keychain"
    :label "Keychain access"
    :default false
    :type "boolean"
    :editor "switch"}
   {:id "tools_allow"
    :section ["tools"]
    :key "allow"
    :label "Allowed tools"
    :default []
    :type "array"
    :editor "tools"
    :applies "next_call"
    :description
    "Only these tools of each extension in the list stay on. Lists of wider scopes also apply."}
   {:id "tools_deny"
    :section ["tools"]
    :key "deny"
    :label "Turned-off tools"
    :default []
    :type "array"
    :editor "tools"
    :applies "next_call"
    :description "These tools are off. Lists of wider scopes also apply."}])

(def ^:private sections
  "The configuration sections that these settings write."
  [["workspace"] ["jail"] ["tools"]])

(defn setting? [id] (boolean (some #(= id (:id %)) fields)))

(defonce ^:private tool-names-fn (atom (constantly nil)))

(defn set-tool-names-fn!
  "Install `f`, which takes a workspace root and answers the tool names that a tool list
   can name there, or nil when it cannot tell."
  [f]
  (reset! tool-names-fn f))

(defn tool-names
  "The tool names that a tool list can name in `target`, or nil when they are unknown."
  [target]
  (@tool-names-fn (:root target)))

(defn tool-pattern
  "Compile one entry of a tool list. `*` matches any characters."
  [s]
  (re-pattern (str/join ".*" (map #(java.util.regex.Pattern/quote %) (str/split s #"\*" -1)))))

(defn check-tool-names!
  "Throw a 400 error for an entry of `names` that matches no tool of `target`."
  [target names]
  (when-let [known (tool-names target)]
    (let [matches? (fn [entry]
                     (let [pattern (tool-pattern entry)]
                       (some #(re-matches pattern %) known)))
          unknown (remove matches? names)]

      (when (seq unknown)
        (throw (ex-info (str "Unknown tool: " (str/join ", " unknown) ". Choose a suggested name.")
                        {:status 400 :type :tool/unknown :tools (vec unknown)})))))
  names)

(defn tool-lists
  "The tool lists that apply to `session-id`, from each scope, least specific first, as
   `{:allow names}` and `{:deny names}` maps. A subagent also gets the lists of its parent."
  [db session-id]
  (let [agent
        (store/db-agent-info db session-id)

        lists
        (fn [sid]
          (for [[_ entries]
                (scoped/definition-layers db (scoped/target db "session" sid) ["tools"])

                [k list-key]
                [["allow" :allow] ["deny" :deny]]

                :let [names
                      (get entries k)]
                :when (some? names)]

            {list-key (vec names)}))]

    (vec (concat (some-> (:parent_id agent)
                         lists)
                 (lists session-id)))))

(defn- host-config
  []
  (let [yaml
        (config/load-global-yaml-config-raw)

        machine
        (config/load-global-config-raw)]

    (config/deep-merge-config (or yaml {}) (or machine {}))))

(defn effective-config
  "Resolve access keys independently; unrelated configuration is not session-owned."
  [db target]
  (reduce (fn [raw section]
            (assoc-in raw
              section
              (into {} (map (juxt :name :value)) (scoped/definitions db target section))))
          (host-config)
          sections))

(defn- covered?
  [roots path]
  (some (fn [root]
          (.startsWith (.toPath (java.io.File. ^String path))
                       (.toPath (java.io.File. ^String root))))
        roots))

(defn assert-bounded!
  "Reject expanded grants after canonical path resolution, including symlink aliases."
  [host candidate root]
  (let [opts
        {:base-dir root}

        ceiling
        (policy/snapshot host opts)

        desired
        (policy/snapshot candidate opts)

        h
        (:process-jail ceiling)

        d
        (:process-jail desired)

        hn
        (get-in host ["jail" "network"] {})

        dn
        (get-in candidate ["jail" "network"] {})

        subset?
        (fn [a b]
          (set/subset? (set a) (set b)))

        bounded?
        (and (subset? (:deny-read-rules h) (:deny-read-rules d))
             (subset? (:deny-write-rules h) (:deny-write-rules d))
             (subset? (:deny-exec h) (:deny-exec d))
             ;; A local catalog cannot relax the host's per-root draft restrictions.
             (every? (fn [[path policy]]
                       (= policy (get (:draft-policies desired) path)))
                     (:draft-policies ceiling))
             (or (not (:jail-enabled ceiling))
                 (and (:jail-enabled desired)
                      (every? #(covered? (conj (:allow-read-write h) root) %) (:allow-read-write d))
                      (every? #(covered? (concat (:allow-read h) (:allow-read-write h) [root]) %)
                              (:allow-read d))
                      (or (not (:keychain? d)) (:keychain? h))
                      (subset? (:inbound-ports d) (:inbound-ports h))
                      (or (empty? (get hn "allowed_domains"))
                          (and (seq (get dn "allowed_domains"))
                               (subset? (get dn "allowed_domains") (get hn "allowed_domains"))))
                      (subset? (get hn "denied_domains") (get dn "denied_domains"))
                      (subset? (get dn "exclude_domains") (get hn "exclude_domains"))
                      (or (not (get dn "allow_private")) (true? (get hn "allow_private")))
                      (= (get hn "rules") (get dn "rules")))))]

    (when-not bounded?
      (throw (ex-info
               "Local access settings cannot expand the host policy; change the global policy first"
               {:status 400 :type :settings/host-policy})))
    desired))

(defn snapshot
  "Use the current host ceiling if a later ancestor edit invalidates a local grant. A
   subagent gets its parent's policy, narrowed by its file grant."
  [db session-id]
  (let [agent
        (when session-id (store/db-agent-info db session-id))

        ;; A subagent runs under its parent's policy: its own settings never widen it.
        target
        (scoped/target db "session" (or (:parent_id agent) session-id))

        root
        (or (:root target) (.getCanonicalPath (workspace/cwd)))

        host
        (host-config)

        files
        (:files agent)]

    (policy/restrict (try (assert-bounded! host (effective-config db target) root)
                          (catch clojure.lang.ExceptionInfo e
                            (assoc (policy/snapshot host {:base-dir root})
                              :config-error
                              {"message" (ex-message e)
                               "hint"
                               "Review local access settings; the host policy is in effect."})))
                     files)))

(defn settings
  [db target]
  (let [layers
        (into
          {}
          (map (fn [section]
                 [section
                  (into {} (map (juxt :name identity)) (scoped/definitions db target section))]))
          sections)

        inherited
        (into {}
              (map (fn [section]
                     [section (scoped/inherited-definitions db target section)]))
              sections)]

    (mapv (fn [{:keys [id section key label default type editor choices applies description]}]
            (let [{:keys [value source is-override] :or {value default source "default"}}
                  (get-in layers [section key])

                  parent
                  (get-in inherited [section key] {:value default :source "default"})]

              (cond->
                {:id id
                 :label label
                 :type type
                 :editor editor
                 :schema (str "config.json#/$defs/" (first section) "/properties/" key)
                 :scopes ["global" "project" "group" "session"]
                 :scope (:scope target)
                 :source source
                 :is-override (boolean is-override)
                 :inherited-value (:value parent)
                 :inherited-source (:source parent)
                 :own-value (when is-override value)
                 :applies (or applies "next_turn")
                 :description
                 (or
                   description
                   "Changes apply to the next turn. Local access cannot exceed host permissions.")}
                (= type "boolean")
                (assoc :enabled value)

                (not= type "boolean")
                (assoc :value value)

                choices
                (assoc :choices choices))))
          fields)))

(defn validate-candidate!
  "Validate a complete staged access configuration before any owner is written."
  [target candidate]
  (when-not (validation/valid? candidate)
    (throw (ex-info "Invalid access configuration" {:status 400})))
  (when (not= "global" (:scope target))
    (assert-bounded! (host-config)
                     candidate
                     (or (:root target) (.getCanonicalPath (workspace/cwd)))))
  candidate)

(defn set-setting!
  [db target id action given]
  (let [{:keys [section key]}
        (first (filter #(= id (:id %)) fields))

        value
        (case action
          "inherit"
          nil

          "value"
          given

          (throw (ex-info "Access settings take value or inherit" {:status 400})))

        _
        (when (and (= action "value") (nil? value))
          (throw (ex-info "Use inherit instead of null" {:status 400})))

        _
        (when (and (= ["tools"] section) (sequential? value)) (check-tool-names! target value))

        candidate
        (if (= action "inherit")
          nil
          (assoc-in (effective-config db target) (conj section key) value))]

    (when candidate (validate-candidate! target candidate))
    (scoped/set-definition! db target section key value)
    (first (filter #(= id (:id %)) (settings db target)))))
