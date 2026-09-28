(ns com.blockether.vis.internal.config.scoped-policy
  "Scoped access settings. Local policy can narrow, never expand, the host ceiling."
  (:require [charred.api :as json]
            [clojure.set :as set]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.config.validation :as validation]
            [com.blockether.vis.internal.sandbox.policy :as policy]
            [com.blockether.vis.internal.workspace.core :as workspace]))

(def fields
  [{:id "workspace_filesystem"
    :section ["workspace"]
    :key "filesystem"
    :label "Workspace paths"
    :default []}
   {:id "jail_enabled" :section ["jail"] :key "enabled" :label "Process jail" :default false}
   {:id "jail_filesystem"
    :section ["jail"]
    :key "filesystem"
    :label "Filesystem access"
    :default {}}
   {:id "jail_network" :section ["jail"] :key "network" :label "Network access" :default {}}
   {:id "jail_environment"
    :section ["jail"]
    :key "environment"
    :label "Process environment"
    :default "declared"}
   {:id "jail_deny_exec" :section ["jail"] :key "deny_exec" :label "Denied executables" :default []}
   {:id "jail_keychain" :section ["jail"] :key "keychain" :label "Keychain access" :default false}])

(defn setting? [id] (boolean (some #(= id (:id %)) fields)))

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
          [["workspace"] ["jail"]]))

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
                      (or (not (:inherit-host-env? d)) (:inherit-host-env? h))
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
  "Use the current host ceiling if a later ancestor edit invalidates a local grant."
  [db session-id]
  (let [target
        (scoped/target db "session" session-id)

        root
        (or (:root target) (.getCanonicalPath (workspace/cwd)))

        host
        (host-config)]

    (try (assert-bounded! host (effective-config db target) root)
         (catch clojure.lang.ExceptionInfo e
           (assoc (policy/snapshot host {:base-dir root})
             :config-error {"message" (ex-message e)
                            "hint"
                            "Review local access settings; the host policy is in effect."})))))

(defn settings
  [db target]
  (let [layers (into {}
                     (map (fn [section]
                            [section
                             (into {}
                                   (map (juxt :name identity))
                                   (scoped/definitions db target section))]))
                     [["workspace"] ["jail"]])]
    (mapv
      (fn [{:keys [id section key label default]}]
        (let [{:keys [value source is-override] :or {value default source "default"}}
              (get-in layers [section key])]
          {:id id
           :label label
           :type "string"
           :value (json/write-json-str value)
           :scopes ["global" "project" "group" "session"]
           :scope (:scope target)
           :source source
           :is-override (boolean is-override)
           :multiline true
           :description
           "JSON configuration. Local values stay within host permissions. Applies to the next turn."}))
      fields)))

(defn set-setting!
  [db target id action given]
  (let [{:keys [section key]}
        (first (filter #(= id (:id %)) fields))

        value
        (case action
          "inherit"
          nil

          "value"
          (try (json/read-json given)
               (catch Exception _ (throw (ex-info "Enter valid JSON" {:status 400}))))

          (throw (ex-info "Access settings take value or inherit" {:status 400})))

        _
        (when (and (= action "value") (nil? value))
          (throw (ex-info "Use inherit instead of null" {:status 400})))

        candidate
        (if (= action "inherit")
          nil
          (assoc-in (effective-config db target) (conj section key) value))]

    (when candidate
      (when-not (validation/valid? candidate)
        (throw (ex-info "Invalid access configuration" {:status 400})))
      (when (not= "global" (:scope target))
        (assert-bounded! (host-config)
                         candidate
                         (or (:root target) (.getCanonicalPath (workspace/cwd))))))
    (scoped/set-definition! db target section key value)
    (first (filter #(= id (:id %)) (settings db target)))))
