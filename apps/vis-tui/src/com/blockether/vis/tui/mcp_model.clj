(ns com.blockether.vis.tui.mcp-model
  "Pure reading of one sanitized MCP inventory row.

   Lives apart from `mcp` so BOTH the Settings dialog (`dialogs`, which `mcp`
   requires) and the MCP manager can describe a server without a dependency
   cycle. Nothing here talks to the gateway."
  (:require [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)

(defn flag
  "Wire rows are string-keyed JSON; a missing flag is false, never nil."
  [row k]
  (boolean (get row k)))

(defn server-on?
  "True when the server is BOTH enabled in config and not killed at runtime —
   the single on/off a settings toggle can show."
  [row]
  (and (flag row "enabled") (not (flag row "is_killed"))))

(defn oauth-applicable?
  "An HTTP server with its own Authorization header does not use OAuth."
  [row]
  (and (some? (get row "url"))
       (not-any? #(= "authorization" (str/lower-case %)) (get row "header_names"))))

(defn needs-oauth?
  "Use the challenge verdict, or the old guess if the gateway omitted it."
  [row]
  (and (oauth-applicable? row)
       (not (flag row "is_authorized"))
       (get row "needs_auth" true)))

(defn server-status
  "Human one-liner for one sanitized inventory row.

   Reads the RUNTIME state first — a killed server is killed even though it is
   still enabled in config, which is exactly the distinction the kill verb
   introduces and the one a user cannot otherwise see."
  [row]
  (let [tools (long (or (get row "tools") 0))]
    (str/join " · "
              (cond-> [(cond (flag row "is_killed") "killed"
                             (not (flag row "enabled")) "disabled"
                             (flag row "is_connected") "connected"
                             (= "unhealthy" (get row "status")) "unhealthy"
                             (= "connecting" (get row "status")) "connecting"
                             :else "idle")]
                (pos? tools)
                (conj (str tools (if (= 1 tools) " tool" " tools")))

                (needs-oauth? row)
                (conj "needs sign-in")

                (and (oauth-applicable? row) (flag row "is_authorized"))
                (conj "signed in")

                (not (flag row "is_managed"))
                (conj "config file")))))

(defn server-actions
  "The verbs offered for one row, in the order they matter.

   Kill/start are machine-wide, including for hand-written servers. From a
   scoped view, enable/disable/edit affect only that scope, and removal is
   available only for an override. OAuth works on any HTTP definition that
   does not send its own Authorization header.

   Every verb carries its own transient key and group, so the letter a user learns
   belongs to the verb and not to whatever widget happens to paint it."
  [row]
  (let [local-view?
        (or (flag row "is_scoped") (= "global" (get row "source")))

        oauth?
        (oauth-applicable? row)

        managed?
        (flag row "is_managed")

        authorized?
        (flag row "is_authorized")]

    (cond-> []
      (and (not local-view?) (flag row "is_killed"))
      (conj {:id :start :key "s" :group :runtime :label "Start"})

      (and (not local-view?) (not (flag row "is_killed")))
      (conj {:id :kill :key "k" :group :runtime :label "Kill"})

      (and managed? (flag row "enabled"))
      (conj {:id :disable :key "d" :group :config :label "Disable"})

      (and managed? (not (flag row "enabled")))
      (conj {:id :enable :key "e" :group :config :label "Enable"})

      managed?
      (conj {:id :edit :key "c" :group :config :label "Edit…"})

      (and managed? (or (not local-view?) (flag row "is_override")))
      (conj {:id :remove :key "x" :group :config :label (if local-view? "Use inherited" "Remove")})

      oauth?
      (conj {:id :auth :key "a" :group :account :label (cond authorized? "Sign in again"
                                                              (needs-oauth? row) "Sign in"
                                                              :else "Sign in with OAuth…")})

      (and oauth? authorized?)
      (conj {:id :logout :key "o" :group :account :label "Sign out"})

      :always
      (conj {:id :details :key "v" :group :inspect :label "Details"}))))

(def ^:private transient-groups
  "Group order and headings for `server-transient-spec`; a group with no
   applicable verb is dropped rather than painted empty."
  [[:runtime "Runtime"] [:config "Configuration"] [:account "Account"] [:inspect "Inspect"]])

(defn server-transient-spec
  "The transient for ONE server: the verbs of `server-actions`, grouped by
   what they touch — runtime state, persisted configuration, OAuth, inspection.

   Pure, so the band a user sees is unit-testable without a screen."
  [row]
  (let [actions (server-actions row)]
    {:title (str (get row "name") " · " (server-status row))
     :groups (into []
                   (keep (fn [[group heading]]
                           (let [items (into []
                                             (comp (filter #(= group (:group %)))
                                                   (map #(-> (select-keys % [:key :id :label])
                                                             (assoc :type :action))))
                                             actions)]
                             (when (seq items) {:title heading :items items}))))
                   transient-groups)}))
