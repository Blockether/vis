(ns com.blockether.vis.ext.channel-tui.mcp-model
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

(defn server-status
  "Human one-liner for one sanitized inventory row.

   Reads the RUNTIME state first — a killed server is killed even though it is
   still enabled in config, which is exactly the distinction the kill verb
   introduces and the one a user cannot otherwise see."
  [row]
  (let [tools (long (or (get row "tools") 0))]
    (str/join " · "
              (cond->
                [(cond (flag row "is_killed") "killed"
                       (not (flag row "enabled")) "disabled"
                       (flag row "is_connected") "connected"
                       :else "idle")]
                (pos? tools)
                (conj (str tools (if (= 1 tools) " tool" " tools")))

                (and (get row "url") (not (flag row "is_authorized")))
                (conj "needs sign-in")

                (and (get row "url") (flag row "is_authorized"))
                (conj "signed in")

                (not (flag row "is_managed"))
                (conj "config file")))))

(defn server-actions
  "The verbs offered for one row, in the order they matter.

   Kill/start are RUNTIME and always available (they work on hand-written
   servers too); enable/disable/remove persist and therefore only appear for
   gateway-managed ones; the OAuth verbs only for HTTP servers.

   Every verb carries its own magit key and group, so the letter a user learns
   belongs to the verb and not to whatever widget happens to paint it."
  [row]
  (let
    [http?
     (some? (get row "url"))

     managed?
     (flag row "is_managed")

     authorized?
     (flag row "is_authorized")]

    (cond-> []
      (flag row "is_killed")
      (conj {:id :start :key "s" :group :runtime :label "Start"})

      (not (flag row "is_killed"))
      (conj {:id :kill :key "k" :group :runtime :label "Kill"})

      (and managed? (flag row "enabled"))
      (conj {:id :disable :key "d" :group :config :label "Disable"})

      (and managed? (not (flag row "enabled")))
      (conj {:id :enable :key "e" :group :config :label "Enable"})

      managed?
      (conj {:id :edit :key "c" :group :config :label "Edit…"})

      managed?
      (conj {:id :remove :key "x" :group :config :label "Remove"})

      http?
      (conj {:id :auth :key "a" :group :account :label (if authorized? "Re-authorize" "Sign in")})

      (and http? authorized?)
      (conj {:id :logout :key "o" :group :account :label "Sign out"})

      :always
      (conj {:id :details :key "v" :group :inspect :label "Details"}))))

(def ^:private transient-groups
  "Group order and headings for `server-transient-spec`; a group with no
   applicable verb is dropped rather than painted empty."
  [[:runtime "Runtime"] [:config "Configuration"] [:account "Account"] [:inspect "Inspect"]])

(defn server-transient-spec
  "The magit transient for ONE server: the verbs of `server-actions`, grouped by
   what they touch — runtime state, persisted configuration, OAuth, inspection.

   Pure, so the band a user sees is unit-testable without a screen."
  [row]
  (let [actions (server-actions row)]
    {:title (str (get row "name") " · " (server-status row))
     :groups (into []
                   (keep (fn [[group heading]]
                           (let
                             [items (into []
                                          (comp (filter #(= group (:group %)))
                                                (map #(-> (select-keys % [:key :id :label])
                                                          (assoc :type :action))))
                                          actions)]
                             (when (seq items) {:title heading :items items}))))
                   transient-groups)}))
