(ns com.blockether.vis.ext.channel-tui.mcp
  "TUI MCP server manager — inventory, kill/start, enable/disable, remove, and
   the browser sign-in leg for HTTP servers.

   NOTHING about MCP lives in this process. The GATEWAY owns the configuration,
   the connection pool, the child processes and the OAuth tokens; this namespace
   only renders that inventory and posts verbs to
   `/v1/mcp/servers/:name/{actions,auth}/…`. So a TUI attached to a REMOTE
   gateway manages and authorizes servers exactly like the phone app does, and
   the terminal never holds a token, a PKCE verifier, or a child process.

   `server-status`, `server-rows` and `server-actions` are pure so the row text
   and the offered verbs are unit-testable without a screen."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.internal.external-opener :as opener])
  (:import [com.googlecode.lanterna.screen TerminalScreen]))

(set! *unchecked-math* :warn-on-boxed)

(def ^:private title "MCP Servers")

(defn- flag
  "Wire rows are string-keyed JSON; a missing flag is false, never nil."
  [row k]
  (boolean (get row k)))

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

(defn server-rows
  "Selection rows for the inventory, names column-aligned so the status reads
   down the list."
  [servers]
  (let [width (reduce max 0 (map #(count (str (get % "name"))) servers))]
    (mapv (fn [row]
            (let
              [nm (str (get row "name"))
               pad (apply str (repeat (- width (count nm)) \space))]

              {:label (str nm pad "   " (server-status row)) :server row}))
          servers)))

(defn server-actions
  "The verbs offered for one row, in the order they matter.

   Kill/start are RUNTIME and always available (they work on hand-written
   servers too); enable/disable/remove persist and therefore only appear for
   gateway-managed ones; the OAuth verbs only for HTTP servers."
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
      (conj {:id :start :label "Start — release the kill"})

      (not (flag row "is_killed"))
      (conj {:id :kill :label "Kill — stop the server now"})

      (and managed? (flag row "enabled"))
      (conj {:id :disable :label "Disable — persist off"})

      (and managed? (not (flag row "enabled")))
      (conj {:id :enable :label "Enable — persist on"})

      http?
      (conj {:id :auth
             :label (if authorized? "Re-authorize — browser sign-in" "Sign in — browser OAuth")})

      (and http? authorized?)
      (conj {:id :logout :label "Sign out — forget tokens"})

      managed?
      (conj {:id :remove :label "Remove server"})

      :always
      (conj {:id :details :label "Details"}))))

(defn server-details
  "Read-only detail lines for one row."
  [row]
  (into [(str "name       " (get row "name")) (str "transport  " (get row "transport"))]
        (remove nil?)
        [(when-let [c (get row "command")]
           (str "command    " c))
         (when-let [c (get row "cwd")]
           (str "cwd        " c))
         (when-let [u (get row "url")]
           (str "url        " u)) (str "state      " (server-status row))
         (str "enabled    " (flag row "enabled")) (str "connected  " (flag row "is_connected"))
         (str "killed     " (flag row "is_killed")) (str "tools      " (or (get row "tools") 0))
         (str "managed    "
              (if (flag row "is_managed") "gateway (editable here)" "config file (read-only)"))]))

(defn- authorize!
  "Run one browser OAuth flow for an HTTP MCP server THROUGH THE GATEWAY.

   `auth/start` mints the flow daemon-side and hands back only the authorization
   URL and an opaque flow id — the PKCE verifier never reaches this process. When
   the gateway runs on this machine its loopback listener finishes the flow by
   itself, so an empty paste is answered by a poll; otherwise the user pastes the
   final redirect URL back and the daemon exchanges it."
  [^TerminalScreen screen server]
  (let
    [flow
     (vis/gateway-mcp-auth-start! server)

     flow-id
     (get flow "flow_id")

     url
     (get flow "url")]

    (if-not (and flow-id url)
      (dlg/text-view-dialog! screen title ["No authorization URL came back from vis."])
      (do
        (opener/open! url)
        (let
          [pasted
           (dlg/text-input-dialog!
             screen
             title
             "Paste the final browser URL or authorization code:"
             :body
             [(str "Signing in to `" server "`.") "" "A browser was opened. Finish the login there."
              "" "If the browser can reach this gateway the flow completes on its"
              "own — just press Enter on an empty field to check. Otherwise copy"
              "the URL the browser ended on and paste it here." "" url])

           input
           (some-> pasted
                   str/trim)

           verdict
           (if (str/blank? input)
             (vis/gateway-mcp-auth-poll! server flow-id)
             (vis/gateway-mcp-auth-complete! server flow-id input))

           status
           (get verdict "status")]

          (cond
            ;; Success is silent: parity with the provider dialogs.
            (= "ok" status) (do (vis/gateway-mcp-start-server! server) nil)
            (= "pending" status) (do (vis/gateway-mcp-auth-cancel! server flow-id)
                                     (dlg/text-view-dialog!
                                       screen
                                       title
                                       ["Sign-in was not finished — the flow was cancelled."]))
            :else (dlg/text-view-dialog!
                    screen
                    title
                    [(str "Auth failed: " (or (get verdict "error") "authorization failed"))])))))))

(defn- run-action!
  "Execute one palette verb against one server. Every gateway rejection (409 for
   a hand-written server, 404 for an unknown one) surfaces as a dialog instead of
   a crashed TUI."
  [^TerminalScreen screen row action]
  (let [server (str (get row "name"))]
    (try
      (case action
        :kill
        (vis/gateway-mcp-kill-server! server)

        :start
        (vis/gateway-mcp-start-server! server)

        :enable
        (vis/gateway-mcp-set-server-enabled! server true)

        :disable
        (vis/gateway-mcp-set-server-enabled! server false)

        :auth
        (authorize! screen server)

        :logout
        (vis/gateway-mcp-auth-logout! server)

        :remove
        (when (dlg/confirm-dialog! screen
                                   title
                                   [(str "Remove MCP server `" server "`?") ""
                                    "This deletes it from the gateway's own configuration."])
          (vis/gateway-mcp-delete-server! server))

        :details
        (dlg/text-view-dialog! screen (str "MCP · " server) (server-details row))

        nil)
      nil
      (catch Exception e (dlg/text-view-dialog! screen title [(str "MCP: " (ex-message e))]) nil))))

(defn show-mcp-dialog!
  "Open the MCP manager. Loops on the LIVE gateway inventory so every verb's
   effect — a kill, a sign-in, a removal — is visible on the next pass.
   Returns nil when the user closes it."
  [^TerminalScreen screen]
  (loop []

    (let
      [servers (try (vec (vis/gateway-mcp-servers))
                    (catch Exception e
                      (dlg/text-view-dialog! screen title [(str "MCP: " (ex-message e))])
                      nil))]
      (cond (nil? servers) nil
            (empty? servers) (dlg/text-view-dialog!
                               screen
                               title
                               ["No MCP servers configured." ""
                                "Add one from the Companion app's Settings, or declare it"
                                "under `mcp:` in vis.yml."])
            :else (when-let [pick (dlg/select-dialog! screen title (server-rows servers))]
                    (let [row (:server pick)]
                      (when-let
                        [action (dlg/select-dialog! screen
                                                    (str "MCP · " (get row "name"))
                                                    (server-actions row))]
                        (run-action! screen row (:id action)))
                      (recur)))))))
