(ns com.blockether.vis.ext.channel-tui.mcp
  "TUI MCP server manager — inventory, add, edit, kill/start, enable/disable,
   remove, and the browser sign-in leg for HTTP servers.

   NOTHING about MCP lives in this process. The GATEWAY owns the configuration,
   the connection pool, the child processes and the OAuth tokens; this namespace
   only renders that inventory and posts verbs to `/v1/mcp/servers` and
   `/v1/mcp/servers/:name/{actions,auth}/…`. So a TUI attached to a REMOTE
   gateway adds, edits and authorizes servers exactly like the phone app does,
   and the terminal never holds a token, a PKCE verifier, or a child process.

   `server-status`, `tokenize-command`, `parse-kv`, `row->form`, `form->spec`
   and `spec-problem` are pure so the row text and the wire spec are
   unit-testable without a screen; the verbs themselves, and the transient band
   that offers them, are pure in `mcp-model`.

   Every question a verb asks — the transport, each field, the save confirm, a
   gateway refusal — is asked in the BAND the verb was fired from (`dlg/band-questions`),
   so adding, editing, authorizing and removing a server never stacks a window
   over the Settings list. The one dialog left is `:details`: a read-only page is
   a viewer, not a menu of keystrokes."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.mcp-model :as mcp-model]
            [com.blockether.vis.internal.external-opener :as opener])
  (:import [com.googlecode.lanterna.screen TerminalScreen]))

(set! *unchecked-math* :warn-on-boxed)

(def ^:private title "MCP Servers")

;; Row reading and the verb list moved to `mcp-model` so the Settings transient
;; can describe a server and offer its verbs without a require cycle through
;; this namespace; re-exported here so `mcp/flag` and `mcp/server-status` stay
;; the manager's own vocabulary.
(def flag mcp-model/flag)

(def server-status mcp-model/server-status)

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

(defn tokenize-command
  "Split a typed command line into argv, honoring single and double quotes so a
   path or a flag value containing a space survives as ONE argument."
  [line]
  (let [flush (fn [^StringBuilder sb out]
                (if (pos? (.length sb)) (conj out (.toString sb)) out))]
    (loop [cs (seq (str line))
           sb (StringBuilder.)
           ^Character q nil
           out []]

      (if (nil? cs)
        (flush sb out)
        (let [c (char (first cs))
              r (next cs)]

          (cond (and q (= c q)) (recur r sb nil out)
                (some? q) (recur r (.append sb c) q out)
                (or (= c \") (= c \')) (recur r sb c out)
                (Character/isWhitespace c) (recur r (StringBuilder.) nil (flush sb out))
                :else (recur r (.append sb c) q out)))))))

(defn parse-kv
  "`KEY=value` entries → a string-keyed map. Entries are separated by newlines or
   commas, blanks are dropped, and the FIRST `=` splits so a value may itself
   contain one. Returns nil when nothing usable was typed — that is how a blank
   field means \"leave whatever the gateway already stores\", which keeps a
   redacted secret alive across an edit."
  [text]
  (let [pairs (into {}
                    (comp (map str/trim)
                          (remove str/blank?)
                          (keep (fn [entry]
                                  (let [i (str/index-of entry "=")]
                                    (when (and i (pos? (long i)))
                                      [(str/trim (subs entry 0 (long i)))
                                       (str/trim (subs entry (inc (long i))))])))))
                    (str/split (str text) #"[\n,]"))]
    (when (seq pairs) pairs)))

(defn kv->text
  "Inverse of `parse-kv` for prefilling the single-line field."
  [m]
  (str/join ", "
            (map (fn [[k v]]
                   (str k "=" v))
                 (sort-by (comp str key) (or m {})))))

(defn row->form
  "Prefill for the form. `nil` is an add. Values the gateway redacted come back
   blank on purpose: saving a blank field keeps the stored one."
  [row]
  {:transport (or (get row "transport") "stdio")
   :command-line (str/join " "
                           (into (if-let [c (get row "command")]
                                   [(str c)]
                                   [])
                                 (map str (get row "args"))))
   :cwd (str (or (get row "cwd") ""))
   :url (str (or (get row "url") ""))
   :env (kv->text (get row "env"))
   :headers (kv->text (get row "headers"))
   :timeout-ms (str (or (get row "timeout_ms") ""))
   :is-enabled (if (contains? row "enabled") (flag row "enabled") true)})

(defn form->spec
  "The wire spec for the gateway's save and test verbs. Only the keys the chosen
   transport owns are sent and blank optionals are dropped, so an edit that
   leaves the header field empty preserves the secret the daemon holds."
  [{:keys [transport command-line cwd url env headers timeout-ms is-enabled]}]
  (let [http?
        (= "streamable_http" transport)

        argv
        (tokenize-command command-line)

        timeout
        (parse-long (str/trim (str timeout-ms)))]

    (cond-> {"transport" (if http? "streamable_http" "stdio") "enabled" (boolean is-enabled)}
      (and (not http?) (seq argv))
      (assoc "command"
        (first argv) "args"
        (vec (rest argv)))

      (and (not http?) (not (str/blank? (str cwd))))
      (assoc "cwd" (str/trim (str cwd)))

      (and (not http?) (parse-kv env))
      (assoc "env" (parse-kv env))

      (and http? (not (str/blank? (str url))))
      (assoc "url" (str/trim (str url)))

      (and http? (parse-kv headers))
      (assoc "headers" (parse-kv headers))

      (some? timeout)
      (assoc "timeout_ms" timeout))))

(defn spec-problem
  "What must hold before the gateway is called at all, so a typo comes back as a
   sentence here instead of a 400 from the daemon."
  [server spec]
  (let [transport (get spec "transport")]
    (cond (str/blank? (str server)) "A server name is required."
          (and (= "stdio" transport) (str/blank? (str (get spec "command"))))
          "A stdio server needs a command to run."
          (and (= "streamable_http" transport) (str/blank? (str (get spec "url"))))
          "An HTTP server needs a URL."
          :else nil)))

(defn tool-count
  "The test verb reports tools either as a count or as the tool list itself."
  [verdict]
  (let [t (get verdict "tools")]
    (cond (number? t) (long t)
          (coll? t) (long (count t))
          :else 0)))

(defn- band-questions
  "Everything ONE MCP verb may ASK, bound to the frame it was fired from. The
   caller's region is snapshotted ONCE (`dlg/host-band-region`) so a taller
   question hands back the rows it covered when it closes."
  [^TerminalScreen screen g region]
  (dlg/band-questions screen g (dlg/host-band-region screen region)))

(defn- ask-all
  "Run `[key ask-fn]` prompts in order, threading the form through them. ANY Esc
   abandons the whole form by returning nil, so nothing half-typed is saved."
  [form prompts]
  (reduce (fn [acc [k ask!]]
            (if-let [v (ask! acc)]
              (assoc acc k v)
              (reduced nil)))
          form
          prompts))

(defn- field-prompts
  "The fields one transport actually has, as prompts for `ask-all`. Each is ONE
   band question: the label is the band's own title, and the example the old
   dialog spent four lines of prose on rides IN the field as a placeholder — the
   only part of that prose that ever answered the question."
  [read! transport]
  (let [timeout [:timeout-ms
                 (fn [f]
                   (read! "Timeout in ms (blank for the default):"
                          {:initial (str (:timeout-ms f))
                           :placeholder "how long the gateway waits for this server"}))]]
    (if (= "streamable_http" transport)
      [[:url
        (fn [f]
          (read! "URL:"
                 {:initial (str (:url f))
                  :placeholder "https://example.com/mcp — sign in afterwards with `Sign in`"}))]
       [:headers
        (fn [f]
          (read! "Headers, KEY=value, comma separated:"
                 {:initial (str (:headers f))
                  :placeholder "blank keeps what the gateway already stores"}))] timeout]
      [[:command-line
        (fn [f]
          (read! "Command:"
                 {:initial (str (:command-line f))
                  :placeholder "npx -y @modelcontextprotocol/server-filesystem /srv/data"}))]
       [:cwd
        (fn [f]
          (read! "Working directory (blank for the default):"
                 {:initial (str (:cwd f))
                  :placeholder "resolved on the gateway's machine, not on this one"}))]
       [:env
        (fn [f]
          (read! "Environment, KEY=value, comma separated:"
                 {:initial (str (:env f))
                  :placeholder "blank keeps what the gateway already stores"}))] timeout])))

(defn- collect-spec!
  "Ask for every field of one server IN THE CALLER'S BAND. `row` is nil for an add
   and the existing sanitized row for an edit. Returns `[name spec]`, or nil the
   moment the user escapes a prompt."
  [q row]
  (let [form
        (row->form row)

        heading
        (if row (str "MCP · edit " (get row "name")) "MCP · add server")]

    (when-let [transport
               (if row
                 (:transport form)
                 ((:choose! q)
                   heading
                   [{:key \s :id "stdio" :label "stdio — the gateway runs a local command"}
                    {:key \h
                     :id "streamable_http"
                     :label "http — a remote streamable HTTP endpoint"}]))]
      (when-let [server (if row
                          (str (get row "name"))
                          (some-> ((:read! q)
                                    "Name:"
                                    {:placeholder "a short id for this server, e.g. filesystem"})
                                  str/trim))]
        (when-let [filled (ask-all (assoc form :transport transport)
                                   (field-prompts (:read! q) transport))]
          [server (form->spec filled)])))))

(defn save-server!
  "Add or edit ONE gateway-managed server, every question inside the CALLER'S own
   band. The candidate is dialed BY THE GATEWAY before anything is persisted, so a
   wrong command or an unreachable URL is reported while it is still just a form.
   Nothing is written on this machine — the daemon owns the configuration, which
   is why this works unchanged against a remote gateway."
  [^TerminalScreen screen g region row]
  (let [q (band-questions screen g region)]
    (try (when-let [[server spec] (collect-spec! q row)]
           (if-let [problem (spec-problem server spec)]
             ((:note! q) title problem)
             (let [verdict (try (vis/gateway-mcp-test-server! server spec)
                                (catch Exception e {"error" (ex-message e)}))
                   tools (tool-count verdict)
                   summary (if (get verdict "is_connected")
                             (str "Connected · " tools (if (= 1 tools) " tool" " tools"))
                             (str "Could not connect: "
                                  (or (get verdict "error") "the server did not answer")))]

               (when ((:confirm! q)
                       (str (if row "Save changes to `" "Add MCP server `") server "`?")
                       {:cost summary
                        :yes-label (if row "Yes, save it" "Yes, add it")
                        :no-label "Discard it"})
                 (vis/gateway-mcp-save-server! server spec)
                 nil))))
         (catch Exception e ((:note! q) title (str "MCP: " (ex-message e))) nil))))

(defn- authorize!
  "Run one browser OAuth flow for an HTTP MCP server THROUGH THE GATEWAY, in the
   band the verb was fired from.

   `auth/start` mints the flow daemon-side and hands back only the authorization
   URL and an opaque flow id — the PKCE verifier never reaches this process. When
   the gateway runs on this machine its loopback listener finishes the flow by
   itself, so an empty paste is answered by a poll; otherwise the user pastes the
   final redirect URL back and the daemon exchanges it."
  [q server]
  (let [flow
        (vis/gateway-mcp-auth-start! server)

        flow-id
        (get flow "flow_id")

        url
        (get flow "url")]

    (if-not (and flow-id url)
      ((:note! q) title "No authorization URL came back from vis.")
      (do
        (opener/open! url)
        (let [pasted
              ((:read! q)
                (str "Signing in to `" server "` — paste the final browser URL, or Enter to check:")
                {:placeholder url})

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
            ;; Success is silent: parity with the provider bands.
            (= "ok" status) (do (vis/gateway-mcp-start-server! server) nil)
            (= "pending" status)
            (do (vis/gateway-mcp-auth-cancel! server flow-id)
                ((:note! q) title "Sign-in was not finished — the flow was cancelled."))
            :else ((:note! q)
                    title
                    (str "Auth failed: " (or (get verdict "error") "authorization failed")))))))))

(defn run-action!
  "Execute one palette verb against one server IN THE CALLER'S BAND. Every gateway
   rejection (409 for a hand-written server, 404 for an unknown one) comes back as
   a line in that same band, never as a window over the list it was fired from."
  [^TerminalScreen screen g region row action]
  (let [region
        (dlg/host-band-region screen region)

        q
        (band-questions screen g region)

        server
        (str (get row "name"))]

    (try (case action
           :kill
           (vis/gateway-mcp-kill-server! server)

           :start
           (vis/gateway-mcp-start-server! server)

           :enable
           (vis/gateway-mcp-set-server-enabled! server true)

           :disable
           (vis/gateway-mcp-set-server-enabled! server false)

           :auth
           (authorize! q server)

           :logout
           (vis/gateway-mcp-auth-logout! server)

           :edit
           (save-server! screen g region row)

           :remove
           (when ((:confirm! q)
                   (str "Remove MCP server `" server "`?")
                   {:cost "Deletes it from the gateway's own configuration."
                    :yes-label "Yes, remove"
                    :no-label "Keep it"})
             (vis/gateway-mcp-delete-server! server))

           ;; The one thing here that is a VIEW and not a verb: a read-only page of
           ;; detail lines belongs in a viewer, not in a menu of keystrokes.
           :details
           (dlg/text-view-dialog! screen (str "MCP · " server) (server-details row))

           nil)
         nil
         (catch Exception e ((:note! q) title (str "MCP: " (ex-message e))) nil))))
