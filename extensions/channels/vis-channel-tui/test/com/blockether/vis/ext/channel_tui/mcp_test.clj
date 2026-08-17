(ns com.blockether.vis.ext.channel-tui.mcp-test
  "The MCP manager's row text and offered verbs are pure, so they are asserted
   here without a terminal: what a user is allowed to do to a server depends on
   whether the gateway manages it, whether it is killed, and whether it is an
   HTTP server that still needs a browser sign-in.

   The verbs themselves are asserted through a `dlg/band-questions` stub: what a
   verb ASKS, and that it asks it in the caller's band, is the contract - opening
   a dialog from a transient is the regression these guard."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.mcp :as mcp]
            [com.blockether.vis.ext.channel-tui.mcp-model :as mcp-model]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private managed-stdio
  {"name" "files"
   "transport" "stdio"
   "command" "npx files"
   "enabled" true
   "is_connected" true
   "is_managed" true
   "is_killed" false
   "tools" 7})

(def ^:private http-server
  {"name" "notion"
   "transport" "http"
   "url" "https://mcp.example.com/sse"
   "enabled" true
   "is_connected" false
   "is_managed" true
   "is_killed" false
   "is_authorized" false
   "tools" 0})

(def ^:private handwritten
  {"name" "repo"
   "transport" "stdio"
   "command" "repo-mcp"
   "enabled" true
   "is_connected" true
   "is_managed" false
   "is_killed" false
   "tools" 2})

(defdescribe
  mcp-status-line-test
  (it "reports runtime state before config state, so a killed server reads killed"
      (expect (= "connected · 7 tools" (mcp/server-status managed-stdio)))
      (expect (= "killed · 7 tools" (mcp/server-status (assoc managed-stdio "is_killed" true))))
      ;; enabled in config, killed at runtime: the distinction kill introduces.
      (expect (= "disabled"
                 (mcp/server-status (assoc managed-stdio
                                      "enabled" false
                                      "is_connected" false
                                      "tools" 0))))
      (expect (= "idle" (mcp/server-status {"name" "x" "enabled" true "is_managed" true}))))
  (it "flags an HTTP server that has no gateway token yet"
      (expect (= "idle · needs sign-in" (mcp/server-status http-server)))
      (expect (= "idle · signed in" (mcp/server-status (assoc http-server "is_authorized" true)))))
  (it "marks hand-written servers so the read-only verbs are not a surprise"
      (expect (= "connected \u00b7 2 tools \u00b7 config file" (mcp/server-status handwritten)))))

(defdescribe
  mcp-actions-test
  (it "offers kill on a live server and start on a killed one, never both"
      (expect (= [:kill :disable :edit :remove :details]
                 (mapv :id (mcp-model/server-actions managed-stdio))))
      (expect (= [:start :disable :edit :remove :details]
                 (mapv :id (mcp-model/server-actions (assoc managed-stdio "is_killed" true))))))
  (it "offers kill/start for hand-written servers but no persisting verb"
      ;; Kill is runtime-only, so it works on a server the gateway must not rewrite.
      (expect (= [:kill :details] (mapv :id (mcp-model/server-actions handwritten))))
      (expect (= [:start :details]
                 (mapv :id (mcp-model/server-actions (assoc handwritten "is_killed" true))))))
  (it "offers enable instead of disable when a managed server is off"
      (expect (= [:kill :enable :edit :remove :details]
                 (mapv :id (mcp-model/server-actions (assoc managed-stdio "enabled" false))))))
  (it "offers OAuth only for HTTP servers, and sign-out only once signed in"
      (expect (= [:kill :disable :edit :remove :auth :details]
                 (mapv :id (mcp-model/server-actions http-server))))
      (expect (= "Sign in"
                 (->> (mcp-model/server-actions http-server)
                      (filter #(= :auth (:id %)))
                      first
                      :label)))
      (let [signed (mcp-model/server-actions (assoc http-server "is_authorized" true))]
        (expect (= [:kill :disable :edit :remove :auth :logout :details] (mapv :id signed)))
        (expect (= "Re-authorize"
                   (->> signed
                        (filter #(= :auth (:id %)))
                        first
                        :label))))
      (expect (empty? (filter (comp #{:auth :logout} :id)
                              (mcp-model/server-actions managed-stdio)))))
  (it "binds ONE magit key per verb, and never the same key twice on a row"
      (let [ks (mapv :key (mcp-model/server-actions (assoc http-server "is_authorized" true)))]
        (expect (= ["k" "d" "c" "x" "a" "o" "v"] ks))
        (expect (= (count ks) (count (set ks))))))
  (it "groups the verbs by what they touch, and drops a group with nothing in it"
      (let [spec (mcp-model/server-transient-spec (assoc http-server "is_authorized" true))]
        ;; the popup names the server AND its live state, so the band is self-describing
        (expect (= "notion \u00b7 idle \u00b7 signed in" (:title spec)))
        (expect (= ["Runtime" "Configuration" "Account" "Inspect"] (mapv :title (:groups spec))))
        (expect (= [{:key "a" :type :action :id :auth :label "Re-authorize"}
                    {:key "o" :type :action :id :logout :label "Sign out"}]
                   (:items (nth (:groups spec) 2))))
        ;; nothing about a hand-written server is persisted here: no Configuration
        (expect (= ["Runtime" "Inspect"]
                   (mapv :title (:groups (mcp-model/server-transient-spec handwritten)))))))
  (it "shows the managed tier and the runtime flags in the details view"
      (let [lines (mcp/server-details handwritten)]
        (expect (some #(= "name       repo" %) lines))
        (expect (some #(= "killed     false" %) lines))
        (expect (some #(= "managed    config file (read-only)" %) lines))
        (expect (not-any? #(str/starts-with? % "url ") lines)))
      (expect (some #(= "managed    gateway (editable here)" %) (mcp/server-details http-server)))))

(defdescribe mcp-palette-command-test
             (it "is reachable from the command palette, in any session"
                 (let
                   [cmd (->> dlg/palette-commands
                             (filter #(= :mcp (:id %)))
                             first)]
                   (expect (some? cmd))
                   (expect (= "MCP Servers" (:label cmd)))
                   ;; Untagged, so it survives the turnless gating too.
                   (expect (some #(= :mcp (:id %))
                                 (dlg/palette-commands-for {:has-turns? false}))))))

(defdescribe
  mcp-form-test
  (it "keeps a quoted argument whole when splitting a typed command line"
      (expect (= ["npx" "-y" "@modelcontextprotocol/server-filesystem" "/srv/data"]
                 (mcp/tokenize-command "npx -y @modelcontextprotocol/server-filesystem /srv/data")))
      (expect (= ["cmd" "a b" "c d"] (mcp/tokenize-command "cmd \"a b\" 'c d'")))
      (expect (= [] (mcp/tokenize-command "   ")))
      (expect (= [] (mcp/tokenize-command nil))))
  (it "reads KEY=value entries, splitting on the first = only"
      (expect (= {"A" "1" "B" "2"} (mcp/parse-kv "A=1, B=2")))
      (expect (= {"URL" "a=b"} (mcp/parse-kv "URL=a=b")))
      ;; Blank means "keep what the gateway stores", so it must not become {}.
      (expect (nil? (mcp/parse-kv "  ")))
      (expect (nil? (mcp/parse-kv nil))))
  (it "sends only the keys the chosen transport owns"
      (expect (= {"transport" "stdio"
                  "enabled" true
                  "command" "npx"
                  "args" ["-y" "files"]
                  "cwd" "/srv"
                  "env" {"A" "1"}
                  "timeout_ms" 5000}
                 (mcp/form->spec {:transport "stdio"
                                  :command-line "npx -y files"
                                  :cwd " /srv "
                                  :env "A=1"
                                  :timeout-ms "5000"
                                  :is-enabled true})))
      ;; A leftover stdio field from the prefilled form must not reach an HTTP save.
      (expect (= {"transport" "streamable_http"
                  "enabled" false
                  "url" "https://mcp.example.com/mcp"
                  "headers" {"Authorization" "Bearer t"}}
                 (mcp/form->spec {:transport "streamable_http"
                                  :url " https://mcp.example.com/mcp "
                                  :headers "Authorization=Bearer t"
                                  :command-line "npx -y files"
                                  :cwd "/srv"
                                  :is-enabled false}))))
  (it "drops blank optionals so an edit preserves the gateway's stored secret"
      (expect (= {"transport" "streamable_http" "enabled" true "url" "https://mcp.example.com/mcp"}
                 (mcp/form->spec {:transport "streamable_http"
                                  :url "https://mcp.example.com/mcp"
                                  :headers ""
                                  :timeout-ms ""
                                  :is-enabled true}))))
  (it "round-trips an existing row through the edit form"
      (let
        [row
         (assoc managed-stdio
           "command" "npx"
           "args" ["-y" "files"]
           "cwd" "/srv"
           "env" {"A" "1"}
           "timeout_ms" 5000)

         form
         (mcp/row->form row)]

        (expect (= "npx -y files" (:command-line form)))
        (expect (= "A=1" (:env form)))
        (expect (= "stdio" (:transport form)))
        (expect (true? (:is-enabled form)))
        (expect (= {"transport" "stdio"
                    "enabled" true
                    "command" "npx"
                    "args" ["-y" "files"]
                    "cwd" "/srv"
                    "env" {"A" "1"}
                    "timeout_ms" 5000}
                   (mcp/form->spec form)))
        (expect (false? (:is-enabled (mcp/row->form (assoc row "enabled" false)))))
        ;; An add starts empty but still defaults to enabled.
        (expect (true? (:is-enabled (mcp/row->form nil))))))
  (it "catches the mistakes that would otherwise come back as a gateway 400"
      (expect (= "A server name is required."
                 (mcp/spec-problem "  " {"transport" "stdio" "command" "x"})))
      (expect (= "A stdio server needs a command to run."
                 (mcp/spec-problem "files" {"transport" "stdio"})))
      (expect (= "An HTTP server needs a URL."
                 (mcp/spec-problem "notion" {"transport" "streamable_http"})))
      (expect (nil? (mcp/spec-problem "files"
                                      (mcp/form->spec {:transport "stdio"
                                                       :command-line "npx files"}))))
      (expect (nil? (mcp/spec-problem "notion"
                                      (mcp/form->spec {:transport "streamable_http"
                                                       :url "https://mcp.example.com/mcp"})))))
  (it "counts tools whether the test verb answers with a number or a list"
      (expect (= 2 (mcp/tool-count {"tools" 2})))
      (expect (= 2 (mcp/tool-count {"tools" [{"name" "a"} {"name" "b"}]})))
      (expect (= 0 (mcp/tool-count {})))))

(defn- band-stub
  "A `dlg/band-questions` stand-in. `answers` maps a question's label to what the
   user answers (a missing label IS an Esc); every question asked lands in `log`."
  [answers log]
  (fn [& _]
    {:read! (fn [label opts]
              (swap! log conj [:read label opts])
              (get answers label))
     :choose! (fn [title choices]
                (swap! log conj [:choose title choices])
                (get answers title))
     :confirm! (fn [question opts]
                 (swap! log conj [:confirm question opts])
                 (get answers question))
     :note! (fn [title line]
              (swap! log conj [:note title line])
              nil)}))

(defdescribe
  mcp-band-verbs-test
  ;; Regression (user report, Settings -> MCP Servers): adding a server opened up
  ;; to nine DIALOGS stacked on the settings list (transport, then one per field,
  ;; then the confirm), and removing one, signing in and every gateway refusal
  ;; opened their own windows too - while the companion app asks all of it inline.
  ;; The verbs are a transient, so their questions belong in the SAME band.
  (it
    "asks the transport, every field and the save confirm in the caller's own band"
    (let
      [log
       (atom [])

       saved
       (atom nil)

       dialed
       (atom nil)]

      (with-redefs
        [dlg/host-band-region
         (fn [_screen region]
           region)

         dlg/band-questions
         (band-stub {"MCP · add server" "stdio"
                     "Name:" " files "
                     "Command:" "npx -y files"
                     "Working directory (blank for the default):" "/srv"
                     "Environment, KEY=value, comma separated:" "A=1"
                     "Timeout in ms (blank for the default):" ""
                     "Add MCP server `files`?" true}
                    log)

         vis/gateway-mcp-test-server!
         (fn [server spec]
           (reset! dialed [server spec])
           {"is_connected" true "tools" 3})

         vis/gateway-mcp-save-server!
         (fn [server spec]
           (reset! saved [server spec]))

         ;; A verb reached from a transient must never answer with a window.
         dlg/select-dialog!
         (fn [& _]
           (throw (ex-info "the transport opened a dialog" {})))

         dlg/text-input-dialog!
         (fn [& _]
           (throw (ex-info "a field opened a dialog" {})))

         dlg/confirm-dialog!
         (fn [& _]
           (throw (ex-info "the save confirm opened a dialog" {})))

         dlg/text-view-dialog!
         (fn [& _]
           (throw (ex-info "the add opened a dialog" {})))]

        (mcp/save-server! nil nil nil nil)
        (expect (= ["files"
                    {"transport" "stdio"
                     "enabled" true
                     "command" "npx"
                     "args" ["-y" "files"]
                     "cwd" "/srv"
                     "env" {"A" "1"}}]
                   @saved))
        ;; Dialed by the GATEWAY before anything is persisted.
        (expect (= @dialed @saved))
        (expect (= ["MCP · add server" "Name:" "Command:"
                    "Working directory (blank for the default):"
                    "Environment, KEY=value, comma separated:"
                    "Timeout in ms (blank for the default):" "Add MCP server `files`?"]
                   (mapv second @log)))
        ;; The one useful line of the old four-line dialog body survives as the
        ;; field's own placeholder, where it is read while typing.
        (expect (str/includes? (str @log) "npx -y @modelcontextprotocol/server-filesystem"))
        ;; Saying yes has a cost, and the band says what it is.
        (let [[_ _ opts] (last @log)]
          (expect (= "Connected · 3 tools" (:cost opts)))
          (expect (= "Yes, add it" (:yes-label opts)))))))
  (it "abandons the whole form when one field is escaped, and saves nothing"
      (let
        [log
         (atom [])

         saved?
         (atom false)]

        (with-redefs
          [dlg/host-band-region
           (fn [_screen region]
             region)

           dlg/band-questions
           (band-stub {"MCP · add server" "stdio" "Name:" "files"} log)

           vis/gateway-mcp-test-server!
           (fn [& _]
             (throw (ex-info "dialed an abandoned form" {})))

           vis/gateway-mcp-save-server!
           (fn [& _]
             (reset! saved? true))]

          (mcp/save-server! nil nil nil nil)
          (expect (= false @saved?))
          (expect (= ["MCP · add server" "Name:" "Command:"] (mapv second @log))))))
  (it "removes a server through the same band, saying what removal costs"
      (let
        [log
         (atom [])

         removed
         (atom nil)]

        (with-redefs
          [dlg/host-band-region
           (fn [_screen region]
             region)

           dlg/band-questions
           (band-stub {"Remove MCP server `files`?" true} log)

           vis/gateway-mcp-delete-server!
           (fn [server]
             (reset! removed server))

           dlg/confirm-dialog!
           (fn [& _]
             (throw (ex-info "removal opened a dialog" {})))]

          (mcp/run-action! nil nil nil managed-stdio :remove)
          (expect (= "files" @removed))
          (let [[_ question opts] (first @log)]
            (expect (str/includes? question "Remove MCP server"))
            (expect (str/includes? (:cost opts) "gateway's own configuration"))
            (expect (= "Yes, remove" (:yes-label opts)))))))
  (it "keeps the server when the removal confirm is declined"
      (let [removed? (atom false)]
        (with-redefs
          [dlg/host-band-region (fn [_screen region]
                                  region)
           dlg/band-questions (band-stub {} (atom []))
           vis/gateway-mcp-delete-server! (fn [& _]
                                            (reset! removed? true))]

          (mcp/run-action! nil nil nil managed-stdio :remove)
          (expect (= false @removed?)))))
  (it "reports a gateway rejection in the SAME band instead of a window"
      (let [log (atom [])]
        (with-redefs
          [dlg/host-band-region (fn [_screen region]
                                  region)
           dlg/band-questions (band-stub {} log)
           vis/gateway-mcp-kill-server! (fn [_]
                                          (throw (ex-info "mcp server is not gateway-managed: 409"
                                                          {})))
           dlg/text-view-dialog! (fn [& _]
                                   (throw (ex-info "a refusal opened a dialog" {})))]

          (mcp/run-action! nil nil nil handwritten :kill)
          (expect (= :note (ffirst @log)))
          (expect (str/includes? (str @log) "not gateway-managed")))))
  (it "still shows the read-only details as a viewer, not as a menu of keys"
      (let [viewed (atom nil)]
        (with-redefs
          [dlg/host-band-region (fn [_screen region]
                                  region)
           dlg/band-questions (band-stub {} (atom []))
           dlg/text-view-dialog! (fn [_screen title lines]
                                   (reset! viewed [title lines]))]

          (mcp/run-action! nil nil nil managed-stdio :details)
          (expect (= "MCP · files" (first @viewed)))
          (expect (some #(str/starts-with? % "tools") (second @viewed)))))))
