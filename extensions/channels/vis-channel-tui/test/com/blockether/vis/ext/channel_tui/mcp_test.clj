(ns com.blockether.vis.ext.channel-tui.mcp-test
  "The MCP manager's row text and offered verbs are pure, so they are asserted
   here without a terminal: what a user is allowed to do to a server depends on
   whether the gateway manages it, whether it is killed, and whether it is an
   HTTP server that still needs a browser sign-in."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.mcp :as mcp]
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
      (expect (= "connected · 2 tools · config file" (mcp/server-status handwritten))))
  (it "column-aligns names so the status reads down the list"
      (let [rows (mcp/server-rows [managed-stdio handwritten])]
        (expect (= 2 (count rows)))
        (expect (= [managed-stdio handwritten] (mapv :server rows)))
        (expect (= "files   connected · 7 tools" (:label (first rows))))
        (expect (= "repo    connected · 2 tools · config file" (:label (second rows))))
        (expect (apply = (map #(.indexOf ^String (:label %) "connected") rows))))))

(defdescribe
  mcp-actions-test
  (it "offers kill on a live server and start on a killed one, never both"
      (expect (= [:kill :disable :edit :remove :details]
                 (mapv :id (mcp/server-actions managed-stdio))))
      (expect (= [:start :disable :edit :remove :details]
                 (mapv :id (mcp/server-actions (assoc managed-stdio "is_killed" true))))))
  (it "offers kill/start for hand-written servers but no persisting verb"
      ;; Kill is runtime-only, so it works on a server the gateway must not rewrite.
      (expect (= [:kill :details] (mapv :id (mcp/server-actions handwritten))))
      (expect (= [:start :details]
                 (mapv :id (mcp/server-actions (assoc handwritten "is_killed" true))))))
  (it "offers enable instead of disable when a managed server is off"
      (expect (= [:kill :enable :edit :remove :details]
                 (mapv :id (mcp/server-actions (assoc managed-stdio "enabled" false))))))
  (it "offers OAuth only for HTTP servers, and sign-out only once signed in"
      (expect (= [:kill :disable :auth :edit :remove :details]
                 (mapv :id (mcp/server-actions http-server))))
      (expect (= "Sign in — browser OAuth"
                 (->> (mcp/server-actions http-server)
                      (filter #(= :auth (:id %)))
                      first
                      :label)))
      (let [signed (mcp/server-actions (assoc http-server "is_authorized" true))]
        (expect (= [:kill :disable :auth :logout :edit :remove :details] (mapv :id signed)))
        (expect (= "Re-authorize — browser sign-in"
                   (->> signed
                        (filter #(= :auth (:id %)))
                        first
                        :label))))
      (expect (empty? (filter (comp #{:auth :logout} :id) (mcp/server-actions managed-stdio)))))
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
