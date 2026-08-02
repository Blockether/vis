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
      (expect (= [:kill :disable :remove :details] (mapv :id (mcp/server-actions managed-stdio))))
      (expect (= [:start :disable :remove :details]
                 (mapv :id (mcp/server-actions (assoc managed-stdio "is_killed" true))))))
  (it "offers kill/start for hand-written servers but no persisting verb"
      ;; Kill is runtime-only, so it works on a server the gateway must not rewrite.
      (expect (= [:kill :details] (mapv :id (mcp/server-actions handwritten))))
      (expect (= [:start :details]
                 (mapv :id (mcp/server-actions (assoc handwritten "is_killed" true))))))
  (it "offers enable instead of disable when a managed server is off"
      (expect (= [:kill :enable :remove :details]
                 (mapv :id (mcp/server-actions (assoc managed-stdio "enabled" false))))))
  (it "offers OAuth only for HTTP servers, and sign-out only once signed in"
      (expect (= [:kill :disable :auth :remove :details]
                 (mapv :id (mcp/server-actions http-server))))
      (expect (= "Sign in — browser OAuth"
                 (->> (mcp/server-actions http-server)
                      (filter #(= :auth (:id %)))
                      first
                      :label)))
      (let [signed (mcp/server-actions (assoc http-server "is_authorized" true))]
        (expect (= [:kill :disable :auth :logout :remove :details] (mapv :id signed)))
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
