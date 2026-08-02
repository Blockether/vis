(ns com.blockether.vis.internal.foundation.mcp.core-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.ctx-renderer :as renderer]
            [com.blockether.vis.internal.foundation.mcp.client :as client]
            [com.blockether.vis.internal.foundation.mcp.core :as mcp]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  mcp-native-contract-test
  (it "keeps native/Python alias routing in each compact description"
      (let [symbols (get-in mcp/vis-extension [:ext/engine :ext.engine/symbols])]
        (doseq [s symbols]
          (let [description (:ext.symbol/description s)]
            (expect (str/includes? description "In `python_execution`"))
            (expect (< (count description) 350))))))
  (it "closes the dispatcher schemas while leaving MCP tool args open"
      (let
        [symbols
         (get-in mcp/vis-extension [:ext/engine :ext.engine/symbols])

         call
         (first (filter #(= "mcp__call" (:ext.symbol/name %)) symbols))]

        (doseq [s symbols]
          (expect (false? (get-in s [:ext.symbol/schema :additionalProperties]))))
        (expect (= "object" (get-in call [:ext.symbol/schema :properties "args" :type])))))
  (it "exposes exactly ONE verb — the inventory is ctx's job, connecting is the gateway's"
      (let
        [names (set (map :ext.symbol/name
                         (get-in mcp/vis-extension [:ext/engine :ext.engine/symbols])))]
        (expect (= #{"mcp__call"} names))
        ;; Server names, status and tool names ride in `env.mcp`, so a listing
        ;; verb would only re-fetch what the session object already carries.
        (expect (not (contains? names "mcp__servers")))
        (expect (not (contains? names "mcp__tools")))
        ;; A session must not be able to yank a connection every other session is
        ;; using, nor be expected to establish one the daemon already owes it.
        (expect (not (contains? names "mcp__connect")))
        (expect (not (contains? names "mcp__disconnect"))))))

(defdescribe
  gateway-mcp-management-test
  (it "persists gateway server values while returning only a sanitized inventory row"
      (let
        [saved
         (atom nil)

         reconnect!
         (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'reconcile-async!)]

        (with-redefs-fn {#'config/load-global-config-raw (constantly {})
                         #'config/load-config-raw (constantly {})
                         #'config/save-config! (fn [value source]
                                                 (reset! saved [value source]))
                         reconnect! (constantly nil)}
          (fn []
            (let
              [row (mcp/save-gateway-server! "filesystem"
                                             {"transport" "stdio"
                                              "command" "npx"
                                              "env" {"API_TOKEN" "not-for-the-client"}})]
              (expect (= "filesystem" (get row "name")))
              (expect (= "stdio" (get row "transport")))
              (expect (nil? (get row "env")))
              (expect (nil? (get row "headers")))
              (expect (= :gateway-mcp (second @saved)))
              (expect (= "not-for-the-client"
                         (get-in (first @saved)
                                 ["mcp" "servers" "filesystem" "env" "API_TOKEN"]))))))))
  (it "tests a candidate without persisting it and always closes its connection"
      (let
        [seen
         (atom nil)

         closed
         (atom nil)]

        (with-redefs
          [client/connect
           (fn [name spec]
             (reset! seen [name spec])
             ::connection)

           client/list-tools
           (constantly [{"name" "list_files" "description" "Enumerate files"}])

           client/close
           (fn [conn]
             (reset! closed conn))]

          (let
            [result (mcp/test-gateway-server! "remote"
                                              {"transport" "streamable_http"
                                               "url" "https://mcp.example.test/mcp"
                                               "headers" {"Authorization" "Bearer private"}})]
            (expect (= "remote" (get result "name")))
            (expect (= [{"name" "list_files" "description" "Enumerate files"}]
                       (get result "tools")))
            (expect (nil? (get result "headers")))
            (expect (= ::connection @closed))
            (expect (= :streamable-http (get-in @seen [1 :transport])))
            (expect (= "Bearer private" (get-in @seen [1 :headers "Authorization"])))))))
  (it "keeps persisted env and headers when a save omits them"
      (let
        [store
         (atom {"mcp" {"servers" {"remote" {"transport" "streamable_http"
                                            "url" "https://mcp.example.test/mcp"
                                            "headers" {"Authorization" "Bearer keep-me"}}}}})

         reconnect!
         (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'reconcile-async!)]

        (with-redefs-fn {#'config/load-global-config-raw (fn []
                                                           @store)
                         #'config/load-config-raw (fn []
                                                    @store)
                         #'config/save-config! (fn [value _source]
                                                 (reset! store value))
                         reconnect! (constantly nil)}
          (fn []
            ;; The sanitized inventory a client reads carries no secret, so a save
            ;; that round-trips through the UI omits `headers` entirely.
            (mcp/save-gateway-server! "remote"
                                      {"transport" "streamable_http"
                                       "url" "https://mcp.example.test/mcp"})
            (expect (= "Bearer keep-me"
                       (get-in @store ["mcp" "servers" "remote" "headers" "Authorization"])))
            ;; An explicit value — including an empty map — still replaces.
            (mcp/save-gateway-server!
              "remote"
              {"transport" "streamable_http" "url" "https://mcp.example.test/mcp" "headers" {}})
            (expect (= {} (get-in @store ["mcp" "servers" "remote" "headers"])))))))
  (it
    "refuses to write a server that a hand-written config file declares"
    (let
      [saved
       (atom nil)

       reconnect!
       (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'reconcile-async!)

       merged
       {"mcp" {"servers" {"team" {"transport" "stdio" "command" "echo"}}}}

       thrown
       (fn [f]
         (try (f) ::no-throw (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))]

      (with-redefs-fn {#'config/load-global-config-raw (constantly {})
                       #'config/load-config-raw (constantly merged)
                       #'config/save-config! (fn [value source]
                                               (reset! saved [value source]))
                       reconnect! (constantly nil)}
        (fn []
          ;; `team` lives in a user file that wins on merge: writing it here would
          ;; either be shadowed or fork a stale duplicate into the machine state.
          (expect (= :mcp/not-managed
                     (thrown #(mcp/save-gateway-server! "team"
                                                        {"transport" "stdio" "command" "echo"}))))
          (expect (= :mcp/not-managed (thrown #(mcp/set-gateway-server-enabled! "team" false))))
          (expect (= :mcp/not-managed (thrown #(mcp/delete-gateway-server! "team"))))
          (expect (nil? @saved))
          ;; A client can post anything; the shape check runs before every write.
          (expect (= :mcp/invalid-server
                     (thrown #(mcp/save-gateway-server! "team" (first ["not-an-object"])))))
          (expect (= :mcp/not-found (thrown #(mcp/set-gateway-server-enabled! "absent" false))))
          (expect (= :mcp/not-found (thrown #(mcp/delete-gateway-server! "absent"))))))))
  (it "carries the non-secret rest of a spec so a client can edit it without losing it"
      (let
        [machine {"mcp" {"servers" {"owned" {"transport" "stdio"
                                             "command" "npx"
                                             "args" ["-y" "server-filesystem" "/srv"]
                                             "cwd" "/srv"
                                             "timeout_ms" 45000
                                             "env" {"API_TOKEN" "never-leaves"}}}}}]
        (with-redefs-fn {#'config/load-global-config-raw (constantly machine)
                         #'config/load-config-raw (constantly machine)}
          (fn []
            (let [row (first (get (mcp/gateway-servers) "servers"))]
              ;; The TUI and the app build their edit form from this row: without
              ;; `args` an edit round-trip saves the server back with its
              ;; arguments silently dropped.
              (expect (= ["-y" "server-filesystem" "/srv"] (get row "args")))
              (expect (= 45000 (get row "timeout_ms")))
              (expect (= "/srv" (get row "cwd")))
              ;; Secrets still never cross: a save that omits them keeps them.
              (expect (nil? (get row "env")))
              (expect (nil? (get row "headers"))))))))
  (it "marks the rows this gateway owns and counts tools that are not cached yet"
      (let
        [machine
         {"mcp" {"servers" {"owned" {"transport" "stdio" "command" "npx"}}}}

         merged
         (assoc-in machine ["mcp" "servers" "team"] {"transport" "stdio" "command" "echo"})

         tool-count
         (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'tool-count)]

        (with-redefs-fn {#'config/load-global-config-raw (constantly machine)
                         #'config/load-config-raw (constantly merged)}
          (fn []
            (let [rows (get (mcp/gateway-servers) "servers")]
              (expect (= ["owned" "team"] (mapv #(get % "name") rows)))
              (expect (= [true false] (mapv #(get % "is_managed") rows))))))
        (with-redefs [client/list-tools (constantly [{"name" "a"} {"name" "b"}])]
          ;; A freshly connected server's tool cache is still nil; reading the atom
          ;; alone reported 0 tools for every healthy server.
          (expect (= 2 (tool-count {:name "owned" :tools (atom nil)})))
          (expect (= 1 (tool-count {:name "owned" :tools (atom [{"name" "a"}])})))
          (expect (= 0 (tool-count nil)))))))

(defdescribe
  gateway-mcp-runtime-test
  (it
    "keeps a killed server down across reconciles until it is started again"
    (let
      [store
       (atom {"mcp" {"servers" {"local" {"transport" "stdio" "command" "echo"}}}})

       connects
       (atom [])

       conns
       @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'conns)

       killed
       @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'killed)

       reconcile!
       (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'reconcile!)]

      (with-redefs-fn {#'config/load-global-config-raw (fn []
                                                         @store)
                       #'config/load-config-raw (fn []
                                                  @store)
                       #'config/save-config! (fn [value _source]
                                               (reset! store value))
                       #'client/connect (fn [name _spec]
                                          (swap! connects conj name)
                                          ::connection)
                       #'client/list-tools (constantly [])
                       #'client/close (constantly nil)}
        (fn []
          (try (reset! conns {})
               (reset! killed #{})
               (reconcile!)
               (expect (= ["local"] @connects))
               (let [row (mcp/kill-gateway-server! "local")]
                 (expect (true? (get row "is_killed")))
                 (expect (false? (get row "is_connected"))))
               (expect (empty? @conns))
               ;; The brake has to survive the per-turn reconcile: closing the
               ;; connection alone let the very next turn respawn the stdio child
               ;; the user just killed.
               (reconcile!)
               (expect (= ["local"] @connects))
               (let [row (mcp/start-gateway-server! "local")]
                 (expect (false? (get row "is_killed")))
                 (expect (true? (get row "is_connected"))))
               (expect (= ["local" "local"] @connects))
               ;; A kill is a RUNTIME brake, never an edit to anybody's config.
               (expect (= {"transport" "stdio" "command" "echo"}
                          (get-in @store ["mcp" "servers" "local"])))
               (finally (reset! conns {}) (reset! killed #{})))))))
  (it
    "refuses a tool call on a killed server instead of quietly respawning it"
    (let
      [store
       (atom {"mcp" {"servers" {"local" {"transport" "stdio" "command" "echo"}}}})

       connects
       (atom [])

       conns
       @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'conns)

       killed
       @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'killed)

       call!
       (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'mcp-call-impl)]

      (with-redefs-fn {#'config/load-global-config-raw (fn []
                                                         @store)
                       #'config/load-config-raw (fn []
                                                  @store)
                       #'client/connect (fn [name _spec]
                                          (swap! connects conj name)
                                          ::connection)
                       #'client/list-tools (constantly [])
                       #'client/alive? (constantly true)
                       #'client/close (constantly nil)}
        (fn []
          (try (reset! conns {})
               (reset! killed #{})
               (mcp/kill-gateway-server! "local")
               ;; A TOOL CALL is a connect path of its own. With the brake checked
               ;; only in `reconcile!`, this single line respawned the stdio child
               ;; the user had just stopped — the toggle looked obeyed and wasn't.
               (let [answer (str (call! {:session-id "s1"} "local" "anything" {}))]
                 (expect (empty? @connects))
                 (expect (empty? @conns))
                 ;; ...and it says WHY: a killed server is configured and enabled,
                 ;; so "not configured" would send the user editing a correct file.
                 (expect (str/includes? answer "was stopped"))
                 (expect (str/includes? answer "actions/start")))
               (expect (str/includes? (str (call! {:session-id "s1"} "local")) "was stopped"))
               (expect (empty? @connects))
               (finally (reset! conns {}) (reset! killed #{})))))))
  (it "refuses OAuth where it cannot apply and answers unknown flows as such"
      (let
        [merged
         {"mcp" {"servers" {"local" {"transport" "stdio" "command" "echo"}}}}

         thrown
         (fn [f]
           (try (f) ::no-throw (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))]

        (with-redefs-fn {#'config/load-global-config-raw (constantly merged)
                         #'config/load-config-raw (constantly merged)}
          (fn []
            (expect (= :mcp/invalid-server (thrown #(mcp/start-gateway-server-auth! "local"))))
            (expect (= :mcp/not-found (thrown #(mcp/start-gateway-server-auth! "absent"))))
            ;; An expired or forged flow id is a typed refusal the gateway maps to a
            ;; 404 — a client polling a stale flow must never see a stack trace.
            (expect (= :mcp/oauth-flow-not-found (thrown #(mcp/poll-gateway-server-auth! "gone"))))
            (expect (= :mcp/oauth-flow-not-found
                       (thrown #(mcp/complete-gateway-server-auth! "gone" "code-1"))))
            ;; Cancelling one is idempotent: a client may always retry the cleanup.
            (expect (get (mcp/cancel-gateway-server-auth! "gone") "is_cancelled"))))))
  (it "takes the authorization code from a pasted redirect URL or a bare code"
      (let
        [code-of
         (requiring-resolve 'com.blockether.vis.internal.foundation.mcp.oauth/code-of)

         thrown
         (fn [f]
           (try (f) ::no-throw (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))]

        ;; A user on another device can only hand back the whole URL their browser
        ;; landed on, so both shapes have to work.
        (expect (= "abc123" (code-of "abc123")))
        (expect (= "abc123" (code-of "  abc123  ")))
        (expect (= "abc123" (code-of "http://127.0.0.1:8976/callback?code=abc123&state=xyz")))
        (expect (= :mcp/oauth-error
                   (thrown #(code-of "http://127.0.0.1:8976/callback?error=access_denied"))))
        (expect (= :mcp/oauth-error (thrown #(code-of "   "))))))
  (it
    "reaps a dead connection and respawns it with nobody asking"
    (let
      [store
       (atom {"mcp" {"servers" {"local" {"transport" "stdio" "command" "echo"}}}})

       connects
       (atom [])

       dead
       (atom #{})

       conns
       @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'conns)

       killed
       @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'killed)

       reconcile!
       (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'reconcile!)]

      (with-redefs-fn {#'config/load-global-config-raw (fn []
                                                         @store)
                       #'config/load-config-raw (fn []
                                                  @store)
                       #'client/connect (fn [_name _spec]
                                          (let [c (keyword (str "conn-" (count @connects)))]
                                            (swap! connects conj "local")
                                            c))
                       #'client/alive? (fn [c]
                                         (not (contains? @dead c)))
                       #'client/list-tools (constantly [])
                       #'client/close (constantly nil)}
        (fn []
          (try (reset! conns {})
               (reset! killed #{})
               (reconcile!)
               (expect (= ["local"] @connects))
               ;; The stdio child crashes out from under the daemon. Nothing in a
               ;; session can notice or repair that, which is exactly why there is
               ;; no connect verb: the pool has to heal itself.
               (swap! dead conj (get-in @conns ["local" :conn]))
               (reconcile!)
               (expect (= ["local" "local"] @connects))
               (expect (false? (contains? @dead (get-in @conns ["local" :conn]))))
               (finally (reset! conns {}) (reset! killed #{})))))))
  (it "arms exactly one daemon-wide health loop once a server is configured"
      (let
        [store
         (atom {"mcp" {"servers" {"local" {"transport" "stdio" "command" "echo"}}}})

         supervisor
         @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'supervisor)

         ensure-supervisor!
         (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'ensure-supervisor!)]

        (with-redefs-fn {#'config/load-global-config-raw (fn []
                                                           @store)
                         #'config/load-config-raw (fn []
                                                    @store)}
          (fn []
            (let [previous @supervisor]
              (try (reset! supervisor nil)
                   (ensure-supervisor!)
                   (let [^java.util.concurrent.ScheduledExecutorService ex @supervisor]
                     (expect (instance? java.util.concurrent.ScheduledExecutorService ex))
                     ;; Idempotent: a second look at MCP must not leave a second
                     ;; scheduler reconciling the same pool.
                     (ensure-supervisor!)
                     (expect (identical? ex @supervisor))
                     (.shutdownNow ex))
                   (finally (reset! supervisor previous))))))))
  (it
    "closes a connection that finishes racing an already accepted kill"
    (let
      [store
       (atom {"mcp" {"servers" {"local" {"transport" "stdio" "command" "echo"}}}})

       connects
       (atom [])

       closes
       (atom [])

       entered
       (promise)

       release-connect
       (promise)

       conns
       @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'conns)

       killed
       @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'killed)

       call!
       (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'mcp-call-impl)]

      (with-redefs-fn {#'config/load-global-config-raw (fn []
                                                         @store)
                       #'config/load-config-raw (fn []
                                                  @store)
                       #'client/connect (fn [name _spec]
                                          (swap! connects conj name)
                                          (deliver entered true)
                                          @release-connect
                                          ::connection)
                       #'client/list-tools (constantly [])
                       #'client/alive? (constantly true)
                       #'client/close (fn [conn]
                                        (swap! closes conj conn))}
        (fn []
          (reset! conns {})
          (reset! killed #{})
          (let [request (future (call! {:session-id "s1"} "local"))]
            (try (expect (= true (deref entered 5000 ::timeout)))
                 ;; The kill can win while `connect` is still handshaking and before
                 ;; there is a pooled conn for `disconnect!` to see.
                 (mcp/kill-gateway-server! "local")
                 (deliver release-connect true)
                 (let [answer (deref request 5000 ::timeout)]
                   (expect (not= ::timeout answer))
                   (expect (= [::connection] @closes))
                   (expect (empty? @conns))
                   (expect (str/includes? (str answer) "was stopped")))
                 (finally (deliver release-connect true)
                          (future-cancel request)
                          (reset! conns {})
                          (reset! killed #{}))))))))
  (it
    "tears a killed server down without freezing the pool for every other server"
    (let
      [store
       (atom {"mcp" {"servers" {"slow" {"transport" "stdio" "command" "echo"}
                                "other" {"transport" "stdio" "command" "echo"}}}})

       conns
       @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'conns)

       killed
       @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'killed)

       ensure-connected!
       (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'ensure-connected!)

       in-close
       (promise)

       release-close
       (promise)]

      (with-redefs-fn {#'config/load-global-config-raw (fn []
                                                         @store)
                       #'config/load-config-raw (fn []
                                                  @store)
                       #'client/connect (fn [name _spec]
                                          (keyword name))
                       #'client/list-tools (constantly [])
                       #'client/alive? (constantly true)
                       ;; Stands in for the real teardown: an stdio tree takes up to
                       ;; two seconds (SIGTERM then SIGKILL) and an HTTP close waits
                       ;; on a DELETE that a hung server may never answer.
                       #'client/close (fn [_conn]
                                        (deliver in-close true)
                                        @release-close
                                        nil)}
        (fn []
          (try (reset! conns {})
               (reset! killed #{})
               (ensure-connected! "slow")
               (let [kill (future (mcp/kill-gateway-server! "slow"))]
                 (try (expect (= true (deref in-close 5000 ::timeout)))
                      ;; The kill is still inside its teardown. Connecting an
                      ;; UNRELATED server publishes into the same pool: with the
                      ;; teardown held under the pool monitor, this waited for a
                      ;; process to die in every other session too.
                      (let [connect (future (ensure-connected! "other"))]
                        (expect (= :other (deref connect 3000 ::timeout))))
                      (finally (deliver release-close true) (deref kill 5000 nil))))
               (finally (deliver release-close true) (reset! conns {}) (reset! killed #{}))))))))

(defdescribe
  mcp-ctx-block-test
  (it
    "puts each visible server's TOOL NAMES in ctx, keyed by name so one server diffs alone"
    (let
      [store
       (atom {"mcp" {"servers" {"alpha" {"transport" "stdio" "command" "echo"}
                                "beta" {"transport" "stdio" "command" "echo"}}}})

       conns
       @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'conns)

       killed
       @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'killed)

       reconcile!
       (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'reconcile!)

       reconcile-async!
       (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'reconcile-async!)

       contribute
       (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'contribute)

       block
       (fn [c]
         (get-in c ["session_env" "mcp" "servers"]))]

      (with-redefs-fn {#'config/load-global-config-raw (fn []
                                                         @store)
                       #'config/load-config-raw (fn []
                                                  @store)
                       #'client/connect (fn [_name _spec]
                                          {:transport :stdio :tools (atom nil)})
                       #'client/list-tools (constantly [{"name" "write_file"} {"name" "read_file"}])
                       #'client/alive? (constantly true)
                       #'client/close (constantly nil)
                       reconcile-async! (constantly nil)}
        (fn []
          (try (reset! conns {})
               (reset! killed #{})
               (reconcile!)
               (let [before (block (contribute {:session-id "s1"}))]
                 ;; A MAP keyed by server name, not a list: the ctx delta is a
                 ;; structural diff, so a list would re-send every server on any change.
                 (expect (= ["alpha" "beta"] (vec (keys before))))
                 ;; NAMES, sorted — the whole point is answering "what can I call?"
                 ;; without spending a listing round trip first.
                 (expect (= ["read_file" "write_file"] (get-in before ["alpha" "tools"])))
                 (expect (= "connected" (get-in before ["alpha" "status"])))
                 (expect (= "global" (get-in before ["alpha" "scope"])))
                 (expect (= "stdio" (get-in before ["alpha" "transport"])))
                 (mcp/kill-gateway-server! "beta")
                 (let
                   [after (block (contribute {:session-id "s1"}))
                    delta (renderer/render-ctx-delta {"env" {"mcp" {"servers" before}}}
                                                     {"env" {"mcp" {"servers" after}}})]

                   ;; A stopped server stays VISIBLE with a status: "no such server"
                   ;; and "stopped, start it again" are different answers.
                   (expect (= "killed" (get-in after ["beta" "status"])))
                   (expect (nil? (get-in after ["beta" "tools"])))
                   ;; Minimal diff: only the server that moved is re-sent.
                   (expect (str/includes? delta "session[\"env\"][\"mcp\"][\"servers\"][\"beta\"]"))
                   (expect (not (str/includes? delta "alpha")))))
               (finally (reset! conns {}) (reset! killed #{}))))))))

(defdescribe
  mcp-call-two-shapes-test
  (it
    "answers `server` alone with schemas, and refuses an unknown tool with the names it has"
    (let
      [store
       (atom {"mcp" {"servers" {"alpha" {"transport" "stdio" "command" "echo"}}}})

       conns
       @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'conns)

       reconcile!
       (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'reconcile!)

       reconcile-async!
       (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'reconcile-async!)

       call!
       (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'mcp-call-impl)

       called
       (atom nil)]

      (with-redefs-fn {#'config/load-global-config-raw (fn []
                                                         @store)
                       #'config/load-config-raw (fn []
                                                  @store)
                       #'client/connect (fn [_name _spec]
                                          {:transport :stdio :tools (atom nil)})
                       #'client/list-tools (constantly [{"name" "write_file"
                                                         "description" "Write it"
                                                         "inputSchema" {"type" "object"}}])
                       #'client/alive? (constantly true)
                       #'client/close (constantly nil)
                       #'client/call-tool (fn [_conn tool args]
                                            (reset! called [tool args])
                                            {"content" [{"text" "ok"}]})
                       reconcile-async! (constantly nil)}
        (fn []
          (try (reset! conns {})
               (reconcile!)
               ;; Naming ONLY the server IS the schema lookup - the reason no separate
               ;; listing verb has to exist, and nothing is invoked to get it.
               (let [listed (:result (call! {:session-id "s1"} "alpha"))]
                 (expect (= "alpha" (get listed "server")))
                 (expect (= [{"name" "write_file"
                              "description" "Write it"
                              "input_schema" {"type" "object"}}]
                            (get listed "tools")))
                 (expect (nil? @called)))
               ;; A name ctx never advertised is refused WITH the real ones, instead of
               ;; being forwarded to the server as a guess.
               (let [refusal (str (call! {:session-id "s1"} "alpha" "wrte_file" {}))]
                 (expect (str/includes? refusal "exposes no tool"))
                 (expect (str/includes? refusal "write_file"))
                 (expect (nil? @called)))
               (let [answer (:result (call! {:session-id "s1"} "alpha" "write_file" {"path" "p"}))]
                 (expect (= ["write_file" {"path" "p"}] @called))
                 (expect (= "ok" (get-in answer ["content" 0 "text"]))))
               (finally (reset! conns {}))))))))

(defdescribe
  mcp-oauth-visibility-test
  (it
    "tells \"sign in\" apart from \"it is down\", in ctx and in every refusal"
    (let
      [store
       (atom {"mcp" {"servers" {"down" {"transport" "streamable_http"
                                        "url" "https://down.example.test/mcp"}
                                "signed-out" {"transport" "streamable_http"
                                              "url" "https://signed-out.example.test/mcp"}}}})

       resolve*
       (fn [sym]
         (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core sym))

       conns
       @(resolve* 'conns)

       killed
       @(resolve* 'killed)

       configured-servers
       (resolve* 'configured-servers)

       reconcile-async!
       (resolve* 'reconcile-async!)

       contribute
       (resolve* 'contribute)

       unavailable-err
       (resolve* 'unavailable-err)

       call-failed-err
       (resolve* 'call-failed-err)

       msg
       (fn [r]
         (get-in r [:error :message]))

       hint
       (fn [r]
         (get-in r [:error :hint]))

       env
       {:session-id "s1"}]

      (with-redefs-fn {#'config/load-global-config-raw (fn []
                                                         @store)
                       #'config/load-config-raw (fn []
                                                  @store)
                       #'client/connect (fn [_name _spec]
                                          (throw (ex-info "connection refused" {})))
                       #'client/alive? (constantly false)
                       #'client/close (constantly nil)
                       reconcile-async! (constantly nil)}
        (fn []
          (try
            (reset! conns {})
            (reset! killed #{})
            ;; Only a real Bearer challenge means "sign in". EVERY http server
            ;; without a static Authorization header gets a synthesised
            ;; :bearer-fn, so keying off that alone would send the user of a
            ;; server that is merely DOWN hunting for a login screen that does
            ;; not exist.
            (reset! (:www-auth-atom (get (configured-servers) "signed-out"))
              "Bearer resource_metadata=\"https://signed-out.example.test/.well-known/oauth-protected-resource\"")
            (let [block (get-in (contribute env) ["session_env" "mcp" "servers"])]
              (expect (= "needs_auth" (get-in block ["signed-out" "status"])))
              (expect (= "disconnected" (get-in block ["down" "status"])))
              ;; snake_case on every wire surface: `/v1/mcp/servers` answers
              ;; `streamable_http`, so ctx may not spell it `streamable-http`.
              (expect (= "streamable_http" (get-in block ["signed-out" "transport"]))))
            (expect (str/includes? (msg (unavailable-err env "signed-out")) "not authorized"))
            (expect (str/includes? (hint (unavailable-err env "signed-out")) "/auth/start"))
            ;; Configured but unreachable is a transport problem: calling it "not
            ;; configured" while listing it as enabled in the same breath is a lie.
            (expect (str/includes? (msg (unavailable-err env "down")) "unreachable"))
            (expect (str/includes? (msg (unavailable-err env "ghost")) "not configured"))
            ;; Nothing below the extension may escape as a throw: an exception
            ;; ends the turn, an envelope leaves the model free to react.
            (expect (str/includes? (msg (call-failed-err env
                                                         "signed-out"
                                                         "x"
                                                         (ex-info "401"
                                                                  {:type :mcp/oauth-required})))
                                   "not authorized"))
            (expect (str/includes? (msg (call-failed-err env "down" "x" (ex-info "boom" {})))
                                   "boom"))
            (finally (reset! conns {}) (reset! killed #{}))))))))
