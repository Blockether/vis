(ns com.blockether.vis.internal.foundation.mcp.core-test
  (:require [babashka.http-client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.context.renderer :as renderer]
            [com.blockether.vis.internal.docs.corpus :as doc-corpus]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.mcp.client :as client]
            [com.blockether.vis.internal.foundation.mcp.core :as mcp]
            [com.blockether.vis.internal.foundation.mcp.oauth :as oauth]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.internal.python.extensions :as py-extensions]
            [com.blockether.vis.test-python-context :as tpc]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.util :as util]
            [lazytest.core :refer [around-each defdescribe expect it set-ns-context!]]))

;; Direct MCP calls need the same op registration the distribution manifest supplies.
(set-ns-context!
  [(around-each
     [f]
     (let [registered? (some #(= "foundation-mcp" (:ext/name %)) (extension/registered-extensions))]
       (when-not registered? (mcp/register!))
       (try (with-redefs [config/load-global-yaml-config-raw (constantly {})
                          config/load-global-config-raw (constantly {})]

              (f))
            (finally (when-not registered? (extension/deregister-extension! "foundation-mcp"))))))])

(defdescribe mcp-native-contract-test
             (it
               "exposes exactly ONE verb — the inventory is ctx's job, connecting is the gateway's"
               (let [names (set (map :ext.symbol/name
                                     (get-in mcp/vis-extension [:ext/engine :ext.engine/symbols])))]
                 (expect (= #{"mcp__call"} names))
                 ;; Server names and status ride in `env.mcp`, and `apropos` and `doc`
                 ;; give the tools: a listing verb would only fetch them again.
                 (expect (not (contains? names "mcp__servers")))
                 (expect (not (contains? names "mcp__tools")))
                 ;; A session must not be able to yank a connection every other session is
                 ;; using, nor be expected to establish one the daemon already owes it.
                 (expect (not (contains? names "mcp__connect")))
                 (expect (not (contains? names "mcp__disconnect"))))))

(defdescribe
  mcp-sandbox-name-test
  "Regression: every string the MODEL reads has to name the binding it can type.
   The wire tool is `mcp__call`, and a session that copied that name out of
   `doc(\"linear/save_issue\")` spent four iterations on NameErrors. A block now
   types the name of a server, and no name of the sandbox is `mcp` or `mcp_call`."
  (it "puts a callable form, not the wire name, on every tool entry"
      (with-redefs-fn {#'mcp/visible-servers (constantly {"linear" {}})
                       #'mcp/conn-of (fn [& _]
                                       {:tools (atom [{"name" "save_issue"
                                                       "description" "File one."}])})}
        (fn []
          (let [[server entry] (#'mcp/doc-corpus-entries)]
            (expect (= "linear" (:name server)))
            (expect (= "linear.save_issue" (:name entry)))
            (expect (= "await linear.save_issue()" (:call entry)))
            ;; The FIRST line is what `apropos` prints as the row's body, so the
            ;; call form has to live there and not below the description.
            (expect (str/includes? (first (str/split-lines (:text entry)))
                                   "await linear.save_issue()"))
            (expect (not (str/includes? (str (:call entry) (:text entry)) "mcp__call")))))))
  (it "lets apropos find a tool by the server's description"
      ;; #466: the server's description sat below a template that every MCP tool
      ;; shares, so a search by intent missed it.
      (with-redefs-fn {#'mcp/visible-servers (constantly {"linear" {}})
                       #'mcp/conn-of (fn [& _]
                                       {:tools (atom [{"name" "save_issue"
                                                       "description" "Create or update a ticket."}
                                                      {"name" "list_teams"
                                                       "description" "List teams."}])})}
        (fn []
          (let [es (#'mcp/doc-corpus-entries)]
            (expect (= ["linear.save_issue"] (mapv :name (doc-corpus/search es "ticket"))))
            (expect (= ["linear"] (mapv :name (doc-corpus/search es "server")))))))))

(defdescribe
  mcp-call-env-injection-test
  (it
    "injects the live caller session through the Python boundary for every call shape"
    ;; Regression: the verb with only a server reached an env-first implementation without env.
    (let [env
          (atom nil)

          seen
          (atom [])

          called
          (atom [])]

      (with-redefs-fn {#'mcp/ensure-connected! (fn [sid server]
                                                 (swap! seen conj [sid server])
                                                 ::connection)
                       #'client/list-tools (constantly [{"name" "read"
                                                         "inputSchema" {"type" "object"}}])
                       #'client/call-tool (fn [_ tool args & _]
                                            (swap! called conj [tool args])
                                            {"content" [{"type" "text" "text" "ok"}]})}
        (fn []
          (tpc/with-own
            [ctx
             {(extension/sandbox-binding mcp/vis-extension 'call)
              (get (extension/wrap-extension-thunked mcp/vis-extension #(deref env)) 'call)}]
            (doseq [sid ["caller-session" "other-session"]]
              (reset! env {:session-id sid})
              (let
                [result
                 (ep/run-python-block
                   ctx
                   (str
                     "assert (await __vis_mcp_call__('alpha'))['tools'][0]['name'] == 'read'\n"
                     "assert (await __vis_mcp_call__('alpha', 'read'))['content'][0]['text'] == 'ok'\n"
                     "assert (await __vis_mcp_call__('alpha', 'read', {'query': 'one'}))['content'][0]['text'] == 'ok'\n"
                     "assert (await __vis_mcp_call__(server='alpha', tool='read', args={'query': 'two'}))['content'][0]['text'] == 'ok'\n"
                     "print('mcp-ok')"))]
                (expect (nil? (:error result)))
                (expect (= "mcp-ok\n" (:stdout result)))))
            (expect (= (concat (repeat 4 ["caller-session" "alpha"])
                               (repeat 4 ["other-session" "alpha"]))
                       @seen))
            (expect (= (apply concat
                         (repeat 2 [["read" {}] ["read" {"query" "one"}] ["read" {"query" "two"}]]))
                       @called))))))))

(defdescribe mcp-session-definitions-snapshot-test
             ;; Perf pass 2026-11: one context render read the session's MCP definitions from
             ;; the store for each of its four to six server-list computations.
             (it "reads the session definitions once inside one settings snapshot"
                 (let [reads
                       (atom 0)

                       env
                       {:db-info ::db :session-id "s"}]

                   (with-redefs [scoped/target
                                 (fn [& _]
                                   {:scope "session"})

                                 scoped/definitions
                                 (fn [& _]
                                   (swap! reads inc)
                                   [])]

                     (binding [extension/*current-environment* env]
                       (util/with-memo-scope (#'mcp/visible-servers "s")
                                             (#'mcp/visible-servers "s"))))
                   (expect (= 1 @reads)))))

(defdescribe
  mcp-connect-tool-cache-test
  ;; Perf pass 2026-11: a context render met a fresh conn and paid the `tools/list`
  ;; round trip itself, 420 ms on the turn thread.
  (it "publishes a new connection with its tools already listed"
      (let [requests
            (atom [])

            conn
            {:tools (atom nil)
             :alive-fn (constantly true)
             :close-fn (fn []
                         nil)
             :request-fn (fn [method _ _]
                           (swap! requests conj method)
                           {"tools" [{"name" "read"}]})}

            pool
            (atom {})]

        (with-redefs [client/connect (constantly conn)]
          (#'mcp/ensure-in-pool! pool "remote" "remote" {:transport :stdio} (constantly true)))
        (expect (= ["tools/list"] @requests))
        (expect (= [{"name" "read"}] (#'mcp/conn-tools (get-in @pool ["remote" :conn]))))
        (expect (= ["tools/list"] @requests)))))

(defdescribe
  mcp-failed-catalog-status-test
  (it "marks a failed inventory listing disconnected in the same snapshot"
      ;; Regression: Linear rejected credentials while inventory still claimed connected, 0 tools.
      (let [alive
            (atom true)

            conn
            {:tools (atom nil) :alive-fn #(deref alive) :close-fn #(reset! alive false)}]

        (with-redefs-fn {#'mcp/conn-of (constantly conn)
                         #'client/list-tools
                         (fn [_]
                           (throw (ex-info "Rejected" {:type :mcp/http-error :status 401})))
                         #'oauth/token-status (constantly {"is_authorized" false})
                         #'mcp/reconcile-async! (constantly nil)
                         #'mcp/visible-servers (constantly {"remote" {:transport :streamable-http}})
                         #'mcp/needs-auth? (constantly true)}
          (fn []
            (let [row (#'mcp/server-summary "remote" {"transport" "streamable_http"} false)]
              (expect (false? (get row "is_connected")))
              (expect (= 0 (get row "tools")))
              (expect (false? @alive))
              (expect (= "needs_auth"
                         (get-in (#'mcp/contribute {:session-id "s"})
                                 ["session_env" "mcp" "servers" "remote" "status"]))))))))
  (it "reports a tool catalog failure instead of returning a successful empty catalog"
      (with-redefs-fn {#'mcp/ensure-connected! (constantly {:tools (atom nil)})
                       #'client/list-tools (fn [_]
                                             (throw (ex-info "Catalog unavailable"
                                                             {:type :mcp/http-error :status 503})))
                       #'mcp/needs-auth? (constantly false)}
        (fn []
          (let [result (#'mcp/mcp-call-impl {:session-id "s"} "remote")]
            (expect (str/includes? (str result) "Catalog unavailable"))
            (expect (nil? (:result result))))))))

(defdescribe
  gateway-mcp-auth-validation-test
  (it "allows an OAuth challenge to be saved for sign-in without claiming a connection"
      (with-redefs [oauth/make-bearer-fn
                    (fn [& _]
                      (constantly nil))

                    http/request
                    (fn [_]
                      {:status 401
                       :headers {"www-authenticate"
                                 "Bearer resource_metadata=\"https://gateway.example.com/oauth\""}
                       ;; The transport reads the body as a stream.
                       :body (java.io.ByteArrayInputStream. (.getBytes "Unauthorized" "UTF-8"))})]

        (expect (= {"name" "remote" "is_connected" false "tools" []}
                   (mcp/test-gateway-server! "remote"
                                             {"transport" "streamable_http"
                                              "url" "https://gateway.example.com/mcp"})))))
  (it "does not accept missing challenges, static credentials, or non-auth failures"
      (doseq [[status challenge headers] [[401 nil {}] [401 "Basic realm=mcp" {}] [500 "Bearer" {}]
                                          [401 "Bearer" {"Authorization" "Bearer private"}]]]
        (let [failure (ex-info "Rejected" {:type :mcp/http-error :status status})]
          (with-redefs [client/connect (fn [_ spec]
                                         (when-let [a (:www-auth-atom spec)]
                                           (reset! a challenge))
                                         (throw failure))]
            (expect (identical? failure
                                (try (mcp/test-gateway-server! "remote"
                                                               {"transport" "streamable_http"
                                                                "url"
                                                                "https://gateway.example.com/mcp"
                                                                "headers" headers})
                                     (catch clojure.lang.ExceptionInfo e e)))))))))

(defdescribe
  mcp-sign-in-backoff-test
  (it "asks an OAuth server that nobody signed in to once per backoff, not on every connect"
      ;; Regression #458: 45 `HTTP 401 on initialize` connects in two minutes of turns.
      (let [requests
            (atom 0)

            missing
            (java.io.File. (System/getProperty "java.io.tmpdir")
                           (str "vis-mcp-none-" (random-uuid) ".edn"))]

        (with-redefs-fn {#'oauth/token-file (constantly missing)
                         #'http/request (fn [_]
                                          (swap! requests inc)
                                          {:status 401
                                           :headers {"www-authenticate" "Bearer realm=\"mcp\""}
                                           :body ""})
                         #'config/load-global-config-raw
                         (constantly {"mcp" {"servers" {"remote"
                                                        {"transport" "streamable_http"
                                                         "url" "https://mcp.example.test/mcp"}}}})}
          (fn []
            (#'mcp/reset-server-cache!)
            (try (dotimes [_ 3]
                   (expect (nil? (#'mcp/ensure-connected! "remote"))))
                 (expect (= 1 @requests))
                 (finally (#'mcp/clear-auth-backoff! "remote") (#'mcp/reset-server-cache!))))))))

(defdescribe
  mcp-server-name-test
  (it "stores one lower-case name, whatever blanks, case or older key the user gave"
      (let [store (atom {"mcp" {"servers" {"Figma Dev" {"transport" "stdio" "command" "echo"}}}})]
        (with-redefs-fn {#'config/load-global-config-raw (fn []
                                                           @store)
                         #'config/update-machine-config! (fn [f & _]
                                                           (swap! store f))
                         #'mcp/reconcile-async! (constantly nil)}
          (fn []
            (#'mcp/reset-server-cache!)
            (try (expect (= "figma-dev"
                            (get (mcp/save-gateway-server! "  FIGMA\n dev "
                                                           {"transport" "stdio" "command" "true"})
                                 "name")))
                 (expect (= {"figma-dev" {"transport" "stdio" "command" "true"}}
                            (get-in @store ["mcp" "servers"])))
                 (expect (= :mcp/invalid-name
                            (try (mcp/save-gateway-server! " \n- "
                                                           {"transport" "stdio" "command" "true"})
                                 nil
                                 (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
                 (finally (#'mcp/reset-server-cache!))))))))

(defdescribe
  gateway-mcp-management-test
  (it "persists gateway server values while returning only a sanitized inventory row"
      (let [saved
            (atom nil)

            reconnect!
            (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'reconcile-async!)]

        (with-redefs-fn {#'config/load-global-config-raw (constantly {})
                         #'config/load-global-yaml-config-raw (constantly {})
                         #'config/save-config! (fn [value source]
                                                 (reset! saved [value source]))
                         reconnect! (constantly nil)}
          (fn []
            (let [row (mcp/save-gateway-server! "filesystem"
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
      (let [seen
            (atom nil)

            closed
            (atom nil)]

        (with-redefs [client/connect
                      (fn [name spec]
                        (reset! seen [name spec])
                        ::connection)

                      client/list-tools
                      (constantly [{"name" "list_files" "description" "Enumerate files"}])

                      client/close
                      (fn [conn]
                        (reset! closed conn))]

          (let [result (mcp/test-gateway-server! "remote"
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
  (it
    "keeps persisted env and headers when a save omits them"
    (let [store
          (atom {"mcp" {"servers" {"remote" {"transport" "streamable_http"
                                             "url" "https://mcp.example.test/mcp"
                                             "headers" {"Authorization" "Bearer keep-me"}}}}})

          reconnect!
          (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'reconcile-async!)]

      (with-redefs-fn {#'config/load-global-config-raw (fn []
                                                         @store)
                       #'config/load-global-yaml-config-raw (fn []
                                                              @store)
                       #'config/save-config! (fn [value _source]
                                               (reset! store value))
                       #'config/update-machine-config! (fn [f & _]
                                                         (let [value (f @store)]
                                                           (when (and value (not= value @store))
                                                             (reset! store value))))
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
    (let [saved
          (atom nil)

          reconnect!
          (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'reconcile-async!)

          merged
          {"mcp" {"servers" {"team" {"transport" "stdio" "command" "echo"}}}}

          thrown
          (fn [f]
            (try (f) ::no-throw (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))]

      (with-redefs-fn {#'config/load-global-config-raw (constantly {})
                       #'config/load-global-yaml-config-raw (constantly merged)
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
  (it "refuses to sign out of a server that it does not know and that has no tokens"
      (let [forgotten
            (atom [])

            merged
            {"mcp" {"servers" {"team" {"transport" "stdio" "command" "echo"}}}}

            tokens
            {"stale" {"is_authorized" true}}]

        (with-redefs-fn {#'config/load-global-config-raw (constantly {})
                         #'config/load-global-yaml-config-raw (constantly merged)
                         #'oauth/token-status (fn [server]
                                                (get tokens server {"is_authorized" false}))
                         #'oauth/forget! #(swap! forgotten conj %)}
          (fn []
            (expect (= :mcp/not-found
                       (try (mcp/logout-gateway-server-auth! "absent")
                            (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
            (expect (= [] @forgotten))
            ;; A configured server, and the tokens of a removed one, are signed out.
            (mcp/logout-gateway-server-auth! "team")
            (mcp/logout-gateway-server-auth! "stale")
            (expect (= ["team" "stale"] @forgotten))))))
  (it "carries the non-secret rest of a spec so a client can edit it without losing it"
      (let [machine {"mcp" {"servers" {"owned" {"transport" "stdio"
                                                "command" "npx"
                                                "args" ["-y" "server-filesystem" "/srv"]
                                                "cwd" "/srv"
                                                "timeout_ms" 45000
                                                "env" {"API_TOKEN" "never-leaves"}}}}}]
        (with-redefs-fn {#'config/load-global-config-raw (constantly machine)
                         #'config/load-global-yaml-config-raw (constantly machine)}
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
      (let [machine
            {"mcp" {"servers" {"owned" {"transport" "stdio" "command" "npx"}}}}

            merged
            (assoc-in machine ["mcp" "servers" "team"] {"transport" "stdio" "command" "echo"})

            tool-count
            (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'tool-count)]

        ;; Regression, CI run 33990746062: this inventory-only fixture launched
        ;; an "owned" connection after its mocks had escaped into the next test.
        (with-redefs-fn {#'com.blockether.vis.internal.foundation.mcp.core/reconcile-async!
                         (constantly nil)
                         #'config/load-global-config-raw (constantly machine)
                         #'config/load-global-yaml-config-raw (constantly merged)}
          (fn []
            (let [rows (get (mcp/gateway-servers) "servers")]
              (expect (= ["owned" "team"] (mapv #(get % "name") rows)))
              (expect (= [true false] (mapv #(get % "is_managed") rows))))))
        (with-redefs [client/list-tools (constantly [{"name" "a"} {"name" "b"}])]
          ;; A freshly connected server's tool cache is still nil; reading the atom
          ;; alone reported 0 tools for every healthy server.
          (expect (= 2 (tool-count {:name "owned" :tools (atom nil) :alive-fn (constantly true)})))
          (expect (= 1
                     (tool-count
                       {:name "owned" :tools (atom [{"name" "a"}]) :alive-fn (constantly true)})))
          (expect (= 0 (tool-count nil)))))))

(defdescribe
  mcp-unhealthy-status-test
  (it
    "stops describing a failed handshake as connecting, and clears it on recovery or a new spec"
    (let [spec
          (atom {:transport :stdio :command "echo"})

          failing?
          (atom true)

          conns
          @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'conns)

          killed
          @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'killed)

          failures
          @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'connect-failures)

          reconciling?
          @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'reconciling?)

          summary
          #(get
             (#'mcp/server-summary "stuck" {"transport" "stdio" "command" (:command @spec)} false)
             "status")]

      (with-redefs-fn {#'mcp/configured-servers (fn []
                                                  {"stuck" @spec})
                       #'client/connect (fn [_ _]
                                          (case @failing?
                                            true
                                            (throw (ex-info "handshake failed" {}))

                                            :auth
                                            (throw (ex-info "Sign in needed"
                                                            {:type :mcp/oauth-required}))

                                            {:tools (atom []) :alive-fn (constantly true)}))
                       #'client/list-tools (constantly [])
                       #'client/close (constantly nil)
                       #'mcp/reconcile-async! (constantly nil)}
        (fn []
          ;; Regression, CI run 41626644174: a reconcile that an earlier test started ran
          ;; under these redefs and marked "stuck" unhealthy before the first check. Hold
          ;; the single-flight guard, so no background reconcile runs during this case.
          (let [deadline (+ (System/currentTimeMillis) 10000)]
            (while (and (not (compare-and-set! reconciling? false true))
                        (< (System/currentTimeMillis) deadline))
              (Thread/sleep 5)))
          (try (reset! conns {})
               (reset! killed #{})
               (reset! failures {})
               (expect (= "connecting" (summary)))
               (#'mcp/ensure-connected! "stuck")
               (expect (= "unhealthy" (summary)))
               (expect (= "unhealthy"
                          (get-in (#'mcp/contribute {:session-id "session"})
                                  ["session_env" "mcp" "servers" "stuck" "status"])))
               (reset! failing? :auth)
               (#'mcp/ensure-connected! "stuck")
               (expect (= "connecting" (summary)))
               (swap! spec assoc :command "new-command")
               (expect (= "connecting" (summary)))
               (reset! failing? false)
               (#'mcp/ensure-connected! "stuck")
               (expect (= "connected" (summary)))
               (finally (reset! conns {})
                        (reset! killed #{})
                        (#'mcp/clear-auth-backoff! "stuck")
                        (reset! failures {})
                        (reset! reconciling? false))))))))

(defdescribe
  mcp-session-failure-cleanup-test
  (it "drops a failed session-scoped server verdict when its session detaches"
      (let [sid
            "failed-session-test"

            failures
            @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'connect-failures)]

        (with-redefs-fn {#'mcp/configured-servers (constantly {})
                         #'mcp/reconcile-async! (constantly nil)
                         #'client/connect (fn [_ _]
                                            (throw (ex-info "unavailable" {})))}
          (fn []
            (try (reset! failures {})
                 (mcp/set-session-servers! sid {"broken" {"transport" "stdio" "command" "echo"}})
                 (expect (contains? @failures [sid "broken"]))
                 (mcp/clear-session-servers! sid)
                 (expect (not (contains? @failures [sid "broken"])))
                 (finally (mcp/clear-session-servers! sid) (reset! failures {}))))))))

(defdescribe
  gateway-mcp-runtime-test
  (it
    "keeps a killed server down across reconciles until it is started again"
    (let [store
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
                       #'config/load-global-yaml-config-raw (fn []
                                                              @store)
                       #'config/save-config! (fn [value _source]
                                               (reset! store value))
                       #'client/connect (fn [name _spec]
                                          (swap! connects conj name)
                                          {:tools (atom []) :alive-fn (constantly true)})
                       #'client/list-tools (constantly [])
                       #'client/close (constantly nil)}
        (fn []
          (try (reset! conns {})
               (reset! killed #{})
               (reconcile!)
               (expect (= #{"local"} (set @connects)))
               (expect (contains? @conns "local"))
               (let [row (mcp/kill-gateway-server! "local")]
                 (expect (true? (get row "is_killed")))
                 (expect (false? (get row "is_connected"))))
               (expect (empty? @conns))
               ;; Everything below is counted FROM THE KILL, never from the start of
               ;; the case: the health loop and the per-turn nudge reconcile this
               ;; same pool off other threads, so an exact connect log is a race
               ;; while "nothing reconnects while the brake is on" is the contract.
               (let [killed-at (count @connects)]
                 ;; The brake has to survive the per-turn reconcile: closing the
                 ;; connection alone let the very next turn respawn the stdio child
                 ;; the user just killed.
                 (reconcile!)
                 (expect (= killed-at (count @connects)))
                 (let [row (mcp/start-gateway-server! "local")]
                   (expect (false? (get row "is_killed")))
                   (expect (true? (get row "is_connected"))))
                 (expect (< killed-at (count @connects)))
                 (expect (= #{"local"} (set @connects))))
               ;; A kill is a RUNTIME brake, never an edit to anybody's config.
               (expect (= {"transport" "stdio" "command" "echo"}
                          (get-in @store ["mcp" "servers" "local"])))
               (finally (reset! conns {}) (reset! killed #{})))))))
  (it
    "refuses a tool call on a killed server instead of quietly respawning it"
    (let [store
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
                       #'config/load-global-yaml-config-raw (fn []
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
      (let [merged
            {"mcp" {"servers" {"local" {"transport" "stdio" "command" "echo"}}}}

            thrown
            (fn [f]
              (try (f) ::no-throw (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))]

        (with-redefs-fn {#'config/load-global-config-raw (constantly merged)
                         #'config/load-global-yaml-config-raw (constantly merged)}
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
      (let [code-of
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
    (let [store
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
                       #'config/load-global-yaml-config-raw (fn []
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
               (expect (= #{"local"} (set @connects)))
               ;; The stdio child crashes out from under the daemon. Nothing in a
               ;; session can notice or repair that, which is exactly why there is
               ;; no connect verb: the pool has to heal itself.
               (let [crashed-at (count @connects)]
                 (swap! dead conj (get-in @conns ["local" :conn]))
                 (reconcile!)
                 ;; A RECONNECT, counted from the crash — not a call log. The health
                 ;; loop and the per-turn nudge reconcile this same pool off other
                 ;; threads, so an exact connect count is a race; "the dead conn was
                 ;; replaced by a live one" is the contract.
                 (expect (< crashed-at (count @connects)))
                 (expect (false? (contains? @dead (get-in @conns ["local" :conn])))))
               (finally (reset! conns {}) (reset! killed #{})))))))
  (it "arms exactly one daemon-wide health loop once a server is configured"
      (let [store
            (atom {"mcp" {"servers" {"local" {"transport" "stdio" "command" "echo"}}}})

            supervisor
            @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'supervisor)

            ensure-supervisor!
            (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'ensure-supervisor!)]

        (with-redefs-fn {#'config/load-global-config-raw (fn []
                                                           @store)
                         #'config/load-global-yaml-config-raw (fn []
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
    (let [store
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
                       #'config/load-global-yaml-config-raw (fn []
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
    (let [store
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
                       #'config/load-global-yaml-config-raw (fn []
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
    (let [store
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
                       #'config/load-global-yaml-config-raw (fn []
                                                              @store)
                       #'client/connect (fn [_name _spec]
                                          {:transport :stdio :tools (atom nil)})
                       #'client/list-tools (constantly [{"name" "write_file"} {"name" "read_file"}])
                       #'client/alive? (constantly true)
                       #'client/close (constantly nil)
                       reconcile-async! (constantly nil)}
        (fn []
          (try
            (reset! conns {})
            (reset! killed #{})
            (reconcile!)
            (let [before (block (contribute {:session-id "s1"}))]
              ;; A MAP keyed by server name, not a list: the ctx delta is a
              ;; structural diff, so a list would re-send every server on any change.
              (expect (= ["alpha" "beta"] (vec (keys before))))
              ;; One row for each server, with the name that a block types. The
              ;; tools stay out: `apropos` and `doc` give them.
              (expect
                (= {"python_name" "alpha" "scope" "global" "transport" "stdio" "status" "connected"}
                   (get before "alpha")))
              (mcp/kill-gateway-server! "beta")
              (let [after (block (contribute {:session-id "s1"}))
                    delta (renderer/render-ctx-delta {"env" {"mcp" {"servers" before}}}
                                                     {"env" {"mcp" {"servers" after}}})]

                ;; A stopped server stays VISIBLE with a status: "no such server"
                ;; and "stopped, start it again" are different answers.
                (expect (= "killed" (get-in after ["beta" "status"])))
                ;; Minimal diff: only the server that moved is re-sent.
                (expect (str/includes? delta "session[\"env\"][\"mcp\"][\"servers\"][\"beta\"]"))
                (expect (not (str/includes? delta "alpha")))))
            (finally (reset! conns {}) (reset! killed #{}))))))))

(defdescribe
  mcp-call-two-shapes-test
  (it
    "answers `server` alone with schemas, and refuses an unknown tool with the names it has"
    (let [store
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
                       #'config/load-global-yaml-config-raw (fn []
                                                              @store)
                       #'client/connect (fn [_name _spec]
                                          {:transport :stdio :tools (atom nil)})
                       #'client/list-tools (constantly [{"name" "write_file"
                                                         "description" "Write it"
                                                         "inputSchema" {"type" "object"}}])
                       #'client/alive? (constantly true)
                       #'client/close (constantly nil)
                       #'client/call-tool (fn [_conn tool args & _]
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

(defn- with-alpha
  "Run `(f call!)` with the configured server `alpha`, where `client` maps a var of
   the MCP client to its stand-in. `call!` is the host verb."
  [client f]
  (let [store
        {"mcp" {"servers" {"alpha" {"transport" "stdio" "command" "echo"}}}}

        conns
        @(ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'conns)]

    (with-redefs-fn (merge {#'config/load-global-config-raw (constantly store)
                            #'config/load-global-yaml-config-raw (constantly store)
                            #'client/list-tools (constantly [{"name" "write_file"}])
                            #'client/alive? (constantly true)
                            #'client/close (constantly nil)}
                           client)
      (fn []
        (try (reset! conns {})
             (f (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'mcp-call-impl))
             (finally (reset! conns {})))))))

(defn- era-calls
  "Call a tool of a server that refuses each request of an older revision. The
   connections are stateless from the connect number `modern-from`. Returns the
   answer and the era of each connection that got the call."
  [modern-from]
  (let [connects
        (atom 0)

        calls
        (atom [])]

    (with-alpha {#'client/connect (fn [_ _]
                                    {:transport :streamable-http
                                     :tools (atom nil)
                                     :modern? (>= (long (swap! connects inc)) (long modern-from))})
                 #'client/call-tool (fn [conn & _]
                                      (swap! calls conj (:modern? conn))
                                      (if (:modern? conn)
                                        {"content" [{"text" "ok"}]}
                                        (throw (ex-info "HTTP 400 on tools/call"
                                                        {:type :mcp/http-error :status 400}))))}
                (fn [call!]
                  [(call! {:session-id "s1"} "alpha" "write_file" {}) @calls]))))

(defdescribe mcp-era-change-test
             (it "connects again and repeats the call when the server changed its era"
                 (let [[answer calls] (era-calls 2)]
                   (expect (= "ok" (get-in (:result answer) ["content" 0 "text"])))
                   (expect (= [false true] calls))))
             (it "reports the refusal, with no second call, when the era is the same"
                 (let [[answer calls] (era-calls 9)]
                   (expect (str/includes? (str answer) "HTTP 400 on tools/call"))
                   (expect (= [false] calls)))))

(defdescribe
  mcp-method-call-test
  (it "sends an MCP request for a resource, with the roots of the session"
      (let [sent
            (atom nil)

            root
            (.getCanonicalPath (io/file (System/getProperty "java.io.tmpdir")))

            env
            {:session-id "s1" :workspace/root root}]

        (with-alpha
          {#'client/connect
           (fn [_ spec]
             {:transport :stdio :tools (atom nil) :spec spec :server-capabilities {"resources" {}}})
           #'client/request (fn [_conn method params _timeout-ms opts]
                              (reset! sent [method params (:roots opts)])
                              {"resultType" "complete"
                               "contents" [{"uri" "file:///a" "text" "A"}]})}
          (fn [call!]
            (expect (= ["resources/list" "resources/templates/list" "resources/read"]
                       (get (:result (call! env "alpha")) "methods")))
            (expect (= {"server" "alpha"
                        "method" "resources/read"
                        "contents" [{"uri" "file:///a" "text" "A"}]}
                       (:result (call! env "alpha" "resources/read" {"uri" "file:///a"}))))
            (expect (= ["resources/read" {"uri" "file:///a"} [(str "file://" root)]]
                       (update @sent
                               2
                               #(mapv (fn [r]
                                        (get r "uri"))
                                      %))))
            (expect (str/includes? (str (call! env "alpha" "prompts/list")) "has no prompts")))))))

(def ^:private catalog-tools
  [{"name" "resolve-library-id"
    "description" "Find a library."
    "inputSchema" {"type" "object"
                   "properties"
                   {"query" {"type" "string"} "limit" {"type" "integer"} "page-size" {}}
                   "required" ["query"]}}])

(defn- with-catalog
  "Run `(f call!)` with the server `alpha`, which has one tool, two prompts and two
   resources. `sent` gets each request as `[method params]`."
  [sent f]
  (with-alpha
    {#'client/connect (fn [_ spec]
                        {:transport :stdio
                         :tools (atom catalog-tools)
                         :lists (atom {})
                         :spec spec
                         :server-capabilities {"resources" {} "prompts" {}}})
     #'client/list-tools (constantly catalog-tools)
     #'client/call-tool (fn [_ tool args & _]
                          (swap! sent conj ["tools/call" {"name" tool "arguments" args}])
                          {"content" [{"type" "text" "text" "ok"}]
                           "structuredContent" {"library" "/x/y"}})
     #'client/request
     (fn [_conn method params & _]
       (swap! sent conj [method params])
       (case method
         "prompts/list"
         {"prompts" [{"name" "code-review" "description" "Review a change."}
                     {"name" "greet" "arguments" [{"name" "who" "required" true}]}]}

         "prompts/get"
         {"messages" [{"role" "user" "content" {"type" "text" "text" "Review it with care."}}]}

         "resources/list"
         {"resources" [{"uri" "file:///notes.txt" "name" "notes.txt" "mimeType" "text/plain"}
                       {"uri" "file:///dot.png" "name" "dot.png" "mimeType" "image/png"}]}

         "resources/read"
         {"contents" [(if (= "file:///dot.png" (get params "uri"))
                        {"uri" "file:///dot.png" "mimeType" "image/png" "blob" "iVBORw=="}
                        {"uri" (get params "uri") "text" "zażółć"})]}))}
    f))

(defn- servers-ctx
  "The context of a session that sees `servers`, each with the name that the host
   gives it."
  [& servers]
  {"env" {"mcp" {"servers" (into {}
                                 (map (fn [server]
                                        [server {"python_name" (#'mcp/python-name server)}]))
                                 servers)}}})

(defn- bind-verb!
  "Bind the symbols of the MCP extension in `ctx`, as the engine does at each turn."
  [ctx]
  (loop-env/sync-active-extension-symbols!
    {:python-context ctx :extensions (atom [mcp/vis-extension]) :active-extensions (atom [])}
    [mcp/vis-extension]))

(defn- extension-call!
  "Call `tool` through `vis.tools`, as an extension of a session with the MCP
   extension does."
  [tool args kwargs]
  (binding [extension/*current-environment* {:extensions (atom [mcp/vis-extension])
                                             :active-extensions (atom [mcp/vis-extension])}]
    (#'py-extensions/call-tool! ::caller tool args kwargs)))

(defdescribe
  mcp-extension-tool-test
  (it "runs a tool and a prompt of a server for an extension, by the name that a block types"
      (let [sent (atom [])]
        (with-catalog
          sent
          (fn [_call!]
            (expect (= "resolve-library-id"
                       (get (extension-call! "alpha.resolve_library_id" [] {"query" "x"}) "tool")))
            (extension-call! "alpha.resolve-library-id" [{"query" "y"}] {})
            (extension-call! "alpha.prompts.greet" [] {"who" "Ann"})
            (expect (= [["tools/call" {"name" "resolve-library-id" "arguments" {"query" "x"}}]
                        ["tools/call" {"name" "resolve-library-id" "arguments" {"query" "y"}}]
                        ["prompts/get" {"name" "greet" "arguments" {"who" "Ann"}}]]
                       (filterv #(#{"tools/call" "prompts/get"} (first %)) @sent)))
            (expect (= ["resolve-library-id"]
                       (mapv #(get % "name") (get (extension-call! "alpha" [] {}) "tools"))))))))
  (it "has no tool for the host verb or for a server that the session does not see"
      (with-catalog (atom [])
                    (fn [_call!]
                      (doseq [tool ["mcp_call" "beta.read"]]
                        (expect (= :unknown-tool
                                   (try (extension-call! tool ["alpha"] {})
                                        (catch clojure.lang.ExceptionInfo e
                                          (:error (ex-data e)))))))))))

(defdescribe
  mcp-doc-handle-test
  (it "prints the page of a tool before a call listed it, by the name that a block types"
      (with-catalog (atom [])
                    (fn [_call!]
                      (let [entry (doc-corpus/resolve-entry "alpha.resolve_library_id")]
                        (expect (= "alpha.resolve_library_id" (:name entry)))
                        (expect (= "await alpha.resolve_library_id(query=...)" (:call entry)))
                        (expect (str/includes? (:text entry) "Find a library."))
                        ;; The schema is JSON, as a block reads it.
                        (expect (str/includes? (:text entry) "\"required\":[\"query\"]"))
                        ;; The handles of the older pages still have their page.
                        (expect (= entry (doc-corpus/resolve-entry "mcp.alpha.resolve_library_id")))
                        (expect (= entry (doc-corpus/resolve-entry "alpha/resolve-library-id")))))))
  (it "prints a prompt with its messages, and asks no messages of a prompt that needs an argument"
      (let [sent (atom [])]
        (with-catalog sent
                      (fn [_call!]
                        (let [review (doc-corpus/resolve-entry "alpha.prompts.code_review")
                              greet (doc-corpus/resolve-entry "alpha.prompts.greet")]

                          (expect (= "prompt" (:kind review)))
                          (expect (str/includes? (:text review) "Review it with care."))
                          (expect (= "await alpha.prompts.greet(who=...)" (:call greet)))
                          (expect (str/includes? (:text greet) "`who` (required)"))
                          (expect (= [["prompts/get" {"name" "code-review"}]]
                                     (filterv #(= "prompts/get" (first %)) @sent))))))))
  (it "prints the server for a name that it does not have, and nothing for an unknown server"
      (with-catalog (atom [])
                    (fn [_call!]
                      (let [page (:text (doc-corpus/resolve-entry "alpha.nope"))]
                        (expect (str/includes? page "has no tool `nope`"))
                        (expect (str/includes? page "`resolve-library-id` - Find a library."))
                        (expect (str/includes? page "`code-review` - Review a change."))
                        (expect (nil? (doc-corpus/resolve-entry "beta.read")))
                        (expect (nil? (doc-corpus/resolve-entry "mcp.beta.read")))))))
  (it "lists the server, its tools and its prompts for apropos without a request"
      (let [sent (atom [])]
        (with-catalog sent
                      (fn [call!]
                        (call! {} "alpha")
                        (reset! sent [])
                        (expect (= ["alpha" "alpha.resolve_library_id" "alpha.prompts.code_review"
                                    "alpha.prompts.greet"]
                                   (mapv :name (#'mcp/doc-corpus-entries))))
                        (expect (empty? @sent))))))
  (it "connects a server that nobody tried, so the first apropos has its tools"
      (with-catalog (atom [])
                    (fn [_call!]
                      (expect (some #{"alpha.resolve_library_id"}
                                    (map :name (#'mcp/doc-corpus-entries))))))))

(defdescribe
  mcp-identifier-name-test
  (it "calls the tool and gets the prompt that an identifier names"
      (let [sent (atom [])]
        (with-catalog
          sent
          (fn [call!]
            (let [answer (:result (call! {} "alpha" "resolve_library_id" {"query" "x"}))]
              (expect (= "resolve-library-id" (get answer "tool")))
              ;; The typed result of the tool comes with its content.
              (expect (= {"library" "/x/y"} (get answer "structured_content")))
              (call! {} "alpha" "prompts/get" {"name" "code_review"})
              (expect (= [["tools/call" {"name" "resolve-library-id" "arguments" {"query" "x"}}]
                          ["prompts/get" {"name" "code-review"}]]
                         (filterv #(#{"tools/call" "prompts/get"} (first %)) @sent))))))))
  (it
    "calls a tool, reads its page and finds it through the name of the server in the sandbox"
    (let [sent (atom [])]
      (with-catalog
        sent
        (fn [_call!]
          (tpc/with-own
            [ctx {}]
            (bind-verb! ctx)
            (ep/bind-ctx! ctx (servers-ctx "alpha"))
            (let
              [result
               (ep/run-python-block
                 ctx
                 (str
                   "r = await alpha.resolve_library_id(query='x')\n"
                   "print(r['tool'], r['content'][0]['text'], r['structured_content']['library'])\n"
                   "print(doc(alpha.resolve_library_id).splitlines()[0])\n"
                   "print('Review it with care.' in doc(alpha.prompts.code_review))\n"
                   "print([i.name for i in apropos('find a library')])\n"
                   "print('`resolve-library-id` - Find a library.' in doc(alpha))\n"
                   "print((await alpha.prompts['code-review']())['messages'][0]['role'])\n"
                   ;; No name of the sandbox is `mcp` or `mcp_call`.
                   "print('mcp' in globals(), 'mcp_call' in globals())\n" "import inspect\n"
                   "print(inspect.signature(alpha.resolve_library_id))\n"
                   "print(inspect.signature(alpha.prompts.greet))\n"))]
              (expect (nil? (:error result)))
              (expect (= (str "resolve-library-id ok /x/y\n"
                              "# alpha.resolve_library_id\n" "True\n"
                              "['alpha.resolve_library_id']\n" "True\n"
                              "user\n" "False False\n"
                              "(*, query: str, limit: int = ..., **arguments) -> dict\n"
                              "(*, who: str) -> dict\n")
                         (:stdout result)))))))))
  (it "leaves a name to Python and to a value of the session, and gives a freed name to the server"
      (with-catalog
        (atom [])
        (fn [_call!]
          (tpc/with-own
            [ctx {}]
            (bind-verb! ctx)
            (ep/run-python-block ctx "alpha = 5\n")
            (ep/bind-ctx! ctx (servers-ctx "alpha" "time"))
            (let [taken
                  (ep/run-python-block
                    ctx
                    (str "import time\n"
                         "print(alpha, type(time).__name__, __vis_mcp_names__, mcp_time)\n"
                         "del alpha\n"))

                  _
                  (ep/bind-ctx! ctx (servers-ctx "alpha" "time"))

                  freed
                  (ep/run-python-block
                    ctx
                    "print((await alpha.resolve_library_id(query='x'))['tool'])\n")]

              (expect (nil? (:error taken)))
              (expect (= "5 module ['mcp_time'] <mcp_time: doc(mcp_time) prints its page>\n"
                         (:stdout taken)))
              (expect (nil? (:error freed)))
              (expect (= "resolve-library-id\n" (:stdout freed))))))))
  (it "takes away the name of a server that left"
      (tpc/with-own [ctx {}]
                    (ep/bind-ctx! ctx (servers-ctx "alpha" "figma-dev"))
                    (ep/bind-ctx! ctx (servers-ctx "figma-dev"))
                    (let [result (ep/run-python-block
                                   ctx
                                   "print('alpha' in globals(), figma_dev, __vis_mcp_names__)\n")]
                      (expect (nil? (:error result)))
                      (expect (= "False <figma_dev: doc(figma_dev) prints its page> ['figma_dev']\n"
                                 (:stdout result)))))))

(defdescribe
  mcp-python-name-test
  (it "gives a server its lower-case snake name, and leaves a taken name to its owner"
      (expect (= "figma_dev" (#'mcp/python-name "figma-dev")))
      (expect (= "mcp_time" (#'mcp/python-name "time")))
      (expect (= "mcp_mcp" (#'mcp/python-name "mcp")))
      (expect (= "mcp_1password" (#'mcp/python-name "1password")))
      ;; Two servers never get one name: `mcp_time` is the server `time`.
      (expect (= "mcp_mcp_time" (#'mcp/python-name "mcp-time")))
      (expect (= "mcp_server_time" (#'mcp/python-name "mcp-server-time"))))
  (it
    "gives no server a name that Python or a new sandbox has"
    (tpc/with-own
      [ctx {}]
      (let
        [result
         (ep/run-python-block
           ctx
           (str
             "import builtins, keyword, re, sys\n"
             "names = set(sys.stdlib_module_names) | set(dir(builtins)) | set(globals())\n"
             "names |= set(keyword.kwlist) | set(keyword.softkwlist)\n"
             "print(' '.join(sorted(n for n in names if re.fullmatch('[a-z][a-z0-9_]*', n))))\n"))]
        (expect (nil? (:error result)))
        (expect (= []
                   (filterv #(= % (#'mcp/python-name %))
                     (str/split (str/trim (:stdout result)) #" "))))))))

(defdescribe
  mcp-resource-attachment-test
  (it "lists the resources of a connected server as attachments, and reads and shows one"
      (let [shown (atom [])]
        (with-catalog
          (atom [])
          (fn [call!]
            (call! {} "alpha")
            (tpc/with-own
              [ctx {}]
              (let
                [result
                 (binding [mpl-capture/*attachment-reinspection-sink* shown]
                   (ep/run-python-block
                     ctx
                     (str
                       "rows = [r for r in list_attachments() if r['source'] == 'mcp']\n"
                       "print([(r['id'], r['filename'], r['media_type']) for r in rows])\n"
                       "print(read_attachment('file:///notes.txt').decode('utf-8'))\n"
                       "print(get_attachment('notes.txt')['uri'])\n"
                       "print(show_attachment('mcp:alpha:file:///dot.png')['media_type'])\n"
                       "print(read_attachment('mcp:alpha:file:///other.txt').decode('utf-8'))\n")))]
                (expect (nil? (:error result)))
                (expect (= (str "[('mcp:alpha:file:///notes.txt', 'notes.txt', 'text/plain'),"
                                " ('mcp:alpha:file:///dot.png', 'dot.png', 'image/png')]\n"
                                "zażółć\n" "file:///notes.txt\n"
                                "image/png\n" "zażółć\n")
                           (:stdout result)))
                (expect (= [["mcp:alpha:file:///dot.png" "image/png" "iVBORw=="]]
                           (mapv (juxt :id :media-type :base64) @shown))))))))))

(defdescribe
  mcp-oauth-visibility-test
  (it
    "tells \"sign in\" apart from \"it is down\", in ctx and in every refusal"
    (let [store
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
                       #'config/load-global-yaml-config-raw (fn []
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
            ;; The server asked for more scope: only a new sign-in grants it.
            (reset! (:www-auth-atom (get (configured-servers) "signed-out"))
              "Bearer error=\"insufficient_scope\", scope=\"files:write\"")
            (let [refusal (call-failed-err env
                                           "signed-out"
                                           "x"
                                           (ex-info "MCP signed-out HTTP 403 on tools/call"
                                                    {:type :mcp/http-error :status 403}))]
              (expect (str/includes? (msg refusal) "files:write"))
              (expect (str/includes? (hint refusal) "/auth/start")))
            (finally (reset! conns {}) (reset! killed #{}))))))))

(defdescribe
  mcp-string-keyed-surfaces-test
  (it
    "answers EVERY surface with string keys - no keyword may reach the model, a client, or the wire"
    (let [store
          (atom {"mcp" {"servers" {"alpha" {"transport" "stdio" "command" "echo" "env" {"S" "x"}}
                                   "remote" {"transport" "streamable_http"
                                             "url" "https://remote.example.test/mcp"}}}})

          resolve*
          (fn [sym]
            (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core sym))

          conns
          @(resolve* 'conns)

          killed
          @(resolve* 'killed)

          reconcile!
          (resolve* 'reconcile!)

          reconcile-async!
          (resolve* 'reconcile-async!)

          call!
          (resolve* 'mcp-call-impl)

          contribute
          (resolve* 'contribute)

          env
          {:session-id "s1"}

          ;; `:message` and `:hint` are the ENGINE's failure contract, shared by every
          ;; extension; everything MCP itself carries alongside them has to be a string.
          payload
          (fn [v]
            (if (and (map? v) (or (contains? v :result) (contains? v :error)))
              [(:result v) (dissoc (:error v) :message :hint)]
              [v]))

          keywords-in
          (fn keywords-in [x path]
            (cond (map? x) (mapcat (fn [[k v]]
                                     (concat (when (keyword? k) [(conj path k)])
                                             (keywords-in v (conj path k))))
                                   x)
                  (sequential? x) (apply concat
                                    (map-indexed (fn [i v]
                                                   (keywords-in v (conj path i)))
                                                 x))
                  (keyword? x) [(conj path x)]
                  :else nil))]

      (with-redefs-fn {#'config/load-global-config-raw (fn []
                                                         @store)
                       #'config/load-global-yaml-config-raw (fn []
                                                              @store)
                       #'config/save-config! (fn [value _source]
                                               (reset! store value)
                                               nil)
                       #'client/connect (fn [_name _spec]
                                          {:transport :stdio :tools (atom nil)})
                       #'client/list-tools (constantly [{"name" "write_file"
                                                         "description" "Write it"
                                                         "inputSchema" {"type" "object"}}])
                       #'client/alive? (constantly true)
                       #'client/close (constantly nil)
                       #'client/call-tool (fn [_conn _tool _args & _]
                                            {"content" [{"text" "ok"}] "isError" true})
                       reconcile-async! (constantly nil)}
        (fn []
          (try
            (reset! conns {})
            (reset! killed #{})
            (reconcile!)
            (let [surfaces
                  [(mcp/gateway-servers)
                   (mcp/save-gateway-server! "beta"
                                             {"transport" "stdio"
                                              "command" "ls"
                                              "args" ["-l"]
                                              "cwd" "/tmp"
                                              "timeout_ms" 900})
                   (mcp/set-gateway-server-enabled! "beta" false) (mcp/kill-gateway-server! "beta")
                   (mcp/start-gateway-server! "beta") (mcp/delete-gateway-server! "beta")
                   (mcp/test-gateway-server! "probe" {"transport" "stdio" "command" "echo"})
                   (mcp/gateway-server-auth-status "remote")
                   (mcp/set-session-servers! "s1" {"sess" {"transport" "stdio" "command" "echo"}})
                   (mcp/session-servers "s1") (contribute env)
                   ;; The model-facing verb, in every shape it can land in.
                   (call! env "alpha") (call! env "alpha" "write_file" {"path" "p"})
                   (call! env "alpha" "nope" {}) (call! env "ghost")
                   (oauth/token-status "no-such-server")
                   (oauth/cancel-authorization! "no-such-flow")]

                  values
                  (vec (mapcat payload surfaces))]

              (expect (= [] (vec (mapcat #(keywords-in % []) values))))
              ;; Already snake_case strings, so the wire encoder has NOTHING left to
              ;; rewrite: what a surface returns is exactly what a client receives.
              (expect (= values (mapv wire/->wire values))))
            (finally (mcp/clear-session-servers! "s1") (reset! conns {}) (reset! killed #{}))))))))

(defdescribe auth-backoff-test
             ;; Regression, gateway CPU audit: a JFR profile of a live gateway caught one
             ;; unauthorized server being re-asked every couple of seconds - the per-turn
             ;; reconcile, the handlers' nudges and the health loop each paying an HTTP
             ;; round trip for the same 401 that only a human sign-in can change.
             (it
               "asks a server awaiting sign-in once per window, again on start or a spec change"
               (let [connects
                     (atom 0)

                     spec-a
                     {"transport" "streamable_http" "url" "https://mcp.example.test/mcp"}

                     spec-b
                     {"transport" "streamable_http" "url" "https://mcp.example.test/v2/mcp"}

                     cfg
                     (atom {"mcp" {"servers" {"remote" spec-a}}})

                     ensure!
                     (var-get #'mcp/ensure-connected!)]

                 (reset! (var-get #'mcp/auth-backoff) {})
                 (try (with-redefs-fn {#'config/load-global-yaml-config-raw (fn []
                                                                              @cfg)
                                       #'client/connect (fn [_ _]
                                                          (swap! connects inc)
                                                          (throw (ex-info "401"
                                                                          {:type :mcp/oauth-required
                                                                           :server "remote"})))}
                        (fn []
                          (expect (nil? (ensure! nil "remote")))
                          (expect (nil? (ensure! nil "remote")))
                          (expect (nil? (ensure! nil "remote")))
                          (expect (= 1 @connects) "one refusal, then silence for the window")
                          ;; A human pressed start: ask now.
                          ((var-get #'mcp/revive!) "remote")
                          (expect (nil? (ensure! nil "remote")))
                          (expect (= 2 @connects))
                          ;; The server was edited: a new spec is a new question.
                          (reset! cfg {"mcp" {"servers" {"remote" spec-b}}})
                          (expect (nil? (ensure! nil "remote")))
                          (expect (= 3 @connects))
                          (expect (nil? (ensure! nil "remote")))
                          (expect (= 3 @connects))))
                      (finally (reset! (var-get #'mcp/auth-backoff) {})
                               (reset! (var-get #'mcp/servers-cache) {:hash ::none :value {}}))))))
