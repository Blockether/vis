(ns com.blockether.vis.internal.foundation.mcp.core-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.foundation.mcp.client :as client]
            [com.blockether.vis.internal.foundation.mcp.core :as mcp]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe mcp-native-contract-test
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
                   (expect (= "object"
                              (get-in call [:ext.symbol/schema :properties "args" :type]))))))

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
              (expect (= "filesystem" (:name row)))
              (expect (= "stdio" (:transport row)))
              (expect (nil? (:env row)))
              (expect (nil? (:headers row)))
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
            (expect (= "remote" (:name result)))
            (expect (= [{:name "list_files" :description "Enumerate files"}] (:tools result)))
            (expect (nil? (:headers result)))
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
            (let [rows (:servers (mcp/gateway-servers))]
              (expect (= ["owned" "team"] (mapv :name rows)))
              (expect (= [true false] (mapv :is-managed rows))))))
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

      (with-redefs-fn {#'config/load-global-config-raw (fn [] @store)
                       #'config/load-config-raw (fn [] @store)
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
                 (expect (true? (:is-killed row)))
                 (expect (false? (:is-connected row))))
               (expect (empty? @conns))
               ;; The brake has to survive the per-turn reconcile: closing the
               ;; connection alone let the very next turn respawn the stdio child
               ;; the user just killed.
               (reconcile!)
               (expect (= ["local"] @connects))
               (let [row (mcp/start-gateway-server! "local")]
                 (expect (false? (:is-killed row)))
                 (expect (true? (:is-connected row))))
               (expect (= ["local" "local"] @connects))
               ;; A kill is a RUNTIME brake, never an edit to anybody's config.
               (expect (= {"transport" "stdio" "command" "echo"}
                          (get-in @store ["mcp" "servers" "local"])))
               (finally (reset! conns {}) (reset! killed #{})))))))
  (it
    "refuses OAuth where it cannot apply and answers unknown flows as such"
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
          (expect (:is-cancelled (mcp/cancel-gateway-server-auth! "gone")))))))
  (it
    "takes the authorization code from a pasted redirect URL or a bare code"
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
      (expect (= :mcp/oauth-error (thrown #(code-of "   ")))))))
