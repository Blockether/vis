(ns com.blockether.vis.mcp-conformance
  "The client that the official MCP client conformance suite tests: the MCP client
   of Vis with its real configuration and OAuth code. `bin/mcp-conformance` runs
   the suite. The suite starts this client one time for each scenario, with the
   server URL as the last argument, the scenario in MCP_CONFORMANCE_SCENARIO and
   the data of the scenario in MCP_CONFORMANCE_CONTEXT."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.mcp.client :as client]
            [com.blockether.vis.internal.foundation.mcp.core :as core]
            [com.blockether.vis.internal.foundation.mcp.oauth :as oauth])
  (:import (java.net URI)))

(def ^:private server-name "conformance")

(def ^:private max-sign-ins 3)

(defn- log
  [& parts]
  (binding [*out* *err*]
    (println "[vis-conformance]" (str/join " " parts))))

(defn- context
  []
  (let [raw (System/getenv "MCP_CONFORMANCE_CONTEXT")]
    (if (seq raw) (json/read-json raw) {})))

(def ^:private no-redirects (delay (http/client {:follow-redirects :never})))

(defn- browser!
  "Do what a browser does with the authorization URL when the person approves at
   once: follow the redirect to the callback of Vis."
  [url]
  (let [response
        (http/request {:uri url :method :get :throw false :client @no-redirects})

        location
        (get-in response [:headers "location"])]

    (when-not location
      (throw (ex-info
               (str "The authorization endpoint answered " (:status response) " without a redirect")
               {})))
    (http/request
      {:uri (str (.resolve (URI/create url) ^String location)) :method :get :throw false})))

(defn- sign-in!
  "Sign in as the gateway does, with the simulated browser as the person."
  [spec]
  (let [flow (oauth/start-authorization! server-name
                                         (:url spec)
                                         {:www-auth (some-> (:www-auth-atom spec)
                                                            deref)
                                          :auth-hint (:auth spec)
                                          :callback-mode nil})]
    (browser! (get flow "url"))
    (loop [polls 0]
      (let [row (oauth/poll-authorization! (get flow "flow_id"))]
        (case (get row "status")
          "ok"
          row

          "error"
          (throw (ex-info (str "Sign-in failed: " (get row "error")) {}))

          (if (> polls 150)
            (throw (ex-info "Sign-in did not finish" {}))
            (do (Thread/sleep 100) (recur (inc polls)))))))))

(defn- wants-sign-in?
  "True when a person can change `failure`. A grant that needs no person gets its
   token by itself: its refusal is the answer."
  [spec failure]
  (let [chain (take-while some? (iterate ex-cause failure))]
    (boolean (and (not (oauth/machine-grant? (:auth spec)))
                  (or (some #(= :mcp/oauth-required (:type (ex-data %))) chain)
                      (and (some-> (:www-auth-atom spec)
                                   deref)
                           (some #(#{401 403} (:status (ex-data %))) chain)))))))

(defn- with-sign-in
  "The result of `(f)`. When the server asks for a sign-in, sign in and call `f`
   again, at most `max-sign-ins` times."
  [spec sign-ins f]
  (loop []

    (let [outcome (try {:result (f)} (catch Throwable failure {:failure failure}))]
      (if-let [failure (:failure outcome)]
        (do (when-not (wants-sign-in? spec failure) (throw failure))
            (when (>= (long @sign-ins) (long max-sign-ins))
              (throw
                (ex-info (str "Still requires sign-in after " @sign-ins " sign-ins") {} failure)))
            (swap! sign-ins inc)
            (sign-in! spec)
            (recur))
        (:result outcome)))))

(defn- connection!
  "The live connection in `conn*`. Like the pool of Vis, it closes a dead
   connection and connects again."
  [spec conn*]
  (when-not (some-> @conn*
                    client/alive?)
    (some-> @conn*
            client/close)
    (reset! conn* (client/connect server-name spec)))
  @conn*)

(defn- tool-calls
  "The tool calls that `scenario` wants, each as `[tool arguments]`."
  [scenario tools]
  (cond (str/starts-with? scenario "auth/") [["test-tool" {}]]
        (= scenario "initialize") []
        (= scenario "tools_call") [["add_numbers" {"a" 2 "b" 3}]]
        (= scenario "sse-retry") [["test_reconnection" {}]]
        (= scenario "elicitation-sep1034-client-defaults") [["test_client_elicitation_defaults" {}]]
        ;; The list of the tools is the test.
        (= scenario "request-metadata") []
        (= scenario "sep-2322-client-request-state")
        [["test_mrtr_echo_state" {}] ["test_mrtr_no_state" {}] ["test_mrtr_unrelated" {}]
         ["test_mrtr_no_result_type" {}]]
        (= scenario "http-custom-headers") (mapv (fn [call]
                                                   [(get call "name") (get call "arguments")])
                                                 (get (context) "toolCalls"))
        ;; Each tool that the client lists: it must leave out a tool with a bad header.
        (contains? #{"http-standard-headers" "http-invalid-tool-headers"} scenario)
        (mapv (fn [tool]
                [(get tool "name") {"region" "us-west1"}])
              tools)
        (= scenario "json-schema-2020-12-preservation")
        (let [tool (or (some #(when (= "json_schema_2020_12_tool" (get % "name")) %) tools)
                       (throw (ex-info "json_schema_2020_12_tool was not listed" {})))]
          [["json_schema_echo" {"schema" (get tool "inputSchema")}]])
        :else (throw (ex-info (str "Unknown scenario " scenario) {}))))

(defn- auth-of
  "The `auth` block of the configuration for `scenario`, from the data of the
   scenario. Like the reference client of the suite: the client that the scenario
   registered before, else the client document of the suite."
  [scenario]
  (let [{client-id "client_id" client-secret "client_secret" :as data}
        (context)

        client
        (cond-> {}
          (string? client-id)
          (assoc "client_id" client-id)

          (string? client-secret)
          (assoc "client_secret" client-secret))]

    (cond (str/starts-with? scenario "auth/client-credentials-")
          (cond-> (assoc client "grant" "client_credentials")
            (get data "private_key_pem")
            (assoc "private_key"
              (get data "private_key_pem") "signing_algorithm"
              (get data "signing_algorithm")))
          (= scenario "auth/wif-jwt-bearer") (assoc client
                                               "grant" "jwt_bearer"
                                               "assertion" (get data "valid_jwt"))
          (= scenario "auth/enterprise-managed-authorization")
          (merge client
                 {"grant" "enterprise_managed"}
                 (select-keys data
                              ["idp_client_id" "idp_id_token" "idp_issuer" "idp_token_endpoint"]))
          (seq client) client
          :else {"client_metadata_url" "https://conformance-test.local/client-metadata.json"})))

(defn- run-scenario!
  [url scenario]
  (let [auth
        (auth-of scenario)

        spec
        (#'core/->client-spec
         server-name
         {:transport "streamable_http" :url url :timeout_ms 20000 :auth auth})

        conn*
        (atom nil)

        sign-ins
        (atom 0)]

    (try (let [tools (with-sign-in spec sign-ins #(client/list-tools (connection! spec conn*)))]
           (doseq [[tool arguments] (tool-calls scenario tools)]
             (let [result (with-sign-in spec
                                        sign-ins
                                        #(client/call-tool (connection! spec conn*)
                                                           tool
                                                           arguments
                                                           20000
                                                           ;; The person sends the form as it is.
                                                           {:on-elicitation (constantly
                                                                              {"action" "accept"
                                                                               "content" {}})}))]
               (when (get result "isError")
                 (throw (ex-info (str "Tool " tool
                                      " failed: " (json/write-json-str (get result "content")))
                                 {}))))))
         (finally (some-> @conn*
                          client/close)))))

(defn -main
  [& arguments]
  (let [url
        (last arguments)

        scenario
        (System/getenv "MCP_CONFORMANCE_SCENARIO")

        failure
        (try (when-not (and url scenario)
               (throw (ex-info "Usage: MCP_CONFORMANCE_SCENARIO=<scenario> <server URL>" {})))
             (run-scenario! url scenario)
             nil
             (catch Throwable failure failure))]

    (when failure
      (log "error:"
           (str/join " <- "
                     (map (fn [cause]
                            (str (ex-message cause)
                                 (when-let [data (not-empty (dissoc (ex-data cause)
                                                              :body
                                                              :www-authenticate))]
                                   (str " " (pr-str data)))))
                          (take-while some? (iterate ex-cause failure))))))
    (shutdown-agents)
    (System/exit (if failure 1 0))))
