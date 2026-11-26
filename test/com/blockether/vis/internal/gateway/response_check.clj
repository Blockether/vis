(ns com.blockether.vis.internal.gateway.response-check
  "A test hook that checks each JSON answer of the gateway against the response
   schema of its operation (#487). The Lazytest hook [[answers]] of the `:test`
   alias checks the answers of each gateway and contract test namespace: the answers
   of a router, and the answers of a handler that a test calls directly. After a run
   that has each gateway test namespace, it fails the run when a JSON operation has
   no checked answer and no fixture answer, and is not in [[unchecked-operations]]. It
   also fails the run when an operation has only a fixture answer and is not in
   [[fixture-only-operations]]. Use [[checked]] in `around-each` for a router outside this hook."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.contract.wire :as wire]
            [lazytest.hooks :as hooks]
            [lazytest.test-case :as test-case]
            [reitit.core :as reitit]
            [reitit.ring :as rr]
            [ring.core.protocols :as ring-protocols]))

(defonce ^{:doc "The `[method path]` operations whose answers the hook checked."} checked-operations
  (atom #{}))

(defn- json-answer?
  [response]
  (and (<= 200 (long (or (:status response) 0)) 299)
       (some-> (get-in response [:headers "Content-Type"])
               str/lower-case
               (str/includes? "application/json"))))

(defn- buffered
  "The `response` with its body read into a string, and that string."
  [response]
  (let [out (java.io.ByteArrayOutputStream.)]
    (ring-protocols/write-body-to-stream (:body response) response out)
    (let [text (.toString out "UTF-8")]
      [(assoc response :body text) text])))

(defn- check-operation!
  "Throw when the JSON answer `response` of the contract operation `[method path]`
   does not match its response schema. Record the operation when it matches."
  [[method path] response]
  (let [{:keys [response-schema] :as operation} (gateway-contract/operation method path)]
    (if
      (and response-schema (= :json (:response operation)) (map? response) (json-answer? response))
      (let [[response text]
            (if (string? (:body response)) [response (:body response)] (buffered response))
            body (wire/parse-json text)
            errors (document/explain-json "gateway" (name response-schema) body)]

        (when errors
          (throw (ex-info (str "The answer of " (str/upper-case (name method))
                               " " path
                               " does not match the gateway schema " (name response-schema))
                          {:operation [method path] :errors errors :body body})))
        (swap! checked-operations conj [method path])
        response)
      response)))

(defn- check!
  "Throw when the JSON answer of `request` does not match its operation schema."
  [router request response]
  (if-let [path (some-> (reitit/match-by-path router (:uri request))
                        :template)]
    (check-operation! [(:request-method request) path] response)
    response))

(defn checked
  "Run `f` while each handler that `reitit.ring/ring-handler` makes checks its
   JSON answers against the gateway response schemas."
  [f]
  (let [ring-handler rr/ring-handler]
    (with-redefs [rr/ring-handler (fn [router & more]
                                    (let [handler (apply ring-handler router more)]
                                      (fn ([request] (check! router request (handler request)))
                                        ([request respond raise] (handler request respond
                                                                   raise)))))]
      (f))))

(def unchecked-operations
  "The JSON operations that no test answer and no fixture checks, each with its
   reason. Only an operation that needs an outside service can be here. This map can
   only get smaller: the run fails when an entry gets a checked answer."
  {[:delete "/v1/devices/:token"] "needs the push relay and a paired device"
   [:delete "/v1/sessions/:sid/speech/jobs/:job-id"]
   "needs a local speech or voice model and a running job"
   [:delete "/v1/sessions/:sid/turns/:tid"] "needs a running engine turn in the registry"
   [:delete "/v1/sessions/:sid/voice/jobs/:job-id"]
   "needs a local speech or voice model and a running job"
   [:get "/v1/automations/runs/:run-id"] "needs a saved automation and an engine run"
   [:get "/v1/decisions/aliases/:alias"] "needs an installed decision model"
   [:get "/v1/decisions/models/:model-ref"] "needs an installed decision model"
   [:get "/v1/sessions/:sid/agents/:child-id/timeline"] "needs a running engine turn with subagents"
   [:get "/v1/sessions/:sid/client-calls"]
   "needs a registered client extension with a pending engine call"
   [:get "/v1/sessions/:sid/context"] "needs a live engine session context"
   [:get "/v1/sessions/:sid/speech/jobs/:job-id"]
   "needs a local speech or voice model and a running job"
   [:get "/v1/sessions/:sid/voice/jobs/:job-id"]
   "needs a local speech or voice model and a running job"
   [:get "/v1/speech/jobs/:job-id"] "needs a local speech or voice model and a running job"
   [:get "/v1/voice/jobs/:job-id"] "needs a local speech or voice model and a running job"
   [:patch "/v1/speech/voices/:voice-id"] "needs a local speech or voice model and a running job"
   [:post "/v1/apps/:view-id/actions"] "needs a live app view from an extension"
   [:post "/v1/automations/:automation-id/run"] "needs a saved automation and an engine run"
   [:post "/v1/devices"] "needs the push relay and a paired device"
   [:post "/v1/devices/actions/test"] "needs the push relay and a paired device"
   [:post "/v1/evals/leaderboard"] "needs eval results from model runs"
   [:post "/v1/extensions/reload"] "needs a loaded extension set and an engine reload"
   [:post "/v1/mcp/servers"] "needs a running MCP server process"
   [:post "/v1/mcp/servers/:name/actions/kill"] "needs a running MCP server process"
   [:post "/v1/mcp/servers/:name/actions/start"] "needs a running MCP server process"
   [:post "/v1/providers/:provider-id/auth/complete"]
   "needs a provider sign-in flow with an outside service"
   [:post "/v1/providers/:provider-id/logout"]
   "needs a provider sign-in flow with an outside service"
   [:post "/v1/sessions/:sid/agents"] "needs a running engine turn with subagents"
   [:post "/v1/sessions/:sid/agents/answer"] "needs a running engine turn with subagents"
   [:post "/v1/sessions/:sid/agents/deny"] "needs a running engine turn with subagents"
   [:post "/v1/sessions/:sid/agents/extend"] "needs a running engine turn with subagents"
   [:post "/v1/sessions/:sid/agents/grant"] "needs a running engine turn with subagents"
   [:post "/v1/sessions/:sid/cancel-current"] "needs a running engine turn in the registry"
   [:post "/v1/sessions/:sid/client-calls/:call_id/activity"]
   "needs a registered client extension with a pending engine call"
   [:post "/v1/sessions/:sid/client-calls/:call_id/result"]
   "needs a registered client extension with a pending engine call"
   [:post "/v1/sessions/:sid/speech"] "needs a local speech or voice model and a running job"
   [:post "/v1/sessions/:sid/turns/:tid/cancel"] "needs a running engine turn in the registry"
   [:post "/v1/sessions/:sid/voice"] "needs a local speech or voice model and a running job"
   [:post "/v1/settings/:id/preview"] "needs an extension setting with a preview function"
   [:post "/v1/systemone"] "needs an installed decision model"
   [:put "/v1/mcp/servers/:name"] "needs a running MCP server process"})

(def fixture-only-operations
  "The JSON operations that have a fixture answer but no checked test answer, each
   with its reason. A handler change on them is not found. Only an operation that needs
   an outside service can be here. This map can only get smaller: the run fails when an
   entry gets a checked answer."
  {})

(def ^:private gateway-test-prefix "com.blockether.vis.internal.gateway.")

(def ^:private checked-prefixes
  "The test namespaces whose answers the hook checks."
  [gateway-test-prefix "com.blockether.vis.contract."])

(defonce ^:private active? (atom false))

(defonce ^:private originals (atom nil))

(defn- request-operation
  "The `[method path]` of `request`: the template of the route that a router matched,
   else the one operation of `operations` with the method of the request. A direct call
   without a method names an operation only when the handler serves one."
  [request operations]
  (let [method (:request-method request)]
    (if-let [path (some-> (:reitit.core/match request)
                          :template)]
      [method path]
      (let [candidates (if method (filterv #(= method (first %)) operations) operations)]
        (when (= 1 (count candidates)) (first candidates))))))

(defn- wrap-handler
  "A handler that checks the synchronous JSON answer of `handler`, which serves
   `operations`."
  [operations handler]
  (fn ([request] (let [response
                       (handler request)

                       operation
                       (when @active? (request-operation request operations))]

                   (if operation (check-operation! operation response) response))) ([request respond
                                                                                     raise]
                                                                                    (handler request
                                                                                      respond
                                                                                      raise))))

(defn- handler-map-vars
  "The vars of the handler maps that the built-in router binds."
  []
  (let [route-handlers @(requiring-resolve
                          'com.blockether.vis.internal.gateway.server/route-handlers)]
    (vec (for [m route-handlers]
           (or (some (fn [the-ns]
                       (some (fn [[_ v]]
                               (when (and (bound? v) (identical? m @v)) v))
                             (ns-interns the-ns)))
                     (all-ns))
               (throw (ex-info "A gateway handler map has no var" {:operations (keys m)})))))))

(defn- install!
  "Replace each built-in handler with a checking one: in its handler map, in the
   router's list of maps and in the var that names it. Each handler gets one wrapper,
   so a test that compares a var with a router entry still sees one function."
  []
  (when-not @originals
    (let [route-var
          (requiring-resolve 'com.blockether.vis.internal.gateway.server/route-handlers)

          map-vars
          (handler-map-vars)

          served
          (reduce (fn [acc [operation handler]]
                    (update acc handler (fnil conj []) operation))
                  {}
                  (mapcat deref map-vars))

          wrapped
          (into {}
                (for [[handler operations] served]
                  [handler (wrap-handler operations handler)]))

          saved
          (atom {route-var @route-var})]

      (doseq [map-var
              map-vars

              [_ v]
              (ns-interns (:ns (meta map-var)))

              :when (and (bound? v) (contains? wrapped @v) (not (contains? @saved v)))]

        (swap! saved assoc v @v)
        (alter-var-root v (constantly (get wrapped @v))))
      (doseq [map-var map-vars]
        (swap! saved assoc map-var @map-var)
        (alter-var-root map-var
                        (fn [m]
                          (update-vals m wrapped))))
      (alter-var-root route-var (constantly (mapv deref map-vars)))
      (reset! originals @saved))))

(defn- uninstall!
  []
  (doseq [[v root] @originals]
    (alter-var-root v (constantly root)))
  (reset! originals nil))

(defn- fixture-operations
  "The `[method path]` operations that `fixtures/gateway-answers.json` has an answer for."
  []
  (set (for [k
             (keys (get (wire/parse-json (slurp (io/resource
                                                  "vis-contract/fixtures/gateway-answers.json")))
                        "answers"))

             :let [[method path]
                   (str/split k #" " 2)]]

         [(keyword (str/lower-case method)) path])))

(defn- json-operations
  "The `[method path]` operations of the contract that answer JSON."
  []
  (set (for [{:keys [path operations]}
             gateway-contract/route-table

             [method {:keys [response]}]
             operations

             :when (= :json response)]

         [method path])))

(defn uncovered-operations
  "The JSON operations that have no checked answer in `checked` and no fixture answer."
  [checked]
  (let [fixtures (fixture-operations)]
    (set (remove #(or (contains? checked %) (contains? fixtures %)) (json-operations)))))

(defn fixture-only-operations-of
  "The JSON operations that have a fixture answer and no checked answer in `checked`.
   A handler change on these operations is not found by a test."
  [checked]
  (set (remove #(contains? checked %) (filter (fixture-operations) (json-operations)))))

(defn- test-namespaces-on-disk
  "The names of the gateway test namespaces under `test/`."
  []
  (let [dir
        (io/file "test/com/blockether/vis/internal/gateway")

        root
        (.toPath dir)]

    (set (for [^java.io.File f
               (file-seq dir)

               :let [path
                     (str (.relativize root (.toPath f)))]
               :when (str/ends-with? path "_test.clj")]

           (str gateway-test-prefix
                (-> (subs path 0 (- (count path) (count ".clj")))
                    (str/replace "/" ".")
                    (str/replace "_" "-")))))))

(defn- run-namespaces
  [run]
  (set (keep (fn [suite]
               (some-> (:doc suite)
                       str))
             (:children run))))

(defn- list-failure
  "A message when the operation set `actual` and the keys of the list `listed` do not
   agree, or nil."
  [list-name actual listed missing-text]
  (let [listed
        (set (keys listed))

        missing
        (sort (remove listed actual))

        stale
        (sort (remove actual listed))]

    (when (or (seq missing) (seq stale))
      (str "Gateway answer coverage (#487) does not agree with `"
           list-name
           "`."
           (when (seq missing) (str " " missing-text ": " (pr-str (vec missing)) "."))
           (when (seq stale)
             (str " These listed operations have a checked answer now; remove them: "
                  (pr-str (vec stale))
                  "."))))))

(defn coverage-failure
  "A message when the checked operations do not agree with the lists
   [[unchecked-operations]] and [[fixture-only-operations]], or nil."
  [checked]
  (some->> [(list-failure "unchecked-operations"
                          (uncovered-operations checked)
                          unchecked-operations
                          "These JSON operations have no checked answer")
            (list-failure "fixture-only-operations"
                          (fixture-only-operations-of checked)
                          fixture-only-operations
                          "These JSON operations have only a fixture answer")]
           (remove nil?)
           seq
           (str/join " ")))

(defn- checked-namespace?
  [suite]
  (let [n (str (:doc suite))]
    (some #(str/starts-with? n %) checked-prefixes)))

(hooks/defhook
  answers
  "Check the JSON answers of the gateway in the gateway and contract test namespaces.
   After a run with each gateway test namespace, fail the run when the answer
   coverage does not agree with `unchecked-operations`."
  (pre-test-run
    [_config m]
    (when (some checked-namespace? (:children m)) (reset! checked-operations #{}) (install!))
    m)
  (pre-test-suite [_config suite]
                  (when (= :lazytest/ns (:type suite))
                    (reset! active? (boolean (checked-namespace? suite))))
                  suite)
  (post-test-run [config results]
                 (reset! active? false)
                 (when @originals (uninstall!))
                 (let [on-disk (test-namespaces-on-disk)]
                   (if-let [message (and (seq on-disk)
                                         (every? (run-namespaces results) on-disk)
                                         (empty? (:var-filter config))
                                         (coverage-failure @checked-operations))]
                     (do (println (str "\nFAIL " message))
                         (update results
                                 :children
                                 conj
                                 (test-case/test-case-result
                                   :fail
                                   (test-case/test-case
                                     {:doc "gateway answer coverage"
                                      :ns (the-ns
                                            'com.blockether.vis.internal.gateway.response-check)})
                                   (ex-info message {}))))
                     results))))
