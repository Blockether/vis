(ns com.blockether.vis.internal.gateway.server-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.gateway.client :as client]
            [com.blockether.vis.internal.gateway.discovery :as discovery]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.resources :as resources]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.loop :as lp]
            [reitit.ring :as rr]
            [ring.middleware.params :as ring-params]))

(defn- rv
  "Resolve a (possibly private) var in the server namespace for with-redefs-fn."
  [sym]
  (ns-resolve 'com.blockether.vis.internal.gateway.server sym))

(defn- server-state [] @(rv 'server-state))

(defn- with-server-state!
  [m f]
  (let [state-atom (server-state)]
    (reset! state-atom m)
    (try (f) (finally (reset! state-atom nil)))))

(defn- with-stop-stub!
  [stops extra f]
  (with-redefs-fn (merge {#'state/running-turn-count (constantly 0)
                          #'server/stop! (fn []
                                           (swap! stops inc))}
                         extra)
    f))

(defn- wait-until
  [pred]
  (loop [remaining 20]
    (cond (pred) true
          (zero? remaining) false
          :else (do (Thread/sleep 10) (recur (dec remaining))))))

(deftest gateway-router-compiles-with-project-action-routes
  (testing "static project actions do not conflict with the dynamic project-id route"
    (is (some? ((rv 'router) "test-token" [])))))

(deftest foreground-daemon-does-not-refcount-stop
  (testing "a manually-run `vis gateway start` is user-owned, not client-refcounted"
    (let [stops (atom 0)]
      (with-stop-stub! stops
                       {}
                       (fn []
                         (with-server-state! {:managed? false
                                              :saw-client? true
                                              :started-at-ms (System/currentTimeMillis)
                                              :clients {}
                                              :sse-clients #{}}
                                             (fn []
                                               ((rv 'maybe-stop-when-idle!))
                                               (Thread/sleep 80)
                                               (is (zero? @stops)))))))))

(deftest managed-daemon-stops-when-last-client-is-gone
  (testing "an auto-spawned gateway self-reaps once there are no clients and no turn"
    (let [stops (atom 0)]
      (with-stop-stub! stops
                       {}
                       (fn []
                         (with-server-state! {:managed? true
                                              :saw-client? true
                                              :started-at-ms (System/currentTimeMillis)
                                              :clients {}
                                              :sse-clients #{}}
                                             (fn []
                                               ((rv 'maybe-stop-when-idle!))
                                               (is (wait-until #(= 1 @stops))))))))))

(deftest killed-client-lease-does-not-pin-managed-daemon
  (testing "dead recorded client pids are ignored, so SIGKILLed TUIs still let the daemon die"
    (let [stops (atom 0)]
      (with-stop-stub! stops
                       {#'discovery/pid-alive? (constantly false)}
                       (fn []
                         (with-server-state! {:managed? true
                                              :saw-client? true
                                              :started-at-ms (System/currentTimeMillis)
                                              :clients {"c1" {:pid 12345 :kind "clojure-client"}}
                                              :sse-clients #{}}
                                             (fn []
                                               ((rv 'maybe-stop-when-idle!))
                                               (is (wait-until #(= 1 @stops))))))))))

(deftest managed-daemon-gets-startup-grace-before-first-client
  (testing "the daemon does not exit in the gap between self-registration and first client lease"
    (let [stops (atom 0)]
      (with-stop-stub! stops
                       {}
                       (fn []
                         (with-server-state! {:managed? true
                                              :saw-client? false
                                              :started-at-ms (System/currentTimeMillis)
                                              :clients {}
                                              :sse-clients #{}}
                                             (fn []
                                               ((rv 'maybe-stop-when-idle!))
                                               (Thread/sleep 80)
                                               (is (zero? @stops)))))))))

;; ── daemon-side Resources start (the F4 "add background" flow over the gateway) ──
;; A resource started by the agent/TUI must register in the DAEMON's registry so
;; every channel's Resources list shows it. These lock the two handlers that make
;; that work: GET startables (serialize the declarative registry with options the
;; daemon proposes from its env) and POST start (run the chosen :start-fn HERE).

(defn- body-stream
  [m]
  (java.io.ByteArrayInputStream. (.getBytes ^String (wire/json-str m) "UTF-8")))

(def ^:private fake-startables
  ;; one options-based (nREPL) + one fields-based (MCP) startable
  [{:kind :nrepl
    :label "nREPL"
    :dir? true
    :options-label "aliases"
    :options-fn (fn [_env]
                  [":dev" ":test"])
    :start-fn (fn [_ _]
                :ignored)}
   {:kind :mcp-stdio
    :label "Add MCP (stdio)"
    :dir? false
    :fields [{:name :cmd :label "Command" :required true} {:name :cwd :label "Directory"}]
    :start-fn (fn [_ _]
                :ignored)}])

(defn- with-startables!
  "Run `f` with the registry + session env stubbed to `starts` (a startable vec)."
  [starts f]
  (with-redefs-fn {#'extension/registered-startable-resources (constantly starts)
                   #'lp/env-for (constantly {:workspace/root "/tmp/demo"})}
    f))

(deftest startables-handler-serializes-registry-with-proposed-options
  (testing
    "GET startables returns wire descriptors a remote UI can render, options proposed daemon-side"
    (with-startables! fake-startables
                      (fn []
                        (let [resp
                              ((rv 'startables-handler) {:path-params {:sid (str (random-uuid))}})

                              ;; the wire IS the canonical shape: snake_case STRING keys
                              startables
                              (get (wire/parse-json (:body resp)) "startables")

                              nrepl
                              (first startables)

                              mcp
                              (second startables)]

                          (is (= 200 (:status resp)))
                          ;; nREPL: dir + options proposed from env, hyphenated key survives the wire
                          (is (= "nrepl" (get nrepl "kind")))
                          (is (true? (get nrepl "is_dir")))
                          (is (true? (get nrepl "is_options")))
                          (is (= "aliases" (get nrepl "options_label")))
                          (is (= [":dev" ":test"] (get nrepl "options")))
                          ;; MCP: declared fields ride across, no options
                          (is (= "mcp-stdio" (get mcp "kind")))
                          (is (nil? (get mcp "is_options")))
                          (is (= 2 (count (get mcp "fields"))))
                          ;; the non-serializable fns never cross the wire
                          (is (not (contains? nrepl "start_fn")))
                          (is (not (contains? nrepl "options_fn"))))))))

(deftest resource-start-handler-runs-start-fn-daemon-side
  (testing "POST start resolves the startable and runs its :start-fn in the daemon"
    (let [started
          (atom [])

          starts
          [{:kind :nrepl
            :label "nREPL"
            :dir? true
            :start-fn (fn [env selected]
                        (swap! started conj [:nrepl (:startable/dir env) (vec selected)]))}
           {:kind :mcp-stdio
            :label "Add MCP (stdio)"
            :start-fn (fn [_env fields]
                        (swap! started conj [:mcp fields]))}
           {:kind :boom
            :label "Boom"
            :start-fn (fn [_ _]
                        (throw (ex-info "nope" {})))}]

          sid
          (str (random-uuid))

          call
          (fn [m]
            (with-startables! starts
                              (fn []
                                ((rv 'resource-start-handler)
                                  {:path-params {:sid sid} :body (body-stream m)}))))]

      ;; The daemon runs :start-fn OFF the request thread (a REPL/nREPL boot is
      ;; slow and would block the POST past the client timeout), so the handler
      ;; answers "starting" immediately and the effect lands asynchronously.
      (testing "options-based start threads the chosen dir + selected values (async)"
        (let [resp (call {:kind "nrepl" :dir "/tmp/x" :selected [":dev" ":test"]})]
          (is (= 200 (:status resp)))
          (is (= "starting" (get (wire/parse-json (:body resp)) "result")))
          (is (wait-until #(= [:nrepl "/tmp/x" [":dev" ":test"]] (first @started))))))
      (testing "fields-based start passes the STRING-keyed field map through (async)"
        (let [resp (call {:kind "mcp-stdio" :selected {:cmd "npx server" :cwd "/tmp"}})]
          (is (= 200 (:status resp)))
          (is (wait-until #(= [:mcp {"cmd" "npx server" "cwd" "/tmp"}] (second @started))))))
      (testing "unknown kind → 404" (is (= 404 (:status (call {:kind "nope"})))))
      (testing "missing kind → 400" (is (= 400 (:status (call {})))))
      (testing
        "a throwing :start-fn never crashes the handler — 'starting', failure swallowed off-thread"
        (let [resp (call {:kind "boom"})]
          (is (= 200 (:status resp)))
          (is (= "starting" (get (wire/parse-json (:body resp)) "result"))))))))

(deftest wrap-auth-accepts-gateway-secret-header
  (testing "a token-gated gateway authenticates the internal client's X-Vis-Gateway-Secret"
    (with-server-state!
      {:require-token? true}
      (fn []
        (let [wrap-auth
              (rv 'wrap-auth)

              handler
              (fn [_req]
                {:status 200 :body "ok"})

              app
              (wrap-auth handler "sekret" [])

              req
              (fn [headers]
                {:uri "/v1/sessions" :headers headers})]

          (testing "no credential → 401" (is (= 401 (:status (app (req {}))))))
          (testing "Authorization: Bearer with the right token → 200"
            (is (= 200 (:status (app (req {"authorization" "Bearer sekret"}))))))
          (testing
            "X-Vis-Gateway-Secret carrying the same secret → 200 (the internal client's carrier)"
            (is (= 200 (:status (app (req {"x-vis-gateway-secret" "sekret"}))))))
          (testing "X-Vis-Gateway-Secret with a wrong secret → 401"
            (is (= 401 (:status (app (req {"x-vis-gateway-secret" "nope"})))))))))))

(deftest wrap-auth-disabled-on-loopback-default
  (testing "with auth off (loopback default) every request passes without a token"
    (with-server-state! {:require-token? false}
                        (fn []
                          (let [wrap-auth
                                (rv 'wrap-auth)

                                app
                                (wrap-auth (fn [_req]
                                             {:status 200})
                                           "sekret"
                                           [])]

                            (is (= 200 (:status (app {:uri "/v1/sessions" :headers {}})))))))))

(deftest parse-multi-sids-parses-and-filters
  (testing "sid[:cursor] comma list — cursor defaults to 0, unknown/non-UUID sids dropped"
    (let [sid-a
          (java.util.UUID/randomUUID)

          sid-b
          (java.util.UUID/randomUUID)

          a
          (str sid-a)

          b
          (str sid-b)]

      (with-redefs-fn {#'state/soul (fn [sid]
                                      (contains? #{sid-a sid-b} sid))}
        (fn []
          (let [parse (rv 'parse-multi-sids)]
            ;; sids are parsed to java.util.UUID — the registry's key type
            ;; (path-sid parity). A string key registered a ghost registry
            ;; entry: idle tabs would miss live queue and turn events.
            (is (= [[sid-a 10] [sid-b 0]]
                   (parse {:query-params {"sids" (str a ":10, " b " , zzz:3")}})))
            (is (nil? (parse {:query-params {}})))
            (is (nil? (parse {:query-params {"sids" ""}})))
            (testing "a syntactically valid but UNKNOWN UUID is dropped"
              (is (= [] (parse {:query-params {"sids" (str (java.util.UUID/randomUUID))}}))))
            (testing "Last-Event-ID overrides the cursor for the SINGLE-sid case (native reconnect)"
              (is (= [[sid-a 42]]
                     (parse {:query-params {"sids" (str a ":0")} :headers {"last-event-id" "42"}})))
              (is (= [[sid-a 7]]
                     (parse {:query-params {"sids" a} :headers {"last-event-id" "7"}}))))
            (testing
              "Last-Event-ID is IGNORED for multi-sid (one header can't resume N per-session seqs)"
              (is (= [[sid-a 10] [sid-b 0]]
                     (parse {:query-params {"sids" (str a ":10," b)}
                             :headers {"last-event-id" "42"}}))))
            (testing "a non-numeric Last-Event-ID is ignored"
              (is (= [[sid-a 3]]
                     (parse {:query-params {"sids" (str a ":3")}
                             :headers {"last-event-id" ""}}))))))))))

(deftest multi-sse-fans-many-sessions-down-one-stream
  (testing
    "every listed session's events ride ONE connection, tagged by :session_id, deduped per session"
    (with-redefs-fn {#'server/stop! (fn []
                                      nil)}
      (fn []
        (with-server-state!
          {}
          (fn []
            (let [multi-sse-body
                  (rv 'multi-sse-body)

                  write-body
                  (requiring-resolve 'ring.core.protocols/write-body-to-stream)

                  sid-a
                  (str (java.util.UUID/randomUUID))

                  sid-b
                  (str (java.util.UUID/randomUUID))

                  baos
                  (java.io.ByteArrayOutputStream.)

                  body
                  (multi-sse-body [[sid-a 0] [sid-b 0]] false)

                  fut
                  (future (try (write-body body {} baos) (catch Throwable _ nil)))]

              (Thread/sleep 150)
              (state/append-event! sid-a "test.alpha" {:n 1})
              (state/append-event! sid-b "test.beta" {:n 2})
              (state/append-event! sid-a "test.alpha2" {:n 3})
              (Thread/sleep 200)
              (future-cancel fut)
              (let [s (String. (.toByteArray baos) "UTF-8")]
                (testing "both sessions surfaced on the single stream"
                  (is (re-find (re-pattern sid-a) s))
                  (is (re-find (re-pattern sid-b) s))
                  (is (re-find #"test.alpha2" s)))
                (testing "per-session dedup keeps each session's own monotonic run"
                  (is (= 2 (count (re-seq (re-pattern sid-a) s))))
                  (is (= 1 (count (re-seq (re-pattern sid-b) s)))))))))))))

;; ── Resource rid rides the QUERY STRING, not a path segment (issue #14) ──
;; A resource id can embed an absolute path — an nREPL id is `nrepl:/Users/…/ws`.
;; Percent-encoded into a PATH SEGMENT its `/` becomes `%2F`, which Jetty rejects
;; with "Ambiguous URI path separator" (400) — that 400 threw out of the client
;; and wedged F4 when you clicked logs on the clojure nREPL. The fix moves rid to
;; the `rid` query param on stop/restart/logs. These lock that in on BOTH halves.

(def ^:private nrepl-rid
  "A real-shaped nREPL resource id: the `/`-embedding absolute path that broke."
  "nrepl:/Users/fierycod/vis")

(deftest resource-client-builds-query-param-urls
  (testing "stop/restart/logs put rid in the ?rid= query, never a path segment (no %2F in path)"
    (let [sent (atom [])]
      (with-redefs-fn {#'client/send-json! (fn [method path & _]
                                             (swap! sent conj [method path])
                                             {:result "ok" :lines ["a"]})}
        (fn []
          (let [sid (str (random-uuid))]
            (client/stop-resource! sid nrepl-rid)
            (client/restart-resource! sid nrepl-rid)
            (client/resource-logs sid nrepl-rid)
            (let [[[_ stop] [_ restart] [_ logs]] @sent]
              (testing "each url ends with the rid encoded in a query param"
                (is (= (str "/v1/sessions/"
                            sid
                            "/resources/stop?rid=nrepl%3A%2FUsers%2Ffierycod%2Fvis")
                       stop))
                (is (= (str "/v1/sessions/"
                            sid
                            "/resources/restart?rid=nrepl%3A%2FUsers%2Ffierycod%2Fvis")
                       restart))
                (is (= (str "/v1/sessions/"
                            sid
                            "/resources/logs?rid=nrepl%3A%2FUsers%2Ffierycod%2Fvis")
                       logs)))
              (testing
                "the raw rid never leaks into the PATH portion (would trip the ambiguous-slash 400)"
                (doseq [[_ path] @sent]
                  (is (not (re-find #"resources/nrepl" path))))))))))))

(deftest resource-handlers-read-rid-from-query-param
  (testing "stop/restart/logs handlers forward the rid QUERY param to the resources ns"
    (let [seen
          (atom [])

          sid
          (str (random-uuid))

          req
          {:path-params {:sid sid} :query-params {"rid" nrepl-rid}}]

      (with-redefs-fn {#'resources/stop! (fn [_ rid]
                                           (swap! seen conj [:stop rid])
                                           {:result "stopped"})
                       #'resources/restart! (fn [_ rid]
                                              (swap! seen conj [:restart rid])
                                              {:result "restarted"})
                       #'resources/logs (fn [_ rid]
                                          (swap! seen conj [:logs rid])
                                          ["line-1"])}
        (fn []
          (let [stop
                ((rv 'resource-stop-handler) req)

                restart
                ((rv 'resource-restart-handler) req)

                logs
                ((rv 'resource-logs-handler) req)]

            (testing "each handler answers 200 and threads the exact slash-embedding rid through"
              (is (= 200 (:status stop)))
              (is (= 200 (:status restart)))
              (is (= 200 (:status logs)))
              (is (= [[:stop nrepl-rid] [:restart nrepl-rid] [:logs nrepl-rid]] @seen)))
            (testing "logs handler surfaces the captured lines"
              (is (= ["line-1"] (get (wire/parse-json (:body logs)) "lines"))))))))))

(deftest resource-handlers-404-on-unknown-session
  (testing "a non-uuid sid is rejected before any resources call — 404, resources ns untouched"
    (let [touched
          (atom false)

          req
          {:path-params {:sid "not-a-uuid"} :query-params {"rid" nrepl-rid}}]

      (with-redefs-fn {#'resources/stop! (fn [& _]
                                           (reset! touched true)
                                           {})
                       #'resources/logs (fn [& _]
                                          (reset! touched true)
                                          nil)}
        (fn []
          (is (= 404 (:status ((rv 'resource-stop-handler) req))))
          (is (= 404 (:status ((rv 'resource-logs-handler) req))))
          (is (false? @touched)))))))

(deftest resource-rid-survives-router-as-query-param
  (testing
    "the client's encoded url routes to the static handler and decodes rid back verbatim (no 400)"
    (let [seen
          (atom nil)

          echo
          (fn [request]
            (reset! seen {:sid (get-in request [:path-params :sid])
                          :rid (get-in request [:query-params "rid"])})
            {:status 200 :body "ok"})

          app
          (-> (rr/ring-handler (rr/router [["/v1/sessions/:sid/resources/stop" {:post echo}]
                                           ["/v1/sessions/:sid/resources/logs" {:get echo}]]))
              ring-params/wrap-params)

          sid
          (str (random-uuid))

          ;; exactly the shape the client emits: rid percent-encoded into the query
          enc
          (fn [s]
            (java.net.URLEncoder/encode ^String s "UTF-8"))

          resp
          (app {:request-method :get
                :uri (str "/v1/sessions/" sid "/resources/logs")
                :query-string (str "rid=" (enc nrepl-rid))})]

      (testing "static logs route matches (a path-segment %2F would 404/400 instead)"
        (is (= 200 (:status resp))))
      (testing "the handler sees the sid and the FULL slash-embedding rid, decoded"
        (is (= {:sid sid :rid nrepl-rid} @seen))))))
