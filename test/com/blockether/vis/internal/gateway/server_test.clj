(ns com.blockether.vis.internal.gateway.server-test
  (:require [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.gateway.client :as client]
            [com.blockether.vis.internal.gateway.discovery :as discovery]
            [com.blockether.vis.internal.gateway.protocol :as protocol]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.providers :as providers]
            [com.blockether.vis.internal.resources :as resources]
            [com.blockether.vis.internal.slash :as slash]
            [com.blockether.vis.internal.theme :as theme]
            [com.blockether.vis.internal.toggles :as toggles]
            [reitit.ring :as rr]
            [ring.adapter.jetty :as jetty]
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

(deftest list-turns-status-filter-routes-to-queued-overlay
  (let
    [sid
     (random-uuid)

     calls
     (atom [])

     request
     {:path-params {:sid (str sid)}}]

    (with-redefs-fn {#'state/soul (fn [actual]
                                    (= sid actual))
                     #'state/list-queued-turns (fn [actual]
                                                 (swap! calls conj [:queued actual])
                                                 [])
                     #'state/list-turns (fn [actual]
                                          (swap! calls conj [:all actual])
                                          [])}
      #(do (is (= 200
                  (:status ((rv 'list-turns-handler)
                             (assoc request :query-params {"status" "queued"})))))
           (is (= [[:queued sid]] @calls))
           (reset! calls [])
           (is (= 200 (:status ((rv 'list-turns-handler) request))))
           (is (= [[:all sid]] @calls))))))

(deftest draft-management-client-builds-canonical-routes
  (let
    [sent
     (atom [])

     sid
     (str (random-uuid))

     wid
     (str (random-uuid))]

    (with-redefs-fn {#'client/send-json! (fn [method path body]
                                           (swap! sent conj [method path body])
                                           {"workspace" {"root" "/repo"}})}
      (fn []
        (is (= {"root" "/repo"} (client/create-draft! sid "feature-c" false)))
        (is (= {"root" "/repo"} (client/abandon-draft! sid wid "done")))
        (is (= [["POST" (str "/v1/sessions/" sid "/workspace/drafts")
                 {:label "feature-c" :blank false}]
                ["DELETE" (str "/v1/sessions/" sid "/workspace/drafts/" wid) {:reason "done"}]]
               @sent))))))

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
  (testing "dead recorded client pids are reaped, so SIGKILLed TUIs still let the daemon die"
    (let [stops (atom 0)]
      (with-stop-stub! stops
                       {#'discovery/pid-alive-cached? (constantly false)
                        (rv 'log-client-lease-warning!) (fn [& _])}
                       (fn []
                         (with-server-state! {:managed? true
                                              :saw-client? true
                                              :started-at-ms (System/currentTimeMillis)
                                              :clients {"c1" {:pid 12345 :kind "clojure-client"}}
                                              :sse-clients #{}}
                                             (fn []
                                               ((rv 'reap-client-leases!))
                                               ((rv 'maybe-stop-when-idle!))
                                               (is (wait-until #(= 1 @stops))))))))))

(deftest client-count-is-constant-time-and-does-not-probe-pids
  (testing "status reads the already-reaped lease map without OS liveness work"
    (with-server-state! {:clients {"c1" {:pid 10} "c2" {:pid 11}} :sse-clients #{"s1"}}
                        (fn []
                          (with-redefs
                            [discovery/pid-alive-cached? (fn [_]
                                                           (throw (ex-info "must not probe" {})))]
                            (is (= 3 ((rv 'client-count)))))))))

(deftest compact-client-leases-removes-dead-and-duplicate-pids
  (testing "one sweep probes each pid once, keeps nil-pid leases, and preserves identity when clean"
    (let
      [checks
       (atom [])

       clients
       {"live-old" {:pid 10} "live-duplicate" {:pid 10} "dead" {:pid 20} "browser" {:pid nil}}

       compact
       (rv 'compact-client-leases)]

      (with-redefs
        [discovery/pid-alive-cached? (fn [pid]
                                       (swap! checks conj pid)
                                       (= 10 pid))]
        (let [{after :clients :keys [dead duplicates]} (compact clients)]
          (is (= 1 dead))
          (is (= 1 duplicates))
          (is (= #{"browser"} (set (filter #(nil? (get-in after [% :pid])) (keys after)))))
          (is (= 2 (count after)))
          (is (= #{10 20} (set @checks))))
        (let
          [clean {"live" {:pid 10} "browser" {:pid nil}}
           {after :clients :keys [dead duplicates]} (compact clean)]

          (is (identical? clean after))
          (is (zero? dead))
          (is (zero? duplicates)))))))

(deftest registering-a-pid-upserts-its-single-process-lease
  (testing "re-registration replaces the old opaque id but preserves other processes and browsers"
    (let
      [register
       (rv 'register-client-lease)

       before
       {"old" {:pid 10} "other" {:pid 20} "browser" {:pid nil}}

       {after :clients :keys [replaced]}
       (register before "new" {:pid 10 :kind "clojure-client"})]

      (is (= 1 replaced))
      (is (= #{"new" "other" "browser"} (set (keys after))))
      (is (= 10 (get-in after ["new" :pid]))))))

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


(defn- body-stream
  [m]
  (java.io.ByteArrayInputStream. (.getBytes ^String (wire/json-str m) "UTF-8")))

(deftest capabilities-advertise-gateway-voice-and-attachment-contract
  (testing "a gateway without the optional voice extension reports it honestly"
    (with-redefs-fn {(rv 'voice-asr-resolve) (constantly nil)}
      (fn []
        (let
          [response
           ((rv 'capabilities-handler) {})

           body
           (wire/parse-json (:body response))]

          (is (= 200 (:status response)))
          (is (= 1 (get body "version")))
          (is (true? (get-in body ["features" "attachments" "enabled"])))
          (is (= 8 (get-in body ["features" "attachments" "max_files"])))
          (is (= (* 5 1024 1024) (get-in body ["features" "attachments" "max_file_bytes"])))
          (is (false? (get-in body ["features" "voice" "enabled"])))
          (is (= "unavailable" (get-in body ["features" "voice" "model" "status"])))))))
  (testing "voice support includes the current model state without starting a download"
    (with-redefs-fn {(rv 'voice-asr-resolve) (fn [fn-name]
                                               (case fn-name
                                                 "model-state"
                                                 (constantly {:state :ready})

                                                 "transcribe-file!"
                                                 identity

                                                 nil))}
      (fn []
        (let
          [body (-> ((rv 'capabilities-handler) {})
                    :body
                    wire/parse-json)]
          (is (true? (get-in body ["features" "voice" "enabled"])))
          (is (= "audio/wav" (get-in body ["features" "voice" "transport"])))
          (is (= "ready" (get-in body ["features" "voice" "model" "status"]))))))))

(deftest theme-handler-shares-the-tui-palette-and-persistence
  (let [saved (atom nil)]
    (with-redefs
      [config/load-config-raw (constantly {"providers" [{"id" "demo"}]
                                           "tui_settings" {"theme_name" "solarized-dark"}})
       config/save-config! #(reset! saved %)]

      (let
        [get-response ((rv 'get-theme-handler) {})
         current (wire/parse-json (:body get-response))]

        (is (= 200 (:status get-response)))
        (is (= "solarized-dark" (get current "id")))
        (is (= (get (theme/theme->web-css-vars (theme/theme "solarized-dark")) "--bg")
               (get-in current ["css_vars" "--bg"])))
        (doseq
          [css-var ["--dialog-title-bg" "--dialog-title-fg" "--dialog-border" "--dialog-shadow"
                    "--dialog-hint" "--input-field-bg" "--button-bg" "--button-fg"
                    "--user-bubble-bg" "--user-bubble-fg" "--user-role-fg" "--ai-bubble-bg"
                    "--ai-bubble-fg" "--ai-role-fg" "--iteration-header-fg" "--iteration-header-bg"
                    "--answer-bg" "--answer-fg" "--md-h1-fg" "--md-h2-fg" "--md-h3-fg" "--code-bg"
                    "--code-fg" "--code-ok-bg" "--code-err-bg" "--code-success" "--code-error"
                    "--code-syntax-keyword" "--code-syntax-special" "--code-syntax-string"
                    "--code-syntax-number" "--code-syntax-comment" "--result-bg" "--code-result"
                    "--code-duration" "--footer-fg" "--footer-muted" "--footer-spinner"]]
          (is (string? (get-in current ["css_vars" css-var])) css-var))
        (is (some #(= "vis-dark" (get % "id")) (get current "themes")))
        ;; Each theme in the list carries its own browser-ready palette so the
        ;; companion can pin a local theme (e.g. light) without a POST.
        (doseq [t (get current "themes")]
          (is (string? (get-in t ["css_vars" "--bg"])) (get t "id"))))
      (let
        [set-response ((rv 'set-theme-handler) {:body (body-stream {:id "vis-dark"})})
         updated (wire/parse-json (:body set-response))]

        (is (= 200 (:status set-response)))
        (is (= "vis-dark" (get updated "id")))
        (is (= "vis-dark" (get-in @saved ["tui_settings" "theme_name"])))
        (is (= [{"id" "demo"}] (get @saved "providers")))))))

(deftest theme-handler-rejects-unknown-themes
  (let [saved? (atom false)]
    (with-redefs
      [config/load-config-raw (constantly {})
       config/save-config! (fn [_]
                             (reset! saved? true))]

      (let [response ((rv 'set-theme-handler) {:body (body-stream {:id "not-a-theme"})})]
        (is (= 404 (:status response)))
        (is (false? @saved?))))))

(deftest slashes-handler-uses-the-web-palette-and-includes-native-commands
  (let [seen (atom nil)]
    (with-redefs
      [slash/slash-palette (fn [channel extra]
                             (reset! seen [channel extra])
                             (conj (vec extra) {:name "/rename" :doc "Rename"}))]
      (let
        [response ((rv 'slashes-handler) {})
         body (wire/parse-json (:body response))]

        (is (= 200 (:status response)))
        (is (= :web (first @seen)))
        (is (some #(= "/help" (:name %)) (second @seen)))
        (is (some #(= "/rename" (get % "name")) (get body "commands")))))))


(deftest wrap-auth-accepts-gateway-secret-header
  (testing "a token-gated gateway authenticates the internal client's X-Vis-Gateway-Secret"
    (with-server-state!
      {:require-token? true}
      (fn []
        (let
          [wrap-auth
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
                          (let
                            [wrap-auth
                             (rv 'wrap-auth)

                             app
                             (wrap-auth (fn [_req]
                                          {:status 200})
                                        "sekret"
                                        [])]

                            (is (= 200 (:status (app {:uri "/v1/sessions" :headers {}})))))))))

(deftest cors-preflight-is-answered-before-auth
  (testing
    "a cross-origin browser reaches a token-gated gateway: preflight OPTIONS is 204 without a token, and CORS headers ride every response so the browser can read even a 401"
    (with-server-state!
      {:require-token? true}
      (fn []
        (let
          [app
           ((rv 'app) "sekret" [])

           origin
           "http://100.109.18.77:5273"

           preflight
           (app {:request-method :options
                 :uri "/v1/sessions"
                 :headers {"origin" origin
                           "access-control-request-headers" "authorization,content-type"}})

           noauth
           (app {:request-method :get
                 :uri "/v1/sessions"
                 :headers {"origin" origin
                           protocol/protocol-header (str protocol/protocol-version)}})

           authed
           (app {:request-method :get
                 :uri "/v1/sessions"
                 :headers {"origin" origin
                           "authorization" "Bearer sekret"
                           protocol/protocol-header (str protocol/protocol-version)}})]

          (testing "preflight OPTIONS short-circuits auth with 204"
            (is (= 204 (:status preflight)))
            (is (= origin (get-in preflight [:headers "Access-Control-Allow-Origin"])))
            (is (= "true" (get-in preflight [:headers "Access-Control-Allow-Credentials"])))
            (is (= "authorization,content-type"
                   (get-in preflight [:headers "Access-Control-Allow-Headers"]))))
          (testing "a 401 still carries CORS so the browser surfaces the error, not an opaque block"
            (is (= 401 (:status noauth)))
            (is (= origin (get-in noauth [:headers "Access-Control-Allow-Origin"]))))
          (testing "an authenticated request gets its data with CORS"
            (is (= 200 (:status authed)))
            (is (= origin (get-in authed [:headers "Access-Control-Allow-Origin"])))))))))

(deftest parse-multi-sids-parses-and-filters
  (testing "sid[:cursor] comma list — cursor defaults to 0, unknown/non-UUID sids dropped"
    (let
      [sid-a
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

(deftest transcript-window-params-never-degrade-to-the-full-transcript
  (let
    [q
     (fn [qs]
       (ring-params/assoc-query-params {:path-params {:sid (str (java.util.UUID/randomUUID))}
                                        :query-string qs}
                                       "UTF-8"))

     query-long
     (rv 'query-long)

     seen
     (atom nil)

     call
     (fn [qs]
       (reset! seen ::unset)
       (with-redefs-fn {#'state/transcript-page (fn [_sid opts]
                                                  (reset! seen opts)
                                                  {:turns [] :total 0 :offset 0 :has-more false})}
         #(let
            [r
             ((rv 'transcript-handler) (q qs))]

            [(:status r) @seen])))]

    (testing "ring hands back a VECTOR for a repeated param, so parsing must not throw"
      (is (= ["1" "2"] (get-in (q "limit=1&limit=2") [:query-params "limit"])))
      (is (= 2 (query-long (q "limit=1&limit=2") "limit"))))
    (testing "absent or blank stays nil — that is what 'unwindowed' means"
      (is (nil? (query-long (q "") "limit")))
      (is (nil? (query-long (q "limit=  ") "limit"))))
    (testing "0 and negatives reach the caller verbatim; clamping is transcript-page's job"
      (is (= 0 (query-long (q "limit=0") "limit")))
      (is (= -5 (query-long (q "limit=-5") "limit"))))
    (testing "no window params = the whole transcript, so an older client is unaffected"
      (is (= [200 {:limit nil :offset nil}] (call ""))))
    (testing "?limit=0 honestly means zero rows, never the whole transcript"
      (is (= [200 {:limit 0 :offset nil}] (call "limit=0"))))
    (testing "a duplicated param answers on its last value instead of a 500"
      (is (= [200 {:limit 2 :offset nil}] (call "limit=1&limit=2"))))
    (testing "a present-but-unparsable window param is a 400, not a silent full-transcript fallback"
      (is (= 400 (first (call "limit=abc"))))
      (is (= 400 (first (call "limit=10&offset=nope")))))))

(deftest multi-sse-fans-many-sessions-down-one-stream
  (testing
    "every listed session's events ride ONE connection, tagged by :session_id, deduped per session"
    (with-redefs-fn {#'server/stop! (fn []
                                      nil)}
      (fn []
        (with-server-state!
          {}
          (fn []
            (let
              [multi-sse-body
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
                  ;; Each session rides ONE subscription.ready control frame
                  ;; (carrying its :session_id in the JSON data) plus one frame per
                  ;; distinct event. The sid appears only in each frame's data, so its
                  ;; count is the reliable per-session dedup signal:
                  ;; sid-a: ready + test.alpha + test.alpha2 = 3; sid-b: ready + test.beta = 2.
                  (is (re-find #"subscription.ready" s))
                  (is (= 3 (count (re-seq (re-pattern sid-a) s))))
                  (is (= 2 (count (re-seq (re-pattern sid-b) s)))))))))))))

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
    (let
      [seen
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
          (let
            [stop
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
    (let
      [touched
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
    (let
      [seen
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

(deftest toggle-id-wire-contract-test
  (testing "settings rows expose the canonical string id unchanged"
    (toggles/register-toggle! {:id "server_test_toggle" :label "Test" :default false})
    (is (= "server_test_toggle"
           (:id ((rv 'toggle-json) (toggles/toggle-spec "server_test_toggle"))))))
  (testing "the settings mutation endpoint rejects keyword-like, namespaced, and kebab ids"
    (doseq [id [":server_test_toggle" "vis/server_test_toggle" "server-test-toggle"]]
      (is (= 400
             (:status ((rv 'set-setting-handler) {:query-params {"id" id "action" "toggle"}}))))))
  (testing "a canonical but unknown string id remains a distinct 404"
    (is (= 404
           (:status ((rv 'set-setting-handler)
                      {:query-params {"id" "unknown_toggle" "action" "toggle"}}))))))

(deftest provider-models-handler-serves-live-catalog-daemon-side
  (testing
    "GET /v1/providers/:id/models fetches the LIVE catalog DAEMON-side (gateway owns OAuth token) and emits snake_case hidden_count"
    (with-redefs-fn {#'providers/default-model-names (constantly ["claude-opus-4-8"])
                     #'providers/model-options (fn [_ _ show-all?]
                                                 {:models ["claude-opus-4-8" "claude-sonnet-5"]
                                                  :hidden-count (if show-all? 0 4)})}
      (fn []
        (let
          [resp
           ((rv 'provider-models-handler) {:path-params {:provider-id "anthropic-coding-plan"}})

           body
           (wire/parse-json (:body resp))]

          (is (= 200 (:status resp)))
          (is (= ["claude-opus-4-8" "claude-sonnet-5"] (get body "models")))
          (is (= 4 (get body "hidden_count"))))
        (let
          [resp
           ((rv 'provider-models-handler)
             {:path-params {:provider-id "anthropic-coding-plan"}
              :query-params {"show_all" "true"}})

           body
           (wire/parse-json (:body resp))]

          (is (= 0 (get body "hidden_count"))))))))

(deftest set-session-model-handler-validates-against-the-gateway-fleet
  (testing
    "PATCH /model pins only providers THIS gateway serves; the model name stays free (live catalog)"
    (let
      [sid
       (str (java.util.UUID/randomUUID))

       wrote
       (atom nil)

       body
       (fn [m]
         {:path-params {:sid sid}
          :body (java.io.ByteArrayInputStream. (.getBytes (wire/json-str m) "UTF-8"))})]

      (with-redefs-fn {#'providers/configured-providers-cached
                       (constantly [{:id :zai-coding-plan} {:id :anthropic-coding-plan}])
                       #'state/set-session-model! (fn [_sid p m]
                                                    (reset! wrote [p m]))
                       #'state/session-model (fn [_sid]
                                               @wrote)}
        (fn []
          (testing "a configured provider is accepted"
            (let
              [resp ((rv 'set-session-model-handler)
                      (body {:provider "zai-coding-plan" :model "glm-5.2"}))]
              (is (= 200 (:status resp)))
              (is (= ["zai-coding-plan" "glm-5.2"] @wrote))))
          (testing "a model outside vis.yml is fine — the live catalog offers more"
            (is (= 200
                   (:status ((rv 'set-session-model-handler)
                              (body {:provider "zai-coding-plan" :model "glm-live-preview"})))))
            (is (= ["zai-coding-plan" "glm-live-preview"] @wrote)))
          (testing "an unknown provider is a 400 and writes NOTHING"
            (reset! wrote :untouched)
            (let
              [resp ((rv 'set-session-model-handler)
                      (body {:provider "not-on-this-gateway" :model "x"}))]
              (is (= 400 (:status resp)))
              (is (= "unknown-provider" (get-in (wire/parse-json (:body resp)) ["error" "type"])))
              (is (= :untouched @wrote))))
          (testing "blank/omitted provider still clears or pins by model alone"
            (reset! wrote nil)
            (is (= 200
                   (:status ((rv 'set-session-model-handler)
                              (body {:provider "  " :model "glm-5.2"})))))
            (is (= [nil "glm-5.2"] @wrote))))))))

(deftest router-handler-assembles-string-keyed-fleet-with-status
  (testing "GET /v1/router returns every model plus the one explicit default pair"
    (with-redefs-fn
      {#'providers/picker-fleet (constantly [{:id :anthropic-coding-plan
                                              :base-url "https://api.anthropic.com/v1"
                                              :models [{:name "claude-opus-4-8"}
                                                       {:name "claude-sonnet-5"}]}])
       #'providers/default-selection (constantly {:provider-id :anthropic-coding-plan
                                                  :model "claude-sonnet-5"})
       #'providers/provider-status (constantly {:is-authenticated true :source :auth-file})
       #'providers/provider-limits-safe
       (constantly
         {:provider-id :anthropic-coding-plan :status :ok :static {} :dynamic {:limits []}})}
      (fn []
        (let
          [resp
           ((rv 'router-handler) {})

           provs
           (get (wire/parse-json (:body resp)) "providers")

           p0
           (first provs)]

          (is (= 200 (:status resp)))
          (is (= "anthropic-coding-plan" (get p0 "id")))
          (is (= "https://api.anthropic.com/v1" (get p0 "base_url")))
          (is (= ["claude-opus-4-8" "claude-sonnet-5"] (get p0 "models")))
          (is (true? (get p0 "is_default")))
          (is (= "claude-sonnet-5" (get p0 "default_model")))
          ;; connection verdict is the snake_case STRING key — no keyword restore
          (is (true? (get-in p0 ["status" "is_authenticated"])))
          (is (= "auth-file" (get-in p0 ["status" "source"])))
          (is (every? string? (keys (get p0 "status"))))
          ;; limits ride embedded, string-keyed too
          (is (= "ok" (get-in p0 ["limits" "status"]))))))))

(deftest router-default-handler-persists-one-pair
  (let
    [saved
     (atom nil)

     request
     {:body (java.io.ByteArrayInputStream. (.getBytes (wire/json-str {"provider"
                                                                      "anthropic-coding-plan"
                                                                      "model" "claude-fable-5"})
                                                      "UTF-8"))}]

    (with-redefs
      [providers/save-default-selection! (fn [provider model source]
                                           (reset! saved [provider model source])
                                           {:provider-id :anthropic-coding-plan
                                            :model "claude-fable-5"})]
      (let
        [resp ((rv 'router-default-handler) request)
         body (wire/parse-json (:body resp))]

        (is (= 200 (:status resp)))
        (is (= ["anthropic-coding-plan" "claude-fable-5" :gateway] @saved))
        (is (= "anthropic-coding-plan" (get body "default_provider")))
        (is (= "claude-fable-5" (get body "default_model")))))))

(deftest gateway-prometheus-runtime-metrics-test
  (let
    [text ((rv 'prometheus-text)
            {:tokens-input 12
             :tokens-output 7
             :turns-executing 2
             :turns-waiting 1
             :env-cache-size 3
             :env-heap-pressure true
             :jvm-heap-used-bytes 1024
             :jvm-gc-count-total 4
             :jvm-thread-count 9})]
    (testing "preserves the existing labelled token counter"
      (is (re-find #"vis_turn_tokens_total\{kind=\"input\"\} 12" text))
      (is (re-find #"vis_turn_tokens_total\{kind=\"output\"\} 7" text)))
    (testing "exports resource-pressure and concurrency gauges"
      (is (re-find #"vis_turns_executing 2" text))
      (is (re-find #"vis_turns_waiting 1" text))
      (is (re-find #"vis_env_heap_pressure 1" text))
      (is (re-find #"vis_jvm_heap_used_bytes 1024" text)))))

(defn- non-loopback-ipv4
  "First live non-loopback IPv4 address, i.e. the kind of concrete host `--pair`
   binds. nil on a machine with no such interface, where the mirror is moot."
  []
  (->> (enumeration-seq (java.net.NetworkInterface/getNetworkInterfaces))
       (filter (fn [^java.net.NetworkInterface i]
                 (.isUp i)))
       (mapcat (fn [^java.net.NetworkInterface i]
                 (enumeration-seq (.getInetAddresses i))))
       (filter (fn [a]
                 (instance? java.net.Inet4Address a)))
       (remove (fn [^java.net.InetAddress a]
                 (.isLoopbackAddress a)))
       (map (fn [^java.net.InetAddress a]
              (.getHostAddress a)))
       first))

(deftest pair-bind-still-answers-on-loopback
  (testing
    "a concrete non-loopback bind (what --pair picks) also serves 127.0.0.1,
            so the local TUI attaches to THIS gateway instead of seeing a free port
            and spawning a second one"
    (if-let [host (non-loopback-ipv4)]
      (let
        [port (with-open [s (java.net.ServerSocket. 0)]
                (.getLocalPort s))
         server (jetty/run-jetty (constantly {:status 200 :headers {} :body "ok"})
                                 {:port port
                                  :host host
                                  :join? false
                                  :configurator ((rv 'loopback-mirror-configurator) port)})]

        (try (is (= #{host "127.0.0.1"}
                    (set (map (fn [^org.eclipse.jetty.server.ServerConnector c]
                                (.getHost c))
                              (.getConnectors ^org.eclipse.jetty.server.Server server)))))
             (is (= "ok" (slurp (str "http://" host ":" port "/"))))
             (is (= "ok" (slurp (str "http://127.0.0.1:" port "/"))))
             (finally (.stop ^org.eclipse.jetty.server.Server server))))
      (is (nil? (non-loopback-ipv4)) "no non-loopback interface here; nothing to mirror"))))
