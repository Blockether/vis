(ns com.blockether.vis.internal.gateway.server-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.gateway.discovery :as discovery]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.loop :as lp]))

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

                              ;; the client applies kebab-keys to restore the engine's hyphen shape
                              startables
                              (mapv wire/kebab-keys (:startables (wire/parse-json (:body resp))))

                              nrepl
                              (first startables)

                              mcp
                              (second startables)]

                          (is (= 200 (:status resp)))
                          ;; nREPL: dir + options proposed from env, hyphenated key survives the wire
                          (is (= "nrepl" (:kind nrepl)))
                          (is (true? (:dir? nrepl)))
                          (is (true? (:options? nrepl)))
                          (is (= "aliases" (:options-label nrepl)))
                          (is (= [":dev" ":test"] (:options nrepl)))
                          ;; MCP: declared fields ride across, no options
                          (is (= "mcp-stdio" (:kind mcp)))
                          (is (nil? (:options? mcp)))
                          (is (= 2 (count (:fields mcp))))
                          ;; the non-serializable fns never cross the wire
                          (is (not (contains? nrepl :start-fn)))
                          (is (not (contains? nrepl :options-fn))))))))

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
          (is (= "starting" (:result (wire/parse-json (:body resp)))))
          (is (wait-until #(= [:nrepl "/tmp/x" [":dev" ":test"]] (first @started))))))
      (testing "fields-based start passes the keyword-keyed field map through (async)"
        (let [resp (call {:kind "mcp-stdio" :selected {:cmd "npx server" :cwd "/tmp"}})]
          (is (= 200 (:status resp)))
          (is (wait-until #(= [:mcp {:cmd "npx server" :cwd "/tmp"}] (second @started))))))
      (testing "unknown kind → 404" (is (= 404 (:status (call {:kind "nope"})))))
      (testing "missing kind → 400" (is (= 400 (:status (call {})))))
      (testing
        "a throwing :start-fn never crashes the handler — 'starting', failure swallowed off-thread"
        (let [resp (call {:kind "boom"})]
          (is (= 200 (:status resp)))
          (is (= "starting" (:result (wire/parse-json (:body resp))))))))))
