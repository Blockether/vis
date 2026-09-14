(ns com.blockether.vis.tui.agent-team-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.agent-team :as team]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.frame :as frame]
            [com.blockether.vis.tui.header :as header]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as theme]
            [lazytest.core :as lt]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [com.googlecode.lanterna TerminalSize]
           [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.html HtmlTerminal HtmlTerminalView]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(lt/set-ns-context! [(lt/around-each [f]
                                     (with-redefs [vis/setting (constantly {"enabled" true})]
                                       (f)))])

(deftest feature-opt-in-controls-fetches-and-cached-availability-test
  (let [enabled
        (atom false)

        requests
        (atom 0)]

    (with-redefs [vis/setting
                  (fn [id]
                    (is (= "subagents" id))
                    {"enabled" @enabled})

                  vis/request!
                  (fn [& _]
                    (swap! requests inc)
                    {:status 200 :body "[]"})]

      (is (not (team/enabled? "experimental-team")))
      (team/fetch! "experimental-team")
      (is (zero? @requests))
      (is (not (team/enabled? "experimental-team")))
      (reset! enabled true)
      (team/fetch! "experimental-team")
      (is (team/enabled? "experimental-team"))
      (is (= 1 @requests))
      (reset! enabled false)
      (team/fetch! "experimental-team")
      (is (not (team/enabled? "experimental-team")))
      (is (= 1 @requests)))
    (with-redefs [vis/setting (fn [_]
                                (throw (java.io.IOException. "offline")))]
      (team/fetch! "experimental-team")
      (is (not (team/enabled? "experimental-team"))))))

(deftest read-errors-are-recoverable-test
  (doseq [response [{:status 503} {:status 200 :body "not json"} {:status 200 :body "{}"}
                    {:status 200 :body "[1]"}]]
    (with-redefs [vis/request! (fn [& _]
                                 response)]
      (is (string? (:error (team/fetch! "leader"))))))
  (with-redefs [vis/request! (fn [& _]
                               (throw (java.io.IOException. "offline")))]
    (is (string? (:error (team/fetch! "leader"))))))

(deftest team-row-information-test
  (let [row
        {"session_id" "child"
         "parent_id" "parent"
         "depth" 2
         "task" "Verify routing"
         "status" "budget_limited"
         "model" "small"
         "iterations_used" 4
         "iteration_budget" 4
         "routing_locked" true
         "usage" {"cost_usd" 0.025}}

        label
        (:label (last (team/items [row] "parent")))]

    (is (str/includes? label "budget limited"))
    (is (str/includes? label "4/4"))
    (is (str/includes? label "$0.025"))
    (is (str/includes? label "depth 2"))
    (is (str/includes? label "locked"))
    (is (= :inspect (:action (second (team/items [] "parent")))))))

(def review-agent
  {"session_id" "child"
   "parent_id" "parent"
   "depth" 1
   "task" "Verify routing"
   "status" "running"
   "model" "small"
   "iterations_used" 2
   "iteration_budget" 4
   "routing_locked" true
   "usage" {"cost_usd" 0.025}})

(deftest modal-loading-and-keyboard-test
  (let [snapshot
        (atom {:loading? true})

        c
        (team/component #(deref snapshot) "parent")

        initial
        (:init c)

        loading
        ((:measure c) initial 40 20)]

    (is (str/includes? (:title loading) "Loading"))
    (is (= initial ((:on-key c) initial (KeyStroke. KeyType/Enter) loading)))
    (is (= {::dlg/done nil} ((:on-key c) initial (KeyStroke. KeyType/Escape) loading)))
    (reset! snapshot {:agents [review-agent]})
    (let [geom
          ((:measure c) initial 40 20)

          next
          (reduce (fn [state _]
                    ((:on-key c) state (KeyStroke. KeyType/ArrowDown) geom))
                  initial
                  (range 2))]

      (is (= "parent"
             (:session-id (::dlg/done ((:on-key c) next (KeyStroke. KeyType/Enter) geom))))))
    (reset! snapshot {:agents []})
    (is (some #(str/includes? (:label %) "No subagents yet")
              (:filtered ((:measure c) initial 40 20))))
    (reset! snapshot {:enabled? false})
    (let [disabled ((:measure c) initial 40 20)]
      (is (str/includes? (:title disabled) "Disabled"))
      (is (not-any? :action (:filtered disabled))))))

(deftest inspect-and-confirmed-cancel-test
  (doseq [confirmed? [false true]]
    (let [choices (atom [{:agent review-agent} nil])
          actions (atom [{:action :stop} {:action (if confirmed? :stop :keep)}])
          requests (atom [])]

      (with-redefs [team/choose-team! (fn [& _]
                                        (let [r (first @choices)]
                                          (swap! choices rest)
                                          r))
                    dlg/select-dialog! (fn [& _]
                                         (let [r (first @actions)]
                                           (swap! actions rest)
                                           r))
                    vis/request! (fn [& args]
                                   (swap! requests conj args)
                                   {:status 200})]

        (is (nil? (team/show! nil "leader" nil)))
        (is (= (if confirmed?
                 [[:post "/v1/sessions/leader/agents/cancel" {:body {:session_id "child"}}]]
                 [])
               @requests)))))
  (with-redefs [team/choose-team!
                (fn [& _]
                  {:agent review-agent})

                dlg/select-dialog!
                (fn [& _]
                  {:action :inspect})]

    (is (= "child" (team/show! nil "leader" nil))))
  (with-redefs [team/choose-team! (fn [& _]
                                    {:action :inspect :session-id "parent"})]
    (is (= "parent" (team/show! nil "leader" "parent")))))

(deftest rendered-team-states-and-terminal-parity-test
  (doseq [cols
          [40 80 140]

          [snapshot text]
          [[{:loading? true} "Loading"] [{:agents []} "No subagents"]
           [{:error "Could not load team. Refresh to retry."} "Unavailable"]
           [{:agents [(assoc review-agent "status" "completed")]} "Verify routing"]
           [{:agents [(assoc review-agent "status" "budget_limited")]} "Verify routing"]
           [{:agents [(assoc review-agent "pending_input" true)]} "Verify routing"]]]

    (let [rows
          20

          c
          (team/component (constantly snapshot) "parent")

          s
          (:init c)

          geom
          ((:measure c) s cols rows)

          s
          ((:reconcile c) s geom)

          paint
          (fn [g]
            (p/set-colors! g theme/text-fg theme/terminal-bg)
            (p/fill-rect! g 0 0 cols rows)
            ((:paint c) g s geom))

          view
          (frame/view cols
                      rows
                      (fn [g _]
                        (paint g)))

          html
          (HtmlTerminalView/render view (TerminalSize. cols rows) "Agent team review")

          vt
          (DefaultVirtualTerminal. (TerminalSize. cols rows))

          ht
          (-> (HtmlTerminal/builder)
              (.initialSize (TerminalSize. cols rows))
              (.columnRange cols cols)
              (.rowRange rows rows)
              (.browserResize false)
              (.build))

          vs
          (TerminalScreen. vt)

          hs
          (TerminalScreen. ht)]

      (try (.startScreen vs)
           (.startScreen hs)
           (paint (.newTextGraphics vs))
           (paint (.newTextGraphics hs))
           (let [lines (str/join "\n"
                                 (for [y (range rows)]
                                   (apply str
                                     (for [x (range cols)]
                                       (.getCharacterString (.getBackCharacter vs x y))))))]
             (is (str/includes? lines text)))
           (is (str/includes? html "data-live=\"false\""))
           (doseq [x
                   (range cols)

                   y
                   (range rows)]

             (is (= (.getBackCharacter vs x y) (.getBackCharacter hs x y))))
           (finally (.stopScreen vs) (.stopScreen hs) (.close vt) (.close ht))))))

(deftest compact-and-wide-header-omits-team-status-test
  (doseq [compact?
          [true false]

          improve?
          [true false]]

    (let [panel
          (header/header-actions-component nil false nil improve? compact?)

          html
          (HtmlTerminalView/render panel (.getPreferredSize panel) "Header actions")]

      (is (not (str/includes? html "Agents")))
      (is (not (str/includes? html "Subagent")))
      (is (= (not compact?) (str/includes? html "help (")))
      (is (= improve? (str/includes? html "improve ("))))))

(deftest active-header-refresh-without-inspector-test
  ;; The header used to remain unknown until the inspector was opened.
  (let [start
        (ns-resolve 'com.blockether.vis.tui.agent-team 'start-refresh!)

        changed
        (java.util.concurrent.LinkedBlockingQueue.)

        sid
        (atom "live-header")

        status
        (atom "running")

        calls
        (atom 0)]

    (is (some? start))
    (when start
      (with-redefs [vis/request!
                    (fn [& _]
                      (swap! calls inc)
                      {:status 200
                       :body (str "[{\"session_id\":\"child\",\"task\":\"Work\",\"status\":\""
                                  @status
                                  "\"}]")})]
        (let [stop (start #(deref sid) #(.offer changed true))]
          (try (is (.poll changed 2 java.util.concurrent.TimeUnit/SECONDS))
               (is (= "running" (get-in (team/summary @sid) [:agents 0 "status"])))
               (is (= "Agents 1 active/1 cached" (team/header-label @sid false)))
               (reset! status "completed")
               (is (.poll changed 7 java.util.concurrent.TimeUnit/SECONDS))
               (is (= "completed" (get-in (team/summary @sid) [:agents 0 "status"])))
               (is (= "Agents 0 active/1 cached" (team/header-label @sid false)))
               (reset! sid "other-header")
               (reset! status "cancelled")
               (is (.poll changed 2 java.util.concurrent.TimeUnit/SECONDS))
               (is (= "cancelled" (get-in (team/summary @sid) [:agents 0 "status"])))
               (is (= 3 @calls))
               (finally (stop)))
          (let [stopped-calls @calls]
            (reset! sid "after-stop")
            (is (nil? (.poll changed 1 java.util.concurrent.TimeUnit/SECONDS)))
            (is (= stopped-calls @calls))))))))

(deftest header-observation-states-test
  (doseq [[snapshot label] [[nil "Agents ?"] [{:error "Offline"} "Agents !"]
                            [{:agents [] :checked-at 0} "Agents stale"]
                            [{:agents [] :checked-at (System/currentTimeMillis)}
                             "Agents 0 active/0 cached"]]]
    (with-redefs [team/summary (constantly snapshot)]
      (is (= label (team/header-label "leader" false))))))

(deftest old-tab-response-does-not-repaint-new-tab-test
  (let [entered
        (promise)

        release
        (promise)

        changed
        (java.util.concurrent.LinkedBlockingQueue.)

        sid
        (atom "old-tab")]

    (with-redefs [vis/request! (fn [_ url _]
                                 (when (str/includes? url "old-tab")
                                   (deliver entered true)
                                   (deref release 2000 nil))
                                 {:status 200 :body "[]"})]
      (let [stop (team/start-refresh! #(deref sid) #(.offer changed @sid))]
        (try (is (deref entered 2000 false))
             (reset! sid nil)
             (deliver release true)
             (is (nil? (.poll changed 1 java.util.concurrent.TimeUnit/SECONDS)))
             (reset! sid "new-tab")
             (is (= "new-tab" (.poll changed 2 java.util.concurrent.TimeUnit/SECONDS)))
             (finally (stop)))))))
