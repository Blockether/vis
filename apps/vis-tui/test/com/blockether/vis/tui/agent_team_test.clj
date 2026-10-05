(ns com.blockether.vis.tui.agent-team-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.agent-team :as team]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.frame :as frame]
            [com.blockether.vis.tui.header :as header]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as theme]
            [lazytest.core :as lt :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna TerminalSize]
           [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.html HtmlTerminal HtmlTerminalView]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(lt/set-ns-context! [(lt/around-each [f]
                                     (with-redefs [vis/setting (constantly {"enabled" true})]
                                       (f)))])

(defdescribe
  feature-opt-in-controls-fetches-and-cached-availability-test
  (it "feature opt in controls fetches and cached availability"
      (let [enabled
            (atom false)

            requests
            (atom 0)]

        (with-redefs [vis/setting
                      (fn [id _target]
                        (expect (= "subagents" id))
                        {"enabled" @enabled})

                      vis/request!
                      (fn [& _]
                        (swap! requests inc)
                        {:status 200 :body "[]"})]

          (expect (not (team/enabled? "experimental-team")))
          (team/fetch! "experimental-team")
          (expect (zero? @requests))
          (expect (not (team/enabled? "experimental-team")))
          (reset! enabled true)
          (team/fetch! "experimental-team")
          (expect (team/enabled? "experimental-team"))
          (expect (= 1 @requests))
          (reset! enabled false)
          (team/fetch! "experimental-team")
          (expect (not (team/enabled? "experimental-team")))
          (expect (= 1 @requests)))
        (with-redefs [vis/setting (fn [& _]
                                    (throw (java.io.IOException. "offline")))]
          (team/fetch! "experimental-team")
          (expect (not (team/enabled? "experimental-team")))))))

(defdescribe subagents-follow-the-session-scope-test
             ;; Regression, issue #311: a project `.vis/config.yml` overlay can turn Subagents on
             ;; for its sessions. The team header read the GLOBAL value instead.
             (it "reads Subagents for the session, not the global value"
                 (let [reads (atom [])]
                   (with-redefs [vis/setting (fn [id target]
                                               (swap! reads conj [id target])
                                               {"enabled" (= "session" (:scope target))})
                                 vis/request! (fn [& _]
                                                {:status 200 :body "[]"})]

                     (team/fetch! "project-session")
                     (expect (team/enabled? "project-session"))
                     (expect (= [["subagents" {:scope "session" :target-id "project-session"}]]
                                @reads))))))
(defdescribe read-errors-are-recoverable-test
             (it "read errors are recoverable"
                 (doseq [response [{:status 503} {:status 200 :body "not json"}
                                   {:status 200 :body "{}"} {:status 200 :body "[1]"}]]
                   (with-redefs [vis/request! (fn [& _]
                                                response)]
                     (expect (string? (:error (team/fetch! "leader"))))))
                 (with-redefs [vis/request! (fn [& _]
                                              (throw (java.io.IOException. "offline")))]
                   (expect (string? (:error (team/fetch! "leader")))))))

(defdescribe team-row-information-test
             (it "team row information"
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

                   (expect (str/includes? label "budget limited"))
                   (expect (str/includes? label "4/4"))
                   (expect (str/includes? label "$0.025"))
                   (expect (str/includes? label "depth 2"))
                   (expect (str/includes? label "locked"))
                   (expect (= :inspect (:action (second (team/items [] "parent"))))))))

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

(defdescribe
  modal-loading-and-keyboard-test
  (it
    "modal loading and keyboard"
    (let [snapshot
          (atom {:loading? true})

          c
          (team/component #(deref snapshot) "parent")

          initial
          (:init c)

          loading
          ((:measure c) initial 40 20)]

      (expect (str/includes? (:title loading) "Loading"))
      (expect (= initial ((:on-key c) initial (KeyStroke. KeyType/Enter) loading)))
      (expect (= {::dlg/done nil} ((:on-key c) initial (KeyStroke. KeyType/Escape) loading)))
      (reset! snapshot {:agents [review-agent]})
      (let [geom
            ((:measure c) initial 40 20)

            next
            (reduce (fn [state _]
                      ((:on-key c) state (KeyStroke. KeyType/ArrowDown) geom))
                    initial
                    (range 2))]

        (expect (= "parent"
                   (:session-id (::dlg/done ((:on-key c) next (KeyStroke. KeyType/Enter) geom))))))
      (reset! snapshot {:agents []})
      (expect (some #(str/includes? (:label %) "No subagents yet")
                    (:filtered ((:measure c) initial 40 20))))
      (reset! snapshot {:enabled? false})
      (let [disabled ((:measure c) initial 40 20)]
        (expect (str/includes? (:title disabled) "Disabled"))
        (expect (not-any? :action (:filtered disabled)))))))

(defdescribe
  inspect-and-confirmed-cancel-test
  (it "inspect and confirmed cancel"
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

            (expect (nil? (team/show! nil "leader" nil)))
            (expect (= (if confirmed?
                         [[:post "/v1/sessions/leader/agents/cancel" {:body {:session_id "child"}}]]
                         [])
                       @requests)))))
      (with-redefs [team/choose-team!
                    (fn [& _]
                      {:agent review-agent})

                    dlg/select-dialog!
                    (fn [& _]
                      {:action :inspect})]

        (expect (= "child" (team/show! nil "leader" nil))))
      (with-redefs [team/choose-team! (fn [& _]
                                        {:action :inspect :session-id "parent"})]
        (expect (= "parent" (team/show! nil "leader" "parent"))))))

(defdescribe
  rendered-team-states-and-terminal-parity-test
  (it
    "rendered team states and terminal parity"
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
               (expect (str/includes? lines text)))
             (expect (str/includes? html "data-live=\"false\""))
             (doseq [x
                     (range cols)

                     y
                     (range rows)]

               (expect (= (.getBackCharacter vs x y) (.getBackCharacter hs x y))))
             (finally (.stopScreen vs) (.stopScreen hs) (.close vt) (.close ht)))))))

(defdescribe compact-and-wide-header-omits-team-status-test
             (it
               "compact and wide header omits team status"
               (doseq [compact?
                       [true false]

                       improve?
                       [true false]]

                 (let [panel
                       (header/header-actions-component nil false nil improve? compact?)

                       html
                       (HtmlTerminalView/render panel (.getPreferredSize panel) "Header actions")]

                   (expect (not (str/includes? html "Agents")))
                   (expect (not (str/includes? html "Subagent")))
                   (expect (= (not compact?) (str/includes? html "help (")))
                   (expect (= improve? (str/includes? html "improve (")))))))

(defdescribe
  active-header-refresh-without-inspector-test
  (it "active header refresh without inspector"
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

        (expect (some? start))
        (when start
          (with-redefs [vis/request!
                        (fn [& _]
                          (swap! calls inc)
                          {:status 200
                           :body (str "[{\"session_id\":\"child\",\"task\":\"Work\",\"status\":\""
                                      @status
                                      "\"}]")})]
            (let [stop (start #(deref sid) #(.offer changed true))]
              (try (expect (.poll changed 2 java.util.concurrent.TimeUnit/SECONDS))
                   (expect (= "running" (get-in (team/summary @sid) [:agents 0 "status"])))
                   (expect (= "Agents 1 active/1 cached" (team/header-label @sid false)))
                   (reset! status "completed")
                   (expect (.poll changed 7 java.util.concurrent.TimeUnit/SECONDS))
                   (expect (= "completed" (get-in (team/summary @sid) [:agents 0 "status"])))
                   (expect (= "Agents 0 active/1 cached" (team/header-label @sid false)))
                   (reset! sid "other-header")
                   (reset! status "cancelled")
                   (expect (.poll changed 2 java.util.concurrent.TimeUnit/SECONDS))
                   (expect (= "cancelled" (get-in (team/summary @sid) [:agents 0 "status"])))
                   (expect (= 3 @calls))
                   (finally (stop)))
              (let [stopped-calls @calls]
                (reset! sid "after-stop")
                (expect (nil? (.poll changed 1 java.util.concurrent.TimeUnit/SECONDS)))
                (expect (= stopped-calls @calls)))))))))

(defdescribe header-observation-states-test
             (it "header observation states"
                 (doseq [[snapshot label] [[nil "Agents ?"] [{:error "Offline"} "Agents !"]
                                           [{:agents [] :checked-at 0} "Agents stale"]
                                           [{:agents [] :checked-at (System/currentTimeMillis)}
                                            "Agents 0 active/0 cached"]]]
                   (with-redefs [team/summary (constantly snapshot)]
                     (expect (= label (team/header-label "leader" false)))))))

(defdescribe old-tab-response-does-not-repaint-new-tab-test
             (it "old tab response does not repaint new tab"
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
                       (try (expect (deref entered 2000 false))
                            (reset! sid nil)
                            (deliver release true)
                            (expect (nil? (.poll changed 1 java.util.concurrent.TimeUnit/SECONDS)))
                            (reset! sid "new-tab")
                            (expect (= "new-tab"
                                       (.poll changed 2 java.util.concurrent.TimeUnit/SECONDS)))
                            (finally (stop))))))))
