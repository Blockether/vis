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
                   (expect (str/includes? label "locked"))
                   (expect (= :inspect (:action (second (team/items [] "parent")))))
                   (expect (str/starts-with? (:label (last (team/items [(assoc row "role" "audit")]
                                                                       nil)))
                                             "audit · ")))))

(def review-agent
  {"session_id" "child"
   "parent_id" "parent"
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

(defdescribe
  footer-team-chip-test
  (it "names each agent with its state and leaves while no agent works or waits"
      (doseq [[agents label]
              [[nil nil] [[{"status" "completed"}] nil]
               [[{"status" "completed"}
                 {"status" "running" "iterations_used" 3 "iteration_budget" 20}
                 {"status" "running" "pending_input" true "pending_input_count" 2}
                 {"status" "queued"}] "Agents: A3 asks you (2) · A1 done · A2 3/20 · A4 queued"]
               [[{"status" "running" "role" "verify" "iterations_used" 1 "iteration_budget" 9}
                 {"status" "queued"}
                 {"status" "running" "role" "research" "iterations_used" 2 "iteration_budget" 9}
                 {"status" "completed" "role" "implement"}]
                "Agents: A3 2/9 · A4 done · A1 1/9 · A2 queued"]]]
        (with-redefs [team/summary (constantly {:agents agents})]
          (expect (= label (team/footer-label "leader")))))))

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

(def ^:private waiting-team
  [{"session_id" "c1"
    "parent_id" "leader"
    "leader_id" "leader"
    "team_id" "turn-1"
    "task" "Read docs"
    "status" "running"
    "role" "research"
    "iterations_used" 4
    "iteration_budget" 10
    "activity" "Patched ~/src/a.clj"}
   {"session_id" "c2"
    "parent_id" "leader"
    "leader_id" "leader"
    "team_id" "turn-1"
    "task" "Fix parser"
    "status" "running"
    "role" "implement"
    "iterations_used" 2
    "iteration_budget" 10
    "question" "Keep the old API?"
    "question_entry_id" 30}
   {"session_id" "c3"
    "parent_id" "leader"
    "leader_id" "leader"
    "team_id" "turn-1"
    "task" "Run tests"
    "status" "running"
    "role" "verify"
    "iterations_used" 10
    "iteration_budget" 10
    "budget_request" {"iterations" 20 "reason" "More tests"}
    "question_entry_id" 12}
   {"session_id" "c4"
    "parent_id" "leader"
    "leader_id" "leader"
    "team_id" "turn-2"
    "task" "Later team"
    "status" "completed"
    "role" "audit"}])

(defn- key-of
  ([ch] (KeyStroke. (char ch) false false))
  ([ch ctrl?] (KeyStroke. (char ch) (boolean ctrl?) false)))

(defdescribe waiting-agents-first-test
             (it "puts waiting agents first, keeps numbers per team and says asks you in the footer"
                 (with-redefs [team/summary (constantly {:agents waiting-team})]
                   (expect (= [[2 "c2"] [3 "c3"] [1 "c1"] [1 "c4"]]
                              (mapv (fn [[n agent]]
                                      [n (get agent "session_id")])
                                    (team/numbered waiting-team))))
                   (expect (= "Agents: A2 asks you · A3 asks you · A1 4/10"
                              (team/footer-label "leader"))))))

(defdescribe
  team-card-test
  (it "draws one row per agent of the team with state, bar and activity"
      (let [team-card
            (team/card "turn-1" waiting-team)

            rows
            (:rows team-card)]

        (expect (= "Team · 3 agents · 1 running · 2 asks you" (team/card-title team-card)))
        ;; Every part starts in the same column on each row, also for a longer state or count.
        (expect (= ["A2  implement  asks you  ────────── 2/10   Fix parser"
                    "A3  verify     asks you  ────────── 10/10  Run tests"
                    "A1  research   running   ────────── 4/10   Read docs"]
                   (mapv team/card-row-text rows)))
        ;; The used part of the bar has its own colour, not its own glyph.
        (expect (= [2 10 4] (mapv #(#'team/bar-done (:used %) (:budget %)) rows)))
        (expect (zero? (#'team/bar-done 3 nil)))
        (expect (= 1 (count (set (map #(str/index-of (team/card-row-text %) (:task %)) rows)))))
        (expect (= "└ Patched ~/src/a.clj" (team/card-detail-text (nth rows 2))))
        (expect (= "└ waiting for you · see Questions below" (team/card-detail-text (first rows))))
        (expect (nil? (team/card "turn-9" waiting-team)))
        (let [lines
              (map :line (team/card-entries team-card))

              row
              (first (filter #(str/includes? % "Read docs") lines))

              detail
              (first (filter #(str/includes? % "Patched") lines))]

          (expect (= (str/index-of row "running") (str/index-of detail "└"))
                  "The detail row starts under the state column"))))
  (it "decorates only the last assistant message of the team turn"
      (with-redefs [team/summary (constantly {:agents waiting-team})]
        (let [messages [{:role :user :session-turn-id "turn-1" :text "go"}
                        {:role :assistant :session-turn-id "turn-1" :text "ok"}
                        {:role :assistant :session-turn-id "turn-3" :text "other"}]
              decorated (team/decorate-messages messages "leader")]

          (expect (= "turn-1" (get-in decorated [1 :agent-team-card :team-id])))
          (expect (nil? (:agent-team-card (nth decorated 0))))
          (expect (nil? (:agent-team-card (nth decorated 2))))
          (expect (identical? (nth decorated 1) (nth (team/decorate-messages messages "leader") 1)))
          (let [projected (team/with-card-lines (assoc (nth decorated 1)
                                                  :prewrapped-lines ["ok"]
                                                  :line-meta [nil]))]
            (expect (= (count (:prewrapped-lines projected)) (count (:line-meta projected))))
            (expect (some #(str/includes? % "Team · 3 agents") (:prewrapped-lines projected))))))))

(defdescribe
  questions-inbox-test
  (it
    "orders requests oldest first and answers with the actions of each kind"
    (let [posts (atom [])]
      (with-redefs [team/summary (constantly {:agents waiting-team})
                    vis/worker-future (fn [_ f]
                                        (f))
                    team/fetch! (fn [_]
                                  nil)
                    vis/request! (fn [method path opts]
                                   (swap! posts conj [method path (:body opts)])
                                   {:status 200 :body "[]"})]

        (let [sid (str "inbox-" (random-uuid))
              changed (atom 0)
              opts {:changed! #(swap! changed inc)
                    :open-session! identity
                    :input-text (constantly " Yes, keep it ")
                    :reset-input! (constantly nil)}]

          (expect (= [:budget :question] (mapv :kind (team/requests sid))))
          (expect (pos? (team/inbox-height sid 80)))
          (expect (not (team/inbox-owns-key? sid (key-of \1))))
          (expect (= " Questions · 1 of 2 " (:title (team/inbox-view sid 80))))
          (expect (str/includes? (:head (team/inbox-view sid 80)) "NEW"))
          (expect (team/focus-inbox! sid))
          (expect (not (str/includes? (:head (team/inbox-view sid 80)) "NEW")))
          (expect (= "1 Grant +20 · 2 Deny · C-d deny · Tab next · Esc later"
                     (:hint (team/inbox-view sid 80))))
          (expect (team/inbox-owns-key? sid (key-of \1)))
          (expect (not (team/inbox-owns-key? sid (key-of \a))))
          (team/inbox-key! sid (key-of \1) opts)
          (expect (= [[:post "/v1/sessions/" "/agents/extend" {:session_id "c3" :iterations 20}]]
                     (mapv (fn [[m p b]]
                             [m (subs p 0 13) (subs p (str/last-index-of p "/agents")) b])
                           @posts)))
          (team/focus-inbox! sid)
          (team/inbox-key! sid (KeyStroke. KeyType/Tab) opts)
          (expect (= "Fix parser" (get-in (nth (team/requests sid) 1) [:agent "task"])))
          (team/inbox-key! sid (key-of \1) opts)
          (expect (team/inbox-owns-key? sid (KeyStroke. KeyType/Enter)))
          (expect (not (team/inbox-owns-key? sid (key-of \x))))
          (team/inbox-key! sid (KeyStroke. KeyType/Enter) opts)
          (expect (= {:session_id "c2" :text "Yes, keep it"} (last (map last @posts))))
          (expect (str/ends-with? (second (last @posts)) "/agents/answer"))
          (expect (not (team/inbox-owns-key? sid (KeyStroke. KeyType/Enter))))
          (team/focus-inbox! sid)
          (team/inbox-key! sid (KeyStroke. KeyType/Escape) opts)
          (expect (not (team/inbox-owns-key? sid (key-of \1))))
          (expect (pos? @changed)))))))

(defdescribe child-breadcrumb-test
             (it "names the path from the leader to the child session"
                 (with-redefs [team/summary (constantly {:agents waiting-team})]
                   (expect (= {:parent-id "leader" :text "Leader → A1 research · Read docs"}
                              (team/breadcrumb "c1")))
                   (expect (nil? (team/breadcrumb "unknown"))))))
