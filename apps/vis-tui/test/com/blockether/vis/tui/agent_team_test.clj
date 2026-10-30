(ns com.blockether.vis.tui.agent-team-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.agent-team :as team]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.frame :as frame]
            [com.blockether.vis.tui.interactions :as interactions]
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

(defdescribe
  failed-refresh-keeps-dock-test
  ;; Regression (user report): one failed team refresh hid the agent dock until the next
  ;; refresh. The snapshot dropped the last agent list on a gateway or settings error.
  (it "keeps the last agent list after a failed refresh"
      (let [body (str "[{\"session_id\":\"c1\",\"team_id\":\"turn-1\",\"task\":\"Fix the parser\","
                      "\"status\":\"running\",\"created_at\":0}]")]
        (with-redefs [vis/request! (constantly {:status 200 :body body})]
          (team/fetch! "flaky-leader"))
        (expect (= 1 (team/dock-height "flaky-leader" "turn-1" 120)))
        (with-redefs [vis/request! (fn [& _]
                                     (throw (java.io.IOException. "offline")))]
          (expect (string? (:error (team/fetch! "flaky-leader"))))
          (expect (= 1 (team/dock-height "flaky-leader" "turn-1" 120))
                  "A gateway error keeps the dock")
          (expect (= "Agents !" (team/header-label "flaky-leader" false))
                  "The header still shows the failed refresh"))
        (with-redefs [vis/setting (fn [& _]
                                    (throw (java.io.IOException. "offline")))
                      vis/request! (constantly {:status 503})]

          (team/fetch! "flaky-leader")
          (expect (team/enabled? "flaky-leader") "A settings error keeps Subagents on")
          (expect (= 1 (team/dock-height "flaky-leader" "turn-1" 120))
                  "A settings error keeps the dock")))))

(defdescribe team-row-information-test
             (it "team row information"
                 (let [row
                       {"session_id" "child"
                        "parent_id" "parent"
                        "team_id" "turn-1"
                        "task" "Verify routing"
                        "title" "Verify routing"
                        "status" "budget_limited"
                        "result" "Stopped at the budget"
                        "created_at" 1000
                        "finished_at" 65000}

                       labels
                       (mapv :label (team/items [row] "parent"))]

                   (expect (some #(str/starts-with? % "Turn · 1 agent") labels))
                   (expect (some #(and (str/includes? % "Verify routing")
                                       (str/includes? % "FAILED")
                                       (str/includes? % "Stopped at the budget")
                                       (str/ends-with? % "1m 04s"))
                                 labels))
                   (expect (= :parent (:action (second (team/items [] "parent")))))
                   (expect (some #(str/starts-with? % "AUDIT  #1")
                                 (map :label
                                      (team/items [(dissoc (assoc row "role" "audit") "title")]
                                                  nil)))))))

(def review-agent
  {"session_id" "child"
   "parent_id" "parent"
   "team_id" "turn-1"
   "task" "Verify routing"
   "title" "Verify routing"
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
                    ;; The turn header comes first, then the agent, Refresh team and Open parent.
                    (range 3))]

        (expect (= "child"
                   (:agent-id (::dlg/done ((:on-key c) initial (KeyStroke. KeyType/Enter) geom))))
                "Enter on the first agent row opens its timeline")
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
  (it "opens the timeline of a row, stops it after a confirmation and opens its session"
      (doseq [confirmed? [false true]]
        (let [choices (atom [{:agent-id "child"} nil])
              actions (atom [{:action :stop} {:action (if confirmed? :stop :keep)} nil])
              requests (atom [])]

          (with-redefs [team/choose-team! (fn [& _]
                                            (let [r (first @choices)]
                                              (swap! choices rest)
                                              r))
                        team/fetch! (constantly nil)
                        team/summary (constantly {:agents [review-agent]})
                        dlg/select-dialog! (fn [& _]
                                             (let [r (first @actions)]
                                               (swap! actions rest)
                                               r))
                        vis/request! (fn [& args]
                                       (swap! requests conj args)
                                       {:status 200
                                        :body "{\"session_id\":\"child\",\"items\":[]}"})]

            (expect (nil? (team/show! nil "leader" nil)))
            (expect (= (if confirmed?
                         [[:post "/v1/sessions/leader/agents/cancel" {:body {:session_id "child"}}]]
                         [])
                       (filterv #(= :post (first %)) @requests))))))
      (with-redefs [team/choose-team!
                    (fn [& _]
                      {:agent-id "child"})

                    team/summary
                    (constantly {:agents [review-agent]})

                    vis/request!
                    (constantly {:status 404})

                    dlg/select-dialog!
                    (fn [& _]
                      {:action :inspect})]

        (expect (= "child" (team/show! nil "leader" nil))))
      (with-redefs [team/choose-team! (fn [& _]
                                        {:action :parent :session-id "parent"})]
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
             ;; The status stands at the end of the row and stays in view at 40 columns.
             [{:agents [(assoc review-agent "status" "completed")]} "DONE"]
             [{:agents [(assoc review-agent "status" "budget_limited")]} "FAILED"]
             [{:agents [(assoc review-agent "pending_input" true)]} "RUNNING"]]]

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
  summary-counts-test
  (it
    "counts the rows of each status in a fixed order"
    (doseq [[agents label]
            [[nil ""] [[{"status" "completed"}] "1 DONE"]
             [[{"status" "completed"} {"status" "running" "iterations_used" 3 "iteration_budget" 20}
               {"status" "running" "pending_input" true "pending_input_count" 2}
               {"status" "queued"}] "2 RUNNING · 1 QUEUED · 1 DONE"]
             ;; A question goes to the leader, not to the person, so its agent still runs.
             [[{"status" "running" "role" "verify"} {"status" "running" "question" "Keep it?"}
               {"status" "running" "role" "research"} {"status" "completed" "role" "implement"}]
              "3 RUNNING · 1 DONE"]
             ;; Four status words only: a passed verdict is done; a failed verdict, a cancel and
             ;; a spent budget are failed.
             [[{"status" "completed" "outcome" "passed"} {"status" "completed" "outcome" "failed"}
               {"status" "failed"} {"status" "cancelled"} {"status" "budget_limited"}]
              "1 DONE · 4 FAILED"]]]
      (expect (= label (team/summary-counts (map-indexed #(team/team-row (inc %1) %2) agents)))))))

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
    "iteration_budget" 10}
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

(defdescribe waiting-agents-test
             (it "keeps role order and numbers per team"
                 (expect (= [[1 "c1"] [2 "c2"] [3 "c3"] [1 "c4"]]
                            (mapv (fn [[n agent]]
                                    [n (get agent "session_id")])
                                  (team/numbered waiting-team)))))
             ;; The leader answers questions and grant requests. The person never sees them.
             (it "shows no question or grant request of a waiting agent"
                 (let [rows (team/team-rows "turn-1" waiting-team)]
                   (expect (= ["RUNNING" "RUNNING" "RUNNING"] (mapv :status rows)))
                   (expect (= ["" "" ""] (mapv :now rows))))
                 (with-redefs [team/summary (constantly {:agents waiting-team})]
                   (let [text (str/join "\n" (team/dock-text "leader" nil 120 0))]
                     (doseq [hidden ["asking" "waiting" "Keep the old API?" "Asks for"]]
                       (expect (not (str/includes? text hidden)) hidden))))))

(def ^:private now-ms 1700000100000)

(defn- agent-row
  [n status & {:as more}]
  (merge {"session_id" (str "s" n)
          "parent_id" "leader"
          "leader_id" "leader"
          "team_id" "turn-1"
          "task" (str "Task " n)
          "status" status
          "created_at" (- now-ms 64000)}
         more))

(def ^:private every-state
  "One agent for each status that the design names."
  [(agent-row 1 "queued" "role" "research" "title" "Map the gateway routes")
   (agent-row 2 "running"
              "role" "implement"
              "title" "Fix the parser"
              "activity" "Patched src/a.clj")
   (agent-row 3 "running"
              "role" "research"
              "note" "Two routes left to map"
              "activity" "Read src/routes.clj"
              "question" "Keep the old API?")
   (agent-row 4 "running" "role" "implement" "pending_input" true)
   (agent-row 5 "completed"
              "role" "research"
              "title" "Read docs"
              "result" "Found 3 routes"
              "finished_at" (- now-ms 4000))
   (agent-row 6 "completed"
              "role" "verify"
              "title" "Run tests"
              "outcome" "passed"
              "result" "All 120 tests pass"
              "finished_at" now-ms)
   (agent-row 7 "completed"
              "role" "verify"
              "title" "Check lint"
              "outcome" "failed"
              "result" "2 reflection warnings"
              "finished_at" now-ms)
   (agent-row 8 "failed"
              "role" "audit"
              "title" "Review diff"
              "result" "Provider error"
              "finished_at" now-ms)
   (agent-row 9 "cancelled" "title" "Old task" "finished_at" now-ms)
   (agent-row 10 "budget_limited"
              "role" "implement"
              "title" "Port the theme"
              "resumed_at" (- now-ms 30000)
              "finished_at" now-ms)])

(defdescribe status-word-test
             (it "names each status with one of four words in capitals, without icons"
                 (expect (= ["QUEUED" "RUNNING" "RUNNING" "RUNNING" "DONE" "DONE" "FAILED" "FAILED"
                             "FAILED" "FAILED"]
                            (mapv team/status-word every-state)))))

(defdescribe
  team-rows-test
  (it
    "shows role, title or #N, status, now or result and time in one line"
    (let [rows
          (team/team-rows "turn-1" every-state)

          columns
          (team/row-columns rows)

          lines
          (mapv #(first (team/row-lines % columns 100 now-ms)) rows)]

      (expect (= (range 1 11) (map :n rows)) "Rows keep their start order")
      (expect (str/starts-with? (nth lines 0) "RESEARCH   Map the gateway routes  "))
      ;; The status is the last text column, right before the time.
      (expect (str/ends-with? (nth lines 0) "QUEUED"))
      (expect (not (re-find #"\d+s$" (nth lines 0))) "A queued agent shows no time")
      (expect (re-find #"Patched src/a\.clj +RUNNING +1m 04s$" (nth lines 1)))
      (expect (str/ends-with? (nth lines 1) "1m 04s") "Running time counts from the start")
      (expect (str/includes? (nth lines 2) "#3"))
      ;; The newest note wins over the Activity line. The question goes to the leader only.
      (expect (str/includes? (nth lines 2) "RUNNING  "))
      (expect (str/includes? (nth lines 2) "Two routes left to map"))
      (expect (not (str/includes? (nth lines 2) "Read src/routes.clj")))
      (expect (not (str/includes? (nth lines 2) "Keep the old API?")))
      ;; A form of a subagent opens in the leader session, so its row only runs.
      (expect (str/includes? (nth lines 3) "RUNNING"))
      (expect (not (str/includes? (nth lines 3) "needs you")))
      (expect (str/includes? (nth lines 4) "Found 3 routes"))
      (expect (str/ends-with? (nth lines 4) "1m 00s") "A finished agent shows its duration")
      (expect (str/includes? (nth lines 5) "DONE"))
      (expect (str/includes? (nth lines 5) "All 120 tests pass"))
      (expect (str/includes? (nth lines 6) "FAILED"))
      ;; Without a result, the row text gives the end that `FAILED` hides.
      (expect (re-find #"cancelled +FAILED" (nth lines 8)))
      (expect (str/ends-with? (nth lines 9) "30s") "A resumed task counts from the resume")
      (expect (re-find #"out of budget +FAILED +30s$" (nth lines 9)))
      (expect (every? #(re-matches #"[\x20-\x7e]*" %) lines) "Rows hold no icons or marks")))
  (it "ends a long title and a long result with an ellipsis"
      (let [long-agent
            (agent-row 1 "completed"
                       "title" (apply str (repeat 40 "t"))
                       "result" (apply str (repeat 200 "r"))
                       "finished_at" now-ms)

            row
            (first (team/team-rows "turn-1" [long-agent]))

            line
            (first (team/row-lines row (team/row-columns [row]) 80 now-ms))]

        (expect (str/includes? line (apply str (repeat 40 "t"))))
        (expect (str/includes? line "r…"))
        (expect (<= (count line) 80))))
  (it "takes two lines on a phone width"
      (let [rows
            (team/team-rows "turn-1" every-state)

            columns
            (team/row-columns rows)

            running
            (second rows)

            [head now]
            (team/row-lines running columns 40 now-ms)]

        (expect (team/narrow? 40))
        (expect (re-find #"RUNNING +1m 04s$" head) "The status stands right before the time")
        (expect (str/ends-with? head "1m 04s"))
        (expect (str/starts-with? head "Fix the parser") "The title gets the first line")
        (expect (= "  IMPLEMENT  Patched src/a.clj" now)
                "The role and the text share the second line")
        (expect (= 2 (team/row-line-count (first rows) 40)) "A queued row keeps its role")
        (expect
          (= 1 (team/row-line-count (team/team-row 9 {"status" "completed" "title" "Old task"}) 40))
          "A row without role or text keeps one line"))))

(defdescribe
  team-card-test
  (it "keeps the final state of the team in the turn after the turn ends"
      (with-redefs [team/summary (constantly {:agents (filterv #(not (contains? #{"queued"
                                                                                  "running"}
                                                                                (get % "status")))
                                                        every-state)})]
        (let [messages [{:role :user :session-turn-id "turn-1" :text "go"}
                        {:role :assistant :session-turn-id "turn-1" :text "ok"}
                        {:role :assistant :session-turn-id "turn-3" :text "other"}]
              decorated (team/decorate-messages messages "leader")]

          (expect (= "turn-1" (get-in decorated [1 :agent-team-card :team-id])))
          (expect (nil? (:agent-team-card (nth decorated 0))))
          (expect (nil? (:agent-team-card (nth decorated 2))))
          (expect (identical? (nth decorated 1) (nth (team/decorate-messages messages "leader") 1)))
          (expect (= messages (team/decorate-messages messages "leader" "turn-1"))
                  "The running turn shows its team in the dock, not in the transcript")
          (let [projected (team/with-card-lines (assoc (nth decorated 1)
                                                  :prewrapped-lines ["ok"]
                                                  :line-meta [nil])
                                                100)]
            (expect (= (count (:prewrapped-lines projected)) (count (:line-meta projected))))
            (expect (some #(str/includes? % "All 120 tests pass") (:prewrapped-lines projected)))
            (expect (not-any? #(str/includes? % "Team ·") (:prewrapped-lines projected)))))))
  (it "keeps a team with a working agent in the dock after its turn ends"
      (with-redefs [team/summary (constantly {:agents waiting-team})]
        (let [messages [{:role :assistant :session-turn-id "turn-1" :text "ok"}
                        {:role :assistant :session-turn-id "turn-2" :text "later"}]
              decorated (team/decorate-messages messages "leader")]

          (expect (nil? (:agent-team-card (first decorated))))
          (expect (= "turn-2" (get-in decorated [1 :agent-team-card :team-id])))
          (expect (= #{"turn-1"} (team/dock-team-ids waiting-team nil)))
          (expect (= #{"turn-1" "turn-2"} (team/dock-team-ids waiting-team "turn-2")))))))

(defdescribe
  dock-test
  (it "shows one summary line, and the agent table only after C-x b a"
      (with-redefs [team/summary (constantly {:agents every-state})]
        (expect (not (team/dock-expanded? "leader")))
        (let [[line :as text] (team/dock-text "leader" "turn-1" 120 now-ms)]
          (expect (= 1 (count text)) "The dock is one line")
          (expect (str/starts-with? line "Agents   "))
          (expect (str/includes? line "3 RUNNING"))
          ;; The team time follows the head: from the first spawn to now while agents work.
          (expect (str/starts-with? line "Agents   1m 04s   3 RUNNING"))
          (expect (not (str/includes? line "waiting")))
          (expect (not (str/includes? line "asking")))
          (expect (str/ends-with? line "C-x b a details")))
        (try (expect (true? (team/toggle-dock! "leader")))
             (let [text (team/dock-text "leader" "turn-1" 120 now-ms)]
               (expect (str/ends-with? (first text) "C-x b a hide details"))
               (expect (= 11 (count text)) "The table caps its rows and counts the rest")
               (expect (some #(str/includes? % "+5 more") text))
               (expect (= 11 (team/dock-height "leader" "turn-1" 120))))
             (finally (team/toggle-dock! "leader")))
        (expect (= 1 (team/dock-height "leader" "turn-1" 120)) "C-x b a again hides the table")
        (expect (= ["Agents"] (team/dock-text "leader" "turn-1" 6 now-ms))
                "A narrow line drops the counts and the hint"))
      (with-redefs [team/summary (constantly {:agents (drop 4 every-state)})]
        (expect (zero? (team/dock-height "leader" nil 120)) "No dock after a finished team")
        (expect (= 1 (team/dock-height "leader" "turn-1" 120)) "The running turn keeps its dock")
        ;; A team without a working agent stops its time at the last end.
        (expect (str/starts-with? (first (team/dock-text "leader" "turn-1" 120 (+ now-ms 60000)))
                                  "Agents   1m 04s   ")))
      (with-redefs [team/summary (constantly {:agents []})]
        (expect (zero? (team/dock-height "leader" "turn-1" 120)) "An empty team has no dock"))))

(defdescribe
  dock-scroll-test
  ;; Regression, user report: the agent team in the dock does not scroll, so rows past the cap
  ;; never show.
  (it "scrolls the agent table to the rows past the cap and takes the wheel on every dock line"
      (with-redefs [team/summary (constantly {:agents every-state})]
        (expect (not (team/scroll-dock! "leader" "turn-1" 1)) "A collapsed dock does not move")
        (team/toggle-dock! "leader")
        (try (let [rows (fn []
                          (team/dock-text "leader" "turn-1" 120 now-ms))]
               (expect (not-any? #(str/includes? % "Run tests") (rows)))
               (expect (true? (team/scroll-dock! "leader" "turn-1" 3)))
               (expect (some #(str/includes? % "Run tests") (rows)) "A hidden row comes into view")
               (expect (some #(str/includes? % "↑3 above  +2 more") (rows)))
               (expect (= 11 (team/dock-height "leader" "turn-1" 120)) "Scrolling keeps the height")
               (expect (true? (team/scroll-dock! "leader" "turn-1" 100)))
               (expect (some #(str/includes? % "↑5 above") (rows)))
               (expect (not-any? #(str/includes? % "more") (rows)))
               (expect (false? (team/scroll-dock! "leader" "turn-1" 1))
                       "The last row stops the wheel")
               (expect (true? (team/scroll-dock! "leader" "turn-1" -100)))
               (expect (some #(str/includes? % "+5 more") (rows)))
               (let [terminal (DefaultVirtualTerminal. (TerminalSize. 120 20))
                     screen (doto (TerminalScreen. terminal) (.startScreen))]

                 (binding [interactions/hit-map (interactions/create-hit-map)]
                   (.beginFrame interactions/hit-map)
                   (team/draw-dock! (.newTextGraphics screen) "leader" "turn-1" 2 120)
                   (.commitFrame interactions/hit-map)
                   (doseq [y (range 2 13)]
                     (expect (= "leader" (:dock (.lookup interactions/hit-map (int 40) (int y))))
                             (str "Dock line " y " takes the wheel")))
                   (expect (nil? (:dock (.lookup interactions/hit-map (int 40) (int 13))))))))
             (finally (team/scroll-dock! "leader" "turn-1" -100) (team/toggle-dock! "leader"))))))

(defdescribe child-traffic-test
             (it "hides Council requests that the leader's own agents send"
                 (with-redefs [team/summary (constantly {:agents waiting-team})]
                   (let [ids (team/child-session-ids "leader")
                         council (fn [author]
                                   {:role :user
                                    :request-kind :council
                                    :council {"author_session_id" author}})]

                     (expect (team/child-traffic? (council "c2") ids))
                     (expect (not (team/child-traffic? (council "peer") ids)) "Peer messages stay")
                     (expect (not (team/child-traffic? {:role :user :text "hi"} ids)))
                     (expect (not (team/child-traffic? (council "c2") #{})))))))

(defdescribe
  timeline-test
  (it "reads the timeline route and lists its steps with times"
      (let [calls
            (atom [])

            body
            (str "{\"session_id\":\"s 1\",\"items\":["
                 "{\"at\":1700000000000,\"kind\":\"started\",\"text\":\"Fix the parser\"},"
                 "{\"at\":1700000005000,\"kind\":\"note\",\"text\":\"Read tests\"},"
                 "{\"at\":1700000064000,\"kind\":\"finished\",\"status\":\"completed\","
                 "\"outcome\":\"passed\",\"text\":\"All pass\"}]}")]

        (with-redefs [vis/request! (fn [method url _]
                                     (swap! calls conj [method url])
                                     {:status 200 :body body})]
          (let [{:keys [items]} (team/timeline! "leader" "s 1")
                utc (java.time.ZoneId/of "UTC")]

            (expect (= [[:get "/v1/sessions/leader/agents/s+1/timeline"]] @calls))
            (expect (= ["22:13:20  started  Fix the parser" "22:13:25  note  Read tests"
                        "22:14:24  DONE  All pass"]
                       (mapv #(team/timeline-line % utc) items)))
            (expect (= "22:14:24  FAILED  out of budget"
                       (team/timeline-line
                         {"at" 1700000064000 "kind" "finished" "status" "budget_limited"}
                         utc)))
            (expect (= [:inspect :stop nil nil nil nil]
                       (mapv :action (team/timeline-items {"status" "running"} {:items items}))))))
        (with-redefs [vis/request! (constantly {:status 404})]
          (expect (string? (:error (team/timeline! "leader" "s1"))))
          (expect (some #(= "No steps yet" (:label %))
                        (team/timeline-items {"status" "completed"} {:items []})))))))

(defdescribe
  child-breadcrumb-test
  (it "names the path from the leader to the child session"
      (with-redefs [team/summary (constantly {:agents waiting-team})]
        (expect (= {:parent-id "leader" :text "Leader → RESEARCH #1"} (team/breadcrumb "c1")))
        (with-redefs [team/summary (constantly {:agents
                                                (assoc-in waiting-team [1 "title"] "Fix parser")})]
          (expect (= "Leader → IMPLEMENT Fix parser" (:text (team/breadcrumb "c2")))))
        (expect (nil? (team/breadcrumb "unknown"))))))
