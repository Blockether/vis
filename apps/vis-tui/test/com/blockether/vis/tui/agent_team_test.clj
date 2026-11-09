(ns com.blockether.vis.tui.agent-team-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.tui.agent-team :as team]
            [com.blockether.vis.tui.chat :as chat]
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

(defn- apply-work!
  "Do the queued team work of `sid` as the refresh worker does. Returns the new snapshot."
  [sid]
  (#'team/sync! sid (#'team/take-work! sid)))

(defn- no-team-read
  "A `vis/request!` that fails the test: nothing reads the team from the gateway."
  [& args]
  (throw (ex-info "No team read" {:args args})))

(defdescribe feature-opt-in-controls-cached-availability-test
             (it "shows the ready frame rows only while Subagents is on"
                 (let [enabled
                       (atom false)

                       rows
                       [{"session_id" "c" "task" "Work" "status" "running"}]]

                   (with-redefs [vis/setting
                                 (fn [id _target]
                                   (expect (= "subagents" id))
                                   {"enabled" @enabled})

                                 vis/request!
                                 no-team-read]

                     (team/stream-ready! "experimental-team" rows)
                     (apply-work! "experimental-team")
                     (expect (not (team/enabled? "experimental-team")))
                     (expect (= [] (:agents (team/summary "experimental-team"))))
                     (reset! enabled true)
                     (team/settings-changed! "experimental-team")
                     (apply-work! "experimental-team")
                     (expect (team/enabled? "experimental-team"))
                     (expect (= rows (:agents (team/summary "experimental-team"))))
                     (reset! enabled false)
                     (team/settings-changed! "experimental-team")
                     (apply-work! "experimental-team")
                     (expect (not (team/enabled? "experimental-team")))))))

(defdescribe subagents-follow-the-session-scope-test
             ;; Regression, issue #311: a project `.vis/config.yml` overlay can turn Subagents on
             ;; for its sessions. The team header read the GLOBAL value instead.
             (it "reads Subagents for the session, not the global value"
                 (let [reads (atom [])]
                   (with-redefs [vis/setting (fn [id target]
                                               (swap! reads conj [id target])
                                               {"enabled" (= "session" (:scope target))})]
                     (team/settings-changed! "project-session")
                     (apply-work! "project-session")
                     (expect (team/enabled? "project-session"))
                     (expect (= [["subagents" {:scope "session" :target-id "project-session"}]]
                                @reads))))))

(defdescribe failed-setting-read-keeps-dock-test
             ;; Regression (user report): one failed refresh hid the agent dock. The snapshot dropped the
             ;; last agent list on a settings error.
             (it "keeps the agent list and Subagents on after a failed setting read"
                 (team/stream-ready! "flaky-leader"
                                     [{"session_id" "c1"
                                       "team_id" "turn-1"
                                       "task" "Fix the parser"
                                       "status" "running"
                                       "created_at" 0}])
                 (apply-work! "flaky-leader")
                 (expect (= 2 (team/dock-height "flaky-leader" "turn-1" 120)))
                 (with-redefs [vis/setting (fn [& _]
                                             (throw (java.io.IOException. "offline")))]
                   (team/settings-changed! "flaky-leader")
                   (apply-work! "flaky-leader")
                   (expect (team/enabled? "flaky-leader") "A settings error keeps Subagents on")
                   (expect (= 2 (team/dock-height "flaky-leader" "turn-1" 120))
                           "A settings error keeps the dock"))))

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
                   (expect (some #(str/starts-with? % "AUDIT      #1")
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
                "Enter on the first agent row opens its session")
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
  open-row-test
  ;; User report 2026-11-03: a row opened a timeline sheet first, not the session of its agent.
  (it "opens the session of a chosen row with no dialog between"
      (with-redefs [team/choose-team!
                    (fn [& _]
                      {:agent-id "child"})

                    dlg/select-dialog!
                    (fn [& _]
                      (throw (ex-info "No dialog may open" {})))]

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

(defdescribe refresh-repaint-test
             ;; Report: an idle TUI drew a full frame every 5 s, after each unchanged team refresh.
             (let [shown-change?
                   @(ns-resolve 'com.blockether.vis.tui.agent-team 'shown-change?)

                   idle
                   {:agents [{"session_id" "c" "task" "Work" "status" "completed"}] :enabled? true}

                   fresh
                   (assoc idle :checked-at 1000)]

               (it "repaints after the first refresh" (expect (true? (shown-change? nil idle))))
               (it "draws nothing for an unchanged idle team"
                   (expect (false? (shown-change? fresh (assoc idle :checked-at 2000)))))
               (it "repaints when the team changes"
                   (expect (true? (shown-change? fresh (assoc idle :agents [])))))
               (it "repaints a stale header that the refresh makes fresh"
                   (expect (true? (shown-change? (assoc fresh :stale? true) idle))))
               (it "repaints an active agent for its elapsed time"
                   (let [active {:agents [{"session_id" "c" "task" "Work" "status" "running"}]
                                 :enabled? true}]
                     (expect (true? (shown-change? (assoc active :checked-at 1000) active)))))))

(defdescribe
  active-header-refresh-without-inspector-test
  (it "active header refresh without inspector"
      ;; The header used to remain unknown until the inspector was opened.
      (let [changed
            (java.util.concurrent.LinkedBlockingQueue.)

            sid
            (atom "live-header")

            row
            (fn [s]
              {"session_id" "child" "task" "Work" "status" s})]

        (with-redefs [vis/request! no-team-read]
          ;; Before the worker starts, so its first change has the ready rows.
          (team/stream-ready! @sid [(row "running")])
          (let [stop (team/start-refresh! #(deref sid) #(.offer changed true))]
            (try (expect (.poll changed 2 java.util.concurrent.TimeUnit/SECONDS))
                 (expect (= "running" (get-in (team/summary @sid) [:agents 0 "status"])))
                 (expect (= "Agents 1 active/1 cached" (team/header-label @sid false)))
                 (.clear changed)
                 (team/team-updated! @sid
                                     {:team {"active" 0 "total" 1} :agents [(row "completed")]})
                 (expect (.poll changed 2 java.util.concurrent.TimeUnit/SECONDS))
                 (expect (= "completed" (get-in (team/summary @sid) [:agents 0 "status"])))
                 (expect (= "Agents 0 active/1 cached" (team/header-label @sid false)))
                 (team/stream-ready! "other-header" [(row "cancelled")])
                 (reset! sid "other-header")
                 (expect (.poll changed 2 java.util.concurrent.TimeUnit/SECONDS))
                 (expect (= "cancelled" (get-in (team/summary @sid) [:agents 0 "status"])))
                 (finally (stop)))
            (.clear changed)
            (reset! sid "after-stop")
            (team/stream-ready! "after-stop" [(row "running")])
            (expect (nil? (.poll changed 1 java.util.concurrent.TimeUnit/SECONDS))))))))

(defdescribe header-observation-states-test
             (it "header observation states"
                 ;; An old snapshot is current while its event stream is connected. Only a dropped stream
                 ;; makes it stale, until the next read.
                 (doseq [[snapshot label] [[nil "Agents ?"]
                                           [{:agents [] :checked-at 0 :stale? true} "Agents stale"]
                                           [{:agents [] :checked-at 0} "Agents 0 active/0 cached"]]]
                   (with-redefs [team/summary (constantly snapshot)]
                     (expect (= label (team/header-label "leader" false)))))))

(defdescribe
  team-update-merge-test
  ;; Contract fixture team-updated.json: the gateway pushes only the changed rows. A wrong
  ;; merge moves, doubles or drops agent rows in the dock.
  (it "applies the session.team_updated rows of the contract fixture"
      (let [{:strs [base events merged]}
            (json/read-json (slurp (io/resource "vis-contract/fixtures/team-updated.json")))]
        (expect (= merged (reduce #(team/merge-agents %1 (get %2 "agents")) base events))))))

(defdescribe
  summary-counts-test
  (it
    "counts the rows of each status in a fixed order"
    (doseq [[agents label]
            [[nil ""] [[{"status" "completed"}] "• 1 DONE"]
             [[{"status" "completed"} {"status" "running" "iterations_used" 3 "iteration_budget" 20}
               {"status" "running" "pending_input" true "pending_input_count" 2}
               {"status" "queued"}] "• 2 RUNNING  • 1 QUEUED  • 1 DONE"]
             ;; A question goes to the leader, not to the person, so its agent still runs.
             [[{"status" "running" "role" "verify"} {"status" "running" "question" "Keep it?"}
               {"status" "running" "role" "research"} {"status" "completed" "role" "implement"}]
              "• 3 RUNNING  • 1 DONE"]
             ;; Four status words only: a passed verdict is done; a failed verdict, a cancel and
             ;; a spent budget are failed.
             [[{"status" "completed" "outcome" "passed"} {"status" "completed" "outcome" "failed"}
               {"status" "failed"} {"status" "cancelled"} {"status" "budget_limited"}]
              "• 1 DONE  • 4 FAILED"]]]
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

                   (with-redefs [vis/setting (fn [_ target]
                                               (when (= "old-tab" (:target-id target))
                                                 (deliver entered true)
                                                 (deref release 2000 nil))
                                               {"enabled" true})]
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

;; User report: the titles of an AUDIT team and an IMPLEMENT team did not line up.
(defdescribe
  role-column-test
  (it "gives the role column one width in every team"
      (let [title-at (fn [role]
                       (let [rows (team/team-rows
                                    "turn-1"
                                    [(agent-row 1 "running" "role" role "title" "Report")])]
                         [(some #(str/index-of % "Report") (team/table-text rows 120 now-ms))
                          (str/index-of
                            (first (team/row-lines (first rows) (team/row-columns rows) 100 now-ms))
                            "Report")]))]
        (expect (= (title-at "audit") (title-at "implement"))))))

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
          (expect (= #{"turn-1" "turn-2"} (team/dock-team-ids waiting-team "turn-2"))))))
  ;; User report 2026-11-05: the card came after the rest of the transcript, when the team read
  ;; ended. The transcript turn holds its team rows, so the card needs no read.
  (it "paints the team that the transcript turn holds before any team read"
      (let [row
            {"session_id" "c1"
             "parent_id" "leader"
             "leader_id" "leader"
             "team_id" "turn-1"
             "title" "Contract review"
             "task" "Review"
             "status" "completed"}

            messages
            (@#'chat/turns->messages
             [{"turn_id" "turn-1"
               "request" "go"
               "status" "completed"
               "created_at" 1
               "content" [{"type" "markdown" "markdown" "ok"}]
               "agents" [row]}])]

        (with-redefs [team/summary (constantly nil)]
          (let [card (some :agent-team-card (team/decorate-messages messages "leader"))]
            (expect (= "turn-1" (:team-id card)))
            (expect (= ["c1"] (mapv :session-id (:rows card))))))
        (with-redefs [team/summary (constantly {:agents [(assoc row "result" "All keys pass")]})]
          (expect (= ["All keys pass"]
                     (mapv :now
                           (:rows (some :agent-team-card
                                        (team/decorate-messages messages "leader")))))
                  "A row that the team read gives replaces the stored row")))))

(defdescribe
  dock-test
  (it "shows one summary line, and the agent table only after C-x b a"
      (with-redefs [team/summary (constantly {:agents every-state})]
        (expect (not (team/dock-expanded? "leader")))
        (let [[line :as text] (team/dock-text "leader" "turn-1" 120 now-ms)]
          (expect (= 2 (count text)) "The dock is one line and a blank line")
          ;; User report 2026-10-31: the dock touched the C-g row under it.
          (expect (= "" (peek text)) "A blank line parts the dock from the row below")
          (expect (str/starts-with? line "Agents   "))
          (expect (str/includes? line "3 RUNNING"))
          ;; The team time follows the head: from the first spawn to now while agents work.
          (expect (str/starts-with? line "Agents   1m 04s   • 3 RUNNING"))
          (expect (not (str/includes? line "waiting")))
          (expect (not (str/includes? line "asking")))
          (expect (str/ends-with? line "C-x b a details")))
        (try (expect (true? (team/toggle-dock! "leader")))
             (let [text (team/dock-text "leader" "turn-1" 120 now-ms)]
               (expect (str/ends-with? (first text) "C-x b a hide details"))
               (expect (= 12 (count text)) "The table caps its rows and counts the rest")
               (expect (some #(str/includes? % "+5 more") text))
               (expect (= 12 (team/dock-height "leader" "turn-1" 120))))
             (finally (team/toggle-dock! "leader")))
        (expect (= 2 (team/dock-height "leader" "turn-1" 120)) "C-x b a again hides the table")
        (expect (= ["Agents" ""] (team/dock-text "leader" "turn-1" 6 now-ms))
                "A narrow line drops the counts and the hint"))
      (with-redefs [team/summary (constantly {:agents (drop 4 every-state)})]
        (expect (zero? (team/dock-height "leader" nil 120)) "No dock after a finished team")
        (expect (= 2 (team/dock-height "leader" "turn-1" 120)) "The running turn keeps its dock")
        ;; A team without a working agent stops its time at the last end.
        (expect (str/starts-with? (first (team/dock-text "leader" "turn-1" 120 (+ now-ms 60000)))
                                  "Agents   1m 04s   ")))
      (with-redefs [team/summary (constantly {:agents []})]
        (expect (zero? (team/dock-height "leader" "turn-1" 120)) "An empty team has no dock"))))

(defdescribe
  dock-keys-test
  ;; User report 2026-10-31: the TUI had no keys to open one subagent from the dock.
  (it "shows a digit on the first nine dock rows, and the digit names the agent of its row"
      (with-redefs [team/summary (constantly {:agents every-state})]
        (team/toggle-dock! "leader")
        (try (let [text (team/dock-text "leader" "turn-1" 120 now-ms)
                   row-1 (first (filter #(str/starts-with? % "│ 1 │") text))]

               (expect (some? row-1) "Row 1 shows its digit")
               (expect (str/includes? row-1 "Map the gateway routes"))
               (expect (= "s1" (team/dock-agent-id "leader" "turn-1" 1)))
               (expect (nil? (team/dock-agent-id "leader" "turn-1" 10)) "Row 10 has no digit"))
             (finally (team/toggle-dock! "leader")))))
  (it "opens no agent without a dock"
      (with-redefs [team/summary (constantly {:agents (drop 4 every-state)})]
        (expect (nil? (team/dock-agent-id "leader" nil 1))))))

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
               (expect (= 12 (team/dock-height "leader" "turn-1" 120)) "Scrolling keeps the height")
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
                   (let [ids (team/child-session-ids "leader" [])
                         council (fn [author]
                                   {:role :user
                                    :request-kind :council
                                    :council {"author_session_id" author}})]

                     (expect (team/child-traffic? (council "c2") ids))
                     (expect (not (team/child-traffic? (council "peer") ids)) "Peer messages stay")
                     (expect (not (team/child-traffic? {:role :user :text "hi"} ids)))
                     (expect (not (team/child-traffic? (council "c2") #{}))))))
             ;; The transcript turn holds its team, so the traffic of its children hides before
             ;; the first team read.
             (it "hides the traffic of the children that the transcript turns hold"
                 (with-redefs [team/summary (constantly nil)]
                   (expect (= #{"c9"}
                              (team/child-session-ids
                                "leader"
                                [{:role :assistant :session-agents [{"session_id" "c9"}]}]))))))

(defdescribe
  row-markdown-test
  (it "shows a Markdown note in its row without the markers"
      (let [row (first (team/team-rows "turn-1"
                                       [(agent-row 1 "running" "note" "**Fixing** the `parser`")]))]
        (expect (str/includes? (first (team/row-lines row (team/row-columns [row]) 120 now-ms))
                               "Fixing the parser")))))

(defdescribe
  child-breadcrumb-test
  (it "names the path from the leader to the child session"
      (with-redefs [team/summary (constantly {:agents waiting-team})]
        (expect (= {:parent-id "leader" :text "LEADER → RESEARCH | #1"} (team/breadcrumb "c1")))
        (with-redefs [team/summary (constantly {:agents
                                                (assoc-in waiting-team [1 "title"] "Fix parser")})]
          (expect (= "LEADER → IMPLEMENT | Fix parser" (:text (team/breadcrumb "c2")))))
        (expect (nil? (team/breadcrumb "unknown"))))))
