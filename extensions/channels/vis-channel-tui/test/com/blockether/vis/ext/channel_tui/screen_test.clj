(ns com.blockether.vis.ext.channel-tui.screen-test
  "Tests for the TUI channel entry point. The bulk of the namespace
   is Lanterna-bound and exercised by the integration smoke + render
   benchmark; this suite focuses on the pure helpers - currently the
   `--session-id` / `--resume` argument parser, where a silent
   accept of unknown flags previously masked typos like
   `--sessions-id`."
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.scroll :as scroll]
            [com.blockether.vis.ext.channel-tui.screen :as screen]
            [com.blockether.vis.ext.channel-tui.selection :as selection]
            [com.blockether.vis.internal.external-opener :as opener]
            [com.blockether.vis.ext.channel-tui.state :as state]
            [lazytest.core :refer [defdescribe it expect]])
  (:import [com.googlecode.lanterna TerminalPosition]
           [com.googlecode.lanterna.input MouseAction MouseActionType]
           [com.googlecode.lanterna.terminal.ansi UnixLikeTerminal$CtrlCBehaviour]))

(def ^:private parse-args (deref #'screen/parse-args))

(def ^:private live-progress-only-change? (deref #'screen/live-progress-only-change?))

(def ^:private partial-live-frame? (deref #'screen/partial-live-frame?))

(def ^:private input-only-change? (deref #'screen/input-only-change?))

(def ^:private mouse-wheel-delta (deref #'screen/mouse-wheel-delta))

(def ^:private coalesce-wheel-input (deref #'screen/coalesce-wheel-input))

(def ^:private coalesce-drag-input (deref #'screen/coalesce-drag-input))

(def ^:private coalesced-drag-scroll-amount (deref #'screen/coalesced-drag-scroll-amount))

(def ^:private header-hover-only-change? (deref #'screen/header-hover-only-change?))

(def ^:private handle-channel-event! (deref #'screen/handle-channel-event!))

(def ^:private submit-input! (deref #'screen/submit-input!))

(def ^:private registry-slash-commands (deref #'screen/registry-slash-commands))

(def ^:private slash-spec->menu-command (deref #'screen/slash-spec->menu-command))

(def ^:private menu-commands (deref #'screen/menu-commands))

(def ^:private command-palette-extra-commands (deref #'screen/command-palette-extra-commands))

(def ^:private copy-session-id! (deref #'screen/copy-session-id!))

(def ^:private copy-selection! (deref #'screen/copy-selection!))

(def ^:private copy-bubble! (deref #'screen/copy-bubble!))

(def ^:private activate-tab-entry-hit! (deref #'screen/activate-tab-entry-hit!))

(def ^:private open-click-target! (deref #'screen/open-click-target!))

(def ^:private choose-frame-path (deref #'screen/choose-frame-path))

(def ^:private frame-change-flags (deref #'screen/frame-change-flags))

(def ^:private park-wait-ms (deref #'screen/park-wait-ms))

(def ^:private spinner-tick-ms (deref #'screen/spinner-tick-ms))

(def ^:private bubble-selectable-ranges (deref #'screen/bubble-selectable-ranges))

(def ^:private selected-transcript-text (deref #'screen/selected-transcript-text))

(def ^:private release-selection-focus (deref #'screen/release-selection-focus))

(def ^:private input-selectable-ranges (deref #'screen/input-selectable-ranges))

(def ^:private session-summary (deref #'screen/session-summary))

(def ^:private latest-modified-first (deref #'screen/latest-modified-first))

(def ^:private session-sort-key (deref #'screen/session-sort-key))

(def ^:private pre-resolve-session-id! (deref #'screen/pre-resolve-session-id!))

(def ^:private terminal-ctrl-c-behaviour (deref #'screen/terminal-ctrl-c-behaviour))

(def ^:private terminal-interrupt-action (deref #'screen/terminal-interrupt-action))

(def ^:private handle-terminal-interrupt! (deref #'screen/handle-terminal-interrupt!))

(def ^:private print-session-id-on-exit! (deref #'screen/print-session-id-on-exit!))

(defn- user-error?
  "True when `f` throws an ex-info carrying the `:vis/user-error` flag -
   the contract the channel entry point relies on to print a clean
   `vis: <msg>` line and exit 2 instead of a Java stack trace."
  [f]
  (try (f) false (catch clojure.lang.ExceptionInfo e (true? (:vis/user-error (ex-data e))))))

(defdescribe
  render-heartbeat-test
  (it "keeps live render heartbeat at 80ms" (expect (= 80 (deref #'screen/spinner-tick-ms))))
  (it "classifies progress-only loading ticks for partial repaint"
      (let
        [base {:loading? true
               :messages [{:role :assistant :text "live"}]
               :input {:lines [""]}
               :progress {:iterations []}
               :render-version 1
               :layout {:total-h 1}}]
        (expect (live-progress-only-change? base
                                            (assoc base
                                              :progress {:iterations [:new]}
                                              :render-version 2
                                              :layout {:total-h 2})))
        (expect (not (live-progress-only-change? base
                                                 (assoc base
                                                   :input {:lines ["typed"]}
                                                   :progress {:iterations [:new]}))))))
  (it "does not use partial live repaint for scroll changes during streaming"
      (let
        [base
         {:loading? true
          :scroll scroll/follow
          :messages [{:role :user :text "old"} {:role :assistant :text "live"}]
          :input {:lines [""]}
          :progress {:iterations []}
          :render-version 1
          :layout {:total-h 100}}

         scrolled
         (assoc base
           :scroll (scroll/parked 20)
           :render-version 2)]

        (expect (false? (live-progress-only-change? base scrolled)))
        (expect (false? (boolean (partial-live-frame? base scrolled true {:total-h 100}))))))
  (it "does not use partial live repaint while cancellation is in flight"
      (let
        [base
         {:loading? true
          :cancelling? true
          :messages [{:role :assistant :text "live"}]
          :input {:lines [""]}
          :progress {:iterations []}
          :render-version 1
          :layout {:total-h 10}}

         cancelling
         (assoc base
           :progress {:iterations [:new]}
           :render-version 2
           :layout {:total-h 12})]

        (expect (live-progress-only-change? base cancelling))
        (expect (false? (boolean (partial-live-frame? base cancelling true {:total-h 10}))))))
  (it "classifies header hover bumps as header-only repaints"
      (let
        [base
         {:loading? false
          :messages [{:role :assistant :text "stable body"}]
          :input {:lines [""]}
          :render-version 1
          :layout {:total-h 10}}

         bumped
         (assoc base
           :render-version 2
           :layout {:total-h 10})

         header-region
         {:kind :copy-id :bounds {:row 1 :col 60 :width 12}}

         body-region
         {:kind :url :bounds {:row 8 :col 4 :width 12}}]

        (expect (header-hover-only-change? base bumped nil header-region))
        (expect (header-hover-only-change? base bumped header-region nil))
        (expect (not (header-hover-only-change? base bumped nil body-region)))
        (expect (not (header-hover-only-change? base
                                                (assoc bumped :input {:lines ["typed"]})
                                                nil
                                                header-region))))))

(defdescribe wheel-coalescing-test
             (it "classifies wheel actions to signed deltas"
                 (let
                   [up
                    (MouseAction. MouseActionType/SCROLL_UP 1 (TerminalPosition. 10 4))

                    down
                    (MouseAction. MouseActionType/SCROLL_DOWN 1 (TerminalPosition. 10 4))

                    click
                    (MouseAction. MouseActionType/CLICK_DOWN 1 (TerminalPosition. 10 4))]

                   (expect (= -1 (mouse-wheel-delta up)))
                   (expect (= 1 (mouse-wheel-delta down)))
                   (expect (nil? (mouse-wheel-delta click)))))
             (it "coalesces wheel floods and preserves first non-wheel key"
                 (let
                   [first-wheel
                    (MouseAction. MouseActionType/SCROLL_UP 1 (TerminalPosition. 3 7))

                    second-wheel
                    (MouseAction. MouseActionType/SCROLL_UP 1 (TerminalPosition. 3 7))

                    non-wheel
                    (MouseAction. MouseActionType/CLICK_DOWN 1 (TerminalPosition. 3 7))

                    queue
                    (atom [second-wheel non-wheel])

                    poll-next
                    (fn []
                      (let [v @queue]
                        (when-let [k (first v)]
                          (swap! queue subvec 1)
                          k)))

                    {:keys [wheel-delta next-key]}
                    (coalesce-wheel-input first-wheel poll-next)]

                   (expect (= -2 wheel-delta))
                   (expect (= non-wheel next-key))
                   (expect (empty? @queue))))
             (it "drops net-zero wheel jitter (up then down)"
                 (let
                   [first-wheel
                    (MouseAction. MouseActionType/SCROLL_UP 1 (TerminalPosition. 1 1))

                    second-wheel
                    (MouseAction. MouseActionType/SCROLL_DOWN 1 (TerminalPosition. 1 1))

                    queue
                    (atom [second-wheel])

                    poll-next
                    (fn []
                      (let [v @queue]
                        (when-let [k (first v)]
                          (swap! queue subvec 1)
                          k)))

                    {:keys [wheel-delta]}
                    (coalesce-wheel-input first-wheel poll-next)]

                   (expect (nil? wheel-delta)))))

(defdescribe drag-coalescing-test
             (it "coalesces drag bursts and keeps last drag event + first non-drag"
                 (let
                   [d1
                    (MouseAction. MouseActionType/DRAG 1 (TerminalPosition. 5 5))

                    d2
                    (MouseAction. MouseActionType/DRAG 1 (TerminalPosition. 5 6))

                    d3
                    (MouseAction. MouseActionType/DRAG 1 (TerminalPosition. 5 7))

                    click
                    (MouseAction. MouseActionType/CLICK_DOWN 1 (TerminalPosition. 5 7))

                    queue
                    (atom [d2 d3 click])

                    poll-next
                    (fn []
                      (let [v @queue]
                        (when-let [k (first v)]
                          (swap! queue subvec 1)
                          k)))

                    {:keys [key drag-events next-key]}
                    (coalesce-drag-input d1 poll-next)]

                   (expect (= 3 drag-events))
                   (expect (= d3 key))
                   (expect (= click next-key))
                   (expect (empty? @queue))))
             (it "scales drag auto-scroll amount with a bounded coalesce factor"
                 (expect (= 4 (coalesced-drag-scroll-amount 4 1)))
                 (expect (= 12 (coalesced-drag-scroll-amount 4 3)))
                 ;; bounded by drag-autoscroll-max-coalesce-factor (= 8)
                 (expect (= 32 (coalesced-drag-scroll-amount 4 99)))))

(defdescribe
  slash-menu-test
  (it "slash-spec->menu-command adapts a top-level slash spec"
      ;; `:id` carries the full dot-separated path so the
      ;; `run-command!` dispatcher can detect a slash entry by its
      ;; `:slash/spec` key and resubmit through the engine slash registry.
      (let [adapted (slash-spec->menu-command {:slash/name "workspace" :slash/doc "Workspace ops"})]
        (expect (= :workspace (:id adapted)))
        (expect (= "/workspace" (:slash/text adapted)))
        (expect (= "Workspace ops" (:label adapted)))))
  (it "slash-spec->menu-command adapts a nested slash spec"
      (let
        [adapted (slash-spec->menu-command
                   {:slash/name "new" :slash/parent ["workspace"] :slash/doc "Create workspace"})]
        (expect (= :workspace.new (:id adapted)))
        (expect (= "/workspace new" (:slash/text adapted)))
        (expect (= "Create workspace" (:label adapted)))
        (expect (= "workspace new" (:slash/name adapted)))))
  (it "registry-slash-commands lists children but hides their group root"
      (with-redefs
        [vis/registered-slashes
         (constantly [{:slash/name "workspace" :slash/doc "Workspace ops"}
                      {:slash/name "apply" :slash/parent ["workspace"] :slash/doc "Apply"}
                      {:slash/name "voice"
                       :slash/doc "Voice toggle"
                       :slash/availability-fn (fn [{ch :channel/id}]
                                                (= :tui ch))}
                      {:slash/name "help"
                       :slash/doc "CLI help"
                       :slash/availability-fn (fn [{ch :channel/id}]
                                                (= :cli ch))}
                      {:slash/name "start" :slash/doc "Hidden alias" :slash/hidden? true}
                      {:slash/name "broken"
                       :slash/doc "Broken availability"
                       :slash/availability-fn (fn [_ctx]
                                                (throw (ex-info "boom" {})))}])]
        ;; `workspace` is a group root (parent of `apply`); its run-fn only
        ;; reprints the child list the palette already shows, so it is
        ;; suppressed. The child `workspace.apply` and the leaf `voice` stay.
        (let [ids (mapv :id (registry-slash-commands))]
          (expect (= #{:workspace.apply :voice} (set ids))))))
  (it "menu-commands keeps slash registry for typed slash suggestions"
      (with-redefs
        [vis/registered-slashes (constantly [{:slash/name "voice" :slash/doc "Voice toggle"}])]
        (let [ids (mapv :id (menu-commands nil))]
          (expect (some #{:new-session} ids))
          (expect (some #{:voice} ids)))))
  (it "Ctrl+K palette gets no registry slash roots by default"
      (with-redefs
        [vis/registered-slashes (constantly [{:slash/name "voice" :slash/doc "Voice toggle"}
                                             {:slash/name "workspace" :slash/doc "Workspace ops"}])]
        (expect (= [] (command-palette-extra-commands))))))

(defdescribe channel-status-error-routing-test
             (it "routes error status events to the notification lane only"
                 (let
                   [events
                    (atom [])

                    notified
                    (atom nil)]

                   (with-redefs
                     [state/dispatch
                      (fn [event]
                        (swap! events conj event))

                      vis/notify!
                      (fn [text & kvs]
                        (reset! notified [text kvs]))]

                     (handle-channel-event! {:op :status/set
                                             :id :voice/piper
                                             :text "Voice response failed: synthesize-file"
                                             :level :error})
                     (expect (= [[:channel-status-clear :voice/piper]] @events))
                     (expect (= ["Voice response failed: synthesize-file"
                                 [:level :error :ttl-ms 5000]]
                                @notified)))))
             (it "clears ready status events instead of storing them forever"
                 (let [events (atom [])]
                   (with-redefs
                     [state/dispatch (fn [event]
                                       (swap! events conj event))]
                     (handle-channel-event! {:op :status/set
                                             :id :voice/piper
                                             :text "Voice response complete 100%"
                                             :phase :ready
                                             :level :info})
                     (expect (= [[:channel-status-clear :voice/piper]] @events))))))

(defdescribe workspace-entry-click-test
             (it "switches to the clicked workspace and refreshes active session state"
                 (reset! state/app-db
                   {:tabs [{:id :main :label "Main" :active? true} {:id :tab-1 :label "Tab 1"}]
                    :active-tab-id :main
                    :session {:id "main-c"}
                    :messages [{:role :user :text "main prompt"}]
                    :input (input/paste-text (input/empty-input) "main draft")
                    :input-history ["main prompt"]
                    :tab-locals {:tab-1 {:session {:id "tab-c"}
                                         :messages [{:role :user :text "tab prompt"}]
                                         :input (input/paste-text (input/empty-input) "tab draft")
                                         :input-history ["tab prompt"]}}})
                 (let [refreshes (atom [])]
                   (activate-tab-entry-hit! #(swap! refreshes conj %)
                                            {:kind :workspace-entry :index 1})
                   (expect (= :tab-1 (:active-tab-id @state/app-db)))
                   (expect (= {:id "tab-c"} (:session @state/app-db)))
                   (expect (= [{:role :user :text "tab prompt"}] (:messages @state/app-db)))
                   (expect (= "tab draft" (input/input->text (:input @state/app-db))))
                   (expect (= [false] @refreshes))
                   (activate-tab-entry-hit! #(swap! refreshes conj %)
                                            {:kind :workspace-entry :index 1})
                   (expect (= [false] @refreshes)))))

(defdescribe terminal-interrupt-test
             (it "configures Lanterna to trap Ctrl+C instead of exiting inside pollInput"
                 (expect (= UnixLikeTerminal$CtrlCBehaviour/TRAP (terminal-ctrl-c-behaviour))))
             (it "clears a non-empty draft before quitting on terminal interrupts"
                 (expect (= :clear-input
                            (terminal-interrupt-action {:input (input/paste-text (input/empty-input)
                                                                                 "draft")})))
                 (expect (= :quit (terminal-interrupt-action {:input (input/empty-input)}))))
             (it "dispatches reset-input for the first interrupt and shutdown for the next"
                 (let
                   [old-db
                    @state/app-db

                    events
                    (atom [])]

                   (try (with-redefs
                          [state/dispatch (fn [event]
                                            (swap! events conj event))]
                          (reset! state/app-db {:input (input/paste-text (input/empty-input)
                                                                         "draft")})
                          (handle-terminal-interrupt!)
                          (reset! state/app-db {:input (input/empty-input)})
                          (handle-terminal-interrupt!))
                        (expect (= [[:reset-input] [:shutdown]] @events))
                        (finally (reset! state/app-db old-db))))))

(defdescribe
  channel-main-shutdown-agents-test
  (it
    "calls (shutdown-agents) on the success path so the JVM exits without
       the ~60s agent thread-pool keep-alive that looks like 'Ctrl+C froze
       vis'. Regression: TUI used to only call (vis/shutdown!) (Telemere
       handlers); the agent pool's non-daemon threads kept the JVM alive
       long after the screen was torn down. The CLI channel path
       has always called shutdown-agents - this test pins the same
       guarantee for the TUI channel."
    (let [calls (atom [])]
      (with-redefs
        [screen/redirect-stdio-to-log! (fn []
                                         (swap! calls conj :redirect))
         vis/init! (fn []
                     (swap! calls conj :init))
         screen/run-chat! (fn [_opts]
                            (swap! calls conj :run))
         screen/print-session-id-on-exit! (fn []
                                            (swap! calls conj :print-id))
         vis/shutdown! (fn []
                         (swap! calls conj :vis-shutdown))
         clojure.core/shutdown-agents (fn []
                                        (swap! calls conj :shutdown-agents))]

        (screen/channel-main []))
      ;; Order matters: print the resume id after the TUI exits, then stop
      ;; Telemere handlers, THEN drain the agent pool - the former may flush a
      ;; final log write that rides the agent pool, and shutdown-agents will
      ;; refuse new work.
      (expect (= [:redirect :init :run :print-id :vis-shutdown :shutdown-agents] @calls)))))

(defdescribe startup-resume-test
             (it "--session-id reconciles orphaned running turns before rebuilding history"
                 ;; The sweep now goes through the gateway (`gateway-reconcile-running-turns!`)
                 ;; rather than poking the DB directly — but it must STILL run before the
                 ;; resume so the rebuilt history carries no stale :running turns.
                 (let
                   [calls
                    (atom [])

                    resumed
                    {:id "c1" :history [{:role :assistant :text "interrupted"}]}]

                   (with-redefs
                     [vis/gateway-reconcile-running-turns!
                      (fn []
                        (swap! calls conj :reconcile)
                        1)

                      chat/resume-session
                      (fn [cid]
                        (swap! calls conj [:resume cid])
                        (expect (= "c1" cid))
                        resumed)]

                     (expect (= resumed (pre-resolve-session-id! {:session-id "c1"})))
                     (expect (= [:reconcile [:resume "c1"]] @calls))))))

(defdescribe
  session-switcher-data-test
  (it "uses latest turn creation time as modification time and sorts newest first"
      (with-redefs
        [vis/gateway-list-turns (fn [session-id]
                                  (case session-id
                                    "old"
                                    [{"created_at" #inst "2024-01-04T00:00:00.000-00:00"}]

                                    "new"
                                    [{"created_at" #inst "2024-01-02T00:00:00.000-00:00"}
                                     {"created_at" #inst "2024-01-08T00:00:00.000-00:00"}]

                                    []))]
        (let
          [old-summary (session-summary {"id" "old"
                                         "created_at" #inst "2024-01-01T00:00:00.000-00:00"})
           new-summary (session-summary {"id" "new"
                                         "created_at" #inst "2024-01-03T00:00:00.000-00:00"})]

          (expect (= 1 (get old-summary "turn_count")))
          (expect (= 2 (get new-summary "turn_count")))
          (expect (= #inst "2024-01-08T00:00:00.000-00:00" (get new-summary "modified_at")))
          (expect (= ["new" "old"]
                     (mapv #(get % "id") (latest-modified-first [old-summary new-summary]))))))))

(defdescribe submit-input-test
             (it "dispatches send before reset so paste placeholders can expand"
                 (let
                   [events
                    (atom [])

                    payload
                    "therapy line 1\ntherapy line 2"

                    token
                    (input/format-paste-placeholder {:id 1 :content payload})

                    input-state
                    (input/paste-text (input/empty-input) (str "context " token))]

                   (with-redefs
                     [state/dispatch (fn [event]
                                       (swap! events conj event))]
                     (submit-input! {:session {:id "c1"} :loading? false} input-state)
                     (expect (= [[:send-message (str "context " token)] [:reset-input]]
                                @events))))))

(defdescribe
  selectable-ranges-test
  (it
    "clips transcript selection to message content rows only"
    (expect
      (= [{:row 4 :col 2 :width 11} {:row 5 :col 2 :width 11}]
         (bubble-selectable-ranges
           {:visible
            [{:top -1 :height 4 :projected {:role :assistant :prewrapped-lines ["first" "second"]}}
             {:top 4 :height 3 :projected {:role :assistant :prewrapped-lines ["below viewport"]}}]}
           4
           5
           20))))
  (it "keeps assistant code selection one column inside and answers aligned with Vis"
      (expect (= [{:row 5 :col 3 :width 11} {:row 6 :col 2 :width 11}]
                 (bubble-selectable-ranges
                   {:visible [{:top 0
                               :height 3
                               :projected {:role :assistant
                                           :prewrapped-lines [(str p/MARKER_CODE "(+ 1 2)")
                                                              (str p/MARKER_ANSWER_TXT "done")]}}]}
                   4
                   6
                   20))))
  (it "does not mark role banners, padding, provider footers, or gap rows as selectable"
      (expect (= [{:row 6 :col 4 :width 11}]
                 (bubble-selectable-ranges
                   {:visible [{:top 0 :height 5 :projected {:role :user :text "siema"}}]}
                   4
                   6
                   20))))
  (it "sorts sessions by real turns, latest modified time, then turn count by default"
      (let
        [old-with-turns
         {"id" :old
          "turn_count" 1
          "modified_at" #inst "2024-01-02T00:00:00.000-00:00"
          "created_at" #inst "2024-01-01T00:00:00.000-00:00"}

         latest-empty
         {"id" :empty
          "turn_count" 0
          "modified_at" #inst "2024-01-10T00:00:00.000-00:00"
          "created_at" #inst "2024-01-10T00:00:00.000-00:00"}

         latest-with-turns
         {"id" :latest
          "turn_count" 2
          "modified_at" #inst "2024-01-03T00:00:00.000-00:00"
          "created_at" #inst "2024-01-01T00:00:00.000-00:00"}

         same-latest-more-turns
         {"id" :more-turns
          "turn_count" 5
          "modified_at" #inst "2024-01-03T00:00:00.000-00:00"
          "created_at" #inst "2024-01-01T00:00:00.000-00:00"}]

        (expect (= [1 1704240000000 2] (session-sort-key latest-with-turns)))
        (expect (= [:more-turns :latest :old :empty]
                   (mapv #(get % "id")
                         (latest-modified-first [old-with-turns latest-empty latest-with-turns
                                                 same-latest-more-turns]))))))
  (it "copies transcript content without role labels, answer separators, or model metadata"
      (let
        [ranges
         (bubble-selectable-ranges {:visible [{:top 0
                                               :height 6
                                               :projected
                                               {:role :assistant
                                                :prewrapped-lines
                                                ["(done (v/p \"hi\"))" (str p/MARKER_ANSWER_SEP "")
                                                 (str p/MARKER_ANSWER_TXT "hi there")]}}]}
                                   0
                                   6
                                   40)

         rows
         ["  Vis                                   " "  (done (v/p \"hi\"))          "
          "────────────────────────────────────────" "  hi there                              "
          "                    zai/glm / 1 iter    " "                                        "]]

        (expect (= "(done (v/p \"hi\"))\nhi there"
                   (selection/selected-text rows
                                            {:anchor (selection/point 0 0)
                                             :focus (selection/point 39 5)}
                                            ranges)))))
  (it "copies transcript selection from document rows after auto-scroll"
      (let
        [message
         {:role :assistant :prewrapped-lines ["line zero" "line one" "line two" "line three"]}

         layout
         {:total-h 6 :heights [6] :offsets [0 6]}

         sel
         {:anchor (selection/point 0 1) :focus (selection/point 39 4)}]

        (expect (= "line zero\nline one\nline two\nline three"
                   (selected-transcript-text [message] layout 40 {} {} sel)))))
  (it "copies visible live text for pending assistant drag selection"
      (let
        [message
         {:role :assistant :pending? true :text "Sending request to provider..."}

         layout
         {:total-h 4
          :heights [4]
          :offsets [0 4]
          :visible [{:idx 0
                     :top 0
                     :height 4
                     :projected {:role :assistant
                                 :text "live visible text"
                                 :prewrapped-lines ["live visible text"]}}]}

         sel
         {:anchor (selection/point 0 1) :focus (selection/point 39 1)}]

        (expect (= "live visible text" (selected-transcript-text [message] layout 40 {} {} sel)))))
  (it "uses release viewport for drag-copy focus after scrolling beyond the first screen"
      (expect (= (selection/point 7 42)
                 (release-selection-focus (selection/point 3 10)
                                          (selection/point 7 18)
                                          false
                                          (selection/point 7 6)
                                          {:viewport-top 2 :eff-scroll 38}))))
  (it "keeps pre-expanded double-click line focus on release"
      (expect (= (selection/point 20 10)
                 (release-selection-focus (selection/point 3 10)
                                          (selection/point 20 10)
                                          true
                                          (selection/point 7 6)
                                          {:viewport-top 2 :eff-scroll 38}))))
  (it "marks input text rows as selectable without input padding"
      (expect (= [{:row 11 :col 2 :width 16} {:row 12 :col 2 :width 16}]
                 (input-selectable-ranges 10 2 20)))))

(defdescribe
  clipboard-copy-actions-test
  (it "session-id copy uses the same icon-era notification TTL"
      (let
        [copied
         (promise)

         notified
         (atom nil)]

        (with-redefs-fn {#'input/clipboard-copy! (fn [text]
                                                   (deliver copied text)
                                                   true)
                         #'vis/notify! (fn [text & kvs]
                                         (reset! notified [text kvs]))}
          (fn []
            (copy-session-id! "123e4567-e89b-12d3-a456-426614174000")
            (expect (= "123e4567-e89b-12d3-a456-426614174000" (deref copied 1000 ::timeout)))
            (expect (= ["✓ Copied session ID" [:level :success :ttl-ms 1500]] @notified))))))
  (it "mouse selection copy uses the shared success notification contract"
      (let
        [copied
         (promise)

         notified
         (atom nil)]

        (with-redefs-fn {#'input/clipboard-copy! (fn [text]
                                                   (deliver copied text)
                                                   true)
                         #'vis/notify! (fn [text & kvs]
                                         (reset! notified [text kvs]))}
          (fn []
            (copy-selection! "selected text")
            (expect (= "selected text" (deref copied 1000 ::timeout)))
            (expect (= ["✓ Copied selection" [:level :success :ttl-ms 1500]] @notified))))))
  (it "single-click bubble copy uses the shared success notification contract"
      (let
        [copied
         (promise)

         notified
         (atom nil)]

        (with-redefs-fn {#'input/clipboard-copy! (fn [text]
                                                   (deliver copied text)
                                                   true)
                         #'vis/notify! (fn [text & kvs]
                                         (reset! notified [text kvs]))}
          (fn []
            (copy-bubble! "whole bubble")
            (expect (= "whole bubble" (deref copied 1000 ::timeout)))
            (expect (= ["✓ Copied bubble" [:level :success :ttl-ms 1500]] @notified))))))
  (it "single-click bubble copy strips ANSI/control-picture artifacts"
      (let [copied (promise)]
        (with-redefs-fn {#'input/clipboard-copy! (fn [text]
                                                   (deliver copied text)
                                                   true)
                         #'vis/notify! (fn [& _]
                                         nil)}
          (fn []
            (copy-bubble! (str "\u001B[32m(def\u001B[0m x 1)\n" "\u241B[31mok\u241B[0m"))
            (expect (= "(def x 1)\nok" (deref copied 1000 ::timeout)))))))
  (it "input mouse selection copy names the input in the notification"
      (let
        [copied
         (promise)

         notified
         (atom nil)]

        (with-redefs-fn {#'input/clipboard-copy! (fn [text]
                                                   (deliver copied text)
                                                   true)
                         #'vis/notify! (fn [text & kvs]
                                         (reset! notified [text kvs]))}
          (fn []
            (copy-selection! "typed mistake" :input)
            (expect (= "typed mistake" (deref copied 1000 ::timeout)))
            (expect (= ["✓ Copied input selection" [:level :success :ttl-ms 1500]] @notified))))))
  (it "file click targets open through the editor path, not the generic URL opener"
      (let
        [editor-opened
         (promise)

         url-opened
         (promise)]

        (with-redefs-fn {#'opener/open-file-in-editor! (fn [target]
                                                         (deliver editor-opened target)
                                                         {:status :ok})
                         #'opener/open! (fn [target]
                                          (deliver url-opened target)
                                          {:status :ok})}
          (fn []
            (open-click-target! {:kind :file :url "deps.edn#L42"})
            (expect (= "deps.edn#L42" (deref editor-opened 1000 ::timeout)))
            (expect (= ::timeout (deref url-opened 100 ::timeout)))))))
  (it "URL click targets keep using the generic opener"
      (let [url-opened (promise)]
        (with-redefs-fn {#'opener/open! (fn [target]
                                          (deliver url-opened target)
                                          {:status :ok})}
          (fn []
            (open-click-target! {:kind :url :url "https://example.com"})
            (expect (= "https://example.com" (deref url-opened 1000 ::timeout))))))))

(defdescribe session-id-exit-print-test
             (it "prints the active session id after the TUI exits"
                 (let
                   [bytes
                    (java.io.ByteArrayOutputStream.)

                    ps
                    (java.io.PrintStream. bytes true "UTF-8")]

                   (with-redefs-fn {#'screen/current-session-id (fn []
                                                                  "abc123")
                                    #'vis/original-stdout ps}
                     (fn []
                       (print-session-id-on-exit!)
                       (.flush ps)
                       (let [out (.toString bytes "UTF-8")]
                         (expect (= "\rResume with:\nvis channels tui --session-id abc123\n"
                                    out))))))))

(defdescribe parse-args-test
             (it "no args -> empty opts map" (expect (= {} (parse-args []))))
             (it "--resume sets :resume true" (expect (= {:resume true} (parse-args ["--resume"]))))
             (it "--session-id captures the next token as the id"
                 (expect (= {:session-id "abc123"} (parse-args ["--session-id" "abc123"]))))
             (it "--session-id + --resume coexist (caller decides precedence)"
                 (expect (= {:session-id "abc123" :resume true}
                            (parse-args ["--session-id" "abc123" "--resume"]))))
             (it "unknown flag throws :vis/user-error (regression: typo silently swallowed)"
                 ;; `vis channels tui --sessions-id <uuid>` used to succeed
                 ;; silently and start a fresh session. The user reported it
                 ;; explicitly: the flag with a stray "s" must blow up.
                 (expect (user-error? #(parse-args ["--sessions-id"
                                                    "d8aff512-d60d-42b6-a009-041f1bec3891"]))))
             (it "unknown flag error message names the bad flag and shows usage"
                 (try (parse-args ["--sessions-id" "x"])
                      (expect false "expected ex-info")
                      (catch clojure.lang.ExceptionInfo e
                        (let [msg (.getMessage e)]
                          (expect (re-find #"--sessions-id" msg))
                          (expect (re-find #"Usage:" msg))))))
             (it "--session-id without a value -> :vis/user-error"
                 (expect (user-error? #(parse-args ["--session-id"]))))
             (it "--session-id followed by another flag -> :vis/user-error (no value)"
                 ;; Catches the case where the user types `--session-id --resume`
                 ;; and `--resume` would otherwise be silently treated as the id.
                 (expect (user-error? #(parse-args ["--session-id" "--resume"]))))
             (it "non-flag positional arg also errors (no positional API today)"
                 (expect (user-error? #(parse-args ["stray-positional"])))))

(defdescribe
  input-only-fast-path-test
  (it "classifies a same-height input edit as an input-only frame"
      (let
        [cols
         80

         base
         {:input {:lines ["hello"]} :scroll nil :messages [] :loading? false}

         typed
         (assoc base :input {:lines ["hello world"]})]

        (expect (true? (boolean (input-only-change? base typed cols))))))
  (it "falls through around inline suggestion triggers so stale picker rows clear"
      (let
        [cols
         80

         base
         {:input {:lines ["open @src"]} :scroll nil :messages [] :loading? false}

         file-complete
         (assoc base :input {:lines ["open @src "]})

         slash-base
         {:input {:lines ["/new-tab"]} :scroll nil :messages [] :loading? false}

         slash-complete
         (assoc slash-base :input {:lines ["/new-tab "]})]

        (expect (false? (boolean (input-only-change? base file-complete cols))))
        (expect (false? (boolean (input-only-change? slash-base slash-complete cols))))))
  (it "falls through to the full painter when the input box height changes"
      ;; A keystroke that wraps the input to a new visual row resizes the
      ;; transcript band (input-box-h feeds inner-h), so the fast path MUST NOT
      ;; fire — the transcript needs a real re-layout.
      (let
        [cols
         80

         base
         {:input {:lines ["hi"]} :scroll nil :messages [] :loading? false}

         wrapped
         (assoc base :input {:lines [(apply str (repeat 400 "x"))]})]

        (expect (false? (boolean (input-only-change? base wrapped cols))))))
  (it "falls through when any non-input key differs"
      (let
        [cols
         80

         base
         {:input {:lines ["a"]} :scroll nil :messages [] :loading? false}

         edited
         (assoc base
           :input {:lines ["ab"]}
           :messages [{:role :user}])]

        (expect (false? (boolean (input-only-change? base edited cols))))))
  (it "falls through while loading (the live bubble grows)"
      (let
        [cols
         80

         base
         {:input {:lines ["a"]} :scroll nil :messages [] :loading? false}

         edited
         (assoc base
           :input {:lines ["ab"]}
           :loading? true)]

        (expect (false? (boolean (input-only-change? base edited cols))))))
  (it "falls through while a mouse selection / overlay / find bar is active"
      (let
        [cols
         80

         base
         {:input {:lines ["a"]} :scroll nil :messages [] :loading? false}

         edited
         {:input {:lines ["ab"]} :scroll nil :messages [] :loading? false}]

        (expect (false? (boolean
                          (input-only-change? base (assoc edited :mouse-selection {:x 1}) cols))))
        (expect (false? (boolean (input-only-change? base (assoc edited :tasks-open? true) cols))))
        (expect (false? (boolean (input-only-change? base (assoc edited :help-open? true) cols))))
        (expect (false? (boolean (input-only-change? base
                                                     (assoc-in edited [:search :active?] true)
                                                     cols))))))
  (it "needs a real input change and a previous frame"
      (let
        [cols
         80

         base
         {:input {:lines ["a"]} :scroll nil :messages [] :loading? false}]

        (expect (false? (boolean (input-only-change? base base cols))))
        (expect (false? (boolean (input-only-change? nil base cols)))))))

(defdescribe
  render-loop-decomposition-test
  "Pins the pure helpers extracted from `render-loop!` — the frame-path
   decision that caused the streaming CPU spin, now testable in isolation."
  (it "choose-frame-path picks the cheapest path; earlier (cheaper) paths win ties"
      (expect (= :full (choose-frame-path {})))
      (expect (= :header-hover (choose-frame-path {:header-hover-only? true :partial-live? true})))
      (expect (= :partial-live (choose-frame-path {:partial-live? true :scroll-frame? true})))
      (expect (= :header-spinner
                 (choose-frame-path {:header-spinner-only? true :scroll-frame? true})))
      (expect (= :scroll (choose-frame-path {:scroll-frame? true :input-only? true})))
      (expect (= :input (choose-frame-path {:input-only? true}))))
  (it "park-wait-ms drops to the spinner cadence while loading, idle cap otherwise"
      (expect (= spinner-tick-ms (park-wait-ms {} true)))
      (expect (= 250 (park-wait-ms {} false))))
  (it "frame-change-flags takes NO cheap path while recovering from a dialog block"
      (let
        [flags (frame-change-flags {:last-db {}
                                    :db {}
                                    :last-layout {:total-h 10 :inner-h 5}
                                    :last-hover nil
                                    :current-hover nil
                                    :cols 80
                                    :same-size? true
                                    :animate? false
                                    :loading? false
                                    :scroll-anim? false
                                    :overlay-open? false
                                    :was-blocked? true})]
        (expect (every? false? (map boolean (vals flags)))))))
