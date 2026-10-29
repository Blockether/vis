(ns com.blockether.vis.tui.fork-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.projects :as projects]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.terminal-image :as timg]
            [com.blockether.vis.tui.virtual :as virtual]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(def review-message
  "Persisted answer used by the production terminal and HTML review."
  {:role :assistant
   :text "The change is ready. Continue from this answer in a new session."
   :timestamp (java.util.Date. 1789115400000)
   :session-turn-id "turn-2"
   :client-turn-id "local-turn-2"})

(defn header-frame
  "Capture the actual projected answer and its published pointer regions."
  [message width
   {:keys [start viewport-top viewport-h timestamps? agent-name hover-turn-id]
    :or {start 1 viewport-top 0 viewport-h 12 timestamps? true agent-name "Vis"}}]
  (binding [interactions/hit-map (interactions/create-hit-map)]
    (when hover-turn-id
      (.beginFrame interactions/hit-map)
      (.register interactions/hit-map
                 {:bounds {:row 0 :col 0 :width 1}
                  :kind :fork-at-turn
                  :session-id "session-1"
                  :turn-id hover-turn-id})
      (.commitFrame interactions/hit-map)
      (.updateHovered interactions/hit-map
                      (MouseAction. MouseActionType/MOVE 0 (TerminalPosition. 0 0))))
    (let [projected (virtual/project-message message
                                             width
                                             {:show-timestamps timestamps?}
                                             {:session-id "session-1"})
          capture (cap/capture! {:cols (+ width 4)
                                 :rows 12
                                 :paint! (fn [{:keys [g]}]
                                           (.beginFrame interactions/hit-map)
                                           (render/draw-chat-bubble! g
                                                                     projected
                                                                     start
                                                                     2
                                                                     width
                                                                     {:viewport-top viewport-top
                                                                      :viewport-h viewport-h
                                                                      :agent-name agent-name})
                                           (.commitFrame interactions/hit-map))})]

      {:capture capture :regions (vec (.current interactions/hit-map))})))

(defdescribe persisted-answer-has-a-fork-button-after-the-date
             (it "persisted answer has a fork button after the date"
                 (let [{:keys [capture regions]}
                       (header-frame review-message 76 {:viewport-top 4})

                       header
                       (second (str/split-lines (cap/frame-text capture)))

                       hit
                       (first regions)]

                   (expect (nil? (:error capture)))
                   (expect (= 1 (count regions)))
                   (expect (= {:kind :fork-at-turn :session-id "session-1" :turn-id "turn-2"}
                              (select-keys hit [:kind :session-id :turn-id])))
                   (expect (= 5 (get-in hit [:bounds :row])))
                   (expect (= (.indexOf ^String header " Fork from this turn")
                              (get-in hit [:bounds :col])))
                   (expect (= (count " Fork from this turn ") (get-in hit [:bounds :width]))))))

(defdescribe
  narrow-header-keeps-the-turn-number-and-date
  (it "narrow header keeps the turn number and date"
      (let [message
            (assoc review-message :turn-position 123)

            {:keys [capture regions]}
            (header-frame message 32 {})

            header
            (second (str/split-lines (cap/frame-text capture)))]

        (expect (nil? (:error capture)))
        (expect (str/includes? header (str (client/format-date (:timestamp message)) " / T123")))
        (expect (empty? regions)))))

(defdescribe history-keeps-the-persisted-turn-number-not-the-page-index
             (it "history keeps the persisted turn number not the page index"
                 (let [messages (@#'chat/turns->messages
                                 [{"turn_id" "turn-42"
                                   "position" 42
                                   "status" "completed"
                                   "request" "Check the change"
                                   "created_at" 1789115400000
                                   "content" [{"id" "answer" "type" "prose" "markdown" "Ready"}]}])]
                   (expect (= [42 42] (mapv :turn-position messages)))
                   (expect (= [(java.util.Date. 1789115400000) (java.util.Date. 1789115400000)]
                              (mapv :timestamp messages))))))

(defdescribe live-metadata-arrives-even-without-visible-progress
             (it "live metadata arrives even without visible progress"
                 (let [chunks (atom [])]
                   (with-redefs [client/gateway-attach-turn-sync!
                                 (fn [_ _ {:keys [on-event]}]
                                   (on-event {"type" "content.block.started"
                                              "turn_id" "turn-42"
                                              "position" 42
                                              "created_at" 1789115400000})
                                   {"content" []})]
                     (chat/attach! {:id "session-1"} "turn-42" {:on-chunk #(swap! chunks conj %)}))
                   (expect (= [{:phase :turn-metadata
                                :turn-id "turn-42"
                                :turn-position 42
                                :created-at-ms 1789115400000}]
                              @chunks)))))

(defdescribe
  live-turn-metadata-stays-with-its-message-pair-after-completion
  (it
    "live turn metadata stays with its message pair after completion"
    (let [before
          @state/app-db

          old
          (assoc review-message
            :session-turn-id "old"
            :client-turn-id "old")

          messages
          [old (assoc (chat/user-message "Check") :client-turn-id "local-42")
           (assoc (chat/assistant-message [])
             :client-turn-id "local-42"
             :pending? true)]]

      (try (reset! state/app-db {:session {:id "session-1"}
                                 :active-tab-id "session-1"
                                 :render-version 0
                                 :loading? true
                                 :gateway-turn-id "turn-42"
                                 :live-turn-client-id "local-42"
                                 :messages messages})
           (state/dispatch [:sync-turn-metadata nil
                            {:turn-id "foreign" :turn-position 99 :created-at-ms 1}])
           (expect (= messages (:messages @state/app-db)))
           (state/dispatch [:sync-turn-metadata nil
                            {:turn-id "turn-42" :turn-position 42 :created-at-ms 1789115400000}])
           (let [stamped
                 (:messages @state/app-db)

                 completed
                 (@#'state/replace-pending-assistant
                  stamped
                  (assoc (chat/assistant-message []) :client-turn-id "local-42"))]

             (expect (= old (first stamped)))
             (expect (= [42 42] (mapv :turn-position (rest stamped))))
             (expect (= ["turn-42" "turn-42"] (mapv :session-turn-id (rest stamped))))
             (expect (= [(java.util.Date. 1789115400000) (java.util.Date. 1789115400000)]
                        (mapv :timestamp (rest completed))))
             (expect (= [42 42] (mapv :turn-position (rest completed)))))
           (finally (reset! state/app-db before))))))

(defdescribe
  reopened-running-turn-keeps-its-number-and-canonical-date
  (it
    "reopened running turn keeps its number and canonical date"
    (let [before
          @state/app-db

          sid
          (str (random-uuid))

          created-at
          1789115400000]

      (try (doseq [source [:soul :turn]]
             (with-redefs [client/gateway-soul (constantly (cond-> {"id" sid
                                                                    "status" "running"
                                                                    "current_turn_id" "turn-42"
                                                                    "running_request" "Check"
                                                                    "running_started_at"
                                                                    (+ created-at 10000)}
                                                             (= source :soul)
                                                             (assoc "running_position"
                                                               42 "running_created_at"
                                                               created-at)))
                           client/gateway-list-turns (constantly (if (= source :turn)
                                                                   [{"turn_id" "turn-42"
                                                                     "status" "running"
                                                                     "position" 42
                                                                     "created_at" created-at}]
                                                                   []))
                           chat/history-page (fn [& _]
                                               {:messages []})
                           client/worker-future (fn [& _])
                           client/cancellation-set-future! (fn [& _])]

               (let [resumed (chat/resume-session sid)]
                 (reset! state/app-db {:session resumed :active-tab-id sid :render-version 0})
                 (state/dispatch [:attach-running-turn nil resumed])
                 (expect (= [42 42] (mapv :turn-position (:messages @state/app-db))))
                 (expect (= [(java.util.Date. created-at) (java.util.Date. created-at)]
                            (mapv :timestamp (:messages @state/app-db)))))))
           (finally (reset! state/app-db before))))))

(defdescribe narrow-header-keeps-the-date-and-shortens-the-fork-label
             (it "narrow header keeps the date and shortens the fork label"
                 (let [{:keys [capture regions]}
                       (header-frame review-message 40 {:agent-name "助手 with a long name"})

                       header
                       (second (str/split-lines (cap/frame-text capture)))]

                   (expect (nil? (:error capture)))
                   (expect (str/includes? header
                                          (str (client/format-date (:timestamp review-message))
                                               " /  Fork")))
                   (expect (= 1 (count regions)))
                   (expect (<= 2 (get-in (first regions) [:bounds :col])))
                   (expect (= 6 (get-in (first regions) [:bounds :width]))))))

(defdescribe compact-action-preserves-the-agent-name-at-the-width-boundary
             (it "compact action preserves the agent name at the width boundary"
                 (doseq [width [46 47 48]]
                   (let [{:keys [regions]} (header-frame review-message width {})]
                     (expect (= (if (< width 48) 6 21)
                                (get-in (first regions) [:bounds :width])))))))

(defdescribe
  only-persisted-assistant-turns-offer-forking
  (it "only persisted assistant turns offer forking"
      (doseq [message [(dissoc review-message :session-turn-id) (assoc review-message :role :user)
                       (assoc review-message :pending? true) (assoc review-message :status :queued)
                       (assoc review-message :status :running)]]
        (let [{:keys [capture regions]} (header-frame message 76 {})]
          (expect (nil? (:error capture)))
          (expect (empty? regions))))
      (doseq [status [:completed :cancelled :failed]]
        (let [{:keys [regions]} (header-frame (assoc review-message :status status) 76 {})]
          (expect (= 1 (count regions)))))))

(defdescribe clipped-headers-never-register-invisible-buttons
             (doseq [opts [{:start -1} {:start 12} {:viewport-h 0}]]
               (it (str opts) (expect (empty? (:regions (header-frame review-message 76 opts)))))))

(defn await-value
  "Wait for a production render or input transition, returning its observed value."
  [f]
  (let [deadline (+ (System/nanoTime) 5000000000)]
    (loop []

      (if-let [value (f)]
        value
        (when (< (System/nanoTime) deadline) (Thread/sleep 10) (recur))))))

(defn start-fork-screen!
  "Run the real TUI with an in-memory gateway boundary; close! restores its state."
  [terminal fail?]
  (let [before
        @state/app-db

        _
        (.reset interactions/hit-map)

        source-id
        "11111111-1111-4111-8111-111111111111"

        forked-id
        "22222222-2222-4222-8222-222222222222"

        history
        [(assoc review-message
           :session-turn-id "turn-1"
           :text "First answer.") review-message]

        requests
        (atom [])

        notices
        (atom [])

        error
        (atom nil)

        sidebar-reads
        (atom [])

        config
        {:providers [{:id :openai-codex :models [{:name "gpt-5.5"}]}]}

        no-op
        (fn [& _]
          nil)

        bindings
        (merge
          (zipmap [#'screen/configure-terminal-input! #'screen/probe-terminal-cell-size!
                   #'screen/register-terminal-interrupt-handlers! #'screen/enable-terminal-state!
                   #'screen/disable-terminal-state! #'screen/install-ssh-passphrase-prompt!
                   #'screen/start-provider-limits-thread! #'screen/start-workspace-refresh-thread!
                   #'screen/ensure-launch-project-id! #'screen/latest-project-session-id
                   #'screen/session-db-title #'screen/persist-tabs!
                   #'screen/release-workspace-sessions! #'screen/start-deferred-gateway-slash-load!
                   #'client/gateway-reconcile-running-turns! #'client/reload-config!
                   #'client/toggles-hydrate-from-config! #'client/save-toggles!
                   #'client/watch-notifications! #'client/unwatch-notifications!
                   #'client/add-channel-event-listener! #'client/remove-channel-event-listener!
                   #'client/gateway-release-session! #'timg/images-protocol]
                  (repeat no-op))
          {#'screen/create-terminal! (constantly terminal)
           #'screen/start-projects-refresh! (constantly no-op)
           #'screen/refresh-projects! (fn [& args]
                                        (swap! sidebar-reads conj (vec args)))
           #'screen/session-workspace (constantly {"root" "/tmp"})
           #'screen/subscribe-session-live! (constantly no-op)
           #'client/toggle-add-listener! (constantly no-op)
           #'client/load-config (constantly config)
           #'client/load-config-raw (constantly {})
           #'client/notify! (fn [message & _]
                              (swap! notices conj message))
           #'chat/resume-session (fn [sid]
                                   {:id sid
                                    :history (if (= source-id sid) history [(first history)])})
           #'client/fork-session!
           (fn [sid tid]
             (swap! requests conj [sid tid])
             (if fail? (throw (ex-info "Fork unavailable" {})) {"id" forked-id}))})

        runner
        (future (try (with-redefs-fn bindings #(screen/run-chat! {:session-id source-id}))
                     (catch Throwable t (reset! error t))))]

    {:source-id source-id
     :forked-id forked-id
     :history history
     :requests requests
     :notices notices
     :sidebar-reads sidebar-reads
     :error error
     :runner runner
     :close! (fn []
               (state/dispatch [:shutdown])
               (.addInput ^DefaultVirtualTerminal terminal (KeyStroke. KeyType/Escape))
               (deref runner 5000 nil)
               (reset! state/app-db before))}))

(defdescribe
  clicking-a-header-forks-that-turn-once-and-opens-a-new-tab
  (it
    "clicking a header forks that turn once and opens a new tab"
    (doseq [[gesture fail?] [[[MouseActionType/CLICK_DOWN MouseActionType/CLICK_RELEASE] false]
                             [[MouseActionType/CLICK_RELEASE] false]
                             [[MouseActionType/CLICK_DOWN MouseActionType/CLICK_RELEASE] true]]]
      (let [terminal (DefaultVirtualTerminal. (TerminalSize. 100 30))
            {:keys [source-id forked-id history requests notices sidebar-reads error close!]}
            (start-fork-screen! terminal fail?)]

        (try (let [fork-hit
                   #(some (fn [r]
                            (when (and (= :fork-at-turn (:kind r)) (= "turn-1" (:turn-id r))) r))
                          (.current interactions/hit-map))
                   ;; Open the rail on the running screen; startup owns the state before it.
                   first-hit (await-value fork-hit)
                   _ (state/dispatch [:project-sidebar {:open? true}])
                   hit (await-value #(when-let [r (fork-hit)] (when (not= r first-hit) r)))
                   {:keys [col row]} (:bounds hit)]

               (expect (some? hit))
               (when hit
                 ;; The docked rail moves the chat right; hit bounds stay chat-local.
                 (let [left (:chat-left (projects/geometry @state/app-db 100 30))]
                   (doseq [action gesture]
                     (.addInput
                       terminal
                       (MouseAction. action 1 (TerminalPosition. (int (+ col left)) (int row)))))))
               (expect (some? (await-value #(seq @notices))))
               ;; Regression, user report in this Vis session: the fork reached its
               ;; group in the open sidebar only at the next poll.
               (expect (= (if fail? [] [[true]]) @sidebar-reads))
               (expect (= [[source-id "turn-1"]] @requests))
               (if fail?
                 (do (expect (= source-id (get-in @state/app-db [:session :id])))
                     (expect (= 1 (count (:tabs @state/app-db))))
                     (expect (= history (:messages @state/app-db))))
                 (do (expect (= forked-id (get-in @state/app-db [:session :id])))
                     (expect (= 2 (count (:tabs @state/app-db))))
                     (expect (= [(first history)] (:messages @state/app-db))))))
             (finally (close!)))
        (expect (nil? @error))))))
