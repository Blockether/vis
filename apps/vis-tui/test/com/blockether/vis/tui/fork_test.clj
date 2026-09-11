(ns com.blockether.vis.tui.fork-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.terminal-image :as timg]
            [com.blockether.vis.tui.virtual :as virtual]
            [com.blockether.vis.tui.theme :as theme]
            [com.blockether.vis.tui.shared-theme :as shared]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]])
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

(deftest persisted-answer-has-a-fork-button-before-the-date
  (let [{:keys [capture regions]}
        (header-frame review-message 76 {:viewport-top 4})

        header
        (second (str/split-lines (cap/frame-text capture)))

        hit
        (first regions)

        date
        (client/format-date (:timestamp review-message))]

    (is (nil? (:error capture)))
    (is (str/includes? header (str "Fork at this turn  | " date)))
    (is (= 1 (count regions)))
    (is (= {:kind :fork-at-turn :session-id "session-1" :turn-id "turn-2"}
           (select-keys hit [:kind :session-id :turn-id])))
    (is (= 5 (get-in hit [:bounds :row])))
    (is (= (.indexOf ^String header " Fork at this turn ") (get-in hit [:bounds :col])))
    (is (= (count " Fork at this turn ") (get-in hit [:bounds :width])))))

(deftest fork-hover-uses-readable-ink-and-only-highlights-the-target-turn
  (let [original @theme/active-theme-id]
    (try (doseq [id (keys shared/built-in-themes)]
           (theme/apply-theme! id)
           (doseq [turn-id ["turn-2" "another-turn"]]
             (let [{:keys [capture regions]}
                   (header-frame review-message 76 {:hover-turn-id turn-id})
                   col (get-in (first regions) [:bounds :col])
                   cell (get-in capture [:frames 0 1 col])
                   hovered? (= "turn-2" turn-id)
                   ink theme/button-fg]

               (is (= [(.getRed ink) (.getGreen ink) (.getBlue ink)] (:fg cell)))
               (is (= hovered? (:bold cell)))
               (is (= hovered? (:underline cell))))))
         (finally (theme/apply-theme! original)))))

(deftest narrow-header-keeps-the-date-and-shortens-the-fork-label
  (let [{:keys [capture regions]}
        (header-frame review-message 36 {:agent-name "助手 with a long name"})

        header
        (second (str/split-lines (cap/frame-text capture)))]

    (is (nil? (:error capture)))
    (is (str/includes? header (str "Fork  | " (client/format-date (:timestamp review-message)))))
    (is (= 1 (count regions)))
    (is (<= 2 (get-in (first regions) [:bounds :col])))
    (is (= 6 (get-in (first regions) [:bounds :width])))))

(deftest compact-action-preserves-the-agent-name-at-the-width-boundary
  (doseq [width [40 41 42]]
    (let [{:keys [capture regions]} (header-frame review-message width {})
          header (second (str/split-lines (cap/frame-text capture)))]

      (is (str/starts-with? header "  Vis "))
      (is (= (if (< width 42) 6 19) (get-in (first regions) [:bounds :width]))))))

(deftest fork-button-does-not-depend-on-timestamps
  (let [{:keys [capture regions]}
        (header-frame review-message 76 {:timestamps? false})

        header
        (second (str/split-lines (cap/frame-text capture)))]

    (is (str/includes? header "Fork at this turn"))
    (is (not (str/includes? header "|")))
    (is (= 1 (count regions)))))

(deftest only-persisted-assistant-turns-offer-forking
  (doseq [message [(dissoc review-message :session-turn-id) (assoc review-message :role :user)
                   (assoc review-message :pending? true) (assoc review-message :status :queued)
                   (assoc review-message :status :running)]]
    (let [{:keys [capture regions]} (header-frame message 76 {})]
      (is (nil? (:error capture)))
      (is (not (str/includes? (cap/frame-text capture) "Fork")))
      (is (empty? regions))))
  (doseq [status [:completed :cancelled :failed]]
    (let [{:keys [regions]} (header-frame (assoc review-message :status status) 76 {})]
      (is (= 1 (count regions))))))

(deftest clipped-headers-never-register-invisible-buttons
  (doseq [opts [{:start -1} {:start 12} {:viewport-h 0}]]
    (testing (str opts) (is (empty? (:regions (header-frame review-message 76 opts)))))))

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

        config
        {:providers [{:id :openai-codex :models [{:name "gpt-5.5"}]}]}

        no-op
        (fn [& _]
          nil)

        bindings
        (merge (zipmap
                 [#'screen/configure-terminal-input! #'screen/probe-terminal-cell-size!
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
                #'screen/session-workspace (constantly {:root "/tmp"})
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
     :error error
     :runner runner
     :close! (fn []
               (state/dispatch [:shutdown])
               (.addInput ^DefaultVirtualTerminal terminal (KeyStroke. KeyType/Escape))
               (deref runner 5000 nil)
               (reset! state/app-db before))}))

(deftest clicking-a-header-forks-that-turn-once-and-opens-a-new-tab
  (doseq [[gesture fail?] [[[MouseActionType/CLICK_DOWN MouseActionType/CLICK_RELEASE] false]
                           [[MouseActionType/CLICK_RELEASE] false]
                           [[MouseActionType/CLICK_DOWN MouseActionType/CLICK_RELEASE] true]]]
    (let [terminal (DefaultVirtualTerminal. (TerminalSize. 100 30))
          {:keys [source-id forked-id history requests notices error close!]}
          (start-fork-screen! terminal fail?)]

      (try
        (let [hit (await-value
                    #(some (fn [r]
                             (when (and (= :fork-at-turn (:kind r)) (= "turn-1" (:turn-id r))) r))
                           (.current interactions/hit-map)))
              {:keys [col row]} (:bounds hit)]

          (is (some? hit))
          (when hit
            (doseq [action gesture]
              (.addInput terminal (MouseAction. action 1 (TerminalPosition. (int col) (int row))))))
          (is (some? (await-value #(seq @notices))))
          (is (= [[source-id "turn-1"]] @requests))
          (if fail?
            (do (is (= source-id (get-in @state/app-db [:session :id])))
                (is (= 1 (count (:tabs @state/app-db))))
                (is (= history (:messages @state/app-db)))
                (is (= ["Fork unavailable"] @notices)))
            (do (is (= forked-id (get-in @state/app-db [:session :id])))
                (is (= 2 (count (:tabs @state/app-db))))
                (is (= [(first history)] (:messages @state/app-db)))
                (is (= ["Forked session at turn"] @notices)))))
        (finally (close!)))
      (is (nil? @error)))))
