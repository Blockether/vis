(ns com.blockether.vis.ext.channel-tui.screen-test
  "Tests for the TUI channel entry point. The bulk of the namespace
   is Lanterna-bound and exercised by the integration smoke + render
   benchmark; this suite focuses on the pure helpers - currently the
   `--session-id` / `--resume` argument parser, where a silent
   accept of unknown flags previously masked typos like
   `--sessions-id`."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.keymap :as keymap]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.scroll :as scroll]
            [com.blockether.vis.ext.channel-tui.screen :as screen]
            [com.blockether.vis.ext.channel-tui.selection :as selection]
            [com.blockether.vis.internal.external-opener :as opener]
            [com.blockether.vis.ext.channel-tui.state :as state]
            [com.blockether.vis.ext.channel-tui.terminal-image :as timg]
            [com.blockether.vis.ext.channel-tui.terminals :as term]
            [com.blockether.vis.ext.channel-tui.virtual :as virtual]
            [lazytest.core :refer [defdescribe it expect]])
  (:import [com.googlecode.lanterna TerminalPosition]
           [com.googlecode.lanterna.screen TerminalScreen]
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

(def ^:private live-view-wheel-event (deref #'screen/live-view-wheel-event))

(def ^:private smooth-wheel! (deref #'screen/smooth-wheel!))

(def ^:private release-wheel-momentum! (deref #'screen/release-wheel-momentum!))
(def ^:private activate-live-region! (deref #'screen/activate-live-region!))

(def ^:private header-hover-only-change? (deref #'screen/header-hover-only-change?))

(def ^:private handle-channel-event! (deref #'screen/handle-channel-event!))

(def ^:private submit-input! (deref #'screen/submit-input!))

(def ^:private registry-slash-commands (deref #'screen/registry-slash-commands))

(def ^:private slash-spec->menu-command (deref #'screen/slash-spec->menu-command))

(def ^:private menu-commands (deref #'screen/menu-commands))

(def ^:private slash-suggestions-for-db (deref #'screen/slash-suggestions-for-db))

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

(def ^:private bubble-copy-regions (deref #'screen/bubble-copy-regions))

(def ^:private disclosure-copy-regions (deref #'screen/disclosure-copy-regions))

(def ^:private bubble-copy-hit (deref #'screen/bubble-copy-hit))

(def ^:private selected-transcript-text (deref #'screen/selected-transcript-text))

(def ^:private release-selection-focus (deref #'screen/release-selection-focus))

(def ^:private selection-copy-payload (deref #'screen/selection-copy-payload))

(def ^:private input-selectable-ranges (deref #'screen/input-selectable-ranges))

(def ^:private session-summary (deref #'screen/session-summary))

(def ^:private latest-modified-first (deref #'screen/latest-modified-first))

(def ^:private session-sort-key (deref #'screen/session-sort-key))

(def ^:private apply-draft-picker-choice! (deref #'screen/apply-draft-picker-choice!))

(def ^:private pre-resolve-session-id! (deref #'screen/pre-resolve-session-id!))

(def ^:private terminal-ctrl-c-behaviour (deref #'screen/terminal-ctrl-c-behaviour))

(def ^:private terminal-interrupt-action (deref #'screen/terminal-interrupt-action))

(def ^:private handle-terminal-interrupt! (deref #'screen/handle-terminal-interrupt!))

(def ^:private print-session-id-on-exit! (deref #'screen/print-session-id-on-exit!))

(def ^:private authenticated-provider-config (deref #'screen/authenticated-provider-config))

(def ^:private enable-terminal-escape-modes! (deref #'screen/enable-terminal-escape-modes!))

(def ^:private disable-terminal-escape-modes! (deref #'screen/disable-terminal-escape-modes!))

(defn- user-error?
  "True when `f` throws an ex-info carrying the `:vis/user-error` flag -
   the contract the channel entry point relies on to print a clean
   `vis-agent: <msg>` line and exit 2 instead of a Java stack trace."
  [f]
  (try (f) false (catch clojure.lang.ExceptionInfo e (true? (:vis/user-error (ex-data e))))))

(defdescribe
  terminal-control-mode-lifecycle-test
  (it
    "disables IXON on screen setup and restores it on teardown"
    (let [calls
          (atom [])

          record
          (fn [event]
            (fn [& _]
              (swap! calls conj event)))]

      (with-redefs [vis/tty-out
                    (delay (java.io.ByteArrayOutputStream.))

                    input/enable-bracketed-paste!
                    (record :enable-paste)

                    input/enable-sgr-mouse!
                    (record :enable-mouse)

                    input/disable-literal-next!
                    (record :disable-iexten)

                    input/disable-software-flow-control!
                    (record :disable-ixon)

                    input/set-default-bg!
                    (record :set-bg)

                    input/disable-bracketed-paste!
                    (record :disable-paste)

                    input/disable-sgr-mouse!
                    (record :disable-mouse)

                    input/reset-default-bg!
                    (record :reset-bg)

                    input/restore-software-flow-control!
                    (record :restore-ixon)

                    input/restore-literal-next!
                    (record :restore-iexten)]

        (enable-terminal-escape-modes! nil)
        (disable-terminal-escape-modes! nil))
      (expect (= [:enable-paste :enable-mouse :disable-iexten :disable-ixon :set-bg :disable-paste
                  :disable-mouse :reset-bg :restore-ixon :restore-iexten]
                 @calls)))))

(defdescribe
  render-heartbeat-test
  (it "keeps live render heartbeat at 80ms" (expect (= 80 (deref #'screen/spinner-tick-ms))))
  (it "classifies progress-only loading ticks for partial repaint"
      (let [base {:loading? true
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
      (let [base
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
      (let [base
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
      (let [base
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
                 (let [up
                       (MouseAction. MouseActionType/SCROLL_UP 1 (TerminalPosition. 10 4))

                       down
                       (MouseAction. MouseActionType/SCROLL_DOWN 1 (TerminalPosition. 10 4))

                       click
                       (MouseAction. MouseActionType/CLICK_DOWN 1 (TerminalPosition. 10 4))]

                   (expect (= -1 (mouse-wheel-delta up)))
                   (expect (= 1 (mouse-wheel-delta down)))
                   (expect (nil? (mouse-wheel-delta click)))))
             (it "coalesces wheel floods and preserves first non-wheel key"
                 (let [first-wheel
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
                 (let [first-wheel
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

;; Reported in Vis session 22b3489b-336f-42d0-9bc8-806dff2de86f: scrolling a live view
;; with a MacBook trackpad crawled at a third of the speed of the transcript beside it,
;; and the gesture's own inertia tail bounced the pane back against the reader.
(defdescribe
  live-view-wheel-physics-test
  (it "steps a tall pane by the shared notch and a compact one by one row"
      (with-redefs-fn {(ns-resolve 'com.blockether.vis.ext.channel-tui.screen 'live-band-pane)
                       (fn [_db _my]
                         {:view {:id "view-1"} :visible 20})}
        (fn []
          (expect (= [:live-view-scroll "view-1" -6] (live-view-wheel-event {} 9 -2)))))
      (with-redefs-fn {(ns-resolve 'com.blockether.vis.ext.channel-tui.screen 'live-band-pane)
                       (fn [_db _my]
                         {:view {:id "view-1"} :visible 4})}
        (fn []
          (expect (= [:live-view-scroll "view-1" -2] (live-view-wheel-event {} 9 -2))))))
  (it "absorbs an inertia tail reversal on the band's own momentum"
      (let [mom
            (volatile! 0)

            at
            (volatile! 0)]

        (expect (= -3 (:eff (smooth-wheel! mom at -3))))
        (expect (nil? (:eff (smooth-wheel! mom at 1))))
        (expect (neg? (long @mom)))))
  (it "keeps the band's momentum and the transcript's apart"
      (let [mom
            (volatile! 0)

            at
            (volatile! 0)

            live-mom
            (volatile! 0)

            live-at
            (volatile! 0)]

        (smooth-wheel! mom at -3)
        (expect (zero? (long @live-mom)))
        (smooth-wheel! live-mom live-at 2)
        (expect (= -3 (long @mom)))
        (expect (= 2 (long @live-mom)))))
  (it "releases a surface's directional lock only once the hold window expired"
      (let [mom
            (volatile! 5)

            at
            (volatile! (System/currentTimeMillis))]

        (release-wheel-momentum! mom at :live-view)
        (expect (= 5 (long @mom)))
        (vreset! at (- (System/currentTimeMillis) (* 2 (long scroll/momentum-hold-ms))))
        (release-wheel-momentum! mom at :live-view)
        (expect (zero? (long @mom))))))
;; Reported in Vis session a64d44c2-8228-455f-926e-b3381f19a93b: wheel input
;; accelerated when it crossed into the live table, its rows had no TUI action, and
;; the live transient had no minimize/restore action.
(defdescribe
  live-view-pointer-actions-test
  (it "keeps a coalesced live-table gesture at terminal row granularity"
      (with-redefs-fn {(ns-resolve 'com.blockether.vis.ext.channel-tui.screen 'live-band-pane)
                       (fn [_db _my]
                         {:view {:id "view-1"}})}
        (fn []
          (expect (= [:live-view-scroll "view-1" -2] (live-view-wheel-event {} 9 -2))))))
  (it "returns wheel input over a compact status line to the transcript"
      (with-redefs-fn {(ns-resolve 'com.blockether.vis.ext.channel-tui.screen 'live-band-pane)
                       (fn [_db _my]
                         {:view {:id "view-1"} :is-minimized true})}
        (fn []
          (expect (nil? (live-view-wheel-event {} 9 -2))))))
  (it "routes minimize and restore clicks through pane state only"
      (let [events (atom [])]
        (with-redefs [state/dispatch #(swap! events conj %)]
          (expect (true? (activate-live-region! {} {:kind :live-minimize :view-id "view-1"})))
          (expect (true? (activate-live-region! {} {:kind :live-restore :view-id "view-1"}))))
        (expect (= [[:live-view-minimize "view-1"] [:bump-render-version]
                    [:live-view-restore "view-1"] [:bump-render-version]]
                   @events))))
  (it "routes a clicked row through shared local or gateway focus"
      (let [remote-called
            (promise)

            local-called
            (promise)

            db
            {:live-views [{:view {:id "view-1" :session-id "session-1"}}]}

            hit
            {:kind :live-focus :view-id "view-1" :node-id "jobs" :item-id "macos"}]

        (with-redefs [vis/live-views
                      (constantly [])

                      vis/gateway-focus-live-view!
                      (fn [& args]
                        (deliver remote-called args))]

          (expect (true? (activate-live-region! db hit)))
          (expect (= ["session-1" "view-1" "jobs" ["macos"]]
                     (deref remote-called 1000 ::timed-out))))
        (with-redefs [vis/live-views
                      (constantly [{:id "view-1"}])

                      vis/focus-live-view!
                      (fn [& args]
                        (deliver local-called args))]

          (expect (true? (activate-live-region! db hit)))
          (expect (= ["view-1" "jobs" ["macos"]] (deref local-called 1000 ::timed-out)))))))

(defdescribe drag-coalescing-test
             (it "coalesces drag bursts and keeps last drag event + first non-drag"
                 (let [d1
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
      (let [adapted (slash-spec->menu-command {:slash/name "new"
                                               :slash/parent ["workspace"]
                                               :slash/doc "Create workspace"})]
        (expect (= :workspace.new (:id adapted)))
        (expect (= "/workspace new" (:slash/text adapted)))
        (expect (= "Create workspace" (:label adapted)))
        (expect (= "workspace new" (:slash/name adapted)))))
  (it "registry-slash-commands lists children but hides their group root"
      (with-redefs [vis/registered-slashes
                    (constantly
                      [{:slash/name "workspace" :slash/doc "Workspace ops"}
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
      (with-redefs [vis/registered-slashes (constantly [{:slash/name "voice"
                                                         :slash/doc "Voice toggle"}])]
        (let [ids (mapv :id (menu-commands nil))]
          (expect (some #{:new-session} ids))
          (expect (some #{:voice} ids)))))
  (it "Ctrl+K palette gets no registry slash roots by default"
      (with-redefs [vis/registered-slashes
                    (constantly [{:slash/name "voice" :slash/doc "Voice toggle"}
                                 {:slash/name "workspace" :slash/doc "Workspace ops"}])]
        (expect (= [] (command-palette-extra-commands))))))

(defdescribe channel-status-error-routing-test
             (it "routes error status events to the notification lane only"
                 (let [events
                       (atom [])

                       notified
                       (atom nil)]

                   (with-redefs [state/dispatch
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
                   (with-redefs [state/dispatch (fn [event]
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

(defdescribe
  draft-picker-gateway-test
  (it "keeps the selected current location as a no-op"
      (let [calls (atom [])]
        (with-redefs [vis/gateway-stash-draft! (fn [& xs]
                                                 (swap! calls conj xs))
                      vis/gateway-resume-draft! (fn [& xs]
                                                  (swap! calls conj xs))]

          (expect (= {:changed? false :message "Already on feature-a"}
                     (apply-draft-picker-choice!
                       "sid"
                       {:action :draft :label "feature-a" :current? true})))
          (expect (empty? @calls)))))
  (it "routes trunk and draft switches through the canonical gateway APIs"
      (let [calls
            (atom [])

            trunk
            {"root" "/repo"}

            draft
            {"root" "/draft/feature-b"}]

        (with-redefs [vis/gateway-stash-draft!
                      (fn [sid]
                        (swap! calls conj [:stash sid])
                        trunk)

                      vis/gateway-resume-draft!
                      (fn [sid wid]
                        (swap! calls conj [:resume sid wid])
                        draft)]

          (expect (= trunk
                     (:workspace (apply-draft-picker-choice! "sid"
                                                             {:action :trunk :label "Trunk"}))))
          (expect (= draft
                     (:workspace (apply-draft-picker-choice!
                                   "sid"
                                   {:action :draft :workspace-id "ws-b" :label "feature-b"}))))
          (expect (= [[:stash "sid"] [:resume "sid" "ws-b"]] @calls))))))
(it "routes create and abandon through canonical gateway APIs"
    (let [calls
          (atom [])

          created
          {"root" "/draft/new"}

          trunk
          {"root" "/repo"}]

      (with-redefs [vis/gateway-create-draft!
                    (fn [sid label clean?]
                      (swap! calls conj [:create sid label clean?])
                      created)

                    vis/gateway-abandon-draft!
                    (fn [sid wid reason]
                      (swap! calls conj [:abandon sid wid reason])
                      trunk)]

        (expect (= created
                   (:workspace (apply-draft-picker-choice! "sid"
                                                           {:action :new :label "feature-c"}))))
        (expect (= trunk
                   (:workspace (apply-draft-picker-choice! "sid"
                                                           {:action :abandon
                                                            :workspace-id "ws-c"
                                                            :label "feature-c"
                                                            :reason "not needed"}))))
        (expect (= [[:create "sid" "feature-c" false] [:abandon "sid" "ws-c" "not needed"]]
                   @calls)))))

(defdescribe terminal-interrupt-test
             (it "configures Lanterna to trap Ctrl+C instead of exiting inside pollInput"
                 (expect (= UnixLikeTerminal$CtrlCBehaviour/TRAP (terminal-ctrl-c-behaviour))))
             (it "clears a non-empty draft before quitting on terminal interrupts"
                 (expect (= :clear-input
                            (terminal-interrupt-action {:input (input/paste-text (input/empty-input)
                                                                                 "draft")})))
                 (expect (= :quit (terminal-interrupt-action {:input (input/empty-input)}))))
             (it "dispatches reset-input for the first interrupt and shutdown for the next"
                 (let [old-db
                       @state/app-db

                       events
                       (atom [])]

                   (try (with-redefs [state/dispatch (fn [event]
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
      (with-redefs [screen/redirect-stdio-to-log! (fn []
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

(defdescribe authenticated-provider-startup-config-test
             (it "uses authenticated OAuth presets when no persisted config exists"
                 (let [providers [{:id :openai-codex :models [{:name "gpt-5.5"}]}]]
                   (with-redefs [vis/authenticated-preset-providers (constantly providers)]
                     (expect (= {:providers providers} (authenticated-provider-config))))))
             (it "falls through to onboarding when no authenticated preset is available"
                 (with-redefs [vis/authenticated-preset-providers (constantly [])]
                   (expect (nil? (authenticated-provider-config))))))

(defdescribe startup-resume-test
             (it "--session-id reconciles orphaned running turns before rebuilding history"
                 ;; The sweep now goes through the gateway (`gateway-reconcile-running-turns!`)
                 ;; rather than poking the DB directly — but it must STILL run before the
                 ;; resume so the rebuilt history carries no stale :running turns.
                 (let [calls
                       (atom [])

                       resumed
                       {:id "c1" :history [{:role :assistant :text "interrupted"}]}]

                   (with-redefs [vis/gateway-reconcile-running-turns!
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
      (with-redefs [vis/gateway-list-turns
                    (fn [session-id]
                      (case session-id
                        "old"
                        [{"created_at" #inst "2024-01-04T00:00:00.000-00:00"}]

                        "new"
                        [{"created_at" #inst "2024-01-02T00:00:00.000-00:00"}
                         {"created_at" #inst "2024-01-08T00:00:00.000-00:00"}]

                        []))]
        (let [old-summary (session-summary {"id" "old"
                                            "created_at" #inst "2024-01-01T00:00:00.000-00:00"})
              new-summary (session-summary {"id" "new"
                                            "created_at" #inst "2024-01-03T00:00:00.000-00:00"})]

          (expect (= 1 (get old-summary "turn_count")))
          (expect (= 2 (get new-summary "turn_count")))
          (expect (= #inst "2024-01-08T00:00:00.000-00:00" (get new-summary "modified_at")))
          (expect (= ["new" "old"]
                     (mapv #(get % "id") (latest-modified-first [old-summary new-summary]))))))))

(defdescribe
  submit-input-test
  (it "dispatches send before reset so paste placeholders can expand"
      (let [events
            (atom [])

            payload
            "therapy line 1\ntherapy line 2"

            token
            (input/format-paste-placeholder {:id 1 :content payload})

            input-state
            (input/paste-text (input/empty-input) (str "context " token))]

        (with-redefs [state/dispatch (fn [event]
                                       (swap! events conj event))]
          (submit-input! {:session {:id "c1"} :loading? false :active-tab-id :tab-a} input-state)
          ;; The submitting tab is pinned into the event so a tab
          ;; switch between Enter and the reduce cannot reroute it.
          (expect (= [[:send-message (str "context " token) :tab-a] [:reset-input]] @events))))))

(defdescribe
  selectable-ranges-test
  (it
    "clips transcript selection to message content rows only"
    (expect
      (= [{:row 4 :col 2 :width 11 :line-id 0} {:row 5 :col 2 :width 11 :line-id 0}]
         (bubble-selectable-ranges
           {:visible
            [{:top -1 :height 4 :projected {:role :assistant :prewrapped-lines ["first" "second"]}}
             {:top 4 :height 3 :projected {:role :assistant :prewrapped-lines ["below viewport"]}}]}
           4
           5
           20))))
  (it "keeps assistant code selection one column inside and answers aligned with Vis"
      (expect (= [{:row 5 :col 3 :width 11 :line-id 0} {:row 6 :col 2 :width 11 :line-id 1}]
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
      (expect (= [{:row 6 :col 4 :width 11 :line-id 0}]
                 (bubble-selectable-ranges
                   {:visible [{:top 0 :height 5 :projected {:role :user :text "siema"}}]}
                   4
                   6
                   20))))
  (it "sorts sessions by real turns, latest modified time, then turn count by default"
      (let [old-with-turns
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
  (it
    "copies transcript content without role labels, answer separators, or model metadata"
    (let [ranges
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
      (let [message
            {:role :assistant :prewrapped-lines ["line zero" "line one" "line two" "line three"]}

            layout
            {:total-h 6 :heights [6] :offsets [0 6]}

            sel
            {:anchor (selection/point 0 1) :focus (selection/point 39 4)}]

        (expect (= "line zero\nline one\nline two\nline three"
                   (selected-transcript-text [message] layout 40 {} {} sel)))))
  (it "strips the baked output indent from result rows and keeps highlight/copy columns aligned"
      ;; Regression: `render/->result` bakes `tool-output-indent` into MARKER_RESULT
      ;; op-card body rows. `selection-output-indent-markers` must list that marker so
      ;; the copy path drops the inset from the text AND shifts the selectable column
      ;; by the indent width — otherwise result-row copy keeps stray leading spaces and
      ;; the highlighted cells drift 2 columns left of the text the user sees.
      (let [result-line
            (str p/MARKER_RESULT "  the result body text")

            plain-line
            (str p/MARKER_RESULT "no-indent result")

            message
            {:role :assistant :prewrapped-lines [plain-line result-line]}

            layout
            {:total-h 4
             :heights [4]
             :offsets [0 4]
             :visible [{:top 0 :height 4 :idx 0 :projected message}]}

            ;; result row is document/screen row 2 (content-top = offset 0 + 1)
            sel
            {:anchor (selection/point 0 2) :focus (selection/point 200 2)}

            ranges
            (bubble-selectable-ranges layout 0 6 40)

            plain-col
            (:col (first (filter #(= 1 (:row %)) ranges)))

            result-col
            (:col (first (filter #(= 2 (:row %)) ranges)))]

        (expect (= "the result body text" (selected-transcript-text [message] layout 40 {} {} sel))
                "copied result text has no baked leading indent")
        (expect (= 2 (- (long result-col) (long plain-col)))
                "result row is selectable starting after the 2-col output indent")))
  ;; Regression, issue #119: a failed turn's error card is painted like a user
  ;; bubble - one pad row under the role label, text inset by the bubble's
  ;; horizontal padding - but selection geometry only made that adjustment for
  ;; `:user` messages. Every range on a provider/turn failure therefore named the
  ;; blank pad row ABOVE the sentence and started 2 columns left of it, so a drag
  ;; over the error copied an empty string and the reason could only be read out
  ;; of `~/.vis/vis.log`.
  (it
    "selects the failed-turn error card on the rows and columns it is painted on"
    (let [message
          {:role :assistant
           :status :failed
           :content
           [{"id" "e1" "type" "error" "code" "turn_failed" "message" "provider stream stalled"}]
           :prewrapped-lines [(str p/MARKER_ANSWER_TXT "turn_failed provider stream stalled")]}

          layout-of
          (fn [msg]
            {:total-h 6
             :heights [6]
             :offsets [0 6]
             :visible [{:idx 0 :top 0 :height 6 :projected msg}]})

          ;; The same bubble WITHOUT error blocks: a plain answer, painted flush
          ;; against the message column on the row right below the role label.
          plain-range
          (first (bubble-selectable-ranges (layout-of (dissoc message :content)) 0 10 60))

          error-range
          (first (bubble-selectable-ranges (layout-of message) 0 10 60))

          ;; Screen rows as `render/draw-chat-bubble!` paints the card: role label,
          ;; pad row, then the sentence inset by `h-pad`.
          rows
          (mapv #(format "%-60s" %)
                ["  Vis" ""
                 (str (apply str (repeat (+ (long render/MESSAGE_MARGIN_LEFT) 2) \space))
                      "turn_failed provider stream stalled") "" "" ""])]

      (expect (= (inc (long (:row plain-range))) (long (:row error-range)))
              "the card's pad row pushes its text one row down")
      (expect (= (+ 2 (long (:col plain-range))) (long (:col error-range)))
              "the card's text is inset by the bubble's horizontal padding")
      (expect (= "turn_failed provider stream stalled"
                 (selection/selected-text rows
                                          {:anchor (selection/point 0 0)
                                           :focus (selection/point 59 5)}
                                          (bubble-selectable-ranges (layout-of message) 0 10 60)))
              "dragging over the card copies the error sentence")))
  (it
    "copies a multi-bubble chunk that auto-scrolled off-screen while dragging"
    (let [messages
          [{:role :assistant :prewrapped-lines ["Line A0" "Line A1" "Line A2"]}
           {:role :assistant :prewrapped-lines ["Line B0" "Line B1"]}
           {:role :assistant :prewrapped-lines ["Line C0" "Line C1" "Line C2"]}
           {:role :assistant :prewrapped-lines ["Line D0" "Line D1"]}]

          ;; Viewport scrolled to the bottom: only the last bubble is on
          ;; screen, every earlier bubble has scrolled out of :visible.
          layout
          {:total-h 18
           :heights [5 4 5 4]
           :offsets [0 5 9 14 18]
           :visible [{:idx 3
                      :top 14
                      :height 4
                      :projected {:role :assistant :prewrapped-lines ["Line D0" "Line D1"]}}]}]

      ;; Whole-document drag still copies every off-screen row, not just
      ;; the one visible bubble.
      (expect
        (=
          "Line A0\nLine A1\nLine A2\nLine B0\nLine B1\nLine C0\nLine C1\nLine C2\nLine D0\nLine D1"
          (selected-transcript-text messages
                                    layout
                                    40
                                    {}
                                    {}
                                    {:anchor (selection/point 0 1)
                                     :focus (selection/point 39 16)})))
      ;; A partial chunk landing entirely off-screen also survives.
      (expect (= "Line B1\nLine C0\nLine C1"
                 (selected-transcript-text messages
                                           layout
                                           40
                                           {}
                                           {}
                                           {:anchor (selection/point 0 7)
                                            :focus (selection/point 39 11)})))
      ;; Reversed drag (focus above anchor) normalizes to the same chunk.
      (expect
        (=
          "Line A0\nLine A1\nLine A2\nLine B0\nLine B1\nLine C0\nLine C1\nLine C2\nLine D0\nLine D1"
          (selected-transcript-text messages
                                    layout
                                    40
                                    {}
                                    {}
                                    {:anchor (selection/point 39 16)
                                     :focus (selection/point 0 1)})))))
  (it
    "copies a very long off-screen drag spanning dozens of scrolled-out bubbles"
    (let [n-bubbles
          60

          lines-per
          5

          messages
          (vec (for [b (range n-bubbles)]
                 {:role :assistant
                  :prewrapped-lines (vec (for [l (range lines-per)]
                                           (format "B%02d-L%d" b l)))}))

          height-per
          (+ lines-per 2)

          heights
          (vec (repeat n-bubbles height-per))

          total-h
          (* n-bubbles height-per)

          offsets
          (vec (reductions + 0 heights))

          ;; Viewport scrolled to the very bottom: only the LAST bubble is
          ;; visible; 59 earlier bubbles (400+ rows) are scrolled off-screen.
          layout
          {:total-h total-h
           :heights heights
           :offsets offsets
           :visible [{:idx (dec n-bubbles)
                      :top (nth offsets (dec n-bubbles))
                      :height height-per
                      :projected (nth messages (dec n-bubbles))}]}

          all-lines
          (fn [from to]
            (str/join "\n"
                      (for [b
                            (range from to)

                            l
                            (range lines-per)]

                        (format "B%02d-L%d" b l))))]

      ;; Full-document drag from top to bottom copies all 300 content lines.
      (expect (= (all-lines 0 n-bubbles)
                 (selected-transcript-text messages
                                           layout
                                           40
                                           {}
                                           {}
                                           {:anchor (selection/point 0 1)
                                            :focus (selection/point 39 (- total-h 2))})))
      ;; Reversed long drag normalizes to the same full document.
      (expect (= (all-lines 0 n-bubbles)
                 (selected-transcript-text messages
                                           layout
                                           40
                                           {}
                                           {}
                                           {:anchor (selection/point 39 (- total-h 2))
                                            :focus (selection/point 0 1)})))
      ;; A long partial chunk entirely off-screen (bubbles 10..50) survives.
      (expect (= (all-lines 10 51)
                 (selected-transcript-text messages
                                           layout
                                           40
                                           {}
                                           {}
                                           {:anchor (selection/point 0 (+ (nth offsets 10) 1))
                                            :focus (selection/point 39 (+ (nth offsets 50) 5))})))))
  (it "copies scaled trace bubbles faithfully while scrolled through the REAL virtual layout"
      ;; The hand-crafted layouts above pin the copy math, but the height cache,
      ;; window slicing and projection all live in `virtual/layout`. This drives
      ;; the ACTUAL layout with 40 completed trace turns (user + assistant), each
      ;; carrying a DISTINCT identity (`:timestamp`) so message-content-fingerprint
      ;; keys never collide — the production invariant. Then it copies while the
      ;; viewport sits mid-document and asserts perfect fidelity: every answer, in
      ;; order, no duplicates, nothing dropped across the scroll boundary.
      (let [cols
            60

            n
            40

            messages
            (vec (mapcat (fn [i]
                           [{:role :user
                             :text (format "Q%d question mark" i)
                             :timestamp (java.util.Date. (+ 1000000 (* i 2)))}
                            {:role :assistant
                             :text (format "ANSWER-%02d prose body for turn %d." i i)
                             :traces [{:thinking (format "thinking %02d" i)
                                       :forms [{:code (format "(+ %d 1)" i)
                                                :result (str (inc i))
                                                :success? true
                                                :silent? false}]}]
                             :iteration-count 1
                             :timestamp (java.util.Date. (+ 1000001 (* i 2)))}])
                         (range n)))

            _
            (virtual/invalidate-heights!)

            vh
            12

            total-h
            (long (:total-h (virtual/layout messages cols {} nil vh {})))

            ;; Viewport parked in the middle of the document: dozens of bubbles
            ;; scrolled off both above and below the visible window.
            layout
            (virtual/layout messages cols {} (quot total-h 2) vh {})

            offsets
            (:offsets layout)

            answer-order
            (fn [text]
              (re-seq #"ANSWER-\d\d" text))

            ;; Whole-document drag from the first row to the last.
            full
            (selected-transcript-text messages
                                      layout
                                      cols
                                      {}
                                      {}
                                      {:anchor (selection/point 0 0)
                                       :focus (selection/point (dec cols) (dec total-h))})

            full-answers
            (answer-order full)

            ;; Off-screen partial chunk: assistant turns 10..25, sliced by their
            ;; real document offsets (assistant of turn k is messages index 2k+1).
            from-k
            10

            to-k
            25

            chunk
            (selected-transcript-text
              messages
              layout
              cols
              {}
              {}
              {:anchor (selection/point 0 (nth offsets (inc (* 2 from-k))))
               :focus (selection/point (dec cols) (dec (nth offsets (+ 2 (* 2 to-k)))))})]

        ;; Every answer present, in order, none duplicated.
        (expect (= (mapv #(format "ANSWER-%02d" %) (range n)) full-answers)
                "full-document scroll-copy keeps every answer in order")
        (expect (= (count full-answers) (count (distinct full-answers)))
                "no bubble is copied twice")
        (expect (= n (count (distinct (re-seq #"Q\d+ question mark" full))))
                "every user question is copied too")
        ;; Partial chunk copies exactly its span, nothing more, nothing less.
        (expect (= (mapv #(format "ANSWER-%02d" %) (range from-k (inc to-k))) (answer-order chunk))
                "a scrolled partial chunk copies exactly its spanned answers")))
  (it "copies visible live text for pending assistant drag selection"
      (let [message
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
  (it "copies the on-screen live view of a streaming bubble, not its off-screen transcript"
      ;; While a turn streams, `layout` renders the compact live PROGRESS view
      ;; (spinner + current activity). It is far shorter than the message's full
      ;; transcript projection and it is what SIZES the bubble's document rows.
      ;; Copy must read that on-screen paint, NOT re-project the full body — else
      ;; the off-screen transcript head overflows the compressed row span and the
      ;; copied text is misaligned garbage that never matches what the user sees.
      (let [live-msg
            {:role :assistant
             :text "Prose answer OFFSCREENANSWERMARK trailing."
             :traces (vec (repeat 6
                                  {:thinking "thinking line"
                                   :forms
                                   [{:code "(+ 1 2)" :result "3" :success? true :silent? false}
                                    {:code "(* 2 3)" :result "6" :success? true :silent? false}
                                    {:code "(dec 9)" :result "8" :success? true :silent? false}]}))
             :iteration-count 6
             :timestamp #inst "2026-04-30T00:00:00"}

            messages
            [{:role :user :text "the question"} live-msg]

            cols
            60

            ;; bottom-locked (scroll nil) + loading? true -> the live bubble is the
            ;; short progress view; the tall transcript stays off-screen/unrendered.
            layout
            (virtual/layout messages cols {} nil 12 {:loading? true})

            sel
            {:anchor (selection/point 0 0)
             :focus (selection/point (dec cols) (dec (long (:total-h layout))))}

            copied
            (selected-transcript-text messages layout cols {} {} sel)]

        ;; The user bubble above copies verbatim.
        (expect (str/includes? copied "the question"))
        ;; The live bubble copies exactly the rendered on-screen progress view.
        (expect (str/includes? copied "Vis is calling the provider")
                "streaming bubble copies its on-screen live view")
        ;; The full transcript body is NOT on screen while streaming, so a token
        ;; that lives only in the full projection must never leak into the copy.
        (expect (not (str/includes? copied "OFFSCREENANSWERMARK"))
                "off-screen transcript must not overflow into the copy")))
  (it "keeps a fully-visible pending bubble on its live paint (no head injection)"
      ;; `:top` >= 0 means nothing scrolled off; the canonical projection may
      ;; still be placeholder IR, so the live paint must be used verbatim.
      (let [message
            {:role :assistant :pending? true :text "Sending request to provider..."}

            layout
            {:total-h 4
             :heights [4]
             :offsets [0 4]
             :visible [{:idx 0
                        :top 0
                        :height 4
                        :projected {:role :assistant :prewrapped-lines ["live visible text"]}}]}

            sel
            {:anchor (selection/point 0 1) :focus (selection/point 39 3)}]

        (expect (= "live visible text" (selected-transcript-text [message] layout 40 {} {} sel)))))
  (it "copies a fully-visible multi-line streaming pending bubble with correct structure"
      ;; The live paint of a still-streaming answer holds several wrapped lines.
      ;; Dragging over the whole bubble must copy EVERY live line, in order,
      ;; newline-joined — not just the first/last or the placeholder IR text.
      (let [message
            {:role :assistant
             :pending? true
             :text "placeholder IR"
             :prewrapped-lines ["live line one" "live line two" "live line three" "live line four"]}

            layout
            {:total-h 6
             :heights [6]
             :offsets [0 6]
             :visible [{:idx 0
                        :top 0
                        :height 6
                        :projected {:role :assistant
                                    :prewrapped-lines ["live line one" "live line two"
                                                       "live line three" "live line four"]}}]}

            ;; whole-bubble drag spanning header/pad down past the last line
            sel-all
            {:anchor (selection/point 0 0) :focus (selection/point 39 5)}

            ;; partial mid drag over only the middle two live lines
            sel-mid
            {:anchor (selection/point 0 2) :focus (selection/point 39 3)}]

        (expect (= "live line one\nlive line two\nlive line three\nlive line four"
                   (selected-transcript-text [message] layout 40 {} {} sel-all))
                "whole streaming bubble copies all live lines in order")
        (expect (= "live line two\nlive line three"
                   (selected-transcript-text [message] layout 40 {} {} sel-mid))
                "partial drag copies exactly the spanned live lines")))
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
      (let [copied
            (promise)

            notified
            (promise)]

        (with-redefs-fn {#'input/clipboard-copy! (fn [text]
                                                   (deliver copied text)
                                                   true)
                         #'vis/notify! (fn [text & kvs]
                                         (deliver notified [text kvs]))}
          (fn []
            (copy-session-id! "123e4567-e89b-12d3-a456-426614174000")
            ;; The MARKED id lands on the clipboard: a bare UUID pasted
            ;; anywhere else says nothing about what it addresses.
            (expect (= "vis_session_id#123e4567-e89b-12d3-a456-426614174000"
                       (deref copied 1000 ::timeout)))
            (expect (= ["✓ Copied session ID" [:level :success :ttl-ms 1500]]
                       (deref notified 1000 ::timeout)))))))
  (it "mouse selection copy uses the shared success notification contract"
      (let [copied
            (promise)

            notified
            (promise)]

        (with-redefs-fn {#'input/clipboard-copy! (fn [text]
                                                   (deliver copied text)
                                                   true)
                         #'vis/notify! (fn [text & kvs]
                                         (deliver notified [text kvs]))}
          (fn []
            (copy-selection! "selected text")
            (expect (= "selected text" (deref copied 1000 ::timeout)))
            (expect (= ["✓ Copied selection" [:level :success :ttl-ms 1500]]
                       (deref notified 1000 ::timeout)))))))
  (it "a transcript drag whose document rebuild comes back blank falls back to the painted cells"
      ;; Regression: an expanded / live disclosure bubble can desync the
      ;; virtual-document rebuild from the on-screen paint, so it returns blank
      ;; while the user sees a highlight. Without the fallback the release cond
      ;; silently no-ops — no copy, no notification. The visible cells must win.
      (expect (= "visible highlighted body"
                 (selection-copy-payload :transcript "" (constantly "visible highlighted body"))))
      (expect
        (= "visible highlighted body"
           (selection-copy-payload :transcript "   \n  " (constantly "visible highlighted body"))))
      ;; A non-blank rebuild is authoritative (survives auto-scroll off-screen rows).
      (expect (= "document rebuild"
                 (selection-copy-payload :transcript "document rebuild" (constantly "visible"))))
      ;; Non-transcript sources already extract from cells; pass the value through.
      (expect (= "" (selection-copy-payload :input "" (constantly "visible")))))
  (it "single-click bubble copy uses the shared success notification contract"
      (let [copied
            (promise)

            notified
            (promise)]

        (with-redefs-fn {#'input/clipboard-copy! (fn [text]
                                                   (deliver copied text)
                                                   true)
                         #'vis/notify! (fn [text & kvs]
                                         (deliver notified [text kvs]))}
          (fn []
            (copy-bubble! "whole bubble")
            (expect (= "whole bubble" (deref copied 1000 ::timeout)))
            (expect (= ["✓ Copied bubble" [:level :success :ttl-ms 1500]]
                       (deref notified 1000 ::timeout)))))))
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
      (let [copied
            (promise)

            notified
            (promise)]

        (with-redefs-fn {#'input/clipboard-copy! (fn [text]
                                                   (deliver copied text)
                                                   true)
                         #'vis/notify! (fn [text & kvs]
                                         (deliver notified [text kvs]))}
          (fn []
            (copy-selection! "typed mistake" :input)
            (expect (= "typed mistake" (deref copied 1000 ::timeout)))
            (expect (= ["✓ Copied input selection" [:level :success :ttl-ms 1500]]
                       (deref notified 1000 ::timeout)))))))
  (it "file click targets open through the editor path, not the generic URL opener"
      (let [editor-opened
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
  ;; A `vis-doc` card is a HANDLE: a PDF or an HTML page has nothing a terminal
  ;; can paint, so the click must reach the OS viewer. The file lives in the
  ;; display cache OUTSIDE the workspace, which the cwd-confined `open!` refuses
  ;; — only `open-local!` can hand it to Preview / the browser.
  (it "document click targets open the host PDF/HTML in the system viewer"
      (let [local-opened
            (promise)

            url-opened
            (promise)]

        (with-redefs-fn {#'opener/open-local! (fn [target]
                                                (deliver local-opened target)
                                                {:status :ok})
                         #'opener/open! (fn [target]
                                          (deliver url-opened target)
                                          {:status :ok})}
          (fn []
            (open-click-target! {:kind :doc :url "/var/tmp/vis-display/doc-9f2.pdf"})
            (expect (= "/var/tmp/vis-display/doc-9f2.pdf" (deref local-opened 1000 ::timeout)))
            (expect (= ::timeout (deref url-opened 100 ::timeout)))))))
  (it "reports clipboard failure instead of claiming the selection was copied"
      (let [notified (promise)]
        (with-redefs-fn {#'input/clipboard-copy! (constantly false)
                         #'vis/notify! (fn [text & kvs]
                                         (deliver notified [text kvs]))}
          (fn []
            (copy-selection! "selected text")
            (expect (= ["Copy failed — terminal clipboard unavailable" [:level :error :ttl-ms 5000]]
                       (deref notified 1000 ::timeout)))))))
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
                 (let [bytes
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
                         (expect (= "\rResume with:\nvis-agent channels tui --session-id abc123\n"
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
                 ;; `vis-agent channels tui --sessions-id <uuid>` used to succeed
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
      (let [cols
            80

            base
            {:input {:lines ["hello"]} :scroll nil :messages [] :loading? false}

            typed
            (assoc base :input {:lines ["hello world"]})]

        (expect (true? (boolean (input-only-change? base typed cols))))))
  (it "falls through around inline suggestion triggers so stale picker rows clear"
      (let [cols
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
      (let [cols
            80

            base
            {:input {:lines ["hi"]} :scroll nil :messages [] :loading? false}

            wrapped
            (assoc base :input {:lines [(apply str (repeat 400 "x"))]})]

        (expect (false? (boolean (input-only-change? base wrapped cols))))))
  (it "falls through when any non-input key differs"
      (let [cols
            80

            base
            {:input {:lines ["a"]} :scroll nil :messages [] :loading? false}

            edited
            (assoc base
              :input {:lines ["ab"]}
              :messages [{:role :user}])]

        (expect (false? (boolean (input-only-change? base edited cols))))))
  (it "falls through while loading (the live bubble grows)"
      (let [cols
            80

            base
            {:input {:lines ["a"]} :scroll nil :messages [] :loading? false}

            edited
            (assoc base
              :input {:lines ["ab"]}
              :loading? true)]

        (expect (false? (boolean (input-only-change? base edited cols))))))
  (it "falls through while a mouse selection / overlay / find bar is active"
      (let [cols
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
      (let [cols
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
      (let [flags (frame-change-flags {:last-db {}
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

(defdescribe tab-order-persistence-test
             (it "persists through one adopt-and-reorder call without listing or assigning tabs"
                 (let [pid
                       (random-uuid)

                       ids
                       [(random-uuid) (random-uuid)]

                       calls
                       (atom [])]

                   (with-redefs [vis/gateway-list-sessions
                                 (fn [& _]
                                   (swap! calls conj :list))

                                 vis/gateway-assign-project!
                                 (fn [& _]
                                   (swap! calls conj :assign))

                                 vis/gateway-reorder-project-sessions!
                                 (fn [actual-pid actual-ids]
                                   (swap! calls conj [:reorder actual-pid actual-ids]))]

                     ((deref #'screen/persist-tabs-order!) pid ids))
                   (expect (= [[:reorder pid ids]] @calls)))))

(defdescribe fitting-image-placements-test
             ;; A picture whose reserved box runs past the BOTTOM of the transcript
             ;; band must shrink whole (aspect-preserving) into the rows that are
             ;; left — never source-crop to a decapitated top slice. Source-cropping
             ;; is only right when the user has scrolled INTO the picture.
             (let [fit
                   (deref #'screen/fitting-image-placements)

                   img
                   {:path "/tmp/shot.png" :cols 40 :rows 40}

                   band
                   (fn [n first-idx row0]
                     (mapv
                       (fn [i]
                         {:row (+ (long row0) i) :col 2 :img img :img-idx (+ (long first-idx) i)})
                       (range n)))]

               (it "kitty: a bottom-overflowing box shrinks to the visible rows"
                   (with-redefs [timg/images-protocol (constantly :kitty)]
                     (let [got (:img (first (fit (band 10 0 5) 0 15)))]
                       (expect (= 10 (long (:rows got))))
                       (expect (= 10 (long (:cols got))))
                       (expect (nil? (:crop-bottom got))))))
               (it "kitty: a box scrolled INTO still source-crops at native scale"
                   (with-redefs [timg/images-protocol (constantly :kitty)]
                     (let [got (:img (first (fit (band 20 20 0) 0 30)))]
                       (expect (= 40 (long (:rows got))))
                       (expect (= 20 (long (:crop-top got)))))))
               (it "iterm2 normalizes every partial image into the visible rows"
                   (with-redefs [timg/images-protocol (constantly :iterm2)]
                     (let [bottom (:img (first (fit (band 10 0 5) 0 15)))
                           scrolled (:img (first (fit (band 20 20 0) 0 30)))]

                       (expect (= 10 (long (:rows bottom))))
                       (expect (= 10 (long (:cols bottom))))
                       (expect (= 20 (long (:rows scrolled))))
                       (expect (= 20 (long (:cols scrolled)))))))))

;; Regression, reported bug: dropping a picture into a turn and letting the view
;; auto-scroll to the bottom tore the drawing off its frame — it kept the screen
;; row it already had, so it slid downward out of its box and over the chrome
;; below. The 80ms live tick repaints the WHOLE messages band whenever
;; auto-bottom follow shifts `eff-scroll`, but it never re-placed the terminal
;; graphics, which no cell repaint can move: every picture stayed pinned to the
;; row the last FULL frame gave it while its reserved box scrolled out from
;; under it.
(defdescribe
  live-frame-image-placement-test
  (let [fence
        (str "\n````vis-image\n" "[Image #1: shot.png]\n"
             "/tmp/shot.png\n" "image/png\n"
             "800x100\n" "12 kB\n````\n")

        db
        {:messages
         [{:role :user :text (str "look at this" fence) :timestamp (java.util.Date. 2000000)}
          {:role :assistant :text "streaming" :timestamp (java.util.Date. 2000001)}]
         :input {:lines [""] :crow 0 :ccol 0}
         :progress {:iterations [{:thinking "t"}]}
         :loading? true
         :settings {}
         :session {:id "s1"}}

        ;; One live tick against a real virtual terminal: what the tick
        ;; handed the graphics painter, and where the caption the picture
        ;; hangs under actually landed on screen.
        live-tick!
        (fn [previous-layout]
          (let [{terminal :terminal ^TerminalScreen scr :screen}
                (term/virtual-screen)

                painted
                (atom :never-called)]

            (virtual/invalidate-heights!)
            (with-redefs [timg/graphical-terminal?
                          (constantly true)

                          timg/images-protocol
                          (constantly :kitty)

                          screen/paint-terminal-images!
                          (fn [regions]
                            (reset! painted (vec regions)))]

              (let [layout
                    ((deref #'screen/render-live-bubble-frame!) scr 80 30 db 1000 previous-layout)]
                (.refresh scr)
                {:painted @painted
                 :layout layout
                 :caption-row (first (keep-indexed (fn [i line]
                                                     (when (str/includes? line "Image #1") i))
                                                   (term/grid terminal)))}))))]

    (it "a follow-mode tick re-places the picture on the rows it just painted"
        (let [{:keys [painted caption-row]}
              (live-tick! {:eff-scroll 99 :cols 80 :rows 30})

              placement
              (first painted)]

          (expect (some? caption-row))
          (expect (= 1 (count painted)))
          ;; The reserved box opens on the row under the `[Image #1: …]`
          ;; caption this very tick painted — not where the last full
          ;; frame left it.
          (expect (= (inc (long caption-row)) (long (:row placement))))
          (expect (= 5 (long (:rows (:img placement)))))
          (expect (= "/tmp/shot.png" (:path (:img placement))))))
    (it "a tick that did not shift the transcript leaves the graphics layer alone"
        ;; The cheap live-band path repaints no image row, so an empty
        ;; placement set there would delete every picture on screen.
        (let [settled
              (:layout (live-tick! {:eff-scroll 99 :cols 80 :rows 30}))

              {:keys [painted]}
              (live-tick! settled)]

          (expect (= :never-called painted))))))

;; Regression: pressing C-x made every inline image in the transcript disappear.
;; The C-x hydra went through the MODAL `with-dialog-lock`, which deletes the
;; whole Kitty graphics layer so a full-screen dialog is the top surface; a band
;; only ever covers the rows it paints.
(defdescribe images-above-band-test
             (let [above
                   (deref #'screen/images-above-band)

                   region
                   (fn [row rows]
                     {:row row :col 2 :img {:path "/tmp/shot.png" :cols 10 :rows rows}})]

               (it "keeps every picture whose box ends above the band's first row"
                   (expect (= [(region 2 5)] (above [(region 2 5) (region 9 5)] 12))))
               (it "drops a picture the band would sit on top of"
                   (expect (= [] (above [(region 10 4)] 12)))
                   (expect (= [] (above [(region 12 1)] 12))))
               (it "the C-x band's top row leaves room for the transcript above it"
                   (let [db
                         {:layout {:cols 100 :rows 30 :messages-top 1 :input-h 3}}

                         top
                         ((deref #'screen/band-top-row) db (keymap/prefix-spec db))]

                     ;; The hydra is four PANES, not every row stacked: it must not
                     ;; measure tall enough to reach the header and swallow every
                     ;; picture on screen.
                     (expect (> (long top) 10))
                     (expect (seq (above [(region 3 4)] top)))))))

(defdescribe
  provider-limits-active-provider-test
  "The background limits poller must resolve the SAME provider the footer
   renders. When it read the gateway's stored session model first, a session
   whose local pref had just been cycled to Codex got a report stamped
   `:anthropic-coding-plan`; `footer/report-for-current-provider` drops a
   foreign-provider report, so the usage row sat on \"limits: loading…\"
   forever while the poller kept refreshing the wrong plan."
  (it "prefers the local per-session model pref over the gateway session model"
      (let [active-provider-id
            (deref #'screen/active-provider-id)

            old-db
            @state/app-db]

        (try (with-redefs [vis/gateway-session-model (fn [_]
                                                       {:provider "anthropic-coding-plan"
                                                        :model "claude-opus-5"})]
               (reset! state/app-db {:session {:id "s1"}
                                     :session-model-pref {:provider "openai-codex"
                                                          :model "gpt-5.6-terra"}})
               (expect (= :openai-codex (active-provider-id)))
               (swap! state/app-db dissoc :session-model-pref)
               (expect (= :anthropic-coding-plan (active-provider-id))))
             (finally (reset! state/app-db old-db))))))

(defn- painted-bubble-grid
  "Paint ONE chat bubble into a virtual terminal at `start-row` and return every
   screen row as text. The click geometry under test must agree with what this
   real paint puts on screen, not with a second copy of the layout arithmetic."
  [message ^long start-row]
  (let [vt
        (term/virtual-screen)

        ^TerminalScreen scr
        (:screen vt)]

    (render/draw-chat-bubble! (.newTextGraphics scr)
                              message
                              start-row
                              render/MESSAGE_MARGIN_LEFT
                              (- 80 (long render/MESSAGE_SIDE_PAD))
                              {:viewport-top 0 :viewport-h 0})
    (.refresh scr)
    (term/grid (:terminal vt))))

;; Regression, reported bug: with a live turn on screen, clicking an UNCOLLAPSED
;; disclosure to copy just that block copied the WHOLE assistant bubble instead.
;; `disclosure-copy-regions` mapped content line `i` to `text-top + top + i`,
;; but `render/draw-chat-bubble!` paints it one row lower - under the
;; role/timestamp row. Every per-block target therefore sat one row too high:
;; the summary row was claimed by the block below it and the block's LAST body
;; row was claimed by nobody, so a click there fell through to the whole-bubble
;; copy region. Short streaming blocks are mostly "last row", which is why a
;; live turn made it constant.
(defdescribe
  disclosure-copy-region-geometry-test
  (it
    "expanded-disclosure copy targets sit on the rows the painter draws"
    (let [text-top
          3

          top
          2

          node-id
          "thinking:i1:reasoning"

          block
          {:kind :copy-block-body :node-id node-id :text "why one\nwhy two"}

          message
          {:role :assistant
           :prewrapped-lines ["▾ REASONING" "why one" "why two"]
           :line-meta [nil block block]}

          grid
          (painted-bubble-grid message (+ text-top top))

          row-of
          (fn [needle]
            (first (keep-indexed (fn [i line]
                                   (when (str/includes? line needle) i))
                                 grid)))

          summary-row
          (row-of "REASONING")

          first-body-row
          (row-of "why one")

          last-body-row
          (row-of "why two")

          regions
          (disclosure-copy-regions {:visible [{:idx 0 :top top :height 6 :projected message}]}
                                   text-top
                                   12
                                   80)

          hit-at
          (fn [row]
            (:node-id (bubble-copy-hit {:row row :col (inc (long render/MESSAGE_MARGIN_LEFT))}
                                       regions)))]

      ;; The painter really does reserve chrome above the content, so the
      ;; naive `text-top + top + i` row is the wrong one.
      (expect (= [(+ text-top top 1) (+ text-top top 2) (+ text-top top 3)]
                 [summary-row first-body-row last-body-row]))
      ;; One copy target per PAINTED body row - no more, no less.
      (expect (= [first-body-row last-body-row] (mapv :row regions)))
      ;; Both body rows copy their own block, including the last one, which
      ;; used to miss and copy the entire bubble.
      (expect (= [node-id node-id] [(hit-at first-body-row) (hit-at last-body-row)]))
      ;; And the fix did not slide the targets down onto the summary row.
      (expect (nil? (hit-at summary-row))))))

;; Regression, Vis session 15315f1b-585f-4e78-97e9-71401915d092: a short
;; THINKING band had no per-block copy target. Its one visible reasoning row was
;; therefore almost always a whole-bubble hit, copying every iteration and the
;; final answer instead of the reasoning block the user clicked.
(defdescribe
  short-thinking-copy-region-test
  (it
    "gives a short thinking row precedence over whole-assistant-bubble copy"
    (let [text-top
          3

          top
          2

          first-thinking
          "Planning UX review and code inspection"

          second-thinking
          "Searching TUI and settings commands"

          raw-message
          {:id "turn-1"
           :role :assistant
           :text "Final answer"
           :traces [{:thinking first-thinking} {:thinking second-thinking}]}

          opts
          {:session-id "sid" :session-turn-id "turn-1" :detail-expansions {}}

          payload
          (render/format-answer-with-thinking-data (:text raw-message)
                                                   (:traces raw-message)
                                                   (- 80 (long render/MESSAGE_SIDE_PAD))
                                                   {:show-thinking true :show-iterations true}
                                                   nil
                                                   false
                                                   opts)

          projected
          (assoc raw-message
            :prewrapped-lines (:lines payload)
            :line-meta (:line-meta payload))

          layout
          {:visible [{:idx 0 :top top :height 50 :projected projected}]}

          grid
          (painted-bubble-grid projected (+ text-top top))

          thinking-row
          (first (keep-indexed (fn [i line]
                                 (when (str/includes? line first-thinking) i))
                               grid))

          point
          {:row thinking-row :col (inc (long render/MESSAGE_MARGIN_LEFT))}

          disclosure-hit
          (bubble-copy-hit point (disclosure-copy-regions layout text-top 80 80))

          whole-bubble-hit
          (bubble-copy-hit point
                           (bubble-copy-regions layout
                                                [raw-message]
                                                text-top
                                                80
                                                80
                                                {:show-thinking true :show-iterations true}
                                                opts))]

      (expect (= first-thinking (:text disclosure-hit)))
      ;; The broad fallback really is the complete turn pasted in the report; the
      ;; per-thinking hit above is what must win before it.
      (expect (str/includes? (force (:text whole-bubble-hit)) second-thinking)))))

;; A `/draft …` line asks exactly what the draft band asks, so the BAND answers
;; it — inside the session's own frame, with the command the slash named already
;; pressed. Typing `/draft new` and pressing `d` on the band are one path now;
;; the modal text-input window that used to read the label is gone.

(defdescribe draft-slash-band-test
             (it "a `/draft …` line opens the band, on the command it already named"
                 (let [band
                       (var-get #'screen/draft-slash-for-input)

                       typed
                       #(band (reduce input/insert-char (input/empty-input) (seq %)))]

                   (expect (= {:pressed nil} (typed "/draft")))
                   (expect (= {:pressed :new-dirty} (typed "/draft new")))
                   ;; Completing the slash from the overlay leaves a trailing space.
                   (expect (= {:pressed :new-dirty} (typed "/draft new ")))
                   (expect (= {:pressed :new-clean} (typed "/draft clean")))
                   (expect (= {:pressed :switch} (typed "/draft resume")))
                   (expect (= {:pressed :switch} (typed "/draft list")))
                   (expect (= {:pressed :abandon} (typed "/draft abandon")))
                   ;; A line that carries its own answer, and the two verbs with nothing
                   ;; to ask, run as the engine slashes they are — the band would have no
                   ;; question left to put.
                   (expect (nil? (typed "/draft new feature-x")))
                   (expect (nil? (typed "/draft apply")))
                   (expect (nil? (typed "/draft stash")))
                   (expect (nil? (typed "/export")))
                   (expect (nil? (typed "hello"))))))

;; Regression (user report, from the TUI): ArrowUp recalled `/reload` out of the
;; input history and the slash overlay opened over the composer — and while it
;; is up the overlay OWNS ArrowUp/Down, so the next press moved ITS selection
;; instead of walking further back through the ring.
(defdescribe
  recalled-line-paints-no-slash-overlay-test
  (it "a buffer the user TYPED offers the overlay; a RECALLED one does not"
      (let [buffer
            #(reduce input/insert-char (input/empty-input) (seq %))

            ;; What ArrowUp dropped in the box — `/reload` in the report, and any
            ;; slash line the palette can answer here (a unit-test JVM carries no
            ;; engine slash registry, so match a built-in: `Search in Session`).
            recalled
            (buffer "/s")]

        (expect (seq (slash-suggestions-for-db nil {:input recalled :slash-command-index 0})))
        ;; THIS is what used to spring open over the composer and, because an open
        ;; overlay owns ArrowUp, strand the user on the entry they just recalled.
        (expect (nil? (slash-suggestions-for-db
                        nil
                        {:input recalled :slash-command-index 0 :slash-command-hidden? true}))))))
