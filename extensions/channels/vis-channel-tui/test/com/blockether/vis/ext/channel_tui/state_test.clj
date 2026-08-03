(ns com.blockether.vis.ext.channel-tui.state-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.scroll :as scroll]
            [com.blockether.vis.ext.channel-tui.state :as state]
            [com.blockether.vis.ext.channel-tui.virtual :as virtual]
            [lazytest.core :refer [defdescribe expect it]]))

;; The `"openai_codex_verbosity"` enum toggle is registered by the OpenAI Codex
;; PROVIDER extension in production (it lives next to the backend it tunes), a
;; module this channel-tui test suite does not load. Register it here so the
;; `:settings` projection and the verbosity-cycle events resolve against a real
;; `:enum` toggle, mirroring production.
(vis/register-toggle! {:id "openai_codex_verbosity"
                       :label "Verbosity"
                       :description "Output detail hint passed to the OpenAI Codex backend."
                       :type :enum
                       :choices ["low" "medium" "high"]
                       :default "low"
                       :owner :vis
                       :group :provider
                       :persist? true
                       :settings? false
                       :visible-fn (fn []
                                     (boolean (vis/has-provider? :openai-codex)))})

(defn- await-enqueue!
  "Gateway queue effects post on the FIFO gateway-queue thread and hand back that
   Future. Tests await the round-trip the TUI deliberately does not."
  [^java.util.concurrent.Future fut]
  (when fut (.get fut 10 java.util.concurrent.TimeUnit/SECONDS)))

(defn- flush-queue-io!
  "Block until the queue thread has drained everything posted so far: one no-op
   queued BEHIND the work under test, which suffices because it is one FIFO thread."
  []
  (await-enqueue! (#'state/gateway-queue-io!
                   (fn []
                     :flushed))))

(defdescribe turn-extra-body-test
             (it "omits OpenAI verbosity when the session selects Claude"
                 (with-redefs
                   [vis/get-router
                    (constantly :router)

                    vis/resolve-effective-model
                    (fn [_]
                      {:provider :openai-codex :name "gpt-5.6-sol"})]

                   (expect (nil? (#'state/turn-extra-body
                                  {:session {:id "s1"}
                                   :session-model-pref {:provider "anthropic-coding-plan"
                                                        :model "claude-sonnet-4-6"}
                                   :settings {:openai-codex-verbosity "high"}})))))
             (it "includes verbosity when the session selects OpenAI Codex"
                 (with-redefs
                   [vis/get-router
                    (constantly :router)

                    vis/resolve-effective-model
                    (fn [_]
                      {:provider :anthropic-coding-plan :name "claude-sonnet-4-6"})]

                   (expect (= {:text {:verbosity "high"}}
                              (#'state/turn-extra-body
                               {:session {:id "s1"}
                                :session-model-pref {:provider "openai-codex" :model "gpt-5.6-sol"}
                                :settings {:openai-codex-verbosity "high"}}))))))

(defdescribe always-on-display-test
             (it "thinking, full trace, silent calls, and timestamps are ALWAYS shown"
                 ;; Their toggles were retired (the trace IS the transcript — nothing to
                 ;; hide, same call as show-raw-code). The settings projection hardcodes
                 ;; them on, and the toggles no longer exist in the registry.
                 (let [s (#'state/migrated-toggle-projection)]
                   (expect (true? (:show-thinking s)))
                   (expect (true? (:show-iterations s)))
                   (expect (true? (:show-silent s)))
                   (expect (true? (:show-timestamps s))))
                 (expect (nil? (vis/toggle-spec "show_thinking")))
                 (expect (nil? (vis/toggle-spec "show_timestamps")))))

(defdescribe
  detail-toggle-test
  (it "does not cold-clear render and height caches on disclosure click"
      (let
        [render-invalidations
         (atom 0)

         height-invalidations
         (atom 0)]

        (with-redefs
          [render/invalidate-cache!
           (fn []
             (swap! render-invalidations inc))

           virtual/invalidate-heights!
           (fn []
             (swap! height-invalidations inc))]

          (reset! state/app-db {:detail-expansions {} :render-version 0})
          (state/dispatch [:toggle-detail "cid" "answer:t11111111:details:d1"])
          (expect (= {["cid" "answer:t11111111:details:d1"] true}
                     (:detail-expansions @state/app-db)))
          (expect (zero? @render-invalidations))
          (expect (zero? @height-invalidations)))))
  (it "explicitly stores collapsed for a default-expanded row (BLOCK / op-row click)"
      ;; Regression: BLOCK header + op rows default to EXPANDED. The old
      ;; absent/true-only toggle could never store \"collapsed\", so clicking to
      ;; collapse a default-expanded row was a no-op. The 3-arg explicit form
      ;; (driven by the click region's current :collapsed?) fixes it.
      (reset! state/app-db {:detail-expansions {} :render-version 0})
      ;; Currently expanded (region :collapsed? false) → click collapses it.
      (state/dispatch [:toggle-detail "cid" "t1/i1:block" false])
      (expect (= {["cid" "t1/i1:block"] false} (:detail-expansions @state/app-db)))
      (expect
        (false?
          (#'render/detail-expanded? (:detail-expansions @state/app-db) "cid" "t1/i1:block" true)))
      ;; Click again (region :collapsed? true) → expands it.
      (state/dispatch [:toggle-detail "cid" "t1/i1:block" true])
      (expect (= {["cid" "t1/i1:block"] true} (:detail-expansions @state/app-db)))
      (expect
        (true?
          (#'render/detail-expanded? (:detail-expansions @state/app-db) "cid" "t1/i1:block" true))))
  (it "stores preview switcher mode on the same detail-expansions bus"
      (reset! state/app-db {:detail-expansions {} :render-version 0})
      (state/dispatch [:select-preview-mode "cid" "iteration:t11111111:i1:b1:preview-switch" :raw])
      (expect (= {["cid" "iteration:t11111111:i1:b1:preview-switch"] :raw}
                 (:detail-expansions @state/app-db))))
  (it
    "applies external input to an inactive recording-origin workspace"
    (let
      [external-input-fn
       (-> #'state/event-registry
           deref
           deref
           (get :external-input)
           :fn)

       input-state
       (fn [text]
         {:lines [text] :crow 0 :ccol (count text)})

       db
       {:active-tab-id :second
        :tabs [{:id :first :label "First"} {:id :second :label "Second" :active? true}]
        :input (input-state "second draft")
        :input-history-index :second-index
        :input-history-draft "second-draft"
        :slash-command-index 7
        :slash-command-hidden? true
        :tab-locals {:first {:input (input-state "first draft")
                             :input-history-index :first-index
                             :input-history-draft "first-draft"
                             :slash-command-index 3
                             :slash-command-hidden? true}}}

       next-db
       (external-input-fn db [:external-input :append "rewrite" :first])]

      (expect (= "second draft" (input/input->text (:input next-db))))
      (expect (= :second-index (:input-history-index next-db)))
      (expect (= "first draft\nrewrite"
                 (input/input->text (get-in next-db [:tab-locals :first :input]))))
      (expect (nil? (get-in next-db [:tab-locals :first :input-history-index])))
      (expect (nil? (get-in next-db [:tab-locals :first :input-history-draft])))
      (expect (= 0 (get-in next-db [:tab-locals :first :slash-command-index])))
      (expect (false? (get-in next-db [:tab-locals :first :slash-command-hidden?])))))
  (it "snaps an in-flight parked scroll to the painted row before expanding"
      ;; A wheel/PageDown ease can leave `:offset` far below the row currently
      ;; painted in `:pos`. Keeping that latent target makes the transcript
      ;; continue racing upward as soon as the disclosure changes height.
      (reset! state/app-db {:detail-expansions {}
                            :layout {:eff-scroll 40}
                            :scroll {:mode :at :offset 100 :pos 40}
                            :render-version 0})
      (state/dispatch [:toggle-detail "cid" "answer:t11111111:details:d1" true])
      (expect (= (scroll/parked 40) (:scroll @state/app-db)))))

(defdescribe
  resync-toggle-settings-test
  (it "busts BOTH render caches so a registry toggle (show-thinking) repaints without a restart"
      ;; Regression: flipping a registry-only toggle (e.g. `"show_thinking"`)
      ;; resolved live in the registry but the painter kept handing back
      ;; cached bubble lines (`render/fmt-cache`, keyed on message identity)
      ;; and stale row counts (the `virtual` height cache, whose
      ;; `settings-fingerprint` doesn't track registry-only toggles). The
      ;; new value only showed after a process restart cleared the caches.
      ;; The toggles-registry listener dispatches `:resync-toggle-settings`,
      ;; which must now drop both caches.
      (let
        [render-invalidations
         (atom 0)

         height-invalidations
         (atom 0)]

        (with-redefs
          [render/invalidate-cache!
           (fn []
             (swap! render-invalidations inc))

           virtual/invalidate-heights!
           (fn []
             (swap! height-invalidations inc))]

          (reset! state/app-db {:settings {} :render-version 0})
          (state/dispatch [:resync-toggle-settings])
          (expect (= 1 @render-invalidations))
          (expect (= 1 @height-invalidations))
          ;; The cached :settings projection is still rebuilt from the registry.
          (expect (contains? (:settings @state/app-db) :show-thinking))))))

(defdescribe external-input-test
             (it "append adds transcript text without replacing draft input"
                 (reset! state/app-db {:input (input/paste-text (input/empty-input) "typed")
                                       :input-history-index :stale
                                       :input-history-draft "old"
                                       :render-version 0})
                 (state/dispatch [:external-input :append "voice text"])
                 (expect (= "typed\nvoice text" (input/input->text (:input @state/app-db))))
                 (expect (nil? (:input-history-index @state/app-db)))
                 (expect (nil? (:input-history-draft @state/app-db)))))

(defdescribe channel-status-test
             (it "clears ttl-bound statuses only when the deadline still matches"
                 (reset! state/app-db {:channel-status {} :render-version 0})
                 (state/dispatch [:channel-status-set :voice/input
                                  {:text "○ Voice ready" :level :info :until 100}])
                 (state/dispatch [:channel-status-clear-if-until :voice/input 99])
                 (expect (= "○ Voice ready"
                            (get-in @state/app-db [:channel-status :voice/input :text])))
                 (state/dispatch [:channel-status-clear-if-until :voice/input 100])
                 (expect (nil? (get-in @state/app-db [:channel-status :voice/input])))))

(defdescribe slash-command-selection-test
             (it "clamps selected slash suggestion index for arrows and mouse wheel"
                 (reset! state/app-db {:slash-command-index 0 :render-version 0})
                 (state/dispatch [:move-slash-command-selection 1 3])
                 (expect (= 1 (:slash-command-index @state/app-db)))
                 (state/dispatch [:move-slash-command-selection -1 3])
                 (expect (= 0 (:slash-command-index @state/app-db)))
                 (state/dispatch [:move-slash-command-selection -1 3])
                 (expect (= 0 (:slash-command-index @state/app-db))))
             (it "can hide slash suggestions after tab completion until input is cleared"
                 (reset! state/app-db {:input (input/paste-text (input/empty-input) "/new-tab ")
                                       :slash-command-hidden? false
                                       :render-version 0})
                 (state/dispatch [:hide-slash-command-suggestions])
                 (expect (true? (:slash-command-hidden? @state/app-db)))
                 (state/dispatch [:update-input
                                  (input/paste-text (input/empty-input) "/new-tab arg")])
                 (expect (true? (:slash-command-hidden? @state/app-db)))
                 (state/dispatch [:reset-input])
                 (expect (false? (:slash-command-hidden? @state/app-db)))))

(defdescribe
  tab-entries-test
  (it "adds a workspace and seeds a base workspace when none exist"
      ;; Base workspace inherits the current `:title`; the freshly-added workspace
      ;; starts as `Untitled session` because it has no title yet.
      (reset! state/app-db {:title "Current" :render-version 0})
      (state/dispatch [:create-tab])
      (expect (= [{:id :main :label "Current"}
                  {:id :tab-1 :label state/untitled-session-label :active? true}]
                 (:tabs @state/app-db)))
      (expect (= :tab-1 (:active-tab-id @state/app-db)))
      (expect (= 1 (:render-version @state/app-db))))
  (it "adds the next unique workspace and makes it active"
      ;; New workspaces default to the untitled placeholder; `:set-title`
      ;; renames the active workspace once a title is generated.
      (reset! state/app-db {:tabs [{:id :main :label "Main"}
                                   {:id :tab-1 :label "Tab 1" :active? true}]
                            :active-tab-id :tab-1
                            :render-version 0})
      (state/dispatch [:create-tab])
      (expect (= [{:id :main :label "Main"} {:id :tab-1 :label "Tab 1"}
                  {:id :tab-2 :label state/untitled-session-label :active? true}]
                 (:tabs @state/app-db)))
      (expect (= :tab-2 (:active-tab-id @state/app-db))))
  (it "attaches workspace root to the new workspace and active snapshot"
      (let
        [workspace
         {:workspace/id "ws-1" :workspace/root "/tmp/vis-ws" :main {:branch "feature/ws"}}]
        (reset! state/app-db {:tabs [{:id :main :label "Main" :active? true}]
                              :active-tab-id :main
                              :tab-locals {}
                              :render-version 0})
        (state/dispatch [:create-tab {:workspace workspace}])
        (expect (= "/tmp/vis-ws" (get-in @state/app-db [:tabs 1 :workspace/root])))
        (expect (= workspace (:workspace @state/app-db)))
        (expect (= "/tmp/vis-ws" (:workspace/root @state/app-db)))
        (expect (= "feature/ws" (get-in @state/app-db [:tabs 1 :label])))))
  (it "keeps active root in sync when the backend workspace changes"
      (reset! state/app-db {:workspace {:id "ws-1" :root "/tmp/old"}
                            :workspace/root "/tmp/old"
                            :tabs [{:id :main :label "Main" :active? true}]
                            :active-tab-id :main
                            :tab-locals {}
                            :render-version 0})
      (state/dispatch [:set-workspace {"id" "ws-1" "root" "/tmp/new"}])
      (expect (= "/tmp/new" (:workspace/root @state/app-db)))
      (expect (= "/tmp/new" (get-in @state/app-db [:tab-locals :main :workspace/root]))))
  (it "caps workspaces at eight total entries"
      (reset! state/app-db {:title "Main" :render-version 0})
      (dotimes [_ 10]
        (state/dispatch [:create-tab]))
      (expect (= 8 (count (:tabs @state/app-db))))
      (expect (= [:main :tab-1 :tab-2 :tab-3 :tab-4 :tab-5 :tab-6 :tab-7]
                 (mapv :id (:tabs @state/app-db))))
      (expect (= :tab-7 (:active-tab-id @state/app-db))))
  (it "switches the full transcript, draft, prompt history, and session by workspace"
      (reset! state/app-db {:session {:id "main-c"}
                            :messages [{:role :user :text "main prompt"}]
                            :input (input/paste-text (input/empty-input) "main draft")
                            :input-history ["main prompt"]
                            :pastes {}
                            :paste-counter 0
                            :detail-expansions {}
                            :tabs [{:id :main :label "Main" :active? true}]
                            :active-tab-id :main
                            :tab-locals {}
                            :render-version 0})
      (state/dispatch [:create-tab])
      (state/dispatch [:init-session {:id "tab-c"} [{:role :user :text "tab prompt"}]])
      (state/dispatch [:update-input (input/paste-text (input/empty-input) "tab draft")])
      (state/dispatch [:select-tab-index 0])
      (expect (= {:id "main-c"} (:session @state/app-db)))
      (expect (= [{:role :user :text "main prompt"}] (:messages @state/app-db)))
      (expect (= "main draft" (input/input->text (:input @state/app-db))))
      (expect (= ["main prompt"] (:input-history @state/app-db)))
      (state/dispatch [:select-tab-index 1])
      (expect (= {:id "tab-c"} (:session @state/app-db)))
      (expect (= [{:role :user :text "tab prompt"}] (:messages @state/app-db)))
      (expect (= "tab draft" (input/input->text (:input @state/app-db))))
      (expect (= ["tab prompt"] (:input-history @state/app-db))))
  (it "restores a tab's cached layout but always enters at its latest event"
      (let
        [main-layout
         {:cols 120 :rows 40 :total-h 5000 :inner-h 30 :offsets [0 100 900]}

         tab-layout
         {:cols 120 :rows 40 :total-h 7000 :inner-h 30 :offsets [0 200 1200]}]

        (reset! state/app-db {:session {:id "main-c"}
                              :messages [{:role :user :text "main prompt"}]
                              :scroll {:mode :at :offset 40 :pos 900}
                              :layout main-layout
                              :tabs [{:id :main :label "Main" :active? true}
                                     {:id :tab-1 :label "Tab 1"}]
                              :active-tab-id :main
                              :tab-locals {:tab-1 {:session {:id "tab-c"}
                                                   :messages [{:role :user :text "tab prompt"}]
                                                   :scroll {:mode :at :offset 5 :pos 1200}
                                                   :layout tab-layout}}
                              :render-version 0})
        (state/dispatch [:select-tab-index 1])
        (expect (= tab-layout (:layout @state/app-db)))
        ;; A tab switch is a latest-events jump, not a restoration of where this
        ;; transcript was previously read. This applies equally to a live tab.
        (expect (= scroll/follow (:scroll @state/app-db)))
        (state/dispatch [:select-tab-index 0])
        (expect (= main-layout (:layout @state/app-db)))
        (expect (= scroll/follow (:scroll @state/app-db)))))
  (it "snaps a FOLLOWing tab to the live bottom instead of easing down to it"
      ;; The regression: a hidden FOLLOW tab keeps `:pos` pinned at the bottom of
      ;; the `total-h` it had when last painted, and that grows while it is hidden
      ;; (background turns append; estimate→real height corrections land on the
      ;; next warm). Restoring that stale row made the switch paint the OLD
      ;; position and then visibly scroll DOWN to the real bottom.
      (let [layout {:cols 120 :rows 40 :total-h 5000 :inner-h 30}]
        (reset! state/app-db {:scroll (assoc scroll/follow :pos 4970)
                              :layout layout
                              :tabs [{:id :main :label "Main" :active? true}
                                     {:id :tab-1 :label "Tab 1"}]
                              :active-tab-id :main
                              :tab-locals {:tab-1 {:scroll (assoc scroll/follow :pos 500)
                                                   :layout layout}}
                              :render-version 0})
        (state/dispatch [:select-tab-index 1])
        (expect (= scroll/follow (:scroll @state/app-db)))
        ;; nil layout-offset = exact bottom lock, and nothing left to animate.
        (expect (nil? (scroll/layout-offset (:scroll @state/app-db) 1200)))
        (expect (false? (scroll/animating? (:scroll @state/app-db) 1200)))))
  (it "invalidates cached layout and eased position after terminal geometry changes"
      (reset! state/app-db {:layout {:cols 140 :rows 50 :total-h 5000 :inner-h 40}
                            :tabs [{:id :main :label "Main" :active? true}
                                   {:id :tab-1 :label "Tab 1"}]
                            :active-tab-id :main
                            :tab-locals {:tab-1 {:layout {:cols 120 :rows 40 :total-h 7000}
                                                 :scroll {:mode :at :offset 5 :pos 1200}}}
                            :render-version 0})
      (state/dispatch [:select-tab-index 1])
      (expect (nil? (:layout @state/app-db)))
      (expect (= scroll/follow (:scroll @state/app-db))))
  (it "never leaks the leaving tab's live turn identity onto the incoming tab"
      ;; The regression: `tab-state-keys` omitted the turn identity, so the
      ;; snapshot dropped it AND `restore-tab` (a MERGE over the db root) left
      ;; the departing tab's ids in place — the incoming tab inherited them,
      ;; queued rows landed in the wrong session, and a lost `:cancelling-at-ms`
      ;; wedged `:cancelling?` forever because the self-heal could not time out.
      (reset! state/app-db {:session {:id "main-c"}
                            :gateway-turn-id "turn-main"
                            :live-turn-client-id "client-main"
                            :cancelling? true
                            :cancelling-at-ms 1000
                            :cancel-awaiting-client-id "client-main"
                            :queue-paused true
                            :tabs [{:id :main :label "Main" :active? true}
                                   {:id :tab-1 :label "Tab 1"}]
                            :active-tab-id :main
                            :tab-locals {:tab-1 {:session {:id "tab-c"}}}
                            :render-version 0})
      (state/dispatch [:select-tab-index 1])
      (let [db @state/app-db]
        (expect (= {:id "tab-c"} (:session db)))
        (expect (nil? (:gateway-turn-id db)))
        (expect (nil? (:live-turn-client-id db)))
        (expect (nil? (:cancelling-at-ms db)))
        (expect (nil? (:cancel-awaiting-client-id db)))
        (expect (nil? (:queue-paused db)))
        (expect (false? (:cancelling? db))))
      ;; ...and the parked tab gets its own turn back, intact.
      (state/dispatch [:select-tab-index 0])
      (let [db @state/app-db]
        (expect (= "turn-main" (:gateway-turn-id db)))
        (expect (= "client-main" (:live-turn-client-id db)))
        (expect (= 1000 (:cancelling-at-ms db)))
        (expect (= "client-main" (:cancel-awaiting-client-id db)))
        (expect (true? (:queue-paused db)))
        (expect (true? (:cancelling? db)))))
  (it "mirrors a background tab's queued turn into THAT tab, never the visible one"
      ;; The regression: `update-tab` built the background tab's db as
      ;; `(merge db snapshot)`, so every per-tab key MISSING from a partially
      ;; seeded snapshot (a pre-allocated project tab that was never focused and
      ;; only ever had a `:title` written into its locals) was filled in from the
      ;; tab you are LOOKING AT. A `turn.queued` for the background tab then
      ;; parked the visible tab's session, transcript, queue and live turn id
      ;; into it — the queued row surfaced under the wrong session, and could be
      ;; swallowed entirely by `live-turn-mirror?` matching the borrowed turn id.
      (reset! state/app-db {:session {:id "visible"}
                            :gateway-turn-id "turn-visible"
                            :live-turn-client-id "client-visible"
                            :messages [{:role :user :text "visible transcript"}]
                            :pending-sends [{:turn-id "q-visible" :text "visible queued"}]
                            :tabs [{:id :main :label "Main" :active? true}
                                   {:id :tab-1 :label "Tab 1"}]
                            :active-tab-id :main
                            :tab-locals {:tab-1 {:title "Tab 1"}}
                            :render-version 0})
      (state/dispatch [:sync-queued-turn :tab-1
                       {:op :add :turn-id "q-bg" :client-id "c-bg" :text "background queued"}])
      (let [locals (get-in @state/app-db [:tab-locals :tab-1])]
        (expect (= ["background queued"] (mapv :text (:pending-sends locals))))
        (expect (nil? (:session locals)))
        (expect (nil? (:gateway-turn-id locals)))
        (expect (nil? (:live-turn-client-id locals)))
        (expect (= [] (vec (:messages locals)))))
      ;; The visible tab keeps its own queue, and switching over shows only the
      ;; background tab's row.
      (expect (= ["visible queued"] (mapv :text (:pending-sends @state/app-db))))
      (state/dispatch [:select-tab-index 1])
      (expect (= ["background queued"] (mapv :text (:pending-sends @state/app-db))))
      (state/dispatch [:select-tab-index 0])
      (expect (= ["visible queued"] (mapv :text (:pending-sends @state/app-db)))))
  (it "selects workspaces by zero-based index and cycles to the next workspace"
      (reset! state/app-db {:tabs [{:id :main :label "Main"}
                                   {:id :tab-1 :label "Tab 1" :active? true}
                                   {:id :tab-2 :label "Tab 2"}]
                            :active-tab-id :tab-1
                            :render-version 0})
      (state/dispatch [:select-tab-index 0])
      (expect (= :main (:active-tab-id @state/app-db)))
      (expect (= [{:id :main :label "Main" :active? true} {:id :tab-1 :label "Tab 1"}
                  {:id :tab-2 :label "Tab 2"}]
                 (:tabs @state/app-db)))
      (state/dispatch [:select-tab-index :next])
      (expect (= :tab-1 (:active-tab-id @state/app-db)))
      (state/dispatch [:select-tab-index :next])
      (expect (= :tab-2 (:active-tab-id @state/app-db)))
      (state/dispatch [:select-tab-index :next])
      (expect (= :main (:active-tab-id @state/app-db)))
      (state/dispatch [:select-tab-index :prev])
      (expect (= :tab-2 (:active-tab-id @state/app-db)))
      (state/dispatch [:select-tab-index 99])
      (expect (= :tab-2 (:active-tab-id @state/app-db))))
  (it "selects an already-open workspace by session id"
      (reset! state/app-db {:tabs [{:id :main :label "Main" :active? true}
                                   {:id :tab-1 :label "Tab 1"}]
                            :active-tab-id :main
                            :session {:id "main-c"}
                            :messages [{:role :user :text "main prompt"}]
                            :input (input/paste-text (input/empty-input) "main draft")
                            :input-history ["main prompt"]
                            :tab-locals {:tab-1 {:session {:id "tab-c"}
                                                 :messages [{:role :user :text "tab prompt"}]
                                                 :input (input/paste-text (input/empty-input)
                                                                          "tab draft")
                                                 :input-history ["tab prompt"]}}
                            :render-version 0})
      (state/dispatch [:select-tab-by-session "tab-c"])
      (expect (= :tab-1 (:active-tab-id @state/app-db)))
      (expect (= {:id "tab-c"} (:session @state/app-db)))
      (expect (= [{:role :user :text "tab prompt"}] (:messages @state/app-db)))
      (state/dispatch [:select-tab-by-session "missing"])
      (expect (= :tab-1 (:active-tab-id @state/app-db)))))

(defdescribe
  init-settings-test
  (it "loads the default balanced reasoning level when config has none"
      (with-redefs
        [vis/load-config-raw (fn []
                               {})]
        (state/init!)
        (expect (= "balanced" (get-in @state/app-db [:settings :reasoning-level])))
        (expect (= "low" (get-in @state/app-db [:settings :openai-codex-verbosity])))
        (expect (= :blockether-light (get-in @state/app-db [:settings :theme-name])))
        (expect (not (contains? (:settings @state/app-db) :differentiate-turns)))
        (expect (true? (get-in @state/app-db [:settings :mouse-selection-copy])))))
  (it "hydrates persisted enum toggles into the registry"
      ;; The persistence shape now lives under `:toggles`, not
      ;; `:tui-settings`. `state/init!` keeps the `:settings`
      ;; projection coherent by pulling each migrated toggle's value
      ;; off the registry. (In production `screen/run-chat!` runs
      ;; hydration AFTER `init!` and then dispatches
      ;; `:resync-toggle-settings` — see the regression test below.)
      (vis/toggles-hydrate-from-config! {:toggles {"reasoning_level" :deep}})
      (try (with-redefs
             [vis/load-config-raw (fn []
                                    {})]
             (state/init!)
             (expect (= "deep" (get-in @state/app-db [:settings :reasoning-level]))))
           (finally (vis/toggle-reset-to-default! "reasoning_level"))))
  (it "resync repairs the projection when hydration runs AFTER init! (production order)"
      ;; Regression: `screen/run-chat!` calls `state/init!` FIRST — projecting
      ;; registry DEFAULTS into `:settings` — and only THEN hydrates the toggles
      ;; from config, followed by a `:resync-toggle-settings` dispatch. Without
      ;; that resync the footer keeps showing the default (`balanced`) while the
      ;; real toggle holds the persisted value, so the first Ctrl+X r cycle
      ;; advances the toggle only up to the already-displayed level and appears
      ;; to do nothing.
      (try (with-redefs
             [vis/load-config-raw (fn []
                                    {})]
             (state/init!)                     ;; projects default :balanced
             (vis/toggles-hydrate-from-config! ;; toggle -> persisted :quick
               {:toggles {"reasoning_level" :quick}})
             (expect (= "balanced" ;; stale projection, pre-resync
                        (get-in @state/app-db [:settings :reasoning-level])))
             (state/dispatch [:resync-toggle-settings]) ;; the fix
             (expect (= "quick" (get-in @state/app-db [:settings :reasoning-level]))))
           (finally (vis/toggle-reset-to-default! "reasoning_level"))))
  (it "hydrates Codex verbosity from the toggles registry"
      (vis/toggles-hydrate-from-config! {:toggles {"openai_codex_verbosity" :medium}})
      (try (with-redefs
             [vis/load-config-raw (fn []
                                    {})]
             (state/init!)
             (expect (= "medium" (get-in @state/app-db [:settings :openai-codex-verbosity]))))
           (finally (vis/toggle-reset-to-default! "openai_codex_verbosity"))))
  (it "drops invalid persisted enum values back to registered defaults"
      ;; `hydrate-from-config!` routes through `set-value!` which
      ;; validates against `:choices`. Invalid entries are silently
      ;; skipped — the registered default stands.
      (vis/toggles-hydrate-from-config! {:toggles {"reasoning_level" :turbo
                                                   "openai_codex_verbosity" :loud}})
      (try (with-redefs
             [vis/load-config-raw (fn []
                                    {})]
             (state/init!)
             (expect (= "balanced" (get-in @state/app-db [:settings :reasoning-level])))
             (expect (= "low" (get-in @state/app-db [:settings :openai-codex-verbosity]))))
           (finally (vis/toggle-reset-to-default! "reasoning_level")
                    (vis/toggle-reset-to-default! "openai_codex_verbosity")))))

(defdescribe
  settings-shortcut-test
  (it "commits shortcut settings before notification watchers dispatch render bumps"
      ;; The cycle event mutates the toggle registry; the cached
      ;; `:settings` projection is rebuilt synchronously in the same
      ;; FX :db so notification listeners observe the new value the
      ;; moment they fire.
      (vis/toggle-set-value! "reasoning_level" "deep")
      (try (with-redefs
             [vis/load-config-raw
              (fn []
                {})

              vis/save-config!
              (fn [_])

              vis/get-router
              (constantly :router)

              vis/resolve-effective-model
              (fn [_]
                {:provider :openai :name "gpt-5" :reasoning? true})

              vis/notify!
              (fn [& _]
                (state/dispatch [:bump-render-version]))]

             (reset! state/app-db {:settings {:reasoning-level "deep" :openai-codex-verbosity "low"}
                                   :render-version 0})
             (let [result (future (state/dispatch [:cycle-reasoning-level]) :done)]
               (expect (= :done (deref result 1000 :timeout)))
               (expect (= "quick" (vis/toggle-value "reasoning_level")))
               (expect (= "quick" (get-in @state/app-db [:settings :reasoning-level])))))
           (finally (vis/toggle-reset-to-default! "reasoning_level"))))
  (it "advances reasoning exactly one step when the registry listener dispatches back"
      ;; REGRESSION (Ctrl+X r cycled forever): `dispatch` runs an :fx handler's
      ;; FUNCTION inside `swap! app-db` and only its returned effects after the
      ;; commit. Flipping the toggle in the function body ran the registry
      ;; listener synchronously; the listener's re-entrant
      ;; :resync-toggle-settings dispatch committed an inner swap, the outer CAS
      ;; failed, and the retry cycled the level AGAIN - each retry guaranteeing
      ;; the next. The flip now lives in the :cycle-toggle EFFECT, so one
      ;; keystroke advances exactly one step.
      (vis/toggle-set-value! "reasoning_level" "quick")
      (let
        [dispose (vis/toggle-add-listener! (fn [event]
                                             (state/dispatch [:resync-toggle-settings
                                                              (:id event)])))]
        (try (with-redefs
               [vis/load-config-raw (fn []
                                      {})
                vis/save-config! (fn [_])
                vis/get-router (constantly :router)
                vis/resolve-effective-model (fn [_]
                                              {:provider :openai :name "gpt-5" :reasoning? true})
                vis/notify! (fn [& _])]

               (reset! state/app-db {:settings {:reasoning-level "quick"} :render-version 0})
               (let [result (future (state/dispatch [:cycle-reasoning-level]) :done)]
                 (expect (= :done (deref result 2000 :timeout)))
                 (expect (= "balanced" (vis/toggle-value "reasoning_level")))
                 (expect (= "balanced" (get-in @state/app-db [:settings :reasoning-level])))))
             (finally (dispose) (vis/toggle-reset-to-default! "reasoning_level")))))
  (it "wraps reasoning level from deep back to quick"
      (vis/toggle-set-value! "reasoning_level" "deep")
      (try (with-redefs
             [vis/load-config-raw
              (fn []
                {})

              vis/save-config!
              (fn [_])

              vis/get-router
              (constantly :router)

              vis/resolve-effective-model
              (fn [_]
                {:provider :openai :name "gpt-5" :reasoning? true})

              vis/notify!
              (fn [& _])]

             (reset! state/app-db {:settings {:reasoning-level "deep" :openai-codex-verbosity "low"}
                                   :render-version 0})
             (state/dispatch [:cycle-reasoning-level])
             (expect (= "quick" (vis/toggle-value "reasoning_level")))
             (expect (= "quick" (get-in @state/app-db [:settings :reasoning-level]))))
           (finally (vis/toggle-reset-to-default! "reasoning_level"))))
  (it "leaves reasoning unchanged for fixed-thinking Z.ai models"
      (let [notified (atom nil)]
        (with-redefs
          [vis/get-router (constantly :router)
           vis/resolve-effective-model (fn [_]
                                         {:provider :zai
                                          :name "glm-4.7"
                                          :reasoning? true
                                          :reasoning-style :zai-thinking
                                          :reasoning-effort? false})
           vis/notify! (fn [text & kvs]
                         (reset! notified [text kvs]))]

          (reset! state/app-db {:settings {:reasoning-level "deep" :openai-codex-verbosity "low"}
                                :render-version 0})
          (state/dispatch [:cycle-reasoning-level])
          (expect (= "deep" (get-in @state/app-db [:settings :reasoning-level])))
          (expect (= ["Reasoning effort is not configurable for this model"
                      [:level :warn :ttl-ms 1500]]
                     @notified)))))
  (it "leaves Codex verbosity unchanged for non-Codex providers"
      (let [notified (atom nil)]
        (with-redefs
          [vis/get-router (constantly :router)
           vis/resolve-effective-model (fn [_]
                                         {:provider :zai :name "glm-4.7"})
           vis/notify! (fn [text & kvs]
                         (reset! notified [text kvs]))]

          (reset! state/app-db {:settings {:reasoning-level "balanced"
                                           :openai-codex-verbosity "high"}
                                :render-version 0})
          (state/dispatch [:cycle-codex-verbosity])
          (expect (= "high" (get-in @state/app-db [:settings :openai-codex-verbosity])))
          (expect (= ["Codex verbosity is only available for OpenAI Codex"
                      [:level :warn :ttl-ms 1500]]
                     @notified)))))
  (it
    "cycles Codex verbosity low -> medium -> high -> low"
    (with-redefs
      [vis/load-config-raw
       (fn []
         {})

       vis/save-config!
       (fn [_])

       vis/get-router
       (constantly :router)

       vis/resolve-effective-model
       (fn [_]
         {:provider :openai-codex :name "gpt-5.5" :reasoning? true})

       vis/notify!
       (fn [& _])]

      ;; The cycle advances the GLOBAL toggles registry, not app-db — pin it
      ;; to its :low default so a value another test left in the shared
      ;; registry can't shift where the first step lands (order-dependent flake).
      (vis/toggle-reset-to-default! "openai_codex_verbosity")
      (try (reset! state/app-db {:settings {:reasoning-level "balanced"
                                            :openai-codex-verbosity "low"}
                                 :render-version 0})
           (state/dispatch [:cycle-codex-verbosity])
           (expect (= "medium" (get-in @state/app-db [:settings :openai-codex-verbosity])))
           (state/dispatch [:cycle-codex-verbosity])
           (expect (= "high" (get-in @state/app-db [:settings :openai-codex-verbosity])))
           (state/dispatch [:cycle-codex-verbosity])
           (expect (= "low" (get-in @state/app-db [:settings :openai-codex-verbosity])))
           (finally (vis/toggle-reset-to-default! "openai_codex_verbosity"))))))

(defdescribe session-model-pref-scope-test
             ;; The footer chip PREFERS the optimistic `:session-model-pref` over the
             ;; gateway's stored value. That value belongs to ONE session, so it must not
             ;; outlive it: leaving it behind made the chip advertise the previous
             ;; session's model while turns ran on the newly opened session's real
             ;; preference — "changing the model in the footer didn't reach the session".
             (it "opening another session in the tab drops the previous session's optimistic pick"
                 (with-redefs
                   [vis/notify! (fn [& _]
                                  nil)]
                   (reset! state/app-db {:session {:id "sess-1"}
                                         :session-model-pref {:provider "zai" :model "glm-4.6"}
                                         :render-version 0})
                   (state/dispatch [:init-session {:id "sess-2"} [] {}])
                   (expect (= "sess-2" (get-in @state/app-db [:session :id])))
                   (expect (nil? (:session-model-pref @state/app-db)))))
             (it "a REFUSED switch rolls the optimistic pick back instead of leaving the chip lying"
                 (let [notified (atom [])]
                   (with-redefs
                     [vis/gateway-set-session-model! (fn [_sid _provider _model]
                                                       (throw (ex-info "unknown provider" {})))
                      vis/notify! (fn [text & _]
                                    (swap! notified conj text))]

                     (reset! state/app-db {:session {:id "sess-1"} :render-version 0})
                     (state/dispatch [:set-model "openai" "gpt-5"])
                     (expect (nil? (:session-model-pref @state/app-db)))
                     (expect (some #(re-find #"Model switch failed" (str %)) @notified)))))
             (it "a switch that SUCCEEDS keeps the pick on the chip"
                 (with-redefs
                   [vis/gateway-set-session-model!
                    (fn [_sid provider model]
                      {:provider provider :model model})

                    vis/notify!
                    (fn [& _]
                      nil)]

                   (reset! state/app-db {:session {:id "sess-1"} :render-version 0})
                   (state/dispatch [:set-model "openai" "gpt-5"])
                   (expect (= {:provider "openai" :model "gpt-5"}
                              (:session-model-pref @state/app-db))))))

(defdescribe
  sync-session-model-test
  ;; The per-session model pref is shared, but every channel renders its OWN
  ;; copy: without the gateway's `session.model_updated` broadcast a switch
  ;; made in the companion app (or a sibling TUI) never moved this footer's
  ;; chip, which is the whole "I changed the model and nothing changed" bug.
  (it "projects a sibling channel's model change onto the tab"
      (reset! state/app-db {:session {:id "sess-1"} :render-version 0})
      (state/dispatch [:sync-session-model nil {:provider "zai-coding-plan" :model "glm-4.7"}])
      (expect (= {:provider "zai-coding-plan" :model "glm-4.7"}
                 (:session-model-pref @state/app-db))))
  (it "a CLEARED override drops the local pick back to the router default"
      (reset! state/app-db {:session {:id "sess-1"}
                            :session-model-pref {:provider "zai-coding-plan" :model "glm-4.7"}
                            :render-version 0})
      (state/dispatch [:sync-session-model nil {:provider nil :model nil}])
      (expect (nil? (:session-model-pref @state/app-db)))))

(defdescribe
  model-shortcut-test
  ;; Ctrl+T sets the ACTIVE SESSION's persisted model preference (the shared,
  ;; channel-neutral store the web + engine read) instead of reordering global
  ;; config. Fresh sessions start with no explicit pref, so the first press
  ;; advances from the displayed router default to the next configured entry.
  (it
    "fresh session advances from displayed router default to the next configured model"
    (let
      [set-calls
       (atom [])

       notified
       (atom nil)]

      (with-redefs
        [vis/configured-providers
         (fn []
           [{:id :openai :models [{:name "gpt-5"} {:name "gpt-5-mini"}]}
            {:id :zai :models [{:name "glm-4.6"}]}])

         vis/gateway-session-model
         (fn [_sid]
           nil)

         vis/gateway-set-session-model!
         (fn [sid provider model]
           (swap! set-calls conj [sid provider model])
           {:provider provider :model model})

         state/current-model-info
         (fn []
           {:provider :openai :name "gpt-5"})

         vis/notify!
         (fn [text & kvs]
           (reset! notified [text kvs]))]

        (reset! state/app-db {:session {:id "sess-1"} :render-version 0})
        (state/dispatch [:cycle-model])
        (expect (= [["sess-1" "openai" "gpt-5-mini"]] @set-calls))
        (expect (= ["Model: openai/gpt-5-mini" [:level :info :ttl-ms 1500]] @notified)))))
  (it "advances from the current pref (matched by provider+model) to the next, wrapping"
      (let [set-calls (atom [])]
        (with-redefs
          [vis/configured-providers (fn []
                                      [{:id :openai :models [{:name "gpt-5"} {:name "gpt-5-mini"}]}
                                       {:id :zai :models [{:name "glm-4.6"}]}])
           vis/gateway-session-model (fn [_sid]
                                       {:provider "zai" :model "glm-4.6"}) ; last -> wraps
           vis/gateway-set-session-model! (fn [sid provider model]
                                            (swap! set-calls conj [sid provider model])
                                            {:provider provider :model model})
           state/current-model-info (fn []
                                      {:provider :openai :name "gpt-5"})
           vis/notify! (fn [_ & _])]

          (reset! state/app-db {:session {:id "sess-1"} :render-version 0})
          (state/dispatch [:cycle-model])
          (expect (= [["sess-1" "openai" "gpt-5"]] @set-calls)))))
  (it "with no active session, asks to open one and sets nothing"
      (let
        [set-calls
         (atom [])

         notified
         (atom nil)]

        (with-redefs
          [vis/db-info
           (fn []
             :db)

           vis/session-model-of
           (fn [_db _sid]
             nil)

           vis/set-session-model!
           (fn [_db sid provider model]
             (swap! set-calls conj [sid provider model])
             model)

           vis/notify!
           (fn [text & kvs]
             (reset! notified [text kvs]))]

          (reset! state/app-db {:config {:providers [{:id :openai :models [{:name "gpt-5"}]}]}
                                :render-version 0})
          (state/dispatch [:cycle-model])
          (expect (empty? @set-calls))
          (expect (= "Open a session first to choose its model" (first @notified)))))))

(defdescribe scrollbar-state-test
             (let
               [scroll-to-y-fn (-> #'state/event-registry
                                   deref
                                   deref
                                   (get :scroll-to-y)
                                   :fn)]
               (it "a scrollbar drag above the bottom parks at the mapped offset (mode :at)"
                   ;; total-h 360, inner-h 56 -> max-s 304; track-h 56, denom 55;
                   ;; mouse-y 28 -> fraction 28/55 -> offset round(.509*304)=155.
                   (let [r (scroll-to-y-fn {:scroll scroll/follow} [:scroll-to-y 28 0 56 360 56])]
                     (expect (= {:mode :at :offset 155} (:scroll r)))))
               (it "a scrollbar drag to the very bottom re-enters FOLLOW"
                   ;; fraction 1.0 -> offset == max-scroll -> stick-to-bottom again, so
                   ;; streamed content keeps the latest message in view.
                   (let [r (scroll-to-y-fn {:scroll scroll/follow} [:scroll-to-y 55 0 56 360 56])]
                     (expect (= scroll/follow (:scroll r)))))))

;; The scroll model is ONE tagged `:scroll` value (see scroll.clj). These
;; cover the event wrappers + the re-pin invariant that killed the
;; "/workspace list flashes to the top then bottom" jump: every transition
;; REPLACES `:scroll`, so no animation target can dangle across frames.
(defdescribe
  scroll-model-test
  (let
    [ev (fn [k]
          (-> #'state/event-registry
              deref
              deref
              (get k)
              :fn))]
    (it "scroll-up parks (mode :at) so streaming follow hands off"
        ;; :scroll-up is an fx event now (it can kick the older-history fetch),
        ;; so the new db hides under :db.
        (let [r (:db ((ev :scroll-up) {:scroll scroll/follow} [:scroll-up 9 200 100]))]
          ;; max-s = 100; ease from the bottom (100) up to 100-9 = 91.
          (expect (= :at (:mode (:scroll r))))
          (expect (= 91 (:offset (:scroll r))))))
    (it "scroll-up near the top of a PAGED session asks for one older page"
        ;; The session opened on its newest turns only, so reaching the top of
        ;; :messages is not reaching the top of the session.
        (let
          [db {:scroll (scroll/parked 300)
               :session {:id "s1" :history-cursor {:offset 40 :total 100 :has-more true}}}
           r ((ev :scroll-up) db [:scroll-up 290 1000 30])]

          (expect (= [[:load-older-history "s1" 40]] (:fx r)))
          ;; latched, so a fast wheel cannot queue a second identical fetch
          (expect (true? (get-in r [:db :session :history-loading?])))
          (expect (nil? (:fx ((ev :scroll-up)
                               (assoc-in db [:session :history-loading?] true)
                               [:scroll-up 290 1000 30]))))
          ;; nothing older left -> no request at all
          (expect (nil? (:fx ((ev :scroll-up)
                               (assoc-in db [:session :history-cursor :has-more] false)
                               [:scroll-up 290 1000 30]))))
          ;; still far from the top -> no request
          (expect (nil? (:fx ((ev :scroll-up) db [:scroll-up 10 1000 30]))))))
    (it "prepend-history splices older turns in and holds the viewport still"
        (let
          [db {:scroll (scroll/parked 10)
               :messages [:m1 :m2]
               :session {:id "s1"
                         :history-loading? true
                         :history-cursor {:offset 40 :total 100 :has-more true}}}
           page {:messages [:o1 :o2] :offset 30 :total 100 :has-more true}
           r ((ev :prepend-history) db [:prepend-history "s1" page 120])]

          (expect (= [:o1 :o2 :m1 :m2] (:messages r)))
          ;; the read bubble stays put: offset grows by the measured page height
          (expect (= 130 (:offset (:scroll r))))
          (expect (= {:offset 30 :total 100 :has-more true} (get-in r [:session :history-cursor])))
          (expect (false? (get-in r [:session :history-loading?])))
          ;; a page for a session this tab no longer shows is dropped whole
          (expect (= db ((ev :prepend-history) db [:prepend-history "other" page 120])))))
    (it "scroll-down landing in the bottom slack band re-arms FOLLOW"
        (let [r ((ev :scroll-down) {:scroll (scroll/parked 90)} [:scroll-down 30 200 100])]
          ;; max-s 100; 90+30 within slack of 100 -> follow (eases the rest).
          (expect (= :follow (:mode (:scroll r))))))
    (it "scroll-down above the slack band stays parked"
        (let [r ((ev :scroll-down) {:scroll (scroll/parked 10)} [:scroll-down 30 200 100])]
          (expect (= :at (:mode (:scroll r))))
          (expect (= 40 (:offset (:scroll r))))))
    (it "ease-scroll walks FOLLOW toward the growing bottom (no teleport)"
        ;; Regression for the streamed big-block "jump jump": a turn appends
        ;; a tall bubble in one frame. FOLLOW's desired row IS the new bottom,
        ;; so ease steps the on-screen pos down toward it instead of snapping.
        (let [r ((ev :ease-scroll) {:scroll (assoc scroll/follow :pos 100)} [:ease-scroll 300 100])]
          ;; max-s 200; step 0.35*(200-100)=35 -> pos 135, still FOLLOW.
          (expect (= {:mode :follow :pos 135} (:scroll r)))))
    (it "ease-scroll settles a parked move and drops :pos"
        (let [r ((ev :ease-scroll) {:scroll {:mode :at :offset 50 :pos 50}} [:ease-scroll 150 100])]
          (expect (= {:mode :at :offset 50} (:scroll r)))))
    (it "ease-scroll preserves :scroll IDENTITY when settled (fast-path survives)"
        ;; Regression for the streaming FULL-frame spin: `scroll/ease` re-`assoc`s
        ;; :pos every tick, so a settled follow-bottom returned a fresh-but-EQUAL
        ;; scroll map each ~80ms pulse. app-db's :scroll churned identity, and the
        ;; render loop's identical?-keyed fast paths (live-progress-only-change?)
        ;; demoted every progress tick to a FULL repaint. The handler must return
        ;; db UNTOUCHED when nothing moved so the cheap partial-live path stays live.
        (let
          [db {:scroll (assoc scroll/follow :pos 200) :progress {:iterations []}}
           ;; max-s = total-h(240) - inner-h(40) = 200, already at bottom -> settled
           once ((ev :ease-scroll) db [:ease-scroll 240 40])
           twice ((ev :ease-scroll) once [:ease-scroll 240 40])]

          ;; no move -> same db object, and :scroll identity is stable across ticks
          (expect (identical? once db))
          (expect (identical? (:scroll once) (:scroll twice)))
          ;; real growth still moves the scroll (view follows the new bottom)
          (expect (not (identical? (:scroll twice)
                                   (:scroll ((ev :ease-scroll) twice [:ease-scroll 340 40])))))))
    (it "set-scroll snap-parks at an exact row (search jump)"
        (let [r ((ev :set-scroll) {:scroll scroll/follow} [:set-scroll 42])]
          (expect (= {:mode :at :offset 42} (:scroll r)))))
    (it "reanchor-scroll shifts the parked offset + pos by the same delta"
        ;; Content above the anchor shrank by 450 rows as estimates resolved;
        ;; the anchored message must stay visually put.
        (let
          [r ((ev :reanchor-scroll)
               {:scroll {:mode :at :offset 1840 :pos 1849}}
               [:reanchor-scroll 1399 -450])]
          (expect (= {:mode :at :offset 1390 :pos 1399} (:scroll r)))))
    (it "message-received holds the painted row before easing a final result"
        ;; Regression (/workspace list "teleports to the new tail"): a result
        ;; lands atomically while an ease was in flight. Keep that painted row
        ;; for the first layout, then let FOLLOW ease from it to the new tail.
        (let
          [message-received-fn (ev :message-received)
           pending-id "turn-1"
           db {:active-tab-id :main
               :session {:id "c1"}
               :loading? true
               :layout {:total-h 120 :inner-h 20}
               :messages [{:role :user :text "/workspace list" :client-turn-id pending-id}
                          {:role :assistant :pending? true :client-turn-id pending-id}]
               :progress {:iterations []}
               ;; An ease was in flight from the prior frame.
               :scroll {:mode :follow :pos 80}}
           {db' :db} (message-received-fn db
                                          [:message-received :main
                                           [:ast {} [:p {} [:span {} "a big table"]]]
                                           {:client-turn-id pending-id}])]

          (expect (= {:mode :follow :pos 80 :reveal-from 100} (:scroll db')))))
    (it "send-message re-pins to a CLEAN FOLLOW"
        (let
          [send-message-fn (ev :send-message)
           db {:session {:id "c1"}
               :active-tab-id :main
               :messages []
               :input-history []
               :scroll {:mode :at :offset 80 :pos 80}
               :settings {:reasoning-level "balanced" :openai-codex-verbosity "low"}
               :pastes {}}]

          (with-redefs
            [input/expand-paste-placeholders (fn [text _]
                                               text)
             input/expand-file-mentions identity
             vis/cancellation-token (fn []
                                      :token)]

            (let [{db' :db} (send-message-fn db [:send-message "hello"])]
              (expect (= scroll/follow (:scroll db')))))))))

(defdescribe
  cancel-turn-test
  (it
    "notifies cancelling instead of relying on footer status"
    (let
      [cancelled
       (atom nil)

       notified
       (atom nil)

       gateway-started
       (promise)

       release-gateway
       (promise)]

      (with-redefs
        [vis/cancel!
         (fn [token]
           (reset! cancelled token))

         vis/gateway-cancel-current-turn!
         (fn [_]
           (deliver gateway-started true)
           @release-gateway
           {:status "cancelling"})

         vis/notify!
         (fn [text & kvs]
           (reset! notified [text kvs]))]

        (reset! state/app-db {:session {:id "s1"}
                              :loading? true
                              :cancel-token :token
                              :cancelling? false
                              :render-version 0})
        ;; This returns while the gateway call is deliberately blocked.
        (state/dispatch [:cancel-turn])
        (expect (= :token @cancelled))
        (expect (= true (deref gateway-started 1000 :timeout)))
        (expect (true? (:cancelling? @state/app-db)))
        (expect (= ["Cancelling current turn..." [:level :info :ttl-ms 2500]] @notified))
        (deliver release-gateway true)))))

(defdescribe cancel-reaches-gateway-after-send-test
             (it "binds the matching send-message turn so Esc reaches the gateway"
                 (let [cancelled-gateway (promise)]
                   (with-redefs
                     [vis/cancel! (fn [_]
                                    nil)
                      vis/notify! (fn [_ & _]
                                    nil)
                      vis/gateway-cancel-turn! (fn [sid tid]
                                                 (deliver cancelled-gateway [sid tid])
                                                 {:status "cancelling"})]

                     ;; State after :send-message submitted a fresh turn: its client id is
                     ;; known locally, while its server-minted turn id is not.
                     (reset! state/app-db {:session {:id "s1"}
                                           :active-tab-id "s1"
                                           :render-version 0
                                           :loading? true
                                           :cancel-token :token
                                           :cancelling? false
                                           :live-turn-client-id "cid-1"
                                           :gateway-turn-id nil
                                           :turn-start-ms 10})
                     (state/dispatch [:sync-turn-clock nil
                                      {:turn-id "gw-turn-1" :client-id "cid-1" :started-at-ms 123}])
                     (expect (= "gw-turn-1" (:gateway-turn-id @state/app-db)))
                     (state/dispatch [:cancel-turn])
                     (expect (= ["s1" "gw-turn-1"] (deref cancelled-gateway 1000 :timeout)))))))

(defdescribe cancel-auto-fires-on-late-bind-test
             ;; REGRESSION: Esc can beat the submit POST, receive `no-running-turn`, and
             ;; clear locally. An immediate identical resend then races the OLD POST's
             ;; delayed turn.started. Correlate starts by client id so the old ghost is
             ;; cancelled, never rebound as the new visible turn.
             (it
               "quick cancel plus identical resend cancels only the delayed old start"
               (let
                 [cancelled-gateway
                  (promise)

                  terminal-cleared
                  (promise)

                  send-message-fn
                  (:fn (get @@#'state/event-registry :send-message))]

                 (with-redefs
                   [vis/cancel!
                    (fn [_]
                      nil)

                    vis/notify!
                    (fn [text & _]
                      (when (= "Turn is no longer running; cleared local cancelling state." text)
                        (deliver terminal-cleared true)))

                    vis/gateway-cancel-current-turn!
                    (fn [_]
                      {:error :no-running-turn})

                    vis/gateway-cancel-turn!
                    (fn [sid tid]
                      (deliver cancelled-gateway [sid tid])
                      {:status "cancelling"})]

                   (reset! state/app-db {:session {:id "s1"}
                                         :active-tab-id "s1"
                                         :render-version 0
                                         :workspace {:workspace/root "."}
                                         :messages []
                                         :input-history []
                                         :pastes {}
                                         :paste-counter 0
                                         :loading? true
                                         :cancel-token :old-token
                                         :cancelling? false
                                         :live-turn-client-id "cid-old"
                                         :gateway-turn-id nil
                                         :turn-start-ms 10})
                   ;; Esc won before the old POST registered. The terminal-looking gateway
                   ;; response clears the visible turn, but its exact identity survives.
                   (state/dispatch [:cancel-turn])
                   (expect (= true (deref terminal-cleared 1000 :timeout)))
                   (expect (false? (:loading? @state/app-db)))
                   (expect (= "cid-old" (:cancel-awaiting-client-id @state/app-db)))
                   ;; The user immediately sends the exact same text again. The normal send
                   ;; path must preserve the old marker while assigning a fresh identity.
                   (let
                     [{resent-db :db}
                      (send-message-fn @state/app-db [:send-message "same text" nil])

                      new-client-id
                      (:live-turn-client-id resent-db)]

                     (reset! state/app-db resent-db)
                     (expect (= "cid-old" (:cancel-awaiting-client-id @state/app-db)))
                     (expect (not= "cid-old" new-client-id))
                     ;; The old POST starts late. Cancel that gateway turn, but do not bind
                     ;; its id or clock onto the new optimistic request.
                     (state/dispatch [:sync-turn-clock nil
                                      {:turn-id "gw-old" :client-id "cid-old" :started-at-ms 123}])
                     (expect (= ["s1" "gw-old"] (deref cancelled-gateway 1000 :timeout)))
                     (expect (nil? (:gateway-turn-id @state/app-db)))
                     (expect (true? (:loading? @state/app-db)))
                     (expect (nil? (:cancel-awaiting-client-id @state/app-db)))
                     ;; Once the queued resend starts, only its matching id may bind.
                     (state/dispatch
                       [:sync-turn-clock nil
                        {:turn-id "gw-new" :client-id new-client-id :started-at-ms 456}])
                     (expect (= "gw-new" (:gateway-turn-id @state/app-db)))
                     (expect (= 456 (:turn-start-ms @state/app-db))))))))

(defdescribe
  cancel-turn-stale-gateway-test
  (it
    "clears stale cancelling state when gateway turn is already terminal"
    (let
      [cancelled
       (atom nil)

       cancelled-gateway
       (atom nil)

       notified
       (atom nil)

       terminal-cleared
       (promise)]

      (with-redefs
        [vis/cancel!
         (fn [token]
           (reset! cancelled token))

         vis/gateway-cancel-turn!
         (fn [sid tid]
           (reset! cancelled-gateway [sid tid])
           {:error :not-running :status "interrupted"})

         vis/notify!
         (fn [text & kvs]
           (reset! notified [text kvs])
           (when (= "Turn is no longer running; cleared local cancelling state." text)
             (deliver terminal-cleared true)))]

        (reset! state/app-db {:session {:id "s1"}
                              :loading? true
                              :cancel-token :token
                              :gateway-turn-id "turn-1"
                              :cancelling? false
                              :progress {:iterations []}
                              :turn-start-ms 10
                              :render-version 0})
        (state/dispatch [:cancel-turn])
        (expect (= true (deref terminal-cleared 1000 :timeout)))
        (let [db @state/app-db]
          (expect (= :token @cancelled))
          (expect (= ["s1" "turn-1"] @cancelled-gateway))
          (expect (false? (:loading? db)))
          (expect (false? (:cancelling? db)))
          (expect (nil? (:cancel-token db)))
          (expect (nil? (:gateway-turn-id db)))
          (expect (nil? (:progress db)))
          (expect (nil? (:turn-start-ms db)))
          (expect (= ["Turn is no longer running; cleared local cancelling state."
                      [:level :info :ttl-ms 2500]]
                     @notified)))))))

(defdescribe
  cancel-gateway-confirmation-test
  (it "retries a transient gateway failure instead of dropping the cancel"
      (let [attempts (atom 0)]
        (with-redefs
          [vis/gateway-cancel-turn! (fn [sid tid]
                                      (expect (= ["s1" "turn-1"] [sid tid]))
                                      (if (= 1 (swap! attempts inc))
                                        (throw (ex-info "connection reset" {}))
                                        {:status "cancelling"}))]
          (expect (= {:status "cancelling"}
                     (#'state/gateway-cancel-turn-or-current! "s1" "turn-1")))
          (expect (= 2 @attempts)))))
  (it
    "does not unlock resending on a local cancel before the gateway ACK"
    (let
      [message-fn
       (-> #'state/event-registry
           deref
           deref
           (get :message-received)
           :fn)

       cancel-result-fn
       (-> #'state/event-registry
           deref
           deref
           (get :gateway-cancel-result)
           :fn)

       cancel-key
       1000

       pending-id
       "client-1"

       initial-db
       {:active-tab-id :main
        :session {:id "s1"}
        :input (input/empty-input)
        :loading? true
        :cancelling? true
        :cancelling-at-ms cancel-key
        :cancel-token :token
        :gateway-turn-id "turn-1"
        :messages [{:role :user :text "first" :client-turn-id pending-id}
                   {:role :assistant :pending? true :client-turn-id pending-id}]
        :progress {:iterations []}
        :submitted-input {:text "first" :pastes {} :paste-counter 0}
        :pending-sends [{:text "correction" :client-id "queued-1" :mine? true}]}

       local-result
       (message-fn initial-db
                   [:message-received :main [:ast {} [:p {} [:span {} "Cancelled by user."]]]
                    {:status :cancelled :client-turn-id pending-id}])

       local-db
       (:db local-result)

       ack-result
       (cancel-result-fn local-db [:gateway-cancel-result cancel-key {:status "cancelling"}])]

      (expect (true? (:loading? local-db)))
      (expect (true? (:cancelling? local-db)))
      (expect (= cancel-key (:cancelling-at-ms local-db)))
      ;; ...but ONLY the invisible send gate stays armed. The turn the user was
      ;; watching is over: no live progress, no elapsed clock ticking on under a
      ;; transcript whose rows were already dropped and whose prompt is back in
      ;; the composer. Anything else reads as a frozen, mismatched frame.
      (expect (nil? (:progress local-db)))
      (expect (nil? (:turn-start-ms local-db)))
      (expect (nil? (:submitted-input local-db)))
      (expect (empty? (:messages local-db)))
      (expect (= "first" (input/input->text (:input local-db))))
      ;; The queued backlog comes back WITH the prompt, not one ACK later.
      (expect (= [[:dispatch [:restore-pending-to-input :main]]] (:fx local-result)))
      (expect (false? (:loading? (:db ack-result))))
      (expect (false? (:cancelling? (:db ack-result))))
      (expect (= [[:notify "Cancellation accepted. You can send again." :info 2500]
                  [:dispatch [:restore-pending-to-input :main]]]
                 (:fx ack-result))))))

(defdescribe cancel-settles-once-test
             ;; REGRESSION: a cancel settles in two halves — the LOCAL attach worker's
             ;; synthetic `:cancelled` result and the daemon's `:gateway-cancel-result`
             ;; ACK — and their order is not guaranteed. Only the local half restored the
             ;; editor, so when the ACK won the race the turn was released (`:loading?`
             ;; false, sends allowed) while the composer stayed EMPTY, the submitted
             ;; prompt sat unreachable in `:submitted-input` and a pending assistant
             ;; placeholder no event would resolve stayed in the transcript. Either half
             ;; must now produce the same settled frame.
             (let
               [handler
                (fn [id]
                  (-> #'state/event-registry
                      deref
                      deref
                      (get id)
                      :fn))

                pending-id
                "client-1"

                cancel-key
                1000

                cancelling-db
                (fn [iterations]
                  {:active-tab-id :main
                   :session {:id "s1"}
                   :loading? true
                   :cancelling? true
                   :cancelling-at-ms cancel-key
                   :cancel-token :token
                   :gateway-turn-id "turn-1"
                   :live-turn-client-id pending-id
                   :turn-start-ms 1000
                   :input (input/empty-input)
                   :messages [{:role :user :text "first" :client-turn-id pending-id}
                              {:role :assistant :pending? true :client-turn-id pending-id}]
                   :progress {:iterations iterations}
                   :submitted-input {:text "first" :pastes {} :paste-counter 0}
                   :pending-sends []})]

               (it "hands the prompt back when the gateway ACK beats the local result"
                   (let
                     [acked
                      (:db ((handler :gateway-cancel-result)
                             (cancelling-db [])
                             [:gateway-cancel-result cancel-key {:status "cancelling"}]))

                      ;; The late local result must find nothing left to settle.
                      settled
                      (:db ((handler :message-received)
                             acked
                             [:message-received :main
                              [:ast {} [:p {} [:span {} "Cancelled by user."]]]
                              {:status :cancelled :client-turn-id pending-id}]))]

                     (expect (= "first" (input/input->text (:input acked))))
                     (expect (empty? (:messages acked)))
                     (expect (nil? (:submitted-input acked)))
                     (expect (false? (:loading? acked)))
                     (expect (nil? (:progress acked)))
                     (expect (= "first" (input/input->text (:input settled))))
                     (expect (empty? (:messages settled)))))
               (it "keeps the bubble and only refills the editor when the cancel had work"
                   (let
                     [acked (:db ((handler :gateway-cancel-result)
                                   (cancelling-db [{:n 1 :blocks [{:kind :tool}]}])
                                   [:gateway-cancel-result cancel-key {:status "cancelling"}]))]
                     (expect (= 2 (count (:messages acked))))
                     (expect (= [{:n 1 :blocks [{:kind :tool}]}]
                                (get-in acked [:messages 1 :terminal-pending :trace])))
                     (expect (= "first" (input/input->text (:input acked))))
                     (expect (nil? (:submitted-input acked)))))
               (it "hands the prompt back when the cancel self-heals"
                   (let
                     [healed (:db ((handler :cancel-self-heal-tick)
                                    (cancelling-db [])
                                    [:cancel-self-heal-tick (+ cancel-key 60000)]))]
                     (expect (= "first" (input/input->text (:input healed))))
                     (expect (empty? (:messages healed)))
                     (expect (nil? (:submitted-input healed)))
                     (expect (false? (:cancelling? healed)))))
               (it "never overwrites a newer draft typed while the cancel settles"
                   (let
                     [typed
                      (assoc (cancelling-db []) :input (#'state/text->input-state "new idea"))

                      acked
                      (:db ((handler :gateway-cancel-result)
                             typed
                             [:gateway-cancel-result cancel-key {:status "cancelling"}]))]

                     (expect (= "new idea" (input/input->text (:input acked))))
                     (expect (nil? (:submitted-input acked)))))))


(defdescribe cancel-self-heal-test
             ;; REGRESSION (design edge): `:cancel-turn` flips `:cancelling?` and
             ;; waits for the daemon's terminal `turn.completed` (cancelled) event
             ;; to release it. If that event NEVER lands — an SSE reconnect gap
             ;; right at cancel, or the daemon dying mid-unwind — the flag sticks
             ;; true, every send parks purely local (the enqueue race guard), and
             ;; input is wedged until the daemon's ~6-minute stall watchdog fires:
             ;; a freeze, to a human. The render-loop heartbeat pokes
             ;; `:cancel-self-heal-tick`, which self-heals once the pending flag
             ;; outlives `cancel-self-heal-timeout-ms` (8s). Pure over an injected
             ;; `now-ms`, so the dropped-event scenario is deterministic here.
             (let
               [heal-fn (-> #'state/event-registry
                            deref
                            deref
                            (get :cancel-self-heal-tick)
                            :fn)]
               (it "no-ops while the pending cancel is younger than the timeout"
                   (with-redefs
                     [vis/cancel! (fn [_]
                                    (throw (ex-info "self-heal must not fire early" {})))]
                     (let
                       [db {:active-tab-id :main
                            :session {:id "s1"}
                            :loading? true
                            :cancel-token :token
                            :cancelling? true
                            :cancelling-at-ms 1000}
                        ;; 1s elapsed ≪ 8s timeout
                        {db' :db} (heal-fn db [:cancel-self-heal-tick 2000])]

                       (expect (true? (:cancelling? db')))
                       (expect (true? (:loading? db'))))))
               (it "clears the stuck cancel once it outlives the timeout"
                   (let [cancelled (atom nil)]
                     (with-redefs
                       [vis/cancel! (fn [tok]
                                      (reset! cancelled tok))]
                       (let
                         [db {:active-tab-id :main
                              :session {:id "s1"}
                              :loading? true
                              :cancel-token :token
                              :cancelling? true
                              :progress {:iterations []}
                              :turn-start-ms 10
                              :cancelling-at-ms 1000}
                          ;; 8.5s elapsed > 8s timeout
                          {db' :db fx :fx} (heal-fn db [:cancel-self-heal-tick 9500])]

                         ;; The pure handler schedules the local interrupt before network I/O.
                         (expect (= [:cancel-local-turn :token] (first fx)))
                         ;; Turn state fully cleared → input flows again.
                         (expect (false? (:cancelling? db')))
                         (expect (false? (:loading? db')))
                         (expect (nil? (:cancel-token db')))
                         (expect (nil? (:cancelling-at-ms db')))
                         ;; The user is told, and with no authored backlog nothing is restored.
                         (expect (some #(= :notify (first %)) fx))
                         (expect (not-any? #(= :dispatch (first %)) fx))))))
               (it "restores the authored backlog to the editor when it self-heals"
                   (with-redefs
                     [vis/cancel! (fn [_]
                                    nil)]
                     (let
                       [db {:active-tab-id :main
                            :session {:id "s1"}
                            :loading? true
                            :cancel-token :token
                            :cancelling? true
                            :cancelling-at-ms 0
                            :pending-sends [{:text "my correction" :client-id "c1" :mine? true}]}
                        {fx :fx} (heal-fn db [:cancel-self-heal-tick 20000])]

                       ;; The correction the user typed during the cancel comes back
                       ;; to the editor rather than being silently dropped.
                       (expect (some #{[:dispatch [:restore-pending-to-input :main]]} fx)))))
               (it "never fires when no cancel is pending, even with a stale timestamp"
                   (with-redefs
                     [vis/cancel! (fn [_]
                                    (throw (ex-info "self-heal must not fire when idle" {})))]
                     (let
                       [db
                        {:active-tab-id :main :loading? true :cancelling? false :cancelling-at-ms 0}
                        {db' :db} (heal-fn db [:cancel-self-heal-tick 999999])]

                       (expect (false? (:cancelling? db')))
                       (expect (true? (:loading? db'))))))))

(defdescribe session-refresh-reconciles-in-flight-state-test
             (it "clears stale cancelling state when refreshed session is terminal"
                 (reset! state/app-db {:session {:id "s1"}
                                       :loading? true
                                       :cancelling? true
                                       :cancel-token :token
                                       :gateway-turn-id "turn-1"
                                       :progress {:iterations []}
                                       :turn-start-ms 10
                                       :render-version 0})
                 (state/dispatch [:init-session {:id "s1" :status "idle"}
                                  [{:role :user :text "cancelled"}
                                   {:role :assistant :text "interrupted" :status :interrupted}]
                                  {:root "/tmp"}])
                 (let [db @state/app-db]
                   (expect (false? (:loading? db)))
                   (expect (false? (:cancelling? db)))
                   (expect (nil? (:cancel-token db)))
                   (expect (nil? (:gateway-turn-id db)))
                   (expect (nil? (:progress db)))
                   (expect (nil? (:turn-start-ms db)))))
             (it "preserves active turn state when refreshed session is still running"
                 (reset! state/app-db {:session {:id "s1"}
                                       :loading? true
                                       :cancelling? true
                                       :cancel-token :token
                                       :gateway-turn-id "turn-1"
                                       :progress {:iterations [{:status :running}]}
                                       :turn-start-ms 10
                                       :render-version 0})
                 (state/dispatch [:init-session
                                  {:id "s1" :status "running" :current_turn_id "turn-1"}
                                  [{:role :user :text "running"} {:role :assistant :pending? true}]
                                  {:root "/tmp"}])
                 (let [db @state/app-db]
                   (expect (true? (:loading? db)))
                   (expect (true? (:cancelling? db)))
                   (expect (= :token (:cancel-token db)))
                   (expect (= "turn-1" (:gateway-turn-id db)))
                   (expect (= {:iterations [{:status :running}]} (:progress db)))
                   (expect (= 10 (:turn-start-ms db))))))

(defdescribe
  attach-running-turn-canonical-clock-test
  (it "seeds turn-start-ms from the gateway's started_at, not local attach time"
      (with-redefs
        [vis/worker-future
         (fn [_ _]
           (future nil))

         vis/cancellation-set-future!
         (fn [_ _]
           nil)]

        (reset! state/app-db {:session {:id "s1"} :active-tab-id "s1" :render-version 0})
        (state/dispatch [:attach-running-turn nil
                         {:id "s1"
                          :status "running"
                          :current-turn-id "turn-1"
                          :running-request "hello"
                          :running-started-at 12345}])
        (let [db @state/app-db]
          (expect (true? (:loading? db)))
          (expect (= "turn-1" (:gateway-turn-id db)))
          ;; The canonical gateway clock — NOT this process's now — so two
          ;; TUIs attached to the same running turn show the SAME elapsed.
          (expect (= 12345 (:turn-start-ms db))))))
  (it "falls back to the local clock when the gateway timestamp is missing"
      (with-redefs
        [vis/worker-future
         (fn [_ _]
           (future nil))

         vis/cancellation-set-future!
         (fn [_ _]
           nil)]

        (reset! state/app-db {:session {:id "s1"} :active-tab-id "s1" :render-version 0})
        (let [before (System/currentTimeMillis)]
          (state/dispatch
            [:attach-running-turn nil
             {:id "s1" :status "running" :current-turn-id "turn-1" :running-request "hello"}])
          (expect (<= before (long (:turn-start-ms @state/app-db)))))))
  (it "does not leave the turn it attaches as running ALSO showing under Queued"
      ;; Regression: the backlog mirror seeds :pending-sends from the session's
      ;; :queued-turns snapshot, which can still list the turn that has since
      ;; started. Attaching that turn as running must strip it from the queue so
      ;; it paints once (live) and not a second time as "Queued"; the genuinely
      ;; queued sibling turn stays.
      (with-redefs
        [vis/worker-future
         (fn [_ _]
           (future nil))

         vis/cancellation-set-future!
         (fn [_ _]
           nil)]

        (reset! state/app-db {:session {:id "s1"}
                              :active-tab-id "s1"
                              ;; Mirrored while it was still queued (gateway truth,
                              ;; so it carries the gateway turn id) - it is the
                              ;; running turn now, not a second queued one.
                              :pending-sends [{:text "hello" :turn-id "turn-1"}]
                              :render-version 0})
        (state/dispatch [:attach-running-turn "s1"
                         {:id "s1"
                          :status "running"
                          :current-turn-id "turn-1"
                          :running-request "hello"
                          :queued-turns [{:turn-id "turn-1" :text "hello" :queued-at-ms 1}
                                         {:turn-id "turn-2" :text "world" :queued-at-ms 2}]}])
        (let [db @state/app-db]
          (expect (= "turn-1" (:gateway-turn-id db)))
          ;; Both the stale gateway snapshot and its unbound local echo are gone;
          ;; only the genuinely queued sibling remains.
          (expect (= ["turn-2"] (mapv :turn-id (:pending-sends db))))
          (expect (not (some #(= "hello" (:text %)) (:pending-sends db)))))))
  (it "never seeds the tab's own LIVE turn from a stale queued-turns snapshot"
      ;; Cross-validation: the backlog seed and :sync-queued-turn must share ONE
      ;; rule. A tab already attached to turn-1 re-attaches (tab reopen / project
      ;; switch) with a snapshot taken while turn-1 was still queued; the seed runs
      ;; even though this branch does not re-attach, so without the shared
      ;; `live-turn-mirror?` gate it painted the running turn as Queued.
      (reset! state/app-db {:session {:id "s1"}
                            :active-tab-id "s1"
                            :loading? true
                            :gateway-turn-id "turn-1"
                            :pending-sends []
                            :render-version 0})
      (state/dispatch [:attach-running-turn "s1"
                       {:id "s1"
                        :status "running"
                        :current-turn-id "turn-1"
                        :running-request "hello"
                        :queued-turns [{:turn-id "turn-1" :text "hello" :queued-at-ms 1}
                                       {:turn-id "turn-2" :text "world" :queued-at-ms 2}]}])
      (expect (= ["turn-2"] (mapv :turn-id (:pending-sends @state/app-db))))))

(defdescribe
  attach-running-turn-drains-idle-queue-test
  ;; Auto-start on open/resume (option a): when a tab attaches to an IDLE
  ;; session that still carries a server-side queued backlog (left queued by a
  ;; cancel, or submitted from a sibling channel while we were away), the
  ;; handler must kick the daemon to start the head turn RIGHT AWAY via the
  ;; :drain-idle-queue fx — not leave it sitting invisibly queued.
  (it "fires :drain-idle-queue for an idle session with a queued backlog"
      (let [drained (atom nil)]
        (with-redefs
          [vis/gateway-drain-idle! (fn [sid]
                                     (reset! drained sid))]
          (reset! state/app-db {:session {:id "s1"} :active-tab-id "s1" :render-version 0})
          (state/dispatch
            [:attach-running-turn "s1"
             {:id "s1" :status "idle" :queued-turns [{:turn-id "q1" :text "hi" :queued-at-ms 1}]}])
          (flush-queue-io!)
          (expect (= "s1" @drained)))))
  (it "does not drain when the idle session has no queued backlog"
      (let [drained (atom :unset)]
        (with-redefs
          [vis/gateway-drain-idle! (fn [sid]
                                     (reset! drained sid))]
          (reset! state/app-db {:session {:id "s1"} :active-tab-id "s1" :render-version 0})
          (state/dispatch [:attach-running-turn "s1" {:id "s1" :status "idle" :queued-turns []}])
          (expect (= :unset @drained))))))

(defn- inert-schedule!
  "Deterministic stand-in for the real trailing-flush timer.

   Cadence assertions below only care about the SYNCHRONOUS dispatch decisions,
   so the scheduled trailing flush must never fire on a background thread: the
   shared `progress-trailing-flush-scheduler` raced them on slow CI hosts and
   painted an extra (harmless in production, fatal to an `=`) duplicate frame."
  [_f _delay-ms]
  (reify
    java.util.concurrent.Future
      (cancel [_ _] true)
      (isCancelled [_] false)
      (isDone [_] false)
      (get [_] nil)
      (get [_ _ _] nil)))

(defdescribe
  live-progress-rate-test
  (it "coalesces reasoning redraws to the 80ms frame cadence and flushes lifecycle chunks"
      (let
        [make-progress-render-updater
         @#'state/make-progress-render-updater

         events
         (atom [])

         now-ms
         (atom 0)

         update!
         (make-progress-render-updater #(swap! events conj %) #(long @now-ms) inert-schedule!)]

        (update! [:t0] {:phase :reasoning})
        (reset! now-ms 79)
        (update! [:t79] {:phase :reasoning})
        (reset! now-ms 80)
        (update! [:t80] {:phase :reasoning})
        (reset! now-ms 81)
        (update! [:done] {:phase :iteration-final})
        (expect (= [[:set-progress-iterations [:t0]] [:set-progress-iterations [:t80]]
                    [:set-progress-iterations [:done]]]
                   @events))))
  (it "content stream CANNOT starve reasoning frames — each phase keeps its own throttle clock"
      ;; Regression: before the per-phase clocks, every `:content`
      ;; chunk (which streams per-token alongside `:reasoning`)
      ;; reset the shared throttle, so after the first reasoning
      ;; frame landed the bubble froze on "I" / "The" until the
      ;; terminal `:iteration-final` chunk.
      (let
        [make-progress-render-updater
         @#'state/make-progress-render-updater

         events
         (atom [])

         now-ms
         (atom 0)

         update!
         (make-progress-render-updater #(swap! events conj %) #(long @now-ms) inert-schedule!)]

        ;; First reasoning frame lands.
        (update! [:r 0] {:phase :reasoning})
        ;; Content begins streaming per-token, every 10ms.
        (doseq [t (range 10 80 10)]
          (reset! now-ms t)
          (update! [:c (long t)] {:phase :content}))
        ;; 80ms after the first reasoning frame, a new reasoning
        ;; chunk MUST dispatch — content traffic must not have
        ;; reset the reasoning throttle clock.
        (reset! now-ms 80)
        (update! [:r 80] {:phase :reasoning})
        (let [reasoning-events (filterv #(= :r (first (second %))) @events)]
          (expect (= [[:set-progress-iterations [:r 0]] [:set-progress-iterations [:r 80]]]
                     reasoning-events)))))
  (it "content stream is throttled on its own clock and never blocks reasoning"
      (let
        [make-progress-render-updater
         @#'state/make-progress-render-updater

         events
         (atom [])

         now-ms
         (atom 0)

         update!
         (make-progress-render-updater #(swap! events conj %) #(long @now-ms) inert-schedule!)]

        ;; Hammer both streams in lockstep for 200ms.
        (doseq [t (range 0 201 10)]
          (reset! now-ms t)
          (update! [:r (long t)] {:phase :reasoning})
          (update! [:c (long t)] {:phase :content}))
        (let
          [tag-counts (reduce (fn [m [_ tag]]
                                (update m (first tag) (fnil inc 0)))
                              {}
                              @events)]
          ;; 200ms / 80ms cadence → frames at t ∈ {0, 80, 160}.
          (expect (= 3 (get tag-counts :r)))
          (expect (= 3 (get tag-counts :c)))))))

(defdescribe
  reasoning-sentence-buffer-test
  (let
    [clip
     #'state/clip-reasoning-to-sentence

     clip-live
     #'state/clip-live-reasoning]

    (it "holds a short boundary-less partial back (no 1-2 char leading stub)"
        (expect (= "" (clip "I" 200)))
        (expect (= "" (clip "I thi" 200))))
    (it "reveals up to (and including) the last sentence boundary"
        (expect (= "I think so." (clip "I think so. And ne" 200)))
        (expect (= "One. Two!" (clip "One. Two! Thr" 200))))
    (it "keeps trailing closing punctuation with the boundary"
        (expect (= "He said \"go.\"" (clip "He said \"go.\" Then" 200))))
    (it "escape hatch: a long boundary-less tail is revealed whole"
        (let [s (apply str (repeat 250 "x"))]
          (expect (= s (clip s 200))))
        ;; boundary present but a very long partial after it → reveal all
        (let [s (str "Ok. " (apply str (repeat 250 "y")))]
          (expect (= s (clip s 200)))))
    (it "empty / nil stays empty" (expect (= "" (clip "" 200))) (expect (= "" (clip nil 200))))
    (it "clip-live only touches entries still streaming reasoning"
        (let
          [streaming
           {:iteration 0 :thinking "I think so. And mo" :forms [] :done? false :final nil}

           with-form
           {:iteration 0 :thinking "I think so. And mo" :forms [{:code "x"}] :done? false}

           done
           {:iteration 0 :thinking "I think so. And mo" :forms [] :done? true :final :ok}]

          ;; live streaming entry → clipped to the last sentence
          (expect (= "I think so." (:thinking (first (clip-live [streaming])))))
          ;; a form has landed → full thinking revealed
          (expect (= "I think so. And mo" (:thinking (first (clip-live [with-form])))))
          ;; iteration finished → full thinking revealed
          (expect (= "I think so. And mo" (:thinking (first (clip-live [done])))))
          ;; non-map timeline entries pass through untouched
          (expect (= [:t0] (clip-live [:t0])))))))

(defdescribe
  live-progress-trailing-flush-test
  ;; Regression: leading-edge-only throttling pinned the live
  ;; bubble on the FIRST reasoning frame ("I" / "The") for the
  ;; entire duration of a server-side stall between the end of a
  ;; short reasoning burst and the start of the content stream.
  ;; The model finishes reasoning fast (within the 80ms window after
  ;; the first dispatched chunk), then the provider takes 5-30s to
  ;; emit the first content delta. No chunks fire during the stall,
  ;; so app-db's :progress slot stays on the first frame and the
  ;; spinner ticks repaint stale text. Trailing-edge flush guarantees
  ;; the latest dropped timeline reaches the screen within one
  ;; throttle interval even when the stream goes quiet.
  (it
    "flushes the latest dropped timeline within the throttle window when the stream stalls"
    (let
      [make-progress-render-updater
       @#'state/make-progress-render-updater

       events
       (atom [])

       now-ms
       (atom 0)

       scheduled
       (atom [])

       schedule-fn
       (fn [^Runnable f ^long delay-ms]
         (let [token (gensym "sched")]
           (swap! scheduled conj {:token token :run f :delay-ms delay-ms})
           token))

       update!
       (make-progress-render-updater #(swap! events conj %) #(long @now-ms) schedule-fn)]

      ;; First reasoning chunk lands and dispatches.
      (update! [:t 0] {:phase :reasoning})
      ;; Burst of 4 more chunks, all inside the 80ms window → dropped
      ;; but stashed as pending; the FIRST drop schedules the timer,
      ;; subsequent drops only overwrite the pending timeline.
      (doseq [t [10 20 30 40]]
        (reset! now-ms t)
        (update! [:t (long t)] {:phase :reasoning}))
      ;; Exactly one trailing flush should be scheduled.
      (expect (= 1 (count @scheduled)))
      (let [{:keys [delay-ms run]} (first @scheduled)]
        ;; Delay is `interval - elapsed` from the dispatch at t=0.
        ;; First drop at t=10 → delay-ms = 80 - (10 - 0) = 70.
        (expect (= 70 delay-ms))
        ;; Stream stalls. Advance virtual clock past the schedule and
        ;; fire the timer manually.
        (reset! now-ms 80)
        (run))
      ;; Trailing flush carries the LATEST pending timeline ([:t 40]),
      ;; not the first dispatched one.
      (expect (= [[:set-progress-iterations [:t 0]] [:set-progress-iterations [:t 40]]] @events))))
  (it
    "a fresh chunk arriving on the trailing edge cancels the scheduled flush"
    (let
      [make-progress-render-updater
       @#'state/make-progress-render-updater

       events
       (atom [])

       now-ms
       (atom 0)

       scheduled
       (atom [])

       cancelled
       (atom 0)

       schedule-fn
       (fn [^Runnable f ^long delay-ms]
         (let
           [fut (reify
                  java.util.concurrent.Future
                    (cancel [_ _] (swap! cancelled inc) true)
                    (isCancelled [_] false)
                    (isDone [_] false)
                    (get [_] nil)
                    (get [_ _ _] nil))]
           (swap! scheduled conj {:run f :delay-ms delay-ms :fut fut})
           fut))

       update!
       (make-progress-render-updater #(swap! events conj %) #(long @now-ms) schedule-fn)]

      (update! [:t 0] {:phase :reasoning})
      (reset! now-ms 30)
      (update! [:t 30] {:phase :reasoning}) ;; dropped → schedules flush
      ;; Time crosses the 80ms boundary, next chunk is due. The
      ;; dispatch must cancel the trailing-edge timer so it does
      ;; not fire a duplicate frame afterwards.
      (reset! now-ms 90)
      (update! [:t 90] {:phase :reasoning})
      (expect (= 1 @cancelled))
      (expect (= [[:set-progress-iterations [:t 0]] [:set-progress-iterations [:t 90]]] @events))))
  (it "a lifecycle chunk arriving between drops cancels the trailing flush"
      ;; Lifecycle chunks (`:iteration-final`, `:form-result`, …)
      ;; bypass the throttle and ALWAYS dispatch. They also carry the
      ;; latest cumulative timeline, so the trailing flush would just
      ;; produce a duplicate frame. Today the lifecycle dispatch does
      ;; NOT touch the per-phase throttle clocks (preserves the
      ;; per-phase isolation), but the pending trailing flush MUST
      ;; still no-op because the latest pending state was published.
      ;; This test pins the desired behavior: the pending slot is
      ;; cleared the moment the dispatched lifecycle delivers it.
      (let
        [make-progress-render-updater
         @#'state/make-progress-render-updater

         events
         (atom [])

         now-ms
         (atom 0)

         scheduled
         (atom [])

         schedule-fn
         (fn [^Runnable f ^long delay-ms]
           (let [t (gensym)]
             (swap! scheduled conj {:token t :run f :delay-ms delay-ms})
             t))

         update!
         (make-progress-render-updater #(swap! events conj %) #(long @now-ms) schedule-fn)]

        (update! [:r 0] {:phase :reasoning})
        (reset! now-ms 20)
        (update! [:r 20] {:phase :reasoning}) ;; dropped + scheduled
        ;; Lifecycle event fires (e.g. response-parse :start).
        (reset! now-ms 25)
        (update! [:r 25] {:phase :response-parse}) ;; ALWAYS dispatched
        ;; Trailing flush still fires later (we do not cancel from a
        ;; non-throttled path — cheap), but with a stale-but-still-latest
        ;; pending it just dispatches the same shape again. That is
        ;; acceptable because the render loop coalesces by version.
        ;; Hard contract: between [r 0] and [r 25] the lifecycle
        ;; chunk DID deliver the latest pending shape immediately.
        (expect (= [[:set-progress-iterations [:r 0]] [:set-progress-iterations [:r 25]]]
                   @events)))))

(defdescribe
  send-message-test
  (it
    "does not send reasoning effort or verbosity for Z.ai fixed-thinking models"
    (let
      [send-message-fn
       (-> #'state/event-registry
           deref
           deref
           (get :send-message)
           :fn)

       db
       {:session {:id "c1"}
        :active-tab-id :main
        :messages []
        :messages-scroll 0
        :input-history []
        :settings {:reasoning-level "deep" :openai-codex-verbosity "high"}
        :pastes {}}]

      (with-redefs
        [input/expand-paste-placeholders
         (fn [text _]
           text)

         input/expand-file-mentions
         identity

         vis/cancellation-token
         (fn []
           :token)

         vis/get-router
         (fn []
           :router)

         vis/resolve-effective-model
         (fn [_]
           {:provider :zai
            :name "glm-4.7"
            :reasoning? true
            :reasoning-style :zai-thinking
            :reasoning-effort? false})]

        (let
          [{:keys [fx]}
           (send-message-fn db [:send-message "hello"])

           [event]
           fx]

          (expect (= [:session-turn :main {:id "c1"} "hello" :token nil nil {}] (subvec event 0 8)))
          (expect (nil? (nth event 8)))
          (expect (string? (nth event 9)))))))
  (it
    "forwards routing trace from turn result to message metadata"
    (let
      [session-turn-fx
       (-> #'state/fx-registry
           deref
           deref
           (get :session-turn))

       received
       (atom [])

       trace
       [{"provider_id" "p1" "model" "m1" "status" 429 "reason" "transient_error"}]]

      (with-redefs
        [vis/worker-future
         (fn [_label thunk]
           (thunk)
           :future)

         vis/cancellation-set-future!
         (fn [_token _future])

         state/dispatch
         (fn [event]
           (swap! received conj event))

         chat/turn!
         (fn [_session _text _opts]
           {"content" [{"id" "b1" "type" "prose" "markdown" "ok"}]
            "model" "m2"
            "provider" "p2"
            "llm_selected" {"provider" "p1" "model" "m1"}
            "llm_actual" {"provider" "p2" "model" "m2"}
            "is_llm_fallback" true
            "llm_routing_trace" trace})]

        (session-turn-fx :main {:id "c1"} "hello" :token nil nil {} {} "turn-1")
        ;; The turn also dispatches workspace re-sync + live F2 ctx-panel
        ;; refreshes after the answer commits, so don't assume
        ;; :message-received is the *last* event — select it explicitly.
        (let
          [[event-id workspace-id _answer metadata] (->> @received
                                                         (filter #(= :message-received (first %)))
                                                         last)]
          (expect (= :message-received event-id))
          (expect (= :main workspace-id))
          (expect (= "m2" (:model metadata)))
          (expect (= "p2" (:provider metadata)))
          (expect (= {"provider" "p1" "model" "m1"} (:llm-selected metadata)))
          (expect (= {"provider" "p2" "model" "m2"} (:llm-actual metadata)))
          (expect (true? (:llm-fallback? metadata)))
          (expect (= trace (:llm-routing-trace metadata)))))))
  (it
    "restores a cancelled prompt to the input instead of rendering a cancelled answer"
    (let
      [send-message-fn
       (-> #'state/event-registry
           deref
           deref
           (get :send-message)
           :fn)

       reset-input-fn
       (-> #'state/event-registry
           deref
           deref
           (get :reset-input)
           :fn)

       message-received-fn
       (-> #'state/event-registry
           deref
           deref
           (get :message-received)
           :fn)

       token
       (input/format-paste-placeholder {:id 1 :content "hello"})

       text
       (str "edit me " token)

       initial-messages
       [{:role :assistant :text "previous"}]

       db
       {:session {:id "c1"}
        :messages initial-messages
        :messages-scroll 9
        :input-history ["prior"]
        :input-history-index nil
        :input-history-draft nil
        :settings {:reasoning-level "balanced" :openai-codex-verbosity "low"}
        :pastes {1 {:id 1 :content "hello"}}
        :paste-counter 1}]

      (with-redefs
        [vis/cancellation-token (fn []
                                  :token)]
        (let
          [sent-db (:db (send-message-fn db [:send-message text]))
           reset-db (reset-input-fn sent-db [:reset-input])
           restored-db (:db (message-received-fn reset-db
                                                 [:message-received
                                                  [:ast {} [:p {} [:span {} "Cancelled by user."]]]
                                                  {:status :cancelled}]))]

          (expect (= initial-messages (:messages restored-db)))
          (expect (= text (input/input->text (:input restored-db))))
          (expect (= {1 {:id 1 :content "hello"}} (:pastes restored-db)))
          (expect (= 1 (:paste-counter restored-db)))
          (expect (= ["prior"] (:input-history restored-db)))
          (expect (false? (:loading? restored-db)))
          (expect (not-any? #(= "Cancelled by user." (:text %)) (:messages restored-db))))))))

(defdescribe
  gateway-disconnect-reattach-test
  (let
    [session-turn-fx
     (get @@#'state/fx-registry :session-turn)

     session-attach-fx
     (get @@#'state/fx-registry :session-attach)

     reattach-fn
     (:fn (get @@#'state/event-registry :reattach-disconnected-turn))

     session
     {:id "session-1"}

     token
     (Object.)

     disconnect
     (ex-info "SSE disconnected" {:gateway-disconnected true :turn-id "turn-1"})]

    (it "reattaches a submitted turn instead of rendering a false terminal error"
        (let [events (atom [])]
          (with-redefs
            [vis/worker-future (fn [_ thunk]
                                 (thunk)
                                 :future)
             vis/cancellation-set-future! (fn [_ _])
             state/dispatch #(swap! events conj %)
             chat/turn! (fn [& _]
                          (throw disconnect))]

            (session-turn-fx :main session "hello" token nil nil {} {} "client-1")
            (expect (= [[:reattach-disconnected-turn :main session "turn-1" token "client-1"]]
                       @events)))))
    (it "reattaches again when an attach stream disconnects"
        (let [events (atom [])]
          (with-redefs
            [vis/worker-future (fn [_ thunk]
                                 (thunk)
                                 :future)
             vis/cancellation-set-future! (fn [_ _])
             state/dispatch #(swap! events conj %)
             chat/attach! (fn [& _]
                            (throw disconnect))]

            (session-attach-fx :main session "turn-1" token "client-1")
            (expect (= [[:reattach-disconnected-turn :main session "turn-1" token "client-1"]]
                       @events)))))
    (it
      "reattaches only while the same tab turn is still live"
      (let [db {:active-tab-id :main :loading? true :cancel-token token :gateway-turn-id "turn-1"}]
        (with-redefs [vis/cancellation-token (constantly :next-token)]
          (let
            [accepted
             (reattach-fn db [:reattach-disconnected-turn :main session "turn-1" token "client-1"])
             stale (reattach-fn db
                                [:reattach-disconnected-turn :main session "turn-1" (Object.)
                                 "client-1"])]

            (expect (= [[:session-attach :main session "turn-1" :next-token "client-1"]]
                       (:fx accepted)))
            (expect (= :next-token (:cancel-token (:db accepted))))
            (expect (nil? (:fx stale)))))))))

(defdescribe
  pending-send-queue-test
  (it "registers a busy-tab submission with the gateway and shows it in the queue"
      ;; The gateway stays the queue of RECORD — the submission goes out as a real
      ;; queued turn, with its paste snapshot — while the row is painted locally at
      ;; once, and reconciled from gateway truth (ack / broadcast, both through
      ;; `:sync-queued-turn`) into the SAME row.
      (let
        [enqueue-fn
         (-> #'state/event-registry
             deref
             deref
             (get :enqueue-message)
             :fn)

         db
         {:active-tab-id :b
          :input-history []
          :pastes {}
          :paste-counter 0
          :tab-locals {:a {:session {:id "a"}
                           :loading? true
                           :pending-sends []
                           :input-history []
                           :pastes {1 {:id 1 :content "payload"}}
                           :paste-counter 1}}}

         result
         (enqueue-fn db [:enqueue-message "queued" :a])

         queued
         (get-in result [:db :tab-locals :a :pending-sends])]

        (expect (= ["queued"] (mapv :text queued)))
        ;; It belongs to tab :a, not to the ACTIVE tab's root state.
        (expect (empty? (:pending-sends (:db result))))
        (let
          [gw
           (first (filter #(= :gateway-enqueue (first %)) (:fx result)))

           entry
           (nth gw 3)]

          ;; The submission (with its paste snapshot) travels to the gateway, not
          ;; into a local queue.
          (expect (= :a (nth gw 1)))
          (expect (= "queued" (:text entry)))
          (expect (= {1 {:id 1 :content "payload"}} (:pastes entry))))))
  (it "paints ONE visible queued row on Enter and reconciles the gateway ack into it"
      ;; REGRESSION (real session): a message submitted while a turn was running got
      ;; registered with the gateway but NOTHING appeared in the TUI — the channel
      ;; painted no row until the ack came back. Invisible reads as swallowed: the user
      ;; pressed Enter again and the SAME text landed as TWO queued gateway turns 7 ms
      ;; apart, both drained, both answered, the second one paid for. So the row is
      ;; painted on Enter and gateway truth is merged INTO it, by correlation id.
      (let
        [registry
         (-> #'state/event-registry
             deref
             deref)

         enqueue-fn
         (:fn (get registry :enqueue-message))

         sync-fn
         (:fn (get registry :sync-queued-turn))

         drain-fn
         (:fn (get registry :drain-pending))

         gw-fx
         (fn [result]
           (filterv #(= :gateway-enqueue (first %)) (:fx result)))

         rows
         (fn [db]
           (get-in db [:tab-locals :a :pending-sends]))

         db
         {:active-tab-id :b
          :input-history []
          :pastes {}
          :paste-counter 0
          :tab-locals {:a {:session {:id "a"}
                           :loading? true
                           :pending-sends []
                           :input-history []
                           :pastes {}
                           :paste-counter 0}}}

         first-result
         (enqueue-fn db [:enqueue-message "double tap" :a])

         second-result
         (enqueue-fn (:db first-result) [:enqueue-message "double tap" :a])

         row
         (first (rows (:db first-result)))

         client-id
         (:client-id (nth (first (gw-fx first-result)) 3))]

        ;; VISIBLE the instant Enter is pressed: one queued row, ours, still awaiting
        ;; the gateway ack (so it carries no turn id yet).
        (expect (= ["double tap"] (mapv :text (rows (:db first-result)))))
        (expect (true? (:mine? row)))
        (expect (true? (:awaiting-ack? row)))
        (expect (nil? (:turn-id row)))
        ;; The first Enter registers exactly one gateway turn …
        (expect (= 1 (count (gw-fx first-result))))
        ;; … and the second, still inside the round-trip, registers neither a turn nor
        ;; a second row.
        (expect (empty? (gw-fx second-result)))
        (expect (= 1 (count (rows (:db second-result)))))
        ;; A row still in flight is NEVER drained locally: that would send the same
        ;; text twice, once from here and once as the queued turn being confirmed.
        (let [idle (assoc-in (:db first-result) [:tab-locals :a :loading?] false)]
          (expect (empty? (:fx (drain-fn idle [:drain-pending :a])))))
        ;; The ack carries the correlation id we sent as the idempotency key, so it
        ;; upgrades THAT row rather than appending a second one.
        (let
          [acked
           (sync-fn (:db first-result)
                    [:sync-queued-turn :a
                     {:op :add :turn-id "t-9" :client-id client-id :text "double tap"}])

           acked-row
           (first (rows acked))]

          (expect (string? client-id))
          (expect (= 1 (count (rows acked))))
          (expect (= "t-9" (:turn-id acked-row)))
          (expect (nil? (:awaiting-ack? acked-row))))))
  (it "lets a DELIBERATE repeat queue again — only the same KEYPRESS is dropped"
      ;; The guard has to stay NARROW. The submit path clears the editor on Enter
      ;; unconditionally, so swallowing a submission deletes the user's text with nothing
      ;; on screen to explain it — the same invisibility that caused the double queue,
      ;; pointed the other way. Repeating yourself ("continue", "yes", the same nudge) is
      ;; ordinary once the first row is visible and acked, and must go out.
      (let
        [enqueue-fn
         (-> #'state/event-registry
             deref
             deref
             (get :enqueue-message)
             :fn)

         db
         {:active-tab-id :b
          :input-history []
          :pastes {}
          :paste-counter 0
          :tab-locals {:a {:session {:id "a"}
                           :loading? true
                           ;; acked long ago: turn id bound, nothing in flight
                           :pending-sends [{:text "continue"
                                            :client-id "c-1"
                                            :turn-id "t-1"
                                            :mine? true
                                            :queued-at-ms (- (System/currentTimeMillis) 60000)}]
                           :input-history []
                           :pastes {}
                           :paste-counter 0}}}

         result
         (enqueue-fn db [:enqueue-message "continue" :a])]

        (expect (= 1 (count (filterv #(= :gateway-enqueue (first %)) (:fx result)))))
        (expect (= ["continue" "continue"]
                   (mapv :text (get-in result [:db :tab-locals :a :pending-sends]))))))
  (it "SAYS so when it drops an identical keypress"
      ;; A silent drop is indistinguishable from a swallowed message — exactly the bug
      ;; this whole path exists to prevent. Suppression must be visible.
      (let
        [enqueue-fn
         (-> #'state/event-registry
             deref
             deref
             (get :enqueue-message)
             :fn)

         db
         {:active-tab-id :b
          :input-history []
          :pastes {}
          :paste-counter 0
          :tab-locals {:a {:session {:id "a"}
                           :loading? true
                           :pending-sends
                           [{:text "double tap" :client-id "c-1" :mine? true :awaiting-ack? true}]
                           :input-history []
                           :pastes {}
                           :paste-counter 0}}}

         result
         (enqueue-fn db [:enqueue-message "double tap" :a])]

        (expect (empty? (filterv #(= :gateway-enqueue (first %)) (:fx result))))
        (expect (= 1 (count (get-in result [:db :tab-locals :a :pending-sends]))))
        (expect (some #(= :notify (first %)) (:fx result)))))
  (it "records a retraction the ack can act on, and infers NOTHING from a missing row"
      ;; A cancel (`:restore-pending-to-input`) or `:clear-pending-sends` drops a row the
      ;; gateway has not NAMED yet — its turn id is still being minted inside the open
      ;; POST — so the delete has to wait for the ack, and the correlation id is recorded
      ;; for it. Absence alone must never count as retraction: a row also vanishes when
      ;; its TAB closes, and that path deliberately re-submits the text under the same
      ;; idempotency key, so deleting there would destroy a message the user still wants.
      (let
        [retracted?
         #'state/submission-retracted?

         mark
         #'state/mark-retracted

         db
         (fn [w]
           {:active-tab-id :b :tab-locals {:a w}})]

        (expect (false? (retracted? (db {:pending-sends []}) :a "c-1")))
        (expect (true?
                  (retracted? (db (mark {} [{:client-id "c-1" :awaiting-ack? true}])) :a "c-1")))
        ;; a row the gateway already named is deleted by turn id, never recorded here
        (expect (false? (retracted? (db (mark {} [{:client-id "c-1" :turn-id "t-1"}])) :a "c-1")))
        ;; someone else's correlation id is not ours
        (expect (false? (retracted? (db (mark {} [{:client-id "c-2"}])) :a "c-1")))
        ;; no db proves nothing — never delete on a guess
        (expect (false? (retracted? nil :a "c-1")))))
  (it
    "a cancel and an explicit clear both record their un-named rows as retracted"
    ;; Through the real events: the row leaves the queue AND its correlation id is left
    ;; behind, so the enqueue ack can delete the turn the gateway is about to name.
    (let
      [registry
       (-> #'state/event-registry
           deref
           deref)

       run
       (fn [event-id db]
         ((:fn (get registry event-id)) db [event-id :a]))

       db
       {:active-tab-id :a
        :session {:id "s"}
        :input (input/empty-input)
        :input-history []
        :pastes {}
        :paste-counter 0
        :pending-sends [{:text "take it back" :client-id "c-1" :mine? true :awaiting-ack? true}]}]

      (doseq [event-id [:restore-pending-to-input :clear-pending-sends]]
        (let [{db' :db} (run event-id db)]
          (expect (= [] (vec (:pending-sends db'))))
          (expect (= ["c-1"] (vec (:retracted-sends db'))))))))
  (it "never queues a submission while a cancel is in flight (:cancelling?)"
      ;; REGRESSION: pressing Esc to cancel, then typing a new message, parked that
      ;; message in the queue (`:pending-sends`) behind the turn being torn down —
      ;; "I cancel and write something else and I get it in the queue". A submission
      ;; during the cancel window is a FRESH intent: it must NOT be queued (and must
      ;; never fire :gateway-enqueue). The submit path keeps the text in the editor.
      (let
        [enqueue-fn
         (-> #'state/event-registry
             deref
             deref
             (get :enqueue-message)
             :fn)

         db
         {:active-tab-id :a
          :input-history []
          :pastes {}
          :paste-counter 0
          :tab-locals {:a {:session {:id "a"}
                           :loading? true
                           :cancelling? true
                           :pending-sends []
                           :input-history []
                           :pastes {}
                           :paste-counter 0}}}

         result
         (enqueue-fn db [:enqueue-message "typed during cancel" :a])]

        ;; Nothing lands in the queue …
        (expect (empty? (get-in result [:db :tab-locals :a :pending-sends])))
        ;; … and no server-side queued turn is registered.
        (expect (not-any? #(= :gateway-enqueue (first %)) (:fx result)))
        (expect (some #(= :notify (first %)) (:fx result)))))
  (it "schedules queue drain as an effect after message commit"
      (let
        [message-received-fn
         (-> #'state/event-registry
             deref
             deref
             (get :message-received)
             :fn)

         pending-id
         "turn-1"

         db
         {:active-tab-id :main
          :session {:id "c1"}
          :loading? true
          :messages [{:role :user :text "first" :client-turn-id pending-id}
                     {:role :assistant :pending? true :client-turn-id pending-id}]
          :progress {:iterations []}
          :pending-sends [{:text "second" :pastes {} :paste-counter 0}]}

         {:keys [db fx]}
         (message-received-fn db
                              [:message-received :main [:ast {} [:p {} [:span {} "ok"]]]
                               {:client-turn-id pending-id}])]

        (expect (= [[:dispatch [:drain-pending :main]]] fx))
        (expect (false? (:loading? db)))
        (expect (= ["second"] (mapv :text (:pending-sends db))))))
  (it "drains one queued item without nested provider dispatch"
      (let
        [drain-fn
         (-> #'state/event-registry
             deref
             deref
             (get :drain-pending)
             :fn)

         db
         {:active-tab-id :main
          :pending-sends [{:text "second" :pastes {2 {:id 2 :content "p"}} :paste-counter 2}
                          {:text "third" :pastes {} :paste-counter 0}]}

         {:keys [db fx]}
         (drain-fn db [:drain-pending :main])]

        (expect (= [[:dispatch [:send-message "second" :main]]] fx))
        (expect (= ["third"] (mapv :text (:pending-sends db))))
        (expect (= {2 {:id 2 :content "p"}} (:pastes db)))
        (expect (= 2 (:paste-counter db))))))

(defdescribe edit-queued-via-history-up-test
             (it "ArrowUp on an empty box pulls the newest queued message back for editing"
                 (let
                   [history-up-fn
                    (-> #'state/event-registry
                        deref
                        deref
                        (get :history-up)
                        :fn)

                    db
                    {:input-history ["prev-sent"]
                     :input-history-index nil
                     :input (input/empty-input)
                     :pending-sends
                     [{:text "first" :pastes {} :paste-counter 0}
                      {:text "queued msg" :pastes {1 {:id 1 :content "p"}} :paste-counter 1}]}

                    result
                    (:db (history-up-fn db [:history-up]))]

                   (expect (= "queued msg" (input/input->text (:input result))))
                   (expect (= ["first"] (mapv :text (:pending-sends result))))
                   (expect (= {1 {:id 1 :content "p"}} (:pastes result)))
                   (expect (= 1 (:paste-counter result)))))
             (it "ArrowUp with a non-empty box browses input-history, leaving the queue intact"
                 (let
                   [history-up-fn
                    (-> #'state/event-registry
                        deref
                        deref
                        (get :history-up)
                        :fn)

                    db
                    {:input-history ["prev-sent"]
                     :input-history-index nil
                     :input {:lines ["typing…"] :crow 0 :ccol 6}
                     :pending-sends [{:text "queued msg" :pastes {} :paste-counter 0}]}

                    result
                    (:db (history-up-fn db [:history-up]))]

                   (expect (= "prev-sent" (input/input->text (:input result))))
                   (expect (= ["queued msg"] (mapv :text (:pending-sends result))))))
             (it "ArrowUp with an empty box and empty queue browses input-history"
                 (let
                   [history-up-fn
                    (-> #'state/event-registry
                        deref
                        deref
                        (get :history-up)
                        :fn)

                    db
                    {:input-history ["prev-sent"]
                     :input-history-index nil
                     :input (input/empty-input)
                     :pending-sends []}

                    result
                    (:db (history-up-fn db [:history-up]))]

                   (expect (= "prev-sent" (input/input->text (:input result)))))))

(defdescribe
  cancel-restores-pending-to-input-test
  (it "message-received on CANCEL routes the backlog to the editor, not a drain"
      ;; Regression: cancelling a turn with a queued backlog used to
      ;; auto-send (drain) the next message — and that auto-sent turn
      ;; couldn't be cancelled. A cancel must instead restore the queue.
      (let
        [message-received-fn
         (-> #'state/event-registry
             deref
             deref
             (get :message-received)
             :fn)

         pending-id
         "turn-1"

         db
         {:active-tab-id :main
          :session {:id "c1"}
          :loading? true
          :messages [{:role :user :text "first" :client-turn-id pending-id}
                     {:role :assistant :pending? true :client-turn-id pending-id}]
          :progress {:iterations []}
          :submitted-input {:text "first" :pastes {} :paste-counter 0}
          :pending-sends [{:text "second" :pastes {} :paste-counter 0 :client-id "c1" :mine? true}]}

         {:keys [db fx]}
         (message-received-fn db
                              [:message-received :main
                               [:ast {} [:p {} [:span {} "Cancelled by user."]]]
                               {:status :cancelled :client-turn-id pending-id}])]

        (expect (= [[:dispatch [:restore-pending-to-input :main]]] fx))
        (expect (false? (:loading? db)))
        ;; queue survives the commit; the follow-up fx clears + restores it.
        (expect (= ["second"] (mapv :text (:pending-sends db))))))
  (it
    "restores every pristine editor shape without losing submitted metadata"
    (let
      [restore-fns
       [#'state/restore-submitted-input #'state/restore-editor-only]

       submissions
       [{:text "first" :pastes {1 "old paste"} :paste-counter 1}
        {:text "line one\nline two" :pastes {2 "multi"} :paste-counter 7} {:text "defaults"}]]

      (doseq
        [restore
         restore-fns

         submitted
         submissions]

        (let
          [db (restore {:input (input/empty-input)
                        :pastes {99 "stale"}
                        :paste-counter 99
                        :input-history-index 4
                        :input-history-draft "stale draft"
                        :slash-command-index 3
                        :slash-command-hidden? true
                        :submitted-input submitted}
                       submitted)]
          (expect (= (:text submitted) (input/input->text (:input db))))
          (expect (= (or (:pastes submitted) {}) (:pastes db)))
          (expect (= (or (:paste-counter submitted) 0) (:paste-counter db)))
          (expect (nil? (:input-history-index db)))
          (expect (nil? (:input-history-draft db)))
          (expect (= 0 (:slash-command-index db)))
          (expect (false? (:slash-command-hidden? db)))
          (expect (nil? (:submitted-input db)))))))
  (it
    "never overwrites any non-pristine draft while cancellation settles"
    (let
      [submitted
       {:text "old prompt" :pastes {1 "old paste"} :paste-counter 1}

       visible-submitted
       (input/expand-paste-placeholders (:text submitted) (:pastes submitted))

       messages
       [{:role :user :text "old prompt"} {:role :assistant :pending? true}]

       drafts
       [(reduce input/insert-char (input/empty-input) "x")
        (reduce input/insert-char (input/empty-input) "   ") {:lines ["" ""] :crow 1 :ccol 0}
        {:lines ["new" "draft"] :crow 0 :ccol 1}
        (reduce input/insert-char (input/empty-input) "[Paste #9]")]

       editor-meta
       {:pastes {9 "fresh paste"}
        :paste-counter 9
        :input-history-index 2
        :input-history-draft "new history draft"
        :slash-command-index 5
        :slash-command-hidden? true}

       editor-keys
       [:input :pastes :paste-counter :input-history-index :input-history-draft :slash-command-index
        :slash-command-hidden?]]

      (doseq [draft drafts]
        (let
          [base (merge {:input draft
                        :messages messages
                        :input-history ["older" visible-submitted]
                        :submitted-input submitted}
                       editor-meta)
           dropped (#'state/restore-submitted-input base submitted)
           retained (#'state/restore-editor-only base submitted)]

          (expect (= (select-keys base editor-keys) (select-keys dropped editor-keys)))
          (expect (= (select-keys base editor-keys) (select-keys retained editor-keys)))
          (expect (nil? (:submitted-input dropped)))
          (expect (nil? (:submitted-input retained)))
          (expect (empty? (:messages dropped)))
          (expect (= messages (:messages retained)))
          (expect (= ["older"] (:input-history dropped)))
          (expect (= ["older" visible-submitted] (:input-history retained)))))))
  (it "restoration cleanup is independent from whether a newer draft wins"
      (let
        [submitted
         {:text "old" :pastes {} :paste-counter 0}

         messages
         [{:role :user :text "old"} {:role :assistant :pending? true}]

         base
         {:messages messages
          :loading? true
          :cancelling? true
          :turn-start-ms 123
          :input-history ["keep" "old"]
          :submitted-input submitted}]

        (doseq [draft [(input/empty-input) (reduce input/insert-char (input/empty-input) "new")]]
          (let [db (#'state/restore-submitted-input (assoc base :input draft) submitted)]
            (expect (false? (:loading? db)))
            (expect (false? (:cancelling? db)))
            (expect (nil? (:turn-start-ms db)))
            (expect (nil? (:submitted-input db)))
            (expect (empty? (:messages db)))
            (expect (= ["keep"] (:input-history db)))
            (expect (= (if (input/input-empty? draft) "old" "new")
                       (input/input->text (:input db))))))))
  (it
    "an async FX ACK retries against and preserves many concurrent editor updates"
    (let
      [ack-id
       ::blocking-ack-many-edits

       edit-id
       ::concurrent-edit-many

       effect-id
       ::record-winning-edit-state

       entered
       (promise)

       release
       (promise)

       attempts
       (atom 0)

       effects
       (atom [])

       old-db
       @state/app-db

       registry
       @#'state/event-registry

       fx-registry
       @#'state/fx-registry]

      (swap! registry assoc
        ack-id
        {:type :fx
         :fn (fn [db _]
               (when (= 1 (swap! attempts inc)) (deliver entered true) @release)
               {:db (assoc db :acknowledged? true) :fx [[effect-id (:typed db)]]})}
        edit-id
        {:type :db
         :fn (fn [db [_ ch]]
               (update db :typed (fnil conj []) ch))})
      (swap! fx-registry assoc
        effect-id
        (fn [winning-typed]
          (swap! effects conj winning-typed)))
      (reset! state/app-db {:typed []})
      (let [worker (future (state/dispatch [ack-id]))]
        (try (expect (= true (deref entered 1000 ::timeout)))
             (doseq [ch (range 20)]
               (state/dispatch [edit-id ch]))
             (deliver release true)
             (expect (not= ::timeout (deref worker 1000 ::timeout)))
             (expect (< 1 @attempts))
             (expect (true? (:acknowledged? @state/app-db)))
             (expect (= (vec (range 20)) (:typed @state/app-db)))
             (expect (= [(vec (range 20))] @effects))
             (finally (deliver release true)
                      (deref worker 1000 nil)
                      (swap! registry dissoc ack-id edit-id)
                      (swap! fx-registry dissoc effect-id)
                      (reset! state/app-db old-db))))))
  (it
    "a pure-effect FX retries without reverting a concurrent edit and runs once"
    (let
      [fx-event-id
       ::blocking-pure-effect

       edit-id
       ::edit-during-pure-effect

       effect-id
       ::record-pure-effect

       entered
       (promise)

       release
       (promise)

       attempts
       (atom 0)

       effects
       (atom [])

       old-db
       @state/app-db

       registry
       @#'state/event-registry

       fx-registry
       @#'state/fx-registry]

      (swap! registry assoc
        fx-event-id
        {:type :fx
         :fn (fn [db _]
               (when (= 1 (swap! attempts inc)) (deliver entered true) @release)
               {:fx [[effect-id (:draft db)]]})}
        edit-id
        {:type :db
         :fn (fn [db _]
               (assoc db :draft "typed after Esc"))})
      (swap! fx-registry assoc effect-id #(swap! effects conj %))
      (reset! state/app-db {:stable :value})
      (let [worker (future (state/dispatch [fx-event-id]))]
        (try (expect (= true (deref entered 1000 ::timeout)))
             (state/dispatch [edit-id])
             (deliver release true)
             (expect (not= ::timeout (deref worker 1000 ::timeout)))
             (expect (< 1 @attempts))
             (expect (= :value (:stable @state/app-db)))
             (expect (= "typed after Esc" (:draft @state/app-db)))
             (expect (= ["typed after Esc"] @effects))
             (finally (deliver release true)
                      (deref worker 1000 nil)
                      (swap! registry dissoc fx-event-id edit-id)
                      (swap! fx-registry dissoc effect-id)
                      (reset! state/app-db old-db))))))
  (it
    "two simultaneous cancellation ACKs merge with intervening typing"
    (let
      [ack-a
       ::simultaneous-ack-a

       ack-b
       ::simultaneous-ack-b

       edit-id
       ::edit-between-acks

       effect-id
       ::record-simultaneous-ack

       entered-a
       (promise)

       entered-b
       (promise)

       release
       (promise)

       attempts-a
       (atom 0)

       attempts-b
       (atom 0)

       effects
       (atom [])

       old-db
       @state/app-db

       registry
       @#'state/event-registry

       fx-registry
       @#'state/fx-registry

       handler
       (fn [ack-key entered attempts]
         {:type :fx
          :fn (fn [db _]
                (when (= 1 (swap! attempts inc)) (deliver entered true) @release)
                {:db (assoc db ack-key true) :fx [[effect-id ack-key]]})})]

      (swap! registry assoc
        ack-a
        (handler :ack-a? entered-a attempts-a)
        ack-b
        (handler :ack-b? entered-b attempts-b)
        edit-id
        {:type :db
         :fn (fn [db _]
               (assoc db :draft "third prompt"))})
      (swap! fx-registry assoc effect-id #(swap! effects conj %))
      (reset! state/app-db {})
      (let
        [worker-a
         (future (state/dispatch [ack-a]))

         worker-b
         (future (state/dispatch [ack-b]))]

        (try (expect (= true (deref entered-a 1000 ::timeout)))
             (expect (= true (deref entered-b 1000 ::timeout)))
             (state/dispatch [edit-id])
             (deliver release true)
             (expect (not= ::timeout (deref worker-a 1000 ::timeout)))
             (expect (not= ::timeout (deref worker-b 1000 ::timeout)))
             (expect (true? (:ack-a? @state/app-db)))
             (expect (true? (:ack-b? @state/app-db)))
             (expect (= "third prompt" (:draft @state/app-db)))
             (expect (= #{:ack-a? :ack-b?} (set @effects)))
             (expect (= 2 (count @effects)))
             (expect (or (< 1 @attempts-a) (< 1 @attempts-b)))
             (finally (deliver release true)
                      (deref worker-a 1000 nil)
                      (deref worker-b 1000 nil)
                      (swap! registry dissoc ack-a ack-b edit-id)
                      (swap! fx-registry dissoc effect-id)
                      (reset! state/app-db old-db))))))
  (it "restore-pending-to-input appends queued prompts and deletes gateway records"
      (let
        [restore-fn
         (-> #'state/event-registry
             deref
             deref
             (get :restore-pending-to-input)
             :fn)

         db
         {:active-tab-id :main
          :session {:id "c1"}
          :input (input/empty-input)
          :pastes {}
          :paste-counter 0
          :pending-sends [{:text "second" :pastes {} :paste-counter 0 :turn-id "t-2" :mine? true}
                          {:text "third" :pastes {} :paste-counter 0 :turn-id "t-3" :mine? true}]}

         {:keys [db fx]}
         (restore-fn db [:restore-pending-to-input :main])]

        (expect (= "second\n\nthird" (input/input->text (:input db))))
        (expect (empty? (:pending-sends db)))
        (expect (some #(and (= :gateway-delete-queued (first %)) (= "t-2" (nth % 2))) fx))
        (expect (some #(and (= :gateway-delete-queued (first %)) (= "t-3" (nth % 2))) fx))))
  (it "a send while a cancel is in flight stays in the editor — never queued, never server-side"
      ;; Regression: cancel (`:cancelling?`) then immediately send. The send used
      ;; to still fire `:gateway-enqueue`, registering a SERVER-SIDE queued turn.
      ;; The cancel's restore deletes gateway records by :turn-id, but that id is
      ;; bound LATE by an async round-trip — so the restore raced ahead, the
      ;; orphaned turn survived and auto-drained (= SENT) while the text ALSO
      ;; landed back in the editor: "sent AND queued at the same time". A send
      ;; during a cancel is a FRESH intent — it stays purely in the EDITOR (nothing
      ;; queued locally, nothing registered server-side) so the user re-sends it
      ;; cleanly once the cancel settles.
      (let
        [send-fn
         (-> #'state/event-registry
             deref
             deref
             (get :send-message)
             :fn)

         db
         {:active-tab-id :main
          :session {:id "c1"}
          :workspace {:workspace/root "."}
          :loading? true
          :cancelling? true
          :input (input/empty-input)
          :pastes {}
          :paste-counter 0
          :pending-sends []
          :input-history []}

         {:keys [fx] cancelling-db :db}
         (send-fn db [:send-message "second" :main])

         {normal-fx :fx}
         (send-fn (assoc db :cancelling? false) [:send-message "second" :main])]

        ;; cancel window: kept in the editor — NOTHING queued locally, NOTHING
        ;; registered server-side, and the user is told to resend.
        (expect (not-any? #(= :gateway-enqueue (first %)) fx))
        (expect (empty? (:pending-sends cancelling-db)))
        (expect (some #(= :notify (first %)) fx))
        ;; normal in-flight queue (not cancelling) still registers server-side.
        (expect (some #(= :gateway-enqueue (first %)) normal-fx))))
  (it "a gateway enqueue that never lands is staged locally so no text is lost"
      ;; The ONLY case this channel writes a queue row of its own: the gateway
      ;; never accepted the submission, so there is no server record to mirror and
      ;; nothing else would ever show the text again. Such a row has NO turn id -
      ;; that is what marks it locally owned.
      (let
        [stage-fn
         (-> #'state/event-registry
             deref
             deref
             (get :stage-queued-locally)
             :fn)

         db
         {:active-tab-id :main :session {:id "c1"} :pending-sends []}

         db'
         (stage-fn db [:stage-queued-locally :main {:text "second" :client-id "c1c"}])

         row
         (first (:pending-sends db'))]

        (expect (= "second" (:text row)))
        (expect (nil? (:turn-id row)))
        (expect (true? (:mine? row)))
        ;; …and FLAGGED as never-sent, so the queued strip can say so instead of
        ;; painting it exactly like a server-backed row.
        (expect (true? (:unsent? row)))))
  (it "paints the queued row from the gateway ACK, and the broadcast is a no-op"
      ;; The enqueue fx feeds the SAME `:sync-queued-turn` writer the `turn.queued`
      ;; broadcast feeds, keyed by the same gateway turn id: whichever lands first
      ;; wins and the other changes nothing. An accepted-but-already-RUNNING turn
      ;; is not a queue row at all.
      (with-redefs
        [vis/gateway-submit-turn! (fn [_ opts]
                                    {:turn {"turn_id" "t-7"
                                            "status" "queued"
                                            "request" (:request opts)
                                            "idempotency_key" (:idempotency-key opts)}})]
        (reset! state/app-db
          {:session {:id "c1"} :active-tab-id :main :render-version 0 :pending-sends []})
        (let
          [enqueue-fx (get @@#'state/fx-registry :gateway-enqueue)
           client-id (#'state/mint-client-id :main)]

          (await-enqueue! (enqueue-fx
                            :main
                            {:id "c1"}
                            {:text "second" :agent-text "second" :client-id client-id :mine? true}
                            nil
                            nil
                            {}
                            nil))
          (expect (= ["t-7"] (mapv :turn-id (:pending-sends @state/app-db))))
          ;; ours, because the id came back from the gateway unchanged
          (expect (true? (:mine? (first (:pending-sends @state/app-db)))))
          ;; the broadcast for the same turn adds nothing
          (state/dispatch [:sync-queued-turn :main
                           {:op :add :turn-id "t-7" :client-id client-id :text "second"}])
          (expect (= 1 (count (:pending-sends @state/app-db)))))))
  (it "a queued image submit sends the COLLAPSED display copy, not the raw temp path"
      ;; Regression: `:gateway-enqueue` sent only `:agent-text` (paste placeholders
      ;; EXPANDED, so a pasted screenshot is a bare `/var/folders/…/clipboard-….png`).
      ;; The gateway then stored that as the turn's request and every channel — this
      ;; TUI's own transcript and the companion — painted the raw path. The collapsed
      ;; `vis-image` copy must ride along as `:display-request`.
      (let
        [sent
         (atom nil)

         preview
         "\n````vis-image\n[Image #1: shot.png, 44KB]\n/tmp/shot.png\nimage/png\n\n44KB\n````\n"]

        (with-redefs
          [vis/gateway-submit-turn!
           (fn [_ opts]
             (reset! sent opts)
             {:turn {"turn_id" "t-9" "status" "queued" "request" (:request opts)}})]
          (reset! state/app-db
            {:session {:id "c1"} :active-tab-id :main :render-version 0 :pending-sends []})
          (let [enqueue-fx (get @@#'state/fx-registry :gateway-enqueue)]
            (await-enqueue! (enqueue-fx :main
                                        {:id "c1"}
                                        {:text "look [Image #1: shot.png, 44KB]"
                                         :preview-text preview
                                         :agent-text "look \n/tmp/shot.png\n"
                                         :client-id "x"
                                         :mine? true}
                                        nil
                                        nil
                                        {}
                                        nil))
            (expect (= preview (:display-request @sent)))
            (expect (= "look \n/tmp/shot.png\n" (:request @sent)))
            ;; and the queue row itself shows the collapsed copy
            (expect (= preview (:preview-text (first (:pending-sends @state/app-db)))))))))
  (it "an accepted turn the gateway STARTED is never mirrored as queued"
      (with-redefs
        [vis/gateway-submit-turn! (fn [_ _]
                                    {:turn {"turn_id" "t-8" "status" "running"}})]
        (reset! state/app-db
          {:session {:id "c1"} :active-tab-id :main :render-version 0 :pending-sends []})
        (let [enqueue-fx (get @@#'state/fx-registry :gateway-enqueue)]
          (await-enqueue! (enqueue-fx
                            :main
                            {:id "c1"}
                            {:text "second" :agent-text "second" :client-id "x" :mine? true}
                            nil
                            nil
                            {}
                            nil))
          (expect (= [] (:pending-sends @state/app-db)))))))

(defdescribe
  gateway-enqueue-off-input-thread-test
  (it "a busy-time submit never blocks the caller — the TUI input thread"
      ;; Regression: `dispatch` runs effects on the DISPATCHING thread, and for a
      ;; submission that thread is the key loop (`screen/submit-input!`). The
      ;; enqueue POST was inline, so one unreachable daemon froze the editor for
      ;; the whole `ensure-gateway!` respawn wait plus the request timeout — with
      ;; no cursor, no feedback, nothing. It now hands off to the FIFO queue thread.
      (with-redefs
        [vis/gateway-submit-turn!
         (fn [_ _]
           (Thread/sleep 1500)
           {:turn {"turn_id" "t-slow" "status" "queued"}})

         vis/notify!
         (fn [& _]
           nil)]

        (reset! state/app-db {:session {:id "c1"}
                              :active-tab-id :main
                              :render-version 0
                              :loading? true
                              :pending-sends []})
        (let
          [t0
           (System/currentTimeMillis)

           _
           (state/dispatch [:enqueue-message "while busy"])

           blocked
           (- (System/currentTimeMillis) t0)]

          (expect (< blocked 500))
          ;; …and the row still lands, painted from the gateway ACK.
          (Thread/sleep 2500)
          (expect (= ["t-slow"] (mapv :turn-id (:pending-sends @state/app-db)))))))
  (it "a lost round-trip is retried under the idempotency key, never duplicated"
      ;; The daemon can die mid-POST and respawn seconds later. Retrying is safe BY
      ;; CONSTRUCTION, not by hope: `:idempotency-key` is the gateway's dedup key,
      ;; so the second attempt replays THE SAME turn instead of queueing a twin.
      (let [calls (atom 0)]
        (with-redefs
          [vis/gateway-submit-turn! (fn [_ opts]
                                      (if (= 1 (swap! calls inc))
                                        (throw (java.net.ConnectException. "Connection refused"))
                                        {:turn {"turn_id" "t-same"
                                                "status" "queued"
                                                "idempotency_key" (:idempotency-key opts)}}))
           vis/notify! (fn [& _]
                         nil)]

          (reset! state/app-db {:session {:id "c1"}
                                :active-tab-id :main
                                :render-version 0
                                :loading? true
                                :pending-sends []})
          (await-enqueue! ((get @@#'state/fx-registry :gateway-enqueue)
                            :main
                            {:id "c1"}
                            {:text "x" :agent-text "x" :client-id "cid" :mine? true}
                            nil
                            nil
                            {}
                            nil))
          (expect (= 2 @calls))
          (expect (= ["t-same"] (mapv :turn-id (:pending-sends @state/app-db)))))))
  (it "a session with NO id stages the row instead of leaving it awaiting an ack"
      ;; `:awaiting-ack?` is what tells `:drain-pending` to leave the head alone. If the
      ;; fx just returned when there is no session to POST to, that flag would stay set
      ;; forever: a row that says "Queued", never sends, and wedges every message queued
      ;; behind it. Staging locally is the only exit that keeps the queue alive.
      (with-redefs
        [vis/notify! (fn [& _]
                       nil)]
        (reset! state/app-db
          {:session {}
           :active-tab-id :main
           :render-version 0
           :loading? true
           :pending-sends [{:text "orphan" :client-id "cid-x" :mine? true :awaiting-ack? true}]})
        ((get @@#'state/fx-registry :gateway-enqueue)
          :main
          {}
          {:text "orphan" :agent-text "orphan" :client-id "cid-x" :mine? true}
          nil
          nil
          {}
          nil)
        (let [row (first (:pending-sends @state/app-db))]
          (expect (= "orphan" (:text row)))
          (expect (true? (:unsent? row)))
          (expect (nil? (:awaiting-ack? row))))))
  (it "a submission RETRACTED mid-flight deletes the turn it was standing for"
      ;; Cancelling pulls the row back into the editor and `:clear-pending-sends` drops
      ;; it — but neither can delete a gateway record whose turn id did not exist yet, so
      ;; the turn used to run anyway with nothing on screen that ever said it would. The
      ;; ack settles it: no row, no turn.
      (let [deleted (atom nil)]
        (with-redefs
          [vis/gateway-submit-turn! (fn [_ _]
                                      {:turn {"turn_id" "t-gone" "status" "queued"}})
           vis/gateway-delete-queued-turn! (fn [sid tid]
                                             (reset! deleted [sid tid])
                                             {:ok true})
           vis/notify! (fn [& _]
                         nil)]

          ;; the row is already gone: the user took the message back mid-round-trip
          (reset! state/app-db {:session {:id "c1"}
                                :active-tab-id :main
                                :render-version 0
                                :loading? true
                                :pending-sends []
                                ;; the cancel/clear that removed the row left this behind
                                :retracted-sends ["cid-gone"]})
          (await-enqueue!
            ((get @@#'state/fx-registry :gateway-enqueue)
              :main
              {:id "c1"}
              {:text "take it back" :agent-text "take it back" :client-id "cid-gone" :mine? true}
              nil
              nil
              {}
              nil))
          (expect (= ["c1" "t-gone"] @deleted))
          ;; and nothing is painted back onto the screen
          (expect (= [] (:pending-sends @state/app-db))))))
  (it "a submission the gateway never took is staged unsent AND drained, not orphaned"
      ;; Both halves of the rescue. The row is FLAGGED (the strip must not imply the
      ;; server has it), and the drain is nudged: the turn we queued behind may have
      ;; finished DURING the failing round-trip, and its terminal — the only other
      ;; thing that pops the queue — has already passed, so nothing would ever send it.
      (let
        [sent
         (atom [])

         prev
         (get @@#'state/fx-registry :session-turn)]

        (state/reg-fx :session-turn
                      (fn [_ _ text & _]
                        (swap! sent conj text)))
        (try (with-redefs
               [vis/gateway-submit-turn!
                (fn [_ _]
                  (throw (java.net.ConnectException. "refused")))

                vis/notify!
                (fn [& _]
                  nil)]

               (reset! state/app-db {:session {:id "c1"}
                                     :active-tab-id :main
                                     :render-version 0
                                     :loading? false
                                     :pending-sends []})
               (await-enqueue!
                 ((get @@#'state/fx-registry :gateway-enqueue)
                   :main
                   {:id "c1"}
                   {:text "rescue me" :agent-text "rescue me" :client-id "cid3" :mine? true}
                   nil
                   nil
                   {}
                   nil))
               (expect (= ["rescue me"] @sent))
               (expect (empty? (:pending-sends @state/app-db))))
             (finally (state/reg-fx :session-turn prev)))))
  (it "queue posts reach the daemon in the order they were typed"
      ;; One FIFO thread, not a thread per submit: an add and its delete may never
      ;; invert, so ordering is part of the fix — not a side effect of it.
      (let [seen (atom [])]
        (with-redefs
          [vis/gateway-submit-turn! (fn [_ opts]
                                      (Thread/sleep 60)
                                      (swap! seen conj (:request opts))
                                      {:turn {"turn_id" (str "t-" (:request opts))
                                              "status" "running"}})
           vis/notify! (fn [& _]
                         nil)]

          (reset! state/app-db {:session {:id "c1"}
                                :active-tab-id :main
                                :render-version 0
                                :loading? true
                                :pending-sends []})
          (let
            [enqueue-fx (get @@#'state/fx-registry :gateway-enqueue)
             futs (mapv (fn [n]
                          (enqueue-fx :main
                                      {:id "c1"}
                                      {:text n :agent-text n :client-id n :mine? true}
                                      nil
                                      nil
                                      {}
                                      nil))
                        ["1" "2" "3"])]

            (doseq [f futs]
              (await-enqueue! f))
            (expect (= ["1" "2" "3"] @seen)))))))

(defdescribe set-title-background-tab-test
             (it "relabels a background tab live without touching the active tab"
                 ;; Regression: a background session's async auto-title must land on its
                 ;; OWN tab while you stay on another tab. The title listener dispatches
                 ;; [:set-title title session-id] for every open session; :set-title
                 ;; resolves the owning tab via tab-id-for-session and relabels it in
                 ;; place. The active tab's :title must stay untouched.
                 (reset! state/app-db {:session {:id "active-session"}
                                       :title "Active"
                                       :tabs [{:id :main :label "Active" :active? true}
                                              {:id :tab-1 :label "Tab 1"}]
                                       :active-tab-id :main
                                       :tab-locals {:tab-1 {:session {:id "bg-session"}}}
                                       :render-version 0})
                 (state/dispatch [:set-title "Background Title" "bg-session"])
                 ;; Active tab's title untouched.
                 (expect (= "Active" (:title @state/app-db)))
                 ;; Background tab relabeled in its tab-locals and in the strip.
                 (expect (= "Background Title" (get-in @state/app-db [:tab-locals :tab-1 :title])))
                 (expect (= "Background Title"
                            (-> @state/app-db
                                :tabs
                                (nth 1)
                                :label))))
             (it "renames the active tab when :set-title carries the active session-id"
                 (reset! state/app-db {:session {:id "active-session"}
                                       :title "Old"
                                       :tabs [{:id :main :label "Old" :active? true}
                                              {:id :tab-1 :label "Tab 1"}]
                                       :active-tab-id :main
                                       :tab-locals {:tab-1 {:session {:id "bg-session"}}}
                                       :render-version 0})
                 (state/dispatch [:set-title "New" "active-session"])
                 (expect (= "New" (:title @state/app-db)))
                 (expect (= "New"
                            (-> @state/app-db
                                :tabs
                                (nth 0)
                                :label)))
                 ;; Background tab untouched.
                 (expect (= "Tab 1"
                            (-> @state/app-db
                                :tabs
                                (nth 1)
                                :label))))
             (it "is a no-op for a session-id that owns no open tab"
                 (reset! state/app-db {:session {:id "active-session"}
                                       :title "Active"
                                       :tabs [{:id :main :label "Active" :active? true}]
                                       :active-tab-id :main
                                       :tab-locals {}
                                       :render-version 0})
                 (state/dispatch [:set-title "Ghost" "unknown-session"])
                 (expect (= "Active" (:title @state/app-db)))
                 (expect (= "Active"
                            (-> @state/app-db
                                :tabs
                                (nth 0)
                                :label)))))

(defdescribe progress-trailing-flush-no-regress-test
             ;; Regression ("I see thinking but no code"): a throttled :reasoning chunk
             ;; schedules a trailing-edge flush holding a timeline SNAPSHOT taken before
             ;; the forms exist. A later :form-result dispatches the full timeline (with
             ;; code), but the stale reasoning flush would then fire and re-dispatch the
             ;; codeless snapshot — wiping the code. A lifecycle dispatch must cancel
             ;; pending throttled flushes so the freshest timeline wins.
             (it
               "a form-result dispatch cancels the stale reasoning flush; code is not wiped"
               (let
                 [make
                  @#'state/make-progress-render-updater

                  dispatched
                  (atom [])

                  scheduled
                  (atom nil)

                  now
                  (atom 0)

                  fake-future
                  (reify
                    java.util.concurrent.Future
                      (cancel [_ _] true)
                      (isCancelled [_] false)
                      (isDone [_] false)
                      (get [_] nil)
                      (get [_ _ _] nil))

                  schedule-fn
                  (fn [task _delay]
                    (reset! scheduled task)
                    fake-future)

                  update!
                  (make (fn [[_ tl]]
                          (swap! dispatched conj tl))
                        (fn []
                          @now)
                        schedule-fn)

                  thinking-only
                  {:iterations [{:thinking "hm" :forms []}]}

                  with-code
                  {:iterations [{:thinking "hm" :forms [{:code "git_status()" :success? true}]}]}]

                 ;; 1) reasoning fires immediately (first chunk, due)
                 (reset! now 0)
                 (update! thinking-only {:phase :reasoning})
                 ;; 2) reasoning within the throttle window → dropped + trailing flush scheduled
                 (reset! now 10)
                 (update! thinking-only {:phase :reasoning})
                 (expect (some? @scheduled))
                 ;; 3) the tool call lands: form-result dispatches the timeline WITH code
                 (reset! now 20)
                 (update! with-code {:phase :form-result})
                 ;; 4) the previously-scheduled reasoning flush fires LATE
                 (@scheduled)
                 ;; The LAST thing the bubble saw must still carry the code, not regress.
                 (expect (= with-code (last @dispatched)))
                 (expect (some #(seq (:forms %)) (:iterations (last @dispatched)))))))

(defdescribe message-received-clears-cancel-flags-test
             ;; Regression (the 4f0f6ac1 stuck tab): a turn that ends in a
             ;; provider/transport ERROR must still clear the in-flight flags. :loading?
             ;; drives the running border, :cancelling? the "Cancelling…" line, and both
             ;; clear ONLY on :message-received; :cancel-token holds the (now dead) turn
             ;; future. If a fatal turn skipped :message-received the tab would hang
             ;; forever showing a running border + "Cancelling…" that no Esc can clear
             ;; (Esc on a spent token is a no-op). The engine's fatal path returns
             ;; {:status :error}, the turn-runner dispatches :message-received with that
             ;; status, and this handler MUST reset all three.
             (it "an error :message-received clears :loading?, :cancelling? and :cancel-token"
                 (reset! state/app-db {:session {:id "c1"}
                                       :tabs [{:id :main :label "s" :active? true}]
                                       :active-tab-id :main
                                       :messages
                                       [{:role :user :text "hi"}
                                        {:role :assistant :pending? true :client-turn-id "t1"}]
                                       :loading? true
                                       :cancelling? true
                                       :cancel-token :tok
                                       :turn-start-ms 0
                                       :scroll scroll/follow
                                       :render-version 0})
                 (state/dispatch [:message-received (vis/markdown->ast "Could not reach provider")
                                  {:status :error :client-turn-id "t1"}])
                 (let [db @state/app-db]
                   (expect (false? (:loading? db)))
                   (expect (false? (:cancelling? db)))
                   (expect (nil? (:cancel-token db)))
                   ;; the pending assistant bubble was resolved, not left dangling
                   (expect (not (some #(and (= :assistant (:role %)) (true? (:pending? %)))
                                      (:messages db)))))))

(defdescribe
  sync-queued-turn-test
  ;; The gateway is the queue of record; :sync-queued-turn mirrors ONE queue
  ;; event (queued/updated/deleted/drained) into this tab's :pending-sends.
  (it "mirrors a sibling's queue add / update / delete into pending-sends"
      (reset! state/app-db {:session {:id "s1"} :active-tab-id "s1" :render-version 0})
      (state/dispatch [:sync-queued-turn nil {:op :add :turn-id "q1" :text "hello"}])
      (let [q (:pending-sends @state/app-db)]
        (expect (= 1 (count q)))
        (expect (= "q1" (:turn-id (first q))))
        (expect (= "hello" (:text (first q)))))
      ;; :add is idempotent on the same gateway turn id
      (state/dispatch [:sync-queued-turn nil {:op :add :turn-id "q1" :text "hello"}])
      (expect (= 1 (count (:pending-sends @state/app-db))))
      ;; a queued-prompt edit elsewhere rewrites the text
      (state/dispatch [:sync-queued-turn nil {:op :update :turn-id "q1" :text "edited"}])
      (expect (= "edited" (:text (first (:pending-sends @state/app-db)))))
      ;; the gateway drained (auto-started) or a sibling deleted it: entry drops
      (state/dispatch [:sync-queued-turn nil {:op :delete :turn-id "q1"}])
      (expect (= [] (:pending-sends @state/app-db))))
  (it "appends the gateway row instead of absorbing a locally staged one"
      ;; A staged row (the gateway enqueue never landed) has no turn id and
      ;; is NOT a candidate for a gateway row to bind to: same text is not
      ;; identity. The gateway row lands as its own row.
      (reset! state/app-db {:session {:id "s1"}
                            :active-tab-id "s1"
                            :render-version 0
                            :pending-sends [{:text "hello" :mine? true}]})
      (state/dispatch [:sync-queued-turn nil {:op :add :turn-id "q1" :text "hello"}])
      (let [q (:pending-sends @state/app-db)]
        (expect (= 2 (count q)))
        (expect (= [nil "q1"] (mapv :turn-id q)))))
  ;; The gateway drained (auto-started) this queued turn and it is now the
  ;; tab's LIVE turn (:gateway-turn-id). A late / out-of-order queue-sync
  ;; add|update for that SAME id — a replayed backlog or an event racing the
  ;; drain+attach — must NOT resurrect it as a "Queued" row while it runs
  ;; (the "sent AND queued at the same time" ghost seen in the TUI).
  (it "never mirrors the tab's currently-running turn as a queued row"
      (reset! state/app-db {:session {:id "s1"}
                            :active-tab-id "s1"
                            :render-version 0
                            :loading? true
                            :gateway-turn-id "turn-1"
                            :pending-sends []})
      ;; A stray :add for the running turn is ignored.
      (state/dispatch [:sync-queued-turn nil {:op :add :turn-id "turn-1" :text "hello"}])
      (expect (= [] (:pending-sends @state/app-db)))
      ;; A genuinely queued sibling (different id) still mirrors.
      (state/dispatch [:sync-queued-turn nil {:op :add :turn-id "turn-2" :text "world"}])
      (expect (= ["turn-2"] (mapv :turn-id (:pending-sends @state/app-db))))
      ;; An already-mirrored entry that becomes the running turn is stripped
      ;; on the next queue-sync op for it (e.g. a replayed :update).
      (reset! state/app-db {:session {:id "s1"}
                            :active-tab-id "s1"
                            :render-version 0
                            :loading? true
                            :gateway-turn-id "turn-1"
                            :pending-sends [{:text "hello" :turn-id "turn-1"}]})
      (state/dispatch [:sync-queued-turn nil {:op :update :turn-id "turn-1" :text "hi"}])
      (state/dispatch [:sync-queued-turn nil {:op :update :turn-id "turn-1" :text "hi"}])
      (expect (= [] (:pending-sends @state/app-db))))
  ;; Cancel a turn, then submit again before the daemon finishes tearing the
  (it "never mirrors the tab's own directly-sent in-flight turn as a queued row"
      (reset! state/app-db {:session {:id "s1"}
                            :active-tab-id "s1"
                            :render-version 0
                            :loading? true
                            :gateway-turn-id nil
                            :live-turn-client-id "c9"
                            :pending-sends []})
      ;; The gateway echoes our own in-flight turn back as queued (turn id
      ;; not bound yet) - recognised by the correlation id we submitted.
      (state/dispatch [:sync-queued-turn nil
                       {:op :add :turn-id "turn-9" :client-id "c9" :text "continue"}])
      (expect (= [] (:pending-sends @state/app-db)))
      ;; A genuinely different queued sibling still mirrors.
      (state/dispatch [:sync-queued-turn nil {:op :add :turn-id "turn-8" :text "other"}])
      (expect (= ["turn-8"] (mapv :turn-id (:pending-sends @state/app-db)))))
  (it "binds the turn id on turn.started so its own late echo stays unmirrored"
      (reset! state/app-db {:session {:id "s1"}
                            :active-tab-id "s1"
                            :render-version 0
                            :loading? true
                            :gateway-turn-id nil
                            :pending-sends []})
      (state/dispatch [:sync-turn-clock nil {:turn-id "turn-9"}])
      (expect (= "turn-9" (:gateway-turn-id @state/app-db)))
      ;; A genuinely-queued sibling with the SAME text mirrors (its id differs).
      (state/dispatch [:sync-queued-turn nil {:op :add :turn-id "turn-10" :text "continue"}])
      (expect (= ["turn-10"] (mapv :turn-id (:pending-sends @state/app-db))))
      ;; The live turn's own late echo is dropped by the exact-id match.
      (state/dispatch [:sync-queued-turn nil {:op :add :turn-id "turn-9" :text "continue"}])
      (expect (= ["turn-10"] (mapv :turn-id (:pending-sends @state/app-db))))))

(defdescribe sync-turn-clock-test
             ;; `turn.started` carries the gateway's CANONICAL started_at (epoch ms).
             ;; The tab's elapsed clock re-seeds from it, so every terminal attached
             ;; to the same work shows the SAME elapsed — local submit/drain/attach
             ;; stamps drift from the actual run start.
             (it "re-seeds :turn-start-ms from the canonical clock while loading"
                 (reset! state/app-db {:session {:id "s1"}
                                       :active-tab-id "s1"
                                       :render-version 0
                                       :loading? true
                                       :turn-start-ms 999999})
                 (state/dispatch [:sync-turn-clock nil {:turn-id "t1" :started-at-ms 1234}])
                 (expect (= 1234 (:turn-start-ms @state/app-db))))
             (it "no-ops when the tab is not mid-turn"
                 (reset! state/app-db
                   {:session {:id "s1"} :render-version 0 :loading? false :turn-start-ms 42})
                 (state/dispatch [:sync-turn-clock nil {:turn-id "t1" :started-at-ms 1234}])
                 (expect (= 42 (:turn-start-ms @state/app-db))))
             (it "binds :gateway-turn-id even when the event carries no clock"
                 (reset! state/app-db {:session {:id "s1"}
                                       :active-tab-id "s1"
                                       :render-version 0
                                       :loading? true
                                       :gateway-turn-id nil
                                       :turn-start-ms 42})
                 (state/dispatch [:sync-turn-clock nil {:turn-id "t1"}])
                 (expect (= 42 (:turn-start-ms @state/app-db)))
                 ;; No clock, but turn.started still carries the id — bind it so
                 ;; :cancel-turn can reach the gateway even for a clock-less start.
                 (expect (= "t1" (:gateway-turn-id @state/app-db))))
             (it "late-binds :gateway-turn-id for a plain send (nil until turn.started)"
                 (reset! state/app-db {:session {:id "s1"}
                                       :active-tab-id "s1"
                                       :render-version 0
                                       :loading? true
                                       :gateway-turn-id nil
                                       :turn-start-ms 999999})
                 (state/dispatch [:sync-turn-clock nil {:turn-id "t7" :started-at-ms 1234}])
                 (let [db @state/app-db]
                   (expect (= "t7" (:gateway-turn-id db)))
                   (expect (= 1234 (:turn-start-ms db)))))
             (it "never clobbers a :gateway-turn-id a drain/attach already bound"
                 (reset! state/app-db {:session {:id "s1"}
                                       :active-tab-id "s1"
                                       :render-version 0
                                       :loading? true
                                       :gateway-turn-id "already"
                                       :turn-start-ms 999999})
                 (state/dispatch [:sync-turn-clock nil {:turn-id "t7" :started-at-ms 1234}])
                 (expect (= "already" (:gateway-turn-id @state/app-db))))
             (it "does not bind :gateway-turn-id when the tab is not mid-turn"
                 (reset! state/app-db {:session {:id "s1"}
                                       :active-tab-id "s1"
                                       :render-version 0
                                       :loading? false
                                       :gateway-turn-id nil})
                 (state/dispatch [:sync-turn-clock nil {:turn-id "t7" :started-at-ms 1234}])
                 (expect (nil? (:gateway-turn-id @state/app-db)))))

(defdescribe sibling-turn-started-test
             ;; The persistent per-session event stream (chat/subscribe-session-events!)
             ;; reports a turn STARTED by a SIBLING channel. An idle tab attaches (via
             ;; :attach-running-turn); a tab already mid-turn (its own submit, or an
             ;; earlier drain/attach) no-ops so nothing double-attaches.
             (it "attaches an idle tab to a sibling-started turn"
                 (with-redefs
                   [vis/worker-future
                    (fn [_ _]
                      (future nil))

                    vis/cancellation-set-future!
                    (fn [_ _]
                      nil)]

                   (reset! state/app-db {:session {:id "s1"} :active-tab-id "s1" :render-version 0})
                   (state/dispatch [:sibling-turn-started nil
                                    {:turn-id "t9" :request "from web" :started-at-ms 777}])
                   (let [db @state/app-db]
                     (expect (true? (:loading? db)))
                     (expect (= "t9" (:gateway-turn-id db)))
                     (expect (= 777 (:turn-start-ms db))))))
             (it "no-ops when the tab is already mid-turn"
                 (reset! state/app-db {:session {:id "s1"}
                                       :render-version 0
                                       :loading? true
                                       :gateway-turn-id "t1"
                                       :turn-start-ms 42})
                 (state/dispatch [:sibling-turn-started nil
                                  {:turn-id "t9" :request "x" :started-at-ms 777}])
                 (let [db @state/app-db]
                   (expect (= "t1" (:gateway-turn-id db)))
                   (expect (= 42 (:turn-start-ms db))))))

(defn- terminal-test-db
  ([] (terminal-test-db {}))
  ([overrides]
   (merge {:session {:id "s1"}
           :active-tab-id "s1"
           :render-version 0
           :loading? true
           :cancelling? false
           :progress {:iterations []}
           :turn-start-ms 10
           :cancel-token ::token
           :gateway-turn-id "t1"
           :live-turn-client-id "c1"
           :submitted-input {:text "first" :pastes {} :paste-counter 0}
           :input {:lines [""] :crow 0 :ccol 0}
           :messages [{:role :user :text "first" :client-turn-id "c1"}
                      {:role :assistant :pending? true :client-turn-id "c1"}]}
          overrides)))

(defn- sync-terminal-without-timer!
  [chunk]
  (with-redefs
    [vis/worker-future (fn [_ _]
                         (future nil))]
    (state/dispatch [:sync-turn-terminal nil chunk])))

(defn- settle-marked-terminal!
  []
  (let
    [terminal (->> (:messages @state/app-db)
                   (keep :terminal-pending)
                   first)]
    (state/dispatch [:settle-turn-terminal nil terminal])))

(defdescribe
  sync-turn-terminal-test
  (it "releases only the matching gateway turn and marks its exact placeholder"
      (reset! state/app-db (terminal-test-db))
      (sync-terminal-without-timer! {:turn-id "other" :status "completed"})
      (expect (true? (:loading? @state/app-db)))
      (sync-terminal-without-timer! {:turn-id "t1" :status "completed"})
      (let
        [db
         @state/app-db

         marker
         (get-in db [:messages 1 :terminal-pending])]

        (expect (false? (:loading? db)))
        (expect (false? (:cancelling? db)))
        (expect (nil? (:progress db)))
        (expect (nil? (:gateway-turn-id db)))
        (expect (= "t1" (:turn-id marker)))
        (expect (= :completed (:status marker)))))
  (it "falls back to submit correlation before turn.started binds an id"
      (reset! state/app-db (terminal-test-db {:gateway-turn-id nil}))
      (sync-terminal-without-timer! {:turn-id "t1" :client-id "c1" :status "completed"})
      (expect (false? (:loading? @state/app-db)))
      (expect (= "c1" (get-in @state/app-db [:messages 1 :terminal-pending :client-id]))))
  (it "does not let a sibling terminal clear an unbound local turn"
      (reset! state/app-db (terminal-test-db {:gateway-turn-id nil}))
      (sync-terminal-without-timer! {:turn-id "sibling" :client-id "theirs" :status "completed"})
      (expect (true? (:loading? @state/app-db)))
      (expect (nil? (get-in @state/app-db [:messages 1 :terminal-pending]))))
  (it "settles a stranded completed worker after the grace path"
      (reset! state/app-db (terminal-test-db))
      (sync-terminal-without-timer! {:turn-id "t1" :client-id "c1" :status "completed"})
      (settle-marked-terminal!)
      (let [assistant (get-in @state/app-db [:messages 1])]
        (expect (not (:pending? assistant)))
        (expect (= :completed (:status assistant)))
        (expect (nil? (:terminal-pending assistant)))
        (expect (false? (:loading? @state/app-db)))))
  (it "replays a gateway-drained sibling start that landed mid-turn"
      ;; The gateway drains the queue ON this turn's terminal: completed ->
      ;; turn.queued.drained (which deletes the mirrored queue row, so
      ;; :drain-pending has nothing left to fire) -> turn.started. That start
      ;; arrives while the tab is still busy, so it must be PARKED and replayed
      ;; once the terminal settles; dropping it loses the message entirely.
      (with-redefs
        [vis/worker-future
         (fn [_ _]
           (future nil))

         vis/cancellation-set-future!
         (fn [_ _]
           nil)]

        (reset! state/app-db (terminal-test-db))
        (state/dispatch [:sibling-turn-started nil
                         {:turn-id "t2" :request "second" :started-at-ms 777}])
        (expect (= "t1" (:gateway-turn-id @state/app-db)))
        (expect (= "t2" (get-in @state/app-db [:deferred-sibling-start :turn-id])))
        (sync-terminal-without-timer! {:turn-id "t1" :client-id "c1" :status "completed"})
        (settle-marked-terminal!)
        (let [db @state/app-db]
          (expect (nil? (:deferred-sibling-start db)))
          (expect (true? (:loading? db)))
          (expect (= "t2" (:gateway-turn-id db)))
          (expect (= 777 (:turn-start-ms db))))))
  (it "settles a stranded failure instead of leaving a pending spinner"
      (reset! state/app-db (terminal-test-db))
      (sync-terminal-without-timer! {:turn-id "t1" :client-id "c1" :status "failed"})
      (settle-marked-terminal!)
      (let [assistant (get-in @state/app-db [:messages 1])]
        (expect (= :failed (:status assistant)))
        (expect (not (:pending? assistant)))
        (expect (= "turn_failed" (get-in assistant [:content 0 "code"])))))
  (it "terminal cancellation uses the pristine-editor restoration contract"
      (reset! state/app-db (terminal-test-db {:cancelling? true :cancelling-at-ms 11}))
      (sync-terminal-without-timer! {:turn-id "t1" :client-id "c1" :status "cancelled"})
      (settle-marked-terminal!)
      (let [db @state/app-db]
        (expect (= "first" (input/input->text (:input db))))
        (expect (= [] (:messages db)))
        (expect (false? (:loading? db)))
        (expect (false? (:cancelling? db)))))
  (it "terminal cancellation never overwrites typing entered during the race"
      (let [draft {:lines ["new draft"] :crow 0 :ccol 4 :selection-anchor [0 1]}]
        (reset! state/app-db (terminal-test-db
                               {:cancelling? true :cancelling-at-ms 11 :input draft}))
        (sync-terminal-without-timer! {:turn-id "t1" :client-id "c1" :status "cancelled"})
        (settle-marked-terminal!)
        (expect (= draft (:input @state/app-db)))
        (expect (= [] (:messages @state/app-db)))))
  (it "preserves the completed trace when the blocking worker is stranded"
      (let [trace [{:id :iter-1 :forms [{:id :form-1}]}]]
        (reset! state/app-db (terminal-test-db {:progress {:iterations trace}}))
        (sync-terminal-without-timer! {:turn-id "t1" :client-id "c1" :status "completed"})
        (settle-marked-terminal!)
        (expect (= trace (get-in @state/app-db [:messages 1 :traces])))))
  (it "a stranded completed turn paints the streamed prose, not \"Turn completed.\""
      ;; `turn.completed` is deliberately LEAN (no :content), so the independent
      ;; terminal path used to fabricate a "Turn completed." notice and drop the
      ;; answer the user just watched stream. The live trace already carries it.
      (let
        [trace [{:id :iter-1 :assistant-prose "first pass"}
                {:id :iter-2 :assistant-prose "final answer"}]]
        (reset! state/app-db (terminal-test-db {:progress {:iterations trace}}))
        (sync-terminal-without-timer! {:turn-id "t1" :client-id "c1" :status "completed"})
        (settle-marked-terminal!)
        (let [blocks (get-in @state/app-db [:messages 1 :content])]
          (expect (= 1 (count blocks)))
          (expect (= "prose" (get (first blocks) "type")))
          (expect (= "final answer" (get (first blocks) "markdown"))))))
  (it "a stranded completed turn with no prose keeps the notice"
      (reset! state/app-db (terminal-test-db {:progress {:iterations [{:id :iter-1}]}}))
      (sync-terminal-without-timer! {:turn-id "t1" :client-id "c1" :status "completed"})
      (settle-marked-terminal!)
      (expect (= "turn_completed"
                 (get (first (get-in @state/app-db [:messages 1 :content])) "code"))))
  (it "a stranded CANCELLED turn keeps its notice even with streamed prose"
      ;; Prose from a cancelled turn is a fragment, not an answer: the cancellation
      ;; notice is the settled truth there.
      (reset! state/app-db (terminal-test-db {:progress {:iterations [{:id :iter-1
                                                                       :assistant-prose "half"}]}}))
      (sync-terminal-without-timer! {:turn-id "t1" :client-id "c1" :status "cancelled"})
      (settle-marked-terminal!)
      (expect (= "turn_cancelled"
                 (get (first (get-in @state/app-db [:messages 1 :content])) "code"))))
  (it "lets the full worker result win inside the grace window"
      (reset! state/app-db (terminal-test-db))
      (sync-terminal-without-timer! {:turn-id "t1" :client-id "c1" :status "completed"})
      (let [terminal (get-in @state/app-db [:messages 1 :terminal-pending])]
        (state/dispatch [:message-received nil (vis/markdown->ast "full answer")
                         {:client-turn-id "c1" :status :completed}])
        (let [before (:messages @state/app-db)]
          (state/dispatch [:settle-turn-terminal nil terminal])
          (expect (= before (:messages @state/app-db)))
          (expect (= (vis/markdown->ast "full answer")
                     (get-in @state/app-db [:messages 1 :content]))))))
  (it "the worker result winning the grace race KEEPS the live trace"
      ;; Regression: `:sync-turn-terminal` clears `:progress` the instant the mux
      ;; sees the terminal event, so the blocking worker's `:message-received`
      ;; found no iterations and published an assistant bubble with only the
      ;; answer — everything the user watched LIVE disappeared on turn end.
      (let [trace [{:id :iter-1 :forms [{:id :form-1}]}]]
        (reset! state/app-db (terminal-test-db {:progress {:iterations trace}}))
        (sync-terminal-without-timer! {:turn-id "t1" :client-id "c1" :status "completed"})
        (state/dispatch [:message-received nil (vis/markdown->ast "full answer")
                         {:client-turn-id "c1" :status :completed}])
        (expect (= trace (get-in @state/app-db [:messages 1 :traces])))))
  (it "a late c1 callback cannot mutate c2 editor or active turn state"
      (reset! state/app-db (-> (terminal-test-db)
                               (assoc :live-turn-client-id "c2"
                                      :gateway-turn-id "t2"
                                      :submitted-input
                                      {:text "second" :pastes {2 "paste"} :paste-counter 2}
                                      :input {:lines ["typing"] :crow 0 :ccol 6})
                               (update :messages
                                       into
                                       [{:role :user :text "second" :client-turn-id "c2"}
                                        {:role :assistant :pending? true :client-turn-id "c2"}])))
      (state/dispatch [:message-received nil (vis/markdown->ast "old answer")
                       {:client-turn-id "c1" :status :cancelled}])
      (let [db @state/app-db]
        (expect (true? (:loading? db)))
        (expect (= "c2" (:live-turn-client-id db)))
        (expect (= "second" (get-in db [:submitted-input :text])))
        (expect (= "typing" (input/input->text (:input db))))
        (expect (not (get-in db [:messages 1 :pending?])))
        (expect (true? (get-in db [:messages 3 :pending?])))))
  (it "a stale generation settles with the trace parked on its own placeholder"
      ;; Issue #61: the next turn owns `:progress` now, so the late callback of the
      ;; PREVIOUS generation has no live iterations to publish. `clear-active-turn-state`
      ;; parks what the user watched onto that generation's placeholder
      ;; (`[:terminal-pending :trace]`); the stale branch must settle from there instead
      ;; of dropping every iteration the user saw.
      (let [trace [{:id :iter-1 :forms [{:id :form-1}]}]]
        (reset! state/app-db (-> (terminal-test-db)
                                 (assoc :live-turn-client-id "c2"
                                        :gateway-turn-id "t2")
                                 (assoc-in [:messages 1 :terminal-pending :trace] trace)
                                 (update :messages
                                         into
                                         [{:role :user :text "second" :client-turn-id "c2"}
                                          {:role :assistant :pending? true :client-turn-id "c2"}])))
        (state/dispatch [:message-received nil (vis/markdown->ast "old answer")
                         {:client-turn-id "c1" :status :cancelled}])
        (let [db @state/app-db]
          (expect (= trace (get-in db [:messages 1 :traces])))
          (expect (not (get-in db [:messages 1 :pending?])))
          (expect (true? (get-in db [:messages 3 :pending?]))))))
  (it "the delayed reconciler isolates c1 after an immediate resend"
      (reset! state/app-db (terminal-test-db))
      (sync-terminal-without-timer! {:turn-id "t1" :client-id "c1" :status "completed"})
      (let [terminal (get-in @state/app-db [:messages 1 :terminal-pending])]
        (swap! state/app-db (fn [db]
                              (-> db
                                  (assoc :loading? true
                                         :live-turn-client-id "c2"
                                         :gateway-turn-id "t2"
                                         :submitted-input {:text "second"})
                                  (update
                                    :messages
                                    into
                                    [{:role :user :text "second" :client-turn-id "c2"}
                                     {:role :assistant :pending? true :client-turn-id "c2"}]))))
        (state/dispatch [:settle-turn-terminal nil terminal])
        (let [db @state/app-db]
          (expect (= :completed (get-in db [:messages 1 :status])))
          (expect (true? (get-in db [:messages 3 :pending?])))
          (expect (true? (:loading? db)))
          (expect (= "c2" (:live-turn-client-id db))))))
  (it "duplicate identified callbacks are idempotent"
      (reset! state/app-db (terminal-test-db))
      (state/dispatch [:message-received nil (vis/markdown->ast "answer")
                       {:client-turn-id "c1" :status :completed}])
      (let [messages (:messages @state/app-db)]
        (state/dispatch [:message-received nil (vis/markdown->ast "duplicate")
                         {:client-turn-id "c1" :status :completed}])
        (expect (= messages (:messages @state/app-db)))
        (expect (= 2 (count (:messages @state/app-db)))))))

(defn- liveness-tick!
  "Run the liveness watchdog with the gateway registry stubbed to `turns` and
   both worker lanes made synchronous, so the probe's verdict lands inline."
  [turns now-ms]
  (with-redefs-fn {#'vis/gateway-list-turns (fn [_sid]
                                              turns)
                   #'vis/worker-future (fn [_ _]
                                         (future nil))
                   #'state/gateway-queue-io! (fn [f]
                                               (f)
                                               nil)}
    #(state/dispatch [:turn-liveness-tick now-ms])))

(defdescribe
  turn-liveness-tick-test
  (it "leaves a fresh in-flight turn alone"
      (reset! state/app-db (terminal-test-db {:turn-start-ms 10}))
      (liveness-tick! [{"turn_id" "t1" "status" "completed"}] 100)
      (expect (true? (:loading? @state/app-db)))
      (expect (nil? (:liveness-probed-at-ms @state/app-db))))
  (it "settles a turn the gateway registry already finished"
      ;; The terminal event never landed (SSE gap / wedged worker), so the bubble
      ;; would stream forever. The registry says otherwise — and the watchdog
      ;; replays that verdict through the ordinary terminal writer.
      (reset! state/app-db (terminal-test-db))
      (liveness-tick! [{"turn_id" "t1" "status" "completed" "idempotency_key" "c1"}] 100000)
      (let [db @state/app-db]
        (expect (false? (:loading? db)))
        (expect (nil? (:gateway-turn-id db)))
        (expect (= :completed (get-in db [:messages 1 :terminal-pending :status])))))
  (it "replays failed turn content from the liveness registry probe"
      (let
        [content [{"type" "error"
                   "code" "provider_unavailable"
                   "message" "Provider unavailable."
                   "is_retryable" true}]]
        (reset! state/app-db (terminal-test-db))
        (liveness-tick!
          [{"turn_id" "t1" "status" "failed" "idempotency_key" "c1" "content" content}]
          100000)
        (expect (= content (get-in @state/app-db [:messages 1 :terminal-pending :content])))))
  (it "keeps a still-running turn live"
      (reset! state/app-db (terminal-test-db))
      (liveness-tick! [{"turn_id" "t1" "status" "running"}] 100000)
      (expect (true? (:loading? @state/app-db)))
      (expect (= 100000 (:liveness-probed-at-ms @state/app-db))))
  (it "throttles repeat probes to one per interval"
      (reset! state/app-db (terminal-test-db))
      (let [calls (atom 0)]
        (with-redefs-fn {#'vis/gateway-list-turns (fn [_sid]
                                                    (swap! calls inc)
                                                    [])
                         #'vis/worker-future (fn [_ _]
                                               (future nil))
                         #'state/gateway-queue-io! (fn [f]
                                                     (f)
                                                     nil)}
          #(do (state/dispatch [:turn-liveness-tick 100000])
               (state/dispatch [:turn-liveness-tick 101000])
               (state/dispatch [:turn-liveness-tick 106000])))
        (expect (= 2 @calls))))
  (it "defers to the cancel self-heal while a cancel is pending"
      (reset! state/app-db (terminal-test-db {:cancelling? true :cancelling-at-ms 10}))
      (liveness-tick! [{"turn_id" "t1" "status" "completed"}] 100000)
      (expect (true? (:loading? @state/app-db)))))

(defn- with-gateway-stubs
  "Run `f` with the gateway registry stubbed to `turns` and both worker lanes made
   synchronous, so any probe a dispatch triggers lands inline."
  [turns f]
  (with-redefs-fn {#'vis/gateway-list-turns (fn [_sid]
                                              turns)
                   #'vis/worker-future (fn [_ _]
                                         (future nil))
                   #'state/gateway-queue-io! (fn [f]
                                               (f)
                                               nil)}
    f))

(defn- gateway-ready!
  "Deliver the server's `subscription.ready` verdict for the active tab. `chunk`
   carries the daemon's own view of the session: which turn it is running, if any."
  [chunk turns now-ms]
  (with-gateway-stubs turns #(state/dispatch [:sync-gateway-ready "s1" chunk now-ms])))

(defn- ready-chunk
  "`subscription.ready` as `gateway-event->chunk` projects it."
  [turn-id]
  {:phase :gateway-ready :gateway-turn-id turn-id :is-state-known true})

(defdescribe sync-gateway-ready-test
             ;; The socket dropped, the daemon finished the turn anyway, and its
             ;; `turn.completed` died with the stream. On resubscribe the server states what
             ;; it is running RIGHT NOW; if that disagrees with the bubble breathing here,
             ;; the gap is proven and this process re-asks the registry at once — instead of
             ;; waiting for the user to type something to shake it loose.
             (it "settles a turn the daemon is no longer running, without waiting out the grace"
                 ;; `:turn-start-ms 10` at now=100 is inside `turn-liveness-grace-ms`, where
                 ;; the render heartbeat deliberately does nothing (see the sibling test).
                 (reset! state/app-db (terminal-test-db {:turn-start-ms 10}))
                 (gateway-ready! (ready-chunk nil)
                                 [{"turn_id" "t1" "status" "completed" "idempotency_key" "c1"}]
                                 100)
                 (let [db @state/app-db]
                   (expect (false? (:loading? db)))
                   (expect (nil? (:gateway-turn-id db)))
                   (expect (= :completed (get-in db [:messages 1 :terminal-pending :status])))))
             (it "settles when the daemon has moved on to a DIFFERENT turn"
                 (reset! state/app-db (terminal-test-db {:turn-start-ms 10}))
                 (gateway-ready! (ready-chunk "t9")
                                 [{"turn_id" "t1" "status" "completed" "idempotency_key" "c1"}]
                                 100)
                 (expect (false? (:loading? @state/app-db))))
             (it "replays canonical content for a turn that failed while disconnected"
                 (let
                   [content [{"type" "error"
                              "code" "provider_unavailable"
                              "message" "Provider unavailable."
                              "is_retryable" true}]]
                   (reset! state/app-db (terminal-test-db {:turn-start-ms 10}))
                   (gateway-ready!
                     (ready-chunk nil)
                     [{"turn_id" "t1" "status" "failed" "idempotency_key" "c1" "content" content}]
                     100)
                   (expect (= content
                              (get-in @state/app-db [:messages 1 :terminal-pending :content])))))
             (it "asks NOTHING when the daemon confirms the turn this tab paints"
                 ;; The whole point of the inversion: agreement is a positive verdict from the
                 ;; source of truth, so a healthy reconnect costs zero round-trips.
                 (reset! state/app-db (terminal-test-db {:turn-start-ms 10}))
                 (let [calls (atom 0)]
                   (with-redefs-fn {#'vis/gateway-list-turns (fn [_sid]
                                                               (swap! calls inc)
                                                               [])
                                    #'vis/worker-future (fn [_ _]
                                                          (future nil))
                                    #'state/gateway-queue-io! (fn [f]
                                                                (f)
                                                                nil)}
                     #(state/dispatch [:sync-gateway-ready "s1" (ready-chunk "t1") 100]))
                   (expect (zero? @calls)))
                 (expect (true? (:loading? @state/app-db)))
                 (expect (nil? (:gateway-resynced-at-ms @state/app-db))))
             (it "probes when an older daemon omits its turn state"
                 ;; No `is_live` on the wire — the frame can only degrade to the previous
                 ;; behaviour, one unconditional read per reconnect.
                 (reset! state/app-db (terminal-test-db {:turn-start-ms 10}))
                 (gateway-ready! {:phase :gateway-ready :gateway-turn-id nil :is-state-known false}
                                 [{"turn_id" "t1" "status" "completed" "idempotency_key" "c1"}]
                                 100)
                 (expect (false? (:loading? @state/app-db))))
             (it "keeps a turn the registry still reports as running"
                 (reset! state/app-db (terminal-test-db {:turn-start-ms 10}))
                 (gateway-ready! (ready-chunk nil) [{"turn_id" "t1" "status" "running"}] 100)
                 (expect (true? (:loading? @state/app-db)))
                 (expect (= 100 (:liveness-probed-at-ms @state/app-db))))
             (it "collapses the copy every open tab's sink receives into one probe"
                 ;; The mux fans one resubscribe out to every subscribed sink, so an N-tab
                 ;; client sees N ready frames for the same reconnect.
                 (reset! state/app-db (terminal-test-db {:turn-start-ms 10}))
                 (let [calls (atom 0)]
                   (with-redefs-fn {#'vis/gateway-list-turns (fn [_sid]
                                                               (swap! calls inc)
                                                               [])
                                    #'vis/worker-future (fn [_ _]
                                                          (future nil))
                                    #'state/gateway-queue-io! (fn [f]
                                                                (f)
                                                                nil)}
                     #(do (state/dispatch [:sync-gateway-ready "s1" (ready-chunk nil) 100])
                          (state/dispatch [:sync-gateway-ready "s1" (ready-chunk nil) 101])
                          (state/dispatch [:sync-gateway-ready "s1" (ready-chunk nil) 6000])))
                   (expect (= 2 @calls)))))

(defdescribe restore-pending-ownership-test
             ;; A cancel pulls back ONLY the rows this tab submitted (`:mine?`, from the
             ;; correlation id the gateway echoed back). Sibling rows must survive:
             ;; them fired turn.queued.deleted at the sibling still blocked on its own
             ;; queued turn, which synthesized a spurious CANCELLED terminal there.
             (it "restores authored entries, keeps sibling mirrors queued"
                 (with-redefs
                   [vis/gateway-delete-queued-turn! (fn [_ _]
                                                      nil)]
                   (reset! state/app-db {:session {:id "s1"}
                                         :render-version 0
                                         :pending-sends
                                         [{:text "mine" :client-id "c1" :mine? true :turn-id "q1"}
                                          {:text "theirs" :turn-id "q2"}]})
                   (state/dispatch [:restore-pending-to-input nil])
                   (let [db @state/app-db]
                     (expect (= ["q2"] (mapv :turn-id (:pending-sends db))))
                     (expect (= ["mine"] (get-in db [:input :lines]))))))
             (it "no-ops when only mirrored entries are pending"
                 (reset! state/app-db {:session {:id "s1"}
                                       :render-version 0
                                       :pending-sends [{:text "theirs" :turn-id "q2"}]})
                 (state/dispatch [:restore-pending-to-input nil])
                 (expect (= 1 (count (:pending-sends @state/app-db))))))

;; Project-grouped tabs: `:tabs` keeps same-project tabs CONTIGUOUS (the
;; strip, C-x N jumps and cycle order all read that one vector), and the
;; per-place snapshot carries each tab's project root.
(defdescribe
  project-grouped-tabs-test
  (it "a new tab opens ADJACENT to its project group, not at the end"
      (reset! state/app-db {:tabs [{:id :main :label "Main" :active? true}]
                            :active-tab-id :main
                            :tab-locals {}
                            :render-version 0})
      (state/dispatch [:create-tab {:workspace {:root "/tmp/proj-a"}}])
      (state/dispatch [:create-tab {:workspace {:root "/tmp/proj-b"}}])
      (state/dispatch [:create-tab {:workspace {:root "/tmp/proj-a"}}])
      ;; tab-3 (proj-a) slots in right after tab-1 (proj-a), before tab-2.
      (expect (= [:main :tab-1 :tab-3 :tab-2] (mapv :id (:tabs @state/app-db))))
      (expect (= :tab-3 (:active-tab-id @state/app-db))))
  (it "a rift draft groups under its trunk via :repo-root"
      (reset! state/app-db {:tabs [{:id :main :label "Main" :active? true}]
                            :active-tab-id :main
                            :tab-locals {}
                            :render-version 0})
      (state/dispatch [:create-tab {:workspace {:root "/tmp/trunk"}}])
      (state/dispatch [:create-tab {:workspace {:root "/tmp/other"}}])
      (state/dispatch [:create-tab {:workspace {:root "/tmp/clones/x" :repo-root "/tmp/trunk"}}])
      (expect (= [:main :tab-1 :tab-3 :tab-2] (mapv :id (:tabs @state/app-db)))))
  (it "a tab with no workspace root still appends at the end"
      (reset! state/app-db
        {:tabs [{:id :main :label "Main" :active? true :workspace {:root "/tmp/proj-a"}}]
         :active-tab-id :main
         :tab-locals {}
         :render-version 0})
      (state/dispatch [:create-tab])
      (expect (= [:main :tab-1] (mapv :id (:tabs @state/app-db)))))
  (it "tab-session-snapshot carries each tab's project root"
      (reset! state/app-db
        {:tabs [{:id :main :label "Main" :active? true :workspace {:root "/tmp/proj-a"}}
                {:id :tab-1 :label "T1" :workspace {:root "/tmp/clones/x" :repo-root "/tmp/proj-b"}}
                {:id :tab-2 :label "T2"}]
         :active-tab-id :main
         :session {:id "sid-main"}
         :tab-locals {:tab-1 {:session {:id "sid-b"}} :tab-2 {:session {:id "sid-c"}}}
         :render-version 0})
      (expect (= {:active "sid-main"
                  :sessions [{:id "sid-main" :root "/tmp/proj-a"}
                             ;; draft → grouped under its trunk (:repo-root)
                             {:id "sid-b" :root "/tmp/proj-b"}
                             ;; no workspace → root absent, id-only entry
                             {:id "sid-c"}]}
                 (state/tab-session-snapshot @state/app-db)))))

(defdescribe
  close-tab-releases-idle-session-test
  ;; Invoke the `:close-tab` event handler directly and inspect the fx it
  ;; emits — no global app-db mutation. Closing the LAST idle view of a
  ;; session must release its daemon runtime + SSE listener; a session that
  ;; is still open elsewhere, or has a running/queued turn, is left alone.
  (let
    [close-tab
     (fn [db tab-id]
       ((-> #'state/event-registry
            deref
            deref
            (get :close-tab)
            :fn)
         db
         [:close-tab tab-id]))

     base
     (fn [extra]
       (merge {:tabs [{:id :main :label "Main" :active? true} {:id :tab-1 :label "T1"}]
               :active-tab-id :main
               :tab-locals {:tab-1 {:session {:id "other"}}}
               :render-version 0}
              extra))]

    (it "closing the last idle view releases its runtime + SSE listener"
        (let [{:keys [db fx]} (close-tab (base {:session {:id "sid-main"}}) :main)]
          (expect (= [[:unassign-session-project "sid-main"] [:release-session-listener "sid-main"]
                      [:release-session-runtime "sid-main"]]
                     fx))
          ;; tab is really gone; the still-open sibling stays
          (expect (= [:tab-1] (mapv :id (:tabs db))))))
    (it "a session still open in another tab is NOT released"
        (let
          [{:keys [fx]} (close-tab (base {:session {:id "shared"}
                                          :tab-locals {:tab-1 {:session {:id "shared"}}}})
                                   :main)]
          (expect (= [] fx))))
    (it "a session with a running turn is left alone (option b)"
        (let [{:keys [fx]} (close-tab (base {:session {:id "busy"} :loading? true}) :main)]
          ;; Closing disowns the project membership, but a busy runtime stays alive.
          (expect (= [[:unassign-session-project "busy"]] fx))))
    (it "a session with queued/pending sends is left alone"
        (let
          [{:keys [fx]} (close-tab (base {:session {:id "queued"}
                                          :pending-sends [{:text "later" :client-id "c-later"}]})
                                   :main)]
          ;; Queued work prevents runtime/listener release; the authored-but-
          ;; unsubmitted sends are handed to the gateway (:submit-orphan-sends)
          ;; instead of being dropped with the closing tab's :tab-locals — carrying
          ;; the correlation id, because a row still awaiting its ack may ALREADY be
          ;; registered and must not queue the same text a second time.
          (expect (= [[:unassign-session-project "queued"]
                      [:submit-orphan-sends "queued" [{:text "later" :client-id "c-later"}]]]
                     fx))))
    (it "closing the last remaining tab is a no-op (no release)"
        (let
          [{:keys [db fx]} (close-tab {:tabs [{:id :main :active? true}]
                                       :active-tab-id :main
                                       :session {:id "solo"}
                                       :tab-locals {}
                                       :render-version 0}
                                      :main)]
          (expect (nil? fx))
          (expect (= [:main] (mapv :id (:tabs db))))))))

(defdescribe
  shell-bang-pending-test
  ;; A `!`/`!&` shell-sugar turn runs LOCALLY with no provider round-trip, so its
  ;; live placeholder must not claim "Sending request to provider…", and its
  ;; settled bubble must not carry a model/provider footer. The TUI knows the
  ;; submission is a bang at submit time, so it flavors the placeholder and marks
  ;; the message `:slash?` (the same command marker a resumed `:tag :user-shell`
  ;; turn gets), which `render/draw-*` uses to drop the footer.
  (let
    [shell-bang-command?
     (deref #'state/shell-bang-command?)

     pending-assistant-for
     (deref #'state/pending-assistant-for)

     replace-pending-assistant
     (deref #'state/replace-pending-assistant)]

    (it "detects `!`/`!&` commands the same way the engine's parse-bang does"
        (expect (true? (shell-bang-command? "!ls -la")))
        (expect (true? (shell-bang-command? "!&tail -f x")))
        (expect (true? (shell-bang-command? "   !grep foo")))
        ;; A bare marker is ordinary prose (normal LLM turn), NOT a command.
        (expect (false? (shell-bang-command? "!")))
        (expect (false? (shell-bang-command? "!& ")))
        (expect (false? (shell-bang-command? "hello world")))
        (expect (false? (shell-bang-command? nil))))
    (it "gives a bang submission a shell placeholder + the :slash? command marker"
        (let [m (pending-assistant-for "!echo hi")]
          (expect (true? (:pending? m)))
          (expect (true? (:slash? m)))
          (expect (= "Running shell command..." (get-in m [:content 0 "message"])))
          ;; The zero-iteration live spinner reads this label instead of
          ;; "Vis is calling the provider" — a bang turn makes NO provider call.
          (expect (= "Running shell command" (:command-phase-label m)))))
    (it "gives a REGISTERED slash command a command placeholder + the :slash? marker"
        ;; A registered `/draft …` slash dispatches LOCALLY (no provider call), so its
        ;; bubble must drop the model/provider footer exactly like a `!` shell turn.
        ;; An UNKNOWN `/foo` (no registered root) falls through to template expansion
        ;; or a normal LLM turn and legitimately keeps its footer; a pasted absolute
        ;; path (`/var/…/shot.png …`) is prose, never a slash.
        (with-redefs
          [com.blockether.vis.core/registered-slashes
           (fn []
             [{:slash/name "draft-blank" :slash/parent []} {:slash/name "draft" :slash/parent []}
              {:slash/name "abandon" :slash/parent ["draft"]}])]
          (let
            [blank (pending-assistant-for "/draft-blank empty-provider-list")
             sub (pending-assistant-for "/draft abandon")
             unk (pending-assistant-for "/nope do a thing")
             path (pending-assistant-for "/var/folders/67/x/shot.png what is this")]

            (expect (true? (:slash? blank)))
            (expect (= "Running command..." (get-in blank [:content 0 "message"])))
            (expect (= "Running command" (:command-phase-label blank)))
            (expect (true? (:slash? sub)))
            (expect (nil? (:slash? unk)))
            (expect (nil? (:slash? path))))))
    (it "leaves a normal submission on the provider placeholder, no command marker"
        (let [m (pending-assistant-for "summarize the repo")]
          (expect (true? (:pending? m)))
          (expect (nil? (:slash? m)))
          (expect (= "Sending request to provider..." (get-in m [:content 0 "message"])))
          ;; A normal turn DOES call the provider, so no override label.
          (expect (nil? (:command-phase-label m)))))
    (it "carries the :slash? command marker from the pending slot onto the settled bubble"
        ;; The settled wire result carries no "slash" flag (dead live-path
        ;; plumbing), so the command marker must survive the pending->settled swap
        ;; or the live footer would reappear until the session is reopened.
        (let
          [msgs
           [{:role :assistant :pending? true :slash? true :client-turn-id "t1"}]

           settled
           {:role :assistant :client-turn-id "t1" :text "done"}

           out
           (replace-pending-assistant msgs settled)]

          (expect (true? (:slash? (first out)))))
        ;; A normal turn's settled bubble is untouched (no marker leaks in).
        (let
          [msgs
           [{:role :assistant :pending? true :client-turn-id "t2"}]

           settled
           {:role :assistant :client-turn-id "t2" :text "done"}

           out
           (replace-pending-assistant msgs settled)]

          (expect (nil? (:slash? (first out))))))))

(defdescribe
  queue-mirror-gateway-owned-test
  "Queue rows are GATEWAY-owned: a busy-time submission is registered server-side
   and the row is painted only from gateway truth (the enqueue ack or the
   `turn.queued` broadcast), always keyed by the gateway turn id. Identity is
   ID-ONLY - turn id, then the correlation id this tab minted as the
   `idempotency_key`. Request TEXT is never identity; text-first matching is what
   let one queued row shadow (or duplicate) another."
  (it "the same turn id from ack and broadcast yields exactly one row"
      (reset! state/app-db
        {:session {:id "s1"} :active-tab-id "s1" :render-version 0 :pending-sends []})
      (state/dispatch [:sync-queued-turn nil {:op :add :turn-id "q1" :client-id "c1" :text "go"}])
      (state/dispatch [:sync-queued-turn nil {:op :add :turn-id "q1" :client-id "c1" :text "go"}])
      (expect (= [{:turn-id "q1" :client-id "c1"}]
                 (mapv #(select-keys % [:turn-id :client-id]) (:pending-sends @state/app-db)))))
  (it "two submissions sharing the same text stay two independent rows"
      (reset! state/app-db
        {:session {:id "s1"} :active-tab-id "s1" :render-version 0 :pending-sends []})
      (state/dispatch [:sync-queued-turn nil {:op :add :turn-id "q1" :client-id "c1" :text "go"}])
      (state/dispatch [:sync-queued-turn nil {:op :add :turn-id "q2" :client-id "c2" :text "go"}])
      (expect (= ["q1" "q2"] (mapv :turn-id (:pending-sends @state/app-db)))))
  (it "ownership comes from the echoed id: only ids this tab minted are ours"
      ;; A cancel may pull only OUR queued text into THIS composer. The correlation
      ;; id encodes process+tab, so ownership survives a re-attach and never
      ;; mistakes a sibling channel's queued message for ours.
      (reset! state/app-db
        {:session {:id "s1"} :active-tab-id "s1" :render-version 0 :pending-sends []})
      (state/dispatch
        [:sync-queued-turn "s1"
         {:op :add :turn-id "q1" :client-id (#'state/mint-client-id "s1") :text "mine"}])
      (state/dispatch [:sync-queued-turn "s1"
                       {:op :add :turn-id "q2" :client-id "other-channel:abc" :text "theirs"}])
      (expect (= [true nil] (mapv :mine? (:pending-sends @state/app-db)))))
  (it "recognises the tab's OWN live turn by correlation id before the id binds"
      (reset! state/app-db {:session {:id "s1"}
                            :active-tab-id "s1"
                            :render-version 0
                            :loading? true
                            :gateway-turn-id nil
                            :live-turn-client-id "c9"
                            :pending-sends []})
      (state/dispatch [:sync-queued-turn nil
                       {:op :add :turn-id "t9" :client-id "c9" :text "anything"}])
      (expect (= [] (:pending-sends @state/app-db))))
  (it "seeds the attach backlog once, marking OUR rows by the echoed id"
      (with-redefs
        [vis/gateway-drain-idle! (fn [_]
                                   nil)]
        (let [mine (#'state/mint-client-id "s1")]
          (reset! state/app-db {:session {:id "s1"}
                                :active-tab-id "s1"
                                :render-version 0
                                :loading? false
                                :pending-sends []})
          (state/dispatch [:attach-running-turn "s1"
                           {:id "s1"
                            :status "idle"
                            :queued-turns
                            [{:turn-id "q5" :client-id mine :text "go" :queued-at-ms 1}
                             {:turn-id "q6" :client-id "other:x" :text "go" :queued-at-ms 2}]}])
          (state/dispatch [:sync-queued-turn "s1"
                           {:op :add :turn-id "q5" :client-id mine :text "go"}])
          (expect (= ["q5" "q6"] (mapv :turn-id (:pending-sends @state/app-db))))
          (expect (= [true nil] (mapv :mine? (:pending-sends @state/app-db))))))))

(defdescribe
  queue-delete-reconcile-test
  "A queued row is removed locally as a fast echo, but the gateway has the last
   word: when the delete does not land, the row is written back through the one
   `:sync-queued-turn` writer instead of hiding a turn that still auto-drains."
  (it "writes the row back when the delete never reached the gateway"
      (with-redefs
        [vis/gateway-delete-queued-turn! (fn [_ _]
                                           (throw (ex-info "connection refused" {})))]
        (reset! state/app-db
          {:session {:id "s1"} :active-tab-id "s1" :render-version 0 :pending-sends []})
        (await-enqueue! ((get @@#'state/fx-registry :gateway-delete-queued)
                          "s1"
                          "q1"
                          "s1"
                          {:text "still queued" :client-id "c1" :mine? true}))
        (let [row (first (:pending-sends @state/app-db))]
          (expect (= "q1" (:turn-id row)))
          (expect (= "still queued" (:text row)))
          ;; provenance survives the round-trip, so a cancel can still reclaim it
          (expect (true? (:mine? row))))))
  (it "keeps the row removed when the gateway says it is gone or already started"
      (doseq [status [404 409]]
        (with-redefs
          [vis/gateway-delete-queued-turn! (fn [_ _]
                                             (throw (ex-info "nope" {:http-status status})))]
          (reset! state/app-db
            {:session {:id "s1"} :active-tab-id "s1" :render-version 0 :pending-sends []})
          (await-enqueue! ((get @@#'state/fx-registry :gateway-delete-queued)
                            "s1"
                            "q1"
                            "s1"
                            {:text "gone" :mine? true}))
          (expect (= [] (:pending-sends @state/app-db))))))
  (it "does nothing more when the delete succeeds"
      (with-redefs
        [vis/gateway-delete-queued-turn! (fn [_ _]
                                           {"status" "deleted"})]
        (reset! state/app-db
          {:session {:id "s1"} :active-tab-id "s1" :render-version 0 :pending-sends []})
        (await-enqueue! ((get @@#'state/fx-registry :gateway-delete-queued)
                          "s1"
                          "q1"
                          "s1"
                          {:text "gone" :mine? true}))
        (expect (= [] (:pending-sends @state/app-db))))))

;; A project's member list IS the tab set that was open last time (closing a tab
;; unassigns the session), so restore must be LOSSLESS. It used to stop at
;; `max-tabs` (8), so relaunching a project with more open tabs silently dropped
;; the rest — and the follow-up persist rewrote the stored order from the
;; truncated strip.
(defdescribe preallocate-project-tabs-test
             (it "restores every member tab, past the manual max-tabs cap"
                 (reset! state/app-db {:tabs [{:id :main :label "Main" :active? true}]
                                       :active-tab-id :main
                                       :session {:id "sid-0"}
                                       :tab-locals {}
                                       :render-version 0})
                 (state/dispatch [:preallocate-project-tabs
                                  (mapv (fn [i]
                                          {:session-id (str "sid-" (inc i))
                                           :label (str "S" (inc i))
                                           :root "/tmp/proj"})
                                        (range 11))])
                 (expect (= 12 (count (:tabs @state/app-db))))
                 (expect (= (mapv #(str "sid-" %) (range 1 12))
                            (->> (:tabs @state/app-db)
                                 (keep :session-id)
                                 vec)))
                 ;; name-only until first focus, and focus never moves
                 (expect (every? :pending? (remove #(= :main (:id %)) (:tabs @state/app-db))))
                 (expect (= :main (:active-tab-id @state/app-db))))
             (it "never duplicates a session already open in a tab"
                 (reset! state/app-db {:tabs [{:id :main :label "Main" :active? true}]
                                       :active-tab-id :main
                                       :session {:id "sid-1"}
                                       :tab-locals {}
                                       :render-version 0})
                 (state/dispatch [:preallocate-project-tabs
                                  [{:session-id "sid-1" :label "S1" :root "/tmp/proj"}
                                   {:session-id "sid-2" :label "S2" :root "/tmp/proj"}]])
                 (expect (= 2 (count (:tabs @state/app-db))))))

(defdescribe
  cancelled-queue-restores-to-input-test
  "A user CANCEL drops the whole pre-cancel backlog server-side and mirrors every
   drop back as `turn.queued.deleted` with reason `cancelled`. Stop means stop -
   but the words the user already wrote are theirs, so each dropped row comes
   back into the owning session's EDITOR as a draft instead of vanishing. A plain
   delete (the user cleared the row) and a drain (the gateway started it) carry no
   reason and restore nothing."
  (it "a cancelled delete moves the queued text back into the input"
      (reset! state/app-db {:session {:id "s1"}
                            :active-tab-id "s1"
                            :render-version 0
                            :input (input/empty-input)
                            :pending-sends []})
      (state/dispatch [:sync-queued-turn nil {:op :add :turn-id "q1" :text "queued words"}])
      (state/dispatch [:sync-queued-turn nil
                       {:op :delete :turn-id "q1" :text "queued words" :reason "cancelled"}])
      (expect (= [] (:pending-sends @state/app-db)))
      (expect (= "queued words" (input/input->text (:input @state/app-db)))))
  (it "restores a SIBLING channel's queued text too - authorship is not consulted"
      ;; The companion queued it, the TUI pressed stop (or the other way round).
      ;; Whoever wrote it, the text has nowhere else to live once the gateway
      ;; dropped the row, so the attached editor keeps it.
      (reset! state/app-db {:session {:id "s1"}
                            :active-tab-id "s1"
                            :render-version 0
                            :input (input/empty-input)
                            :pending-sends []})
      (state/dispatch [:sync-queued-turn "s1"
                       {:op :add :turn-id "q1" :client-id "other-channel:abc" :text "theirs"}])
      (expect (= [nil] (mapv :mine? (:pending-sends @state/app-db))))
      (state/dispatch [:sync-queued-turn "s1"
                       {:op :delete :turn-id "q1" :text "theirs" :reason "cancelled"}])
      (expect (= "theirs" (input/input->text (:input @state/app-db)))))
  (it "appends after whatever is already typed and never sends"
      (reset! state/app-db {:session {:id "s1"}
                            :active-tab-id "s1"
                            :render-version 0
                            :input (#'state/text->input-state "draft")
                            :pending-sends []})
      (state/dispatch [:sync-queued-turn nil {:op :add :turn-id "q1" :text "one"}])
      (state/dispatch [:sync-queued-turn nil {:op :add :turn-id "q2" :text "two"}])
      (state/dispatch [:sync-queued-turn nil
                       {:op :delete :turn-id "q1" :text "one" :reason "cancelled"}])
      (state/dispatch [:sync-queued-turn nil
                       {:op :delete :turn-id "q2" :text "two" :reason "cancelled"}])
      (expect (= "draft\n\none\n\ntwo" (input/input->text (:input @state/app-db))))
      (expect (= [] (:pending-sends @state/app-db)))
      (expect (not (:loading? @state/app-db))))
  (it "a plain delete or a drain leaves the input untouched"
      (reset! state/app-db {:session {:id "s1"}
                            :active-tab-id "s1"
                            :render-version 0
                            :input (input/empty-input)
                            :pending-sends []})
      (state/dispatch [:sync-queued-turn nil {:op :add :turn-id "q1" :text "cleared"}])
      (state/dispatch [:sync-queued-turn nil {:op :delete :turn-id "q1" :text "cleared"}])
      (expect (= [] (:pending-sends @state/app-db)))
      (expect (= "" (input/input->text (:input @state/app-db)))))
  (it "an unmirrored id restores nothing - the text cannot land twice"
      ;; The local cancel path already pulled the row back and deleted the mirror;
      ;; the gateway broadcast that follows must be a no-op.
      (reset! state/app-db {:session {:id "s1"}
                            :active-tab-id "s1"
                            :render-version 0
                            :input (input/empty-input)
                            :pending-sends []})
      (state/dispatch [:sync-queued-turn nil
                       {:op :delete :turn-id "ghost" :text "ghost" :reason "cancelled"}])
      (expect (= "" (input/input->text (:input @state/app-db)))))
  (it "lands in the SESSION's tab, not the tab you happen to be looking at"
      ;; Cancelling while another session is focused must not spill one session's
      ;; words into another's editor.
      (reset! state/app-db {:session {:id "visible"}
                            :input (input/empty-input)
                            :tabs [{:id :main :label "Main" :active? true}
                                   {:id :tab-1 :label "Tab 1"}]
                            :active-tab-id :main
                            :tab-locals {:tab-1 {:title "Tab 1"}}
                            :render-version 0})
      (state/dispatch [:sync-queued-turn :tab-1
                       {:op :add :turn-id "q-bg" :text "background words"}])
      (state/dispatch [:sync-queued-turn :tab-1
                       {:op :delete :turn-id "q-bg" :text "background words" :reason "cancelled"}])
      (expect (= "" (input/input->text (:input @state/app-db))))
      (let [locals (get-in @state/app-db [:tab-locals :tab-1])]
        (expect (= [] (vec (:pending-sends locals))))
        (expect (= "background words" (input/input->text (:input locals)))))))

(defdescribe human-input-dialog-test
             (it "queues a second request behind the open one and reopens it on close"
                 (reset! state/app-db {:render-version 0})
                 (state/dispatch [:human-input-open {:request {:id "r1"}}])
                 (expect (= {:request {:id "r1"}} (:human-input @state/app-db)))
                 ;; A second engine request must not steal the dialog mid-answer.
                 (state/dispatch [:human-input-open {:request {:id "r2"}}])
                 (expect (= {:request {:id "r1"}} (:human-input @state/app-db)))
                 (expect (= [{:request {:id "r2"}}] (:human-input-queue @state/app-db)))
                 (state/dispatch [:human-input-close "r1"])
                 (expect (= {:request {:id "r2"}} (:human-input @state/app-db)))
                 (expect (= [] (:human-input-queue @state/app-db))))
             (it "drops a QUEUED request the engine closed before it was ever shown"
                 (reset! state/app-db {:render-version 0})
                 (state/dispatch [:human-input-open {:request {:id "r1"}}])
                 (state/dispatch [:human-input-open {:request {:id "r2"}}])
                 (state/dispatch [:human-input-close "r2"])
                 (expect (= {:request {:id "r1"}} (:human-input @state/app-db)))
                 (expect (= [] (:human-input-queue @state/app-db))))
             (it "stores each edited form and clears the dialog on an unaddressed close"
                 (reset! state/app-db {:render-version 0})
                 (state/dispatch [:human-input-open {:request {:id "r1"} :focus 0}])
                 (state/dispatch [:human-input-form {:request {:id "r1"} :focus 2}])
                 (expect (= 2 (get-in @state/app-db [:human-input :focus])))
                 (state/dispatch [:human-input-close nil])
                 (expect (nil? (:human-input @state/app-db)))))
