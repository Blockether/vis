(ns com.blockether.vis.ext.channel-tui.state-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.state :as state]
            [com.blockether.vis.ext.channel-tui.virtual :as virtual]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe default-settings-test
  (it "shows provider reasoning in chat UI by default"
    ;; Hidden-by-default policy was retired (state.clj): GLM `reasoning_content`,
    ;; Copilot Claude `reasoning_text`, Codex / Anthropic thinking summaries
    ;; are exactly the signal users want when an agent turn looks like it
    ;; froze. Toggle stays available in Settings (`Show model thinking`).
    (expect (true? (:show-thinking state/default-settings)))
    (expect (true? (:show-iterations state/default-settings)))))

(defdescribe detail-toggle-test
  (it "does not cold-clear render and height caches on disclosure click"
    (let [render-invalidations (atom 0)
          height-invalidations (atom 0)]
      (with-redefs [render/invalidate-cache! (fn [] (swap! render-invalidations inc))
                    virtual/invalidate-heights! (fn [] (swap! height-invalidations inc))]
        (reset! state/app-db {:detail-expansions {}
                              :render-version 0})
        (state/dispatch [:toggle-detail "cid" "answer:t11111111:details:d1"])
        (expect (= {["cid" "answer:t11111111:details:d1"] true}
                  (:detail-expansions @state/app-db)))
        (expect (zero? @render-invalidations))
        (expect (zero? @height-invalidations)))))

  (it "stores preview switcher mode on the same detail-expansions bus"
    (reset! state/app-db {:detail-expansions {}
                          :render-version 0})
    (state/dispatch [:select-preview-mode "cid" "iteration:t11111111:i1:b1:preview-switch" :raw])
    (expect (= {["cid" "iteration:t11111111:i1:b1:preview-switch"] :raw}
              (:detail-expansions @state/app-db))))

  (it "applies external input to an inactive recording-origin workspace"
    (let [external-input-fn (-> #'state/event-registry deref deref (get :external-input) :fn)
          input-state       (fn [text] {:lines [text] :crow 0 :ccol (count text)})
          db                {:active-workspace-id :second
                             :workspaces [{:id :first :label "First"}
                                          {:id :second :label "Second" :active? true}]
                             :input (input-state "second draft")
                             :input-history-index :second-index
                             :input-history-draft "second-draft"
                             :slash-command-index 7
                             :slash-command-hidden? true
                             :workspace-locals {:first {:input (input-state "first draft")
                                                        :input-history-index :first-index
                                                        :input-history-draft "first-draft"
                                                        :slash-command-index 3
                                                        :slash-command-hidden? true}}}
          next-db           (external-input-fn db [:external-input :append "rewrite" :first])]
      (expect (= "second draft" (input/input->text (:input next-db))))
      (expect (= :second-index (:input-history-index next-db)))
      (expect (= "first draft\nrewrite"
                (input/input->text (get-in next-db [:workspace-locals :first :input]))))
      (expect (nil? (get-in next-db [:workspace-locals :first :input-history-index])))
      (expect (nil? (get-in next-db [:workspace-locals :first :input-history-draft])))
      (expect (= 0 (get-in next-db [:workspace-locals :first :slash-command-index])))
      (expect (false? (get-in next-db [:workspace-locals :first :slash-command-hidden?]))))))

(defdescribe external-input-test
  (it "append adds transcript text without replacing draft input"
    (reset! state/app-db {:input (input/paste-text (input/empty-input) "typed")
                          :input-history-index :stale
                          :input-history-draft "old"
                          :render-version 0})
    (state/dispatch [:external-input :append "voice text"])
    (expect (= "typed\nvoice text"
              (input/input->text (:input @state/app-db))))
    (expect (nil? (:input-history-index @state/app-db)))
    (expect (nil? (:input-history-draft @state/app-db)))))

(defdescribe transcript-dump-guard-test
  (it "detects copied assistant trace dumps"
    (expect (true?
              (state/transcript-dump-input?
                "The user is reporting a bug\n\n▾ RESULT [iteration 1 · block 1]\n...")))
    (expect (false?
              (state/transcript-dump-input?
                "Dobra, wróć do (def snapshot-result (v/snapshot))"))))

  (it "filters poisoned transcript dumps out of resumed input history"
    (let [poison "The user is reporting a bug\n\n▾ RESULT [iteration 1 · block 1]\n..."]
      (reset! state/app-db {:render-version 0})
      (state/dispatch [:init-session {:id "c1"}
                       [{:role :user :text "safe prompt"}
                        {:role :assistant :text "ok"}
                        {:role :user :text poison}]])
      (expect (= ["safe prompt"] (:input-history @state/app-db))))))

(defdescribe channel-status-test
  (it "clears ttl-bound statuses only when the deadline still matches"
    (reset! state/app-db {:channel-status {}
                          :render-version 0})
    (state/dispatch [:channel-status-set :voice/input {:text "○ Voice ready"
                                                       :level :info
                                                       :until 100}])
    (state/dispatch [:channel-status-clear-if-until :voice/input 99])
    (expect (= "○ Voice ready" (get-in @state/app-db [:channel-status :voice/input :text])))
    (state/dispatch [:channel-status-clear-if-until :voice/input 100])
    (expect (nil? (get-in @state/app-db [:channel-status :voice/input])))))

(defdescribe slash-command-selection-test
  (it "cycles selected slash suggestion index for arrows and mouse wheel"
    (reset! state/app-db {:slash-command-index 0
                          :render-version 0})
    (state/dispatch [:move-slash-command-selection 1 3])
    (expect (= 1 (:slash-command-index @state/app-db)))
    (state/dispatch [:move-slash-command-selection -1 3])
    (expect (= 0 (:slash-command-index @state/app-db)))
    (state/dispatch [:move-slash-command-selection -1 3])
    (expect (= 2 (:slash-command-index @state/app-db))))

  (it "can hide slash suggestions after tab completion until input is cleared"
    (reset! state/app-db {:input (input/paste-text (input/empty-input) "/new-tab ")
                          :slash-command-hidden? false
                          :render-version 0})
    (state/dispatch [:hide-slash-command-suggestions])
    (expect (true? (:slash-command-hidden? @state/app-db)))
    (state/dispatch [:update-input (input/paste-text (input/empty-input) "/new-tab arg")])
    (expect (true? (:slash-command-hidden? @state/app-db)))
    (state/dispatch [:reset-input])
    (expect (false? (:slash-command-hidden? @state/app-db)))))

(defdescribe workspace-entries-test
  (it "adds a workspace and seeds a base workspace when none exist"
    ;; Base workspace inherits the current `:title`; the freshly-added workspace
    ;; starts as `Untitled session` because it has no title yet.
    (reset! state/app-db {:title "Current"
                          :render-version 0})
    (state/dispatch [:create-workspace])
    (expect (= [{:id :main :label "Current"}
                {:id :tab-1 :label state/untitled-session-label :active? true}]
              (:workspaces @state/app-db)))
    (expect (= :tab-1 (:active-workspace-id @state/app-db)))
    (expect (= 1 (:render-version @state/app-db))))

  (it "adds the next unique workspace and makes it active"
    ;; New workspaces default to the untitled placeholder; `:set-title`
    ;; renames the active workspace once a title is generated.
    (reset! state/app-db {:workspaces [{:id :main :label "Main"}
                                       {:id :tab-1 :label "Tab 1" :active? true}]
                          :active-workspace-id :tab-1
                          :render-version 0})
    (state/dispatch [:create-workspace])
    (expect (= [{:id :main :label "Main"}
                {:id :tab-1 :label "Tab 1"}
                {:id :tab-2 :label state/untitled-session-label :active? true}]
              (:workspaces @state/app-db)))
    (expect (= :tab-2 (:active-workspace-id @state/app-db))))

  (it "attaches workspace root to the new workspace and active snapshot"
    (let [workspace {:workspace/id "ws-1"
                     :workspace/root "/tmp/vis-ws"
                     :main {:branch "feature/ws"}}]
      (reset! state/app-db {:workspaces [{:id :main :label "Main" :active? true}]
                            :active-workspace-id :main
                            :workspace-locals {}
                            :render-version 0})
      (state/dispatch [:create-workspace {:workspace workspace}])
      (expect (= "/tmp/vis-ws" (get-in @state/app-db [:workspaces 1 :workspace/root])))
      (expect (= workspace (:workspace @state/app-db)))
      (expect (= "/tmp/vis-ws" (:workspace/root @state/app-db)))
      (expect (= "feature/ws" (get-in @state/app-db [:workspaces 1 :label])))))

  (it "caps workspaces at eight total entries"
    (reset! state/app-db {:title "Main"
                          :render-version 0})
    (dotimes [_ 10]
      (state/dispatch [:create-workspace]))
    (expect (= 8 (count (:workspaces @state/app-db))))
    (expect (= [:main :tab-1 :tab-2 :tab-3 :tab-4 :tab-5 :tab-6 :tab-7]
              (mapv :id (:workspaces @state/app-db))))
    (expect (= :tab-7 (:active-workspace-id @state/app-db))))

  (it "switches the full transcript, draft, prompt history, and session by workspace"
    (reset! state/app-db {:session {:id "main-c"}
                          :messages [{:role :user :text "main prompt"}]
                          :input (input/paste-text (input/empty-input) "main draft")
                          :input-history ["main prompt"]
                          :pastes {}
                          :paste-counter 0
                          :detail-expansions {}
                          :workspaces [{:id :main :label "Main" :active? true}]
                          :active-workspace-id :main
                          :workspace-locals {}
                          :render-version 0})
    (state/dispatch [:create-workspace])
    (state/dispatch [:init-session {:id "tab-c"} [{:role :user :text "tab prompt"}]])
    (state/dispatch [:update-input (input/paste-text (input/empty-input) "tab draft")])
    (state/dispatch [:select-workspace-index 0])
    (expect (= {:id "main-c"} (:session @state/app-db)))
    (expect (= [{:role :user :text "main prompt"}] (:messages @state/app-db)))
    (expect (= "main draft" (input/input->text (:input @state/app-db))))
    (expect (= ["main prompt"] (:input-history @state/app-db)))
    (state/dispatch [:select-workspace-index 1])
    (expect (= {:id "tab-c"} (:session @state/app-db)))
    (expect (= [{:role :user :text "tab prompt"}] (:messages @state/app-db)))
    (expect (= "tab draft" (input/input->text (:input @state/app-db))))
    (expect (= ["tab prompt"] (:input-history @state/app-db))))

  (it "selects workspaces by zero-based index and cycles to the next workspace"
    (reset! state/app-db {:workspaces [{:id :main :label "Main"}
                                       {:id :tab-1 :label "Tab 1" :active? true}
                                       {:id :tab-2 :label "Tab 2"}]
                          :active-workspace-id :tab-1
                          :render-version 0})
    (state/dispatch [:select-workspace-index 0])
    (expect (= :main (:active-workspace-id @state/app-db)))
    (expect (= [{:id :main :label "Main" :active? true}
                {:id :tab-1 :label "Tab 1"}
                {:id :tab-2 :label "Tab 2"}]
              (:workspaces @state/app-db)))
    (state/dispatch [:select-workspace-index :next])
    (expect (= :tab-1 (:active-workspace-id @state/app-db)))
    (state/dispatch [:select-workspace-index :next])
    (expect (= :tab-2 (:active-workspace-id @state/app-db)))
    (state/dispatch [:select-workspace-index :next])
    (expect (= :main (:active-workspace-id @state/app-db)))
    (state/dispatch [:select-workspace-index :prev])
    (expect (= :tab-2 (:active-workspace-id @state/app-db)))
    (state/dispatch [:select-workspace-index 99])
    (expect (= :tab-2 (:active-workspace-id @state/app-db))))

  (it "selects an already-open workspace by session id"
    (reset! state/app-db {:workspaces [{:id :main :label "Main" :active? true}
                                       {:id :tab-1 :label "Tab 1"}]
                          :active-workspace-id :main
                          :session {:id "main-c"}
                          :messages [{:role :user :text "main prompt"}]
                          :input (input/paste-text (input/empty-input) "main draft")
                          :input-history ["main prompt"]
                          :workspace-locals {:tab-1 {:session {:id "tab-c"}
                                                     :messages [{:role :user :text "tab prompt"}]
                                                     :input (input/paste-text (input/empty-input) "tab draft")
                                                     :input-history ["tab prompt"]}}
                          :render-version 0})
    (state/dispatch [:select-workspace-by-session "tab-c"])
    (expect (= :tab-1 (:active-workspace-id @state/app-db)))
    (expect (= {:id "tab-c"} (:session @state/app-db)))
    (expect (= [{:role :user :text "tab prompt"}] (:messages @state/app-db)))
    (state/dispatch [:select-workspace-by-session "missing"])
    (expect (= :tab-1 (:active-workspace-id @state/app-db)))))

(defdescribe init-settings-test
  (it "loads the default balanced reasoning level when config has none"
    (with-redefs [vis/load-config-raw (fn [] {})]
      (state/init!)
      (expect (= :balanced
                (get-in @state/app-db [:settings :reasoning-level])))
      (expect (= :low
                (get-in @state/app-db [:settings :openai-codex-verbosity])))
      (expect (= :vis-light
                (get-in @state/app-db [:settings :theme-name])))
      (expect (not (contains? (:settings @state/app-db) :differentiate-turns)))
      (expect (true?
                (get-in @state/app-db [:settings :mouse-selection-copy])))
      (expect (false?
                (get-in @state/app-db [:settings :voice/respond?])))))

  (it "normalizes low/medium/high aliases from persisted config"
    (with-redefs [vis/load-config-raw (fn [] {:tui-settings {:reasoning-level "HIGH"}})]
      (state/init!)
      (expect (= :deep
                (get-in @state/app-db [:settings :reasoning-level])))))

  (it "normalizes Codex verbosity from persisted config strings"
    (with-redefs [vis/load-config-raw (fn [] {:tui-settings {:openai-codex-verbosity "MEDIUM"}})]
      (state/init!)
      (expect (= :medium
                (get-in @state/app-db [:settings :openai-codex-verbosity])))))

  (it "honours persisted show-thinking choice in either direction"
    ;; Pre-2026-05 policy force-disabled `:show-thinking` on every load —
    ;; even when the user had explicitly enabled it. That made the Settings
    ;; toggle a one-way switch (off-by-default with no usable on) and
    ;; silently overrode persisted user preference. We now honour both
    ;; persisted true and persisted false; coerce non-booleans to bool so
    ;; legacy strings/nils don't escape `:show-thinking`'s contract.
    (with-redefs [vis/load-config-raw (fn [] {:tui-settings {:show-thinking true}})]
      (state/init!)
      (expect (true?
                (get-in @state/app-db [:settings :show-thinking]))))
    (with-redefs [vis/load-config-raw (fn [] {:tui-settings {:show-thinking false}})]
      (state/init!)
      (expect (false?
                (get-in @state/app-db [:settings :show-thinking])))))

  (it "falls back to balanced / low on invalid persisted values"
    (with-redefs [vis/load-config-raw (fn [] {:tui-settings {:reasoning-level :turbo
                                                             :openai-codex-verbosity :loud}})]
      (state/init!)
      (expect (= :balanced
                (get-in @state/app-db [:settings :reasoning-level])))
      (expect (= :low
                (get-in @state/app-db [:settings :openai-codex-verbosity]))))))

(defdescribe settings-shortcut-test
  (it "commits shortcut settings before notification watchers dispatch render bumps"
    (with-redefs [vis/load-config-raw (fn [] {})
                  vis/save-config! (fn [_])
                  vis/get-router (constantly :router)
                  vis/resolve-effective-model (fn [_] {:provider :openai
                                                       :name "gpt-5"
                                                       :reasoning? true})
                  vis/notify! (fn [& _]
                                (state/dispatch [:bump-render-version]))]
      (reset! state/app-db {:settings {:reasoning-level :deep
                                       :openai-codex-verbosity :low}
                            :render-version 0})
      (let [result (future
                     (state/dispatch [:cycle-reasoning-level])
                     :done)]
        (expect (= :done (deref result 1000 :timeout)))
        (expect (= :quick
                  (get-in @state/app-db [:settings :reasoning-level]))))))

  (it "wraps reasoning level from deep back to quick"
    (with-redefs [vis/load-config-raw (fn [] {})
                  vis/save-config! (fn [_])
                  vis/get-router (constantly :router)
                  vis/resolve-effective-model (fn [_] {:provider :openai
                                                       :name "gpt-5"
                                                       :reasoning? true})
                  vis/notify! (fn [& _])]
      (reset! state/app-db {:settings {:reasoning-level :deep
                                       :openai-codex-verbosity :low}
                            :render-version 0})
      (state/dispatch [:cycle-reasoning-level])
      (expect (= :quick
                (get-in @state/app-db [:settings :reasoning-level])))))

  (it "leaves reasoning unchanged for fixed-thinking Z.ai models"
    (let [notified (atom nil)]
      (with-redefs [vis/get-router (constantly :router)
                    vis/resolve-effective-model (fn [_] {:provider :zai
                                                         :name "glm-4.7"
                                                         :reasoning? true
                                                         :reasoning-style :zai-thinking
                                                         :reasoning-effort? false})
                    vis/notify! (fn [text & kvs] (reset! notified [text kvs]))]
        (reset! state/app-db {:settings {:reasoning-level :deep
                                         :openai-codex-verbosity :low}
                              :render-version 0})
        (state/dispatch [:cycle-reasoning-level])
        (expect (= :deep
                  (get-in @state/app-db [:settings :reasoning-level])))
        (expect (= ["Reasoning effort is not configurable for this model" [:level :warn :ttl-ms 1500]]
                  @notified)))))

  (it "leaves Codex verbosity unchanged for non-Codex providers"
    (let [notified (atom nil)]
      (with-redefs [vis/get-router (constantly :router)
                    vis/resolve-effective-model (fn [_] {:provider :zai
                                                         :name "glm-4.7"})
                    vis/notify! (fn [text & kvs] (reset! notified [text kvs]))]
        (reset! state/app-db {:settings {:reasoning-level :balanced
                                         :openai-codex-verbosity :high}
                              :render-version 0})
        (state/dispatch [:cycle-codex-verbosity])
        (expect (= :high
                  (get-in @state/app-db [:settings :openai-codex-verbosity])))
        (expect (= ["Codex verbosity is only available for OpenAI Codex" [:level :warn :ttl-ms 1500]]
                  @notified)))))

  (it "cycles Codex verbosity low -> medium -> high -> low"
    (with-redefs [vis/load-config-raw (fn [] {})
                  vis/save-config! (fn [_])
                  vis/get-router (constantly :router)
                  vis/resolve-effective-model (fn [_] {:provider :openai-codex
                                                       :name "gpt-5.5"
                                                       :reasoning? true})
                  vis/notify! (fn [& _])]
      (reset! state/app-db {:settings {:reasoning-level :balanced
                                       :openai-codex-verbosity :low}
                            :render-version 0})
      (state/dispatch [:cycle-codex-verbosity])
      (expect (= :medium
                (get-in @state/app-db [:settings :openai-codex-verbosity])))
      (state/dispatch [:cycle-codex-verbosity])
      (expect (= :high
                (get-in @state/app-db [:settings :openai-codex-verbosity])))
      (state/dispatch [:cycle-codex-verbosity])
      (expect (= :low
                (get-in @state/app-db [:settings :openai-codex-verbosity]))))))

(defdescribe model-shortcut-test
  (it "cycles the primary provider model, preserves non-provider config, and rebuilds routers"
    (let [saved     (atom nil)
          rebuilt   (atom nil)
          refreshed (atom nil)
          notified  (atom nil)]
      (with-redefs [vis/load-config-raw (fn [] {:tui-settings {:show-thinking false}
                                                :db-spec {:backend :sqlite :path "vis.db"}})
                    vis/save-config! (fn [config] (reset! saved config))
                    vis/reload-config! (fn [] @saved)
                    vis/rebuild-router! (fn [config]
                                          (reset! rebuilt config)
                                          :router)
                    vis/refresh-cached-routers! (fn [router]
                                                  (reset! refreshed router))
                    vis/notify! (fn [text & kvs] (reset! notified [text kvs]))]
        (reset! state/app-db {:config {:providers [{:id :openai
                                                    :models [{:name "gpt-5"}
                                                             {:name "gpt-5-mini"}]}]}
                              :settings {:reasoning-level :balanced
                                         :openai-codex-verbosity :low}
                              :render-version 0})
        (state/dispatch [:cycle-model])
        (expect (= "gpt-5-mini"
                  (get-in @state/app-db [:config :providers 0 :models 0 :name])))
        (expect (= {:tui-settings {:show-thinking false}
                    :db-spec {:backend :sqlite :path "vis.db"}
                    :providers [{:id :openai
                                 :models [{:name "gpt-5-mini"}
                                          {:name "gpt-5"}]}]}
                  @saved))
        (expect (= @saved @rebuilt))
        (expect (= :router @refreshed))
        (expect (= ["Model: gpt-5-mini" [:level :info :ttl-ms 1500]]
                  @notified)))))

  (it "reports when there is no alternate configured model"
    (let [saved    (atom nil)
          notified (atom nil)]
      (with-redefs [vis/load-config-raw (fn [] {})
                    vis/save-config! (fn [config] (reset! saved config))
                    vis/reload-config! (fn [] {})
                    vis/rebuild-router! (fn [_] :router)
                    vis/refresh-cached-routers! (fn [_])
                    vis/notify! (fn [text & kvs] (reset! notified [text kvs]))]
        (reset! state/app-db {:config {:providers [{:id :openai
                                                    :models [{:name "gpt-5"}]}]}
                              :render-version 0})
        (state/dispatch [:cycle-model])
        (expect (nil? @saved))
        (expect (= "gpt-5"
                  (get-in @state/app-db [:config :providers 0 :models 0 :name])))
        (expect (= ["No alternate models configured" [:level :warn :ttl-ms 1500]]
                  @notified)))))

  (it "cycles through models before advancing providers"
    (let [cycle-primary-model @#'state/cycle-primary-model
          config {:providers [{:id :openai-codex
                               :models [{:name "codex-high"}
                                        {:name "codex-low"}]}
                              {:id :zai
                               :models [{:name "glm-4.6"}]}]}
          first-cycle (cycle-primary-model config)
          second-cycle (cycle-primary-model (:config first-cycle)
                         (:cycle-order first-cycle))
          third-cycle (cycle-primary-model (:config second-cycle)
                        (:cycle-order second-cycle))]
      (expect (= :openai-codex
                (get-in first-cycle [:config :providers 0 :id])))
      (expect (= "codex-low"
                (get-in first-cycle [:config :providers 0 :models 0 :name])))
      (expect (= :zai
                (get-in second-cycle [:config :providers 0 :id])))
      (expect (= "glm-4.6"
                (get-in second-cycle [:config :providers 0 :models 0 :name])))
      (expect (= :openai-codex
                (get-in third-cycle [:config :providers 0 :id])))
      (expect (= "codex-high"
                (get-in third-cycle [:config :providers 0 :models 0 :name]))))))

(defdescribe scrollbar-state-test
  (it "maps scrollbar clicks with the same single-cell thumb used by the renderer"
    (let [scroll-to-y-fn (-> #'state/event-registry deref deref (get :scroll-to-y) :fn)
          db            {:messages-scroll nil}]
      (expect (= 155
                (:messages-scroll
                 (scroll-to-y-fn db [:scroll-to-y 28 0 56 360 56]))))
      (expect (= 304
                (:messages-scroll
                 (scroll-to-y-fn db [:scroll-to-y 55 0 56 360 56])))))))

(defdescribe cancel-turn-test
  (it "notifies cancelling instead of relying on footer status"
    (let [cancelled (atom nil)
          notified  (atom nil)]
      (with-redefs [vis/cancel! (fn [token] (reset! cancelled token))
                    vis/notify! (fn [text & kvs] (reset! notified [text kvs]))]
        (reset! state/app-db {:loading? true
                              :cancel-token :token
                              :cancelling? false
                              :render-version 0})
        (state/dispatch [:cancel-turn])
        (expect (= :token @cancelled))
        (expect (true? (:cancelling? @state/app-db)))
        (expect (= ["Cancelling current turn..." [:level :info :ttl-ms 2500]]
                  @notified))))))

(defdescribe live-progress-rate-test
  (it "coalesces reasoning redraws to the 80ms frame cadence and flushes lifecycle chunks"
    (let [make-progress-render-updater @#'state/make-progress-render-updater
          events (atom [])
          now-ms (atom 0)
          update! (make-progress-render-updater
                    #(swap! events conj %)
                    #(long @now-ms))]
      (update! [:t0] {:phase :reasoning})
      (reset! now-ms 79)
      (update! [:t79] {:phase :reasoning})
      (reset! now-ms 80)
      (update! [:t80] {:phase :reasoning})
      (reset! now-ms 81)
      (update! [:done] {:phase :iteration-final})
      (expect (= [[:set-progress-iterations [:t0]]
                  [:set-progress-iterations [:t80]]
                  [:set-progress-iterations [:done]]]
                @events))))

  (it "content stream CANNOT starve reasoning frames — each phase keeps its own throttle clock"
    ;; Regression: before the per-phase clocks, every `:content`
    ;; chunk (which streams per-token alongside `:reasoning`)
    ;; reset the shared throttle, so after the first reasoning
    ;; frame landed the bubble froze on "I" / "The" until the
    ;; terminal `:iteration-final` chunk.
    (let [make-progress-render-updater @#'state/make-progress-render-updater
          events (atom [])
          now-ms (atom 0)
          update! (make-progress-render-updater
                    #(swap! events conj %)
                    #(long @now-ms))]
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
        (expect (= [[:set-progress-iterations [:r 0]]
                    [:set-progress-iterations [:r 80]]]
                  reasoning-events)))))

  (it "content stream is throttled on its own clock and never blocks reasoning"
    (let [make-progress-render-updater @#'state/make-progress-render-updater
          events (atom [])
          now-ms (atom 0)
          update! (make-progress-render-updater
                    #(swap! events conj %)
                    #(long @now-ms))]
      ;; Hammer both streams in lockstep for 200ms.
      (doseq [t (range 0 201 10)]
        (reset! now-ms t)
        (update! [:r (long t)] {:phase :reasoning})
        (update! [:c (long t)] {:phase :content}))
      (let [tag-counts (reduce (fn [m [_ tag]]
                                 (update m (first tag) (fnil inc 0)))
                         {}
                         @events)]
        ;; 200ms / 80ms cadence → frames at t ∈ {0, 80, 160}.
        (expect (= 3 (get tag-counts :r)))
        (expect (= 3 (get tag-counts :c)))))))

(defdescribe send-message-test
  (it "refuses copied assistant transcript dumps before provider dispatch"
    (let [send-message-fn (-> #'state/event-registry deref deref (get :send-message) :fn)
          db              {:session {:id "c1"}
                           :active-workspace-id :main
                           :messages []
                           :messages-scroll 0
                           :input-history []
                           :settings {:reasoning-level :balanced
                                      :openai-codex-verbosity :low}
                           :pastes {}}
          poison          "The user is reporting a bug\n\n▾ RESULT [iteration 1 · block 1]\n..."]
      (with-redefs [input/expand-paste-placeholders (fn [text _] text)]
        (let [{db' :db fx :fx} (send-message-fn db [:send-message poison])]
          (expect (= db db'))
          (expect (= [[:notify "Input looks like copied assistant transcript; not sent" :warn 4000]]
                    fx))))))

  (it "does not send reasoning effort or verbosity for Z.ai fixed-thinking models"
    (let [send-message-fn (-> #'state/event-registry deref deref (get :send-message) :fn)
          db              {:session {:id "c1"}
                           :active-workspace-id :main
                           :messages []
                           :messages-scroll 0
                           :input-history []
                           :settings {:reasoning-level :deep
                                      :openai-codex-verbosity :high}
                           :pastes {}}]
      (with-redefs [input/expand-paste-placeholders (fn [text _] text)
                    input/expand-file-mentions identity
                    vis/cancellation-token (fn [] :token)
                    vis/get-router (fn [] :router)
                    vis/resolve-effective-model (fn [_] {:provider :zai
                                                         :name "glm-4.7"
                                                         :reasoning? true
                                                         :reasoning-style :zai-thinking
                                                         :reasoning-effort? false})]
        (let [{:keys [fx]} (send-message-fn db [:send-message "hello"])
              [event] fx]
          (expect (= [:rlm-turn :main {:id "c1"} "hello" :token nil nil {}]
                    (subvec event 0 8)))
          (expect (nil? (nth event 8)))
          (expect (string? (nth event 9)))))))

  (it "forwards routing trace from turn result to message metadata"
    (let [rlm-turn-fx (-> #'state/fx-registry deref deref (get :rlm-turn))
          received    (atom [])
          trace       [{:provider-id :p1
                        :model "m1"
                        :status 429
                        :reason :transient-error}]]
      (with-redefs [vis/worker-future (fn [_label thunk]
                                        (thunk)
                                        :future)
                    vis/cancellation-set-future! (fn [_token _future])
                    state/dispatch (fn [event]
                                     (swap! received conj event))
                    chat/turn! (fn [_session _text _opts]
                                 {:answer [:ir {} [:p {} [:span {} "ok"]]]
                                  :model "m2"
                                  :provider :p2
                                  :llm-selected {:provider :p1 :model "m1"}
                                  :llm-actual {:provider :p2 :model "m2"}
                                  :llm-fallback? true
                                  :llm-routing-trace trace})]
        (rlm-turn-fx :main {:id "c1"} "hello" :token nil nil {} {} "turn-1")
        (let [[event-id workspace-id _answer metadata] (last @received)]
          (expect (= :message-received event-id))
          (expect (= :main workspace-id))
          (expect (= "m2" (:model metadata)))
          (expect (= :p2 (:provider metadata)))
          (expect (= {:provider :p1 :model "m1"} (:llm-selected metadata)))
          (expect (= {:provider :p2 :model "m2"} (:llm-actual metadata)))
          (expect (true? (:llm-fallback? metadata)))
          (expect (= trace (:llm-routing-trace metadata)))))))

  (it "restores a cancelled prompt to the input instead of rendering a cancelled answer"
    (let [send-message-fn     (-> #'state/event-registry deref deref (get :send-message) :fn)
          reset-input-fn      (-> #'state/event-registry deref deref (get :reset-input) :fn)
          message-received-fn (-> #'state/event-registry deref deref (get :message-received) :fn)
          token              (input/format-paste-placeholder {:id 1 :content "hello"})
          text               (str "edit me " token)
          initial-messages   [{:role :assistant :text "previous"}]
          db                 {:session {:id "c1"}
                              :messages initial-messages
                              :messages-scroll 9
                              :input-history ["prior"]
                              :input-history-index nil
                              :input-history-draft nil
                              :settings {:reasoning-level :balanced
                                         :openai-codex-verbosity :low}
                              :pastes {1 {:id 1 :content "hello"}}
                              :paste-counter 1}]
      (with-redefs [vis/cancellation-token (fn [] :token)]
        (let [sent-db      (:db (send-message-fn db [:send-message text]))
              reset-db     (reset-input-fn sent-db [:reset-input])
              restored-db  (:db (message-received-fn reset-db
                                  [:message-received
                                   [:ir {} [:p {} [:span {} "Cancelled by user."]]]
                                   {:status :cancelled}]))]
          (expect (= initial-messages (:messages restored-db)))
          (expect (= text (input/input->text (:input restored-db))))
          (expect (= {1 {:id 1 :content "hello"}} (:pastes restored-db)))
          (expect (= 1 (:paste-counter restored-db)))
          (expect (= ["prior"] (:input-history restored-db)))
          (expect (false? (:loading? restored-db)))
          (expect (not-any? #(= "Cancelled by user." (:text %))
                    (:messages restored-db))))))))

(defdescribe pending-send-queue-test
  (it "keeps queued submissions on their workspace snapshot"
    (let [enqueue-fn (-> #'state/event-registry deref deref (get :enqueue-message) :fn)
          db         {:active-workspace-id :b
                      :input-history []
                      :pastes {}
                      :paste-counter 0
                      :workspace-locals {:a {:session {:id "a"}
                                             :loading? true
                                             :pending-sends []
                                             :input-history []
                                             :pastes {1 {:id 1 :content "payload"}}
                                             :paste-counter 1}}}
          result     (enqueue-fn db [:enqueue-message "queued" :a])
          queued     (get-in result [:db :workspace-locals :a :pending-sends])]
      (expect (= ["queued"] (mapv :text queued)))
      (expect (= {1 {:id 1 :content "payload"}} (:pastes (first queued))))
      (expect (empty? (:pending-sends (:db result))))))

  (it "schedules queue drain as an effect after message commit"
    (let [message-received-fn (-> #'state/event-registry deref deref (get :message-received) :fn)
          pending-id          "turn-1"
          db                  {:active-workspace-id :main
                               :session {:id "c1"}
                               :loading? true
                               :messages [{:role :user :text "first" :client-turn-id pending-id}
                                          {:role :assistant :pending? true :client-turn-id pending-id}]
                               :progress {:iterations []}
                               :pending-sends [{:text "second"
                                                :pastes {}
                                                :paste-counter 0}]}
          {:keys [db fx]}      (message-received-fn db
                                 [:message-received :main
                                  [:ir {} [:p {} [:span {} "ok"]]]
                                  {:client-turn-id pending-id}])]
      (expect (= [[:dispatch [:drain-pending :main]]] fx))
      (expect (false? (:loading? db)))
      (expect (= ["second"] (mapv :text (:pending-sends db))))))

  (it "drains one queued item without nested provider dispatch"
    (let [drain-fn       (-> #'state/event-registry deref deref (get :drain-pending) :fn)
          db             {:active-workspace-id :main
                          :pending-sends [{:text "second"
                                           :pastes {2 {:id 2 :content "p"}}
                                           :paste-counter 2}
                                          {:text "third"
                                           :pastes {}
                                           :paste-counter 0}]}
          {:keys [db fx]} (drain-fn db [:drain-pending :main])]
      (expect (= [[:dispatch [:send-message "second" :main]]] fx))
      (expect (= ["third"] (mapv :text (:pending-sends db))))
      (expect (= {2 {:id 2 :content "p"}} (:pastes db)))
      (expect (= 2 (:paste-counter db))))))
