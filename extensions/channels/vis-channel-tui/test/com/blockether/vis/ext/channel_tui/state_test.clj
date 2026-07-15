(ns com.blockether.vis.ext.channel-tui.state-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.scroll :as scroll]
            [com.blockether.vis.ext.channel-tui.state :as state]
            [com.blockether.vis.ext.channel-tui.virtual :as virtual]
            [lazytest.core :refer [defdescribe expect it]]))

;; The `:openai-codex/verbosity` enum toggle is registered by the OpenAI Codex
;; PROVIDER extension in production (it lives next to the backend it tunes), a
;; module this channel-tui test suite does not load. Register it here so the
;; `:settings` projection and the verbosity-cycle events resolve against a real
;; `:enum` toggle, mirroring production.
(vis/register-toggle! {:id :openai-codex/verbosity
                       :label "Verbosity"
                       :description "Output detail hint passed to the OpenAI Codex backend."
                       :type :enum
                       :choices [:low :medium :high]
                       :default :low
                       :owner :vis
                       :group :provider
                       :persist? true
                       :settings? false
                       :visible-fn (fn []
                                     (boolean (vis/has-provider? :openai-codex)))})

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
                 (expect (nil? (vis/toggle-spec :vis/show-thinking)))
                 (expect (nil? (vis/toggle-spec :vis/show-timestamps)))))

(defdescribe
  detail-toggle-test
  (it "does not cold-clear render and height caches on disclosure click"
      (let [render-invalidations
            (atom 0)

            height-invalidations
            (atom 0)]

        (with-redefs [render/invalidate-cache!
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
    (let [external-input-fn
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
      (expect (false? (get-in next-db [:tab-locals :first :slash-command-hidden?]))))))

(defdescribe
  resync-toggle-settings-test
  (it "busts BOTH render caches so a registry toggle (show-thinking) repaints without a restart"
      ;; Regression: flipping a registry-only toggle (e.g. `:vis/show-thinking`)
      ;; resolved live in the registry but the painter kept handing back
      ;; cached bubble lines (`render/fmt-cache`, keyed on message identity)
      ;; and stale row counts (the `virtual` height cache, whose
      ;; `settings-fingerprint` doesn't track registry-only toggles). The
      ;; new value only showed after a process restart cleared the caches.
      ;; The toggles-registry listener dispatches `:resync-toggle-settings`,
      ;; which must now drop both caches.
      (let [render-invalidations
            (atom 0)

            height-invalidations
            (atom 0)]

        (with-redefs [render/invalidate-cache!
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

(defdescribe
  transcript-dump-guard-test
  (it "detects copied assistant trace dumps"
      (expect (true? (state/transcript-dump-input?
                       "The user is reporting a bug\n\n▾ RESULT [iteration 1 · block 1]\n...")))
      (expect (false? (state/transcript-dump-input?
                        "Dobra, wróć do (def snapshot-result (v/snapshot))"))))
  (it "filters poisoned transcript dumps out of resumed input history"
      (let [poison "The user is reporting a bug\n\n▾ RESULT [iteration 1 · block 1]\n..."]
        (reset! state/app-db {:render-version 0})
        (state/dispatch [:init-session {:id "c1"}
                         [{:role :user :text "safe prompt"} {:role :assistant :text "ok"}
                          {:role :user :text poison}]])
        (expect (= ["safe prompt"] (:input-history @state/app-db))))))

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
      (let [workspace
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
      (state/dispatch [:set-workspace {:id "ws-1" :root "/tmp/new"}])
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
  (it "tab switch clears the stale :layout and strips :scroll :pos (scroll-jump regression)"
      ;; The leaving tab's :layout (total-h/offsets) and the eased on-screen :pos
      ;; are derived/display state anchored to the OLD document. restore-tab drops
      ;; both so the first post-switch frame recomputes layout for THIS tab and
      ;; re-resolves the scroll offset, instead of clamping against a foreign
      ;; total-h (the visible \"jump to bottom\").
      (reset! state/app-db {:session {:id "main-c"}
                            :messages [{:role :user :text "main prompt"}]
                            :scroll {:mode :at :offset 40 :pos 900}
                            :layout {:total-h 5000 :offsets [0 100 900]}
                            :tabs [{:id :main :label "Main" :active? true}
                                   {:id :tab-1 :label "Tab 1"}]
                            :active-tab-id :main
                            :tab-locals {:tab-1 {:session {:id "tab-c"}
                                                 :messages [{:role :user :text "tab prompt"}]
                                                 :scroll {:mode :at :offset 5 :pos 7}}}
                            :render-version 0})
      (state/dispatch [:select-tab-index 1])
      (expect (nil? (:layout @state/app-db)))
      (expect (= {:mode :at :offset 5} (:scroll @state/app-db))))
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
      (with-redefs [vis/load-config-raw (fn []
                                          {})]
        (state/init!)
        (expect (= :balanced (get-in @state/app-db [:settings :reasoning-level])))
        (expect (= :low (get-in @state/app-db [:settings :openai-codex-verbosity])))
        (expect (= :blockether-light (get-in @state/app-db [:settings :theme-name])))
        (expect (not (contains? (:settings @state/app-db) :differentiate-turns)))
        (expect (true? (get-in @state/app-db [:settings :mouse-selection-copy])))
        (expect (false? (get-in @state/app-db [:settings :voice/respond])))))
  (it "hydrates persisted enum toggles into the registry"
      ;; The persistence shape now lives under `:toggles`, not
      ;; `:tui-settings`. `state/init!` keeps the `:settings`
      ;; projection coherent by pulling each migrated toggle's value
      ;; off the registry. (In production `screen/run-chat!` runs
      ;; hydration AFTER `init!` and then dispatches
      ;; `:resync-toggle-settings` — see the regression test below.)
      (vis/toggles-hydrate-from-config! {:toggles {:vis/reasoning-level :deep}})
      (try (with-redefs [vis/load-config-raw (fn []
                                               {})]
             (state/init!)
             (expect (= :deep (get-in @state/app-db [:settings :reasoning-level]))))
           (finally (vis/toggle-reset-to-default! :vis/reasoning-level))))
  (it "resync repairs the projection when hydration runs AFTER init! (production order)"
      ;; Regression: `screen/run-chat!` calls `state/init!` FIRST — projecting
      ;; registry DEFAULTS into `:settings` — and only THEN hydrates the toggles
      ;; from config, followed by a `:resync-toggle-settings` dispatch. Without
      ;; that resync the footer keeps showing the default (`balanced`) while the
      ;; real toggle holds the persisted value, so the first Ctrl+X r cycle
      ;; advances the toggle only up to the already-displayed level and appears
      ;; to do nothing.
      (try (with-redefs [vis/load-config-raw (fn []
                                               {})]
             (state/init!)                              ;; projects default :balanced
             (vis/toggles-hydrate-from-config!          ;; toggle -> persisted :quick
               {:toggles {:vis/reasoning-level :quick}})
             (expect (= :balanced                       ;; stale projection, pre-resync
                        (get-in @state/app-db [:settings :reasoning-level])))
             (state/dispatch [:resync-toggle-settings]) ;; the fix
             (expect (= :quick (get-in @state/app-db [:settings :reasoning-level]))))
           (finally (vis/toggle-reset-to-default! :vis/reasoning-level))))
  (it "hydrates Codex verbosity from the toggles registry"
      (vis/toggles-hydrate-from-config! {:toggles {:openai-codex/verbosity :medium}})
      (try (with-redefs [vis/load-config-raw (fn []
                                               {})]
             (state/init!)
             (expect (= :medium (get-in @state/app-db [:settings :openai-codex-verbosity]))))
           (finally (vis/toggle-reset-to-default! :openai-codex/verbosity))))
  (it "drops invalid persisted enum values back to registered defaults"
      ;; `hydrate-from-config!` routes through `set-value!` which
      ;; validates against `:choices`. Invalid entries are silently
      ;; skipped — the registered default stands.
      (vis/toggles-hydrate-from-config! {:toggles {:vis/reasoning-level :turbo
                                                   :openai-codex/verbosity :loud}})
      (try (with-redefs [vis/load-config-raw (fn []
                                               {})]
             (state/init!)
             (expect (= :balanced (get-in @state/app-db [:settings :reasoning-level])))
             (expect (= :low (get-in @state/app-db [:settings :openai-codex-verbosity]))))
           (finally (vis/toggle-reset-to-default! :vis/reasoning-level)
                    (vis/toggle-reset-to-default! :openai-codex/verbosity)))))

(defdescribe
  settings-shortcut-test
  (it "commits shortcut settings before notification watchers dispatch render bumps"
      ;; The cycle event mutates the toggle registry; the cached
      ;; `:settings` projection is rebuilt synchronously in the same
      ;; FX :db so notification listeners observe the new value the
      ;; moment they fire.
      (vis/toggle-set-value! :vis/reasoning-level :deep)
      (try (with-redefs [vis/load-config-raw
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

             (reset! state/app-db {:settings {:reasoning-level :deep :openai-codex-verbosity :low}
                                   :render-version 0})
             (let [result (future (state/dispatch [:cycle-reasoning-level]) :done)]
               (expect (= :done (deref result 1000 :timeout)))
               (expect (= :quick (vis/toggle-value :vis/reasoning-level)))
               (expect (= :quick (get-in @state/app-db [:settings :reasoning-level])))))
           (finally (vis/toggle-reset-to-default! :vis/reasoning-level))))
  (it "wraps reasoning level from deep back to quick"
      (vis/toggle-set-value! :vis/reasoning-level :deep)
      (try (with-redefs [vis/load-config-raw
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

             (reset! state/app-db {:settings {:reasoning-level :deep :openai-codex-verbosity :low}
                                   :render-version 0})
             (state/dispatch [:cycle-reasoning-level])
             (expect (= :quick (vis/toggle-value :vis/reasoning-level)))
             (expect (= :quick (get-in @state/app-db [:settings :reasoning-level]))))
           (finally (vis/toggle-reset-to-default! :vis/reasoning-level))))
  (it "leaves reasoning unchanged for fixed-thinking Z.ai models"
      (let [notified (atom nil)]
        (with-redefs [vis/get-router (constantly :router)
                      vis/resolve-effective-model (fn [_]
                                                    {:provider :zai
                                                     :name "glm-4.7"
                                                     :reasoning? true
                                                     :reasoning-style :zai-thinking
                                                     :reasoning-effort? false})
                      vis/notify! (fn [text & kvs]
                                    (reset! notified [text kvs]))]

          (reset! state/app-db {:settings {:reasoning-level :deep :openai-codex-verbosity :low}
                                :render-version 0})
          (state/dispatch [:cycle-reasoning-level])
          (expect (= :deep (get-in @state/app-db [:settings :reasoning-level])))
          (expect (= ["Reasoning effort is not configurable for this model"
                      [:level :warn :ttl-ms 1500]]
                     @notified)))))
  (it "leaves Codex verbosity unchanged for non-Codex providers"
      (let [notified (atom nil)]
        (with-redefs [vis/get-router (constantly :router)
                      vis/resolve-effective-model (fn [_]
                                                    {:provider :zai :name "glm-4.7"})
                      vis/notify! (fn [text & kvs]
                                    (reset! notified [text kvs]))]

          (reset! state/app-db {:settings {:reasoning-level :balanced :openai-codex-verbosity :high}
                                :render-version 0})
          (state/dispatch [:cycle-codex-verbosity])
          (expect (= :high (get-in @state/app-db [:settings :openai-codex-verbosity])))
          (expect (= ["Codex verbosity is only available for OpenAI Codex"
                      [:level :warn :ttl-ms 1500]]
                     @notified)))))
  (it
    "cycles Codex verbosity low -> medium -> high -> low"
    (with-redefs [vis/load-config-raw
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
      (vis/toggle-reset-to-default! :openai-codex/verbosity)
      (try (reset! state/app-db {:settings {:reasoning-level :balanced :openai-codex-verbosity :low}
                                 :render-version 0})
           (state/dispatch [:cycle-codex-verbosity])
           (expect (= :medium (get-in @state/app-db [:settings :openai-codex-verbosity])))
           (state/dispatch [:cycle-codex-verbosity])
           (expect (= :high (get-in @state/app-db [:settings :openai-codex-verbosity])))
           (state/dispatch [:cycle-codex-verbosity])
           (expect (= :low (get-in @state/app-db [:settings :openai-codex-verbosity])))
           (finally (vis/toggle-reset-to-default! :openai-codex/verbosity))))))

(defdescribe
  model-shortcut-test
  ;; Ctrl+T sets the ACTIVE SESSION's persisted model preference (the shared,
  ;; channel-neutral store the web + engine read) instead of reordering global
  ;; config. Fresh sessions start with no explicit pref, so the first press
  ;; advances from the displayed router default to the next configured entry.
  (it "fresh session advances from displayed router default to the next configured model"
      (let [set-calls
            (atom [])

            notified
            (atom nil)]

        (with-redefs [vis/configured-providers
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
        (with-redefs [vis/configured-providers (fn []
                                                 [{:id :openai
                                                   :models [{:name "gpt-5"} {:name "gpt-5-mini"}]}
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
      (let [set-calls
            (atom [])

            notified
            (atom nil)]

        (with-redefs [vis/db-info
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
             (let [scroll-to-y-fn (-> #'state/event-registry
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
  (let [ev (fn [k]
             (-> #'state/event-registry
                 deref
                 deref
                 (get k)
                 :fn))]
    (it "scroll-up parks (mode :at) so streaming follow hands off"
        (let [r ((ev :scroll-up) {:scroll scroll/follow} [:scroll-up 9 200 100])]
          ;; max-s = 100; ease from the bottom (100) up to 100-9 = 91.
          (expect (= :at (:mode (:scroll r))))
          (expect (= 91 (:offset (:scroll r))))))
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
    (it "set-scroll snap-parks at an exact row (search jump)"
        (let [r ((ev :set-scroll) {:scroll scroll/follow} [:set-scroll 42])]
          (expect (= {:mode :at :offset 42} (:scroll r)))))
    (it "reanchor-scroll shifts the parked offset + pos by the same delta"
        ;; Content above the anchor shrank by 450 rows as estimates resolved;
        ;; the anchored message must stay visually put.
        (let [r ((ev :reanchor-scroll)
                  {:scroll {:mode :at :offset 1840 :pos 1849}}
                  [:reanchor-scroll 1399 -450])]
          (expect (= {:mode :at :offset 1390 :pos 1399} (:scroll r)))))
    (it "message-received re-pins to a CLEAN FOLLOW (no dangling ease target)"
        ;; Regression (/workspace list "flash to top then bottom"): a result
        ;; lands atomically while an ease was in flight. Replacing the whole
        ;; `:scroll` with FOLLOW means nothing can dangle, so the view snaps
        ;; cleanly to the bottom instead of animating up from row 0 first.
        (let [message-received-fn (ev :message-received)
              pending-id "turn-1"
              db {:active-tab-id :main
                  :session {:id "c1"}
                  :loading? true
                  :messages [{:role :user :text "/workspace list" :client-turn-id pending-id}
                             {:role :assistant :pending? true :client-turn-id pending-id}]
                  :progress {:iterations []}
                  ;; An ease was in flight from the prior frame.
                  :scroll {:mode :follow :pos 80}}
              {db' :db} (message-received-fn db
                                             [:message-received :main
                                              [:ir {} [:p {} [:span {} "a big table"]]]
                                              {:client-turn-id pending-id}])]

          (expect (= scroll/follow (:scroll db')))))
    (it "send-message re-pins to a CLEAN FOLLOW"
        (let [send-message-fn (ev :send-message)
              db {:session {:id "c1"}
                  :active-tab-id :main
                  :messages []
                  :input-history []
                  :scroll {:mode :at :offset 80 :pos 80}
                  :settings {:reasoning-level :balanced :openai-codex-verbosity :low}
                  :pastes {}}]

          (with-redefs [input/expand-paste-placeholders (fn [text _]
                                                          text)
                        input/expand-file-mentions identity
                        vis/cancellation-token (fn []
                                                 :token)]

            (let [{db' :db} (send-message-fn db [:send-message "hello"])]
              (expect (= scroll/follow (:scroll db')))))))))

(defdescribe cancel-turn-test
             (it "notifies cancelling instead of relying on footer status"
                 (let [cancelled
                       (atom nil)

                       notified
                       (atom nil)]

                   (with-redefs [vis/cancel!
                                 (fn [token]
                                   (reset! cancelled token))

                                 vis/notify!
                                 (fn [text & kvs]
                                   (reset! notified [text kvs]))]

                     (reset! state/app-db
                       {:loading? true :cancel-token :token :cancelling? false :render-version 0})
                     (state/dispatch [:cancel-turn])
                     (expect (= :token @cancelled))
                     (expect (true? (:cancelling? @state/app-db)))
                     (expect (= ["Cancelling current turn..." [:level :info :ttl-ms 2500]]
                                @notified))))))

(defdescribe cancel-reaches-gateway-after-send-test
             ;; REGRESSION (user report): cancel a running turn, then send a correction.
             ;; The correction landed in BOTH the transcript AND the gateway queue, then
             ;; sat there until the "cancelled" turn finished on its own — because the
             ;; cancel never reached the daemon. Root cause: a plain `:send-message` turn
             ;; has no `:gateway-turn-id` yet (it's minted server-side), and
             ;; `:sync-turn-clock` used to drop the id the `turn.started` chunk carries.
             ;; So `:cancel-turn`'s `(when (and sid tid) …)` short-circuited and the
             ;; daemon kept running. Fix: `:sync-turn-clock` late-binds the id.
             (it
               "cancel of a send-message turn reaches the gateway once turn.started arrives"
               (let [cancelled-gateway (atom nil)]
                 (with-redefs [vis/cancel! (fn [_]
                                             nil)
                               vis/notify! (fn [_ & _]
                                             nil)
                               vis/gateway-cancel-turn! (fn [sid tid]
                                                          (reset! cancelled-gateway [sid tid])
                                                          {:status "cancelling"})]

                   ;; State AFTER `:send-message` submitted a fresh turn: loading, but
                   ;; the gateway turn id is not known yet.
                   (reset! state/app-db {:session {:id "s1"}
                                         :active-tab-id "s1"
                                         :render-version 0
                                         :loading? true
                                         :cancel-token :token
                                         :cancelling? false
                                         :gateway-turn-id nil
                                         :turn-start-ms 10})
                   ;; BEFORE turn.started: a cancel can't reach the gateway — the id is
                   ;; unknown (the turn isn't assigned server-side yet either).
                   (state/dispatch [:cancel-turn])
                   (expect (nil? @cancelled-gateway))
                   ;; turn.started lands, projected as a :turn-start chunk with the id.
                   (state/dispatch [:sync-turn-clock nil {:turn-id "gw-turn-1" :started-at-ms 123}])
                   (expect (= "gw-turn-1" (:gateway-turn-id @state/app-db)))
                   ;; NOW the cancel reaches the daemon with the real (sid, tid).
                   (reset! cancelled-gateway nil)
                   (state/dispatch [:cancel-turn])
                   (expect (= ["s1" "gw-turn-1"] @cancelled-gateway))))))

(defdescribe
  cancel-turn-stale-gateway-test
  (it
    "clears stale cancelling state when gateway turn is already terminal"
    (let [cancelled
          (atom nil)

          cancelled-gateway
          (atom nil)

          notified
          (atom nil)]

      (with-redefs [vis/cancel!
                    (fn [token]
                      (reset! cancelled token))

                    vis/gateway-cancel-turn!
                    (fn [sid tid]
                      (reset! cancelled-gateway [sid tid])
                      {:error :not-running :status "interrupted"})

                    vis/notify!
                    (fn [text & kvs]
                      (reset! notified [text kvs]))]

        (reset! state/app-db {:session {:id "s1"}
                              :loading? true
                              :cancel-token :token
                              :gateway-turn-id "turn-1"
                              :cancelling? true
                              :progress {:iterations []}
                              :turn-start-ms 10
                              :render-version 0})
        (state/dispatch [:cancel-turn])
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
  cancel-self-heal-test
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
  (let [heal-fn (-> #'state/event-registry
                    deref
                    deref
                    (get :cancel-self-heal-tick)
                    :fn)]
    (it "no-ops while the pending cancel is younger than the timeout"
        (with-redefs [vis/cancel! (fn [_]
                                    (throw (ex-info "self-heal must not fire early" {})))]
          (let [db {:active-tab-id :main
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
          (with-redefs [vis/cancel! (fn [tok]
                                      (reset! cancelled tok))]
            (let [db {:active-tab-id :main
                      :session {:id "s1"}
                      :loading? true
                      :cancel-token :token
                      :cancelling? true
                      :progress {:iterations []}
                      :turn-start-ms 10
                      :cancelling-at-ms 1000}
                  ;; 8.5s elapsed > 8s timeout
                  {db' :db fx :fx} (heal-fn db [:cancel-self-heal-tick 9500])]

              ;; Local token re-fired (tears down any lingering attach waiter).
              (expect (= :token @cancelled))
              ;; Turn state fully cleared → input flows again.
              (expect (false? (:cancelling? db')))
              (expect (false? (:loading? db')))
              (expect (nil? (:cancel-token db')))
              (expect (nil? (:cancelling-at-ms db')))
              ;; The user is told, and with no authored backlog nothing is restored.
              (expect (some #(= :notify (first %)) fx))
              (expect (not-any? #(= :dispatch (first %)) fx))))))
    (it "restores the authored backlog to the editor when it self-heals"
        (with-redefs [vis/cancel! (fn [_]
                                    nil)]
          (let [db {:active-tab-id :main
                    :session {:id "s1"}
                    :loading? true
                    :cancel-token :token
                    :cancelling? true
                    :cancelling-at-ms 0
                    :pending-sends [{:text "my correction" :client-id "c1"}]}
                {fx :fx} (heal-fn db [:cancel-self-heal-tick 20000])]

            ;; The correction the user typed during the cancel comes back
            ;; to the editor rather than being silently dropped.
            (expect (some #{[:dispatch [:restore-pending-to-input :main]]} fx)))))
    (it "never fires when no cancel is pending, even with a stale timestamp"
        (with-redefs [vis/cancel! (fn [_]
                                    (throw (ex-info "self-heal must not fire when idle" {})))]
          (let [db {:active-tab-id :main :loading? true :cancelling? false :cancelling-at-ms 0}
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
      (with-redefs [vis/worker-future
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
      (with-redefs [vis/worker-future
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
      (with-redefs [vis/worker-future
                    (fn [_ _]
                      (future nil))

                    vis/cancellation-set-future!
                    (fn [_ _]
                      nil)]

        (reset! state/app-db
          {:session {:id "s1"}
           :active-tab-id "s1"
           ;; The gateway queue snapshot can arrive before its :add event binds
           ;; this local echo. It is the running turn, not a second queued one.
           :pending-sends [{:text "hello" :client-id "local-echo"}]
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
          (expect (not (some #(= "hello" (:text %)) (:pending-sends db))))))))

(defdescribe
  live-progress-rate-test
  (it "coalesces reasoning redraws to the 80ms frame cadence and flushes lifecycle chunks"
      (let [make-progress-render-updater
            @#'state/make-progress-render-updater

            events
            (atom [])

            now-ms
            (atom 0)

            update!
            (make-progress-render-updater #(swap! events conj %) #(long @now-ms))]

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
      (let [make-progress-render-updater
            @#'state/make-progress-render-updater

            events
            (atom [])

            now-ms
            (atom 0)

            update!
            (make-progress-render-updater #(swap! events conj %) #(long @now-ms))]

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
      (let [make-progress-render-updater
            @#'state/make-progress-render-updater

            events
            (atom [])

            now-ms
            (atom 0)

            update!
            (make-progress-render-updater #(swap! events conj %) #(long @now-ms))]

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

(defdescribe
  reasoning-sentence-buffer-test
  (let [clip
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
        (let [streaming
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
    (let [make-progress-render-updater
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
    (let [make-progress-render-updater
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
            (let [fut (reify
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
      (let [make-progress-render-updater
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
  (it "refuses copied assistant transcript dumps before provider dispatch"
      (let [send-message-fn
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
             :settings {:reasoning-level :balanced :openai-codex-verbosity :low}
             :pastes {}}

            poison
            "The user is reporting a bug\n\n▾ RESULT [iteration 1 · block 1]\n..."]

        (with-redefs [input/expand-paste-placeholders (fn [text _]
                                                        text)]
          (let [{db' :db fx :fx} (send-message-fn db [:send-message poison])]
            (expect (= db db'))
            (expect (= [[:notify "Input looks like copied assistant transcript; not sent" :warn
                         4000]]
                       fx))))))
  (it
    "does not send reasoning effort or verbosity for Z.ai fixed-thinking models"
    (let [send-message-fn
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
           :settings {:reasoning-level :deep :openai-codex-verbosity :high}
           :pastes {}}]

      (with-redefs [input/expand-paste-placeholders
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

        (let [{:keys [fx]}
              (send-message-fn db [:send-message "hello"])

              [event]
              fx]

          (expect (= [:session-turn :main {:id "c1"} "hello" :token nil nil {}] (subvec event 0 8)))
          (expect (nil? (nth event 8)))
          (expect (string? (nth event 9)))))))
  (it
    "forwards routing trace from turn result to message metadata"
    (let [session-turn-fx
          (-> #'state/fx-registry
              deref
              deref
              (get :session-turn))

          received
          (atom [])

          trace
          [{:provider-id :p1 :model "m1" :status 429 :reason :transient-error}]]

      (with-redefs [vis/worker-future
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
                      {:answer [:ir {} [:p {} [:span {} "ok"]]]
                       :model "m2"
                       :provider :p2
                       :llm-selected {:provider :p1 :model "m1"}
                       :llm-actual {:provider :p2 :model "m2"}
                       :llm-fallback? true
                       :llm-routing-trace trace})]

        (session-turn-fx :main {:id "c1"} "hello" :token nil nil {} {} "turn-1")
        ;; The turn also dispatches workspace re-sync + live F2 ctx-panel
        ;; refreshes after the answer commits, so don't assume
        ;; :message-received is the *last* event — select it explicitly.
        (let [[event-id workspace-id _answer metadata] (->> @received
                                                            (filter #(= :message-received
                                                                        (first %)))
                                                            last)]
          (expect (= :message-received event-id))
          (expect (= :main workspace-id))
          (expect (= "m2" (:model metadata)))
          (expect (= :p2 (:provider metadata)))
          (expect (= {:provider :p1 :model "m1"} (:llm-selected metadata)))
          (expect (= {:provider :p2 :model "m2"} (:llm-actual metadata)))
          (expect (true? (:llm-fallback? metadata)))
          (expect (= trace (:llm-routing-trace metadata)))))))
  (it
    "restores a cancelled prompt to the input instead of rendering a cancelled answer"
    (let [send-message-fn
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
           :settings {:reasoning-level :balanced :openai-codex-verbosity :low}
           :pastes {1 {:id 1 :content "hello"}}
           :paste-counter 1}]

      (with-redefs [vis/cancellation-token (fn []
                                             :token)]
        (let [sent-db (:db (send-message-fn db [:send-message text]))
              reset-db (reset-input-fn sent-db [:reset-input])
              restored-db (:db (message-received-fn
                                 reset-db
                                 [:message-received [:ir {} [:p {} [:span {} "Cancelled by user."]]]
                                  {:status :cancelled}]))]

          (expect (= initial-messages (:messages restored-db)))
          (expect (= text (input/input->text (:input restored-db))))
          (expect (= {1 {:id 1 :content "hello"}} (:pastes restored-db)))
          (expect (= 1 (:paste-counter restored-db)))
          (expect (= ["prior"] (:input-history restored-db)))
          (expect (false? (:loading? restored-db)))
          (expect (not-any? #(= "Cancelled by user." (:text %)) (:messages restored-db))))))))

(defdescribe
  pending-send-queue-test
  (it "keeps queued submissions on their workspace snapshot"
      (let [enqueue-fn
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
        (expect (= {1 {:id 1 :content "payload"}} (:pastes (first queued))))
        (expect (empty? (:pending-sends (:db result))))))
  (it "never queues a submission while a cancel is in flight (:cancelling?)"
      ;; REGRESSION: pressing Esc to cancel, then typing a new message, parked that
      ;; message in the queue (`:pending-sends`) behind the turn being torn down —
      ;; "I cancel and write something else and I get it in the queue". A submission
      ;; during the cancel window is a FRESH intent: it must NOT be queued (and must
      ;; never fire :gateway-enqueue). The submit path keeps the text in the editor.
      (let [enqueue-fn
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
      (let [message-received-fn
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
                                 [:message-received :main [:ir {} [:p {} [:span {} "ok"]]]
                                  {:client-turn-id pending-id}])]

        (expect (= [[:dispatch [:drain-pending :main]]] fx))
        (expect (false? (:loading? db)))
        (expect (= ["second"] (mapv :text (:pending-sends db))))))
  (it "drains one queued item without nested provider dispatch"
      (let [drain-fn
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
                 (let [history-up-fn
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
                 (let [history-up-fn
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
                 (let [history-up-fn
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
      (let [message-received-fn
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
             :pending-sends [{:text "second" :pastes {} :paste-counter 0 :client-id "c1"}]}

            {:keys [db fx]}
            (message-received-fn db
                                 [:message-received :main
                                  [:ir {} [:p {} [:span {} "Cancelled by user."]]]
                                  {:status :cancelled :client-turn-id pending-id}])]

        (expect (= [[:dispatch [:restore-pending-to-input :main]]] fx))
        (expect (false? (:loading? db)))
        ;; queue survives the commit; the follow-up fx clears + restores it.
        (expect (= ["second"] (mapv :text (:pending-sends db))))))
  (it "restore-pending-to-input appends queued prompts and deletes gateway records"
      (let [restore-fn
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
             :pending-sends
             [{:text "second" :pastes {} :paste-counter 0 :turn-id "t-2" :client-id "c1"}
              {:text "third" :pastes {} :paste-counter 0 :turn-id "t-3" :client-id "c1"}]}

            {:keys [db fx]}
            (restore-fn db [:restore-pending-to-input :main])]

        (expect (= "second\n\nthird" (input/input->text (:input db))))
        (expect (empty? (:pending-sends db)))
        (expect (some #{[:gateway-delete-queued "c1" "t-2"]} fx))
        (expect (some #{[:gateway-delete-queued "c1" "t-3"]} fx))))
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
      (let [send-fn
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
  (it "set-queued-turn-id deletes the orphaned gateway turn when the entry is gone"
      ;; The OTHER half of the "sent AND queued" race: the enqueue registered a
      ;; SERVER-SIDE queued turn, then a cancel restored the backlog to the editor
      ;; (dropping the local entry) BEFORE the turn-id round-trip landed. When the
      ;; late `:set-queued-turn-id` finds no matching client-id, the record is
      ;; orphaned and would auto-drain (= silently SEND); it must be deleted.
      (let [bind-fn
            (-> #'state/event-registry
                deref
                deref
                (get :set-queued-turn-id)
                :fn)

            db
            {:active-tab-id :main :session {:id "c1"} :pending-sends []}

            {:keys [db fx]}
            (bind-fn db [:set-queued-turn-id :main "gone-client" "t-9"])]

        (expect (= [[:gateway-delete-queued "c1" "t-9"]] fx))
        (expect (empty? (:pending-sends db)))))
  (it "set-queued-turn-id binds the turn id when the entry still exists"
      (let [bind-fn
            (-> #'state/event-registry
                deref
                deref
                (get :set-queued-turn-id)
                :fn)

            db
            {:active-tab-id :main
             :session {:id "c1"}
             :pending-sends [{:text "second" :client-id "c1c"}]}

            {:keys [db fx]}
            (bind-fn db [:set-queued-turn-id :main "c1c" "t-2"])]

        (expect (nil? fx))
        (expect (= "t-2" (:turn-id (first (:pending-sends db))))))))

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
               (let [make
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
                     {:iterations [{:thinking "hm"
                                    :forms [{:code "git_status()" :success? true}]}]}]

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
                 (state/dispatch [:message-received (vis/markdown->ir "Could not reach provider")
                                  {:status :error :client-turn-id "t1"}])
                 (let [db @state/app-db]
                   (expect (false? (:loading? db)))
                   (expect (false? (:cancelling? db)))
                   (expect (nil? (:cancel-token db)))
                   ;; the pending assistant bubble was resolved, not left dangling
                   (expect (not (some #(and (= :assistant (:role %)) (true? (:pending? %)))
                                      (:messages db)))))))

(defdescribe sync-queued-turn-test
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
             (it "binds an unbound local echo by text instead of duplicating"
                 (reset! state/app-db {:session {:id "s1"}
                                       :active-tab-id "s1"
                                       :render-version 0
                                       :pending-sends [{:text "hello" :client-id "c1"}]})
                 (state/dispatch [:sync-queued-turn nil {:op :add :turn-id "q1" :text "hello"}])
                 (let [q (:pending-sends @state/app-db)]
                   (expect (= 1 (count q)))
                   (expect (= "q1" (:turn-id (first q)))))))

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
                 (with-redefs [vis/worker-future
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

(defdescribe restore-pending-ownership-test
             ;; A cancel pulls back ONLY the entries this tab authored (:client-id from
             ;; enqueue). Mirrored sibling entries (no :client-id) must survive: deleting
             ;; them fired turn.queued.deleted at the sibling still blocked on its own
             ;; queued turn, which synthesized a spurious CANCELLED terminal there.
             (it "restores authored entries, keeps sibling mirrors queued"
                 (with-redefs [vis/gateway-delete-queued-turn! (fn [_ _]
                                                                 nil)]
                   (reset! state/app-db {:session {:id "s1"}
                                         :render-version 0
                                         :pending-sends
                                         [{:text "mine" :client-id "c1" :turn-id "q1"}
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
  (let [close-tab
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
        (let [{:keys [fx]} (close-tab (base {:session {:id "shared"}
                                             :tab-locals {:tab-1 {:session {:id "shared"}}}})
                                      :main)]
          (expect (= [] fx))))
    (it "a session with a running turn is left alone (option b)"
        (let [{:keys [fx]} (close-tab (base {:session {:id "busy"} :loading? true}) :main)]
          ;; Closing disowns the project membership, but a busy runtime stays alive.
          (expect (= [[:unassign-session-project "busy"]] fx))))
    (it "a session with queued/pending sends is left alone"
        (let [{:keys [fx]}
              (close-tab (base {:session {:id "queued"} :pending-sends [{:text "later"}]}) :main)]
          ;; Queued work also prevents runtime/listener release.
          (expect (= [[:unassign-session-project "queued"]] fx))))
    (it "closing the last remaining tab is a no-op (no release)"
        (let [{:keys [db fx]} (close-tab {:tabs [{:id :main :active? true}]
                                          :active-tab-id :main
                                          :session {:id "solo"}
                                          :tab-locals {}
                                          :render-version 0}
                                         :main)]
          (expect (nil? fx))
          (expect (= [:main] (mapv :id (:tabs db))))))))
