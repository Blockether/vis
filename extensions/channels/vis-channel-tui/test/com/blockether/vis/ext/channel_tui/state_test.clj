(ns com.blockether.vis.ext.channel-tui.state-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.state :as state]
            [com.blockether.vis.ext.channel-tui.virtual :as virtual]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe detail-toggle-test
  (it "does not cold-clear render and height caches on disclosure click"
    (let [render-invalidations (atom 0)
          height-invalidations (atom 0)]
      (with-redefs [render/invalidate-cache! (fn [] (swap! render-invalidations inc))
                    virtual/invalidate-heights! (fn [] (swap! height-invalidations inc))]
        (reset! state/app-db {:detail-expansions {}
                              :render-version 0})
        (state/dispatch [:toggle-detail "cid" "answer:t11111111:proofs:d1"])
        (expect (= {["cid" "answer:t11111111:proofs:d1"] true}
                  (:detail-expansions @state/app-db)))
        (expect (zero? @render-invalidations))
        (expect (zero? @height-invalidations))))))

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
      (expect (true?
                (get-in @state/app-db [:settings :differentiate-turns])))
      (expect (true?
                (get-in @state/app-db [:settings :mouse-selection-copy])))))

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

  (it "falls back to balanced / low on invalid persisted values"
    (with-redefs [vis/load-config-raw (fn [] {:tui-settings {:reasoning-level :turbo
                                                             :openai-codex-verbosity :loud}})]
      (state/init!)
      (expect (= :balanced
                (get-in @state/app-db [:settings :reasoning-level])))
      (expect (= :low
                (get-in @state/app-db [:settings :openai-codex-verbosity]))))))

(defdescribe settings-shortcut-test
  (it "cycles reasoning level and persists the updated TUI settings"
    (let [saved    (atom nil)
          notified (atom nil)]
      (with-redefs [vis/load-config-raw (fn [] {:kept true})
                    vis/save-config! (fn [config] (reset! saved config))
                    vis/get-router (constantly :router)
                    vis/resolve-effective-model (fn [_] {:provider :openai
                                                         :name "gpt-5"
                                                         :reasoning? true})
                    vis/notify! (fn [text & kvs] (reset! notified [text kvs]))]
        (reset! state/app-db {:settings {:reasoning-level :quick
                                         :openai-codex-verbosity :low}
                              :render-version 0})
        (state/dispatch [:cycle-reasoning-level])
        (expect (= :balanced
                  (get-in @state/app-db [:settings :reasoning-level])))
        (expect (= {:kept true
                    :tui-settings {:theme-name :vis-light
                                   :show-thinking true
                                   :show-iterations true
                                   :reasoning-level :balanced
                                   :openai-codex-verbosity :low
                                   :show-timestamps false
                                   :differentiate-turns true
                                   :mouse-selection-copy true}}
                  @saved))
        (expect (= ["Reasoning: balanced" [:level :info :ttl-ms 1500]]
                  @notified)))))

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

(defdescribe send-message-test
  (it "keeps @mentions compact in chat while expanding them for the agent"
    (let [send-message-fn (-> #'state/event-registry deref deref (get :send-message) :fn)
          db              {:conversation {:id "c1"}
                           :messages []
                           :messages-scroll 9
                           :input-history []
                           :settings {:reasoning-level :balanced
                                      :openai-codex-verbosity :high}
                           :pastes {1 {:id 1 :content "PASTED"}}}]
      (with-redefs [input/expand-paste-placeholders (fn [text _] (str text " +paste"))
                    input/expand-file-mentions (fn [text] (str text " +file"))
                    vis/cancellation-token (fn [] :token)
                    vis/get-router (fn [] :router)
                    vis/resolve-effective-model (fn [_] {:provider :openai-codex
                                                         :reasoning? true})]
        (let [{:keys [db fx]} (send-message-fn db [:send-message "see @src/core.clj"])]
          (expect (= "see @src/core.clj +paste"
                    (-> db :messages first :text)))
          (expect (= "see @src/core.clj +paste"
                    (last (:input-history db))))
          (expect (= [[:rlm-turn {:id "c1"}
                       "see @src/core.clj +paste +file"
                       :token
                       :balanced
                       {:text {:verbosity "high"}}]]
                    fx))))))

  (it "does not send reasoning effort or verbosity for Z.ai fixed-thinking models"
    (let [send-message-fn (-> #'state/event-registry deref deref (get :send-message) :fn)
          db              {:conversation {:id "c1"}
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
        (let [{:keys [fx]} (send-message-fn db [:send-message "hello"])]
          (expect (= [[:rlm-turn {:id "c1"} "hello" :token nil nil]] fx))))))

  (it "restores a cancelled prompt to the input instead of rendering a cancelled answer"
    (let [send-message-fn     (-> #'state/event-registry deref deref (get :send-message) :fn)
          reset-input-fn      (-> #'state/event-registry deref deref (get :reset-input) :fn)
          message-received-fn (-> #'state/event-registry deref deref (get :message-received) :fn)
          token              (input/format-paste-placeholder {:id 1 :content "hello"})
          text               (str "edit me " token)
          initial-messages   [{:role :assistant :text "previous"}]
          db                 {:conversation {:id "c1"}
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
              restored-db  (message-received-fn reset-db
                             [:message-received "Cancelled by user." {:status :cancelled}])]
          (expect (= initial-messages (:messages restored-db)))
          (expect (= text (input/input->text (:input restored-db))))
          (expect (= {1 {:id 1 :content "hello"}} (:pastes restored-db)))
          (expect (= 1 (:paste-counter restored-db)))
          (expect (= ["prior"] (:input-history restored-db)))
          (expect (false? (:loading? restored-db)))
          (expect (not-any? #(= "Cancelled by user." (:text %))
                    (:messages restored-db))))))))
