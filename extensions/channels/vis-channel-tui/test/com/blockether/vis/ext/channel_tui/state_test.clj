(ns com.blockether.vis.ext.channel-tui.state-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.state :as state]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe init-settings-test
  (it "loads the default balanced reasoning level when config has none"
    (with-redefs [vis/load-config-raw (fn [] {})]
      (state/init!)
      (expect (= :balanced
                (get-in @state/app-db [:settings :reasoning-level])))
      (expect (= :low
                (get-in @state/app-db [:settings :openai-codex-verbosity])))))

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
                    vis/resolve-effective-model (fn [_] {:provider :openai-codex})]
        (let [{:keys [db fx]} (send-message-fn db [:send-message "see @src/core.clj"])]
          (expect (= "see @src/core.clj +paste"
                    (-> db :messages first :text)))
          (expect (= "see @src/core.clj +paste"
                    (last (:input-history db))))
          (expect (= [[:rlm-query {:id "c1"}
                       "see @src/core.clj +paste +file"
                       :token
                       :balanced
                       {:text {:verbosity "high"}}]]
                    fx)))))))
