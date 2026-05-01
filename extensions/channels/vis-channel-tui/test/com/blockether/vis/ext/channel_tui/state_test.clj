(ns com.blockether.vis.ext.channel-tui.state-test
  (:require [com.blockether.vis.core :as vis]
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
