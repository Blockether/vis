(ns com.blockether.vis.ext.channel-tui.state-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.state :as state]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe init-settings-test
  (it "loads the default balanced reasoning level when config has none"
    (with-redefs [vis/load-config-raw (fn [] {})]
      (state/init!)
      (expect (= :balanced
                (get-in @state/app-db [:settings :reasoning-level])))))

  (it "normalizes low/medium/high aliases from persisted config"
    (with-redefs [vis/load-config-raw (fn [] {:tui-settings {:reasoning-level "HIGH"}})]
      (state/init!)
      (expect (= :deep
                (get-in @state/app-db [:settings :reasoning-level])))))

  (it "falls back to balanced on invalid persisted reasoning values"
    (with-redefs [vis/load-config-raw (fn [] {:tui-settings {:reasoning-level :turbo}})]
      (state/init!)
      (expect (= :balanced
                (get-in @state/app-db [:settings :reasoning-level]))))))
