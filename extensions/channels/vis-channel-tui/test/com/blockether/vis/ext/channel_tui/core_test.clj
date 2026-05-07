(ns com.blockether.vis.ext.channel-tui.core-test
  (:require [com.blockether.vis.ext.channel-tui.core :as tui]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe tui-channel-registration-test
  (it "registers a lightweight TUI channel descriptor"
    (let [channel (-> tui/tui-extension :ext/channels first)]
      (expect (= 'com.blockether.vis.ext.channel-tui.core
                (:ext/namespace tui/tui-extension)))
      (expect (= :tui (:channel/id channel)))
      (expect (= "tui" (:channel/cmd channel)))
      (expect (= tui/tui-usage (:channel/usage channel)))
      (expect (true? (:channel/owns-tty? channel)))
      (expect (ifn? (:channel/main-fn channel)))))

  (it "defers the full screen namespace until channel-main runs"
    (let [calls (atom [])]
      (with-redefs [clojure.core/requiring-resolve
                    (fn [sym]
                      (swap! calls conj sym)
                      (expect (= 'com.blockether.vis.ext.channel-tui.screen/channel-main sym))
                      (fn [args]
                        {:screen-args args}))]
        (expect (= [] @calls))
        (expect (= {:screen-args ["--resume"]}
                  (tui/channel-main ["--resume"])))
        (expect (= ['com.blockether.vis.ext.channel-tui.screen/channel-main] @calls))))))
