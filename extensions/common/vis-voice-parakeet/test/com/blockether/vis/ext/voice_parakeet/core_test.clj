(ns com.blockether.vis.ext.voice-parakeet.core-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.voice-parakeet.core :as voice]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe voice-parakeet-core-test
  (it "registers a TUI channel hook with voice commands"
    (let [hooks (vis/channel-hooks-for :tui)]
      (expect (some #(= :voice/parakeet (:hook-id %)) hooks))
      (let [commands (voice/tui-commands {})]
        (expect (= [:voice-parakeet/start :voice-parakeet/stop :voice-parakeet/cancel]
                  (mapv :id commands)))
        (expect (every? ifn? (map :run-fn commands)))))))
