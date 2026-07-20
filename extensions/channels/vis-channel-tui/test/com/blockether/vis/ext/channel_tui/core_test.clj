(ns com.blockether.vis.ext.channel-tui.core-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.core :as tui]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  tui-channel-registration-test
  (it "registers a lightweight TUI channel descriptor"
      (let
        [channel (-> tui/tui-extension
                     :ext/channels
                     first)]
        (expect (= "channel-tui" (:ext/name tui/tui-extension)))
        (expect (= :tui (:channel/id channel)))
        (expect (= "tui" (:channel/cmd channel)))
        (expect (= tui/tui-usage (:channel/usage channel)))
        (expect (true? (:channel/owns-tty? channel)))
        (expect (ifn? (:channel/main-fn channel)))))
  (it "defers the full screen namespace until channel-main runs"
      (let [calls (atom [])]
        (with-redefs
          [clojure.core/requiring-resolve
           (fn [sym]
             (swap! calls conj sym)
             (expect (= 'com.blockether.vis.ext.channel-tui.screen/channel-main sym))
             (fn [args]
               {:screen-args args}))]
          (expect (= [] @calls))
          (expect (= {:screen-args ["--resume"]} (tui/channel-main ["--resume"])))
          (expect (= ['com.blockether.vis.ext.channel-tui.screen/channel-main] @calls)))))
  (it
    "loads the screen ns when --session-id resolves through the gateway facade"
    (let
      [resolve-calls
       (atom [])

       init-calls
       (atom 0)

       soul-calls
       (atom [])

       exit-calls
       (atom [])]

      (with-redefs
        [clojure.core/requiring-resolve
         (fn [sym]
           (swap! resolve-calls conj sym)
           (fn [args]
             {:screen-args args}))

         vis/init!
         (fn []
           (swap! init-calls inc))

         vis/shutdown!
         (fn []
           nil)

         vis/gateway-soul
         (fn [id]
           (swap! soul-calls conj id)
           {"id" id "channel" "tui"})

         vis/gateway-list-sessions
         (fn [_]
           [])

         com.blockether.vis.ext.channel-tui.core/exit-not-found!
         (fn [cid]
           (swap! exit-calls conj cid))]

        (expect (= {:screen-args ["--session-id" "abcd1234"]}
                   (tui/channel-main ["--session-id" "abcd1234"]))))
      (expect (= 1 @init-calls))
      (expect (= ["abcd1234"] @soul-calls))
      (expect (= [] @exit-calls))
      (expect (= ['com.blockether.vis.ext.channel-tui.screen/channel-main] @resolve-calls))))
  (it
    "skips loading the screen ns when --session-id misses"
    (let
      [resolve-calls
       (atom [])

       init-calls
       (atom 0)

       shutdown-calls
       (atom 0)

       exit-calls
       (atom [])]

      (with-redefs
        [clojure.core/requiring-resolve
         (fn [sym]
           (swap! resolve-calls conj sym)
           (fn [_]
             nil))

         vis/init!
         (fn []
           (swap! init-calls inc))

         vis/shutdown!
         (fn []
           (swap! shutdown-calls inc))

         vis/gateway-soul
         (fn [_]
           nil)

         vis/gateway-list-sessions
         (fn [_]
           [])

         com.blockether.vis.ext.channel-tui.core/exit-not-found!
         (fn [cid]
           (swap! exit-calls conj cid))]

        (tui/channel-main ["--session-id" "deadbeef"]))
      ;; The screen channel-main MUST NOT have been required on the miss path.
      (expect (= [] @resolve-calls))
      (expect (= 1 @init-calls))
      (expect (= ["deadbeef"] @exit-calls)))))