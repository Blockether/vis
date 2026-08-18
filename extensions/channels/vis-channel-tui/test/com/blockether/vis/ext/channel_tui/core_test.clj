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
        ;; The TUI is reached through the root gateway flags like any other command;
        ;; its usage line is where a user looks for that.
        (expect (re-find #"--gateway HOST\[:PORT\] --gateway-token TOKEN" tui/tui-usage))
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

;; Regression: this projection lumped `error` and `notice` together, so it
;; glued the machine code to the front of the message (`**provider_generic**
;; Provider unavailable` — the code ran into the card's headline) and shouted
;; `**turn_cancelled**` at a human who had just pressed Esc.
(defdescribe render-for-tui-error-test
             (it "keeps the machine code on its own line, message in the next paragraph"
                 (expect (= "**provider_unroutable**\n\nNo provider could take this request"
                            (tui/render-for-tui [{"id" "b1"
                                                  "type" "error"
                                                  "code" "provider_unroutable"
                                                  "message"
                                                  "No provider could take this request"}]))))
             (it "prints a notice's sentence alone, without its machine code"
                 (expect (= "Cancelled by user."
                            (tui/render-for-tui [{"id" "b1"
                                                  "type" "notice"
                                                  "code" "turn_cancelled"
                                                  "message" "Cancelled by user."}])))))
