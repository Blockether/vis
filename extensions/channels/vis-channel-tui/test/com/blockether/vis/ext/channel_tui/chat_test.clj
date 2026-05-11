(ns com.blockether.vis.ext.channel-tui.chat-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.internal.extension :as extension]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe rebuild-history-test)

(defdescribe rebuild-history-renders-answer-test
  (it "resumed assistant message routes the stored IR answer through render-answer"
    ;; Regression for convo b7ba1d93: resume path used to pass the
    ;; persisted answer straight into the bubble without going through
    ;; the channel renderer chokepoint. Both live and resume paths now
    ;; share `chat/render-answer`, which dispatches via the
    ;; `:channel/messages-renderer-fn` registered by `channel-tui.core`.
    ;;
    ;; STRICT: the V1 schema persists answers as Nippy-frozen canonical
    ;; IR `[:ir & nodes]`; `db-list-conversation-turns` thaws them on
    ;; read. The resume path receives IR by contract — strings here
    ;; would be a programmer bug and must throw.
    (with-redefs [vis/db-info (fn [] :db)
                  vis/db-list-conversation-turns
                  (fn [_db _cid]
                    [{:id :turn-1
                      :user-request "siema"
                      :answer [:ir {} [:p {} [:span {} "Siema! 👋 What can I do for you?"]]]}])
                  vis/db-list-conversation-turn-iterations
                  (fn [_db _turn-id] [])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            assistant (second history)
            ;; Pre-projection bubble carries `:ir` only — `:text` is
            ;; computed lazily by the walker (`virtual.clj` projection)
            ;; or by clipboard via `vis/render :markdown`. The test
            ;; renders explicitly to assert the IR round-trip.
            ir   (:ir assistant)
            text (vis/render ir :markdown)]
        (expect (vector? ir))
        (expect (= :ir (first ir)))
        (expect (str/includes? text "Siema!")))))

  (it "rebuild-history rejects persisted non-IR answers"
    (with-redefs [vis/db-info (fn [] :db)
                  vis/db-list-conversation-turns
                  (fn [_db _cid]
                    [{:id :turn-bad
                      :user-request "siema"
                      :answer "not ir"}])
                  vis/db-list-conversation-turn-iterations
                  (fn [_db _turn-id] [])]
      (expect (= [] ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")))))

  (it "render-answer throws on raw-string input (strict IR contract)"
    (expect
      (try ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/render-answer))
            "raw markdown string")
        false
        (catch clojure.lang.ExceptionInfo _ true))))

  (it "render-answer accepts nil as the empty placeholder"
    (expect (= "" ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/render-answer)) nil))))

  (it "rebuilds tool-result details from canonical op envelope keys"
    (let [tool-out (extension/success
                     {:op :v/bash
                      :result {:exit 0 :stdout "ok\n" :stderr ""}
                      :metadata {:command "echo ok"
                                 :cwd "."
                                 :target {:path "."}}})]
      (with-redefs [extension/channel-render-tool-result (fn [_] "rendered tool")
                    vis/db-info (fn [] :db)
                    vis/db-list-conversation-turns
                    (fn [_db _cid]
                      [{:id :turn-1
                        :user-request "run"
                        :answer [:ir {}]}])
                    vis/db-list-conversation-turn-iterations
                    (fn [_db _turn-id]
                      [{:id :iter-1}])
                    vis/db-list-iteration-blocks
                    (fn [_db _iteration-id]
                      [{:code "(v/bash \"echo ok\")"
                        :result tool-out}])]
        (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
              trace   (-> history second :traces first)]
          (expect (= [:tool] (:result-kinds trace)))
          (expect (= {:op/symbol :v/bash
                      :op/tag :op.tag/action
                      :command "echo ok"
                      :cwd "."
                      :target {:path "."}
                      :stdout "ok\n"
                      :stderr ""}
                    (first (:result-details trace)))))))))

(defdescribe turn-options-test
  (it "forwards reasoning-default and extra-body to vis/send!"
    ;; STRICT contract: `vis/send!` returns canonical answer-IR; the
    ;; bubble layer renders at the boundary, never here.
    (let [seen (atom nil)
          ir   [:ir {} [:p {} [:span {} "ok"]]]]
      (with-redefs [vis/send! (fn [_id _text opts]
                                (reset! seen opts)
                                {:answer ir})]
        (let [result (chat/turn! {:id "c1"} "hello"
                       {:reasoning-default :deep
                        :extra-body {:text {:verbosity "high"}}})]
          (expect (= ir (:answer result)))
          (expect (= 1 (:iteration-count result)))
          (expect (= :deep (:reasoning-default @seen)))
          (expect (= {:text {:verbosity "high"}} (:extra-body @seen)))))))

  (it "returns canonical IR when cancellation is raised as an exception"
    (with-redefs [vis/send! (fn [& _] (throw (InterruptedException. "cancel")))
                  vis/cancellation? (fn [_] true)]
      (let [result (chat/turn! {:id "c1"} "hello")]
        (expect (= :cancelled (:status result)))
        (expect (= :ir (first (:answer result))))))))
