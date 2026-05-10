(ns com.blockether.vis.ext.channel-tui.chat-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.internal.extension :as extension]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe rebuild-history-test
  (it "renders every persisted block when resuming (no silent elision)"
    ;; Regression: previously blocks whose stored `:result` was
    ;; `:vis/silent` were filtered out of the resumed trace, so the
    ;; reader landed on bindings whose definition was invisible. The
    ;; whole silent mechanism has been removed - every executed block
    ;; survives reload. Only the answer-bearing block is elided.
    (with-redefs [vis/db-info (fn [] :db)
                  vis/db-list-conversation-turns
                  (fn [db conversation-id]
                    (expect (= :db db))
                    (expect (= "c1" conversation-id))
                    [{:id :turn-1
                      :user-request "set title, then inspect"
                      :answer "done"}])
                  vis/db-list-conversation-turn-iterations
                  (fn [db turn-id]
                    (expect (= :db db))
                    (expect (= :turn-1 turn-id))
                    [{:id :iter-1 :thinking "thinking"}])
                  vis/db-list-iteration-blocks
                  (fn [db iter-id]
                    (expect (= :db db))
                    (expect (= :iter-1 iter-id))
                    [{:code "(conversation-title \"Demo\")"
                      :result "Demo"
                      :stdout ""
                      :duration-ms 1}
                     {:code "(+ 1 2)"
                      :result 3
                      :stdout ""
                      :duration-ms 2}
                     {:code "(answer \"done\")"
                      :result :vis/answer
                      :stdout ""
                      :duration-ms 3}])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            assistant (second history)
            trace-entry (first (:traces assistant))]
        ;; The (answer ...) form is still elided so the assistant
        ;; doesn't double-render its own answer prose. Everything
        ;; else - including the conversation-title call - shows up.
        (expect (= ["(conversation-title \"Demo\")" "(+ 1 2)"] (:code trace-entry)))
        (expect (= 2 (count (:results trace-entry))))
        (expect (= [true true] (:successes trace-entry))))))

  (it "renders persisted tool results through the new channel-render dispatcher"
    (let [tool-result {:success? true
                       :result {:lines ["alpha" "beta"]}
                       :info {:op :v/cat}}]
      (with-redefs [vis/db-info (fn [] :db)
                    extension/tool-result? (fn [x] (= :v/cat (get-in x [:info :op])))
                    extension/channel-render-tool-result (fn [result chan-id]
                                                           (expect (= :channel-tui chan-id))
                                                           (expect (= tool-result result))
                                                           "1: alpha\n2: beta")
                    vis/db-list-conversation-turns
                    (fn [_db _conversation-id]
                      [{:id :turn-1 :user-request "cat" :answer ""}])
                    vis/db-list-conversation-turn-iterations
                    (fn [_db _turn-id] [{:id :iter-1 :thinking nil}])
                    vis/db-list-iteration-blocks
                    (fn [_db _iter-id]
                      [{:code "(v/cat \"src/demo.clj\")"
                        :result tool-result
                        :stdout ""
                        :duration-ms 1}])]
        (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
              trace-entry (-> history second :traces first)]
          (expect (= [:tool] (:result-kinds trace-entry)))
          (expect (= ["1: alpha\n2: beta"] (:results trace-entry)))))))

  (it "projects persisted turn cost into resumed assistant messages"
    (with-redefs [vis/db-info (fn [] :db)
                  vis/db-list-conversation-turns
                  (fn [db conversation-id]
                    (expect (= :db db))
                    (expect (= "c1" conversation-id))
                    [{:id :turn-1
                      :user-request "hello"
                      :answer "ok"
                      :provider :zai-coding
                      :model "glm-5.1"
                      :input-tokens 120
                      :output-tokens 30
                      :reasoning-tokens 7
                      :cached-tokens 4
                      :total-cost 0.0123}])
                  vis/db-list-conversation-turn-iterations
                  (fn [db turn-id]
                    (expect (= :db db))
                    (expect (= :turn-1 turn-id))
                    [])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            assistant (second history)]
        (expect (= {:total-cost 0.0123
                    :provider :zai-coding
                    :model "glm-5.1"}
                  (:cost assistant)))
        (expect (= {:input 120 :output 30 :reasoning 7 :cached 4}
                  (:tokens assistant)))))))

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

  (it "render-answer throws on raw-string input (strict IR contract)"
    (expect
      (try ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/render-answer))
            "raw markdown string")
        false
        (catch clojure.lang.ExceptionInfo _ true))))

  (it "render-answer accepts nil as the empty placeholder"
    (expect (= "" ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/render-answer)) nil)))))

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
          (expect (= {:text {:verbosity "high"}} (:extra-body @seen))))))))
