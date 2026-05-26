(ns com.blockether.vis.ext.channel-tui.chat-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.internal.extension :as extension]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe rebuild-history-test)

(defdescribe rebuild-history-renders-answer-test
  (it "resumed assistant message routes the stored IR answer through render-answer"
    ;; Regression for session b7ba1d93: resume path used to pass the
    ;; persisted answer straight into the bubble without going through
    ;; the channel renderer chokepoint. Both live and resume paths now
    ;; share `chat/render-answer`, which dispatches via the
    ;; `:channel/messages-renderer-fn` registered by `channel-tui.core`.
    ;;
    ;; Normal persisted answers are Nippy-frozen canonical IR; the
    ;; legacy/string terminal-answer fallback is covered below.
    (with-redefs [vis/db-info (fn [] :db)
                  vis/db-list-session-turns
                  (fn [_db _cid]
                    [{:id :turn-1
                      :user-request "siema"
                      :answer-markdown "Siema! 👋 What can I do for you?"}])
                  vis/db-list-session-turn-iterations
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

  (it "rebuild-history derives IR from the raw Markdown answer source"
    (with-redefs [vis/db-info (fn [] :db)
                  vis/db-list-session-turns
                  (fn [_db _cid]
                    [{:id :turn-legacy
                      :user-request "siema"
                      :answer-markdown "Cancelled by user."}])
                  vis/db-list-session-turn-iterations
                  (fn [_db _turn-id] [])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            assistant (second history)
            ir (:ir assistant)]
        (expect (= 2 (count history)))
        (expect (= :ir (first ir)))
        (expect (str/includes? (vis/render ir :markdown) "Cancelled by user")))))

  (it "rebuild-history shows cancelled status text when persisted answer is blank"
    (with-redefs [vis/db-info (fn [] :db)
                  vis/db-list-session-turns
                  (fn [_db _cid]
                    [{:id :turn-cancelled
                      :user-request "no live"
                      :prior-outcome :cancelled
                      :answer-markdown ""}])
                  vis/db-list-session-turn-iterations
                  (fn [_db _turn-id] [])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            assistant (second history)
            ir (:ir assistant)]
        (expect (= :cancelled (:status assistant)))
        (expect (str/includes? (vis/render ir :markdown) "Cancelled by user")))))

  (it "rebuild-history marks persisted silent system calls for the TUI visibility toggle"
    (with-redefs [vis/db-info (fn [] :db)
                  vis/db-list-session-turns
                  (fn [_db _cid]
                    [{:id :turn-1
                      :user-request "siema"
                      :answer-markdown "Siema!"}])
                  vis/db-list-session-turn-iterations
                  (fn [_db _turn-id]
                    [{:id :iter-1
                      :code "(set-session-title! \"Greeting\")"
                      :result :vis/silent}])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            trace   (-> history second :traces first)
            form    (-> trace :forms first)]
        (expect (= 1 (count (:forms trace))))
        (expect (true? (:silent? form)))
        (expect (str/includes? (str (:code form)) "set-session-title!")))))

  (it "rebuild-history elides synthetic preflight blocks so they don't render as success"
    (with-redefs [vis/db-info (fn [] :db)
                  vis/db-list-session-turns
                  (fn [_db _cid]
                    [{:id :turn-1
                      :user-request "preflight loop"
                      :answer-markdown ""}])
                  vis/db-list-session-turn-iterations
                  (fn [_db _turn-id]
                    [{:id :iter-1
                      :code "(vis/preflight-error :raw-markdown-fence-leak)"}])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            trace   (-> history second :traces first)]
        (expect (= [] (:forms trace))))))

  (it "rebuild-history preserves mixed-block render segments instead of eliding the answer block"
    ;; New shape: iteration row carries per-form envelopes under :forms.
    ;; Each envelope has :src :tag :result :error :channel; the rebuild
    ;; iterates them rather than treating the whole iteration as one
    ;; opaque block.
    (with-redefs [vis/db-info (fn [] :db)
                  vis/db-list-session-turns
                  (fn [_db _cid]
                    [{:id :turn-1
                      :user-request "mixed"
                      :answer-markdown "Done"}])
                  vis/db-list-session-turn-iterations
                  (fn [_db _turn-id]
                    [{:id :iter-1
                      :code (str "(def x 1)\n"
                              "(set-session-title! \"Mixed\")\n"
                              "(done [:ir [:p \"Done\"]])")
                      :forms [{:scope "t1/i1/f1" :tag :host :src "(def x 1)" :result nil}
                              {:scope "t1/i1/f2" :tag :host :src "(set-session-title! \"Mixed\")" :result :vis/silent}
                              {:scope "t1/i1/f3" :tag :host :src "(done [:ir [:p \"Done\"]])" :result :vis/answer}]}])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            trace   (-> history second :traces first)]
        ;; The answer form is elided per the existing answer-position
        ;; rule; the def + title-set! survive as form records and the
        ;; iteration-level :recaps surface the title summary derived
        ;; from the full iteration source.
        (expect (pos? (count (:forms trace))))
        (expect (some #(re-find #"Mixed" (str %)) (:recaps trace))))))

  (it "rebuild-history prefers durable channel render over runtime-ref placeholder"
    ;; `(def x (v/cat ...))` persists the live var value as
    ;; `{:vis/ref :expr}` (not safely serializable), but the tool call's
    ;; channel-rendered text is durable and should be shown on resume.
    ;; In the new shape the channel sink rides on the form envelope.
    (with-redefs [vis/db-info (fn [] :db)
                  vis/db-list-session-turns
                  (fn [_db _cid]
                    [{:id :turn-1
                      :user-request "read file"
                      :answer-markdown ""}])
                  vis/db-list-session-turn-iterations
                  (fn [_db _turn-id]
                    [{:id :iter-1
                      :code "(def prompt-lines (v/cat \"src/foo.clj\"))"
                      :forms [{:scope "t1/i1/f1"
                               :tag   :host
                               :src   "(def prompt-lines (v/cat \"src/foo.clj\"))"
                               :result {:vis/ref :expr}
                               :channel [{:position 0
                                          :form "(v/cat \"src/foo.clj\")"
                                          :success? true
                                          :result "Read `src/foo.clj` — 10 line(s)."}]}]}])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            trace   (-> history second :traces first)
            rendered (-> trace :forms first :result-render)]
        (expect (str/includes? (str rendered) "Read `src/foo.clj`"))
        (expect (not (str/includes? (str rendered) "<runtime value"))))))

  (it "rebuild-history recovers single visible form duration from old iteration rows"
    ;; Historical envelopes lacked per-form :duration-ms. The row-level
    ;; eval duration is still available; if only one form remains after
    ;; answer elision, copy it onto that form so restored green footers
    ;; show the same right-side duration as live progress.
    (with-redefs [vis/db-info (fn [] :db)
                  vis/db-list-session-turns
                  (fn [_db _cid]
                    [{:id :turn-1
                      :user-request "patch"
                      :answer-markdown ""}])
                  vis/db-list-session-turn-iterations
                  (fn [_db _turn-id]
                    [{:id :iter-1
                      :duration-ms 12
                      :code "(v/patch [])"
                      :forms [{:scope "t24/i1/f1"
                               :tag :mutation
                               :src "(v/patch [])"
                               :result :ok
                               :channel [{:position 0
                                          :form "(v/patch [])"
                                          :success? true
                                          :result "PATCH ok"}]}]}])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            form    (-> history second :traces first :forms first)]
        (expect (= "t24/i1/f1" (:scope form)))
        (expect (= 12 (:duration-ms form))))))

  (it "rebuild-history surfaces per-form errors as errors (not successes) and keeps the tool tag"
    ;; Regression from session 11d4f817: the rebuild path used to fold
    ;; the WHOLE iteration into one synthetic block and read non-existent
    ;; iteration-level `:result` / `:error` / `:channel` keys off the
    ;; persisted row. Errored forms restored as successful and tool tags
    ;; (`:observation` / `:mutation` plus the pi-style preview IR)
    ;; vanished. Acceptance criterion is that the restored bubble looks
    ;; identical to the live one, so we iterate the `:forms` envelope
    ;; vec the loop persists and project each form into the same
    ;; record shape `progress/chunk->form-result` builds live.
    (with-redefs [vis/db-info (fn [] :db)
                  vis/db-list-session-turns
                  (fn [_db _cid]
                    [{:id :turn-1 :user-request "two forms" :answer-markdown ""}])
                  vis/db-list-session-turn-iterations
                  (fn [_db _turn-id]
                    [{:id :iter-1
                      :code (str "(v/cat \"src/foo.clj\")\n(v/cat \"ghost.clj\")")
                      :forms [{:scope "t1/i1/f1"
                               :tag :observation
                               :src "(v/cat \"src/foo.clj\")"
                               :result {:vis.op :v/cat :path "src/foo.clj"}
                               :channel [{:position 0
                                          :form "(v/cat \"src/foo.clj\")"
                                          :success? true
                                          :result "CAT src/foo.clj"}]}
                              {:scope "t1/i1/f2"
                               :tag :observation
                               :src "(v/cat \"ghost.clj\")"
                               :error {:message "file not found: ghost.clj"}}]}])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            forms   (-> history second :traces first :forms)
            [f1 f2] forms]
        ;; The persisted bubble surfaces ONE record per form, not one
        ;; opaque block for the whole iteration.
        (expect (= 2 (count forms)))
        ;; Form 1: succeeded; tool tag and preview IR present.
        (expect (true?  (:success? f1)))
        (expect (= :tool (:result-kind f1)))
        (expect (str/includes? (str (:result-render f1)) "CAT src/foo.clj"))
        ;; Form 2: errored; the rebuild MUST surface it as an error
        ;; instead of papering over with success (the original bug).
        (expect (false? (:success? f2)))
        (expect (= :error (:result-kind f2)))
        (expect (some? (:error f2))))))

  (it "rebuild-history renders legacy runtime-ref payloads as a non-restorable label"
    ;; Historical sessions may still carry `{:vis/ref :expr}` from the
    ;; old definition_state rebuild path. Cross-turn def rehydration
    ;; is gone; render the sentinel as a small static label rather than
    ;; pretending we can restore it.
    (with-redefs [vis/db-info (fn [] :db)
                  vis/db-list-session-turns
                  (fn [_db _cid]
                    [{:id :turn-1
                      :user-request "derive"
                      :answer-markdown ""}])
                  vis/db-list-session-turn-iterations
                  (fn [_db _turn-id]
                    [{:id :iter-1
                      :code "(def prompt-slice (subvec xs 0 2))"
                      :forms [{:scope "t1/i1/f1"
                               :tag :host
                               :src "(def prompt-slice (subvec xs 0 2))"
                               :result {:vis/ref :expr}}]}])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            rendered (-> history second :traces first :forms first :result-render)]
        (expect (str/includes? (str rendered) "legacy runtime value")))))

  (it "render-answer throws on raw-string input (strict IR contract)"
    (expect
      (try ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/render-answer))
            "raw markdown string")
        false
        (catch clojure.lang.ExceptionInfo _ true))))

  (it "render-answer accepts nil as the empty placeholder"
    (expect (= "" ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/render-answer)) nil))))

  (it "rebuilds tool-result details from canonical op envelope keys"
    ;; Generic envelope shaped like a tool result with command/target
    ;; metadata. Asserts chat layer extracts the canonical keys regardless
    ;; of which extension emitted them.
    (let [tool-out (extension/success
                     {:op :v/cat
                      :result {:path "x.txt" :lines ["ok"]}
                      :metadata {:target {:path "x.txt"}}})]
      (with-redefs [extension/render-tool-result (fn [_] "rendered tool")
                    vis/db-info (fn [] :db)
                    vis/db-list-session-turns
                    (fn [_db _cid]
                      [{:id :turn-1
                        :user-request "run"
                        :answer-markdown ""}])
                    vis/db-list-session-turn-iterations
                    (fn [_db _turn-id]
                      [{:id :iter-1
                        :code "(v/cat \"x.txt\")"
                        :result tool-out}])]
        (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
              trace   (-> history second :traces first)
              form    (-> trace :forms first)]
          (expect (= :tool (:result-kind form)))
          (expect (= {:symbol :v/cat
                      :tag :observation
                      :target {:path "x.txt"}}
                    (:result-detail form))))))))

(defdescribe turn-options-test
  (it "forwards reasoning-default and extra-body to vis/send!"
    ;; Markdown answer pipeline: `vis/send!` returns `{:answer markdown}`;
    ;; the bubble layer derives IR via `vis/markdown->ir` at the boundary.
    (let [seen (atom nil)]
      (with-redefs [vis/send! (fn [_id _text opts]
                                (reset! seen opts)
                                {:answer {:answer "ok"}})]
        (let [result (chat/turn! {:id "c1"} "hello"
                       {:reasoning-default :deep
                        :extra-body {:text {:verbosity "high"}}})]
          (expect (vector? (:answer result)))
          (expect (= :ir (first (:answer result))))
          (expect (str/includes? (vis/render (:answer result) :markdown) "ok"))
          (expect (= 1 (:iteration-count result)))
          (expect (= :deep (:reasoning-default @seen)))
          (expect (= {:text {:verbosity "high"}} (:extra-body @seen)))))))

  (it "returns canonical IR when cancellation is raised as an exception"
    (with-redefs [vis/send! (fn [& _] (throw (InterruptedException. "cancel")))
                  vis/cancellation? (fn [_] true)]
      (let [result (chat/turn! {:id "c1"} "hello")]
        (expect (= :cancelled (:status result)))
        (expect (= :ir (first (:answer result)))))))

  (it "coerces raw cancellation text into canonical IR"
    (with-redefs [vis/send! (fn [& _]
                              {:status :cancelled
                               :answer "Cancelled by user."})]
      (let [result (chat/turn! {:id "c1"} "hello")]
        (expect (= :cancelled (:status result)))
        (expect (= :ir (first (:answer result))))
        (expect (str/includes? (vis/render (:answer result) :markdown)
                  "Cancelled by user"))))))
