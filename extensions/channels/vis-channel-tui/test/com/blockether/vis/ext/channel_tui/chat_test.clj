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

  (it "rebuild-history marks persisted silent engine calls for the TUI visibility toggle"
    ;; Python engine: an engine-only form (set_session_title) is silent UI chrome,
    ;; detected by `ctx-engine/engine-form-src?` reading the Python call head.
    (with-redefs [vis/db-info (fn [] :db)
                  vis/db-list-session-turns
                  (fn [_db _cid]
                    [{:id :turn-1
                      :user-request "siema"
                      :answer-markdown "Siema!"}])
                  vis/db-list-session-turn-iterations
                  (fn [_db _turn-id]
                    [{:id :iter-1
                      :code "set_session_title(\"Greeting\")"
                      :result "vis_silent"}])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            trace   (-> history second :traces first)
            form    (-> trace :forms first)]
        (expect (= 1 (count (:forms trace))))
        (expect (true? (:silent? form)))
        (expect (str/includes? (str (:code form)) "set_session_title")))))

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
                              {:scope "t1/i1/f2" :tag :host :src "(set-session-title! \"Mixed\")" :result "vis_silent"}
                              {:scope "t1/i1/f3" :tag :host :src "(done [:ir [:p \"Done\"]])" :result "vis_answer"}]}])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            trace   (-> history second :traces first)]
        ;; Resume keeps ONE restored block PER persisted form envelope —
        ;; parity with the live tracker (the old regroup collapsed every
        ;; envelope into a single merged card and lost intermediate
        ;; results). The structural title segment still survives for
        ;; render.clj to paint.
        (expect (= 3 (count (:forms trace))))
        (expect (some (fn [f] (some #(re-find #"Mixed" (str %)) (:render-segments f)))
                  (:forms trace))))))

  (it "rebuild-history prefers durable channel render over runtime-ref placeholder"
    ;; `(def x (cat ...))` persists the live var value as
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
                      :code "(def prompt-lines (cat \"src/foo.clj\"))"
                      :forms [{:scope "t1/i1/f1"
                               :tag   :host
                               :src   "(def prompt-lines (cat \"src/foo.clj\"))"
                               :result {:vis/ref :expr}
                               :channel [{:position 0
                                          :form "(cat \"src/foo.clj\")"
                                          :success? true
                                          :result {:summary [:ir {} [:p {} [:span {} "Read `src/foo.clj` — 10 line(s)."]]]
                                                   :display [:ir {} [:p {} [:span {} "Read `src/foo.clj` — 10 line(s)."]]]}}]}]}])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            trace   (-> history second :traces first)
            rendered (-> trace :forms first :result-render)]
        (expect (str/includes? (str rendered) "Read `src/foo.clj`"))
        (expect (not (str/includes? (str rendered) "<runtime value"))))))

  (it "rebuild-history recovers single visible form duration from old iteration rows"
    ;; Historical envelopes lacked per-form :duration-ms. The row-level
    ;; eval duration is still available; if only one form remains after
    ;; answer elision, preserve it on the form for transcript/debug use.
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
                      :code "(patch [])"
                      :forms [{:scope "t24/i1/f1"
                               :tag :mutation
                               :src "(patch [])"
                               :result :ok
                               :channel [{:position 0
                                          :form "(patch [])"
                                          :success? true
                                          :result "PATCH ok"}]}]}])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            form    (-> history second :traces first :forms first)]
        (expect (= "t24/i1/f1" (:scope form)))
        (expect (= 12 (:duration-ms form))))))

  (it "rebuild-history keeps per-form envelopes and preserves errors"
    ;; Persisted `:forms` are proof-granularity envelopes. Resume keeps
    ;; one restored block PER envelope (parity with the live tracker's
    ;; one chunk per top-level form); `iteration/canonicalize` derives
    ;; the block-level `:error`/`:status` from the errored form so the
    ;; iteration does not render as success.
    (with-redefs [vis/db-info (fn [] :db)
                  vis/db-list-session-turns
                  (fn [_db _cid]
                    [{:id :turn-1 :user-request "two forms" :answer-markdown ""}])
                  vis/db-list-session-turn-iterations
                  (fn [_db _turn-id]
                    [{:id :iter-1
                      :code (str "(cat \"src/foo.clj\")\n(cat \"ghost.clj\")")
                      :forms [{:scope "t1/i1/f1"
                               :tag :observation
                               :src "(cat \"src/foo.clj\")"
                               :result {:op :cat :path "src/foo.clj"}
                               :channel [{:position 0
                                          :form "(cat \"src/foo.clj\")"
                                          :success? true
                                          :result "CAT src/foo.clj"}]}
                              {:scope "t1/i1/f2"
                               :tag :observation
                               :src "(cat \"ghost.clj\")"
                               :error {:message "file not found: ghost.clj"}}]}])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            trace   (-> history second :traces first)
            forms   (:forms trace)
            [ok-form err-form] forms]
        (expect (= 2 (count forms)))
        (expect (str/includes? (:code ok-form) "src/foo.clj"))
        (expect (true? (:success? ok-form)))
        (expect (str/includes? (:code err-form) "ghost.clj"))
        (expect (false? (:success? err-form)))
        (expect (= :error (:result-kind err-form)))
        (expect (some? (:error err-form)))
        ;; block-level projection: the errored form drives the
        ;; iteration's canonical status + error
        (expect (= :error (:status trace)))
        (expect (some? (:error trace))))))

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
    ;; of which extension emitted them. `:cat` is a real foundation op, but
    ;; this unit test does not load the foundation extension, so its
    ;; `:observation` tag is unregistered. Envelope construction fails
    ;; closed on unregistered ops by design (`op-tag`), so stub it to the
    ;; tag `:cat` carries in production — the envelope is built lazily inside
    ;; the iteration redef, i.e. while the stub is active.
    (with-redefs [extension/op-tag (fn [_op] :observation)
                  extension/render-tool-result (fn [_] "rendered tool")
                  vis/db-info (fn [] :db)
                  vis/db-list-session-turns
                  (fn [_db _cid]
                    [{:id :turn-1
                      :user-request "run"
                      :answer-markdown ""}])
                  vis/db-list-session-turn-iterations
                  (fn [_db _turn-id]
                    [{:id :iter-1
                      :code "(cat \"x.txt\")"
                      :result (extension/success
                                {:op :cat
                                 :result {:path "x.txt" :lines ["ok"]}
                                 :metadata {:target {:path "x.txt"}}})}])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            trace   (-> history second :traces first)
            form    (-> trace :forms first)]
        (expect (= :tool (:result-kind form)))
        (expect (= {:symbol :cat
                    :tag :observation
                    :target {:path "x.txt"}}
                  (:result-detail form)))))))

(defdescribe turn-options-test
  (it "submits through the in-process gateway facade and derives answer IR from terminal events"
    (let [seen (atom nil)
          sink* (atom nil)]
      (with-redefs [vis/gateway-current-seq (fn [_] 0)
                    vis/gateway-subscribe! (fn [_sid _sub-id sink _cursor]
                                             (reset! sink* sink)
                                             [])
                    vis/gateway-submit-turn! (fn [sid opts]
                                               (reset! seen [sid opts])
                                               {:turn {:turn_id "t1"}})
                    vis/gateway-unsubscribe! (fn [& _])]
        (let [f (future (chat/turn! {:id "c1"} "hello"
                          {:reasoning-default :deep
                           :extra-body {:text {:verbosity "high"}}}))]
          (while (nil? @sink*) (Thread/sleep 1))
          (@sink* {:type "turn.completed"
                   :turn_id "t1"
                   :status "completed"
                   :answer_md "ok"})
          (let [result @f]
            (expect (vector? (:answer result)))
            (expect (= :ir (first (:answer result))))
            (expect (str/includes? (vis/render (:answer result) :markdown) "ok"))
            (expect (= 1 (:iteration-count result)))
            (expect (= "c1" (first @seen)))
            (expect (= :deep (:reasoning-default (second @seen))))
            (expect (= {:text {:verbosity "high"}} (:extra-body (second @seen)))))))))

  (it "returns canonical IR when cancellation is raised as an exception"
    (with-redefs [vis/gateway-current-seq (fn [_] 0)
                  vis/gateway-subscribe! (fn [& _] [])
                  vis/gateway-submit-turn! (fn [& _] (throw (InterruptedException. "cancel")))
                  vis/gateway-unsubscribe! (fn [& _])
                  vis/cancellation? (fn [_] true)]
      (let [result (chat/turn! {:id "c1"} "hello")]
        (expect (= :cancelled (:status result)))
        (expect (= :ir (first (:answer result)))))))

  (it "coerces gateway cancellation text into canonical IR"
    (let [sink* (atom nil)]
      (with-redefs [vis/gateway-current-seq (fn [_] 0)
                    vis/gateway-subscribe! (fn [_sid _sub-id sink _cursor]
                                             (reset! sink* sink)
                                             [])
                    vis/gateway-submit-turn! (fn [& _] {:turn {:turn_id "t1"}})
                    vis/gateway-unsubscribe! (fn [& _])]
        (let [f (future (chat/turn! {:id "c1"} "hello"))]
          (while (nil? @sink*) (Thread/sleep 1))
          (@sink* {:type "turn.completed"
                   :turn_id "t1"
                   :status "cancelled"
                   :answer_md "Cancelled by user."})
          (let [result @f]
            (expect (= :cancelled (:status result)))
            (expect (= :ir (first (:answer result))))
            (expect (str/includes? (vis/render (:answer result) :markdown)
                      "Cancelled by user"))))))))
