(ns com.blockether.vis.ext.channel-tui.chat-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.internal.extension :as extension]
            [lazytest.core :refer [defdescribe expect it]]))

;; `rebuild-history` now reads `vis/gateway-transcript` (which delegates to
;; `persistance/db-list-*` directly, NOT the `vis/db-list-*` re-exports the
;; tests redef). This stub composes the same turn+`:iterations` shape from those
;; existing mocks — each test redefs `vis/gateway-transcript` to it.
(defn- compose-transcript
  [sid]
  (mapv #(assoc % :iterations (vec (vis/db-list-session-turn-iterations :db (:id %))))
    (vis/db-list-session-turns :db sid)))

(defdescribe rebuild-history-test)

(defdescribe rebuild-history-renders-answer-test
  (it "resumed assistant message routes the stored IR answer through render-answer"
    ;; The resume path used to pass the
    ;; persisted answer straight into the bubble without going through
    ;; the channel renderer chokepoint. Both live and resume paths now
    ;; share `chat/render-answer`, which dispatches via the
    ;; `:channel/messages-renderer-fn` registered by `channel-tui.core`.
    ;;
    ;; Normal persisted answers are Nippy-frozen canonical IR; the
    ;; legacy/string terminal-answer fallback is covered below.
    (with-redefs [vis/db-info (fn [] :db)
                  vis/gateway-transcript compose-transcript
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
                  vis/gateway-transcript compose-transcript
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
                  vis/gateway-transcript compose-transcript
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
                  vis/gateway-transcript compose-transcript
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
                  vis/gateway-transcript compose-transcript
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
                  vis/gateway-transcript compose-transcript
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

  (it "rebuild-history recovers single visible form duration from old iteration rows"
    ;; Historical envelopes lacked per-form :duration-ms. The row-level
    ;; eval duration is still available; if only one form remains after
    ;; answer elision, preserve it on the form for transcript/debug use.
    (with-redefs [vis/db-info (fn [] :db)
                  vis/gateway-transcript compose-transcript
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
                  vis/gateway-transcript compose-transcript
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
                  vis/db-info (fn [] :db)
                  vis/gateway-transcript compose-transcript
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
  ;; `turn!` blocks on `vis/gateway-submit-turn-sync!` (the canonical sync
  ;; facade) and derives answer IR from its result, passing through caller
  ;; options. We mock that one seam.
  (it "submits through the gateway sync facade, derives answer IR, passes options"
    (let [seen (atom nil)]
      (with-redefs [vis/gateway-submit-turn-sync!
                    (fn [sid opts]
                      (reset! seen [sid opts])
                      {:answer "ok" :iteration-count 1})]
        (let [result (chat/turn! {:id "c1"} "hello"
                       {:reasoning-default :deep
                        :extra-body {:text {:verbosity "high"}}})]
          (expect (vector? (:answer result)))
          (expect (= :ir (first (:answer result))))
          (expect (str/includes? (vis/render (:answer result) :markdown) "ok"))
          (expect (= 1 (:iteration-count result)))
          (expect (= "c1" (first @seen)))
          (expect (= "hello" (:request (second @seen))))
          (expect (= :deep (:reasoning-default (second @seen))))
          (expect (= {:text {:verbosity "high"}} (:extra-body (second @seen))))))))

  (it "returns canonical IR when cancellation is raised as an exception"
    (with-redefs [vis/gateway-submit-turn-sync! (fn [& _] (throw (InterruptedException. "cancel")))
                  vis/cancellation? (fn [_] true)]
      (let [result (chat/turn! {:id "c1"} "hello")]
        (expect (= :cancelled (:status result)))
        (expect (= :ir (first (:answer result)))))))

  (it "coerces gateway cancellation text into canonical IR"
    (with-redefs [vis/gateway-submit-turn-sync!
                  (fn [& _] {:answer "Cancelled by user." :status :cancelled})]
      (let [result (chat/turn! {:id "c1"} "hello")]
        (expect (= :cancelled (:status result)))
        (expect (= :ir (first (:answer result))))
        (expect (str/includes? (vis/render (:answer result) :markdown)
                  "Cancelled by user"))))))

(defdescribe gateway-event-chunk-test
  ;; The gateway wire event ships the raw `:code`; the TUI renders it directly
  ;; (the canonical web `block-code` contract), so the projection just carries
  ;; `:code` straight through — no `:render-segments` reconstruction.
  (let [g->c @#'chat/gateway-event->chunk]
    (it "block.started carries the raw code straight through"
      (let [chunk (g->c {:type "block.started" :iteration 1 :block_id 0
                         :code "git_status()\nprint(42)"})]
        (expect (= :form-start (:phase chunk)))
        (expect (= "git_status()\nprint(42)" (:code chunk)))))

    (it "block.output carries the raw code + stdout straight through"
      (let [chunk (g->c {:type "block.output" :iteration 1 :block_id 0
                         :code "git_status()" :stdout "ok"})]
        (expect (= :form-result (:phase chunk)))
        (expect (= "git_status()" (:code chunk)))
        (expect (= "ok" (:stdout chunk)))))))

(defdescribe restore-block-record-test
  ;; The restore chain — persisted envelope → `envelope->block` → `block->form-record`
  ;; — used to be TWO hand-listed projections, so a display field either forgot
  ;; silently vanished on RESUME while the live stream kept it (exactly how
  ;; print-many `:cards` AND the native-tool card identity were dropped). Both
  ;; builders now project through `vis/form->display` (the ONE display-key
  ;; projection). This guard drives a PERSISTED-SHAPED envelope through the REAL
  ;; restore entry (`it->iteration-entry`) so a drop anywhere in the chain fails.
  (let [it->ie @#'chat/it->iteration-entry
        restore (fn [env]
                  (-> (it->ie {:produced-answer? false :last-iteration-id :iter-1}
                        {:id :iter-1 :code (:src env) :forms [env]})
                    :forms first))]
    (it "a restored print-many envelope keeps its per-result cards (nested colour keywords intact)"
      (let [cards [{:vis/tool-name "cat" :result-summary "read 3 lines"
                    :result-render "x" :tool-color-role :tool-color/read}
                   {:vis/tool-name "rg" :result-summary "5 hits"
                    :result-render "y" :tool-color-role :tool-color/search}]
            rec   (restore {:scope "t1/i2" :tag :host :src "print(await cat('x'))"
                            :vis/tool-name "python_execution" :tool-color-role :tool-color/meta
                            :result-summary "2 printed results" :cards cards})]
        (expect (= cards (:cards rec)))
        (expect (= 2 (count (vis/result-cards rec))))
        (expect (= [:tool-color/read :tool-color/search]
                  (mapv :color-role (vis/result-cards rec))))))
    (it "a restored single NATIVE tool envelope keeps its op-card identity (label/colour/summary)"
      (let [rec  (restore {:scope "t1/i1" :tag :host :src "rg('defn','src')"
                           :vis/tool-name "rg" :tool-color-role :tool-color/search
                           :result-render "a.clj:1: x" :result-summary "8 hits in 1 file"})
            card (vis/result-card rec)]
        (expect (= "RG" (:label card)))
        (expect (= :tool-color/search (:color-role card)))
        (expect (= "8 hits in 1 file" (:summary card)))))))
