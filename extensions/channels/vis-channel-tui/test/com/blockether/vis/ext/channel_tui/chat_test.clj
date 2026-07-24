(ns com.blockether.vis.ext.channel-tui.chat-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [lazytest.core :refer [defdescribe expect it]]))

;; `rebuild-history` now reads `vis/gateway-transcript` (which delegates to
;; `persistance/db-list-*` directly, NOT the `vis/db-list-*` re-exports the
;; tests redef). This stub composes the same turn+`:iterations` shape from those
;; existing mocks — each test redefs `vis/gateway-transcript` to it. The rows
;; pass through `wire/canonical` exactly like the REAL facade (`state/transcript`
;; canonicalizes at the source), so every fixture below exercises the ONE
;; canonical transcript shape a channel actually sees — in-process AND over HTTP.
(defn- compose-transcript
  [sid]
  (wire/canonical (mapv #(assoc %
                           :iterations (vec (vis/db-list-session-turn-iterations :db (:id %))))
                        (vis/db-list-session-turns :db sid))))

(defdescribe rebuild-history-test)

(defdescribe
  rebuild-history-renders-answer-test
  (it "resumed assistant message routes the stored IR answer through render-answer"
      ;; The resume path used to pass the
      ;; persisted answer straight into the bubble without going through
      ;; the channel renderer chokepoint. Both live and resume paths now
      ;; share `chat/render-answer`, which dispatches via the
      ;; `:channel/messages-renderer-fn` registered by `channel-tui.core`.
      ;;
      ;; Normal persisted answers are Nippy-frozen canonical IR; the
      ;; legacy/string terminal-answer fallback is covered below.
      (with-redefs
        [vis/db-info
         (fn []
           :db)

         vis/gateway-transcript
         compose-transcript

         vis/db-list-session-turns
         (fn [_db _cid]
           [{:id :turn-1
             :user-request "siema"
             :content [{"id" "b1" "type" "prose" "markdown" "Siema! 👋 What can I do for you?"}]}])

         vis/db-list-session-turn-iterations
         (fn [_db _turn-id]
           [])]

        (let
          [history
           ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")

           assistant
           (second history)

           blocks
           (:content assistant)

           text
           (:text assistant)]

          (expect (= "prose" (get-in blocks [0 "type"])))
          (expect (str/includes? text "Siema!")))))
  (it "rebuild-history preserves canonical content blocks"
      (with-redefs
        [vis/db-info
         (fn []
           :db)

         vis/gateway-transcript
         compose-transcript

         vis/db-list-session-turns
         (fn [_db _cid]
           [{:id :turn-cancelled
             :user-request "siema"
             :content
             [{"id" "b1" "type" "notice" "code" "turn_cancelled" "message" "Cancelled by user."}]}])

         vis/db-list-session-turn-iterations
         (fn [_db _turn-id]
           [])]

        (let
          [history
           ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")

           assistant
           (second history)

           blocks
           (:content assistant)]

          (expect (= 2 (count history)))
          (expect (= "notice" (get-in blocks [0 "type"])))
          (expect (str/includes? (:text assistant) "Cancelled by user")))))
  (it "rebuild-history shows cancelled status text when persisted answer is blank"
      (with-redefs
        [vis/db-info
         (fn []
           :db)

         vis/gateway-transcript
         compose-transcript

         vis/db-list-session-turns
         (fn [_db _cid]
           [{:id :turn-cancelled :user-request "no live" :prior-outcome :cancelled :content []}])

         vis/db-list-session-turn-iterations
         (fn [_db _turn-id]
           [])]

        (let
          [history
           ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")

           assistant
           (second history)

           blocks
           (:content assistant)]

          (expect (= :cancelled (:status assistant)))
          (expect (= "notice" (get-in blocks [0 "type"])))
          (expect (str/includes? (:text assistant) "Cancelled by user")))))
  (it "rebuild-history marks persisted silent engine calls for the TUI visibility toggle"
      ;; Python engine: an engine-only form (set_session_title) is silent UI chrome,
      ;; detected by `ctx-engine/engine-form-src?` reading the Python call head.
      (with-redefs
        [vis/db-info
         (fn []
           :db)

         vis/gateway-transcript
         compose-transcript

         vis/db-list-session-turns
         (fn [_db _cid]
           [{:id :turn-1 :user-request "siema" :answer-markdown "Siema!"}])

         vis/db-list-session-turn-iterations
         (fn [_db _turn-id]
           [{:id :iter-1 :code "set_session_title(\"Greeting\")" :result "vis_silent"}])]

        (let
          [history
           ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")

           trace
           (-> history
               second
               :traces
               first)

           form
           (-> trace
               :forms
               first)]

          (expect (= 1 (count (:forms trace))))
          (expect (true? (:silent? form)))
          (expect (str/includes? (str (:code form)) "set_session_title")))))
  (it "rebuild-history elides synthetic preflight blocks so they don't render as success"
      (with-redefs
        [vis/db-info
         (fn []
           :db)

         vis/gateway-transcript
         compose-transcript

         vis/db-list-session-turns
         (fn [_db _cid]
           [{:id :turn-1 :user-request "preflight loop" :answer-markdown ""}])

         vis/db-list-session-turn-iterations
         (fn [_db _turn-id]
           [{:id :iter-1 :code "(vis/preflight-error :raw-markdown-fence-leak)"}])]

        (let
          [history
           ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")

           trace
           (-> history
               second
               :traces
               first)]

          (expect (= [] (:forms trace))))))
  (it "rebuild-history preserves mixed-block render segments instead of eliding the answer block"
      ;; New shape: iteration row carries per-form envelopes under :forms.
      ;; Each envelope has :src :tag :result :error :channel; the rebuild
      ;; iterates them rather than treating the whole iteration as one
      ;; opaque block.
      (with-redefs
        [vis/db-info
         (fn []
           :db)

         vis/gateway-transcript
         compose-transcript

         vis/db-list-session-turns
         (fn [_db _cid]
           [{:id :turn-1 :user-request "mixed" :answer-markdown "Done"}])

         vis/db-list-session-turn-iterations
         (fn [_db _turn-id]
           [{:id :iter-1
             :code
             (str "(def x 1)\n" "(set-session-title! \"Mixed\")\n" "(done [:ast [:p \"Done\"]])")
             :forms [{:scope "t1/i1/f1" :tag :host :src "(def x 1)" :result nil}
                     {:scope "t1/i1/f2"
                      :tag :host
                      :src "(set-session-title! \"Mixed\")"
                      :result "vis_silent"}
                     {:scope "t1/i1/f3"
                      :tag :host
                      :src "(done [:ast [:p \"Done\"]])"
                      :result "vis_answer"}]}])]

        (let
          [history
           ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")

           trace
           (-> history
               second
               :traces
               first)]

          ;; Resume keeps ONE restored block PER persisted form envelope —
          ;; parity with the live tracker (the old regroup collapsed every
          ;; envelope into a single merged card and lost intermediate
          ;; results). The structural title segment still survives for
          ;; render.clj to paint.
          (expect (= 3 (count (:forms trace))))
          (expect (some (fn [f]
                          (some #(re-find #"Mixed" (str %)) (:render-segments f)))
                        (:forms trace))))))
  (it "rebuild-history recovers single visible form duration from old iteration rows"
      ;; Historical envelopes lacked per-form :duration-ms. The row-level
      ;; eval duration is still available; if only one form remains after
      ;; answer elision, preserve it on the form for transcript/debug use.
      (with-redefs
        [vis/db-info
         (fn []
           :db)

         vis/gateway-transcript
         compose-transcript

         vis/db-list-session-turns
         (fn [_db _cid]
           [{:id :turn-1 :user-request "patch" :answer-markdown ""}])

         vis/db-list-session-turn-iterations
         (fn [_db _turn-id]
           [{:id :iter-1
             :duration_ms 12
             :code "(patch [])"
             :forms [{:scope "t24/i1/f1"
                      :tag :mutation
                      :src "(patch [])"
                      :result :ok
                      :channel
                      [{:position 0 :form "(patch [])" :success? true :result "PATCH ok"}]}]}])]

        (let
          [history
           ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")

           form
           (-> history
               second
               :traces
               first
               :forms
               first)]

          (expect (= "t24/i1/f1" (:scope form)))
          (expect (= 12 (:duration-ms form))))))
  (it "rebuild-history keeps per-form envelopes and preserves errors"
      ;; Persisted `:forms` are proof-granularity envelopes. Resume keeps
      ;; one restored block PER envelope (parity with the live tracker's
      ;; one chunk per top-level form); `iteration/canonicalize` derives
      ;; the block-level `:error`/`:status` from the errored form so the
      ;; iteration does not render as success.
      (with-redefs
        [vis/db-info
         (fn []
           :db)

         vis/gateway-transcript
         compose-transcript

         vis/db-list-session-turns
         (fn [_db _cid]
           [{:id :turn-1 :user-request "two forms" :answer-markdown ""}])

         vis/db-list-session-turn-iterations
         (fn [_db _turn-id]
           [{:id :iter-1
             :code "(cat \"src/foo.clj\")\n(cat \"ghost.clj\")"
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

        (let
          [history
           ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")

           trace
           (-> history
               second
               :traces
               first)

           forms
           (:forms trace)

           [ok-form err-form]
           forms]

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
  (it "render-answer projects canonical blocks"
      (expect (= "raw **markdown**"
                 ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/render-answer))
                   [{"id" "b1" "type" "prose" "markdown" "raw **markdown**"}]))))
  (it "rebuilds tool-result details from canonical op envelope keys"
      ;; Generic envelope shaped like a tool result with command/target
      ;; metadata. Asserts chat layer extracts the canonical keys regardless
      ;; of which extension emitted them. `:cat` is a real foundation op, but
      ;; this unit test does not load the foundation extension, so its
      ;; `:observation` tag is unregistered. Envelope construction fails
      ;; closed on unregistered ops by design (`op-tag`), so stub it to the
      ;; tag `:cat` carries in production — the envelope is built lazily inside
      ;; the iteration redef, i.e. while the stub is active.
      (with-redefs
        [extension/op-tag
         (fn [_op]
           :observation)

         vis/db-info
         (fn []
           :db)

         vis/gateway-transcript
         compose-transcript

         vis/db-list-session-turns
         (fn [_db _cid]
           [{:id :turn-1 :user-request "run" :answer-markdown ""}])

         vis/db-list-session-turn-iterations
         (fn [_db _turn-id]
           [{:id :iter-1
             :code "(cat \"x.txt\")"
             :result (extension/success {:op :cat
                                         :result {:path "x.txt" :lines ["ok"]}
                                         :metadata {:target {:path "x.txt"}}})}])]

        (let
          [history
           ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")

           trace
           (-> history
               second
               :traces
               first)

           form
           (-> trace
               :forms
               first)]

          (expect (= :tool (:result-kind form)))
          (expect (= {:symbol "cat" :tag "observation" :target {"path" "x.txt"}}
                     (:result-detail form)))))))

(defdescribe
  turn-options-test
  (it "submits through the gateway sync facade without changing content shape"
      (let [seen (atom nil)]
        (with-redefs
          [vis/gateway-submit-turn-sync! (fn [sid opts]
                                           (reset! seen [sid opts])
                                           {"content" [{"id" "b1" "type" "prose" "markdown" "ok"}]
                                            "iteration_count" 1})]
          (let
            [result (chat/turn! {:id "c1"}
                                "hello"
                                {:reasoning-default :deep :extra-body {:text {:verbosity "high"}}})]
            (expect (= "prose" (get-in result ["content" 0 "type"])))
            (expect (= "ok" (get-in result ["content" 0 "markdown"])))
            (expect (= 1 (get result "iteration_count")))
            (expect (= "c1" (first @seen)))
            (expect (= "hello" (:request (second @seen))))
            (expect (= :deep (:reasoning-default (second @seen))))
            (expect (= {:text {:verbosity "high"}} (:extra-body (second @seen))))))))
  (it "returns canonical notice content when cancellation is raised"
      (with-redefs
        [vis/gateway-submit-turn-sync!
         (fn [& _]
           (throw (InterruptedException. "cancel")))

         vis/cancellation?
         (fn [_]
           true)]

        (let [result (chat/turn! {:id "c1"} "hello")]
          (expect (= "cancelled" (get result "status")))
          (expect (= "notice" (get-in result ["content" 0 "type"]))))))
  (it "preserves gateway cancellation content"
      (with-redefs
        [vis/gateway-submit-turn-sync!
         (fn [& _]
           {"content"
            [{"id" "b1" "type" "notice" "code" "turn_cancelled" "message" "Cancelled by user."}]
            "status" "cancelled"})]
        (let [result (chat/turn! {:id "c1"} "hello")]
          (expect (= "cancelled" (get result "status")))
          (expect (= "notice" (get-in result ["content" 0 "type"])))
          (expect (str/includes? (get-in result ["content" 0 "message"]) "Cancelled by user"))))))

(defdescribe
  gateway-event-chunk-test
  ;; The gateway wire event ships the raw `:code`; the TUI renders it directly
  ;; (the canonical web `block-code` contract), so the projection just carries
  ;; `:code` straight through — no `:render-segments` reconstruction.
  (let [g->c @#'chat/gateway-event->chunk]
    (it "block.started carries the raw code straight through"
        (let
          [chunk
           (g->c
             {"type" "block.started" "iteration" 1 "block_id" 0 "code" "git_status()\nprint(42)"})]
          (expect (= :form-start (:phase chunk)))
          (expect (= "git_status()\nprint(42)" (:code chunk)))))
    (it
      "block.output carries the raw code + stdout straight through"
      (let
        [chunk
         (g->c
           {"type" "block.output" "iteration" 1 "block_id" 0 "code" "git_status()" "stdout" "ok"})]
        (expect (= :form-result (:phase chunk)))
        (expect (= "git_status()" (:code chunk)))
        (expect (= "ok" (:stdout chunk)))))
    (it "typed reasoning block delta projects onto :thinking"
        (let
          [chunk (g->c {"type" "content.block.delta"
                        "iteration" 2
                        "block_id" "t1:reasoning:2"
                        "field" "text"
                        "text" "pondering"})]
          (expect (= :reasoning (:phase chunk)))
          (expect (= 2 (:iteration chunk)))
          (expect (= "pondering" (:thinking chunk)))))
    (it "block.preview remains a native preview rather than reasoning/content"
        (let
          [chunk (g->c {"type" "block.preview"
                        "iteration" 1
                        "block_id" 0
                        "code" "print(4"
                        "tool_name" "native_call"
                        "tool_color_role" "tool-color/meta"
                        "result_summary" "run_python"
                        "tool_call_id" "call_1"})]
          (expect (= :tool-preview (:phase chunk)))
          (expect (= "print(4" (:code chunk)))
          (expect (= "native_call" (:vis/tool-name chunk)))
          (expect (= :tool-color/meta (:tool-color-role chunk)))
          (expect (= "call_1" (:svar/tool-call-id chunk)))))))

(defdescribe
  activity-event-chunk-test
  ;; A coarse `activity` wire event (provider wait, response parse, nested
  ;; shell/tool call) projects back to the phase the live spinner reads, so an
  ;; ATTACHED tab shows "Vis is running: …" like a locally-run turn.
  (let [g->c @#'chat/gateway-event->chunk]
    (it "a nested tool activity projects to :tool-start naming the op"
        (expect (= {:phase :tool-start :iteration 2 :tool-event {:op "shell_run"}}
                   (g->c {"type" "activity" "activity" "tool" "op" "shell_run" "iteration" 2}))))
    (it "a shell-run activity projects to :shell-run with its command"
        (expect
          (= {:phase :shell-run :iteration 1 :cmd "clojure -M:test"}
             (g->c
               {"type" "activity" "activity" "shell-run" "cmd" "clojure -M:test" "iteration" 1}))))
    (it "a provider-call activity projects to :provider-call"
        (expect (= {:phase :provider-call :iteration 1}
                   (g->c {"type" "activity" "activity" "provider-call" "iteration" 1}))))))

(defdescribe provider-retry-event-chunk-test
             (let [g->c @#'chat/gateway-event->chunk]
               (it "rehydrates structured retry metadata from the canonical wire event"
                   (let
                     [event (wire/canonical {:type "provider.retry"
                                             :iteration 2
                                             :attempt 1
                                             :max-retries 3
                                             :delay-ms 1000
                                             :error {:type :svar.llm/provider-unavailable
                                                     :message "Provider unavailable"}
                                             :event {:event/type :llm.routing/provider-retry
                                                     :reason :provider-unavailable
                                                     :provider "openai"
                                                     :model "gpt-x"}})
                      chunk (g->c event)]

                     (expect (= :provider-retry-reset (:phase chunk)))
                     (expect (= 2 (:iteration chunk)))
                     (expect (= {:type :svar.llm/provider-unavailable
                                 :message "Provider unavailable"
                                 :attempt 1
                                 :max-retries 3
                                 :delay-ms 1000}
                                (:error chunk)))
                     (expect (= :provider-unavailable (get-in chunk [:event :reason])))
                     (expect (= "openai" (get-in chunk [:event :provider])))))))

(defdescribe
  restore-block-record-test
  ;; The restore chain — persisted envelope → `envelope->block` → `block->form-record`
  ;; — used to be TWO hand-listed projections, so a display field either forgot
  ;; silently vanished on RESUME while the live stream kept it (exactly how
  ;; print-many `:cards` AND the native-tool card identity were dropped). Both
  ;; builders now project through `vis/form->display` (the ONE display-key
  ;; projection). This guard drives a PERSISTED-SHAPED envelope through the REAL
  ;; restore entry (`it->iteration-entry`) so a drop anywhere in the chain fails.
  (let
    [it->ie
     @#'chat/it->iteration-entry

     restore
     (fn [env]
       (-> (it->ie {:produced-answer? false :last-iteration-id :iter-1}
                   (wire/canonical {:id :iter-1 :code (:src env) :forms [env]}))
           :forms
           first))]

    (it "a restored print-many envelope keeps its per-result cards (nested colour keywords intact)"
        (let
          [cards
           [{:vis/tool-name "cat"
             :result-summary "read 3 lines"
             :result-render "x"
             :tool-color-role :tool-color/read}
            {:vis/tool-name "rg"
             :result-summary "5 hits"
             :result-render "y"
             :tool-color-role :tool-color/search}]

           rec
           (restore {:scope "t1/i2"
                     :tag :host
                     :src "print(await cat('x'))"
                     :vis/tool-name "python_execution"
                     :tool-color-role :tool-color/meta
                     :result-summary "2 printed results"
                     :cards cards})]

          (expect (= cards (:cards rec)))
          (expect (= 2 (count (vis/result-cards rec))))
          (expect (= [:tool-color/read :tool-color/search]
                     (mapv :color-role (vis/result-cards rec))))))
    (it "a restored single NATIVE tool envelope keeps its op-card identity (label/colour/summary)"
        (let
          [rec
           (restore {:scope "t1/i1"
                     :tag :host
                     :src "rg('defn','src')"
                     :vis/tool-name "rg"
                     :tool-color-role :tool-color/search
                     :result-render "a.clj:1: x"
                     :result-summary "8 hits in 1 file"})

           card
           (vis/result-card rec)]

          (expect (= "RG" (:label card)))
          (expect (= :tool-color/search (:color-role card)))
          (expect (= "8 hits in 1 file" (:summary card)))))))

;; Regression: a FAILED provider turn's styled card must survive the
;; `turn!`/`attach!` fold. Those fold the engine's provider-error IR onto
;; canonical error blocks are preserved end-to-end.
;; flatten `:error` into plain text on a fresh conversation.
(defdescribe error-content-test
             (it "preserves canonical provider error blocks"
                 (let
                   [blocks [{"id" "e1"
                             "type" "error"
                             "code" "provider_unavailable"
                             "message" "Provider unavailable"
                             "retryable" true}]]
                   (expect (= blocks (chat/error-content {"content" blocks "error" "boom"})))))
             (it "creates a canonical error block when content is absent"
                 (let [out (chat/error-content {"error" "boom"})]
                   (expect (= "error" (get-in out [0 "type"])))
                   (expect (str/includes? (get-in out [0 "message"]) "boom")))))

(defdescribe
  queue-sync-event-chunk-test
  ;; Queue lifecycle events (from ANY sibling channel) project to :queue-sync
  ;; chunks so every attached TUI mirrors the gateway's queued backlog live.
  (let [g->c @#'chat/gateway-event->chunk]
    (it "turn.queued projects to :add with the prompt text"
        (expect (= {:phase :queue-sync :op :add :turn-id "q1" :text "hi"}
                   (g->c {"type" "turn.queued" "turn_id" "q1" "request" "hi"}))))
    (it "turn.queued.updated projects to :update"
        (expect (= {:phase :queue-sync :op :update :turn-id "q1" :text "hi2"}
                   (g->c {"type" "turn.queued.updated" "turn_id" "q1" "request" "hi2"}))))
    (it "turn.queued.deleted projects to :delete"
        (expect (= {:phase :queue-sync :op :delete :turn-id "q1"}
                   (g->c {"type" "turn.queued.deleted" "turn_id" "q1"}))))
    (it "turn.queued.drained (gateway auto-start) projects to :delete"
        (expect (= {:phase :queue-sync :op :delete :turn-id "q1"}
                   (g->c {"type" "turn.queued.drained" "turn_id" "q1"}))))
    (it "turn.started projects to :turn-start with the canonical run-start clock"
        (expect (= {:phase :turn-start :turn-id "t1" :request "hi" :started-at-ms 1234 :server-at-ms nil}
                   (g->c
                     {"type" "turn.started" "turn_id" "t1" "request" "hi" "started_at" 1234}))))))

(defdescribe
  title-sync-event-chunk-test
  ;; `session.title_updated` (auto-title or rename — possibly produced in a
  ;; SIBLING process: another TUI, the web, the serve daemon) must project to a
  ;; :title-sync chunk. Before this projection existed the TUI dropped the
  ;; event entirely, so a foreign-generated title only appeared after closing
  ;; and reopening the tab (which re-reads the DB title).
  (let [g->c @#'chat/gateway-event->chunk]
    (it "projects the titled session's id and the new title"
        (expect (= {:phase :title-sync :session-id "aaa" :title "Tab Sync Fix"}
                   (g->c
                     {"type" "session.title_updated" "session_id" "aaa" "title" "Tab Sync Fix"}))))
    (it "reads string-keyed wire events too (SSE JSON)"
        (expect (= {:phase :title-sync :session-id "bbb" :title "T"}
                   (g->c {"type" "session.title_updated" "session_id" "bbb" "title" "T"}))))
    (it "a foreign copy keeps the TITLED session's id from the payload"
        ;; gateway state/broadcast-title-event! stores a copy on every OTHER
        ;; registered session with the titled session's id in the payload —
        ;; the chunk must carry THAT id, not the subscribed session's.
        (expect (= "other-session"
                   (:session-id (g->c {"type" "session.title_updated"
                                       "session_id" "other-session"
                                       "title" "X"})))))))

(it "rehydrates a structured iteration error for the transient retry row"
    (let
      [g->c
       @#'chat/gateway-event->chunk

       chunk
       (g->c (wire/canonical
               {:type "iteration.error"
                :iteration 3
                :error "upstream reset"
                :error-data {:type :svar.core/http-error :message "upstream reset" :status 503}}))]

      (expect (= :iteration-error (:phase chunk)))
      (expect (= {:type :svar.core/http-error :message "upstream reset" :status 503}
                 (:error chunk)))))
