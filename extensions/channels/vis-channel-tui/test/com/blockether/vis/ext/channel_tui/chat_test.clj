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

(defdescribe session-creation-root-test
             (it "pins a new TUI session to the client invocation root"
                 (let [request
                       (atom nil)

                       sid
                       (java.util.UUID/randomUUID)]

                   (with-redefs [vis/gateway-create-session! (fn [opts]
                                                               (reset! request opts)
                                                               {"id" (str sid)})]
                     (expect (= sid (:id (chat/make-session nil))))
                     (expect (= (vis/workspace-normalize-root (System/getProperty "user.dir"))
                                (:root @request)))))))

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
      (with-redefs [vis/db-info
                    (fn []
                      :db)

                    vis/gateway-transcript
                    compose-transcript

                    vis/db-list-session-turns
                    (fn [_db _cid]
                      [{:id :turn-1
                        :user-request "siema"
                        :content [{"id" "b1"
                                   "type" "prose"
                                   "markdown" "Siema! 👋 What can I do for you?"}]}])

                    vis/db-list-session-turn-iterations
                    (fn [_db _turn-id]
                      [])]

        (let [history
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
      (with-redefs [vis/db-info
                    (fn []
                      :db)

                    vis/gateway-transcript
                    compose-transcript

                    vis/db-list-session-turns
                    (fn [_db _cid]
                      [{:id :turn-cancelled
                        :user-request "siema"
                        :content [{"id" "b1"
                                   "type" "notice"
                                   "code" "turn_cancelled"
                                   "message" "Cancelled by user."}]}])

                    vis/db-list-session-turn-iterations
                    (fn [_db _turn-id]
                      [])]

        (let [history
              ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")

              assistant
              (second history)

              blocks
              (:content assistant)]

          (expect (= 2 (count history)))
          (expect (= "notice" (get-in blocks [0 "type"])))
          (expect (str/includes? (:text assistant) "Cancelled by user")))))
  (it "rebuild-history shows cancelled status text when persisted answer is blank"
      (with-redefs [vis/db-info
                    (fn []
                      :db)

                    vis/gateway-transcript
                    compose-transcript

                    vis/db-list-session-turns
                    (fn [_db _cid]
                      [{:id :turn-cancelled
                        :user-request "no live"
                        :prior-outcome :cancelled
                        :content []}])

                    vis/db-list-session-turn-iterations
                    (fn [_db _turn-id]
                      [])]

        (let [history
              ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")

              assistant
              (second history)

              blocks
              (:content assistant)]

          (expect (= :cancelled (:status assistant)))
          (expect (= "notice" (get-in blocks [0 "type"])))
          (expect (str/includes? (:text assistant) "Cancelled by user")))))
  (it
    "rebuild-history marks persisted silent engine calls for the TUI visibility toggle"
    ;; Python engine: an engine-only form (set_session_title) is silent UI chrome,
    ;; detected by `ctx-engine/engine-form-src?` reading the Python call head.
    (with-redefs [vis/db-info
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

      (let [history
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
      (with-redefs [vis/db-info
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

        (let [history
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
      (with-redefs [vis/db-info
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
                        :code (str "(def x 1)\n"
                                   "(set-session-title! \"Mixed\")\n"
                                   "(done [:ast [:p \"Done\"]])")
                        :forms [{:scope "t1/i1/f1" :tag :host :src "(def x 1)" :result nil}
                                {:scope "t1/i1/f2"
                                 :tag :host
                                 :src "(set-session-title! \"Mixed\")"
                                 :result "vis_silent"}
                                {:scope "t1/i1/f3"
                                 :tag :host
                                 :src "(done [:ast [:p \"Done\"]])"
                                 :result "vis_answer"}]}])]

        (let [history
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
  (it
    "rebuild-history recovers single visible form duration from old iteration rows"
    ;; Historical envelopes lacked per-form :duration-ms. The row-level
    ;; eval duration is still available; if only one form remains after
    ;; answer elision, preserve it on the form for transcript/debug use.
    (with-redefs [vis/db-info
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
                      :forms
                      [{:scope "t24/i1/f1"
                        :tag :mutation
                        :src "(patch [])"
                        :result :ok
                        :channel
                        [{:position 0 :form "(patch [])" :success? true :result "PATCH ok"}]}]}])]

      (let [history
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
      (with-redefs [vis/db-info
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

        (let [history
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
      (with-redefs [extension/op-tag
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

        (let [history
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
        (with-redefs [vis/gateway-submit-turn-sync! (fn [sid opts]
                                                      (reset! seen [sid opts])
                                                      {"content"
                                                       [{"id" "b1" "type" "prose" "markdown" "ok"}]
                                                       "iteration_count" 1})]
          (let [result (chat/turn! {:id "c1"}
                                   "hello"
                                   {:reasoning-default :deep
                                    :extra-body {:text {:verbosity "high"}}})]
            (expect (= "prose" (get-in result ["content" 0 "type"])))
            (expect (= "ok" (get-in result ["content" 0 "markdown"])))
            (expect (= 1 (get result "iteration_count")))
            (expect (= "c1" (first @seen)))
            (expect (= "hello" (:request (second @seen))))
            (expect (= :deep (:reasoning-default (second @seen))))
            (expect (= {:text {:verbosity "high"}} (:extra-body (second @seen))))))))
  (it "forwards the collapsed display copy as :display-request"
      ;; Regression: `turn!` DROPPED `:display-text`, so the gateway only ever saw the
      ;; expanded agent text. A pasted image expands to a bare temp path, which every
      ;; channel then painted verbatim instead of the `vis-image` fence the user wrote.
      (let [seen (atom nil)]
        (with-redefs [vis/gateway-submit-turn-sync! (fn [_ opts]
                                                      (reset! seen opts)
                                                      {"content" []})]
          (chat/turn! {:id "c1"}
                      "look \n/tmp/shot.png\n"
                      {:display-text
                       "\n````vis-image\n[Image #1: shot.png]\n/tmp/shot.png\n````\n"})
          (expect (= "look \n/tmp/shot.png\n" (:request @seen)))
          (expect (str/includes? (:display-request @seen) "````vis-image")))))
  (it "omits :display-request when there is no separate display copy"
      (let [seen (atom nil)]
        (with-redefs [vis/gateway-submit-turn-sync! (fn [_ opts]
                                                      (reset! seen opts)
                                                      {"content" []})]
          (chat/turn! {:id "c1"} "plain")
          (expect (not (contains? @seen :display-request))))))
  (it "returns canonical notice content when cancellation is raised"
      (with-redefs [vis/gateway-submit-turn-sync!
                    (fn [& _]
                      (throw (InterruptedException. "cancel")))

                    vis/cancellation?
                    (fn [_]
                      true)]

        (let [result (chat/turn! {:id "c1"} "hello")]
          (expect (= "cancelled" (get result "status")))
          (expect (= "notice" (get-in result ["content" 0 "type"]))))))
  (it "preserves gateway cancellation content"
      (with-redefs [vis/gateway-submit-turn-sync! (fn [& _]
                                                    {"content" [{"id" "b1"
                                                                 "type" "notice"
                                                                 "code" "turn_cancelled"
                                                                 "message" "Cancelled by user."}]
                                                     "status" "cancelled"})]
        (let [result (chat/turn! {:id "c1"} "hello")]
          (expect (= "cancelled" (get result "status")))
          (expect (= "notice" (get-in result ["content" 0 "type"])))
          (expect (str/includes? (get-in result ["content" 0 "message"]) "Cancelled by user"))))))

;; A NOTICE is prose, not a machine code: the TUI used to lead every notice with
;; `**turn_cancelled** Cancelled by user.` while the companion (`ChatContent.tsx`)
;; printed the sentence alone, so the terminal shouted an internal token at a
;; human who had just pressed Esc. The ERROR card keeps its code — that is what a
;; bug report quotes.
(defdescribe
  notice-markdown-test
  (it "prints a notice's sentence alone, without its machine code"
      (expect (= "Cancelled by user."
                 (chat/content->markdown [{"id" "b1"
                                           "type" "notice"
                                           "code" "turn_cancelled"
                                           "message" "Cancelled by user."}]))))
  ;; Regression: the code was glued to the front of the message, so a provider
  ;; card opened `**provider_generic** Provider unavailable` — the machine token
  ;; ran into the headline and the card's first line read as one sentence nobody
  ;; wrote. The code keeps its line; the message starts the next paragraph.
  (it "still leads an error with its bold machine code, on its own line"
      (expect (= "**turn_failed**\n\nTurn failed."
                 (chat/content->markdown
                   [{"id" "b1" "type" "error" "code" "turn_failed" "message" "Turn failed."}]))))
  (it "keeps a multi-paragraph provider card readable under its code"
      (expect
        (= "**provider_unroutable**\n\nNo provider could take this request\n\nWHAT HAPPENED: ..."
           (chat/content->markdown
             [{"id" "b1"
               "type" "error"
               "code" "provider_unroutable"
               "message" "No provider could take this request\n\nWHAT HAPPENED: ..."}])))))

(defdescribe gateway-event-chunk-test
             ;; The gateway wire event ships the raw `:code`; the TUI renders it directly
             ;; (the canonical web `block-code` contract), so the projection just carries
             ;; `:code` straight through — no `:render-segments` reconstruction.
             (let [g->c @#'chat/gateway-event->chunk]
               (it "block.started carries the raw code straight through"
                   (let [chunk (g->c {"type" "block.started"
                                      "iteration" 1
                                      "block_id" 0
                                      "code" "git_status()\nprint(42)"})]
                     (expect (= :form-start (:phase chunk)))
                     (expect (= "git_status()\nprint(42)" (:code chunk)))))
               (it "block.output carries the raw code + stdout straight through"
                   (let [chunk (g->c {"type" "block.output"
                                      "iteration" 1
                                      "block_id" 0
                                      "code" "git_status()"
                                      "stdout" "ok"})]
                     (expect (= :form-result (:phase chunk)))
                     (expect (= "git_status()" (:code chunk)))
                     (expect (= "ok" (:stdout chunk)))))
               (it "typed reasoning block delta projects onto :thinking"
                   (let [chunk (g->c {"type" "content.block.delta"
                                      "iteration" 2
                                      "block_id" "t1:reasoning:2"
                                      "field" "text"
                                      "text" "pondering"})]
                     (expect (= :reasoning (:phase chunk)))
                     (expect (= 2 (:iteration chunk)))
                     (expect (= "pondering" (:thinking chunk)))))
               (it "block.preview remains a block preview rather than reasoning/content"
                   (let [chunk (g->c {"type" "block.preview"
                                      "iteration" 1
                                      "block_id" 0
                                      "code" "print(4"
                                      "op" "grep"
                                      "result_summary" "12 results"
                                      "tool_call_id" "call_1"})]
                     (expect (= :tool-preview (:phase chunk)))
                     (expect (= "print(4" (:code chunk)))
                     (expect (= "grep" (:op chunk)))
                     (expect (= "call_1" (:svar/tool-call-id chunk)))))))

(defdescribe
  activity-event-chunk-test
  ;; A coarse `activity` wire event (provider wait, response parse, nested
  ;; shell/tool call) projects back to the phase the live spinner reads, so an
  ;; ATTACHED tab shows "Vis is running: …" like a locally-run turn.
  (let [g->c @#'chat/gateway-event->chunk]
    (it "a nested tool activity keeps the precise live label"
        (expect
          (= {:phase :tool-start :iteration 2 :tool-event {:op "shell" :label "clojure -M:test"}}
             (g->c {"type" "activity"
                    "activity" "tool"
                    "op" "shell"
                    "label" "clojure -M:test"
                    "iteration" 2}))))
    (it "a shell-run activity projects to :shell-run with its command"
        (expect
          (= {:phase :shell-run :iteration 1 :cmd "clojure -M:test"}
             (g->c
               {"type" "activity" "activity" "shell-run" "cmd" "clojure -M:test" "iteration" 1}))))
    (it "a provider-call activity projects to :provider-call"
        (expect (= {:phase :provider-call :iteration 1}
                   (g->c {"type" "activity" "activity" "provider-call" "iteration" 1}))))
    ;; Regression, issue #120: the reason a provider request exists never reached an
    ;; attached tab, so a tool-result continuation read like a fresh user submit.
    (it "a provider-call activity carries its continuation reason"
        (expect (= {:phase :provider-call :iteration 3 :reason :tool-result}
                   (g->c {"type" "activity"
                          "activity" "provider-call"
                          "iteration" 3
                          "reason" "tool-result"}))))))

(defdescribe provider-retry-event-chunk-test
             (let [g->c @#'chat/gateway-event->chunk]
               (it "rehydrates structured retry metadata from the canonical wire event"
                   (let [event (wire/canonical {:type "provider.retry"
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

(defdescribe restore-block-record-test
             ;; The restore chain — persisted envelope → `envelope->block` → `block->form-record`
             ;; — used to be TWO hand-listed projections, so a display field either forgot
             ;; silently vanished on RESUME while the live stream kept it (exactly how
             ;; the card's own `:op` identity was dropped). Both
             ;; builders now project through `vis/form->display` (the ONE display-key
             ;; projection). This guard drives a PERSISTED-SHAPED envelope through the REAL
             ;; restore entry (`it->iteration-entry`) so a drop anywhere in the chain fails.
             (let [it->ie
                   @#'chat/it->iteration-entry

                   restore
                   (fn [env]
                     (-> (it->ie {:produced-answer? false :last-iteration-id :iter-1}
                                 (wire/canonical {:id :iter-1 :code (:src env) :forms [env]}))
                         :forms
                         first))]

               (it "a restored single-result envelope keeps its card identity"
                   (let [rec
                         (restore {:scope "t1/i1"
                                   :tag :host
                                   :src "print(await grep(query='defn', paths=['src']))"
                                   :op "grep"
                                   :result-render "a.clj:1: x"
                                   :result-summary "8 hits in 1 file"})

                         card
                         (vis/result-card rec)]

                     (expect (= "grep" (:op card)))
                     (expect (= "8 hits in 1 file" (:summary card)))))))

;; Regression: a FAILED provider turn's styled card must survive the
;; `turn!`/`attach!` fold. Those fold the engine's provider-error IR onto
;; canonical error blocks are preserved end-to-end.
;; flatten `:error` into plain text on a fresh conversation.
(defdescribe error-content-test
             (it "preserves canonical provider error blocks"
                 (let [blocks [{"id" "e1"
                                "type" "error"
                                "code" "provider_unavailable"
                                "message" "Provider unavailable"
                                "retryable" true}]]
                   (expect (= blocks (chat/error-content {"content" blocks "error" "boom"})))))
             (it "creates a canonical error block when content is absent"
                 (let [out (chat/error-content {"error" "boom"})]
                   (expect (= "error" (get-in out [0 "type"])))
                   (expect (str/includes? (get-in out [0 "message"]) "boom"))
                   ;; The `"code"` IS the bold label both the error card and the
                   ;; companion paint ABOVE the sentence, so the sentence must
                   ;; not repeat it: `turn_failed ERROR: turn failed` said the
                   ;; same word twice.
                   (expect (= "turn_failed" (get-in out [0 "code"])))
                   (expect (not (str/includes? (get-in out [0 "message"]) "ERROR:"))))))

(defdescribe
  gateway-disconnect-propagation-test
  (let [disconnect (ex-info "SSE disconnected" {:gateway-disconnected true :turn-id "turn-1"})]
    (it "preserves submit disconnect metadata for the TUI reattach path"
        (with-redefs [vis/gateway-submit-turn-sync! (fn [& _]
                                                      (throw disconnect))]
          (try (chat/turn! {:id "session-1"} "hello")
               (expect false)
               (catch clojure.lang.ExceptionInfo e (expect (identical? disconnect e))))))
    (it "preserves attach disconnect metadata for the TUI reattach path"
        (with-redefs [vis/gateway-attach-turn-sync! (fn [& _]
                                                      (throw disconnect))]
          (try (chat/attach! {:id "session-1"} "turn-1")
               (expect false)
               (catch clojure.lang.ExceptionInfo e (expect (identical? disconnect e))))))))

(defdescribe
  queue-sync-event-chunk-test
  ;; Queue lifecycle events (from ANY sibling channel) project to :queue-sync
  ;; chunks so every attached TUI mirrors the gateway's queued backlog live.
  (let [g->c @#'chat/gateway-event->chunk]
    (it "turn.queued projects to :add with the prompt text"
        (expect (= {:phase :queue-sync :op :add :turn-id "q1" :text "hi"}
                   (g->c {"type" "turn.queued" "turn_id" "q1" "request" "hi"}))))
    ;; The submitter's correlation id (the idempotency key it sent) rides along so
    ;; the channel that queued the turn binds its optimistic row by ID, not text.
    (it "turn.queued carries the submitter's correlation id when the gateway has one"
        (expect
          (= {:phase :queue-sync :op :add :turn-id "q1" :text "hi" :client-id "cid-1"}
             (g->c
               {"type" "turn.queued" "turn_id" "q1" "request" "hi" "idempotency_key" "cid-1"}))))
    (it "turn.queued.updated projects to :update"
        (expect (= {:phase :queue-sync :op :update :turn-id "q1" :text "hi2"}
                   (g->c {"type" "turn.queued.updated" "turn_id" "q1" "request" "hi2"}))))
    ;; An image queued as a path: the gateway ships the chip preview beside the
    ;; raw request, and the chunk carries BOTH - `:text` still re-attaches on
    ;; edit, `:preview-text` is what the queue strip paints.
    (it "turn.queued carries the gateway's path-free preview when there is one"
        (expect (= {:phase :queue-sync
                    :op :add
                    :turn-id "q1"
                    :text "/tmp/shot.png\nlook"
                    :preview-text "\ud83d\uddbc shot.png look"}
                   (g->c {"type" "turn.queued"
                          "turn_id" "q1"
                          "request" "/tmp/shot.png\nlook"
                          "request_preview" "\ud83d\uddbc shot.png look"}))))
    (it "turn.queued.deleted projects to :delete"
        (expect (= {:phase :queue-sync :op :delete :turn-id "q1"}
                   (g->c {"type" "turn.queued.deleted" "turn_id" "q1"}))))
    (it "a cancelled delete carries the reason and the dropped text back"
        ;; The words are how the editor restores them; the reason is how the
        ;; editor tells a user cancel from a plain row delete.
        (expect
          (= {:phase :queue-sync :op :delete :turn-id "q1" :reason "cancelled" :text "queued words"}
             (g->c {"type" "turn.queued.deleted"
                    "turn_id" "q1"
                    "reason" "cancelled"
                    "request" "queued words"}))))
    (it "turn.queued.drained (gateway auto-start) projects to :delete"
        (expect (= {:phase :queue-sync :op :delete :turn-id "q1"}
                   (g->c {"type" "turn.queued.drained" "turn_id" "q1"}))))
    (it "turn.started projects its submit correlation id with the canonical run-start clock"
        (expect (= {:phase :turn-start
                    :turn-id "t1"
                    :client-id "cid-1"
                    :request "hi"
                    :started-at-ms 1234
                    :server-at-ms nil}
                   (g->c {"type" "turn.started"
                          "turn_id" "t1"
                          "idempotency_key" "cid-1"
                          "request" "hi"
                          "started_at" 1234}))))))

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
        ;; registered session; `session_id` names the ring that copy rode in on,
        ;; and `titled_session_id` names its subject — the chunk must carry THAT.
        (expect (= "other-session"
                   (:session-id (g->c {"type" "session.title_updated"
                                       "session_id" "subscribed-session"
                                       "titled_session_id" "other-session"
                                       "title" "X"})))))))

(defdescribe model-sync-event-chunk-test
             ;; `session.model_updated` — this session was repointed at another model
             ;; SOMEWHERE ELSE (the companion app, a sibling TUI process/tab, an embedded
             ;; caller). Dropping the event left the footer chip naming this process's last
             ;; local pick until the tab was reopened, while the turns already ran on the
             ;; provider/model the gateway had stored.
             (let [g->c @#'chat/gateway-event->chunk]
               (it "projects the newly pinned provider/model pair"
                   (expect (= {:phase :model-sync :provider "zai-coding-plan" :model "glm-4.7"}
                              (g->c {"type" "session.model_updated"
                                     "provider" "zai-coding-plan"
                                     "model" "glm-4.7"}))))
               (it "a cleared override still projects — blank pair, not a dropped event"
                   (expect (= {:phase :model-sync :provider nil :model nil}
                              (g->c {"type" "session.model_updated"}))))))

(defdescribe terminal-event-chunk-test
             ;; The persistent mux must carry terminal lifecycle events independently of
             ;; the blocking submit transport, otherwise a completed backend turn can
             ;; leave an optimistic TUI spinner alive forever.
             (let [g->c @#'chat/gateway-event->chunk]
               (it "projects all canonical terminal event variants"
                   (expect
                     (= [{:phase :turn-terminal :turn-id "t1" :client-id "c1" :status "completed"}
                         {:phase :turn-terminal :turn-id "t2" :client-id nil :status "failed"}
                         {:phase :turn-terminal :turn-id "t3" :client-id nil :status "cancelled"}]
                        (mapv g->c
                              [{"type" "turn.completed"
                                "turn_id" "t1"
                                "idempotency_key" "c1"
                                "status" "completed"} {"type" "turn.failed" "turn_id" "t2"}
                               {"type" "turn.cancelled" "turn_id" "t3"}]))))))

(it "rehydrates a structured iteration error for the transient retry row"
    (let [g->c
          @#'chat/gateway-event->chunk

          chunk
          (g->c (wire/canonical {:type "iteration.error"
                                 :iteration 3
                                 :error "upstream reset"
                                 :error-data {:type :svar.core/http-error
                                              :message "upstream reset"
                                              :status 503}}))]

      (expect (= :iteration-error (:phase chunk)))
      (expect (= {:type :svar.core/http-error :message "upstream reset" :status 503}
                 (:error chunk)))))

(defdescribe user-recording-transcript-fence-test
             ;; A persisted RECORDING has no picture to re-render, so the turn used to carry
             ;; nothing but a `memo.m4a` chip — the terminal cannot play it, and what it SAID
             ;; was unreachable. The gateway transcribed it once on the way in; history now
             ;; appends the words as a collapsed `vis-transcript` fence.
             (let [render @#'chat/user-request-with-images]
               (it "appends what a user's recording says, once, as its own fence"
                   (let [out (render "listen to /tmp/memo.m4a"
                                     (wire/canonical [{:filename "memo.m4a"
                                                       :media-type "audio/mp4"
                                                       :source "user"
                                                       :transcription "buy milk and call back"}]))]
                     (expect (str/includes? out "````vis-transcript"))
                     (expect (str/includes? out "[Transcription #1: memo.m4a]"))
                     (expect (str/includes? out "buy milk and call back"))))
               (it "leaves a turn alone when nothing could read the recording"
                   ;; No engine, or audio it could not decode: the chip is what it always was.
                   (let [out (render "listen to /tmp/memo.m4a"
                                     (wire/canonical [{:filename "memo.m4a"
                                                       :media-type "audio/mp4"
                                                       :source "user"}]))]
                     (expect (not (str/includes? out "vis-transcript")))))
               (it "never speaks for the MODEL's own audio artifact"
                   ;; Only what the human attached belongs in the human's own bubble.
                   (let [out (render "make me a sound"
                                     (wire/canonical [{:filename "reply.m4a"
                                                       :media-type "audio/mp4"
                                                       :source "tool"
                                                       :transcription "spoken answer"}]))]
                     (expect (not (str/includes? out "vis-transcript")))))))

(defdescribe explicit-turn-attachment-test
             (it "passes explicit inline attachments to the canonical gateway request"
                 (let [sent
                       (atom nil)

                       attachments
                       [{:filename "screen.png" :media-type "image/png" :base64 "aW1hZ2U="}]]

                   (with-redefs [vis/gateway-submit-turn-sync! (fn [_sid opts]
                                                                 (reset! sent opts)
                                                                 {"content" []})]
                     (chat/turn! {:id "s1"} "describe this" {:attachments attachments})
                     (expect (= {:request "describe this" :attachments attachments}
                                (select-keys @sent [:request :attachments])))
                     (expect (not (str/includes? (:request @sent) "screen.png")))))))
