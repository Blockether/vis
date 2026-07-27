(ns com.blockether.vis.internal.gateway.state-test
  "Wire-event projection. Form errors ship LEAN, single-surface:
   `block.output` carries human text (message + line/col + hint), never the
   pr-str'd op-error map (which nests host trace/data chains), and is omitted
   entirely when an errored op in the form's sink slice already renders the
   same failure as an op card — the web thread painted that failure twice."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.cancellation :as cancellation]
            [com.blockether.vis.internal.gateway.bus :as bus]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance :as persistance]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private tool-error
  {:message "rg spec has unknown keys: spec."
   :data {:phase :python/host :type :vis/tool-failure :symbol :rg}})

(defdescribe
  thinking-newline-normalization-test
  "Gateway-owned thinking normalization keeps live SSE, poll/replay, and session
   consumers in sync. A client may still render defensively, but it must
   not be the first place where blank-line runs disappear."
  (it "streams reasoning deltas with normalized thinking over the gateway"
      (let
        [[type store? payload] (#'state/chunk->event
                                {:phase :reasoning
                                 :iteration 1
                                 :thinking " first  \n\n\t\nsecond\r\n\r\nthird  "
                                 :stream-block-id "t1:reasoning:1"
                                 :stream-delta "first\nsecond\nthird"})]
        (expect (= "content.block.delta" type))
        (expect store?)
        (expect (= "first\nsecond\nthird" (:text payload)))))
  (it "normalizes iteration-boundary thinking for pinned session history"
      (let
        [[type store? payload]
         (#'state/chunk->event
          {:phase :iteration-final :done? true :thinking " alpha\n\n\n beta  \n\t\n gamma "})]
        (expect (= "iteration.completed" type))
        (expect store?)
        (expect (= "alpha\n beta\n gamma" (:thinking payload)))))
  (it "normalizes persisted transcript thinking the same way as live events"
      (with-redefs
        [persistance/db-list-session-turns
         (fn [_ sid]
           [{:id sid :status :success}])

         persistance/db-list-session-turn-iterations
         (fn [_ _]
           [{:thinking " alpha\n\n beta  \n"}])]

        (expect (= "alpha\n beta"
                   (-> (state/transcript :session-1)
                       first
                       (get "iterations")
                       first
                       (get "thinking")))))))

(defdescribe transcript-bubble-footer-test
             (it "ships the exact shared TUI footer and routing note to remote channels"
                 (with-redefs
                   [persistance/db-list-session-turns
                    (fn [_ sid]
                      [{:id sid
                        :status :success
                        :provider :openai
                        :model "gpt/5.4"
                        :input-tokens 11461
                        :input-cache-read-tokens 4096
                        :output-tokens 35
                        :total-cost 0.007
                        :duration-ms 2300}])

                    persistance/db-list-session-turn-iterations
                    (fn [_ _]
                      [{:llm-selected {:provider :anthropic :model "claude/opus"}
                        :llm-actual {:provider :openai :model "gpt/5.4"}
                        :llm-fallback? true
                        :llm-routing-trace [{:event/type :llm.routing/provider-retry}
                                            {:event/type :llm.routing/provider-fallback
                                             :status 429}]}])]

                   (let [turn (first (state/transcript :session-1))]
                     (expect (= "openai/gpt-5.4  ·  11.5k→35 (cached 4.1k)  ·  ~$0.0070  ·  2.3s"
                                (get turn "meta_summary")))
                     (expect (= "↳ from anthropic/claude-opus — 429, retried 1×"
                                (get turn "meta_fallback_note")))))))

(defdescribe
  form-result-error-wire-test
  ;; Op cards are gone, so there is no op-dedup: a form error ALWAYS surfaces
  ;; on the wire. What remains to assert is the lean text shape.
  (it "ships a lean text error (message + line/col + hint), never the pr-str'd map"
      (let
        [[_ _ payload] (#'state/chunk->event
                        {:phase :form-result
                         :position 0
                         :code "1/0"
                         :error {:message "ZeroDivisionError: division by zero"
                                 :hint "check denominator"
                                 :data {:phase :python/runtime :line 1 :column 1}}})]
        (expect (= "ZeroDivisionError: division by zero (line 1, col 1)\nhint: check denominator"
                   (:error payload)))
        (expect (not (str/includes? (:error payload) ":data")))))
  (it "a form error always surfaces on the wire"
      (let
        [[type _ payload] (#'state/chunk->event
                           {:phase :form-result :position 0 :code "rg(...)" :error tool-error})]
        (expect (= "block.output" type))
        (expect (= "rg spec has unknown keys: spec." (:error payload))))))

(defdescribe
  activity-wire-event-test
  "Coarse live-progress phases (provider wait, response parse, nested shell/tool
   call) ship as an EPHEMERAL `activity` event (store? false) so a long call
   never leaves the bubble frozen; nothing persists into the durable trace."
  (it "a nested tool-start ships an ephemeral activity event naming the op"
      (let
        [[type store? payload] (#'state/chunk->event
                                {:phase :tool-start :iteration 2 :tool-event {:op :shell}})]
        (expect (= "activity" type))
        (expect (false? store?))
        (expect (= {:activity "tool" :iteration 2 :op "shell"} payload))))
  (it "provider-call and shell-run project to ephemeral activity events"
      (expect (= ["activity" false {:activity "provider-call" :iteration 1}]
                 (#'state/chunk->event {:phase :provider-call :iteration 1})))
      (expect (= ["activity" false {:activity "shell-run" :iteration 1 :cmd "clojure -M:test"}]
                 (#'state/chunk->event {:phase :shell-run :iteration 1 :cmd "clojure -M:test"}))))
  (it "response-parse :done does NOT emit an activity event (the parse finished)"
      (let [[type] (#'state/chunk->event {:phase :response-parse :iteration 1 :status :done})]
        (expect (not= "activity" type)))))

(defdescribe provider-retry-wire-event-test
             (it "ships structured retry metadata instead of an opaque detail string"
                 (let
                   [[type store? payload] (#'state/chunk->event
                                           {:phase :provider-retry-reset
                                            :iteration 2
                                            :attempt 1
                                            :max-retries 3
                                            :delay-ms 1000
                                            :error {:type :svar.llm/provider-unavailable
                                                    :message "Provider unavailable"
                                                    :mini-trace ["must not cross the wire"]}
                                            :event {:event/type :llm.routing/provider-retry
                                                    :reason :provider-unavailable
                                                    :provider "openai"
                                                    :model "gpt-x"
                                                    :attempt 1
                                                    :delay-ms 1000}})]
                   (expect (= "provider.retry" type))
                   (expect store?)
                   (expect (= 2 (:iteration payload)))
                   (expect (= {:type :svar.llm/provider-unavailable :message "Provider unavailable"}
                              (:error payload)))
                   (expect (= 1 (:attempt payload)))
                   (expect (= 3 (:max-retries payload)))
                   (expect (= 1000 (:delay-ms payload)))
                   (expect (not (contains? (:error payload) :mini-trace))))))

(defdescribe form-event-iteration-wire-test
             ;; THE "live shows reasoning but no code" bug: every streaming chunk carries
             ;; its iteration POSITION under `:iteration`, and that MUST ride the wire
             ;; event — `make-progress-tracker` DROPS any chunk with no iteration, which is
             ;; how `block.started` / `block.output` once lost their forms and the live
             ;; bubble showed reasoning but no code.
             (it "block.started carries :iteration on the wire"
                 (let
                   [[type _ payload]
                    (#'state/chunk->event
                     {:phase :form-start :iteration 1 :position 0 :code "import hashlib"})]
                   (expect (= "block.started" type))
                   (expect (= 1 (:iteration payload)))))
             (it "block.output carries :iteration on the wire"
                 (let
                   [[type _ payload]
                    (#'state/chunk->event
                     {:phase :form-result :iteration 3 :position 0 :code "print(42)" :stdout "42"})]
                   (expect (= "block.output" type))
                   (expect (= 3 (:iteration payload)))))
             (it "reasoning streams as a replayable typed block delta"
                 (let
                   [[type store? payload] (#'state/chunk->event
                                           {:phase :reasoning
                                            :iteration 2
                                            :thinking "hmm"
                                            :stream-block-id "t1:reasoning:2"
                                            :stream-delta "hmm"})]
                   (expect (= "content.block.delta" type))
                   (expect store?)
                   (expect (= 2 (:iteration payload)))
                   (expect (= "t1:reasoning:2" (:block_id payload)))
                   (expect (= "text" (:field payload)))
                   (expect (= "hmm" (:text payload)))))
             (it "iteration-final carries :iteration and complete assistant prose on the wire"
                 (let
                   [[type _ payload] (#'state/chunk->event
                                      {:phase :iteration-final
                                       :iteration 5
                                       :done true
                                       :thinking "t"
                                       :assistant-prose " full prose "})]
                   (expect (= "iteration.completed" type))
                   (expect (= 5 (:iteration payload)))
                   (expect (= "full prose" (:assistant-prose payload)))))
             (it "native-call preview is a distinct replayable block event"
                 (let
                   [[type store? payload] (#'state/chunk->event
                                           {:phase :tool-preview
                                            :iteration 1
                                            :position 0
                                            :code "print(4"
                                            :vis/tool-name "native_call"
                                            :tool-color-role :tool-color/meta
                                            :result-summary "run_python"
                                            :svar/tool-call-id "call_1"})]
                   (expect (= "block.preview" type))
                   (expect store?)
                   (expect (= 1 (:iteration payload)))
                   (expect (= 0 (:block_id payload)))
                   (expect (= "print(4" (:code payload)))
                   (expect (= "native_call" (:tool_name payload)))
                   (expect (= "call_1" (:tool_call_id payload))))))

(defdescribe
  iteration-attachment-descriptor-wire-test
  ;; Live `iteration.completed` carries metadata-ONLY attachment descriptors
  ;; (NEVER base64) so a native client (iOS/RN) learns an image was produced and
  ;; lazy-fetches it from the byte endpoint. `:index` is the position in the SAME
  ;; ordered list the byte endpoint serves, so index N always names one artifact.
  (it "omits :attachments when the iteration produced none"
      (let
        [[type _ payload] (#'state/chunk->event
                           {:phase :iteration-final :iteration 2 :done true :thinking "t"})]
        (expect (= "iteration.completed" type))
        (expect (not (contains? payload :attachments)))))
  (it "omits :attachments when there is no iteration-id to address them"
      (let
        [[_ _ payload] (#'state/chunk->event
                        {:phase :iteration-final :iteration 2 :done true :attachment-count 3})]
        (expect (not (contains? payload :attachments)))))
  (it "projects lean snake-case descriptors and NEVER leaks base64"
      (with-redefs
        [state/iteration-attachments (fn [_iid]
                                       [{:tool-call-id "call_A"
                                         :kind "image"
                                         :media-type "image/png"
                                         :filename "fig.png"
                                         :size 1234
                                         :base64 "SECRET"}
                                        {:tool-call-id nil :media-type "image/svg+xml" :size 0}])]
        (let
          [[_ _ payload] (#'state/chunk->event
                          {:phase :iteration-final
                           :iteration 4
                           :done false
                           :iteration-id "00000000-0000-0000-0000-0000000000ab"
                           :attachment-count 2})
           atts (:attachments payload)]

          (expect (= 2 (count atts)))
          (expect (= [0 1] (mapv :index atts)))
          (expect (= "image/png" (:media_type (first atts))))
          (expect (= "call_A" (:tool_call_id (first atts))))
          (expect (= 1234 (:size (first atts))))
          ;; default kind for the un-kinded second artifact
          (expect (= "image" (:kind (second atts))))
          ;; bytes NEVER ride the frame
          (expect (not (str/includes? (pr-str payload) "SECRET")))
          (expect (every? #(not (contains? % :base64)) atts))))))

(defdescribe
  broadcast-title-poll-parity-test
  "A sibling-session title update must be STORED on every other registered
   session, so the /poll fallback (which reads the replay ring and never
   registers as a subscriber) delivers the identical frame the live SSE
   client gets. Previously the copy was live-only + gated on `:subscribers`,
   so poll clients silently missed it."
  (it
    "stores the sibling title event on a session with NO subscriber (poll-only)"
    (let
      [a
       (java.util.UUID/randomUUID)

       b
       (java.util.UUID/randomUUID)

       registry
       @#'state/registry

       saved
       @registry]

      (try
        ;; b carries no :subscribers — exactly a client on the /poll fallback
        (reset! registry {a {:next-seq 0} b {:next-seq 0}})
        (#'state/broadcast-title-event! a "Tidal Forces")
        (let
          [a-events
           (state/events-since a 0)

           b-events
           (state/events-since b 0)]

          ;; the titled session keeps its own stored event
          (expect (= 1 (count a-events)))
          (expect (= "session.title_updated" (get (first a-events) "type")))
          ;; the sibling (poll-only) session ALSO has it stored → /poll sees it
          (expect (= 1 (count b-events)))
          (expect (= "session.title_updated" (get (first b-events) "type")))
          ;; foreign copy carries the TITLED session's id, not b's
          (expect (= (str a) (get (first b-events) "session_id"))))
        (finally (reset! registry saved))))))

(defdescribe canonical-answer-content-test
             (it "normalizes Markdown to one prose block"
                 (let [blocks (#'state/answer-content {:answer "## hello"})]
                   (expect (= 1 (count blocks)))
                   (expect (= "prose" (get-in blocks [0 "type"])))
                   (expect (= "## hello" (get-in blocks [0 "markdown"])))))
             (it "passes typed error content without creating a second answer shape"
                 (let
                   [blocks [{"id" "e1"
                             "type" "error"
                             "code" "provider_unavailable"
                             "message" "Provider failed"
                             "retryable" true}]]
                   (expect (= blocks (#'state/answer-content blocks))))))

(defdescribe
  list-turns-dedup-test
  "A refreshed web page hydrates from gateway/list-turns. Once the engine DB row
  exists, the completed gateway overlay row must disappear; otherwise the last
  request/response pair renders twice and the transient duplicate has no DB
  iterations disclosure."
  (it
    "prefers the persisted row over a matching completed live row with engine id"
    (let
      [sid
       (java.util.UUID/randomUUID)

       gateway-id
       "gateway-turn"

       engine-id
       (java.util.UUID/randomUUID)

       registry
       @#'state/registry

       saved
       @registry]

      (try (reset! registry {sid {:next-seq 0
                                  :turn-order [gateway-id]
                                  :turns {gateway-id {:turn_id gateway-id
                                                      :engine_turn_id (str engine-id)
                                                      :session_id (str sid)
                                                      :status "completed"
                                                      :request "hello"
                                                      :content
                                                      [{"id" "b1" "type" "prose" "markdown" "hi"}]
                                                      :started_at 1000}}}})
           (with-redefs
             [persistance/db-list-session-turns (fn [_ _]
                                                  [{:id engine-id
                                                    :status :success
                                                    :user-request "hello"
                                                    :content
                                                    [{"id" "b1" "type" "prose" "markdown" "hi"}]
                                                    :iteration-count 2
                                                    :input-tokens 1200
                                                    :input-regular-tokens 500
                                                    :input-cache-write-tokens 100
                                                    :input-cache-read-tokens 600
                                                    :output-tokens 150
                                                    :output-reasoning-tokens 80
                                                    :total-cost 0.0123
                                                    :provider :openai
                                                    :model "gpt-4o"
                                                    :created-at (java.util.Date. 1010)}])]
             (let [turns (state/list-turns sid)]
               (expect (= 1 (count turns)))
               (let [turn (first turns)]
                 (expect (= (str engine-id) (get turn "turn_id")))
                 (expect (= 2 (get turn "iteration_count")))
                 (expect (= {"input" 1200
                             "input_regular" 500
                             "cache_created" 100
                             "cached" 600
                             "output" 150
                             "reasoning" 80}
                            (get turn "tokens")))
                 (expect (= {"total_cost" 0.0123 "provider" "openai" "model" "gpt-4o"}
                            (get turn "cost"))))))
           (finally (reset! registry saved)))))
  (it
    "prefers the persisted row over a matching completed live row with no engine id"
    (let
      [sid
       (java.util.UUID/randomUUID)

       gateway-id
       "gateway-turn"

       engine-id
       (java.util.UUID/randomUUID)

       started
       1000

       registry
       @#'state/registry

       saved
       @registry]

      (try (reset! registry {sid {:next-seq 0
                                  :turn-order [gateway-id]
                                  :turns {gateway-id {:turn_id gateway-id
                                                      :session_id (str sid)
                                                      :status "completed"
                                                      :request "hello"
                                                      :content
                                                      [{"id" "b1" "type" "prose" "markdown" "hi"}]
                                                      :started_at started}}}})
           (with-redefs
             [persistance/db-list-session-turns (fn [_ s]
                                                  (expect (= sid s))
                                                  [{:id engine-id
                                                    :status :success
                                                    :user-request "hello"
                                                    :content
                                                    [{"id" "b1" "type" "prose" "markdown" "hi"}]
                                                    :iteration-count 2
                                                    :created-at (java.util.Date. (+ started 10))}])]
             (let [turns (state/list-turns sid)]
               (expect (= 1 (count turns)))
               (expect (= (str engine-id) (get (first turns) "turn_id")))
               (expect (= 2 (get (first turns) "iteration_count")))))
           (finally (reset! registry saved))))))

(defdescribe
  list-queued-turns-test
  "`GET /turns?status=queued` is a poll: a tray asks it every few seconds only to
  learn whether anything is waiting. It must therefore return the queued overlay
  rows and NOTHING else — no completed history, and no DB hydration at all."
  (it
    "returns only queued rows and never touches turn-history persistence"
    (let
      [sid
       (java.util.UUID/randomUUID)

       registry
       @#'state/registry

       saved
       @registry]

      (try (reset! registry {sid {:next-seq 0
                                  :turn-order ["done" "waiting"]
                                  :turns {"done" {:turn_id "done"
                                                  :session_id (str sid)
                                                  :status "completed"
                                                  :request "already answered"
                                                  :content
                                                  [{"id" "b1" "type" "prose" "markdown" "hi"}]
                                                  :started_at 1000}
                                          "waiting" {:turn_id "waiting"
                                                     :session_id (str sid)
                                                     :status "queued"
                                                     :request "next please"
                                                     :queued_at 2000}}}})
           (with-redefs
             [persistance/db-list-session-turns
              (fn [_ _]
                (throw (ex-info "queued poll must not hydrate history" {})))]
             (let [turns (state/list-queued-turns sid)]
               (expect (= 1 (count turns)))
               (expect (= "waiting" (get (first turns) "turn_id")))
               (expect (= "queued" (get (first turns) "status")))))
           (finally (reset! registry saved)))))
  (it "is empty for a session with nothing waiting"
      (let
        [sid
         (java.util.UUID/randomUUID)

         registry
         @#'state/registry

         saved
         @registry]

        (try (reset! registry {sid {:next-seq 0
                                    :turn-order ["done"]
                                    :turns {"done" {:turn_id "done"
                                                    :session_id (str sid)
                                                    :status "completed"
                                                    :request "already answered"
                                                    :started_at 1000}}}})
             (expect (= [] (state/list-queued-turns sid)))
             (finally (reset! registry saved))))))

(defdescribe
  transcript-page-test
  "The transcript window exists so a client never pays for history it will not
   paint: a long session is tens of megabytes and nearly all of that cost is
   per-turn hydration. The window must therefore be sliced BEFORE hydration,
   and the cursor must be an index — `:position` is neither unique nor
   monotonic in real sessions, so it cannot page anything."
  (it
    "hydrates ONLY the requested window and reports the full total"
    (let
      [sid
       (java.util.UUID/randomUUID)

       rows
       (mapv (fn [n]
               {:id (str "turn-" n) :position 1})
             (range 10))

       hydrated
       (atom [])]

      (with-redefs-fn {#'lp/db-info (constantly ::db)
                       #'persistance/db-list-session-turns (fn [_ _]
                                                             rows)
                       #'persistance/db-list-turns-attachments (fn [_ ids]
                                                                 (zipmap ids (repeat [])))
                       #'state/transcript-turn (fn [_db _att row]
                                                 (swap! hydrated conj (:id row))
                                                 {:turn_id (:id row)})}
        (fn []
          ;; Newest page: no offset given, so the window is the LAST `limit` rows.
          (let [page (state/transcript-page sid {:limit 3})]
            (expect (= ["turn-7" "turn-8" "turn-9"] (mapv #(get % "turn_id") (:turns page))))
            (expect (= 10 (:total page)))
            (expect (= 7 (:offset page)))
            (expect (:has-more page))
            ;; The saving IS this: seven older turns were never hydrated. A
            ;; windowed page hydrates NEWEST-first so the byte budget can stop
            ;; early, so that is the order the rows are visited in.
            (expect (= ["turn-9" "turn-8" "turn-7"] @hydrated)))
          ;; Paging backwards from that window reaches the very beginning.
          (reset! hydrated [])
          (let [page (state/transcript-page sid {:offset 0 :limit 7})]
            (expect (= "turn-0" (get (first (:turns page)) "turn_id")))
            (expect (= "turn-6" (get (last (:turns page)) "turn_id")))
            (expect (= 0 (:offset page)))
            (expect (not (:has-more page)))
            (expect (= 7 (count @hydrated))))
          ;; No window asked for = the whole transcript, so an older client that
          ;; sends no params still gets exactly what it always got.
          (expect (= 10 (count (:turns (state/transcript-page sid {})))))
          ;; A cursor past the end is clamped, never an exception or a wrap-around.
          (let [page (state/transcript-page sid {:offset 99999 :limit 5})]
            (expect (= [] (:turns page)))
            (expect (= 10 (:offset page)))
            (expect (:has-more page)))))))
  (it "caps a WINDOWED page in BYTES, dropping the oldest rows and raising the offset"
      ;; Turn COUNT does not bound bytes: one real 38-turn session encodes its
      ;; newest 24 turns to 9.5 MB because a single turn carried a 5 MB tool
      ;; result. A page that big is the cost paging exists to avoid.
      (let
        [sid
         (java.util.UUID/randomUUID)

         rows
         (mapv (fn [n]
                 {:id (str "turn-" n) :position 1})
               (range 10))

         hydrated
         (atom [])]

        (with-redefs-fn
          {#'lp/db-info (constantly ::db)
           #'persistance/db-list-session-turns (fn [_ _]
                                                 rows)
           #'persistance/db-list-turns-attachments (fn [_ ids]
                                                     (zipmap ids (repeat [])))
           (ns-resolve 'com.blockether.vis.internal.gateway.state 'TRANSCRIPT_PAGE_MAX_BYTES) 2000
           #'state/transcript-turn (fn [_db _att row]
                                     (swap! hydrated conj (:id row))
                                     {:turn_id (:id row) :text (apply str (repeat 900 "x"))})}
          (fn []
            (let [page (state/transcript-page sid {:limit 10})]
              ;; ~930 encoded bytes per row against a 2000-byte budget: two rows
              ;; fit, the third busts it and is left for the next page.
              (expect (= ["turn-8" "turn-9"] (mapv #(get % "turn_id") (:turns page))))
              (expect (= 10 (:total page)))
              ;; The dropped rows raise the offset — which is exactly where the
              ;; client's next `load earlier` resumes, so nothing is skipped.
              (expect (= 8 (:offset page)))
              (expect (:has-more page))
              ;; Newest-first hydration: the seven rows older than the buster are
              ;; never hydrated at all.
              (expect (= ["turn-9" "turn-8" "turn-7"] @hydrated)))
            (reset! hydrated [])
            ;; NO window asked for is the TUI's whole-transcript read: never
            ;; budgeted, or resuming the TUI would silently lose history.
            (let [page (state/transcript-page sid {})]
              (expect (= 10 (count (:turns page))))
              (expect (= 0 (:offset page)))
              (expect (not (:has-more page))))))))
  (it "keeps one row even when that row alone busts the budget, so paging always advances"
      (let
        [sid
         (java.util.UUID/randomUUID)

         rows
         (mapv (fn [n]
                 {:id (str "turn-" n) :position 1})
               (range 10))]

        (with-redefs-fn
          {#'lp/db-info (constantly ::db)
           #'persistance/db-list-session-turns (fn [_ _]
                                                 rows)
           #'persistance/db-list-turns-attachments (fn [_ ids]
                                                     (zipmap ids (repeat [])))
           (ns-resolve 'com.blockether.vis.internal.gateway.state 'TRANSCRIPT_PAGE_MAX_BYTES) 100
           #'state/transcript-turn (fn [_db _att row]
                                     {:turn_id (:id row) :text (apply str (repeat 900 "x"))})}
          (fn []
            ;; An empty page would strand the client: it would fetch forever and
            ;; paint nothing.
            (let [page (state/transcript-page sid {:limit 5})]
              (expect (= ["turn-9"] (mapv #(get % "turn_id") (:turns page))))
              (expect (= 9 (:offset page)))
              (expect (:has-more page)))
            ;; And the page BEFORE it advances by one too, so a walk terminates.
            (let [page (state/transcript-page sid {:offset 4 :limit 5})]
              (expect (= ["turn-8"] (mapv #(get % "turn_id") (:turns page))))
              (expect (= 8 (:offset page))))))))
  (it "drops the lazy provider envelope instead of shipping a Delay"
      (let [sid (java.util.UUID/randomUUID)]
        (with-redefs-fn {#'lp/db-info (constantly ::db)
                         #'persistance/db-list-session-turns (fn [_ _]
                                                               [{:id "turn-0" :position 1}])
                         #'persistance/db-list-turns-attachments (fn [_ ids]
                                                                   (zipmap ids (repeat [])))
                         #'persistance/db-list-session-turn-iterations
                         (fn [_ _]
                           [{:id "it-0"
                             :position 1
                             :llm-assistant-message (delay {:content [{:type "text"}]})}])}
          (fn []
            (let
              [turn (first (:turns (state/transcript-page sid {})))
               iteration (first (get turn "iterations"))]

              ;; Guard against a swallowed hydration failure passing this vacuously.
              (expect (= "it-0" (get iteration "id")))
              ;; Persistence hands the envelope back as a `<-json-lazy` DELAY, which
              ;; JSON-encodes to the useless string "clojure.lang.Delay@1f2e3d" —
              ;; 3031 of a real 247-turn session's 3098 iterations shipped that.
              (expect (not (contains? iteration "llm_assistant_message")))))))))

(defdescribe
  queued-update-payload-test
  "Editing a queued request must also edit the provider message payload;
  otherwise the drained queued turn answers the pre-edit prompt."
  (it "replaces the last user message content"
      (let
        [messages [{:role "system" :content "rules"} {:role "user" :content "old prompt"}
                   {:role "assistant" :content "old answer"} {:role :user :content "queued old"}]]
        (expect (= [{:role "system" :content "rules"} {:role "user" :content "old prompt"}
                    {:role "assistant" :content "old answer"} {:role :user :content "queued new"}]
                   (#'state/replace-last-user-message-content messages "queued new"))))))

(defdescribe
  persisted-duplicate-of-live-test
  ;; Terminal identity is request + status + timestamps; content blocks belong
  ;; to the durable row and are not duplicated onto terminal events.
  (let
    [dup?
     #'state/persisted-duplicate-of-live?

     at
     (fn [ms]
       (java.util.Date. (long ms)))]

    (it "dedups an error turn whose live row has no answer to compare"
        (expect
          (dup?
            {:engine_turn_id nil :status "error" :request "add zprint" :content [] :started_at 1000}
            {:id "soul-1"
             :user-request "add zprint"
             :content
             [{"id" "e1" "type" "error" "code" "failed" "message" "Could not reach provider"}]
             :created-at (at 2000)})))
    (it "does NOT over-dedup two distinct completed answers with the same request"
        (expect (not (dup? {:engine_turn_id nil
                            :status "completed"
                            :request "hi"
                            :content [{"id" "a" "type" "prose" "markdown" "answer A"}]
                            :started_at 1000}
                           {:id "soul-2"
                            :user-request "hi"
                            :content [{"id" "b" "type" "prose" "markdown" "answer B"}]
                            :created-at (at 2000)}))))
    (it "still matches on the engine-turn-id primary key"
        (expect (dup? {:engine_turn_id "eng-9" :status "completed"} {:id "eng-9"})))))

(defdescribe
  mirror-turn-row-test
  "A turn running in a SIBLING process arrives only as bus-mirrored events. To
   render it like a locally-started turn (user bubble + running chip, not bare
   deltas leaking under the previous answer), `ingest-mirrored-event!` must
   materialize a running row in :turns/:turn-order on `turn.started` and mark it
   terminal on `turn.completed`/`turn.failed`/`turn.cancelled` — carrying :engine_turn_id so
   list-turns can dedup it against the durable DB row once persisted."
  (let
    [reg
     @#'state/registry

     sid
     "mirror-test-sid"]

    (it
      "materializes a running row on turn.started, terminal on turn.completed"
      (try (swap! reg assoc sid {:next-seq 0})
           (#'state/ingest-mirrored-event!
            sid
            false
            {"type" "turn.started" "turn_id" "T1" "request" "hello world" "started_at" 777})
           (let [started (get @reg sid)]
             (expect (= "T1" (:current-turn started)))
             (expect (= ["T1"] (:turn-order started)))
             (expect (= "running" (get-in started [:turns "T1" :status])))
             (expect (= "hello world" (get-in started [:turns "T1" :request])))
             ;; The turn row owns its replay boundary. Channels must not scan
             ;; the event ring to rediscover the matching turn.started event.
             (expect (= 1 (get-in started [:turns "T1" :event_start_seq])))
             (expect (= 1 (get (state/get-turn sid "T1") "event_start_seq")))
             ;; the mirror adopts the PRODUCER's canonical run-start clock —
             ;; stamping mirror-local time desynced elapsed across processes
             (expect (= 777 (get-in started [:turns "T1" :started_at]))))
           (#'state/ingest-mirrored-event!
            sid
            false
            {"type" "turn.completed" "turn_id" "T1" "status" "completed" "engine_turn_id" "E1"})
           (let [done (get @reg sid)]
             (expect (nil? (:current-turn done)))
             (expect (= "completed" (get-in done [:turns "T1" :status])))
             (expect (= "E1" (get-in done [:turns "T1" :engine_turn_id]))))
           (swap! reg assoc sid {:next-seq 0})
           (#'state/ingest-mirrored-event!
            sid
            false
            {"type" "turn.started" "turn_id" "T2" "request" "please stop"})
           (#'state/ingest-mirrored-event!
            sid
            false
            {"type" "turn.cancelled" "turn_id" "T2" "status" "cancelled"})
           (let [cancelled (get @reg sid)]
             (expect (nil? (:current-turn cancelled)))
             (expect (= "cancelled" (get-in cancelled [:turns "T2" :status]))))
           (finally (swap! reg dissoc sid))))
    (it "ignores mirrored events for a session this process never touched"
        (expect (nil? (#'state/ingest-mirrored-event!
                       "never-touched-sid"
                       false
                       {"type" "turn.started" "turn_id" "X" "request" "hi"})))
        (expect (not (contains? @reg "never-touched-sid"))))))

(defdescribe
  queue-drain-mirror-event-test
  (it "broadcasts queue drain live without adding it to replay persistence"
      (let
        [sid
         (str "drain-test-" (java.util.UUID/randomUUID))

         registry
         @#'state/registry

         launched
         (atom nil)

         seen
         (atom [])]

        (try (swap! registry assoc
               sid
               {:next-seq 0
                :subscribers {"test" #(swap! seen conj %)}
                :turns
                {"q1"
                 {:turn_id "q1" :session_id sid :status "queued" :request "hello" :queued_at 1}}
                :turn-order ["q1"]})
             (with-redefs-fn {#'state/launch-turn-worker! (fn [& args]
                                                            (reset! launched (vec (take 2 args))))}
               #(#'state/drain-next-queued! sid))
             (expect (= [sid "q1"] @launched))
             (expect (= "streaming" (get (state/get-turn sid "q1") "status")))
             (expect (= ["turn.queued.drained"] (mapv #(get % "type") @seen)))
             (expect (empty? (state/events-since sid 0)))
             (finally (swap! registry dissoc sid))))))

(defdescribe
  drain-idle-test
  ;; Auto-start on open/resume: an attaching channel calls `drain-idle!` to kick
  ;; an orphaned queued backlog into motion — but ONLY when the session is idle.
  ;; A turn already in flight must be left alone (one engine turn per session).
  (it "drain-idle! starts the queued head when the session is idle"
      (let
        [sid
         (str "drain-idle-" (java.util.UUID/randomUUID))

         registry
         @#'state/registry

         launched
         (atom nil)]

        (try (swap! registry assoc
               sid
               {:next-seq 0
                :turns {"q1"
                        {:turn_id "q1" :session_id sid :status "queued" :request "hi" :queued_at 1}}
                :turn-order ["q1"]})
             (with-redefs-fn {#'state/launch-turn-worker! (fn [& args]
                                                            (reset! launched (vec (take 2 args))))}
               #(state/drain-idle! sid))
             (expect (= [sid "q1"] @launched))
             (expect (= "streaming" (get (state/get-turn sid "q1") "status")))
             (finally (swap! registry dissoc sid)))))
  (it "drain-idle! is a no-op while a turn is already running"
      (let
        [sid
         (str "drain-idle-busy-" (java.util.UUID/randomUUID))

         registry
         @#'state/registry

         launched
         (atom nil)

         result
         (atom :unset)]

        (try (swap! registry assoc
               sid
               {:next-seq 0
                :current-turn "r0"
                :turns {"r0" {:turn_id "r0" :session_id sid :status "running" :request "run"}
                        "q1"
                        {:turn_id "q1" :session_id sid :status "queued" :request "hi" :queued_at 1}}
                :turn-order ["r0" "q1"]})
             (with-redefs-fn {#'state/launch-turn-worker! (fn [& args]
                                                            (reset! launched (vec (take 2 args))))}
               #(reset! result (state/drain-idle! sid)))
             (expect (nil? @result))
             (expect (nil? @launched))
             (expect (= "queued" (get (state/get-turn sid "q1") "status")))
             (finally (swap! registry dissoc sid))))))

(defdescribe
  cancelled-backlog-test
  ;; The queue-storm regression: a user cancel must not merely SKIP the backlog
  ;; it stopped, it must DELETE it. Anything left `queued` is a live wire — any
  ;; later terminal, attach kick or resume can start it minutes and turns later.
  (it
    "a user cancel drops the pre-cancel backlog and keeps post-cancel intent"
    (let
      [sid
       (str "cancel-backlog-" (java.util.UUID/randomUUID))

       registry
       @#'state/registry

       launched
       (atom [])

       seen
       (atom [])]

      (try
        (swap! registry assoc
          sid
          {:next-seq 0
           :subscribers {"test" #(swap! seen conj %)}
           :turns
           {"r0" {:turn_id "r0" :session_id sid :status "running" :cancelling_at 100}
            "old" {:turn_id "old" :session_id sid :status "queued" :request "before" :queued_at 50}
            "new" {:turn_id "new" :session_id sid :status "queued" :request "after" :queued_at 150}}
           :turn-order ["r0" "old" "new"]
           :idempotency {"k-old" "old" "k-new" "new"}})
        (with-redefs-fn {#'state/launch-turn-worker! (fn [& args]
                                                       (swap! launched conj (vec (take 2 args))))
                         #'cancellation/cancelled? (constantly true)}
          #(#'state/after-turn-terminal!
             sid
             "r0"
             {:failed? false :transient? false :cancel-token :tok :stalled? false}))
        ;; the stopped backlog is GONE (not merely skipped) — registry row,
        ;; order entry and idempotency key all released
        (expect (nil? (state/get-turn sid "old")))
        (expect (= ["r0" "new"] (get-in @registry [sid :turn-order])))
        (expect (= {"k-new" "new"} (get-in @registry [sid :idempotency])))
        ;; clients are told, so a mirrored "Queued" row and any waiter settle
        (expect (contains? (set (mapv #(get % "type") @seen)) "turn.queued.deleted"))
        ;; "stop that, run THIS" still runs
        (expect (= [[sid "new"]] @launched))
        (finally (swap! registry dissoc sid)))))
  (it "a stalled force-cancel is a failure, not a user stop: backlog survives"
      (let
        [sid
         (str "cancel-stall-" (java.util.UUID/randomUUID))

         registry
         @#'state/registry

         launched
         (atom [])]

        (try (swap! registry assoc
               sid
               {:next-seq 0
                :turns
                {"r0" {:turn_id "r0" :session_id sid :status "running" :cancelling_at 100}
                 "old"
                 {:turn_id "old" :session_id sid :status "queued" :request "before" :queued_at 50}}
                :turn-order ["r0" "old"]})
             (with-redefs-fn {#'state/launch-turn-worker! (fn [& args]
                                                            (swap! launched conj
                                                              (vec (take 2 args))))
                              #'cancellation/cancelled? (constantly true)}
               #(#'state/after-turn-terminal!
                  sid
                  "r0"
                  {:failed? false :transient? false :cancel-token :tok :stalled? true}))
             (expect (= [[sid "old"]] @launched))
             (finally (swap! registry dissoc sid)))))
  (it
    "no path resurrects a cancel-stopped head; an explicit resume still can"
    (let
      [sid
       (str "cancel-gate-" (java.util.UUID/randomUUID))

       registry
       @#'state/registry

       launched
       (atom [])

       seed!
       #(swap! registry assoc
          sid
          {:next-seq 0
           ;; the floor a completed user cancel left behind
           :cancel-floor 100
           ;; …and a paused queue, the state an explicit resume acts on
           :queue-paused {:reason "provider_error" :held 1 :fails 1 :gen 1}
           :turns
           {"r0" {:turn_id "r0" :session_id sid :status "completed" :cancelling_at 100}
            "old" {:turn_id "old" :session_id sid :status "queued" :request "before" :queued_at 50}}
           :turn-order ["r0" "old"]})]

      (try (seed!)
           (with-redefs-fn {#'state/launch-turn-worker! (fn [& args]
                                                          (swap! launched conj
                                                            (vec (take 2 args))))}
             (fn []
               ;; a later terminal drain and an attach kick both refuse it
               (#'state/drain-next-queued! sid)
               (state/drain-idle! sid)
               (expect (= [] @launched))
               (expect (= "queued" (get (state/get-turn sid "old") "status")))
               ;; the user asking for it explicitly is the one override: it
               ;; lifts the floor and drains the head it deliberately resumed
               (state/resume-queue! sid {:auto? false})
               (expect (nil? (get-in @registry [sid :cancel-floor])))
               (expect (= [[sid "old"]] @launched))))
           (finally (swap! registry dissoc sid)))))
  (it "a stopped straggler is skipped, not parked at the head blocking the queue"
      ;; Cross-validation: the gate lives at SELECTION. A pre-cancel turn that
      ;; survived the sweep (a submit that raced it) must not wedge the
      ;; post-cancel "stop that, run THIS" message behind it.
      (let
        [sid
         (str "cancel-skip-" (java.util.UUID/randomUUID))

         registry
         @#'state/registry

         launched
         (atom [])]

        (try (swap! registry assoc
               sid
               {:next-seq 0
                :cancel-floor 1000
                :turns
                {"old"
                 {:turn_id "old" :session_id sid :status "queued" :request "before" :queued_at 500}
                 "new"
                 {:turn_id "new" :session_id sid :status "queued" :request "after" :queued_at 2000}}
                :turn-order ["old" "new"]})
             (with-redefs-fn {#'state/launch-turn-worker! (fn [& args]
                                                            (swap! launched conj
                                                              (vec (take 2 args))))}
               (fn []
                 (state/drain-idle! sid)
                 (expect (= [[sid "new"]] @launched))
                 (expect (= "queued" (get (state/get-turn sid "old") "status")))))
             (finally (swap! registry dissoc sid)))))
  (it "a token cancelled with no user-cancel stamp (shutdown) drains nothing"
      (let
        [sid
         (str "cancel-shutdown-" (java.util.UUID/randomUUID))

         registry
         @#'state/registry

         launched
         (atom [])]

        (try (swap! registry assoc
               sid
               {:next-seq 0
                :turns
                {"r0" {:turn_id "r0" :session_id sid :status "running"}
                 "old"
                 {:turn_id "old" :session_id sid :status "queued" :request "before" :queued_at 50}}
                :turn-order ["r0" "old"]})
             (with-redefs-fn {#'state/launch-turn-worker! (fn [& args]
                                                            (swap! launched conj
                                                              (vec (take 2 args))))
                              #'cancellation/cancelled? (constantly true)}
               #(#'state/after-turn-terminal!
                  sid
                  "r0"
                  {:failed? false :transient? false :cancel-token :tok :stalled? false}))
             (expect (= [] @launched))
             (expect (= "queued" (get (state/get-turn sid "old") "status")))
             (finally (swap! registry dissoc sid))))))

(defdescribe
  delta-coalesce-test
  ;; Model text phases stream LIVE but coalesced to SENTENCE granularity: a frame
  ;; is skipped only while still mid-sentence AND within the time cap. A closed
  ;; sentence, the cap, a `:done?` frame, and the first frame of a phase all pass.
  ;; `last-emit` is [phase iteration] -> {:ms emit-epoch :len emitted-text-length}.
  (let [coalesce? @#'state/coalesce-delta?]
    (it "skips a mid-sentence reasoning delta inside the time cap"
        (expect (true? (coalesce? {[:reasoning 0] {:ms 1000 :len 0}}
                                  {:phase :reasoning :thinking "still going"}
                                  1500))))
    (it "passes once a sentence closes, even inside the time cap"
        (expect (false? (coalesce? {[:reasoning 0] {:ms 1000 :len 0}}
                                   {:phase :reasoning :thinking "done here. "}
                                   1500))))
    (it "passes once the time cap elapses, even mid-sentence"
        (expect (false? (coalesce? {[:reasoning 0] {:ms 1000 :len 0}}
                                   {:phase :reasoning :thinking "still going"}
                                   3001))))
    (it "a :done? frame always passes"
        (expect (false? (coalesce? {[:reasoning 0] {:ms 1000 :len 0}}
                                   {:phase :reasoning :thinking "x" :done? true}
                                   1500))))
    (it "the FIRST frame of a phase always passes (no prior emit)"
        (expect (false? (coalesce? {} {:phase :reasoning :thinking "still going"} 1500))))
    (it "only a sentence in the NEW suffix (past :len) flushes"
        ;; the '.' sits BEFORE :len — already emitted — so the fresh tail is
        ;; mid-sentence and coalesces inside the cap.
        (expect (true? (coalesce? {[:reasoning 0] {:ms 1000 :len 5}}
                                  {:phase :reasoning :thinking "done. more"}
                                  1500)))
        (expect (false? (coalesce? {[:reasoning 0] {:ms 1000 :len 5}}
                                   {:phase :reasoning :thinking "done. more!"}
                                   1500))))
    (it "phases track independent clocks"
        (expect (false? (coalesce? {[:reasoning 0] {:ms 1000 :len 0}}
                                   {:phase :content :content "fresh"}
                                   1050))))
    (it "tool phases always pass"
        (expect (false?
                  (coalesce? {[:reasoning 0] {:ms 1000 :len 0}} {:phase :form-result} 1050)))))
  (it "coalesces native-call code without classifying it as content"
      (let
        [coalesce?
         @#'state/coalesce-delta?

         prior
         {[:tool-preview 1] {:ms 1000 :len 3}}]

        (expect (true? (coalesce? prior {:phase :tool-preview :iteration 1 :code "prin"} 1200)))
        (expect (false? (coalesce? prior {:phase :tool-preview :iteration 1 :code "print\n"} 1200)))
        (expect (false? (coalesce? prior
                                   {:phase :tool-preview :iteration 1 :code "print" :done? true}
                                   1200))))))

(defdescribe volatile-queue-reconciliation-test
             (it "marks orphaned running turns interrupted without reconstructing messages"
                 (let [sweeps (atom 0)]
                   (with-redefs
                     [lp/db-sweep-orphaned-running-turns! (fn []
                                                            (swap! sweeps inc)
                                                            :swept)]
                     (expect (= :swept (state/reconcile-orphaned-turns!)))
                     (expect (= 1 @sweeps))))))

(defdescribe
  turn-stall-watchdog-test
  "A turn wedged with NO chunk activity past the backstop ceiling is
   force-cancelled: the shared cancellation token flips (which closes the
   in-flight stream) and the turn is flagged stalled so the queue can drain.
   This covers a stuck `:provider-call` AND the between-iteration
   `:iteration-final` gap. A legitimately long tool/eval phase is left untouched."
  (let
    [advance
     @#'state/advance-turn-stall-state

     watchdog
     @#'state/start-turn-stall-watchdog!

     registry
     @#'state/registry

     await-cancel
     (fn [token ms]
       (let [deadline (+ (System/currentTimeMillis) (long ms))]
         (loop []

           (cond (cancellation/cancelled? token) true
                 (>= (System/currentTimeMillis) deadline) false
                 :else (do (Thread/sleep 25) (recur))))))]

    (it "does not treat empty streaming callbacks as progress"
        (let
          [initial
           {:phase :reasoning :last-ms 10}

           content-heartbeat
           (advance initial {:phase :content :delta ""} 20)

           reasoning-heartbeat
           (advance content-heartbeat {:phase :reasoning :delta ""} 30)]

          (expect (= {:phase :reasoning :last-ms 10} reasoning-heartbeat))
          (expect (= 40
                     (:last-ms (advance reasoning-heartbeat {:phase :reasoning :delta "more"} 40))))
          (expect (= 50
                     (:last-ms
                       (advance reasoning-heartbeat {:phase :reasoning :delta "" :done? true} 50))))
          (expect (= 60
                     (:last-ms (advance reasoning-heartbeat
                                        {:phase :response-parse :status :started}
                                        60))))))
    (it "force-cancels a turn stuck in :provider-call past the ceiling"
        (let
          [sid
           (str "stall-" (java.util.UUID/randomUUID))

           tid
           "t1"

           token
           (cancellation/cancellation-token)

           stall
           (atom {:phase :provider-call :last-ms (- (System/currentTimeMillis) 60000)})]

          (try (swap! registry assoc sid {:next-seq 0 :current-turn tid})
               ;; await INSIDE with-redefs so the async watchdog thread reads the
               ;; lowered ceiling before with-redefs reverts it (alter-var-root is
               ;; global, not thread-local).
               (with-redefs [state/TURN_STALL_TIMEOUT_MS 150]
                 (watchdog sid tid token stall)
                 (expect (true? (await-cancel token 4000))))
               (expect (true? (:stalled? @stall)))
               (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))
    (it "force-cancels a turn wedged in the between-iteration :iteration-final gap"
        ;; Regression: a turn that finished an iteration but hangs building the
        ;; next provider call (e.g. a blocked auth-header refresh) emits NO more
        ;; chunks and NO terminal event, so its phase stays :iteration-final.
        ;; The old `:provider-call`-only gate never caught it and the session
        ;; queue wedged forever.
        (let
          [sid
           (str "stall-" (java.util.UUID/randomUUID))

           tid
           "t1"

           token
           (cancellation/cancellation-token)

           stall
           (atom {:phase :iteration-final :last-ms (- (System/currentTimeMillis) 60000)})]

          (try (swap! registry assoc sid {:next-seq 0 :current-turn tid})
               (with-redefs [state/TURN_STALL_TIMEOUT_MS 150]
                 (watchdog sid tid token stall)
                 (expect (true? (await-cancel token 4000))))
               (expect (true? (:stalled? @stall)))
               (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))
    (it "leaves a turn alone while it runs a legitimately long tool/eval phase"
        (let
          [sid
           (str "stall-" (java.util.UUID/randomUUID))

           tid
           "t1"

           token
           (cancellation/cancellation-token)

           stall
           (atom {:phase :tool-start :last-ms (- (System/currentTimeMillis) 60000)})]

          (try (swap! registry assoc sid {:next-seq 0 :current-turn tid})
               (with-redefs [state/TURN_STALL_TIMEOUT_MS 150]
                 (watchdog sid tid token stall)
                 (expect (false? (await-cancel token 1200))))
               (expect (nil? (:stalled? @stall)))
               (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))
    (it "leaves a turn alone once it is no longer the current turn"
        (let
          [sid
           (str "stall-" (java.util.UUID/randomUUID))

           token
           (cancellation/cancellation-token)

           stall
           (atom {:phase :provider-call :last-ms (- (System/currentTimeMillis) 60000)})]

          (try
            ;; a DIFFERENT current turn than the one the watchdog guards
            (swap! registry assoc sid {:next-seq 0 :current-turn "other"})
            (with-redefs [state/TURN_STALL_TIMEOUT_MS 150]
              (watchdog sid "t1" token stall)
              (expect (false? (await-cancel token 1200))))
            (expect (nil? (:stalled? @stall)))
            (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))))

(defdescribe gateway-session-order-test
             (it "returns live sessions first and orders each state by recency"
                 (let
                   [order-summaries
                    #'state/order-session-summaries

                    sessions
                    [{"id" "idle-new" "live" false "modified_at" 4000}
                     {"id" "live-old" "live" true "modified_at" 1000}
                     {"id" "idle-old" "live" false "modified_at" 2000}
                     {"id" "live-new" "live" true "last_active_at" (java.util.Date. 3000)}]]

                   (expect (= ["live-new" "live-old" "idle-new" "idle-old"]
                              (mapv #(get % "id") (order-summaries sessions)))))))

(defdescribe
  gateway-prewarm-pool-test
  (it
    "adopts a ready session and requests an asynchronous refill"
    (let
      [pool
       @#'state/prewarm-pool

       prior
       @pool

       sid
       (java.util.UUID/randomUUID)

       refills
       (atom [])]

      (try (reset! pool
             {:ready
              {:api [{:id sid :channel :api :title nil :external-id nil :workspace-id :workspace}]}
              :in-flight {}
              :accepting? true})
           (with-redefs
             [state/ensure-prewarmed!
              #(swap! refills conj %)

              state/claim-prewarmed!
              (fn [session title]
                (assoc session :title title))]

             (let [created (state/create-session! {:channel :api :title "Ready"})]
               (expect (= (str sid) (get created "id")))
               (expect (= "Ready" (get created "title")))
               (expect (= [:api] @refills))
               (expect (empty? (get-in @pool [:ready :api])))))
           (finally (reset! pool prior)))))
  (it "bypasses the pool for a purpose-built workspace"
      (let
        [pool
         @#'state/prewarm-pool

         prior
         @pool

         pooled-id
         (java.util.UUID/randomUUID)

         cold-id
         (java.util.UUID/randomUUID)

         cold-calls
         (atom [])]

        (try (reset! pool {:ready {:api [{:id pooled-id :channel :api}]}
                           :in-flight {}
                           :accepting? true})
             (with-redefs
               [state/create-session-cold!
                (fn [opts]
                  (swap! cold-calls conj opts)
                  {:id cold-id :channel :api :workspace-id (:workspace-id opts)})]
               (let [created (state/create-session! {:channel :api :workspace-id :branch})]
                 (expect (= (str cold-id) (get created "id")))
                 (expect (= [:branch] (mapv :workspace-id @cold-calls)))
                 (expect (= pooled-id (get-in @pool [:ready :api 0 :id])))))
             (finally (reset! pool prior))))))

(it "ships concise structured iteration errors for live retry rendering"
    (let
      [[type _ payload] (#'state/chunk->event
                         {:phase :iteration-error
                          :iteration 1
                          :error {:type :svar.core/http-error
                                  :message "upstream reset"
                                  :status 503
                                  :mini-trace ["private trace"]}})]
      (expect (= "iteration.error" type))
      (expect (= {:type :svar.core/http-error :message "upstream reset" :status 503}
                 (:error-data payload)))
      (expect (not (contains? (:error-data payload) :mini-trace)))))

(defdescribe
  running-turn-start-cursor-test
  "A live-only join to a session with a turn already running (started in the TUI
   or another client) must be able to replay the WHOLE in-flight turn, not only
   the deltas after connect — that is what lets the companion paint the same live
   'Vis is running: …' bubble instead of a bare 'running' row."
  (it "returns the cursor just below the running turn's start seq"
      (let
        [sid
         (str (java.util.UUID/randomUUID))

         registry
         @#'state/registry

         saved
         @registry]

        (try (reset! registry {sid {:next-seq 12
                                    :current-turn "t-run"
                                    :turns {"t-run" {:event_start_seq 7}}}})
             ;; rewinds to replay the in-flight turn from its `turn.started`
             (expect (= 6 (state/running-turn-start-cursor sid)))
             (finally (reset! registry saved)))))
  (it "is nil with no running turn (a live-only join stays live-only)"
      (let
        [sid
         (str (java.util.UUID/randomUUID))

         registry
         @#'state/registry

         saved
         @registry]

        (try (reset! registry {sid {:next-seq 4 :current-turn nil :turns {}}})
             (expect (nil? (state/running-turn-start-cursor sid)))
             (finally (reset! registry saved)))))
  (it "is nil when the running turn has no recorded start seq (foreign, unhydrated)"
      (let
        [sid
         (str (java.util.UUID/randomUUID))

         registry
         @#'state/registry

         saved
         @registry]

        (try (reset! registry {sid {:next-seq 4 :current-turn "t-x" :turns {"t-x" {}}}})
             (expect (nil? (state/running-turn-start-cursor sid)))
             (finally (reset! registry saved))))))

(defdescribe
  queue-failure-pause-test
  "The provider-health queue: a FAILURE with a queued backlog PAUSES instead of
   cascading the next message into a sick provider; an explicit resume drains and
   re-arms the breaker; clean completion advances without pausing; and three
   straight transient failures trip the breaker (no auto-retry, waits for resume)."
  (it
    "pauses on failure, resumes explicitly, drains cleanly, and trips the breaker"
    (let
      [sid
       "queue-pause-test"

       reg
       @#'state/registry

       evs
       (atom [])

       saved
       (into {}
             (for
               [v [#'state/append-event! #'state/drain-next-queued! #'state/schedule-auto-resume!
                   #'state/left-queued-by-cancel? #'cancellation/cancelled?]]
               [v @v]))

       seed!
       (fn []
         (reset! reg {sid {:next-seq 0
                           :current-turn nil
                           :turn-order ["q1" "q2"]
                           :turns {"q1" {:turn_id "q1" :status "queued" :queued_at 1}
                                   "q2" {:turn_id "q2" :status "queued" :queued_at 2}}}})
         (reset! evs []))

       types
       #(mapv first @evs)]

      (try (alter-var-root #'state/append-event!
                           (constantly (fn [_ t p & _]
                                         (swap! evs conj [t p]))))
           (alter-var-root #'state/drain-next-queued!
                           (constantly (fn [_ & _]
                                         (swap! evs conj [:drained]))))
           (alter-var-root #'state/schedule-auto-resume!
                           (constantly (fn [& _]
                                         nil)))
           (alter-var-root #'state/left-queued-by-cancel?
                           (constantly (fn [_ _]
                                         false)))
           (alter-var-root #'cancellation/cancelled?
                           (constantly (fn [_]
                                         false)))
           ;; a transient failure with a backlog HOLDS it — no drain
           (seed!)
           (#'state/after-turn-terminal!
            sid
            "t"
            {:failed? true :transient? true :cancel-token :c :stalled? false})
           (expect (= ["queue.paused"] (types)))
           (expect (some? (state/queue-paused-info sid)))
           ;; explicit resume emits queue.resumed then drains the head, clearing the pause
           (reset! evs [])
           (state/resume-queue! sid {:auto? false})
           (expect (= ["queue.resumed" :drained] (types)))
           (expect (nil? (state/queue-paused-info sid)))
           ;; clean completion clears a stale provider hold before advancing
           (seed!)
           (swap! reg update
             sid
             assoc
             :queue-fails 2
             :queue-paused {:reason "provider_error" :held 2 :fails 2 :gen 1})
           (#'state/after-turn-terminal!
            sid
            "t"
            {:failed? false :transient? false :cancel-token :c :stalled? false})
           (expect (= ["queue.resumed" :drained] (types)))
           (expect (true? (get-in @evs [0 1 :is_auto])))
           (expect (nil? (state/queue-paused-info sid)))
           (expect (nil? (get-in @reg [sid :queue-fails])))
           ;; three straight transient failures trip the breaker; it stays OPEN
           (seed!)
           (dotimes [_ 3]
             (#'state/after-turn-terminal!
              sid
              "t"
              {:failed? true :transient? true :cancel-token :c :stalled? false}))
           (let [third (second (last @evs))]
             (expect (= 3 (:fails third)))
             (expect (true? (:is_breaker_open third)))
             ;; breaker-open no longer waits forever: a half-open probe is
             ;; scheduled so a lasting outage still self-heals when it recovers.
             (expect (some? (:retry_at third))))
           (finally (doseq [[v orig] saved]
                      (alter-var-root v (constantly orig)))
                    (swap! reg dissoc sid))))))

(defdescribe
  queue-retry-renews-cancel-token-test
  "A stall retry must not inherit the token the watchdog cancelled to unwind the
   failed worker. The real queue drain must mint and install a live token."
  (it
    "requeues with no spent token and drains with a fresh cancellable lifetime"
    (let
      [sid
       "queue-retry-token-test"

       tid
       "retry-1"

       reg
       @#'state/registry

       old-token
       (cancellation/cancellation-token)

       queued-after-reset
       (atom nil)

       launched
       (atom nil)]

      (cancellation/cancel! old-token)
      (reset! reg {sid {:next-seq 0
                        :current-turn nil
                        :turn-order [tid]
                        :turns {tid {:turn_id tid
                                     :status "failed"
                                     :request "retry me"
                                     :messages []
                                     :content [{:type "error"}]
                                     :error "provider stream stalled"
                                     :cancel-token old-token
                                     :cancelling_at 1}}}})
      (try (with-redefs-fn {#'state/append-event! (fn [& _]
                                                    nil)
                            #'state/launch-turn-worker! (fn [_ _ _ opts]
                                                          (reset! launched opts))}
             #(do (#'state/re-queue-turn! sid tid)
                  (reset! queued-after-reset (get-in @reg [sid :turns tid]))
                  (#'state/drain-next-queued! sid)))
           (let [retry-token (:cancel-token @launched)]
             (expect (nil? (:cancel-token @queued-after-reset)))
             (expect (nil? (:cancelling_at @queued-after-reset)))
             (expect (some? retry-token))
             (expect (not (identical? old-token retry-token)))
             (expect (false? (cancellation/cancelled? retry-token)))
             (expect (identical? retry-token (get-in @reg [sid :turns tid :cancel-token])))
             (expect (= "running" (get-in @reg [sid :turns tid :status]))))
           (finally (swap! reg dissoc sid))))))

(defdescribe gateway-resource-bounds-test
             (it "retains only the configured replay tail"
                 (with-redefs-fn {#'state/EVENT_RING_MAX 3}
                   (fn []
                     (let
                       [trim-ring
                        (deref #'state/trim-ring)

                        ring
                        (trim-ring [1 2 3 4 5])]

                       (expect (= [3 4 5] ring))
                       (expect (instance? clojure.lang.PersistentQueue ring))))))
             (it "waits for a global turn permit and lets cancellation win without execution"
                 (let
                   [semaphore
                    (java.util.concurrent.Semaphore. 1 true)

                    token
                    (cancellation/cancellation-token)

                    acquire!
                    (deref #'state/acquire-turn-permit!)]

                   (.acquire semaphore)
                   (try (with-redefs-fn {#'state/turn-permits semaphore}
                          (fn []
                            (let [result (future (acquire! token))]
                              (Thread/sleep 150)
                              (expect (not (realized? result)))
                              (cancellation/cancel! token)
                              (expect (= false (deref result 1000 ::timeout))))))
                        (finally (.release semaphore)))))
             (it "keeps one unused prewarm context per channel"
                 (let
                   [pool
                    @#'state/prewarm-pool

                    prior
                    @pool

                    reserve!
                    (deref #'state/reserve-prewarm-slot!)]

                   (try (reset! pool {:ready {} :in-flight {} :accepting? true})
                        (expect (true? (reserve! :api)))
                        (expect (false? (reserve! :api)))
                        (finally (reset! pool prior)))))
             (it "exports concurrency, replay, heap, GC, thread, and env-cache gauges"
                 (let [snapshot (state/metrics-snapshot)]
                   (doseq
                     [k [:turns-executing :turns-waiting :turn-concurrency-limit
                         :replay-events-retained :jvm-heap-used-bytes :process-rss-bytes
                         :jvm-gc-count-total :jvm-thread-count :env-cache-size]]
                     (expect (contains? snapshot k))))))

(defdescribe
  queued-turn-correlation-id-test
  "ONE identity for a queued turn. The submitter's `idempotency_key` rides on the
   turn record AND on `turn.queued`, so a channel binds its optimistic \"Queued\"
   row to the gateway record by ID instead of guessing by request text (two
   identical prompts are indistinguishable by text)."
  (it
    "echoes the submitter's correlation id on the queued record and its event"
    (let
      [registry
       @#'state/registry

       sid
       (str "idem-" (java.util.UUID/randomUUID))

       events
       (atom [])]

      (try (swap! registry assoc sid {:next-seq 0 :current-turn "running-1"})
           (with-redefs-fn {#'state/append-event! (fn [_sid type payload & _]
                                                    (swap! events conj [type payload])
                                                    nil)
                            #'lp/by-id (fn [_]
                                         {:id sid})
                            #'state/session-model (fn [_]
                                                    nil)}
             (fn []
               (let
                 [res
                  (state/submit-turn! sid {:request "hello" :idempotency-key "cid-1"})

                  queued
                  (->> @events
                       (filter (comp #{"turn.queued"} first))
                       first
                       second)]

                 (expect (= "queued" (get-in res [:turn "status"])))
                 (expect (= "cid-1" (get-in res [:turn "idempotency_key"])))
                 (expect (= "cid-1" (:idempotency_key queued)))
                 (expect (= (get-in res [:turn "turn_id"]) (:turn_id queued))))))
           (finally (swap! registry dissoc sid)))))
  (it
    "echoes the submitter's correlation id on turn.started"
    (let
      [registry
       @#'state/registry

       sid
       (str "started-idem-" (java.util.UUID/randomUUID))

       tid
       "turn-1"

       events
       (atom [])]

      (try (swap! registry assoc
             sid
             {:next-seq 0
              :turns {tid {:turn_id tid
                           :session_id sid
                           :status "running"
                           :started_at 123
                           :idempotency_key "cid-1"}}})
           (with-redefs-fn {#'state/append-event! (fn [_sid type payload & _]
                                                    (swap! events conj [type payload])
                                                    nil)
                            #'cancellation/worker-future (fn [& _]
                                                           nil)
                            #'cancellation/cancellation-set-future! (fn [& _]
                                                                      nil)}
             #(#'state/launch-turn-worker! sid tid "hello" {:cancel-token :token}))
           (expect (= "cid-1"
                      (->> @events
                           (filter (comp #{"turn.started"} first))
                           first
                           second
                           :idempotency_key)))
           (finally (swap! registry dissoc sid))))))

(defdescribe
  queued-turn-attachment-preview-test
  "A queued message authored by dropping a screenshot must render as its image,
   not as the raw `/var/folders/…/clipboard-….png` the OS pasted. The gateway
   resolves that ONCE at submit time so every channel (TUI strip, companion
   tray) paints the same row instead of each re-deriving it."
  (it
    "puts byte-free image chips and path-free prose on the record and its event"
    (let
      [registry
       @#'state/registry

       sid
       (str "att-" (java.util.UUID/randomUUID))

       dir
       (.toFile (java.nio.file.Files/createTempDirectory
                  "vis-queued-attachment"
                  (make-array java.nio.file.attribute.FileAttribute 0)))

       png
       (let [f (io/file dir "clipboard-shot.png")]
         (io/copy
           (.decode
             (java.util.Base64/getDecoder)
             "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==")
           f)
         f)

       request
       (str (.getAbsolutePath png) "\nLOOK AT THIS")

       events
       (atom [])]

      (try (swap! registry assoc sid {:next-seq 0 :current-turn "running-1"})
           (with-redefs-fn {#'state/append-event! (fn [_sid type payload & _]
                                                    (swap! events conj [type payload])
                                                    nil)
                            #'lp/by-id (fn [_]
                                         {:id sid})
                            #'state/session-model (fn [_]
                                                    nil)}
             (fn []
               (let
                 [res
                  (state/submit-turn! sid {:request request :display-request "LOOK AT THIS"})

                  queued
                  (->> @events
                       (filter (comp #{"turn.queued"} first))
                       first
                       second)

                  chips
                  (get-in res [:turn "attachment_previews"])]

                 (expect (= "queued" (get-in res [:turn "status"])))
                 ;; The raw request survives untouched — pulling the row back
                 ;; into a composer must re-attach the same file.
                 (expect (= request (get-in res [:turn "request"])))
                 ;; One chip, named, sized, and with NO pixel bytes on it.
                 (expect (= 1 (count chips)))
                 (expect (= "clipboard-shot.png" (get (first chips) "filename")))
                 (expect (= "image/png" (get (first chips) "media_type")))
                 (expect (string? (get (first chips) "size_label")))
                 (expect (not (contains? (first chips) "base64")))
                 ;; Prose the channel paints: chipped, path-free.
                 (expect (= "clipboard-shot.png LOOK AT THIS"
                            (get-in res [:turn "request_preview"])))
                 ;; The submitter's own pre-expansion text is no longer dropped.
                 (expect (= "LOOK AT THIS" (get-in res [:turn "display_request"])))
                 ;; …and the broadcast carries the same row, so a SIBLING channel
                 ;; that never saw the submission paints it identically.
                 (expect (= "clipboard-shot.png LOOK AT THIS" (:request_preview queued)))
                 (expect (= 1 (count (:attachment_previews queued)))))))
           (finally (swap! registry dissoc sid) (.delete png) (.delete dir))))))

(defdescribe
  concurrent-hydrate-test
  "`subscribe!` checks `:current-turn` and THEN hydrates. Two SSE clients
   attaching at once both read it unset, so both mirror the sibling's in-flight
   turn — every event ingested twice, re-sequenced, with no dedup downstream."
  (it
    "hydrates a sibling's in-flight turn exactly ONCE across concurrent subscribers"
    (let
      [tmp
       (java.nio.file.Files/createTempDirectory "hydrate-race"
                                                (make-array java.nio.file.attribute.FileAttribute
                                                            0))

       sid
       (str "hydrate-race-" (java.util.UUID/randomUUID))

       reg
       @#'state/registry

       subs-n
       4]

      (with-redefs
        [bus/events-dir (fn []
                          tmp)]
        (try (spit (#'bus/session-file sid)
                   (str/join (map #(str (wire/json-str %) "\n")
                                  [{:_producer (str (java.util.UUID/randomUUID))
                                    :_pid (var-get #'bus/producer-pid) ; a LIVE producer
                                    :_store true
                                    :schema 1
                                    :seq 5
                                    :type "turn.started"
                                    :turn_id "T-race"
                                    :session_id sid
                                    :request "hi"}])))
             (let
               [barrier (java.util.concurrent.CyclicBarrier. subs-n)
                attach (mapv (fn [i]
                               (future (.await barrier)
                                       (state/subscribe! sid
                                                         (str "sub-" i)
                                                         (fn [_])
                                                         0)))
                             (range subs-n))]

               (run! deref attach))
             (expect
               (= 1 (count (filter #(= "turn.started" (get % "type")) (:events (get @reg sid))))))
             (expect (= "T-race" (:current-turn (get @reg sid))))
             (finally (swap! reg dissoc sid) (.delete (#'bus/session-file sid))))))))

(defdescribe
  transcript-byte-budget-test
  "Windowed transcript pages stay bounded even when individual turns are large."
  (it "caps a window by encoded size and advances from the returned offset"
      (let
        [sid
         (java.util.UUID/randomUUID)

         rows
         (mapv (fn [n]
                 {:id (str "turn-" n) :position n})
               (range 5))]

        (with-redefs-fn
          {#'lp/db-info (constantly ::db)
           #'persistance/db-list-session-turns (fn [_ _]
                                                 rows)
           #'persistance/db-list-turns-attachments (fn [_ ids]
                                                     (zipmap ids (repeat [])))
           (ns-resolve 'com.blockether.vis.internal.gateway.state 'TRANSCRIPT_PAGE_MAX_BYTES) 180
           #'state/transcript-turn (fn [_db _att row]
                                     {:turn_id (:id row) :payload (apply str (repeat 100 "x"))})}
          (fn []
            (let
              [newest
               (state/transcript-page sid {:limit 5})

               earlier
               (state/transcript-page sid {:offset 0 :limit (:offset newest)})]

              (expect (= ["turn-4"] (mapv #(get % "turn_id") (:turns newest))))
              (expect (= 4 (:offset newest)))
              (expect (:has-more newest))
              (expect (= ["turn-3"] (mapv #(get % "turn_id") (:turns earlier))))
              (expect (= 3 (:offset earlier)))
              (expect (:has-more earlier))))))))
