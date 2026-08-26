(ns com.blockether.vis.internal.gateway.state-test
  "Wire-event projection. Form errors ship LEAN, single-surface:
   `block.output` carries human text (message + line/col + hint), never the
   pr-str'd op-error map (which nests host trace/data chains), and is omitted
   entirely when an errored op in the form's sink slice already renders the
   same failure as an op card — the web thread painted that failure twice."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.cancellation :as cancellation]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.gateway.bus :as bus]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis.internal.session-model :as smodel]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private tool-error
  {:message "rg spec has unknown keys: spec."
   :data {:phase :python/host :type :vis/tool-failure :symbol :rg}})

(defdescribe session-model-audit-test
             (it "records changed picker preferences without making audit failure part of selection"
                 (let [recorded
                       (atom nil)

                       before
                       {:provider "anthropic" :model "claude-5"}

                       after
                       {:provider "openai" :model "gpt-5"}]

                   (with-redefs [lp/db-info
                                 (constantly :db)

                                 smodel/model-of
                                 (constantly before)

                                 smodel/set-model!
                                 (constantly after)

                                 smodel/record-switch!
                                 (fn [& args]
                                   (reset! recorded args))

                                 state/append-event!
                                 (fn [& _])]

                     (expect (= after (state/set-session-model! "audit-session" "openai" "gpt-5")))
                     (expect (= [:db "audit-session" before after :gateway] @recorded))))))

(defdescribe
  session-model-broadcast-test
  ;; The pin is shared, but every attached surface renders its OWN copy: the TUI
  ;; footer chip (`chat/gateway-event->chunk` -> `:sync-session-model`) and the
  ;; companion's header chip (`SessionScreen` -> `client.noteSessionModel`) both
  ;; follow THIS frame. Without it, switching the model in one surface leaves
  ;; every other surface naming the model it no longer runs on.
  ;;
  ;; Regression, issue #154: the frame was appended by THIS facade, so a pick moved
  ;; by a writer that never calls it — the engine repointing a session off a
  ;; credential whose 401 it just survived — changed the DB while every chip kept
  ;; naming the dead provider. The broadcast hangs off the session-model store now.
  (let [broadcasts
        (fn [pick]
          (let [appended (atom [])]
            (with-redefs [state/append-event! (fn [& args]
                                                (swap! appended conj (vec args)))]
              (#'state/broadcast-model-event! "sess-1" pick)
              @appended)))

        payload
        (fn [pick]
          (nth (first (broadcasts pick)) 2))]

    (it "broadcasts the new pin live-only, so a cursor replay cannot re-apply it"
        (expect (= [["sess-1" "session.model_updated" {:provider "openai" :model "gpt-5"}
                     {:store? false}]]
                   (broadcasts {:provider "openai" :model "gpt-5"}))))
    (it "a CLEARED override broadcasts too, as a blank pair rather than silence"
        ;; Blank fields are how every client learns to fall back to the router
        ;; default; dropping the event would freeze the last pick on their chips.
        (expect (= [["sess-1" "session.model_updated" {:provider nil :model nil} {:store? false}]]
                   (broadcasts {:provider nil :model ""}))))
    (it "a keyword provider still crosses the wire as its bare name"
        (expect (= {:provider "anthropic" :model "claude-opus-5"}
                   (payload {:provider :anthropic :model "claude-opus-5"}))))
    (it "names the REASON when something other than the human moved the pick"
        ;; The surfaces cannot explain a chip that changed by itself unless the
        ;; frame says why it changed.
        (expect (= {:provider "openai" :model "gpt-5.4" :reason "authentication-fallback"}
                   (payload
                     {:provider "openai" :model "gpt-5.4" :reason :authentication-fallback}))))
    (it "carries no reason for a pick the human made"
        (expect (= [:model :provider] (sort (keys (payload {:provider "openai" :model "gpt-5"}))))))
    (it "is registered on the STORE, so every writer broadcasts through one path"
        (expect (contains? @@#'smodel/model-listeners #'state/broadcast-model-event!)))
    (it "still persists the pick and records the switch"
        (let [recorded
              (atom nil)

              after
              {:provider "openai" :model "gpt-5"}]

          (with-redefs [lp/db-info
                        (constantly :db)

                        smodel/model-of
                        (constantly nil)

                        smodel/set-model!
                        (fn [& _]
                          after)

                        smodel/record-switch!
                        (fn [& args]
                          (reset! recorded (vec args)))

                        state/append-event!
                        (fn [& _])]

            (expect (= after (state/set-session-model! "sess-1" "openai" "gpt-5")))
            (expect (= [:db "sess-1" nil after :gateway] @recorded)))))))

;; Regression, issue #154: changing the next-message selector cancelled the live turn.
(defdescribe
  active-model-switch-test
  (it "leaves the active turn untouched when a human changes the next-message route"
      (let [sid
            (str "model-switch-active-" (java.util.UUID/randomUUID))

            token
            (cancellation/cancellation-token)

            registry
            (atom {sid {:current-turn "active-turn"
                        :turns {"active-turn"
                                {:turn_id "active-turn" :status "running" :cancel-token token}}}})

            notify!
            (fn [pick]
              (doseq [listener @@#'smodel/model-listeners]
                (listener sid pick)))]

        (with-redefs-fn {#'state/registry registry
                         #'state/append-event! (fn [& _]
                                                 nil)
                         #'state/start-cancel-terminal-backstop! (fn [& _]
                                                                   nil)}
          (fn []
            (notify! {:provider "anthropic" :model "claude-opus-5" :reason nil})
            (expect (nil? (cancellation/cancel-reason token)))
            (expect (= "active-turn" (get-in @registry [sid :current-turn])))
            (expect (= "running" (get-in @registry [sid :turns "active-turn" :status]))))))))
(defdescribe
  soul-model-pin-test
  "The session's model PIN rides the soul, so ONE `GET /v1/sessions` already says
   which model each row runs on. Without it every client that names the model has
   to follow up with `GET /v1/sessions/:sid/model` per session it opens."
  (it "carries the persisted pin as model_pref"
      (with-redefs [lp/by-id
                    (constantly {:id "s1"
                                 :channel :api
                                 :title "t"
                                 :model "root-model"
                                 :model-pref {:provider "anthropic" :model "claude-opus-5"}})

                    lp/db-info
                    (constantly nil)

                    persistance/db-session-turn-stats
                    (constantly nil)]

        (let [row (state/soul "s1")]
          (expect (= {"provider" "anthropic" "model" "claude-opus-5"} (get row "model_pref")))
          ;; The state's ROOT model is a DIFFERENT fact (no provider, not the
          ;; user's pick) and keeps its own key.
          (expect (= "root-model" (get row "model"))))))
  (it "omits model_pref for a session on the router default"
      (with-redefs [lp/by-id
                    (constantly {:id "s2" :channel :api :title "t"})

                    lp/db-info
                    (constantly nil)

                    persistance/db-session-turn-stats
                    (constantly nil)]

        (expect (not (contains? (state/soul "s2") "model_pref")))))
  (it "an UNFLUSHED pick beats the persisted row during its debounce window"
      (with-redefs [lp/by-id
                    (constantly {:id "s3"
                                 :channel :api
                                 :title "t"
                                 :model-pref {:provider "anthropic" :model "old"}})

                    lp/db-info
                    (constantly nil)

                    persistance/db-session-turn-stats
                    (constantly nil)

                    smodel/pending-pref
                    (constantly [true {:provider "zai" :model "glm-5.2"}])]

        (expect (= {"provider" "zai" "model" "glm-5.2"} (get (state/soul "s3") "model_pref")))))
  (it "a pending CLEAR drops the pin instead of repainting the old one"
      (with-redefs [lp/by-id
                    (constantly {:id "s4"
                                 :channel :api
                                 :title "t"
                                 :model-pref {:provider "anthropic" :model "old"}})

                    lp/db-info
                    (constantly nil)

                    persistance/db-session-turn-stats
                    (constantly nil)

                    smodel/pending-pref
                    (constantly [true nil])]

        (expect (not (contains? (state/soul "s4") "model_pref"))))))

(defdescribe
  soul-awaiting-input-test
  "A run PARKED on a human is the one state a session list cannot derive: the
   turn is still live, nothing is streaming, and only the operator can move it.
   The soul carries the fact and the navigator order acts on it, so the phone,
   the TUI picker and the gateway's own listing all point at the same session."
  (it "carries the machine-wide waiting fact as is_awaiting_input"
      (with-redefs [lp/by-id
                    (fn [sid]
                      {:id sid :channel :api :title "t"})

                    lp/db-info
                    (constantly nil)

                    persistance/db-session-turn-stats
                    (constantly nil)

                    bus/waiting-requests
                    (constantly {"s-parked" [{"id" "req-1" "since" 1}]})]

        (expect (true? (get (state/soul "s-parked") "is_awaiting_input")))
        (expect (false? (get (state/soul "s-busy") "is_awaiting_input")))))
  ;; Regression, user report (paraphrased: "opening a session makes it the freshest
  ;; thing and it jumps to the top, and the list keeps moving while I read it"): a run
  ;; parked on a human, and a turn in flight, used to LEAD the navigator key, so the
  ;; lifecycle of a turn MOVED rows under the reader - and, because the key is applied
  ;; before the page is cut, pushed another session out of the window entirely.
  (it
    "does not lift a parked session: demand is a FIELD, never a place in the order"
    (let [rows
          [{"id" "live-now" "live" true "modified_at" "2026-01-02T00:00:00Z"}
           {"id" "parked" "live" true "is_awaiting_input" true "modified_at" "2026-01-01T00:00:00Z"}
           {"id" "idle" "modified_at" "2026-01-03T00:00:00Z"}]]
      (expect (= ["idle" "live-now" "parked"]
                 (mapv #(get % "id") (#'state/order-session-summaries rows)))))))
(defdescribe
  thinking-newline-normalization-test
  "Gateway-owned thinking normalization keeps live SSE, poll/replay, and session
   consumers in sync. A client may still render defensively, but it must
   not be the first place where blank-line runs disappear."
  (it "streams reasoning deltas with normalized thinking over the gateway"
      (let [[type store? payload] (#'state/chunk->event
                                   {:phase :reasoning
                                    :iteration 1
                                    :thinking " first  \n\n\t\nsecond\r\n\r\nthird  "
                                    :stream-block-id "t1:reasoning:1"
                                    :stream-delta "first\nsecond\nthird"})]
        (expect (= "content.block.delta" type))
        (expect store?)
        (expect (= "first\nsecond\nthird" (:text payload)))))
  (it "normalizes iteration-boundary thinking for pinned session history"
      (let [[type store? payload]
            (#'state/chunk->event
             {:phase :iteration-final :done? true :thinking " alpha.\n\n\n beta.  \n\t\n gamma. "})]
        (expect (= "iteration.completed" type))
        (expect store?)
        (expect (= "alpha.\n beta.\n gamma." (:thinking payload)))))
  (it "normalizes persisted transcript thinking the same way as live events"
      (with-redefs [persistance/db-list-session-turns
                    (fn [_ sid]
                      [{:id sid :status :success}])

                    persistance/db-list-session-turn-iterations
                    (fn [_ _]
                      [{:thinking " alpha.\n\n beta.  \n"}])]

        (expect (= "alpha.\n beta."
                   (-> (state/transcript :session-1)
                       first
                       (get "iterations")
                       first
                       (get "thinking")))))))

(defdescribe transcript-bubble-footer-test
             (it "ships the exact shared TUI footer and routing note to remote channels"
                 (with-redefs [persistance/db-list-session-turns
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
                     (expect (= "↳ from anthropic/claude-opus — 429, retried 1×, prompt cache lost"
                                (get turn "meta_fallback_note")))))))

(defdescribe transcript-in-flight-footer-test
             ;; The engine persists the row at SUBMIT and stamps routing from the
             ;; last completed iteration, so a turn that is still working already
             ;; has `:llm-actual`. Shipping the footer then painted a
             ;; "provider/model" meta line — with no tokens, cost or duration —
             ;; under a turn the user is still watching stream.
             (it "withholds the footer while the turn is still running"
                 (with-redefs [persistance/db-list-session-turns
                               (fn [_ sid]
                                 [{:id sid :status :running}])

                               persistance/db-list-session-turn-iterations
                               (fn [_ _]
                                 [{:llm-actual {:provider :anthropic :model "claude/opus-5"}}])]

                   (let [turn (first (state/transcript :session-1))]
                     (expect (nil? (get turn "meta_summary")))
                     (expect (nil? (get turn "meta_fallback_note"))))))
             (it "ships the footer for a settled turn with the same routing"
                 (with-redefs [persistance/db-list-session-turns
                               (fn [_ sid]
                                 [{:id sid :status :success :duration-ms 2300}])

                               persistance/db-list-session-turn-iterations
                               (fn [_ _]
                                 [{:llm-actual {:provider :anthropic :model "claude/opus-5"}}])]

                   (let [turn (first (state/transcript :session-1))]
                     (expect (some? (get turn "meta_summary")))))))

(defdescribe
  form-result-error-wire-test
  ;; Op cards are gone, so there is no op-dedup: a form error ALWAYS surfaces
  ;; on the wire. What remains to assert is the lean text shape.
  (it "ships a lean text error (message + line/col + hint), never the pr-str'd map"
      (let [[_ _ payload] (#'state/chunk->event
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
      (let [[type _ payload] (#'state/chunk->event
                              {:phase :form-result :position 0 :code "rg(...)" :error tool-error})]
        (expect (= "block.output" type))
        (expect (= "rg spec has unknown keys: spec." (:error payload))))))

(defdescribe
  activity-wire-event-test
  "Coarse live-progress phases (provider wait, response parse, nested shell/tool
   call) ship as an EPHEMERAL `activity` event (store? false) so a long call
   never leaves the bubble frozen; nothing persists into the durable trace."
  (it "a nested tool-start ships an ephemeral activity event with its precise label"
      (let [[type store? payload]
            (#'state/chunk->event
             {:phase :tool-start :iteration 2 :tool-event {:op :shell :label "clojure -M:test"}})]
        (expect (= "activity" type))
        (expect (false? store?))
        (expect (= {:activity "tool" :iteration 2 :op "shell" :label "clojure -M:test"} payload))))
  (it "provider-call and shell-run project to ephemeral activity events"
      (expect (= ["activity" false {:activity "provider-call" :iteration 1}]
                 (#'state/chunk->event {:phase :provider-call :iteration 1})))
      (expect (= ["activity" false {:activity "shell-run" :iteration 1 :cmd "clojure -M:test"}]
                 (#'state/chunk->event {:phase :shell-run :iteration 1 :cmd "clojure -M:test"}))))
  ;; Regression, issue #120: the activity ticker never said WHY a provider request
  ;; was being made, so an attached client could not tell a tool-result
  ;; continuation from the human's own submit.
  (it "a provider call ships the continuation reason that opened it"
      (expect (= ["activity" false {:activity "provider-call" :iteration 3 :reason "tool-result"}]
                 (#'state/chunk->event {:phase :provider-call :iteration 3 :reason :tool-result}))))
  (it "response-parse :done does NOT emit an activity event (the parse finished)"
      (let [[type] (#'state/chunk->event {:phase :response-parse :iteration 1 :status :done})]
        (expect (not= "activity" type)))))

(defdescribe provider-retry-wire-event-test
             (it "ships structured retry metadata instead of an opaque detail string"
                 (let [[type store? payload] (#'state/chunk->event
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

(defdescribe
  form-event-iteration-wire-test
  ;; THE "live shows reasoning but no code" bug: every streaming chunk carries
  ;; its iteration POSITION under `:iteration`, and that MUST ride the wire
  ;; event — `make-progress-tracker` DROPS any chunk with no iteration, which is
  ;; how `block.started` / `block.output` once lost their forms and the live
  ;; bubble showed reasoning but no code.
  (it "block.started carries :iteration on the wire"
      (let [[type _ payload] (#'state/chunk->event
                              {:phase :form-start :iteration 1 :position 0 :code "import hashlib"})]
        (expect (= "block.started" type))
        (expect (= 1 (:iteration payload)))))
  (it "block.output carries :iteration on the wire"
      (let [[type _ payload]
            (#'state/chunk->event
             {:phase :form-result :iteration 3 :position 0 :code "print(42)" :stdout "42"})]
        (expect (= "block.output" type))
        (expect (= 3 (:iteration payload)))))
  (it "reasoning streams as a replayable typed block delta"
      (let [[type store? payload] (#'state/chunk->event
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
      (let [[type _ payload] (#'state/chunk->event
                              {:phase :iteration-final
                               :iteration 5
                               :done true
                               :thinking "t"
                               :assistant-prose " full prose "})]
        (expect (= "iteration.completed" type))
        (expect (= 5 (:iteration payload)))
        (expect (= "full prose" (:assistant-prose payload)))))
  (it "the streaming code preview is a distinct replayable block event"
      (let [[type store? payload] (#'state/chunk->event
                                   {:phase :tool-preview
                                    :iteration 1
                                    :position 0
                                    :code "print(4"
                                    :svar/tool-call-id "call_1"})]
        (expect (= "block.preview" type))
        (expect store?)
        (expect (= 1 (:iteration payload)))
        (expect (= 0 (:block_id payload)))
        (expect (= "print(4" (:code payload)))
        ;; The preview names no tool: there is exactly one, and a
        ;; card's identity is the printed result's own `op`.
        (expect (nil? (:tool_name payload)))
        (expect (= "call_1" (:tool_call_id payload))))))

(defdescribe
  iteration-attachment-descriptor-wire-test
  ;; Live `iteration.completed` carries metadata-ONLY attachment descriptors
  ;; (NEVER base64) so a native client (iOS/RN) learns an image was produced and
  ;; lazy-fetches it from the byte endpoint. `:index` is the position in the SAME
  ;; ordered list the byte endpoint serves, so index N always names one artifact.
  (it "omits :attachments when the iteration produced none"
      (let [[type _ payload] (#'state/chunk->event
                              {:phase :iteration-final :iteration 2 :done true :thinking "t"})]
        (expect (= "iteration.completed" type))
        (expect (not (contains? payload :attachments)))))
  (it "omits :attachments when there is no iteration-id to address them"
      (let [[_ _ payload] (#'state/chunk->event
                           {:phase :iteration-final :iteration 2 :done true :attachment-count 3})]
        (expect (not (contains? payload :attachments)))))
  ;; User report: an assistant's attachment link had no stable id in the descriptor,
  ;; so tapping its preview link could not identify the artifact to open.
  (it "projects lean snake-case descriptors and NEVER leaks base64"
      (with-redefs [state/iteration-attachments
                    (fn [_iid]
                      [{:id "00000000-0000-0000-0000-0000000000f1"
                        :tool-call-id "call_A"
                        :kind "image"
                        :media-type "image/png"
                        :filename "fig.png"
                        :size 1234
                        :base64 "SECRET"} {:tool-call-id nil :media-type "image/svg+xml" :size 0}])]
        (let [[_ _ payload] (#'state/chunk->event
                             {:phase :iteration-final
                              :iteration 4
                              :done false
                              :iteration-id "00000000-0000-0000-0000-0000000000ab"
                              :attachment-count 2})
              atts (:attachments payload)]

          (expect (= 2 (count atts)))
          (expect (= [0 1] (mapv :index atts)))
          (expect (= "00000000-0000-0000-0000-0000000000f1" (:attachment_id (first atts))))
          (expect (not (contains? (second atts) :attachment_id)))
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
    (let [a
          (java.util.UUID/randomUUID)

          b
          (java.util.UUID/randomUUID)

          registry
          @#'state/registry

          saved
          @registry]

      (try
        ;; b carries no :subscribers — exactly a client on the /poll fallback
        ;; keyed the way `sid-key` spells a session: one entry per session, whatever
        ;; spelling the caller holds
        (reset! registry {(str a) {:next-seq 0} (str b) {:next-seq 0}})
        (#'state/broadcast-title-event! a "Tidal Forces")
        (let [a-events
              (state/events-since a 0)

              b-events
              (state/events-since b 0)]

          ;; the titled session keeps its own stored event
          (expect (= 1 (count a-events)))
          (expect (= "session.title_updated" (get (first a-events) "type")))
          ;; the sibling (poll-only) session ALSO has it stored → /poll sees it
          (expect (= 1 (count b-events)))
          (expect (= "session.title_updated" (get (first b-events) "type")))
          ;; `session_id` ALWAYS names the ring the event was appended to — the
          ;; foreign copy names its subject separately. Re-stamping `session_id`
          ;; here handed a's high seq to b's per-session cursor on both ends of
          ;; the multiplexed SSE stream and silently killed b's live stream.
          (expect (= (str b) (get (first b-events) "session_id")))
          (expect (= (str a) (get (first b-events) "titled_session_id")))
          (expect (nil? (get (first a-events) "titled_session_id"))))
        (finally (reset! registry saved))))))

(defdescribe canonical-answer-content-test
             (it "normalizes Markdown to one prose block"
                 (let [blocks (#'state/answer-content {:answer "## hello"})]
                   (expect (= 1 (count blocks)))
                   (expect (= "prose" (get-in blocks [0 "type"])))
                   (expect (= "## hello" (get-in blocks [0 "markdown"])))))
             (it "passes typed error content without creating a second answer shape"
                 (let [blocks [{"id" "e1"
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
    (let [sid
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
           (with-redefs [persistance/db-list-session-turns
                         (fn [_ _]
                           [{:id engine-id
                             :status :success
                             :user-request "hello"
                             :content [{"id" "b1" "type" "prose" "markdown" "hi"}]
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
    (let [sid
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
           (with-redefs [persistance/db-list-session-turns
                         (fn [_ s]
                           (expect (= sid s))
                           [{:id engine-id
                             :status :success
                             :user-request "hello"
                             :content [{"id" "b1" "type" "prose" "markdown" "hi"}]
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
    (let [sid
          (java.util.UUID/randomUUID)

          registry
          @#'state/registry

          saved
          @registry]

      (try (reset! registry {(str sid) {:next-seq 0
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
           (with-redefs [persistance/db-list-session-turns
                         (fn [_ _]
                           (throw (ex-info "queued poll must not hydrate history" {})))]
             (let [turns (state/list-queued-turns sid)]
               (expect (= 1 (count turns)))
               (expect (= "waiting" (get (first turns) "turn_id")))
               (expect (= "queued" (get (first turns) "status")))))
           (finally (reset! registry saved)))))
  (it "is empty for a session with nothing waiting"
      (let [sid
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
  turn-attachments-test
  "The bytes of a user's images must be reachable from the GATEWAY, not only from
   the device that sent them: an app restart (or a second device) mid-turn has no
   other source, because the live rail ships byte-free chips and the persisted
   row does not exist until the turn lands."
  (it "serves an in-flight turn's inline attachments in the canonical wire shape"
      (let [sid
            (str (java.util.UUID/randomUUID))

            registry
            @#'state/registry

            saved
            @registry]

        (try (reset! registry {sid {:next-seq 0
                                    :turn-order ["t1"]
                                    :turns {"t1" {:turn_id "t1"
                                                  :session_id sid
                                                  :status "running"
                                                  :request "look at this"
                                                  :attachments [{:filename "shot.png"
                                                                 :media-type "image/png"
                                                                 :base64 "QUJD"
                                                                 :size 3}]}}}})
             (let [rows (state/turn-attachments sid "t1")]
               (expect (= 1 (count rows)))
               (expect (= "shot.png" (get (first rows) "filename")))
               (expect (= "image/png" (get (first rows) "media_type")))
               (expect (= "QUJD" (get (first rows) "base64"))))
             (finally (reset! registry saved)))))
  (it "falls back to the attachment store for a turn that has already landed"
      (let [sid
            (str (java.util.UUID/randomUUID))

            registry
            @#'state/registry

            saved
            @registry]

        (try (reset! registry {sid {:next-seq 0 :turns {}}})
             (with-redefs [persistance/db-list-turns-attachments
                           (fn [_ ids]
                             {(str (first ids))
                              [{:filename "landed.png" :media-type "image/png" :base64 "REVG"}]})]
               (let [rows (state/turn-attachments sid "t9")]
                 (expect (= 1 (count rows)))
                 (expect (= "landed.png" (get (first rows) "filename")))
                 (expect (= "REVG" (get (first rows) "base64")))))
             (finally (reset! registry saved)))))
  ;; Regression, issue #1: an image sent from the TUI painted only a filename
  ;; chip in the companion's live user bubble - the picture appeared only after
  ;; the turn landed and the session was reloaded. The TUI authors a message that
  ;; merely NAMES the file (a `vis-image` fence around an absolute path), so the
  ;; gateway holds no inline `:attachments`, and the engine's stored bytes are
  ;; keyed by its own session-turn soul id and do not exist until the turn lands:
  ;; both existing arms missed, and this endpoint answered `[]`.
  (it
    "serves the images a turn's own request TEXT names, before the turn lands"
    (let [sid
          (str (java.util.UUID/randomUUID))

          dir
          (str (java.nio.file.Files/createTempDirectory
                 "vis-turn-attachments"
                 (into-array java.nio.file.attribute.FileAttribute [])))

          png
          (java.io.File. dir "clipboard-2026-08-07-104042.png")

          pixels
          (.decode (java.util.Base64/getDecoder)
                   (str "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR4"
                        "2mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="))

          registry
          @#'state/registry

          saved
          @registry]

      (try (java.nio.file.Files/write (.toPath png)
                                      ^bytes pixels
                                      (into-array java.nio.file.OpenOption []))
           (reset! registry {sid {:next-seq 0
                                  :turn-order ["t5"]
                                  :turns {"t5" {:turn_id "t5"
                                                :session_id sid
                                                :status "running"
                                                :workspace {:root dir}
                                                :request (str "look at this\n\n```vis-image\n"
                                                              (.getAbsolutePath png)
                                                              "\n```")}}}})
           (let [rows (state/turn-attachments sid "t5")]
             (expect (= 1 (count rows)))
             (expect (= "clipboard-2026-08-07-104042.png" (get (first rows) "filename")))
             (expect (= "image/png" (get (first rows) "media_type")))
             (expect (= (.encodeToString (java.util.Base64/getEncoder) pixels)
                        (get (first rows) "base64")))
             (expect (= "user" (get (first rows) "source")))
             (expect (= "both" (get (first rows) "audience"))))
           (finally (reset! registry saved) (.delete png)))))
  (it "is nil for an unknown turn"
      (expect (nil? (state/turn-attachments (str (java.util.UUID/randomUUID)) "nope")))))


(defdescribe
  session-artifacts-test
  "A client's gallery used to be derived from the transcript rows it happened to
   hold. The transcript is read newest-page-first, so a long session listed only
   the artifacts the reader had already scrolled back to. This index answers the
   WHOLE session in one metadata read."
  (it "carries an Activity record's stable identity and form anchor"
      (let [anchor
            {:evaluation-id "eval-1" :iteration 2 :form-index 1}

            rows
            (@#'state/attachment-descriptors
             "iteration-2"
             [{:view-id "activity-view"
               :classification :activity
               :activity-anchor anchor
               :kind "file"
               :media-type "application/vnd.vis.live+ndjson"
               :filename "activity.live.ndjson"}])]

        (expect (= [{"evaluation_id" "eval-1" "iteration" 2 "form_index" 1}]
                   (mapv :activity_anchor rows)))
        (expect (= ["activity-view"] (mapv :view_id rows)))
        (expect (= ["activity"] (mapv :classification rows)))))
  (it
    "indexes every produced artifact with the turn that made it"
    (let [sid
          (java.util.UUID/randomUUID)

          turns
          [{:id "soul-0"} {:id "soul-1"} {:id "soul-2"}]

          att
          (fn [m]
            (merge {:kind "image" :media-type "image/png" :audience "both" :size 10 :position 0}
                   m))]

      (with-redefs-fn {#'lp/db-info (constantly ::db)
                       #'persistance/db-list-session-turns (fn [_ _]
                                                             turns)
                       #'persistance/db-list-session-attachments-meta
                       (fn [_ _]
                         [;; A user's own uploaded image: no iteration, not an artifact the
                          ;; model produced.
                          (att {:filename "upload.png" :turn-soul-id "soul-0" :iteration-id nil})
                          (att {:filename "second.png"
                                :turn-soul-id "soul-0"
                                :iteration-id "i1"
                                :tool-call-id "call_B"})
                          (att {:filename "first.png"
                                :turn-soul-id "soul-0"
                                :iteration-id "i1"
                                :tool-call-id "call_A"})
                          ;; Model-only: absent from the byte endpoint, so absent here.
                          (att {:filename "hidden.png"
                                :audience "model"
                                :turn-soul-id "soul-2"
                                :iteration-id "i9"
                                :tool-call-id "call_C"})
                          (att {:filename "late.png"
                                :turn-soul-id "soul-2"
                                :iteration-id "i9"
                                :tool-call-id "call_D"
                                :version 3})])}
        (fn []
          (let [rows (state/session-artifacts sid)]
            (expect (= ["first.png" "second.png" "late.png"] (mapv :filename rows)))
            ;; The turn ordinal is counted from the START of the session, so a
            ;; client never has to hold the earlier pages to name it.
            (expect (= [1 1 3] (mapv :turn rows)))
            ;; `(tool_call_id, position)` is the byte endpoint's own order, so
            ;; index N names the same artifact everywhere.
            (expect (= [0 1 0] (mapv :index rows)))
            (expect (= ["i1" "i1" "i9"] (mapv :iteration_id rows)))
            (expect (= 3 (:version (last rows)))))))))
  (it "is empty, never an exception, when the read fails"
      (with-redefs-fn {#'lp/db-info (constantly ::db)
                       #'persistance/db-list-session-turns (fn [_ _]
                                                             (throw (ex-info "nope" {})))}
        (fn []
          (expect (= [] (state/session-artifacts (java.util.UUID/randomUUID))))))))

(defdescribe
  transcript-page-test
  "The transcript window exists so a client never pays for history it will not
   paint: a long session is tens of megabytes and nearly all of that cost is
   per-turn hydration. The window must therefore be sliced BEFORE hydration,
   and the cursor must be an index — `:position` is neither unique nor
   monotonic in real sessions, so it cannot page anything."
  (it
    "hydrates ONLY the requested window and reports the full total"
    (let [sid
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
      (let [sid
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
                         (ns-resolve 'com.blockether.vis.internal.gateway.state
                                     'TRANSCRIPT_PAGE_MAX_BYTES)
                         (delay 2000)
                         #'state/transcript-turn (fn [_db _att row]
                                                   (swap! hydrated conj (:id row))
                                                   {:turn_id (:id row)
                                                    :text (apply str (repeat 900 "x"))})}
          (fn []
            (let [page (state/transcript-page sid {:limit 10})]
              ;; ~930 encoded bytes per row against a 2000-byte budget: two rows
              ;; fit and the third busts it. The buster is KEPT — deferring it
              ;; silently swallowed the newest turn carrying a user image, so a
              ;; re-entered session painted no image at all.
              (expect (= ["turn-7" "turn-8" "turn-9"] (mapv #(get % "turn_id") (:turns page))))
              (expect (= 10 (:total page)))
              ;; The dropped rows raise the offset — which is exactly where the
              ;; client's next `load earlier` resumes, so nothing is skipped.
              (expect (= 7 (:offset page)))
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
      (let [sid
            (java.util.UUID/randomUUID)

            rows
            (mapv (fn [n]
                    {:id (str "turn-" n) :position 1})
                  (range 10))]

        (with-redefs-fn {#'lp/db-info (constantly ::db)
                         #'persistance/db-list-session-turns (fn [_ _]
                                                               rows)
                         #'persistance/db-list-turns-attachments (fn [_ ids]
                                                                   (zipmap ids (repeat [])))
                         (ns-resolve 'com.blockether.vis.internal.gateway.state
                                     'TRANSCRIPT_PAGE_MAX_BYTES)
                         (delay 100)
                         #'state/transcript-turn (fn [_db _att row]
                                                   {:turn_id (:id row)
                                                    :text (apply str (repeat 900 "x"))})}
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
              (expect (= 8 (:offset page)))))))))

(defdescribe
  queued-update-payload-test
  "Editing a queued request must also edit the provider message payload;
  otherwise the drained queued turn answers the pre-edit prompt."
  (it "replaces the last user message content"
      (let [messages [{:role "system" :content "rules"} {:role "user" :content "old prompt"}
                      {:role "assistant" :content "old answer"}
                      {:role :user :content "queued old"}]]
        (expect (= [{:role "system" :content "rules"} {:role "user" :content "old prompt"}
                    {:role "assistant" :content "old answer"} {:role :user :content "queued new"}]
                   (#'state/replace-last-user-message-content messages "queued new"))))))

(defdescribe
  persisted-duplicate-of-live-test
  ;; Terminal identity is request + status + timestamps; content blocks belong
  ;; to the durable row and are not duplicated onto terminal events.
  (let [dup?
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
  (let [reg
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
      (let [sid
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
      (let [sid
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
      (let [sid
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

;; Regression, issue #154: queued messages inherited whichever route was selected when they drained.
(defdescribe
  gateway-turn-route-snapshot-test
  "Each submission freezes provider + model together; later picker changes affect only later sends."
  (it "forwards the selected route to an immediately accepted turn"
      (let [registry
            @#'state/registry

            sid
            (str "route-snapshot-accepted-" (java.util.UUID/randomUUID))

            launched
            (atom nil)]

        (try (swap! registry assoc sid {:next-seq 0 :turns {} :turn-order []})
             (with-redefs-fn {#'lp/by-id (fn [_]
                                           {:id sid})
                              #'state/session-model (fn [_]
                                                      {:provider "lmstudio" :model "ornith"})
                              #'state/launch-turn-worker! (fn [& args]
                                                            (reset! launched (vec args)))}
               #(state/submit-turn! sid {:request "hello"}))
             (expect (= "lmstudio" (get-in @launched [3 :provider])))
             (expect (= "ornith" (get-in @launched [3 :model])))
             (expect (= "lmstudio" (get (state/get-turn sid (second @launched)) "provider")))
             (expect (= "ornith" (get (state/get-turn sid (second @launched)) "model")))
             (finally (swap! registry dissoc sid)))))
  (it
    "preserves distinct route snapshots for messages waiting in the same queue"
    (let [registry
          @#'state/registry

          sid
          (str "route-snapshot-queued-" (java.util.UUID/randomUUID))

          pin
          (atom {:provider "openai-codex" :model "gpt-5.6-sol"})

          launched
          (atom [])]

      (try (swap! registry assoc
             sid
             {:next-seq 0
              :current-turn "r0"
              :turns {"r0" {:turn_id "r0" :session_id sid :status "running" :request "run"}}
              :turn-order ["r0"]})
           (with-redefs-fn {#'lp/by-id (fn [_]
                                         {:id sid})
                            #'state/session-model (fn [_]
                                                    @pin)
                            #'state/append-event! (fn [& _]
                                                    nil)
                            #'state/launch-turn-worker! (fn [& args]
                                                          (swap! launched conj (vec args)))}
             (fn []
               (state/submit-turn! sid {:request "first queued"})
               (reset! pin {:provider "anthropic" :model "claude-opus-5"})
               (state/submit-turn! sid {:request "second queued"})
               (reset! pin {:provider "lmstudio" :model "ornith"})
               (let [[q1 q2] (subvec (get-in @registry [sid :turn-order]) 1)]
                 (expect (= ["openai-codex" "gpt-5.6-sol"]
                            (mapv #(get-in @registry [sid :turns q1 %]) [:provider :model])))
                 (expect (= ["anthropic" "claude-opus-5"]
                            (mapv #(get-in @registry [sid :turns q2 %]) [:provider :model])))
                 (swap! registry assoc-in [sid :current-turn] nil)
                 (state/drain-idle! sid)
                 (expect (= ["openai-codex" "gpt-5.6-sol"]
                            (mapv #(get-in (first @launched) [3 %]) [:provider :model])))
                 (swap! registry (fn [entries]
                                   (-> entries
                                       (assoc-in [sid :current-turn] nil)
                                       (assoc-in [sid :turns q1 :status] "completed"))))
                 (state/drain-idle! sid)
                 (expect (= ["anthropic" "claude-opus-5"]
                            (mapv #(get-in (second @launched) [3 %]) [:provider :model]))))))
           (finally (swap! registry dissoc sid))))))

(defdescribe turn-terminal-claim-once-test
             (it "allows exactly one terminal landing for a turn"
                 (let [sid
                       (str "terminal-claim-" (java.util.UUID/randomUUID))

                       tid
                       "one-run"

                       registry
                       @#'state/registry

                       claim!
                       #'state/claim-turn-terminal!

                       release!
                       #'state/release-turn-terminal-claim!

                       token
                       (cancellation/cancellation-token)]

                   (try (swap! registry assoc
                          sid
                          {:current-turn tid
                           :turns {tid {:turn_id tid :status "running" :cancel-token token}}})
                        (expect (true? (claim! sid tid token)))
                        (expect (false? (claim! sid tid token)))
                        (finally (swap! registry dissoc sid) (release! sid tid))))))

(defdescribe
  release-session-busy-test
  ;; Sessions are SHARED. A TUI tab close (or TUI exit) releases the session view,
  ;; and that release used to tear down the daemon runtime unconditionally — which
  ;; killed the turn the companion app was streaming in that same session. Closing
  ;; a view is never a cancel: only an explicit cancel stops work.
  (it
    "leaves running/queued sessions alone and still releases an idle one"
    (let [busy-sid
          (str "release-busy-" (java.util.UUID/randomUUID))

          queued-sid
          (str "release-queued-" (java.util.UUID/randomUUID))

          idle-sid
          (str "release-idle-" (java.util.UUID/randomUUID))

          registry
          @#'state/registry

          calls
          (atom [])]

      (try (swap! registry assoc
             busy-sid
             {:current-turn "t-run" :turns {"t-run" {:turn_id "t-run" :status "running"}}}
             queued-sid
             {:turns {"t-q" {:turn_id "t-q" :status "queued"}}}
             idle-sid
             {:turns {"t-done" {:turn_id "t-done" :status "completed"}}})
           (with-redefs-fn {(requiring-resolve 'com.blockether.vis.internal.resources/stop-all!)
                            (fn [sid]
                              (swap! calls conj [:stop-all sid]))
                            #'lp/close! (fn [sid]
                                          (swap! calls conj [:close sid]))}
             #(run! state/release-session! [busy-sid queued-sid idle-sid]))
           (expect (= [true true false] (mapv state/session-busy? [busy-sid queued-sid idle-sid])))
           (expect (= [[:stop-all idle-sid] [:close idle-sid]] @calls))
           (finally (swap! registry dissoc busy-sid queued-sid idle-sid))))))

(defdescribe
  cancelled-backlog-test
  ;; The queue-storm regression: a user cancel must not merely SKIP the backlog
  ;; it stopped, it must DELETE it. Anything left `queued` is a live wire — any
  ;; later terminal, attach kick or resume can start it minutes and turns later.
  (it
    "a user cancel drops the pre-cancel backlog and keeps post-cancel intent"
    (let [sid
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
      (let [sid
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
    (let [sid
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
               "old"
               {:turn_id "old" :session_id sid :status "queued" :request "before" :queued_at 50}}
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
      (let [sid
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
      (let [sid
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
      (let [coalesce?
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
                   (with-redefs [lp/db-sweep-orphaned-running-turns! (fn []
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
  (let [advance
        @#'state/advance-turn-stall-state

        watchdog
        @#'state/start-turn-stall-watchdog!

        decision
        @#'state/turn-stall-decision

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
        (let [initial
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
        (let [sid
              (str "stall-" (java.util.UUID/randomUUID))

              tid
              "t1"

              token
              (cancellation/cancellation-token)

              stall
              ;; `:produced?` — this turn STREAMED and then went quiet, which is what
              ;; the full stall ceiling is for.
              (atom {:phase :provider-call
                     :started? true
                     :produced? true
                     :last-ms (- (System/currentTimeMillis) 60000)})]

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
        (let [sid
              (str "stall-" (java.util.UUID/randomUUID))

              tid
              "t1"

              token
              (cancellation/cancellation-token)

              stall
              (atom {:phase :iteration-final
                     :started? true
                     :produced? true
                     :last-ms (- (System/currentTimeMillis) 60000)})]

          (try (swap! registry assoc sid {:next-seq 0 :current-turn tid})
               (with-redefs [state/TURN_STALL_TIMEOUT_MS 150]
                 (watchdog sid tid token stall)
                 (expect (true? (await-cancel token 4000))))
               (expect (true? (:stalled? @stall)))
               (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))
    (it "leaves a turn alone while it runs a legitimately long tool/eval phase"
        (let [sid
              (str "stall-" (java.util.UUID/randomUUID))

              tid
              "t1"

              token
              (cancellation/cancellation-token)

              stall
              (atom
                {:phase :tool-start :started? true :last-ms (- (System/currentTimeMillis) 60000)})]

          (try (swap! registry assoc sid {:next-seq 0 :current-turn tid})
               (with-redefs [state/TURN_STALL_TIMEOUT_MS 150]
                 (watchdog sid tid token stall)
                 ;; The watchdog polls every 25ms here, so 400ms is a dozen-plus
                 ;; decisions past the 150ms ceiling — proof, not a longer nap.
                 (expect (false? (await-cancel token 400))))
               (expect (nil? (:stalled? @stall)))
               (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))
    (it "leaves a turn alone once it is no longer the current turn"
        (let [sid
              (str "stall-" (java.util.UUID/randomUUID))

              token
              (cancellation/cancellation-token)

              stall
              (atom {:phase :provider-call
                     :started? true
                     :last-ms (- (System/currentTimeMillis) 60000)})]

          (try
            ;; a DIFFERENT current turn than the one the watchdog guards
            (swap! registry assoc sid {:next-seq 0 :current-turn "other"})
            (with-redefs [state/TURN_STALL_TIMEOUT_MS 150]
              (watchdog sid "t1" token stall)
              (expect (false? (await-cancel token 400))))
            (expect (nil? (:stalled? @stall)))
            (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))
    (it "lands a terminal for a launched turn whose worker body never began"
        ;; Regression: `turn.started` was on the wire and `:current-turn` pinned,
        ;; but nothing ever entered the worker body — so no terminal event could
        ;; ever be emitted and the session wedged on a spinner that could not be
        ;; cancelled or drained. The watchdog now guards from LAUNCH, not from the
        ;; first chunk.
        (let [sid
              (str "stall-" (java.util.UUID/randomUUID))

              tid
              "t1"

              token
              (cancellation/cancellation-token)

              landed
              (atom nil)

              ;; no `:started?`: the worker never stamped proof of life
              stall
              (atom {:phase nil :last-ms (- (System/currentTimeMillis) 60000)})]

          (try (swap! registry assoc sid {:next-seq 0 :current-turn tid})
               (with-redefs [state/TURN_LAUNCH_TIMEOUT_MS
                             150

                             state/CANCEL_TERMINAL_GRACE_MS
                             50

                             state/SILENT_CANCEL_TERMINAL_GRACE_MS
                             50

                             state/fail-orphaned-turn!
                             (fn [_sid _tid _token reason]
                               (reset! landed reason)
                               true)]

                 (watchdog sid tid token stall)
                 (expect (true? (await-cancel token 4000)))
                 (Thread/sleep 400))
               (expect (true? (:stalled? @stall)))
               (expect (str/includes? (str @landed) "turn never started running"))
               (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))
    (it "holds a started turn to the stall ceiling, never the launch ceiling"
        (let [sid
              (str "stall-" (java.util.UUID/randomUUID))

              tid
              "t1"

              token
              (cancellation/cancellation-token)

              stall
              (atom {:phase :awaiting-permit
                     :started? true
                     :last-ms (- (System/currentTimeMillis) 60000)})]

          (try (swap! registry assoc sid {:next-seq 0 :current-turn tid})
               (with-redefs [state/TURN_LAUNCH_TIMEOUT_MS
                             150

                             state/TURN_STALL_TIMEOUT_MS
                             300000]

                 (watchdog sid tid token stall)
                 (expect (false? (await-cancel token 500))))
               (expect (nil? (:stalled? @stall)))
               (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))
    (it "counts the bare :provider-call marker as a lifecycle stamp, not output"
        ;; Regression: the engine emits `{:phase :provider-call}` the INSTANT the
        ;; request goes out, and the stall state counted it as model output. A
        ;; turn the provider never answered therefore looked like a producing one
        ;; and kept both the full cancel grace and the full stall ceiling.
        (let [marker (advance {} {:phase :provider-call :iteration 0 :started-at-ms 1} 100)]
          (expect (= {:phase :provider-call :produced? false :last-ms 100} marker))
          (expect (true? (:produced? (advance marker {:phase :content :delta "hi"} 200))))))
    (it "force-cancels a started turn the provider never answered at all"
        ;; Regression: a turn sat 3m47s with zero iterations, holding the whole
        ;; session queue, because silence from the very first byte was budgeted
        ;; like a stream that died mid-answer (`TURN_STALL_TIMEOUT_MS`, 6min).
        (let [sid
              (str "stall-" (java.util.UUID/randomUUID))

              tid
              "t1"

              token
              (cancellation/cancellation-token)

              stall
              (atom {:phase :provider-call
                     :started? true
                     :last-ms (- (System/currentTimeMillis) 60000)})]

          (try (swap! registry assoc sid {:next-seq 0 :current-turn tid})
               (with-redefs [state/TURN_FIRST_OUTPUT_TIMEOUT_MS
                             150

                             state/TURN_STALL_TIMEOUT_MS
                             300000]

                 (watchdog sid tid token stall)
                 (expect (true? (await-cancel token 4000))))
               (expect (true? (:stalled? @stall)))
               (expect (str/includes? (str (:stall-detail @stall)) "no output at all"))
               (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))
    ;; Regression, issue td-e06f95: the gateway's fixed two-minute first-output
    ;; watchdog cancelled LM Studio before its provider-scoped prefill budget elapsed.
    (it "accepts delayed provider output past the former ceiling without wall-clock sleeps"
        (with-redefs [state/TURN_FIRST_OUTPUT_TIMEOUT_MS
                      150

                      state/TURN_STALL_TIMEOUT_MS
                      300]

          (let [marker
                (advance {}
                         {:phase :provider-call
                          :provider "lmstudio"
                          :first-output-timeout-ms 700
                          :stall-timeout-ms 600}
                         0)

                delayed-output
                (advance (assoc marker :started? true) {:phase :content :delta "ready"} 200)

                completed
                (advance delayed-output {:phase :content :done? true} 201)]

            (expect (false? (:tripped? (decision (assoc marker :started? true) 151))))
            (expect (true? (:produced? delayed-output)))
            (expect (false? (:tripped? (decision completed 599))))
            (expect (true? (:tripped? (decision completed 600)))))))
    (it "honors a provider call's longer bounded first-output ceiling"
        (let [sid
              (str "stall-" (java.util.UUID/randomUUID))

              tid
              "t1"

              token
              (cancellation/cancellation-token)

              marker
              (advance {}
                       {:phase :provider-call
                        :provider "lmstudio"
                        :first-output-timeout-ms 700
                        :stall-timeout-ms 600}
                       (System/currentTimeMillis))

              stall
              (atom (assoc marker :started? true))]

          (try (swap! registry assoc sid {:next-seq 0 :current-turn tid})
               (with-redefs [state/TURN_FIRST_OUTPUT_TIMEOUT_MS
                             150

                             state/TURN_STALL_TIMEOUT_MS
                             300000]

                 (watchdog sid tid token stall)
                 (expect (false? (await-cancel token 350)))
                 (expect (true? (await-cancel token 1200))))
               (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))
    (it "resets per-call output state and provider ceilings at each provider call"
        (let [answered (advance {:produced? true :first-output-timeout-ms 700 :stall-timeout-ms 600}
                                {:phase :provider-call :provider "cloud"}
                                42)]
          (expect (false? (:produced? answered)))
          (expect (not (contains? answered :first-output-timeout-ms)))
          (expect (not (contains? answered :stall-timeout-ms)))))
    (it "holds a turn that already streamed output to the full stall ceiling"
        (let [sid
              (str "stall-" (java.util.UUID/randomUUID))

              tid
              "t1"

              token
              (cancellation/cancellation-token)

              stall
              (atom {:phase :provider-call
                     :started? true
                     :produced? true
                     :last-ms (- (System/currentTimeMillis) 60000)})]

          (try (swap! registry assoc sid {:next-seq 0 :current-turn tid})
               (with-redefs [state/TURN_FIRST_OUTPUT_TIMEOUT_MS
                             150

                             state/TURN_STALL_TIMEOUT_MS
                             300000]

                 (watchdog sid tid token stall)
                 (expect (false? (await-cancel token 400))))
               (expect (nil? (:stalled? @stall)))
               (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))
    (it "never applies the first-output ceiling to a turn queueing for a permit"
        ;; Waiting behind another session's turn for the process-wide execution
        ;; permit is queueing, not a wedged provider.
        (let [sid
              (str "stall-" (java.util.UUID/randomUUID))

              tid
              "t1"

              token
              (cancellation/cancellation-token)

              stall
              (atom {:phase :awaiting-permit
                     :started? true
                     :last-ms (- (System/currentTimeMillis) 60000)})]

          (try (swap! registry assoc sid {:next-seq 0 :current-turn tid})
               (with-redefs [state/TURN_FIRST_OUTPUT_TIMEOUT_MS
                             150

                             state/TURN_STALL_TIMEOUT_MS
                             300000]

                 (watchdog sid tid token stall)
                 (expect (false? (await-cancel token 400))))
               (expect (nil? (:stalled? @stall)))
               (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))
    (it "guards a turn whose record is still running after the pin moved on"
        (let [sid
              (str "stall-" (java.util.UUID/randomUUID))

              token
              (cancellation/cancellation-token)

              stall
              (atom {:phase :provider-call
                     :started? true
                     :produced? true
                     :last-ms (- (System/currentTimeMillis) 60000)})]

          (try (swap! registry assoc
                 sid
                 {:next-seq 0
                  :current-turn "other"
                  :turns {"t1" {:status "running" :cancel-token token}}})
               (with-redefs [state/TURN_STALL_TIMEOUT_MS 150]
                 (watchdog sid "t1" token stall)
                 (expect (true? (await-cancel token 4000))))
               (expect (true? (:stalled? @stall)))
               (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))))

(defdescribe
  turn-launch-orphan-test
  "A throw anywhere AFTER `turn.started` — every caller of `launch-turn-worker!`
   is an HTTP handler or a Throwable-swallowing daemon thread — used to leave a
   public turn nobody runs and nobody ends. It must land a terminal instead."
  (it
    "lands turn.failed when the launch throws after turn.started"
    (let [launch
          @#'state/launch-turn-worker!

          registry
          @#'state/registry

          sid
          (str "launch-" (java.util.UUID/randomUUID))

          tid
          "t1"

          token
          (cancellation/cancellation-token)

          events
          (atom [])

          failed
          (atom nil)]

      (try (swap! registry assoc sid {:next-seq 0 :current-turn tid})
           (with-redefs [state/append-event!
                         (fn [_sid kind _payload]
                           (swap! events conj kind)
                           nil)

                         state/start-turn-stall-watchdog!
                         (fn [& _]
                           nil)

                         state/fail-orphaned-turn!
                         (fn [_sid _tid _token reason]
                           (reset! failed reason)
                           true)

                         cancellation/worker-future
                         (fn [& _]
                           (throw (ex-info "boom" {})))]

             (expect (nil? (launch sid tid "hi" {:cancel-token token}))))
           (expect (= ["turn.started"] @events))
           (expect (str/includes? (str @failed) "turn launch failed"))
           (finally (cancellation/cancel! token) (swap! registry dissoc sid))))))

;; Regression, user report (paraphrased: "the search results are not sorted by
;; freshness"): a search answered in its own relevance order, which had nothing
;; to do with the order the same sessions are LISTED in, so scanning the results
;; meant reading dates that jumped up and down. The store now answers
;; freshest-first and NOTHING re-sorts it - a running session is not lifted over
;; that either, because a band that flips when a turn starts is exactly the motion
;; the navigator's own key just lost.
(defdescribe gateway-search-order-test
             (it "paints the store's freshest-first order and lifts nothing"
                 (with-redefs [lp/db-info
                               (constantly {:db :fake})

                               persistance/db-search-session-matches
                               (fn [_ _ _]
                                 [{:id "new" :rank 0} {:id "old" :rank 2} {:id "run" :rank 1}])

                               ;; A turn in flight is a FIELD on the row, never a place in the answer.
                               bus/live-turns
                               (constantly {"run" "turn-1"})]

                   (expect (= ["new" "old" "run"]
                              (mapv :session_id (state/search-session-matches "q")))))))

(defdescribe gateway-session-order-test
             (it "orders by content time alone, whatever happens to be running"
                 (let [order-summaries
                       #'state/order-session-summaries

                       sessions
                       [{"id" "idle-new" "live" false "modified_at" 4000}
                        {"id" "live-old" "live" true "modified_at" 1000}
                        {"id" "idle-old" "live" false "modified_at" 2000}
                        {"id" "live-new" "live" true "modified_at" (java.util.Date. 3000)}]]

                   (expect (= ["idle-new" "live-new" "idle-old" "live-old"]
                              (mapv #(get % "id") (order-summaries sessions)))))))

;; Regression, same report: with the band gone from the ordering key, a run parked on
;; a human sits wherever its content time puts it - past the end of the first window in
;; a big fleet - so the one state only the operator can clear had to reach a client
;; BESIDE the window instead of being lifted into it.
(defdescribe gateway-awaiting-beside-the-window-test
             (it "answers every parked session in full, outside the window"
                 (with-redefs [lp/db-info
                               (constantly nil)

                               persistance/db-session-turn-stats
                               (constantly nil)

                               lp/by-channel
                               (fn [_]
                                 [{:id "fresh" :title "fresh" :created-at 3000}
                                  {:id "middle" :title "middle" :created-at 2000}
                                  {:id "parked" :title "parked" :created-at 1000}])

                               bus/waiting-requests
                               (constantly {"parked" [{"id" "req-1" "since" 1}]})

                               bus/session-waiting?
                               (fn [sid]
                                 (= "parked" (str sid)))

                               state/soul
                               (fn [sid]
                                 {"id" (str sid)
                                  "created_at" 1000
                                  "is_awaiting_input" (= "parked" (str sid))})]

                   (let [page (state/list-sessions-page :all {:limit 1})]
                     (expect (= ["fresh"] (mapv #(get % "id") (:sessions page))))
                     (expect (= 3 (:total page)))
                     ;; Deep in the fleet, absent from the window, and still complete here.
                     (expect (= ["parked"] (mapv #(get % "id") (:awaiting page))))))))

;; Regression, user report (paraphrased: "the list keeps jumping while I read it"): a
;; window was an OFFSET into an ordering RECOMPUTED per request, so a turn landing while a
;; client walked the pages shifted every row below it - one session arrived twice (a
;; duplicate id) and another vanished from the list, with the merged count still equal to
;; `total`, so nothing downstream could notice.
(defdescribe
  gateway-session-keyset-window-test
  (it
    "pages by the CURSOR of the last row, so a turn landing mid-walk cannot duplicate or drop one"
    (let [fleet (atom [{:id "e" :title "e" :created-at 5000} {:id "d" :title "d" :created-at 4000}
                       {:id "c" :title "c" :created-at 3000} {:id "b" :title "b" :created-at 2000}
                       {:id "a" :title "a" :created-at 1000}])]
      (with-redefs [lp/db-info (constantly nil)
                    persistance/db-session-turn-stats (constantly nil)
                    lp/by-channel (fn [_]
                                    @fleet)
                    bus/waiting-requests (constantly {})
                    state/soul (fn [sid]
                                 ;; The decoration reports the same content time the ranking read,
                                 ;; so the window is the cut a fully decorated sort would have made.
                                 (let [id (str sid)]
                                   {"id" id
                                    "created_at" (some #(when (= id (:id %)) (:created-at %))
                                                       @fleet)}))]

        (let [head (state/list-sessions-page :all {:limit 2})]
          (expect (= ["e" "d"] (mapv #(get % "id") (:sessions head))))
          (expect (= "2:-4000:d" (:next-cursor head)))
          (expect (:has-more head))
          ;; A turn lands in the OLDEST session while the client is still
          ;; walking. Its content time is now the freshest in the fleet, so it
          ;; ranks to the very top - which under an offset pushed every row
          ;; below the head down one place.
          (swap! fleet (fn [rows]
                         (mapv #(if (= "a" (:id %)) (assoc % :created-at 9000) %) rows)))
          (let [page (state/list-sessions-page :all {:limit 2 :after (:next-cursor head)})]
            ;; `d` is not served twice and `c` is not skipped: the cursor names
            ;; a ROW, so the page after it is the same page whatever moved.
            (expect (= ["c" "b"] (mapv #(get % "id") (:sessions page))))
            (expect (= 5 (:total page)))
            (expect (not (:has-more page)))
            (expect (nil? (:next-cursor page))))))))
  (it "breaks a recency tie by id, so a cursor never skips the row beside it"
      (with-redefs [lp/db-info
                    (constantly nil)

                    persistance/db-session-turn-stats
                    (constantly nil)

                    lp/by-channel
                    (fn [_]
                      [{:id "y" :title "y" :created-at 2000} {:id "x" :title "x" :created-at 2000}])

                    bus/waiting-requests
                    (constantly {})

                    state/soul
                    (fn [sid]
                      {"id" (str sid) "created_at" 1000})]

        (let [head (state/list-sessions-page :all {:limit 1})]
          (expect (= ["x"] (mapv #(get % "id") (:sessions head))))
          (expect (= "2:-2000:x" (:next-cursor head)))
          (expect (= ["y"]
                     (mapv #(get % "id")
                           (:sessions
                             (state/list-sessions-page :all {:limit 1 :after "2:-2000:x"}))))))))
  (it "reads a cursor back, and answers nil for anything that is not one"
      (expect (= [2 -4000 "d"] (state/parse-session-cursor "2:-4000:d")))
      ;; Only the first TWO colons end the numbers; the rest is the id.
      (expect (= [0 1 "a:b"] (state/parse-session-cursor "0:1:a:b")))
      ;; The two-part cursor the single-band order used is not one any more: a
      ;; recency alone cannot name a row in a banded list.
      (expect (nil? (state/parse-session-cursor "4000:d")))
      (expect (nil? (state/parse-session-cursor "4000")))
      (expect (nil? (state/parse-session-cursor "2:nope:d")))
      (expect (nil? (state/parse-session-cursor "2:-4000:")))
      (expect (nil? (state/parse-session-cursor "::d")))
      (expect (nil? (state/parse-session-cursor "")))
      (expect (nil? (state/parse-session-cursor nil)))))

;; Regression, user report ("we should go by pages ... and we should decide how many
;; documents a page holds"): the gateway ranked every session it had while the CLIENT
;; decided which of them the list actually shows and in what order, so a
;; `?root=&limit=` window was a different list at the same address - on one machine
;; 1034 sessions counted against 763 painted, the last page 27 pages out - and no
;; client could page anything without downloading the fleet first.
(def ^:private navigator-fleet
  "One of each kind of row: used, an abandoned tap, a starred empty, an empty
   holding unsent words, and a title-less session with a turn in flight."
  [{:id "used" :title "Refactor the pager" :created-at 3000} {:id "blank" :created-at 2500}
   {:id "starred" :created-at 2400 :favorite-rank 1} {:id "typed" :created-at 2300}
   {:id "busy" :created-at 100}])

(defn- navigator-page
  [opts]
  (with-redefs-fn {#'lp/db-info (constantly nil)
                   #'persistance/db-session-turn-stats (constantly nil)
                   #'lp/by-channel (constantly navigator-fleet)
                   #'bus/live-turns (constantly {"busy" "turn-1"})
                   #'bus/waiting-requests (constantly {})
                   #'state/soul (fn [sid]
                                  {"id" (str sid)})}
    (fn []
      (state/list-sessions-page :all opts))))

(defn- navigator-ids [opts] (mapv #(get % "id") (:sessions (navigator-page opts))))

(defdescribe gateway-owns-the-navigator-list-test
             "Which sessions are in the list, and where each one sits, is the GATEWAY's answer."
             (it "leaves out the taps nobody ever used, and counts what is left"
                 ;; "New session" creates the row before the first message exists, so an
                 ;; abandoned tap is a session with no title, no turns and nothing running.
                 (expect (= ["starred" "used" "busy"] (navigator-ids {})))
                 (expect (= 3 (:total (navigator-page {})))))
             (it "lists an empty session the reader marked, and bands it above the rest"
                 ;; A star is the gateway's own fact; unsent words are the device's, and the
                 ;; only part of this list it has to be told about.
                 (expect (= ["starred" "typed" "used" "busy"] (navigator-ids {:dirty ["typed"]})))
                 (expect (= 4 (:total (navigator-page {:dirty ["typed"]})))))
             (it "walks the bands with ONE cursor, so a page boundary inside them holds"
                 (let [head (navigator-page {:limit 1 :dirty ["typed"]})]
                   (expect (= ["starred"] (mapv #(get % "id") (:sessions head))))
                   (expect (= "0:1:starred" (:next-cursor head)))
                   (let [second-page (navigator-page
                                       {:limit 2 :after (:next-cursor head) :dirty ["typed"]})]
                     ;; Across the band boundary: no row served twice, none skipped.
                     (expect (= ["typed" "used"] (mapv #(get % "id") (:sessions second-page))))
                     (expect (= "2:-3000:used" (:next-cursor second-page)))))))

;; Regression, reported in this Vis session ("po co my w ogóle wysyłamy wiersze ... to
;; powinny być najpotrzebniejsze rzeczy"): every listed session carried the prose it
;; opened with (`first_request` - 228 bytes a row, a QUARTER of a 20-row window) plus
;; `external_id` and `owner_id`, which no client paints. The prose was not free either:
;; finding it grouped `user_request` over EVERY turn in the store on every list read.
(defdescribe
  session-row-is-what-a-picker-paints-test
  (it "keeps the store's opening prose out of a listed row"
      (let [stats
            {"used" {:turn-count 2
                     :latest-turn-at (java.util.Date. 3000)
                     :first-request "Refactor the pager so it stops draining the fleet"}}

            page
            (with-redefs-fn {#'lp/db-info (constantly :db)
                             #'persistance/db-session-turn-stats (constantly stats)
                             #'lp/by-channel (constantly [{:id "used"
                                                           :title "Refactor the pager"
                                                           :created-at 3000}])
                             #'bus/live-turns (constantly {})
                             #'bus/waiting-requests (constantly {})
                             #'state/soul (fn [sid]
                                            {"id" (str sid)})}
              (fn []
                (state/list-sessions-page :all {})))

            row
            (first (:sessions page))]

        (expect (= "used" (get row "id")))
        (expect (= 2 (get row "turn_count")))
        (expect (not (contains? row "first_request")))))
  (it "carries neither the owner nor the channel's own id"
      (let [row (with-redefs-fn {#'lp/by-id (constantly {:id "used"
                                                         :channel :cli
                                                         :title "Refactor the pager"
                                                         :created-at 3000
                                                         :external-id "chat-42"
                                                         :owner-id "local"})
                                 #'lp/db-info (constantly :db)
                                 #'persistance/db-session-turn-stats
                                 (constantly {:turn-count 2
                                              :latest-turn-at (java.util.Date. 3000)
                                              :first-request
                                              "Refactor the pager so it stops draining"})
                                 #'bus/live-turn-id (constantly nil)
                                 #'bus/session-waiting? (constantly false)
                                 #'smodel/pending-pref (constantly [false nil])}
                  (fn []
                    (state/soul "used")))]
        (expect (= "used" (get row "id")))
        (expect (= 2 (get row "turn_count")))
        (expect (not (contains? row "first_request")))
        (expect (not (contains? row "external_id")))
        (expect (not (contains? row "owner_id"))))))
;; Regression, same report: a project header carried the GATEWAY's tally while the rows
;; under it were the client's own filtered list, so the header said 1034 over pages that
;; held 763 - the two numbers could only agree by accident.
(defdescribe
  gateway-header-count-is-the-page-count-test
  (it "counts a project exactly as its own pages fill"
      (let [fleet [{:id "s1" :title "One" :created-at 300} {:id "s2" :title "Two" :created-at 200}
                   ;; Never used: not in the list, and not in the header either.
                   {:id "s3" :created-at 100}]]
        (with-redefs-fn {#'lp/db-info (constantly ::db)
                         #'lp/projects (constantly [])
                         #'persistance/db-session-turn-stats (constantly nil)
                         #'lp/by-channel (constantly fleet)
                         #'state/session-project-root (constantly "/repo/a")
                         #'bus/live-turns (constantly {})
                         #'bus/waiting-requests (constantly {})
                         #'state/soul (fn [sid]
                                        {"id" (str sid)})}
          (fn []
            (let [overview (state/projects-overview)
                  header (get-in (into {}
                                       (map (fn [p]
                                              [(get p "root") p]))
                                       (:projects overview))
                                 ["/repo/a" "session_count"])]

              (expect (= 2 header))
              (expect (= header (:total (state/list-sessions-page :all {:root "/repo/a"}))))
              (expect (= 2 (:session_count overview)))))))))

;; Regression, this Vis session (paraphrased: "the TUI should use the limit on the session
;; list too"): a channel that wanted ONE project's tab set, or the session a short id
;; names, downloaded the whole fleet and filtered the answer in its own process.
(defn- three-session-window
  "One `list-sessions-page` answer over a fixed three-session fleet."
  [opts]
  (with-redefs-fn {#'lp/db-info (constantly ::db)
                   #'lp/projects (constantly [])
                   #'persistance/db-session-turn-stats (constantly nil)
                   #'lp/by-channel (constantly
                                     [{:id "aa11" :title "One" :created-at 300 :project-id "p1"}
                                      {:id "aa22" :title "Two" :created-at 200 :project-id "p2"}
                                      {:id "bb33" :title "Three" :created-at 100 :project-id "p1"}])
                   #'bus/live-turns (constantly {})
                   #'bus/waiting-requests (constantly {})
                   #'state/soul (fn [sid]
                                  {"id" (str sid)})}
    (fn []
      (state/list-sessions-page :all opts))))

(defdescribe gateway-cuts-the-window-to-the-question-test
             (it "answers one project's tab set instead of the fleet"
                 (let [got (three-session-window {:project-id "p1"})]
                   (expect (= ["aa11" "bb33"] (mapv #(get % "id") (:sessions got))))
                   (expect (= 2 (:total got)))))
             (it "answers the session a short id names"
                 (let [got (three-session-window {:id-prefix "aa" :limit 2})]
                   (expect (= ["aa11" "aa22"] (mapv #(get % "id") (:sessions got))))
                   (expect (= 2 (:total got)))))
             (it "answers the rows a set of ids names, in its OWN order"
                 ;; What a picker holding ONE window paints a search hit outside it with.
                 (let [got (three-session-window {:ids ["bb33" "aa22"]})]
                   (expect (= ["aa22" "bb33"] (mapv #(get % "id") (:sessions got))))
                   (expect (= 2 (:total got)))))
             (it "leaves the list whole when the window asks nothing of it"
                 (expect (= 3 (:total (three-session-window {}))))))
;; Regression, user report (paraphrased: "clicking a session suddenly makes it the
;; freshest thing and it jumps to the top, and after the gateway restarts the empty
;; sessions sit above the one I actually worked in"): the navigator key read
;; `last_active_at` / the registry's `:last-active`, which is a TOUCH clock -
;; `append-event!` and `set-session-model!` bump it, and a daemon start stamps every
;; session with one hydration time.
(defdescribe gateway-session-touch-order-test
             (it "ignores last_active_at, so touching a row cannot move it"
                 (let [ids
                       (fn [rows]
                         (mapv #(get % "id") (#'state/order-session-summaries rows)))

                       worked
                       [{"id" "worked" "created_at" 1000 "modified_at" 2000}
                        {"id" "turnless" "created_at" 1500}]

                       touched
                       [{"id" "worked" "created_at" 1000 "modified_at" 2000}
                        {"id" "turnless" "created_at" 1500 "last_active_at" 9000}]]

                   (expect (= ["worked" "turnless"] (ids worked)))
                   ;; The two payloads differ ONLY in the touch, so the sequence must
                   ;; be identical: no motion without a change of content.
                   (expect (= (ids worked) (ids touched)))))
             (it "ranks a turnless session by creation, never by the last touch"
                 (let [recency #'state/record-recency-ms]
                   (expect (= 2000 (recency {:created-at 1000} {:latest-turn-at 2000})))
                   (expect (= 1000 (recency {:created-at 1000} nil))))))
(defdescribe
  gateway-prewarm-pool-test
  (it "stamps a built prewarm with its persisted router root"
      (let [sid
            (java.util.UUID/randomUUID)

            create-calls
            (atom [])

            db-calls
            (atom [])]

        (with-redefs-fn {#'lp/create! (fn [channel opts]
                                        (swap! create-calls conj [channel opts])
                                        {:id sid :channel channel :workspace-id :workspace})
                         #'lp/db-info (constantly ::db)
                         #'persistance/db-get-session (fn [db id]
                                                        (swap! db-calls conj [db id])
                                                        {:provider :provider-a :model "model-a"})
                         #'state/put-session! (fn [& _])}
          (fn []
            (let [created (#'state/create-session-cold! {:channel :api :prewarm? true})]
              (expect (= {:id sid
                          :channel :api
                          :workspace-id :workspace
                          :router-root {:provider :provider-a :model "model-a"}}
                         created))
              (expect (= [[:api {:prewarm? true}]] @create-calls))
              (expect (= [[::db sid]] @db-calls)))))))
  (it
    "adopts a ready session and requests an asynchronous refill"
    (let [pool
          @#'state/prewarm-pool

          prior
          @pool

          sid
          (java.util.UUID/randomUUID)

          refills
          (atom [])]

      (try (reset! pool {:ready {:api [{:id sid
                                        :channel :api
                                        :title nil
                                        :external-id nil
                                        :workspace-id :workspace
                                        :router-root {:provider :provider-a :model "model-a"}}]}
                         :in-flight {}
                         :accepting? true})
           (with-redefs [lp/get-router
                         (constantly {:providers [{:id :provider-a :models [{:name "model-a"}]}]})

                         state/ensure-prewarmed!
                         #(swap! refills conj %)

                         state/claim-prewarmed!
                         (fn [session title]
                           (assoc session :title title))]

             (let [created (state/create-session!
                             {:channel :api :title "Ready" :root (System/getProperty "user.dir")})]
               (expect (= (str sid) (get created "id")))
               (expect (= "Ready" (get created "title")))
               (expect (= [:api] @refills))
               (expect (empty? (get-in @pool [:ready :api])))))
           (finally (reset! pool prior)))))
  ;; Regression, issue #159: a ready prewarm built for the previous router root
  ;; was claimed after the default changed, leaving persisted metadata on model A
  ;; while the refreshed environment routed the first turn through model B.
  (it
    "discards a ready session whose router root no longer matches the default"
    (let [pool
          @#'state/prewarm-pool

          prior
          @pool

          pooled-id
          (java.util.UUID/randomUUID)

          cold-id
          (java.util.UUID/randomUUID)

          claims
          (atom [])

          cold-calls
          (atom [])

          dropped
          (atom [])

          deleted
          (atom [])

          refills
          (atom [])]

      (try (reset! pool {:ready {:api [{:id pooled-id
                                        :channel :api
                                        :workspace-id :workspace
                                        :router-root {:provider :provider-a :model "model-a"}}]}
                         :in-flight {}
                         :accepting? true})
           (with-redefs [lp/get-router
                         (constantly {:providers [{:id :provider-b :models [{:name "model-b"}]}]})

                         state/claim-prewarmed!
                         (fn [session title]
                           (swap! claims conj [(:id session) title])
                           (assoc session :title title))

                         state/create-session-cold!
                         (fn [opts]
                           (swap! cold-calls conj opts)
                           {:id cold-id
                            :channel (:channel opts)
                            :title (:title opts)
                            :workspace-id :workspace})

                         state/drop-session!
                         #(swap! dropped conj %)

                         lp/delete!
                         #(swap! deleted conj %)

                         state/ensure-prewarmed!
                         #(swap! refills conj %)]

             (let [created (state/create-session! {:channel :api :title "Current"})]
               (expect (= (str cold-id) (get created "id")))
               (expect (= "Current" (get created "title")))
               (expect (empty? @claims))
               (expect (= [{:channel :api :title "Current"}] @cold-calls))
               (expect (= [pooled-id] @dropped))
               (expect (= [pooled-id] @deleted))
               (expect (= [:api] @refills))))
           (finally (reset! pool prior)))))
  (it "bypasses the pool for a purpose-built workspace"
      (let [pool
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
             (with-redefs [state/create-session-cold!
                           (fn [opts]
                             (swap! cold-calls conj opts)
                             {:id cold-id :channel :api :workspace-id (:workspace-id opts)})]
               (let [created (state/create-session! {:channel :api :workspace-id :branch})]
                 (expect (= (str cold-id) (get created "id")))
                 (expect (= [:branch] (mapv :workspace-id @cold-calls)))
                 (expect (= pooled-id (get-in @pool [:ready :api 0 :id])))))
             (finally (reset! pool prior))))))

(it "ships concise structured iteration errors for live retry rendering"
    (let [[type _ payload] (#'state/chunk->event
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
      (let [sid
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
      (let [sid
            (str (java.util.UUID/randomUUID))

            registry
            @#'state/registry

            saved
            @registry]

        (try (reset! registry {sid {:next-seq 4 :current-turn nil :turns {}}})
             (expect (nil? (state/running-turn-start-cursor sid)))
             (finally (reset! registry saved)))))
  (it "is nil when the running turn has no recorded start seq (foreign, unhydrated)"
      (let [sid
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
  (it
    "holds only later requests after a failure and resumes them explicitly"
    (let [sid
          "queue-pause-test"

          reg
          @#'state/registry

          evs
          (atom [])

          seed!
          (fn []
            (reset! reg {sid {:next-seq 0
                              :current-turn nil
                              :turn-order ["q1" "q2"]
                              :turns {"q1" {:turn_id "q1" :status "failed"}
                                      "q2" {:turn_id "q2" :status "queued" :queued_at 2}}}})
            (reset! evs []))]

      (try (with-redefs-fn {#'state/append-event! (fn [_ type payload & _]
                                                    (swap! evs conj [type payload]))
                            #'state/drain-next-queued! (fn [_ & _]
                                                         (swap! evs conj [:drained nil]))
                            #'state/left-queued-by-cancel? (fn [_ _]
                                                             false)
                            #'cancellation/cancelled? (fn [_]
                                                        false)}
             (fn []
               (seed!)
               (#'state/after-turn-terminal!
                sid
                "q1"
                {:failed? true :cancel-token :token :stalled? false})
               (expect (= [["queue.paused" {:reason "turn_failed" :held 1}]] @evs))
               (expect (= "failed" (get-in @reg [sid :turns "q1" :status])))
               (expect (= "queued" (get-in @reg [sid :turns "q2" :status])))
               (expect (some? (state/queue-paused-info sid)))
               (reset! evs [])
               (state/resume-queue! sid {:auto? false})
               (expect (= ["queue.resumed" :drained] (mapv first @evs)))
               (expect (nil? (state/queue-paused-info sid)))))
           (finally (swap! reg dissoc sid))))))


(defdescribe gateway-resource-bounds-test
             (it "retains only the configured replay tail"
                 (with-redefs-fn {#'state/EVENT_RING_MAX (delay 3)}
                   (fn []
                     (let [trim-ring
                           (deref #'state/trim-ring)

                           ring
                           (trim-ring [1 2 3 4 5])]

                       (expect (= [3 4 5] ring))
                       (expect (instance? clojure.lang.PersistentQueue ring))))))
             (it "waits for a global turn permit and lets cancellation win without execution"
                 (let [semaphore
                       (java.util.concurrent.Semaphore. 1 true)

                       token
                       (cancellation/cancellation-token)

                       acquire!
                       (deref #'state/acquire-turn-permit!)

                       waiting
                       (var-get #'state/turns-waiting)

                       queued
                       (promise)]

                   (.acquire semaphore)
                   (try (with-redefs-fn {#'state/turn-permits (delay semaphore)}
                          (fn []
                            (add-watch waiting
                                       ::queued
                                       (fn [_ _ _ n]
                                         (when (pos? n) (deliver queued true))))
                            (try (let [result (future (acquire! token))]
                                   (expect (= true (deref queued 1000 false)))
                                   (expect (not (realized? result)))
                                   (cancellation/cancel! token)
                                   (expect (= false (deref result 1000 ::timeout))))
                                 (finally (remove-watch waiting ::queued)))))
                        (finally (.release semaphore)))))
             (it "keeps one unused prewarm context per channel"
                 (let [pool
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
                   (doseq [k [:turns-executing :turns-waiting :turn-concurrency-limit
                              :replay-events-retained :jvm-heap-used-bytes :process-rss-bytes
                              :jvm-gc-count-total :jvm-thread-count :env-cache-size]]
                     (expect (contains? snapshot k))))))

(defdescribe
  queued-turn-correlation-id-test
  "ONE identity for a queued turn. The submitter's `idempotency_key` rides on the
   turn record AND on `turn.queued`, so a channel binds its optimistic \"Queued\"
   row to the gateway record by ID instead of guessing by request text (two
   identical prompts are indistinguishable by text)."
  (it "echoes the submitter's correlation id on the queued record and its event"
      (let [registry
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
                 (let [res
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
    (let [registry
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
             #(#'state/launch-turn-worker!
                sid
                tid
                "hello"
                {:cancel-token (cancellation/cancellation-token)}))
           (expect (= "cid-1"
                      (->> @events
                           (filter (comp #{"turn.started"} first))
                           first
                           second
                           :idempotency_key)))
           (finally (swap! registry dissoc sid)))))
  (it "always lands a terminal for a turn whose token is cancelled at launch"
      ;; The worker runs on a FutureTask: cancelling it BEFORE its thread enters
      ;; `run` skips the body entirely. `turn.started` is already on the wire by
      ;; then, so landing nothing pinned `:current-turn` to a turn nobody runs —
      ;; an empty assistant row forever and a queue that never drains again.
      (let [registry
            @#'state/registry

            sid
            (str "cancel-at-launch-" (java.util.UUID/randomUUID))

            tid
            "turn-1"

            token
            (cancellation/cancellation-token)

            landed
            (atom [])]

        (try (swap! registry assoc
               sid
               {:next-seq 0
                :current-turn tid
                :turns {tid {:turn_id tid :session_id sid :status "running" :started_at 123}}})
             (cancellation/cancel! token)
             (with-redefs-fn {#'state/append-event! (fn [& _]
                                                      nil)
                              #'state/cancel-waiting-turn! (fn [_sid t _token]
                                                             (swap! landed conj t)
                                                             nil)}
               #(do (#'state/launch-turn-worker! sid tid "hello" {:cancel-token token})
                    (loop [n 0]
                      (when (and (empty? @landed) (< n 200)) (Thread/sleep 10) (recur (inc n))))))
             (expect (= [tid] @landed))
             (finally (swap! registry dissoc sid)))))
  (it "echoes the submitter's correlation id on every terminal event"
      ;; A channel reconciles its optimistic bubble against the terminal event
      ;; INDEPENDENTLY of its blocking submit worker, and a tab whose submit ack is
      ;; still in flight knows only the correlation id it minted. A terminal without
      ;; that id left the spinner running until the stranded worker returned.
      (let [registry
            @#'state/registry

            sid
            (str "terminal-idem-" (java.util.UUID/randomUUID))

            tid
            "turn-1"

            prose
            (content/prose "full technical answer")

            spoken
            (content/speech "spoken" "The work is complete.")]

        (try (swap! registry assoc
               sid
               {:next-seq 0 :turns {tid {:turn_id tid :status "running" :idempotency_key "cid-1"}}})
             (expect (= [{:turn_id tid :status "completed" :idempotency_key "cid-1"}
                         {:turn_id tid :status "failed" :idempotency_key "cid-1"}
                         {:turn_id tid :status "cancelled" :idempotency_key "cid-1"}]
                        (mapv #(#'state/turn-terminal-payload sid tid %)
                              ["completed" "failed" "cancelled"])))
             ;; A turn with no recorded key stays key-free instead of shipping an
             ;; explicit nil correlation id that would match another tab's nil.
             (expect (= {:turn_id "ghost" :status "failed"}
                        (#'state/turn-terminal-payload sid "ghost" "failed")))
             ;; The terminal frame is the only answer guaranteed to reach a web
             ;; client whose throttled body deltas race completion. It carries
             ;; the visual answer and the secondary spoken projection together.
             (swap! registry assoc-in [sid :turns tid :content] [prose spoken])
             (expect (= [prose spoken]
                        (:content (#'state/turn-terminal-payload sid tid "completed"))))
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
               (let [res
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
    (let [tmp
          (java.nio.file.Files/createTempDirectory "hydrate-race"
                                                   (make-array java.nio.file.attribute.FileAttribute
                                                               0))

          sid
          (str "hydrate-race-" (java.util.UUID/randomUUID))

          reg
          @#'state/registry

          subs-n
          4]

      (with-redefs [bus/events-dir (fn []
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
             (let [barrier (java.util.concurrent.CyclicBarrier. subs-n)
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

(defdescribe transcript-byte-budget-test
             "Windowed transcript pages stay bounded even when individual turns are large."
             (it
               "caps a window by encoded size and advances from the returned offset"
               (let [sid
                     (java.util.UUID/randomUUID)

                     rows
                     (mapv (fn [n]
                             {:id (str "turn-" n) :position n})
                           (range 5))]

                 (with-redefs-fn {#'lp/db-info (constantly ::db)
                                  #'persistance/db-list-session-turns (fn [_ _]
                                                                        rows)
                                  #'persistance/db-list-turns-attachments (fn [_ ids]
                                                                            (zipmap ids
                                                                                    (repeat [])))
                                  (ns-resolve 'com.blockether.vis.internal.gateway.state
                                              'TRANSCRIPT_PAGE_MAX_BYTES)
                                  (delay 180)
                                  #'state/transcript-turn
                                  (fn [_db _att row]
                                    {:turn_id (:id row) :payload (apply str (repeat 100 "x"))})}
                   (fn []
                     (let [newest
                           (state/transcript-page sid {:limit 5})

                           earlier
                           (state/transcript-page sid {:offset 0 :limit (:offset newest)})]

                       ;; The row that BUSTS the budget is kept, not deferred: dropping it
                       ;; would make a single oversized turn (a big image attachment)
                       ;; unreachable on every page. Overshoot is bounded by one turn.
                       (expect (= ["turn-3" "turn-4"] (mapv #(get % "turn_id") (:turns newest))))
                       (expect (= 3 (:offset newest)))
                       (expect (:has-more newest))
                       (expect (= ["turn-1" "turn-2"] (mapv #(get % "turn_id") (:turns earlier))))
                       (expect (= 1 (:offset earlier)))
                       (expect (:has-more earlier))))))))

(defdescribe
  submit-turn-sync-terminal-isolation-test
  "`submit-turn-sync!` subscribes BEFORE it knows its own turn id. Treating an
   unknown id as \"mine\" hands a sibling turn's terminal to this caller, and a
   turn that settled at/just-before the cursor leaves the blocking deref
   parked forever (unlike `attach-turn-sync!`, which recovers from the record)."
  (it "ignores another turn's terminal that lands before the submit returns"
      (let [handler (promise)]
        (with-redefs [state/subscribe! (fn [_ _ h _]
                                         (deliver handler h)
                                         [])
                      state/unsubscribe! (fn [_ _]
                                           nil)
                      state/get-turn (fn [_ _]
                                       nil)
                      state/submit-turn! (fn [_ _]
                                           (@handler {"type" "turn.failed" "turn_id" "OTHER"})
                                           {:turn {"turn_id" "MINE"}})]

          (let [f (future (state/submit-turn-sync! "sid-iso" {}))]
            (expect (= ::pending (deref f 500 ::pending)))
            (@handler {"type" "turn.completed" "turn_id" "MINE"})
            (let [res (deref f 2000 ::pending)]
              (expect (not= ::pending res))
              (expect (nil? (get res "error")))
              (expect (= "MINE" (get res "session_turn_id"))))))))
  (it "returns on a CANCELLED terminal instead of parking on the turn forever"
      ;; Regression: only `turn.completed`/`turn.failed` counted as terminal, so a
      ;; user stop (`turn.cancelled`, what `run-turn!` appends for an Esc or a
      ;; stall force-cancel) left this blocking reader — and the SSE connection
      ;; behind it — alive for the rest of the session, while the queue happily
      ;; drained the next turn into the same channel.
      (let [handler (promise)]
        (with-redefs [state/subscribe! (fn [_ _ h _]
                                         (deliver handler h)
                                         [])
                      state/unsubscribe! (fn [_ _]
                                           nil)
                      state/get-turn (fn [_ _]
                                       nil)
                      state/submit-turn! (fn [_ _]
                                           {:turn {"turn_id" "MINE"}})]

          (let [f (future (state/submit-turn-sync! "sid-cancelled" {}))]
            (@handler {"type" "turn.cancelled" "turn_id" "MINE" "status" "cancelled"})
            (let [res (deref f 2000 ::pending)]
              (expect (not= ::pending res))
              (expect (= "MINE" (get res "session_turn_id"))))))))
  (it "recovers a terminal from the stored record instead of blocking forever"
      (let [handler (promise)]
        (with-redefs [state/subscribe! (fn [_ _ h _]
                                         (deliver handler h)
                                         [])
                      state/unsubscribe! (fn [_ _]
                                           nil)
                      state/get-turn (fn [_ _]
                                       {"turn_id" "MINE" "status" "completed"})
                      state/submit-turn! (fn [_ _]
                                           {:turn {"turn_id" "MINE"}})]

          (let [res (deref (future (state/submit-turn-sync! "sid-settled" {})) 2000 ::pending)]
            (expect (not= ::pending res))
            (expect (nil? (get res "error"))))))))

(defdescribe
  live-block-close-per-iteration-test
  "A live reasoning/prose block must be CLOSED at the iteration boundary that
   ended it, not held open until the turn's terminal flush. A 72-iteration turn
   used to stream 146 half-open blocks for ten minutes and close them all in a
   single burst right before `turn.completed`, so every consumer that paints an
   open reasoning block as live kept the LAST thinking on screen as if the work
   had never finished."
  (it
    "closes each iteration's block at its own iteration.completed"
    (let [sid
          (str (random-uuid))

          tid
          (str (random-uuid))]

      (swap! @#'state/registry assoc
        sid
        {:next-seq 0
         :events []
         :subscribers {}
         :turns {tid {:turn_id tid :status "running"}}
         :turn-order [tid]
         :current-turn tid})
      (with-redefs [lp/send!
                    (fn [_ _ opts]
                      (let [on-chunk (get-in opts [:hooks :on-chunk])]
                        (on-chunk {:phase :reasoning :iteration 1 :thinking "alpha."})
                        (on-chunk
                          {:phase :iteration-final :iteration 1 :thinking "alpha." :done? false})
                        (on-chunk {:phase :reasoning :iteration 2 :thinking "beta."})
                        (on-chunk
                          {:phase :iteration-final :iteration 2 :thinking "beta." :done? true}))
                      {:status :ok :answer nil})]
        (#'state/run-turn! sid tid "hi" {}))
      (let [types
            (mapv #(get % "type") (:events (get @@#'state/registry sid)))

            completed-idx
            (fn [iteration]
              (first (keep-indexed (fn [idx event]
                                     (when (and (= "content.block.completed" (get event "type"))
                                                (str/ends-with? (str (get event "block_id"))
                                                                (str ":" iteration)))
                                       idx))
                                   (:events (get @@#'state/registry sid)))))

            idx-of
            (fn [type]
              (first (keep-indexed #(when (= type %2) %1) types)))

            started-idxs
            (keep-indexed #(when (= "content.block.started" %2) %1) types)

            terminal-idx
            (idx-of "turn.completed")]

        ;; Every started block is closed exactly once.
        (expect (= (count (filter #{"content.block.started"} types))
                   (count (filter #{"content.block.completed"} types))))
        ;; …and iteration 1's block closes BEFORE iteration 2 even starts,
        ;; instead of waiting for the terminal flush.
        (expect (< (long (completed-idx 1)) (long (second started-idxs))))
        (expect (< (long (completed-idx 2)) (long terminal-idx)))))))

(defdescribe
  cancel-terminal-backstop-test
  "A cancelled turn ALWAYS lands a terminal, even when its worker cannot.

   `cancel!` only fires a token: a worker parked in uninterruptible code (the
   observed case: the between-turns Python GC blocked on a GIL held by another
   session's shell subprocess, inside `send!`'s `finally` — i.e. AFTER the engine
   unwound and BEFORE the terminal event) never reaches its own terminal append.
   The session then stays pinned to a turn nobody is running, the queued backlog
   never drains, and every channel shows 'Sending request to provider' forever.
   The backstop lands `turn.cancelled` for it, and the terminal claim guarantees
   the thawed worker cannot land a second one."
  (let [backstop
        @#'state/start-cancel-terminal-backstop!

        claim
        @#'state/claim-turn-terminal!

        registry
        @#'state/registry

        await-flag
        (fn [flag ms]
          (let [deadline (+ (System/currentTimeMillis) (long ms))]
            (loop []

              (cond @flag true
                    (>= (System/currentTimeMillis) deadline) false
                    :else (do (Thread/sleep 25) (recur))))))]

    (it "claims a turn's ONE terminal landing exactly once per run"
        (let [sid
              (str "claim-" (java.util.UUID/randomUUID))

              run
              (cancellation/cancellation-token)]

          (expect (true? (claim sid "t1" run)))
          (expect (false? (claim sid "t1" run)))
          ;; a different turn of the same session is untouched
          (expect (true? (claim sid "t2" run)))))
    (it "lands the terminal for a cancelled worker that never lands its own"
        (let [sid
              (str "backstop-" (java.util.UUID/randomUUID))

              tid
              "t1"

              token
              (cancellation/cancellation-token)

              landed
              (atom false)]

          (try (swap! registry assoc sid {:next-seq 0 :current-turn tid})
               (backstop sid
                         tid
                         token
                         150
                         (fn [& _]
                           (reset! landed true)))
               (expect (true? (await-flag landed 4000)))
               (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))
    ;; Regression, session e8c9dbc9-388d-43a4-8264-9dd5adec4449: landing the
    ;; synthesized terminal freed the session ON PAPER while the wedged worker's
    ;; thread still owned the engine lock, so the very next turn started and then
    ;; parked on it forever — `turn.started` and nothing else, for the life of the
    ;; daemon. A backstop must condemn the engine it just gave up on.
    (it "condemns the session engine it just declared dead"
        (let [sid
              (str "backstop-" (java.util.UUID/randomUUID))

              token
              (cancellation/cancellation-token)

              condemned
              (atom nil)]

          (try (swap! registry assoc sid {:next-seq 0 :current-turn "t1"})
               (with-redefs-fn {#'lp/condemn-env! (fn [id]
                                                    (reset! condemned id)
                                                    true)}
                 (fn []
                   (backstop sid
                             "t1" token
                             150 (fn [& _]
                                   nil))
                   (expect (true? (await-flag condemned 4000)))
                   (expect (= sid @condemned))))
               (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))
    (it "stays silent once the turn is no longer the session's current turn"
        (let [sid
              (str "backstop-" (java.util.UUID/randomUUID))

              token
              (cancellation/cancellation-token)

              landed
              (atom false)]

          (try
            ;; the worker DID land its terminal and the session moved on
            (swap! registry assoc sid {:next-seq 0 :current-turn "other"})
            (backstop sid
                      "t1" token
                      150 (fn [& _]
                            (reset! landed true)))
            (expect (false? (await-flag landed 1200)))
            (finally (cancellation/cancel! token) (swap! registry dissoc sid)))))))

;; Regression: a cancelled turn whose engine had never produced a single chunk
;; still got the full 30s worker grace, so "Vis is cancelling" stayed on screen
;; for half a minute after the user pressed stop — observed on a turn that sat
;; 3m47s with 0 iterations and then took the whole backstop to settle.
(defdescribe
  cancel-terminal-grace-test
  "How long a cancelled worker keeps its own terminal depends on whether it ever
   produced output: a silent turn has nothing to flush and no block to close."
  (let [grace @#'state/cancel-terminal-grace-ms]
    (it "gives a silent turn the short grace and a streaming one the full grace"
        (expect (= (long @#'state/SILENT_CANCEL_TERMINAL_GRACE_MS) (grace nil)))
        (expect (= (long @#'state/SILENT_CANCEL_TERMINAL_GRACE_MS)
                   (grace (atom {:phase :provider}))))
        (expect (= (long @#'state/CANCEL_TERMINAL_GRACE_MS)
                   (grace (atom {:phase :provider :produced? true})))))))

(defdescribe
  append-event-stamp-test
  "`append-event!` canonicalizes the payload ONCE, OUTSIDE the `swap!`. There is
   a single process-wide `registry` atom, so concurrent appends collide and a
   `swap!` body re-runs from scratch on every CAS retry — with `wire/canonical`
   inside it, a megabyte-sized tool result was re-walked per retry (measured:
   15155 walks to append 2400 events, 84% of the work discarded). Hoisting it
   also makes the documented stamp rule absolute."
  (it "stamps identity over payload keys that merely CANONICALIZE onto it"
      (let [sid
            (java.util.UUID/randomUUID)

            registry
            @#'state/registry

            saved
            @registry]

        (try (with-redefs [bus/publish! (fn [& _]
                                          nil)]
               (let [event (state/append-event! sid
                                                "turn.delta"
                                                {:session-id "SOMEONE-ELSES-SESSION"
                                                 :seq 999999
                                                 :type "spoofed"
                                                 :schema 42
                                                 :text "hi"})]
                 (expect (= (str sid) (get event "session_id")))
                 (expect (= 1 (get event "seq")))
                 (expect (= "turn.delta" (get event "type")))
                 (expect (= 1 (get event "schema")))
                 (expect (= "hi" (get event "text")))))
             (finally (reset! registry saved)))))
  (it
    "walks each payload exactly once per append, even under CAS contention"
    (let [registry
          @#'state/registry

          saved
          @registry

          walks
          (atom 0)

          real
          wire/canonical

          ;; Only OUR payloads are counted: the canonical event that flows on to
          ;; fan-out carries string keys, so it can never be mistaken for one.
          payload
          {:bug9 true :rows (vec (repeat 200 {:a 1 :b "x" :nested {:c [1 2 3]}}))}]

      (try (with-redefs [bus/publish!
                         (fn [& _]
                           nil)

                         wire/canonical
                         (fn [p]
                           (when (and (map? p) (contains? p :bug9)) (swap! walks inc))
                           (real p))]

             (->> (range 8)
                  (mapv (fn [_]
                          (future (dotimes [_ 50]
                                    (state/append-event! (java.util.UUID/randomUUID)
                                                         "turn.delta"
                                                         payload)))))
                  (run! deref))
             (expect (= 400 @walks)))
           (finally (reset! registry saved))))))

(defdescribe
  list-turns-dedup-scale-test
  "Hydration dedups live gateway rows against persisted engine rows by an exact
   id match, so the persisted ids belong in a SET. The old `some` rescanned every
   persisted row for every live row, and its fallback arm deep-compares whole
   `:content` vectors — 800 live against 800 persisted measured 14.1 ms, versus
   0.19 ms for the set. A non-blank engine id decides it outright, because the
   fallback arm REQUIRES a blank one and can never fire."
  (it
    "drops the live row a persisted id owns and keeps the one none owns"
    (let [sid
          (java.util.UUID/randomUUID)

          dup-engine
          (str (java.util.UUID/randomUUID))

          own-engine
          (str (java.util.UUID/randomUUID))

          registry
          @#'state/registry

          saved
          @registry

          live
          (fn [tid engine-id]
            {:turn_id tid
             :engine_turn_id engine-id
             :session_id (str sid)
             :status "completed"
             :request "hello"
             :content [{"id" "b1" "type" "prose" "markdown" "hi"}]
             :started_at 1000})

          persisted
          (conj (mapv (fn [i]
                        {:id (str "row-" i)
                         :status :success
                         :user-request (str "q" i)
                         :content []
                         :created-at (java.util.Date. (+ 1000 (long i)))})
                      (range 200))
                {:id dup-engine
                 :status :success
                 :user-request "hello"
                 :content [{"id" "b1" "type" "prose" "markdown" "hi"}]
                 :created-at (java.util.Date. 1500)})]

      (try (reset! registry {(str sid) {:next-seq 0
                                        :turn-order ["gateway-dup" "gateway-own"]
                                        :turns {"gateway-dup" (live "gateway-dup" dup-engine)
                                                "gateway-own" (live "gateway-own" own-engine)}}})
           (with-redefs [persistance/db-list-session-turns
                         (fn [_ _]
                           persisted)

                         persistance/db-list-turns-attachments
                         (fn [_ _]
                           {})]

             (let [ids (set (map #(get % "turn_id") (state/list-turns sid)))]
               (expect (contains? ids dup-engine))
               (expect (not (contains? ids "gateway-dup")))
               (expect (contains? ids "gateway-own"))))
           (finally (reset! registry saved))))))

(defdescribe
  cancel-source-test
  ;; A cancelled turn surfaces everywhere as a bare interrupt, and the daemon
  ;; used to record NOTHING about its origin: a stall force-cancel, the
  ;; shutdown sweep and a user stop were indistinguishable in a post mortem.
  ;; Each entry point now stamps itself on the turn's cancellation token.
  (it
    "stamps the origin of every cancel entry point on the token"
    (let [sid
          (str "cancel-source-" (java.util.UUID/randomUUID))

          token-a
          (cancellation/cancellation-token)

          token-b
          (cancellation/cancellation-token)

          token-c
          (cancellation/cancellation-token)

          registry
          (atom {sid {:current-turn "b"
                      :turns {"a" {:turn_id "a" :status "running" :cancel-token token-a}
                              "b" {:turn_id "b"
                                   :status "running"
                                   :idempotency_key "b-key"
                                   :cancel-token token-b}
                              "c" {:turn_id "c" :status "running" :cancel-token token-c}}}})]

      (with-redefs-fn {#'state/registry registry}
        (fn []
          (expect (= {:status "cancelling"} (state/cancel-turn! sid "a")))
          (expect (= :client-cancel-turn (cancellation/cancel-reason token-a)))
          (expect (= "b" (:turn_id (state/cancel-current-turn! sid "b-key"))))
          (expect (= :client-cancel-current (cancellation/cancel-reason token-b)))
          (expect (= 3 (state/cancel-all-running!)))
          (expect (= :gateway-shutdown (cancellation/cancel-reason token-c)))
          ;; The shutdown sweep does not relabel the two already-attributed
          ;; cancels.
          (expect (= :client-cancel-turn (cancellation/cancel-reason token-a)))
          (expect (= :client-cancel-current (cancellation/cancel-reason token-b))))))))

;; Regression: the tid-less cancel (`POST /v1/sessions/:sid/cancel-current`) fired
;; the cancel token of WHATEVER turn held `:current-turn`, with no proof the caller
;; had ever submitted it. A session is shared, so opening it in a second channel —
;; the TUI, started while the companion app was already working in that session —
;; let one client's Esc kill the OTHER client's running turn.
(defdescribe cancel-current-owner-test
             (it "cancels only the running turn the caller itself submitted"
                 (let [sid
                       (str "cancel-current-owner-" (java.util.UUID/randomUUID))

                       token
                       (cancellation/cancellation-token)

                       registry
                       (atom {sid {:current-turn "companion"
                                   :turns {"companion" {:turn_id "companion"
                                                        :status "running"
                                                        :idempotency_key "companion:1"
                                                        :cancel-token token}}}})]

                   (with-redefs-fn {#'state/registry registry}
                     (fn []
                       ;; A second channel's correlation id names a turn this session is not
                       ;; running: refuse, and leave the companion's turn alone.
                       (expect (= {:error :not-owner :turn_id "companion"}
                                  (state/cancel-current-turn! sid "tui:2")))
                       (expect (nil? (cancellation/cancel-reason token)))
                       ;; No correlation id at all is not a licence to cancel anything.
                       (expect (= {:error :not-owner :turn_id "companion"}
                                  (state/cancel-current-turn! sid nil)))
                       (expect (nil? (cancellation/cancel-reason token)))
                       ;; The submitter's own key still gets the tid-less cancel it needs.
                       (expect (= "companion"
                                  (:turn_id (state/cancel-current-turn! sid "companion:1"))))
                       (expect (= :client-cancel-current (cancellation/cancel-reason token))))))))

;; Regression: pressing Stop wrote a SETTLED durable turn row for the turn that
;; was still live on the wire — status :cancelled (persisted as "interrupted"),
;; 0 iterations, empty content — so every client that refetched the transcript
;; painted a finished "Cancelled by user." row NEXT TO the live bubble that was
;; still cancelling: the same request rendered twice, and a spinner outliving its
;; own answer. The sweep was not even scoped to the cancelled turn — it stamped
;; EVERY :running row of the session.
(defdescribe
  cancel-persists-no-turn-row-test
  "A user cancel touches NO durable turn row. The wire terminal is the only thing
   that settles a turn; a process killed mid-cancel is repaired at startup by
   `loop/db-sweep-orphaned-running-turns!`, which owns exactly that case."
  (it "writes nothing durable while the cancelled turn is still live"
      (let [sid
            (str "cancel-persist-" (java.util.UUID/randomUUID))

            registry
            @#'state/registry

            writes
            (atom [])

            token
            (cancellation/cancellation-token)]

        (try (swap! registry assoc
               sid
               {:current-turn "t1"
                :turns {"t1" {:turn_id "t1" :status "running" :cancel-token token}
                        "t2" {:turn_id "t2" :status "running"}}})
             (with-redefs-fn {(resolve 'com.blockether.vis.internal.loop/db-info) (constantly
                                                                                    {:fake-db true})
                              #'persistance/db-list-session-turns
                              (fn [_ _]
                                [{:id "row-1" :status :running :iteration-count 0 :duration-ms 0}
                                 {:id "row-2" :status :running :iteration-count 7 :duration-ms 42}])
                              #'persistance/db-update-session-turn! (fn [_ id patch]
                                                                      (swap! writes conj [id patch])
                                                                      nil)}
               #(expect (= {:status "cancelling"} (state/cancel-turn! sid "t1"))))
             (expect (= [] @writes))
             (finally (swap! registry dissoc sid))))))

(defdescribe
  delete-session-teardown-test
  ;; Regression: DELETE /v1/sessions/:id ran the ENTIRE live-session teardown on
  ;; the request thread — `resources/stop-all!` (background shells, managed
  ;; REPLs, each stop-fn free to block) and then `lp/close!` (up to 5s waiting on
  ;; the turn lock before disposing the polyglot Context). Deleting a session the
  ;; user had just worked in held the HTTP response for seconds, and the
  ;; companion's non-dismissable "Deleting..." modal froze the whole sessions
  ;; screen behind it. The delete must cost the DB removal, nothing more.
  (it
    "returns once the session row is gone and tears the live runtime down off the request thread"
    (let [sid
          (str "delete-teardown-" (java.util.UUID/randomUUID))

          registry
          @#'state/registry

          deleted
          (atom [])

          stopped
          (promise)

          closed
          (promise)

          release
          (promise)]

      (try (swap! registry assoc sid {:turns {}})
           (with-redefs-fn {(requiring-resolve 'com.blockether.vis.internal.resources/stop-all!)
                            (fn [_sid]
                              ;; a slow stop-fn: a background shell that takes its time dying
                              (deref release 2000 :timeout)
                              (deliver stopped true))
                            #'lp/close! (fn [_sid]
                                          (deliver closed true))
                            #'lp/db-info (constantly :db)
                            (requiring-resolve
                              'com.blockether.vis.internal.workspace/discard-session-clones!)
                            (fn [_db _sid]
                              nil)
                            #'persistance/db-delete-session-tree! (fn [_db id]
                                                                    (swap! deleted conj id))}
             (fn []
               (let [started
                     (System/nanoTime)

                     fut
                     (state/close-session! sid)

                     elapsed-ms
                     (/ (- (System/nanoTime) started) 1e6)]

                 (expect (< elapsed-ms 500) (str "close-session! blocked for " elapsed-ms "ms"))
                 ;; the row is already gone everywhere a client can look
                 (expect (= [sid] @deleted))
                 (expect (not (contains? @registry sid)))
                 (deliver release true)
                 (when (instance? java.util.concurrent.Future fut)
                   (.get ^java.util.concurrent.Future fut))
                 (expect (true? (deref stopped 5000 false)))
                 (expect (true? (deref closed 5000 false))))))
           (finally (deliver release true) (swap! registry dissoc sid))))))

;; Wedged session: `turn.started` with no terminal (issue #105)

(defdescribe
  turn-launch-throw-lands-terminal-test
  ;; Regression, issue #105: `turn.started` was appended OUTSIDE the launch's
  ;; try/catch and the stall watchdog was armed only AFTER it, so any throw in
  ;; the announcement (or anywhere before the worker future existed) escaped
  ;; into an HTTP handler or a Throwable-swallowing daemon thread. The turn was
  ;; already PUBLIC and `:current-turn` already pointed at it, but nothing could
  ;; ever finish it: no worker, no watchdog, no terminal. The session showed an
  ;; empty assistant row forever, every later message queued behind it, and
  ;; re-sending the request just piled another turn onto the same dead pin.
  (it
    "arms the watchdog first and fails the turn when the launch itself throws"
    (let [sid
          (str "launch-throw-" (java.util.UUID/randomUUID))

          tid
          "wedged"

          registry
          @#'state/registry

          token
          (cancellation/cancellation-token)

          armed
          (atom [])

          events
          (atom [])]

      (try (swap! registry assoc
             sid
             {:current-turn tid :turns {tid {:turn_id tid :status "running" :cancel-token token}}})
           (with-redefs-fn {#'state/start-turn-stall-watchdog! (fn [_sid t _token _stall]
                                                                 (swap! armed conj t)
                                                                 nil)
                            #'state/append-event! (fn [_sid type _payload & _opts]
                                                    (swap! events conj type)
                                                    (when (= type "turn.started")
                                                      (throw (ex-info "wire fan-out blew up" {})))
                                                    nil)
                            #'state/finish-turn! (fn [_sid _tid _patch]
                                                   nil)
                            #'state/emit-context-updated! (fn [_sid]
                                                            nil)
                            #'state/after-turn-terminal! (fn [_sid _tid _opts]
                                                           nil)}
             (fn []
               ;; the launch must never propagate: its callers swallow Throwables
               (expect (nil? (#'state/launch-turn-worker! sid tid "hello" {:cancel-token token})))))
           ;; armed BEFORE the turn was announced, so the orphan window is covered
           (expect (= [tid] @armed))
           ;; and the turn ends in a terminal instead of pinning the session
           (expect (= ["turn.started" "turn.failed"] @events))
           (finally (swap! registry dissoc sid) (#'state/release-turn-terminal-claim! sid tid))))))

(defdescribe
  cancel-turn-backstop-test
  ;; Regression, issue #105: `cancel-turn!` only fired the cancellation token.
  ;; A token reaches a turn ONLY through a hook the worker registered, so a turn
  ;; whose launch never got that far — or whose worker is parked in
  ;; uninterruptible native code — ignored the stop completely and kept
  ;; `:current-turn` forever. Esc did nothing, the session could not be freed by
  ;; hand, and the backlog never drained.
  (it
    "arms a terminal backstop so a stop always resolves the turn"
    (let [sid
          (str "cancel-backstop-" (java.util.UUID/randomUUID))

          tid
          "stuck"

          registry
          @#'state/registry

          token
          (cancellation/cancellation-token)

          armed
          (atom nil)]

      (try (swap! registry assoc
             sid
             {:current-turn tid :turns {tid {:turn_id tid :status "running" :cancel-token token}}})
           (with-redefs-fn {#'state/start-cancel-terminal-backstop!
                            (fn [s t tok grace-ms land!]
                              (reset! armed
                                {:sid s :tid t :token tok :grace-ms grace-ms :land land!})
                              nil)}
             (fn []
               (expect (= {:status "cancelling"} (state/cancel-turn! sid tid :test-stop)))))
           (expect (cancellation/cancelled? token))
           (expect (= sid (:sid @armed)))
           (expect (= tid (:tid @armed)))
           (expect (identical? token (:token @armed)))
           (expect (pos? (long (:grace-ms @armed))))
           (expect (ifn? (:land @armed)))
           (finally (swap! registry dissoc sid))))))


(defdescribe
  stall-force-cancel-does-not-replay-test
  ;; Regression, issue #105: a watchdog cancellation used to mark the turn failed,
  ;; then re-queue and auto-resume that same user request. One provider failure
  ;; therefore appeared as a duplicate user bubble and triggered another provider
  ;; call without any new user action.
  (it
    "lands the stall once and holds later queued work without replaying it"
    (let [sid
          (str "stall-backstop-" (java.util.UUID/randomUUID))

          registry
          @#'state/registry

          token
          (cancellation/cancellation-token)

          events
          (atom [])

          launched
          (atom [])]

      (cancellation/cancel! token :stall-watchdog)
      (try (swap! registry assoc
             sid
             {:next-seq 0
              :current-turn "t1"
              :turn-order ["t1" "t2"]
              :turns {"t1" {:turn_id "t1" :status "running" :request "one" :cancel-token token}
                      "t2" {:turn_id "t2" :status "queued" :request "two" :queued_at 2}}})
           (with-redefs-fn {#'state/append-event! (fn [_sid type _payload & _opts]
                                                    (swap! events conj type)
                                                    nil)
                            #'state/emit-context-updated! (fn [_sid]
                                                            nil)
                            #'state/launch-turn-worker! (fn [_sid tid & _]
                                                          (swap! launched conj tid)
                                                          nil)}
             (fn []
               (#'state/cancel-waiting-turn! sid "t1" token)))
           ;; A watchdog force-cancel is one terminal failure, never a replay.
           (expect (= ["turn.failed" "queue.paused"] @events))
           (expect (= "failed" (get-in @registry [sid :turns "t1" :status])))
           (expect (= "one" (get-in @registry [sid :turns "t1" :request])))
           (expect (some? (get-in @registry [sid :queue-paused])))
           ;; The later, distinct request is held for an explicit resume.
           (expect (= [] @launched))
           (expect (= "queued" (get-in @registry [sid :turns "t2" :status])))
           (finally (swap! registry dissoc sid) (#'state/release-turn-terminal-claim! sid "t1"))))))

;; Recursive project delete. Until now DELETE of a project removed the row and
;; scattered its member sessions back to project-less, so there was no way at all
;; to remove a project AND its conversations — the one endpoint that sounded
;; right did the opposite of what a user asking to "remove the project" means.
(defdescribe
  recursive-project-delete-test
  (let [pid
        (java.util.UUID/randomUUID)

        a
        (java.util.UUID/randomUUID)

        b
        (java.util.UUID/randomUUID)

        exec
        (fn [opts]
          (let [log (atom [])]
            (with-redefs [lp/project-session-ids (fn [p]
                                                   (if (= pid p) [a b] []))
                          state/close-session! (fn [sid]
                                                 (swap! log conj [:session sid]))
                          lp/delete-project! (fn [p]
                                               (swap! log conj [:project p]))]

              {:result (state/delete-project! pid opts) :log @log})))]

    (it "keeps the scatter contract by default: the row goes, not one conversation"
        (let [{:keys [result log]} (exec nil)]
          (expect (= [[:project pid]] log))
          (expect (= 0 (:session_count result)))
          (expect (= [] (:deleted_session_ids result)))))
    (it "deletes every member session BEFORE the project row, and names the ids"
        ;; Sessions first: an interrupted teardown must leave a project
        ;; holding survivors, never orphaned sessions with a dead parent.
        (let [{:keys [result log]} (exec {:is-recursive true})]
          (expect (= [[:session a] [:session b] [:project pid]] log))
          (expect (= [(str a) (str b)] (:deleted_session_ids result)))
          (expect (= 2 (:session_count result)))
          (expect (= (str pid) (:project_id result)))))))

;; Regression, issue #105: a stalled provider turn was re-queued automatically,
;; and the blocking submit later received a bare `ERROR: turn failed` instead of
;; the styled failure. The terminal event must carry that content itself.
(defdescribe
  terminal-event-settled-failure-test
  (it
    "returns the settled failure card from the terminal turn"
    (let [sid
          (str "terminal-failure-" (java.util.UUID/randomUUID))

          tid
          "t-stalled"

          reason
          "Provider stream stalled: no output"

          registry
          @#'state/registry]

      (try (swap! registry assoc
             sid
             {:next-seq 0
              :current-turn tid
              :turn-order [tid]
              :turns {tid {:turn_id tid :status "running" :role "assistant"}}})
           (#'state/finish-turn!
            sid
            tid
            {:status "failed"
             :role "assistant"
             :content [(content/error "provider_stalled" reason false)]
             :error reason
             :completed_at 1})
           (let [event
                 (wire/->wire (assoc (#'state/turn-terminal-payload sid tid "failed")
                                :type "turn.failed"
                                :session_id sid))

                 result
                 (#'state/terminal-event->result event tid)]

             (expect (= reason (get result "error")))
             (expect (= 1 (count (get result "content"))))
             (expect (= "failed" (:status (get-in @registry [sid :turns tid])))))
           (finally (swap! registry dissoc sid) (#'state/release-turn-terminal-claim! sid tid))))))

(defdescribe
  attachment-listing-is-lean-and-filtered-test
  "Artifacts are DESCRIBED cheaply and FETCHED lazily. The list a human is shown
   is computed once — the descriptors number it, the byte endpoint indexes it,
   and history repeats it — and listing it never touches a byte of payload."
  ;; Regression: history disagreed with the live frame and with the byte
  ;; endpoint. `transcript-turn` numbered the RAW persisted rows, so a
  ;; model-only artifact reappeared in the transcript AND pushed every later
  ;; index off the artifact the endpoint serves at that index. And every one of
  ;; those listings read (and base64-encoded) every blob of the iteration just
  ;; to report metadata, so a 9-image gallery cost 81 image reads.
  (it "history drops model-only artifacts and renumbers, exactly like the byte endpoint"
      (let [rows
            [{:id "a" :filename "MODEL-ONLY.png" :audience "model" :size 1}
             {:id "b" :filename "SHOWN-1.png" :audience "both" :size 2}
             {:id "c" :filename "SHOWN-2.png" :audience "user" :size 3}]

            byte-lister-calls
            (atom 0)]

        (with-redefs-fn {#'persistance/db-list-session-turn-iterations (fn [_ _]
                                                                         [{:id "iter-1"}])
                         #'persistance/db-list-iterations-attachments (fn [_ _]
                                                                        (swap! byte-lister-calls
                                                                          inc)
                                                                        {"iter-1" rows})
                         #'persistance/db-list-iterations-attachments-meta (fn [_ _]
                                                                             {"iter-1" rows})}
          (fn []
            (let [turn
                  (#'state/transcript-turn ::db {} {:id "turn-1" :position 1})

                  atts
                  (:attachments (first (:iterations turn)))]

              (expect (= ["SHOWN-1.png" "SHOWN-2.png"] (mapv :filename atts)))
              (expect (= [0 1] (mapv :index atts)))
              ;; …and the page never asked for the bytes it does not ship.
              (expect (zero? @byte-lister-calls)))))))
  (it
    "lists metadata only and fetches ONE artifact's bytes, by its own id"
    (let [rows
          [{:id "a" :filename "hidden.png" :audience "model" :size 1 :has-bytes true}
           {:id "b" :filename "shown.png" :audience "both" :size 2 :has-bytes true}]

          meta-calls
          (atom 0)

          byte-lister-calls
          (atom 0)

          reads
          (atom [])]

      (with-redefs-fn {#'lp/db-info (constantly ::db)
                       #'persistance/db-list-iteration-attachments (fn [_ _]
                                                                     (swap! byte-lister-calls inc)
                                                                     rows)
                       #'persistance/db-list-iteration-attachments-meta (fn [_ _]
                                                                          (swap! meta-calls inc)
                                                                          rows)
                       #'persistance/db-read-attachment (fn [_ id]
                                                          (swap! reads conj id)
                                                          {:base64 (.encodeToString
                                                                     (java.util.Base64/getEncoder)
                                                                     (.getBytes "PNG" "UTF-8"))})}
        (fn []
          (let [user (state/user-iteration-attachments "00000000-0000-0000-0000-0000000000ab")]
            (expect (= ["shown.png"] (mapv :filename user)))
            (expect (= 1 @meta-calls))
            (expect (zero? @byte-lister-calls))
            ;; Bytes arrive only when asked for, and only that row's own.
            (expect (= "PNG" (String. ^bytes (state/attachment-bytes (first user)) "UTF-8")))
            (expect (= ["b"] @reads))
            ;; TOTAL: a corrupt payload or a vanished row is a clean nil (404),
            ;; never a decoder exception thrown out of the byte endpoint.
            (expect (nil? (state/attachment-bytes {:base64 "!!! not base64 !!!"})))
            (expect (nil? (state/attachment-bytes {})))))))))

;; Regression, issue #112: a GitHub-Copilot stream went silent and the turn failed with
;; "Provider stream stalled: no output for 362142ms in phase :provider-call" — the error
;; block never said WHICH provider or model had gone quiet, so the TUI could only paint a
;; bare failed card while the log knew it was github-copilot-enterprise / claude-opus-5.
(defdescribe
  stalled-turn-names-its-provider-test
  (let [advance
        @#'state/advance-turn-stall-state

        failure-text
        @#'state/stall-failure-text]

    (it "remembers the provider and model the call was dispatched to"
        (let [dispatched
              (advance {}
                       {:phase :provider-call
                        :iteration 0
                        :started-at-ms 1
                        :provider "github-copilot-enterprise"
                        :model "claude-opus-5"}
                       100)

              ;; later chunks carry no attribution of their own and must not erase it
              streaming
              (advance dispatched {:phase :content :delta "hi"} 200)]

          (expect (= "github-copilot-enterprise" (:provider dispatched)))
          (expect (= "claude-opus-5" (:model dispatched)))
          (expect (= "github-copilot-enterprise" (:provider streaming)))
          (expect (= "claude-opus-5" (:model streaming)))))
    (it "names them in the failure a human reads"
        (expect (= (str "Provider stream stalled (github-copilot-enterprise / claude-opus-5): "
                        "no output for 362142ms in phase :provider-call")
                   (failure-text (atom {:stall-detail
                                        "no output for 362142ms in phase :provider-call"
                                        :provider :github-copilot-enterprise
                                        :model "claude-opus-5"})))))
    (it "refuses to blame a provider the turn never reached"
        (expect (= "Turn stalled before reaching the provider: no worker activity for 5ms"
                   (failure-text (atom {:stall-detail "no worker activity for 5ms"})))))))

;; Regression, issue #128: a session was keyed by whatever spelling the caller
;; held, so a producer with the id as a STRING (gateway.human-input reads it off
;; the request map)
;; appended under that spelling and opened a GHOST entry — its own seq counter,
;; its own replay ring and NO subscribers. `human_input.request` reached the
;; journal and never the SSE fan-out, so the OTP form was never drawn.
(defdescribe registry-sid-spelling-test
             (it "delivers an event appended under the STRING sid to a UUID subscriber"
                 (let [sid
                       (random-uuid)

                       seen
                       (atom [])]

                   (state/subscribe! sid "sub-1" #(swap! seen conj %) 0)
                   (try (state/append-event! (str sid) "human_input.request" {:request {:id "r1"}})
                        (expect (= ["human_input.request"] (mapv #(get % "type") @seen)))
                        (expect (= ["human_input.request"]
                                   (mapv #(get % "type") (state/events-since sid 0))))
                        (finally (state/unsubscribe! sid "sub-1")))))
             (it "reads one session's state under either spelling"
                 (let [sid
                       (random-uuid)

                       seen
                       (atom [])]

                   (state/subscribe! (str sid) "sub-2" #(swap! seen conj %) 0)
                   (try (state/append-event! sid "human_input.request" {:request {:id "r2"}})
                        (expect (= ["human_input.request"] (mapv #(get % "type") @seen)))
                        (expect (= (state/current-seq sid) (state/current-seq (str sid))))
                        (expect (= 1 (count (state/events-since (str sid) 0))))
                        (finally (state/unsubscribe! (str sid) "sub-2"))))))

;; Regression, session e8c9dbc9-388d-43a4-8264-9dd5adec4449: the message typed into a
;; wedged session queued, TOOK its execution permit, then parked on that session's dead
;; engine lock — but the stall state still read `:awaiting-permit`, the stamp the worker
;; writes BEFORE it queues. So the turn kept the queueing exemption from the first-output
;; ceiling, sat six minutes on a spinner, and died as "Provider stream stalled: no output
;; for 360047ms in phase :awaiting-permit" — naming a provider it had never contacted and
;; a phase it had already left.
(defdescribe
  stall-before-the-provider-test
  (let [failure-text @#'state/stall-failure-text]
    (it "leaves :awaiting-permit behind the moment the permit is in hand"
        (let [registry @#'state/registry
              sid (str "engine-start-" (java.util.UUID/randomUUID))
              tid "turn-1"
              seen (promise)]

          (try (swap! registry assoc
                 sid
                 {:next-seq 0
                  :current-turn tid
                  :turns {tid {:turn_id tid :session_id sid :status "running" :started_at 123}}})
               (with-redefs-fn {#'state/append-event! (fn [& _]
                                                        nil)
                                #'state/run-turn! (fn [_sid _tid _request {:keys [stall]}]
                                                    (deliver seen (:phase @stall))
                                                    {:status :ok})}
                 (fn []
                   (#'state/launch-turn-worker!
                    sid
                    tid
                    "hello"
                    {:cancel-token (cancellation/cancellation-token)})
                   (expect (= :engine-start (deref seen 4000 ::timeout)))))
               (finally (swap! registry dissoc sid)))))
    (it "keeps naming the provider once a stream had actually produced output"
        (expect (= "Provider stream stalled: no output for 362142ms in phase :content"
                   (failure-text (atom {:stall-detail "no output for 362142ms in phase :content"
                                        :produced? true})))))
    (it
      "says what a turn wedged before its first provider call really did"
      (expect
        (=
          "Turn stalled before reaching the provider: no output for 360047ms in phase :engine-start"
          (failure-text (atom {:stall-detail "no output for 360047ms in phase :engine-start"})))))))

(defdescribe
  cross-process-liveness-test
  "What `/v1/sessions` calls `live` has to describe the MACHINE, because that is
   what the question means to a client: this gateway is the one authority two apps
   share. `soul` therefore falls back to the bus's cross-process index for a turn
   this process is not running itself."
  ;; Regression: a desktop app and a phone reading the SAME gateway showed
  ;; different sets of live sessions. `live` came only from this process's
  ;; in-memory registry, which learns about a sibling process's turn just when
  ;; somebody SSE-subscribes to that session — so each app lit up exactly the
  ;; sessions it had happened to open, and neither ever saw the whole fleet.
  (it "reports a turn running in a SIBLING process, with nothing subscribed here"
      (with-redefs [lp/by-id
                    (constantly {:id "s-foreign" :channel :api :title "t"})

                    lp/db-info
                    (constantly nil)

                    persistance/db-session-turn-stats
                    (constantly nil)

                    ;; No subscriber, so this process's registry holds no entry for the
                    ;; session at all — the index is the only thing that knows.
                    bus/live-turns
                    (constantly {"s-foreign" "T-foreign"})]

        (let [row (state/soul "s-foreign")]
          (expect (true? (get row "live")))
          (expect (= "running" (get row "status")))
          (expect (= "T-foreign" (get row "current_turn_id"))))))
  (it "stays idle when no process on this machine is running the session"
      (with-redefs [lp/by-id
                    (constantly {:id "s-quiet" :channel :api :title "t"})

                    lp/db-info
                    (constantly nil)

                    persistance/db-session-turn-stats
                    (constantly nil)

                    bus/live-turns
                    (constantly {})]

        (let [row (state/soul "s-quiet")]
          (expect (false? (get row "live")))
          (expect (= "idle" (get row "status")))))))


(defdescribe
  silent-reasoning-opens-no-block-test
  "A reasoning block exists only when the provider actually said something.

   Vis used to open one per iteration on the first stream tick, empty or not:
   1009 of 1227 reasoning blocks in the session journals were
   `started -> \"\" delta -> completed` inside ~200ms — pure noise every consumer
   had to paint around. The first NON-BLANK tick opens the block and carries its
   text, so no consumer ever sees a delta for a block it was not told about."
  (it
    "opens no block for an iteration that only ticked empty reasoning"
    (let [sid
          (str (random-uuid))

          tid
          (str (random-uuid))]

      (swap! @#'state/registry assoc
        sid
        {:next-seq 0
         :events []
         :subscribers {}
         :turns {tid {:turn_id tid :status "running"}}
         :turn-order [tid]
         :current-turn tid})
      (with-redefs [lp/send!
                    (fn [_ _ opts]
                      (let [on-chunk (get-in opts [:hooks :on-chunk])]
                        ;; Iteration 1 thought nothing: empty ticks only.
                        (on-chunk {:phase :reasoning :iteration 1 :thinking ""})
                        (on-chunk {:phase :reasoning :iteration 1 :thinking "" :done? true})
                        (on-chunk {:phase :iteration-final :iteration 1 :thinking "" :done? false})
                        ;; Iteration 2 starts silent, then speaks.
                        (on-chunk {:phase :reasoning :iteration 2 :thinking ""})
                        (on-chunk {:phase :reasoning :iteration 2 :thinking "beta thought."})
                        (on-chunk {:phase :iteration-final
                                   :iteration 2
                                   :thinking "beta thought."
                                   :done? true}))
                      {:status :ok :answer nil})]
        (#'state/run-turn! sid tid "hi" {}))
      (let [events
            (:events (get @@#'state/registry sid))

            of-type
            (fn [type]
              (filterv #(= type (get % "type")) events))

            started
            (of-type "content.block.started")

            deltas
            (of-type "content.block.delta")]

        ;; Iteration 1 never became a block, and neither did iteration 2's
        ;; silent lead-in.
        (expect (= 1 (count started)))
        (expect (str/ends-with? (str (get-in (first started) ["block" "id"])) ":reasoning:2"))
        ;; Every delta belongs to a block the consumer was told about, and none
        ;; of them is empty.
        (expect (seq deltas))
        (expect (= #{(get-in (first started) ["block" "id"])}
                   (set (map #(get % "block_id") deltas))))
        (expect (every? #(seq (str (get % "text"))) deltas))
        ;; …and the block that DID open still closes exactly once.
        (expect (= (count started) (count (of-type "content.block.completed"))))))))

;; Regression: `iteration.completed` shipped the provider's CUT summary as the
;; iteration's thinking, so every surface painted a two-word stub — `So the
;; issue…` on an iteration where the model had spent 24.5s and 2056 tokens.
(defdescribe
  cut-summary-thinking-test
  (it "clips a settled summary at the last sentence it closed"
      (let [[type _ payload] (#'state/chunk->event
                              {:phase :iteration-final
                               :done? true
                               :iteration 1
                               :thinking "Checked the parser. I need…"})]
        (expect (= "iteration.completed" type))
        (expect (= "Checked the parser." (:thinking payload)))))
  (it "ships a punctuation-free Markdown heading as settled thinking"
      (let [[_ _ payload]
            (#'state/chunk->event
             {:phase :iteration-final :done? true :iteration 1 :thinking "**Planning**"})]
        (expect (= "**Planning**" (:thinking payload)))))
  (it "ships no thinking when the provider cut the summary mid-clause"
      (let [[_ _ payload]
            (#'state/chunk->event
             {:phase :iteration-final :done? true :iteration 1 :thinking "So the issue…"})]
        (expect (nil? (:thinking payload)))))
  (it "does the same on the iteration's error boundary"
      (let [[_ _ payload] (#'state/chunk->event
                           {:phase :iteration-error
                            :iteration 1
                            :thinking "I found…"
                            :error {:type :provider :message "boom"}})]
        (expect (nil? (:thinking payload))))))


;; Regression, user report (paraphrased: switching gateways flickered the project
;; list and then the numbers inside it): the counts were derived client-side from
;; downloaded session windows, so they settled page by page.
(defdescribe
  projects-overview-test
  "One gateway-owned answer for the navigator header: a project's counts must not
   be a client-side tally of a downloaded fleet."
  (it
    "groups the fleet by project root and counts sessions, live and awaiting"
    (let [roots {"s1" "/repo/a" "s2" "/repo/a" "s3" "/repo/b"}]
      (with-redefs-fn {#'lp/db-info (constantly ::db)
                       #'lp/projects (fn [_]
                                       [{:id "p-a" :name "Vis" :workspace-root "/repo/a"}])
                       #'persistance/db-session-turn-stats
                       (fn [_]
                         {"s1" {:latest-turn-at 300 :turn-count 2}
                          "s2" {:latest-turn-at 100 :turn-count 1}
                          "s3" {:latest-turn-at 200 :turn-count 1}})
                       #'lp/by-channel (fn [_]
                                         [{:id "s1"} {:id "s2"} {:id "s3"}])
                       #'state/session-project-root (fn [_ sid]
                                                      (get roots (str sid)))
                       #'bus/live-turns (constantly {"s1" "t1"})
                       #'bus/waiting-requests (constantly {"s3" {}})}
        (fn []
          (let [overview (state/projects-overview)
                by-root (into {}
                              (map (fn [p]
                                     [(get p "root") p]))
                              (:projects overview))]

            ;; Freshest project first — liveness is a COUNT here, never a band.
            (expect (= ["/repo/a" "/repo/b"] (mapv #(get % "root") (:projects overview))))
            (expect (= "Vis" (get-in by-root ["/repo/a" "name"])))
            (expect (= "p-a" (get-in by-root ["/repo/a" "project_id"])))
            (expect (= 2 (get-in by-root ["/repo/a" "session_count"])))
            (expect (= 1 (get-in by-root ["/repo/a" "live_count"])))
            (expect (= 300 (get-in by-root ["/repo/a" "last_activity_ms"])))
            ;; A root nothing named still IS a project group; the folder is the
            ;; client's own fallback label.
            (expect (= "" (get-in by-root ["/repo/b" "name"])))
            (expect (= 1 (get-in by-root ["/repo/b" "awaiting_count"])))
            (expect (= {:project_count 2 :session_count 3 :live_count 1 :awaiting_count 1}
                       (select-keys overview
                                    [:project_count :session_count :live_count
                                     :awaiting_count]))))))))
  ;; Regression, user report: `Use project` persisted an empty project, but the
  ;; overview omitted it until a session existed, so the companion could not show it.
  (it "keeps a persisted project visible before its first session"
      (with-redefs-fn {#'lp/db-info (constantly ::db)
                       #'lp/projects
                       (fn [_]
                         [{:id "p-empty" :name "Empty" :workspace-root "/repo/empty"}])
                       #'persistance/db-session-turn-stats (constantly {})
                       #'lp/by-channel (constantly [])
                       #'bus/live-turns (constantly {})
                       #'bus/waiting-requests (constantly {})}
        (fn []
          (let [overview (state/projects-overview)]
            (expect (= 1 (:project_count overview)))
            (expect (= [{"root" "/repo/empty"
                         "project_id" "p-empty"
                         "name" "Empty"
                         "session_count" 0
                         "live_count" 0
                         "awaiting_count" 0
                         "last_activity_ms" 0}]
                       (:projects overview)))))))
  (it "answers empty totals, never an exception, when nothing is persisted"
      (with-redefs-fn {#'lp/db-info (constantly nil)
                       #'lp/by-channel (constantly [])
                       #'bus/live-turns (constantly {})
                       #'bus/waiting-requests (constantly {})}
        (fn []
          (let [overview (state/projects-overview)]
            (expect (= [] (:projects overview)))
            (expect (= 0 (:session_count overview))))))))

;; Regression, reported in this Vis session: a session list learned that a run
;; started, parked on a human or ended ONLY by re-reading its whole window on a
;; timer — a full window per tick to discover that nothing had moved, and a stale
;; row until the next one.
(defdescribe
  fleet-status-deltas-test
  "The fleet feed's entire vocabulary: one frame per session whose state changed,
   and nothing at all while the busy fleet stands still."
  (it "reads the fleet from the machine's markers, never from this process's registry"
      (with-redefs-fn {#'bus/live-turns (constantly {"s1" "t1"})
                       #'bus/waiting-requests (constantly {"s2" [{"id" "r1"}]})}
        (fn []
          (let [snapshot (state/fleet-snapshot)]
            (expect (= {"is_live" true "is_awaiting_input" false "current_turn_id" "t1"}
                       (get snapshot "s1")))
            (expect (= {"is_live" false "is_awaiting_input" true "current_turn_id" nil}
                       (get snapshot "s2")))))))
  (it "says nothing at all while no session changed"
      (let [snapshot {"s1" {"is_live" true "is_awaiting_input" false "current_turn_id" "t1"}}]
        (expect (= [] (state/fleet-status-frames snapshot snapshot)))))
  (it "announces a session that started running"
      (expect (= [{"schema" 1
                   "type" "session.status"
                   "session_id" "s1"
                   "is_live" true
                   "is_awaiting_input" false
                   "current_turn_id" "t1"}]
                 (state/fleet-status-frames
                   {}
                   {"s1" {"is_live" true "is_awaiting_input" false "current_turn_id" "t1"}}))))
  (it "spells out the session that left both indexes, because absence cannot be painted"
      (expect (= [{"schema" 1
                   "type" "session.status"
                   "session_id" "s1"
                   "is_live" false
                   "is_awaiting_input" false
                   "current_turn_id" nil}]
                 (state/fleet-status-frames
                   {"s1" {"is_live" true "is_awaiting_input" false "current_turn_id" "t1"}}
                   {}))))
  (it "reports the run that parked on a human without repeating the sessions around it"
      (let [frames (state/fleet-status-frames
                     {"s1" {"is_live" true "is_awaiting_input" false "current_turn_id" "t1"}
                      "s2" {"is_live" true "is_awaiting_input" false "current_turn_id" "t2"}}
                     {"s1" {"is_live" true "is_awaiting_input" true "current_turn_id" "t1"}
                      "s2" {"is_live" true "is_awaiting_input" false "current_turn_id" "t2"}})]
        (expect (= 1 (count frames)))
        (expect (= "s1" (get (first frames) "session_id")))
        (expect (true? (get (first frames) "is_awaiting_input"))))))
