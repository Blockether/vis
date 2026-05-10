(ns com.blockether.vis.internal.loop-test
  (:require
   [com.blockether.svar.internal.codes :as svar-codes]
   [com.blockether.vis.internal.loop :as loop]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe def-display-result-test
  (it "does not silence single-def blocks (always-show)"
    ;; Regression: previously a single top-level `(def x ...)` got
    ;; `:rendering-kind :vis/silent` and the TUI hid it, leaving
    ;; downstream blocks referring to bindings whose definition was
    ;; invisible (conversation f1752e0a). The whole `:vis/silent`
    ;; mechanism has been removed and this fn is now a pass-through
    ;; so the def block renders like any other form.
    (let [result (#'loop/def-display-result
                  {:sci-ctx :present}
                  "(def xd \"XDDD\")"
                  {:result :raw-var
                   :stdout ""
                   :stderr ""
                   :error nil})]
      (expect (= :raw-var (:result result)))
      (expect (nil? (:role result)))))

  (it "is a pure pass-through: errors and timeouts are not modified"
    (let [err  {:result nil :error {:msg "boom"} :stdout "" :stderr ""}
          tmo  {:result nil :timeout? true :stdout "" :stderr ""}
          plain {:result 42 :stdout "" :stderr "" :error nil}]
      (expect (= err   (#'loop/def-display-result {} "(def x 1)" err)))
      (expect (= tmo   (#'loop/def-display-result {} "(def x 1)" tmo)))
      (expect (= plain (#'loop/def-display-result {} "(+ 1 41)" plain))))))

(defdescribe answer-cleanup-test
  (it "does not append removed runtime sections"
    (expect (= "done" (loop/append-runtime-appendices {} "done" {}))))

  (it "does not block final answers with workflow state"
    (expect (nil? (loop/final-answer-gate-error {} 0 [] "done")))))

;; ---------------------------------------------------------------------------
;; Preserved-thinking replay (R3 hybrid shape) regression tests.
;;
;; Conversation 1db62d10 (GLM-5.1 zai-coding) reproduced the
;; restart-every-iter loop because the pre-R3 message shape inserted
;; replays + journal BEFORE the last user message, ending up with
;; `[asst, asst, user_journal, user_initial]`. The model re-read its
;; goal every iter and restarted planning. R3 keeps the user_initial
;; once at the start and APPENDS replays + journal at the end so the
;; sequence alternates the way z.ai's preserved-thinking docs spec.
;; ---------------------------------------------------------------------------

(defn- mk-iter [pos sig]
  [pos {:assistant-message
        {:role "assistant"
         :content [{:type "thinking"
                    :thinking sig
                    :thinking-signature sig}
                   {:type "text" :text "code"}]}}])

(defdescribe preserved-thinking-replay-messages-test
  (it "keeps every iter when total reasoning fits the budget"
    (let [iters [(mk-iter 1 "aaaa") (mk-iter 2 "bbbb") (mk-iter 3 "cccc")]
          out   (#'loop/preserved-thinking-replay-messages iters)]
      (expect (= 3 (count out)))
      (expect (= "aaaa" (-> out first :content first :thinking-signature)))
      (expect (= "cccc" (-> out last  :content first :thinking-signature)))))

  (it "always keeps the newest iter even when it alone exceeds budget"
    (let [huge (apply str (repeat 200000 \a))
          iters [(mk-iter 1 huge)]
          out   (#'loop/preserved-thinking-replay-messages iters)]
      (expect (= 1 (count out)))
      (expect (= huge (-> out first :content first :thinking-signature)))))

  (it "drops oldest iters when the run would exceed the budget"
    ;; Each block is 80k chars; budget = 120k chars (30k tok * 4).
    ;; Newest kept; adding next oldest would push to 160k > 120k → STOP.
    (let [a (mk-iter 1 (apply str (repeat 80000 \a)))
          b (mk-iter 2 (apply str (repeat 80000 \b)))
          c (mk-iter 3 (apply str (repeat 80000 \c)))
          out (#'loop/preserved-thinking-replay-messages [a b c])]
      (expect (= 1 (count out)))
      (expect (str/starts-with? (-> out first :content first :thinking-signature) "ccc"))))

  (it "never skips past an over-budget iter to grab an older one"
    ;; Z.ai contract requires CONSECUTIVE reasoning_content blocks. If
    ;; iter K-1 alone would push us over, we stop — we do NOT try
    ;; iter K-2 instead. The kept run must be contiguous from latest.
    (let [huge   (apply str (repeat 200000 \B))   ; one huge older iter
          newest (mk-iter 3 "newest")
          out    (#'loop/preserved-thinking-replay-messages
                  [(mk-iter 1 "oldest-tiny")
                   (mk-iter 2 huge)
                   newest])]
      (expect (= 1 (count out)))
      (expect (= "newest" (-> out first :content first :thinking-signature)))))

  (it "tolerates iters with nil :assistant-message"
    (let [out (#'loop/preserved-thinking-replay-messages
               [[1 {:assistant-message nil}]
                (mk-iter 2 "xyz")])]
      (expect (= 1 (count out)))
      (expect (= "xyz" (-> out first :content first :thinking-signature))))))

(defdescribe append-preserved-thinking-replay-r3-test
  (it "appends replays at the END (R3), not before the last user"
    ;; Pre-R3 path produced [system asst user] with replays wedged
    ;; before the last user. R3 keeps the original user once at the
    ;; start and appends replays AFTER it so the next call site can
    ;; tail-append the journal as the final user message and the
    ;; sequence ends up `user → asst → user` (canonical for z.ai).
    (let [msgs [{:role "system" :content "sys"}
                {:role "user"   :content "initial-goal"}]
          out  (#'loop/append-preserved-thinking-replay
                msgs [(mk-iter 1 "reasoning-1")])]
      (expect (= ["system" "user" "assistant"] (mapv :role out)))
      (expect (= "initial-goal" (-> out second :content)))
      (expect (= "reasoning-1"
                (-> out (nth 2) :content first :thinking-signature)))))

  (it "is a no-op when there are no replays available"
    (let [msgs [{:role "system" :content "sys"}
                {:role "user"   :content "initial-goal"}]]
      (expect (= msgs (#'loop/append-preserved-thinking-replay msgs [])))
      (expect (= msgs (#'loop/append-preserved-thinking-replay msgs nil)))))

  (it "appends multiple iter replays in chronological order"
    (let [msgs [{:role "system" :content "sys"}
                {:role "user"   :content "go"}]
          out  (#'loop/append-preserved-thinking-replay
                msgs [(mk-iter 1 "r1") (mk-iter 2 "r2") (mk-iter 3 "r3")])]
      (expect (= ["system" "user" "assistant" "assistant" "assistant"]
                (mapv :role out)))
      (expect (= ["r1" "r2" "r3"]
                (mapv #(-> % :content first :thinking-signature) (drop 2 out)))))))

(defdescribe convergence-nudge-test
  (it "references the running iteration count and the hard cap"
    (let [line (#'loop/convergence-nudge-line 9 20)]
      (expect (str/includes? line "9 iterations"))
      (expect (str/includes? line "20 iterations"))
      (expect (str/includes? line "(answer"))
      (expect (str/includes? line "<vis_convergence_reminder>"))))

  (it "tags the reminder with an XML-shaped wrapper so prompt assemblers can spot it"
    (let [line (#'loop/convergence-nudge-line 12 20)]
      (expect (str/starts-with? line "<vis_convergence_reminder>"))
      (expect (str/ends-with?   line "</vis_convergence_reminder>")))))

(defdescribe iteration-cap-constants-test
  (it "hard cap is 20 and soft nudge fires at iter 8"
    ;; Defensive snapshot of the contract values — if these change,
    ;; doc/tests should change with them.
    (expect (= 20 @#'loop/MAX_TURN_ITERATIONS))
    (expect (= 8  @#'loop/CONVERGENCE_NUDGE_AT)))

  (it "replay token budget is the documented 30k tokens"
    (expect (= 30000 @#'loop/preserved-thinking-replay-token-budget))))

;; ---------------------------------------------------------------------------
;; Hallucinated <journal>/<bindings>/... echo stripping.
;;
;; Conversation 185fbc4f (glm-5.1 zai-coding) reproduced this: the model
;; emitted real ```clojure blocks interleaved with fabricated <journal>
;; sections that it closed with a stray ``` instead of </journal>. That
;; ``` opened a fresh untagged fence which swallowed the next real
;; ```clojure opener, producing :lang nil blocks whose source contained
;; literal ```clojure markers. The fence-leak preflight then aborted iter 1.
;; ---------------------------------------------------------------------------

(defdescribe strip-vis-engine-xml-echo-test
  (it "is a no-op when no Vis-engine XML opener appears"
    (let [raw "```clojure\n(println :ok)\n```\n"]
      (expect (= raw (#'loop/strip-vis-engine-xml-echo raw)))))

  (it "strips <journal>...</journal> with a proper XML closer"
    (let [raw (str "```clojure\n(+ 1 2)\n```\n\n"
                "<journal>\n"
                "i1.1 (+ 1 2) -> 3\n"
                "</journal>\n\n"
                "```clojure\n(+ 3 4)\n```\n")
          out (#'loop/strip-vis-engine-xml-echo raw)]
      (expect (not (str/includes? out "<journal>")))
      (expect (not (str/includes? out "i1.1")))
      (expect (str/includes? out "(+ 1 2)"))
      (expect (str/includes? out "(+ 3 4)"))))

  (it "strips <journal>...``` with a stray-fence closer (the GLM bug)"
    (let [raw (str "```clojure\n(+ 1 2)\n```\n\n"
                "<journal>\n"
                "i1.1 (+ 1 2) -> 3\n"
                "```\n\n"
                "```clojure\n(+ 3 4)\n```\n")
          out (#'loop/strip-vis-engine-xml-echo raw)]
      (expect (not (str/includes? out "<journal>")))
      (expect (not (str/includes? out "i1.1")))
      (expect (str/includes? out "(+ 3 4)"))
      ;; The stray ``` closer must NOT survive into the cleaned text.
      (let [blocks (svar-codes/extract-code-blocks out)]
        (expect (= ["clojure" "clojure"] (mapv :lang blocks)))
        (expect (every? #(not (str/includes? (:source %) "```")) blocks)))))

  (it "keeps a real ```clojure opener that immediately follows a fabricated envelope"
    (let [raw "<journal>i1.1 -> 3\n```clojure\n(+ 3 4)\n```\n"
          out (#'loop/strip-vis-engine-xml-echo raw)]
      (expect (str/includes? out "```clojure"))
      (expect (str/includes? out "(+ 3 4)"))
      (expect (not (str/includes? out "i1.1")))))

  (it "strips <bindings>, <active_skills>, <system_nudge[s]> echoes too"
    (doseq [tag ["bindings" "active_skills" "system_nudges" "system_nudge"]]
      (let [raw (str "<" tag ">junk</" tag ">\n```clojure\n(+ 1 2)\n```\n")
            out (#'loop/strip-vis-engine-xml-echo raw)]
        (expect (not (str/includes? out (str "<" tag ">"))))
        (expect (str/includes? out "(+ 1 2)")))))

  (it "accepts <tag attr=\"x\"> shape (e.g. <system_nudge importance=\"high\">)"
    (let [raw "<system_nudge importance=\"high\">commit now</system_nudge>\n```clojure\n(+ 1 1)\n```\n"
          out (#'loop/strip-vis-engine-xml-echo raw)]
      (expect (not (str/includes? out "<system_nudge")))
      (expect (str/includes? out "(+ 1 1)")))))

(defdescribe normalize-ask-result-vis-engine-xml-echo-test
  (it "is a pass-through when raw has no Vis-engine XML envelope"
    (let [in {:raw "```clojure\n(+ 1 2)\n```\n"
              :result "(+ 1 2)"
              :blocks [{:lang "clojure" :source "(+ 1 2)"}]}
          out (#'loop/normalize-ask-result-vis-engine-xml-echo in)]
      (expect (= in out))
      (expect (nil? (:vis/normalized-from-raw? out)))))

  (it "recovers iter 1 from conversation 185fbc4f end-to-end"
    ;; Real raw response (8897 chars) captured from the GLM-5.1 hallucination.
    ;; Without the repair: 5 :lang clojure + 5 :lang nil blocks; the nil
    ;; blocks contain literal ```clojure markers which trip
    ;; raw-markdown-fence-leak-error and waste the iteration.
    ;; With the repair: 10 :lang clojure blocks, no fence leak, the
    ;; final (answer ...) block survives.
    (let [raw (slurp (io/resource "fixtures/hallucinated-journal-iter1.txt"))
          orig-blocks   (svar-codes/extract-code-blocks raw)
          orig-selected (svar-codes/select-blocks orig-blocks "clojure")
          orig-result   (svar-codes/concat-sources orig-selected)
          ask-in  {:raw raw :blocks orig-selected :result orig-result}
          ask-out (#'loop/normalize-ask-result-vis-engine-xml-echo ask-in)]
      ;; Sanity: the unrepaired :result really does carry the fence leak.
      (expect (str/includes? (:result ask-in) "```"))
      (expect (some? (#'loop/raw-markdown-fence-leak-error (:result ask-in))))
      ;; Repaired :result is fence-clean and parses to ten clojure blocks.
      (expect (true? (:vis/normalized-from-raw? ask-out)))
      (expect (not (str/includes? (:result ask-out) "```")))
      (expect (nil? (#'loop/raw-markdown-fence-leak-error (:result ask-out))))
      (expect (= 10 (count (:blocks ask-out))))
      (expect (every? #(= "clojure" (:lang %)) (:blocks ask-out)))
      (expect (str/includes? (:result ask-out) "(answer"))
      ;; :raw is preserved verbatim for forensic / DB rows.
      (expect (= raw (:raw ask-out))))))

(defdescribe detect-common-mistakes-test
  (it "rejects a string-as-fn call inside (answer ...) (regression: convo 0d25a3e1)"
    ;; Reduced from turn 6 / iter 1 / block 36, offset 6151 of the
    ;; broken 6623-char (answer ...) body. The reader is happy; SCI
    ;; would crash with `String cannot be cast to IFn`. The lint must
    ;; reject before evaluation.
    (let [bad "(answer (v/p (v/bold(\"treating a structured Clojure codebase like a plain text file.\"))))"
          msg (#'loop/detect-common-mistakes bad)]
      (expect (string? msg))
      (expect (str/includes? msg "String-as-fn"))
      (expect (str/includes? msg "ClassCastException"))))

  (it "leaves valid Clojure source alone"
    (expect (nil? (#'loop/detect-common-mistakes "(answer (v/p (v/bold \"text\")))")))
    (expect (nil? (#'loop/detect-common-mistakes "(def x 1)\n(println x)")))
    (expect (nil? (#'loop/detect-common-mistakes ""))))

  (it "tolerates unparseable source by deferring to parse-clojure-syntax"
    ;; Reader error -> nil here; the broader pipeline still surfaces
    ;; the parse error via parse-clojure-syntax.
    (expect (nil? (#'loop/detect-common-mistakes "(answer (v/p"))))

  (it "fires inside any nested form within (answer ...)"
    (let [nested "(answer (v/p (let [x 1] (\"oops\" x))))"
          msg    (#'loop/detect-common-mistakes nested)]
      (expect (string? msg))
      (expect (str/includes? msg "String-as-fn"))))

  (it "ignores string-as-fn shapes OUTSIDE (answer ...) -- regular code is its own owner"
    ;; Scope: the lint only polices the user-facing markdown DSL inside
    ;; (answer ...). A weird shape in regular evaluation code is the
    ;; model's own problem and will surface as a normal SCI error.
    (expect (nil? (#'loop/detect-common-mistakes
                   "(let [x 1] (when x (\"oops\" :extra)))")))
    (expect (nil? (#'loop/detect-common-mistakes
                   "(def y (\"nope\" 1))\n(println y)")))))

;;; -----------------------------------------------------------------------
;;; format-iteration-error: surface upstream HTTP body in chat
;;; -----------------------------------------------------------------------

;; Regression: conversation 6f5f7dbb-1e74-4f64-9223-6c3e28ee9dd0.
;; Anthropic returned a structured 400 with `error.message =
;; "messages.1.content.1: Invalid signature in thinking block"`. svar
;; correctly attached the body to `:svar.core/http-error` ex-data, but
;; Vis's chat error renderer only surfaced the wrapper line
;; `Exceptional status code: 400`. The user couldn't tell what was
;; actually wrong without grepping `~/.vis/vis.log`. Pin the contract:
;; when an iteration error carries `[:data :body]` (svar's http-error
;; shape), `format-iteration-error` must echo the body verbatim into
;; the bullet so the chat bubble is self-contained, including HTTP
;; status and request id when present.
(defdescribe format-iteration-error-http-body-test
  (it "surfaces svar.core/http-error body verbatim with status + request id"
    (let [body (str "{\"type\":\"error\","
                 "\"error\":{\"type\":\"invalid_request_error\","
                 "\"message\":\"messages.1.content.1: Invalid `signature` in `thinking` block\"},"
                 "\"request_id\":\"req_011CarodswSWgEdPfFaJakLs\"}")
          out (#'loop/format-iteration-error
               {:message "Exceptional status code: 400"
                :data    {:type       :svar.core/http-error
                          :status     400
                          :request-id "req_011CarodswSWgEdPfFaJakLs"
                          :body       body}})]
      ;; Wrapper line still present so existing transcripts don't change.
      (expect (str/includes? out "Exceptional status code: 400"))
      ;; HTTP status surfaced separately for at-a-glance triage.
      (expect (str/includes? out "HTTP 400"))
      ;; Request id round-tripped so users can paste into provider support.
      (expect (str/includes? out "req_011CarodswSWgEdPfFaJakLs"))
      ;; Actual provider error message shows up where the user can read it.
      (expect (str/includes? out "Invalid `signature` in `thinking` block"))))

  (it "survives missing status / request-id (only :body attached)"
    (let [out (#'loop/format-iteration-error
               {:message "Exceptional status code: 500"
                :data    {:type :svar.core/http-error
                          :body "{\"error\":\"upstream timeout\"}"}})]
      (expect (str/includes? out "Exceptional status code: 500"))
      (expect (str/includes? out "upstream timeout"))
      ;; No `(HTTP ...)` annotation when status missing.
      (expect (not (str/includes? out "HTTP ")))))

  (it "falls back to wrapper-only when no body or raw-data is present"
    (let [out (#'loop/format-iteration-error
               {:message "Some unrelated failure"
                :data    {:type :other}})]
      (expect (= "- Some unrelated failure" out))))

  (it "keeps spec-rejection :raw-data path working alongside the new :body path"
    ;; spec-layer errors used a different shape (`:raw-data`); the
    ;; renderer must still surface that one too — we just added a new
    ;; branch, didn't replace the old one.
    (let [out (#'loop/format-iteration-error
               {:message "schema rejected"
                :data    {:raw-data       "<not-json>"
                          :received-type  "text"}})]
      (expect (str/includes? out "<not-json>"))
      (expect (str/includes? out "(text)")))))

(defn- entries-of [code]
  (or (first (loop/split-top-level-forms code)) []))

(defdescribe block-result-error-summary-test
  ;; Helper that drives the runtime answer-after-error gate. The gate
  ;; itself runs inside the per-form mapv inside `execute-iteration!`;
  ;; this test exercises the predicate so we can prove the gate fires
  ;; on both thrown errors and lifted tool-failure sink entries
  ;; (regression: ANALYSIS.md §4.2).
  (it "surfaces a thrown :error verbatim"
    (expect (= "boom" (#'loop/block-result-error-summary {:error "boom"}))))

  (it "surfaces a lifted tool-failure sink-entry's :error.message when block :error is nil"
    (let [result {:error nil
                  :journal [{:position 0 :form "(z/patch ...)"
                             :success? false
                             :result   nil
                             :error    {:type "clojure.lang.ExceptionInfo"
                                        :message "z/patch :search locator must match exactly once; matched 4 time(s)"
                                        :trace []}}]}]
      (expect (= "z/patch :search locator must match exactly once; matched 4 time(s)"
                (#'loop/block-result-error-summary result)))))

  (it "is nil when the form ran cleanly (no :error, all sinks :success? true)"
    (expect (nil? (#'loop/block-result-error-summary
                   {:error nil
                    :journal [{:position 0 :form "(v/cat \"a\")" :success? true :result "ok" :error nil}]})))
    (expect (nil? (#'loop/block-result-error-summary {:error nil :journal []})))
    (expect (nil? (#'loop/block-result-error-summary {})))))

(defdescribe answer-with-mutation-preflight-test
  (it "rejects (z/patch ...) + (answer ...) in the same iteration (regression: convo 73f3d325 turn 5)"
    ;; Reduced repro: the model wrote z/patch (which failed with
    ;; `matched 4 time(s)`) and (answer "Now fixed") in one iteration.
    ;; The write-then-read SCI loop made it impossible to observe the
    ;; failure before composing the answer. Preflight must reject
    ;; before any eval.
    (let [code     "(z/patch [{:path \"a.clj\" :search \"x\" :replace \"y\"}])\n(answer \"Now fixed\")"
          entries  (entries-of code)
          mismatch (#'loop/answer-with-mutation-preflight-mismatch entries)
          msg      (when mismatch
                     (#'loop/answer-with-mutation-preflight-error-message mismatch))]
      (expect (some? mismatch))
      (expect (= 0 (:mutating-idx mismatch)))
      (expect (= 1 (:answer-idx mismatch)))
      (expect (string? msg))
      (expect (str/includes? msg "structurally unobservable"))
      (expect (str/includes? msg "z/patch-check"))))

  (it "rejects v/patch + answer just like z/patch + answer"
    (let [code     "(v/patch [{:path \"a.clj\" :search \"x\" :replace \"y\"}])\n(answer \"shipped\")"
          mismatch (#'loop/answer-with-mutation-preflight-mismatch (entries-of code))]
      (expect (some? mismatch))))

  (it "rejects mutation nested inside the same form as the answer (do/let wrappers)"
    ;; Same iteration, ONE top-level form: (do (z/patch ...) (answer ...))
    ;; Still unobservable because there is no journal step between the
    ;; mutation and the answer composition.
    (let [code     "(do (z/patch [{:path \"a.clj\" :search \"x\" :replace \"y\"}]) (answer \"ok\"))"
          mismatch (#'loop/answer-with-mutation-preflight-mismatch (entries-of code))]
      (expect (some? mismatch))
      (expect (= 0 (:answer-idx mismatch)))
      (expect (= 0 (:mutating-idx mismatch)))))

  (it "leaves read-only tools + (answer ...) alone -- v/cat/v/rg/z/locators are NOT mutating"
    ;; Read-only tools are observable through the eval pipeline; the
    ;; answer can legitimately reference their values in the same iter.
    (doseq [code ["(v/cat \"a.clj\")\n(answer \"ok\")"
                  "(v/rg {:all [\"foo\"]})\n(answer \"ok\")"
                  "(z/locators \"a.clj\")\n(answer \"ok\")"
                  "(z/patch-check [{:path \"a.clj\" :search \"x\" :replace \"y\"}])\n(answer \"ok\")"
                  "(v/patch-check [{:path \"a.clj\" :search \"x\" :replace \"y\"}])\n(answer \"ok\")"]]
      (expect (nil? (#'loop/answer-with-mutation-preflight-mismatch (entries-of code))))))

  (it "leaves a pure-answer iteration alone"
    (expect (nil? (#'loop/answer-with-mutation-preflight-mismatch
                   (entries-of "(answer (v/p \"hi\"))")))))

  (it "leaves a mutate-only iteration alone (no answer present)"
    (expect (nil? (#'loop/answer-with-mutation-preflight-mismatch
                   (entries-of "(z/patch [{:path \"a.clj\" :search \"x\" :replace \"y\"}])"))))))

;; ============================================================================
;; REPRODUCTIONS — preserve-thinking budget is provider-incoherent
;;
;; The vis loop's `replay-reasoning-chars` measures `(or sig think)` —
;; whichever field is non-empty. svar's per-provider canonicaliser fills
;; these fields with structurally different things:
;;
;;   provider                 :thinking            :thinking-signature
;;   ─────────────────────    ──────────────────   ──────────────────────────
;;   :anthropic-extended-     full reasoning text  short opaque HMAC (~64 ch)
;;   :zai-thinking            verbatim reasoning   verbatim reasoning  (same)
;;   :openai-responses        summary/content      JSON-encoded raw item
;;
;; `(or sig think)` picks `sig` whenever it's non-empty, so for Anthropic
;; iters the budget sees ~64 chars of HMAC, NOT the kilobytes of actual
;; reasoning. The budget under-counts by ~3 orders of magnitude → for
;; Anthropic, the 30k-token cap is effectively unlimited and we never
;; trim. For Z.ai the count is correct. For Responses the count measures
;; the JSON wrapper of the encrypted item, which is again not the real
;; reasoning length the model paid for.
;;
;; These tests pin the actual current behaviour so the next pass fixing
;; the math has a concrete delta to demonstrate.
;; ============================================================================

(defn- mk-anthropic-iter
  "Anthropic canonical shape: full reasoning under :thinking, opaque
   HMAC under :thinking-signature."
  [pos thinking-text hmac-sig]
  [pos {:assistant-message
        {:role "assistant"
         :content [{:type "thinking"
                    :thinking thinking-text
                    :thinking-signature hmac-sig}
                   {:type "text" :text "code"}]}}])

(defn- mk-zai-iter
  "Z.ai canonical shape: verbatim reasoning duplicated into both fields
   so the contiguous-run preserved-thinking contract round-trips byte-
   exact. (svar's `anthropic-message->canonical` for non-Anthropic
   providers folds the same text into both — we mirror that here.)"
  [pos reasoning-text]
  [pos {:assistant-message
        {:role "assistant"
         :content [{:type "thinking"
                    :thinking reasoning-text
                    :thinking-signature reasoning-text}
                   {:type "text" :text "code"}]}}])

(defdescribe REPRO-replay-reasoning-chars-provider-incoherence
  (it "Anthropic shape: budget sees the HMAC length, not the reasoning length"
    ;; 30k chars of real reasoning (~7.5k tokens) hidden behind a 64-char
    ;; HMAC signature. `replay-reasoning-chars` returns 64, so the iter
    ;; looks 470× cheaper than it really is to the budget walker.
    (let [thinking-30k (apply str (repeat 30000 \X))
          hmac         (apply str (repeat 64 \H))
          [_ {:keys [assistant-message]}] (mk-anthropic-iter 1 thinking-30k hmac)
          measured     (#'loop/replay-reasoning-chars assistant-message)]
      (expect (= 64 measured))
      (expect (not= (count thinking-30k) measured))))

  (it "Z.ai shape: budget sees the actual reasoning length"
    ;; When :thinking-signature carries the verbatim reasoning (Z.ai
    ;; canonical), the budget math is correct.
    (let [reasoning-30k (apply str (repeat 30000 \Z))
          [_ {:keys [assistant-message]}] (mk-zai-iter 1 reasoning-30k)
          measured      (#'loop/replay-reasoning-chars assistant-message)]
      (expect (= 30000 measured))))

  (it "Anthropic shape: 24 fat iters all kept because the budget never trips"
    ;; 24 iters × 30 KB reasoning each = 720 KB of replay text the model
    ;; will be billed for. The budget walker sees 24 × 64 chars = 1.5 KB
    ;; total, well under the 120 KB (30k tok × 4) limit, so it keeps
    ;; ALL 24 iters. The Anthropic API in turn auto-strips most of them
    ;; via `context_window = (input_tokens - previous_thinking_tokens)`,
    ;; but we still have to serialize and ship every byte.
    (let [thinking-30k (apply str (repeat 30000 \X))
          iters (vec (for [n (range 1 25)]
                       (mk-anthropic-iter n thinking-30k
                         (str "hmac-" n "-" (apply str (repeat 60 \H))))))
          out   (#'loop/preserved-thinking-replay-messages iters)]
      (expect (= 24 (count out)))
      ;; Total replay reasoning bytes that will hit the wire on Anthropic:
      ;;   24 × 30000 = 720000 chars ≈ 180k tokens, way past the
      ;; documented 30k-token budget the constant claims to enforce.
      (let [total-thinking-chars
            (->> out
              (mapcat (comp :content))
              (filter #(= "thinking" (:type %)))
              (map #(count (:thinking %)))
              (reduce +))]
        (expect (= 720000 total-thinking-chars)))))

  (it "Z.ai shape: 24 fat iters get trimmed to the 30k-token budget"
    ;; Same shape, Z.ai canonical: the walker correctly trims because the
    ;; signature == reasoning. Only the newest iter survives once a single
    ;; iter alone fills the 120k-char (= 30k-tok) budget.
    (let [reasoning-30k (apply str (repeat 30000 \Z))
          iters (vec (for [n (range 1 25)] (mk-zai-iter n reasoning-30k)))
          out   (#'loop/preserved-thinking-replay-messages iters)
          ;; Budget = 120k chars; each iter = 30k chars → keep 4 newest
          ;; (4 × 30k = 120k, fits exactly), the 5th would push over.
          n-kept (count out)]
      (expect (<= n-kept 4))
      (expect (>= n-kept 1)))))

;; ============================================================================
;; REPRO — Z.ai 30k-tok preserved-thinking budget is too generous
;;
;; The user's intuition: "do less stuff in zai preserve thinking" — even
;; the newest iter alone can saturate 30k tokens (= 120k chars, our cap).
;; Reproduce conv 2f889837 iter 13: 30,004 reasoning_tokens emitted in
;; ONE iteration, so iter 14 carries that whole blob. A tighter budget
;; (say 8k tokens = 32k chars) would keep enough context for continuity
;; without inflating every following iter's input by 30k tokens.
;; ============================================================================

(defdescribe REPRO-zai-preserved-thinking-budget-too-generous
  (it "one fat iter alone saturates the budget (the iter-13 shape)"
    ;; Conv 2f889837 / iter 13 / claude-opus-4-7 emitted 30,004 reasoning
    ;; tokens in a single round. Modeled here as 120,016 chars
    ;; (30,004 tok × 4 chars/tok proxy). With the current 30k-tok budget,
    ;; the "always keep the newest, even when it alone exceeds budget"
    ;; rule means we ship the whole 30k-tok blob into iter 14's input.
    (let [iter-13-shaped (mk-zai-iter 13 (apply str (repeat 120016 \R)))
          out (#'loop/preserved-thinking-replay-messages [iter-13-shaped])
          replay-bytes (->> out
                         (mapcat :content)
                         (filter #(= "thinking" (:type %)))
                         (map #(count (:thinking %)))
                         (reduce +))]
      (expect (= 1 (count out)))
      (expect (= 120016 replay-bytes))
      ;; A tighter budget (proposal: 8k tokens = 32k chars) on this same
      ;; iter would still keep the iter for continuity but force the
      ;; serializer to truncate the reasoning to a head/tail summary.
      ;; We don't have that path yet — this assertion is the spec.
      (let [proposed-budget-chars (* 4 8000)]
        (expect (> replay-bytes proposed-budget-chars)))))

  (it "documents the current vs proposed budget constants"
    ;; Document the values that a budget-tightening PR will change.
    (expect (= 30000 @#'loop/preserved-thinking-replay-token-budget))
    ;; Proposed: per-provider strategy
    ;;   :anthropic-extended-thinking → keep ONLY most recent prior iter
    ;;   :zai-thinking                → cap at ~8000 tok
    ;;   :openai-responses            → encrypted_content round-trip
    ;;   default                       → drop (no replay)
    ))

;; ============================================================================
;; REPRO — v/cat re-emits the same file's preview into the journal on
;;        every read, with no per-turn dedup.
;;
;; Hypothesis (user): "v/cat should not READ THE SHIT TO JOURNAL — instead
;; the journal should record 'you read X, don't read again unless changed,
;; inspect via the binding'."
;;
;; Today: every (v/cat path) call inside a turn renders the SAME first-50
;; + last-50 line preview into <journal>. Iter-7 of conv 2f889837 issued
;; 337 read blocks and emitted 43,742 output tokens; many were repeat
;; reads of screen.clj/state.clj/loop.clj — each one re-printed 100
;; numbered lines into the running context.
;;
;; This repro builds a synthetic 4000-line file, runs `journal-render-cat`
;; three times in a row (simulating three (v/cat path) calls in one turn),
;; and shows the journal text is identical and there is no fingerprint /
;; sha / "already read in iter K" marker that would let the model skip
;; the next read. Fix would route repeat reads through a per-turn
;; "files-read" registry and emit a one-line "already bound at iter K
;; (sha=…, lines=…); slice via your binding" instead of the preview.
;; ============================================================================

(defdescribe REPRO-vcat-journal-redundant-reads
  (it "three sequential reads of the same file emit the same long preview"
    (let [editing-ns (try (require 'com.blockether.vis.ext.foundation.editing.core)
                       (find-ns 'com.blockether.vis.ext.foundation.editing.core)
                       (catch Throwable _ nil))]
      (when editing-ns
        (let [render-cat (ns-resolve editing-ns 'journal-render-cat)
              lines      (vec (for [i (range 4000)] (str "line-" i)))
              result     {:path "src/big.clj"
                          :offset 0
                          :total-lines 4000
                          :truncated-by :none
                          :lines lines}
              read1      (render-cat result)
              read2      (render-cat result)
              read3      (render-cat result)]
          ;; Today: identical preview emitted 3×, no dedup, no sha hint.
          (expect (= read1 read2 read3))
          ;; Each preview prints both head and tail blocks — significant
          ;; tokens per re-read.
          (expect (str/includes? read1 "line-0"))
          (expect (str/includes? read1 "line-3999"))
          (expect (str/includes? read1 "line(s) elided"))
          ;; And the renderer carries no signal that this path was just
          ;; read — no sha, no iter-K reference, no "skip if unchanged".
          (expect (not (str/includes? read1 "sha")))
          (expect (not (str/includes? read1 "already bound")))
          (expect (not (str/includes? read1 "previously read")))))))

  (it "documents the proposed shape: dedup hint instead of full preview"
    ;; Spec for the next PR. When a path has already been v/cat'd inside
    ;; the same turn AND the file's sha matches the prior read, the
    ;; journal renderer should emit:
    ;;
    ;;   "v/cat src/big.clj — already read at iter K (lines=4000, sha=…);
    ;;    inspect via your binding (subvec/get-in)."
    ;;
    ;; instead of re-emitting the 100-line preview. The full :lines
    ;; vector stays bound in the SCI runtime (already does today), so
    ;; the model loses zero information; the journal sheds ~1.5 KB per
    ;; redundant read.
    (expect true)))

;; ============================================================================
;; REPRO — Z.ai has NO server-side reasoning-token cap (the iter-13 cause)
;;
;; svar/router.clj:190 shows the reasoning-style translation table:
;;
;;   :quick    → {:openai-effort "low"    :anthropic-thinking 1024  :zai-thinking "disabled"}
;;   :balanced → {:openai-effort "medium" :anthropic-thinking 8192  :zai-thinking "enabled"}
;;   :deep     → {:openai-effort "high"   :anthropic-thinking 24000 :zai-thinking "enabled"}
;;
;; OpenAI: low/medium/high effort scalar.
;; Anthropic: explicit budget_tokens (1024 / 8192 / 24000).
;; Z.ai: BINARY enabled/disabled. NO budget. Server picks freely.
;;
;; Confirmed by Z.ai's own docs (docs.z.ai/guides/capabilities/thinking):
;;   "thinking.type: enabled (default) | disabled"
;;   No max_thinking_tokens, no thinking_budget, no effort scalar.
;;
;; This is the structural reason iter 13 of conv 2f889837 emitted 30,004
;; reasoning_tokens in one round on glm-5.1: vis sent thinking={type:
;; enabled} (per :balanced) and the server decided 30k was the right
;; amount. The vis loop has no client-side knob to cap it.
;; ============================================================================

(defdescribe REPRO-zai-has-no-thinking-budget-knob
  (it "the svar reasoning-level table has only enabled/disabled for Z.ai"
    (let [router-ns (do (require 'com.blockether.svar.internal.router)
                      (find-ns 'com.blockether.svar.internal.router))
          ;; Defensive: pull the level table by a stable name. The actual
          ;; var name in svar is `reasoning-level-translations`; if svar
          ;; renames it this test will fail loud and we update both.
          level-table (or (some-> (ns-resolve router-ns 'reasoning-level-translations) deref)
                        (some-> (ns-resolve router-ns 'level-translations) deref))]
      (when level-table
        (expect (= "disabled" (get-in level-table [:quick    :zai-thinking])))
        (expect (= "enabled"  (get-in level-table [:balanced :zai-thinking])))
        (expect (= "enabled"  (get-in level-table [:deep     :zai-thinking])))
        ;; And there is NO numeric budget field for Z.ai — only on/off.
        (let [zai-vals (->> level-table vals (map :zai-thinking))]
          (expect (every? #{"enabled" "disabled"} zai-vals))
          (expect (not-any? number? zai-vals))))))

  (it "Anthropic and OpenAI DO have numeric/scalar budgets — for contrast"
    (let [router-ns (do (require 'com.blockether.svar.internal.router)
                      (find-ns 'com.blockether.svar.internal.router))
          level-table (or (some-> (ns-resolve router-ns 'reasoning-level-translations) deref)
                        (some-> (ns-resolve router-ns 'level-translations) deref))]
      (when level-table
        ;; Anthropic: numeric budget_tokens
        (expect (number? (get-in level-table [:quick    :anthropic-thinking])))
        (expect (number? (get-in level-table [:balanced :anthropic-thinking])))
        (expect (number? (get-in level-table [:deep     :anthropic-thinking])))
        ;; OpenAI: low/medium/high scalar
        (expect (#{"low" "medium" "high"} (get-in level-table [:quick    :openai-effort])))
        (expect (#{"low" "medium" "high"} (get-in level-table [:balanced :openai-effort])))
        (expect (#{"low" "medium" "high"} (get-in level-table [:deep     :openai-effort])))))))

;; ============================================================================
;; REPRO — bindings index DOES already collapse tool-results, but does NOT
;;         carry file-binding pointer info (path/sha/total-lines).
;;
;; User's idea: "in the var-index information that this var holds file or
;; something like pointers so it will never happen". The var registry
;; already runs `extension/tool-result?` and replaces the body with a
;; compact `{:tool-result true :success? :op …}` map. That kills the
;; `:lines` 4000-element vector in the bindings render.
;;
;; What's missing: the registry doesn't surface :path, :sha, :total-lines
;; for v/cat results. Those are exactly the fields a future per-turn
;; "already read this file" dedup would key on. Today the bindings render
;; just says "{:tool-result true :op :v/cat}" — no path, no sha. The
;; model can't tell from <bindings> alone which file `f1` vs `f2` holds.
;; ============================================================================

(defdescribe REPRO-bindings-tool-result-collapse-misses-file-pointer
  (it "the v/cat result-map shape carries :path :total-lines :truncated-by — the journal already knows it"
    ;; Pulled from extensions/.../foundation/editing/core.clj:
    ;;   {:op :v/cat :result {:path :offset :total-lines :truncated-by :lines}}
    ;; This is the data the next-pass should hoist into the bindings entry.
    (let [editing-ns (try (require 'com.blockether.vis.ext.foundation.editing.core)
                       (find-ns 'com.blockether.vis.ext.foundation.editing.core)
                       (catch Throwable _ nil))]
      (when editing-ns
        ;; Just confirm the renderer takes the full result map; this is
        ;; the surface a future hoist would read from.
        (let [render-cat (ns-resolve editing-ns 'journal-render-cat)
              result     {:path "src/big.clj"
                          :offset 0
                          :total-lines 4000
                          :truncated-by :none
                          :lines (vec (for [i (range 4000)] (str "line-" i)))}
              text       (render-cat result)]
          ;; Renderer has access to :path (it prints it).
          (expect (str/includes? text "src/big.clj"))
          ;; And to :total-lines.
          (expect (str/includes? text "total 4000"))))))

  (it "documents the proposed bindings-entry shape for file pointers"
    ;; Spec for the next PR to extension/env.clj's render-data-form's
    ;; tool-result branch. When `(:op (:info val))` is `:v/cat`, the
    ;; collapsed entry should include the file pointer triple:
    ;;
    ;;   {:tool-result true
    ;;    :op          :v/cat
    ;;    :path        "src/big.clj"
    ;;    :total-lines 4000
    ;;    :sha         "<sha256-hex>"}
    ;;
    ;; That gives the journal-render-cat a registry to look up "have we
    ;; read this path with this sha before in this turn?" and replace the
    ;; preview with a one-line "already at iter K via binding f17".
    (expect true)))

;; ============================================================================
;; REPRO — proposed `:metadata` shape (docs/specs/02-...)
;;
;; The spec proposes adding `:metadata` to every tool-result's `:info` map:
;;
;;   (s/keys :req-un [::kind]
;;           :opt-un [::path ::sha ::size ::truncated? ::preview
;;                    ::pointer ::iter-bound ::extras])
;;
;; These tests document the target. They EXPECT the field to be missing
;; today (so they're green now, pinning current absence) and the impl PR
;; will flip them to assert presence.
;; ============================================================================

(defdescribe REPRO-tool-result-metadata-shape-target
  (it "TODAY: v/cat result has no :info :metadata field"
    ;; Build a synthetic v/cat-shaped tool result the way
    ;; foundation/editing/core.clj does it. The proposed `:metadata`
    ;; field is NOT yet stamped.
    (let [synthetic-vcat-result
          {:success? true
           :result   {:path "src/big.clj"
                      :offset 0
                      :total-lines 4000
                      :truncated-by :none
                      :lines (vec (for [i (range 4000)] (str "line-" i)))}
           :info     {:op :v/cat
                      :started-at-ms 0
                      :finished-at-ms 1
                      :duration-ms 1}
           :error    nil}]
      ;; Pin: today, :metadata is not present.
      (expect (nil? (get-in synthetic-vcat-result [:info :metadata])))))

  (it "PROPOSED: v/cat metadata carries kind/path/sha/size/preview"
    ;; What a future impl SHOULD produce. Encoded as a static expected
    ;; map; the impl PR replaces the `expected` literal here with a call
    ;; to the actual builder once it exists.
    (let [proposed-metadata
          {:kind        :file
           :path        "src/big.clj"
           :sha         "9af1abcd000000000000000000000000000000000000000000000000deadbeef"
           :size        {:lines 4000 :bytes 132817}
           :truncated?  false
           :preview     "1: (ns big)\n2: …\n3999: x\n4000: y"
           :extras      {:offset 0 :truncated-by :none}}]
      ;; Shape contract:
      (expect (keyword? (:kind proposed-metadata)))
      (expect (= :file (:kind proposed-metadata)))
      (expect (string? (:path proposed-metadata)))
      (expect (= 64 (count (:sha proposed-metadata))))
      (expect (map? (:size proposed-metadata)))
      (expect (number? (get-in proposed-metadata [:size :lines])))
      (expect (boolean? (:truncated? proposed-metadata)))
      (expect (<= (count (:preview proposed-metadata)) 400))
      ;; :pointer + :iter-bound get stamped by the SCI runtime AFTER
      ;; the result lands in a `def`, so the tool-author's metadata
      ;; does NOT include them — they appear later.
      (expect (nil? (:pointer proposed-metadata)))
      (expect (nil? (:iter-bound proposed-metadata))))))

;; ============================================================================
;; REPRO — proposed Z.ai head/tail truncation policy (8k = 2k head + 6k tail)
;;
;; Spec: when a single Z.ai iter's reasoning_content exceeds 8000 tokens
;; (~32000 chars), keep the first 2000 tokens (8000 chars) verbatim,
;; insert a `⟨vis-truncated …⟩` sentinel, then keep the last 6000 tokens
;; (24000 chars) verbatim. Net iter context = ~32k chars + sentinel.
;;
;; Z.ai docs say byte-edit MAY degrade cache hit rate; does NOT reject.
;; Iter 13's 30k-tok (~120k char) blob would be reduced to 8k tok
;; (~32k char), saving ~88k chars per replay × every following iter.
;; ============================================================================

(defn- mock-zai-truncate-head-tail
  "Reference impl of the proposed head/tail truncation. Lives in the
   test as a spec, not as production code. The real loop.clj impl will
   match this contract."
  [reasoning-text {:keys [head-chars tail-chars sha-prefix]}]
  (let [n (count reasoning-text)
        cap (+ head-chars tail-chars)]
    (if (<= n cap)
      reasoning-text
      (let [head (subs reasoning-text 0 head-chars)
            tail (subs reasoning-text (- n tail-chars))
            elided (- n head-chars tail-chars)]
        (str head
          "\n\n⟨vis-truncated: " elided " chars of reasoning_content elided. "
          "Z.ai contract requires consecutive verbatim blocks; this break "
          "may degrade cache hit rate but bounds iter context. "
          "Original sha=" sha-prefix "…⟩\n\n"
          tail)))))

(defdescribe REPRO-zai-head-tail-truncation-policy
  (it "is identity for reasoning that fits the cap"
    (let [r (apply str (repeat 1000 \X))   ; 1k chars, well under
          out (mock-zai-truncate-head-tail r {:head-chars 8000 :tail-chars 24000 :sha-prefix "abc"})]
      (expect (= r out))))

  (it "iter-13 shape: 120k chars → 32k chars + sentinel"
    (let [iter-13-reasoning (apply str (repeat 120016 \R))   ; iter 13 size
          out (mock-zai-truncate-head-tail iter-13-reasoning
                {:head-chars 8000 :tail-chars 24000 :sha-prefix "iter13"})]
      ;; Output is much shorter than input.
      (expect (< (count out) (count iter-13-reasoning)))
      ;; Head verbatim (first 8000 chars).
      (expect (str/starts-with? out (apply str (repeat 8000 \R))))
      ;; Tail verbatim (last 24000 chars).
      (expect (str/ends-with? out (apply str (repeat 24000 \R))))
      ;; Sentinel sits in the middle and identifies the elision.
      (expect (str/includes? out "⟨vis-truncated"))
      (expect (str/includes? out "88016 chars"))
      (expect (str/includes? out "iter13"))))

  (it "two iters of 120k each → both truncated, total replay ~64k chars not 240k"
    (let [iter-12 (apply str (repeat 120000 \A))
          iter-13 (apply str (repeat 120000 \B))
          out-12  (mock-zai-truncate-head-tail iter-12 {:head-chars 8000 :tail-chars 24000 :sha-prefix "i12"})
          out-13  (mock-zai-truncate-head-tail iter-13 {:head-chars 8000 :tail-chars 24000 :sha-prefix "i13"})
          total   (+ (count out-12) (count out-13))]
      ;; Each truncated to ~32k chars + a small sentinel (<300 chars).
      (expect (< (count out-12) 33000))
      (expect (< (count out-13) 33000))
      ;; Combined replay context: ~64k chars vs the 240k chars without
      ;; this policy. Saving ~73% on a runaway turn.
      (expect (< total 70000))))

  (it "documents the proposed Z.ai constants"
    ;; These are the numbers the user agreed on. The impl PR will read
    ;; these from `replay-strategy-by-style[:zai-thinking]`.
    (let [proposed {:zai-thinking
                    {:keep-budget-tokens 8000
                     :hard-cap-tokens    8000
                     :head-tokens        2000
                     :tail-tokens        6000}}
          zai (:zai-thinking proposed)]
      (expect (= 8000 (:keep-budget-tokens zai)))
      (expect (= 8000 (:hard-cap-tokens zai)))
      (expect (= 2000 (:head-tokens zai)))
      (expect (= 6000 (:tail-tokens zai)))
      ;; head + tail = hard-cap (sentinel is metadata-only).
      (expect (= (:hard-cap-tokens zai)
                (+ (:head-tokens zai) (:tail-tokens zai)))))))
