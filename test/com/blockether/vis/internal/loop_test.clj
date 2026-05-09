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
      (expect (nil? (:rendering-kind result)))))

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
