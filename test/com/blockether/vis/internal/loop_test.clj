(ns com.blockether.vis.internal.loop-test
  (:require
   [com.blockether.vis.internal.loop :as loop]
   [clojure.string :as str]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe def-display-result-test
  (it "does not deref def forms into implicit observations"
    (let [result (#'loop/def-display-result
                  {:sci-ctx :present}
                  "(def xd \"XDDD\")"
                  {:result :raw-var
                   :stdout ""
                   :stderr ""
                   :error nil})]
      (expect (= :raw-var (:result result)))
      (expect (= :vis/silent (:rendering-kind result))))))

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
