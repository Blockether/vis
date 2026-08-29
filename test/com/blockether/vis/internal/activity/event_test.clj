(ns com.blockether.vis.internal.activity.event-test
  (:require [com.blockether.vis.internal.activity.event :as event]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [clojure.string :as string]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  activity-event-contract-test
  (it
    "accepts one start and terminal in event order"
    (let [state
          (event/collector)

          ctx
          (event/context)

          invocation
          (event/invocation ctx nil)

          common
          {:operation :grep :presenter :generic}

          start
          (event/start-event ctx invocation (assoc common :args [{:query "needle"}]))

          terminal
          (event/terminal-event ctx
                                invocation
                                (assoc common
                                  :started-at-ms (System/currentTimeMillis)
                                  :outcome :succeeded
                                  :result {:matches 2}))]

      (event/accept! state start)
      (event/accept! state terminal)
      (expect (= #{(:invocation-id start)} (:starts @state)))
      (expect (= #{(:invocation-id start)} (:terminals @state)))
      (expect (not (contains? @state :events)))
      ;; Ownerless: an event names its own invocation, never the evaluation,
      ;; iteration or form it ran in — the form the block becomes is the
      ;; snapshot's only identity.
      (expect (not-any? #(contains? start %) [:schema-version :evaluation-id :form-index]))
      (expect (true? (:succeeded terminal)))))
  (it
    "rejects orphan and duplicate lifecycle edges"
    (let [ctx
          (event/context)

          invocation
          (event/invocation ctx nil)

          common
          {:operation :probe :presenter :generic}

          start
          (event/start-event ctx invocation (assoc common :args []))

          terminal
          (event/terminal-event ctx
                                invocation
                                (assoc common
                                  :started-at-ms (System/currentTimeMillis)
                                  :outcome :failed
                                  :error (ex-info "no" {})))

          orphan
          (try (event/accept! (event/collector) terminal)
               nil
               (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))

          state
          (event/collector)]

      (expect (= :activity/orphan-terminal orphan))
      (event/accept! state start)
      (expect (= :activity/duplicate-start
                 (try (event/accept! state start)
                      nil
                      (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))
  (it "redacts before applying the UTF-8 summary budget"
      (let [ctx
            (event/context)

            invocation
            (event/invocation ctx nil)

            start
            (event/start-event ctx
                               invocation
                               {:operation :probe
                                :presenter :generic
                                :args [{:password (apply str (repeat 900 "密")) :safe "visible"}]})]

        (expect (not (re-find #"密" (:argument-summary start))))
        (expect (re-find #"REDACTED" (:argument-summary start)))
        (expect (<= (event/utf8-bytes (:argument-summary start)) event/max-summary-bytes))))
  (it
    "bounds, classifies, and redacts structured patch diff evidence"
    (let [ctx
          (event/context)

          invocation
          (event/invocation ctx nil)

          diff
          (str "@@ -1,2 +1,3 @@\n"
               " context\n" "-api_token = old-value\n"
               "+api_token = new-value\n"
               (apply str (repeat 200 (str "+" (apply str (repeat 600 "x")) "\n"))))

          terminal
          (event/terminal-event ctx
                                invocation
                                {:operation :patch
                                 :presenter :patch
                                 :started-at-ms (System/currentTimeMillis)
                                 :outcome :succeeded
                                 :result "patched fixture.clj"
                                 :result-envelope
                                 {:metadata {:target {:resolved "fixture.clj"}
                                             :diff diff
                                             :lines {"added" 201 "removed" 1 "modified" 0}}}})

          evidence
          (:diff-evidence terminal)]

      (expect (= :diff (:kind evidence)))
      (expect (= [:hunk :context :deletion :addition] (mapv :kind (take 4 (:lines evidence)))))
      (expect (= ["[REDACTED]" "[REDACTED]"] (mapv :text (take 2 (drop 2 (:lines evidence))))))
      (expect (:is-redacted evidence))
      (expect (:is-truncated evidence))
      (expect (pos? (:omitted-lines evidence)))
      (expect (<= (count (:lines evidence)) event/max-diff-lines))
      (expect (<= (event/utf8-bytes (wire/json-str evidence)) event/max-diff-bytes))))
  ;; Regression, td-ba1627: a `read_session()` terminal event recursively copied
  ;; and printed the whole transcript before truncating it, leaving the await open.
  (it "bounds result traversal before rendering a terminal summary"
      (let [ctx
            (event/context)

            invocation
            (event/invocation ctx nil)

            beyond-budget
            (concat (range 200) (lazy-seq (throw (ex-info "summary traversed too far" {}))))

            terminal
            (event/terminal-event ctx
                                  invocation
                                  {:operation :read-session
                                   :presenter :generic
                                   :started-at-ms (System/currentTimeMillis)
                                   :outcome :succeeded
                                   :result {:transcript beyond-budget}})]

        (expect (true? (:succeeded terminal)))
        (expect (re-find #"…" (:result-summary terminal)))
        (expect (true? (:result-truncated terminal)))
        (expect (<= (event/utf8-bytes (:result-summary terminal)) event/max-detail-bytes))))
  (it "records actual parentage and independent wrapper order"
      (let [ctx
            (event/context)

            outer
            (event/invocation ctx nil)

            inner
            (event/invocation ctx (:invocation-id outer))]

        (expect (= 1 (:invocation-sequence outer)))
        (expect (= 2 (:invocation-sequence inner)))
        (expect (= (:invocation-id outer) (:parent-invocation-id inner)))))
  (it "derives typed shell resources from explicit presenter metadata"
      (let [ctx
            (event/context)

            run
            (event/invocation ctx nil)

            wait
            (event/invocation ctx nil)

            run-terminal
            (event/terminal-event ctx
                                  run
                                  {:operation :shell
                                   :presenter :shell
                                   :args ["npm test"]
                                   :started-at-ms (System/currentTimeMillis)
                                   :outcome :succeeded
                                   :result {"id" "test-1" "status" "running"}})

            wait-start
            (event/start-event ctx
                               wait
                               {:operation :_shell_wait :presenter :shell :args ["test-1" 30]})]

        (expect (= [{:type :shell-handle :id "test-1"}] (:resources run-terminal)))
        (expect (= [{:type :shell-handle :id "test-1"}] (:resources wait-start))))))

(defdescribe
  activity-event-bounded-text-test
  ;; Regression, session ce61af4d: ONE tool result carrying a 3.2 MB single line
  ;; (a minified JSON blob reached by `cat`/`grep`) spent over an hour inside
  ;; `bounded-text`, which walked the cut down one character at a time and
  ;; re-encoded the whole string on every pass. Event construction runs on the
  ;; CALLING thread, so the block's 300 s timeout fired while that thread kept
  ;; burning and every later block in the session queued behind it.
  (it "bounds a multi-megabyte single line without walking it character by character"
      (let [blob
            (str (.repeat "abcdefghij" 400000) "needle")

            answer
            (promise)

            worker
            (doto (Thread. ^Runnable
                           (fn []
                             (deliver answer (event/bounded-text blob event/max-detail-bytes))))
              (.setDaemon true)
              (.start))

            bounded
            (deref answer 5000 ::timed-out)]

        (expect (not= ::timed-out bounded))
        (expect (<= (event/utf8-bytes bounded) event/max-detail-bytes))
        (expect (string/starts-with? bounded "abcdefghij"))
        (expect (string/ends-with? bounded "…"))
        (expect (some? worker))))
  (it "cuts between code points, never inside one"
      (let [blob
            (string/join (repeat 5000 "źółw"))

            bounded
            (event/bounded-text blob 64)]

        (expect (<= (event/utf8-bytes bounded) 64))
        (expect (not (string/includes? bounded "�")))
        (expect (string/ends-with? bounded "…"))))
  (it "leaves text that already fits untouched"
      (expect (= "źółw" (event/bounded-text "źółw" event/max-summary-bytes))))
  (it
    "builds a terminal event for a multi-megabyte single-line result"
    (let [ctx
          (event/context)

          invocation
          (event/invocation ctx nil)

          blob
          (str (.repeat "abcdefghij" 400000) "needle")

          answer
          (promise)

          _worker
          (doto (Thread. ^Runnable
                         (fn []
                           (deliver answer
                                    (event/terminal-event ctx
                                                          invocation
                                                          {:operation :cat
                                                           :presenter :generic
                                                           :args ["minified.json"]
                                                           :started-at-ms (System/currentTimeMillis)
                                                           :outcome :succeeded
                                                           :result blob}))))
            (.setDaemon true)
            (.start))

          terminal
          (deref answer 5000 ::timed-out)]

      (expect (not= ::timed-out terminal))
      (expect (<= (event/utf8-bytes (:result-summary terminal)) event/max-detail-bytes)))))
