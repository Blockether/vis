(ns com.blockether.vis.internal.activity.event-test
  (:require [com.blockether.vis.internal.activity.event :as event]
            [com.blockether.vis.contract.wire :as wire]
            [clojure.string :as string]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- argument-start
  [ctx args]
  (event/start-event ctx
                     (event/invocation ctx nil)
                     {:operation :grep :presenter :generic :args args}))

(defdescribe
  argument-identity-test
  (it "identifies complete argument tuples independently of map insertion order"
      (let [ctx
            (event/context)

            args
            [(array-map :query ["needle"] :paths ["src" "test"])]

            key-of
            #(:argument-key (argument-start ctx %))

            identity
            (key-of args)]

        (expect (string? identity))
        (expect (boolean (and identity (re-matches #"[0-9a-f]{64}" identity))))
        (expect (= identity (key-of [(array-map :paths ["src" "test"] :query ["needle"])])))
        (expect (not= identity (key-of [{:query ["other"] :paths ["src" "test"]}])))
        (expect (not= identity (key-of [{:query ["needle"] :paths ["test" "src"]}])))
        (expect (= (key-of []) (key-of [])))
        (expect (= 5 (count (set (map key-of [[1] ["1"] [1.0] [true] [nil]])))))))
  (it "distinguishes arguments beyond their visible preview and keeps private values off the wire"
      (let [ctx
            (event/context)

            prefix
            (apply str (repeat (inc event/max-summary-bytes) "x"))

            a
            (argument-start ctx [{:query (str prefix "a") :password "fixture-first"}])

            b
            (argument-start ctx [{:query (str prefix "b") :password "fixture-first"}])

            c
            (argument-start ctx [{:query (str prefix "a") :password "fixture-second"}])]

        (expect (every? :argument-key [a b c]))
        (expect (= 3 (count (set (map :argument-key [a b c])))))
        (doseq [public [a b c]]
          (expect (nil? (event/event-error public)))
          (expect (not (string/includes? (wire/json-str public) "fixture-first")))
          (expect (not (string/includes? (wire/json-str public) "fixture-second"))))))
  (it "scopes identities to one block and refuses incomplete or opaque arguments"
      (let [a
            (argument-start (event/context) [{:query "needle"}])

            b
            (argument-start (event/context) [{:query "needle"}])]

        (expect (some? (:argument-key a)))
        (expect (not= (:argument-key a) (:argument-key b))))
      (doseq [args [[(repeat 1)] [(apply str (repeat (inc event/max-detail-bytes) "x"))]
                    [{"__vis_callable__" "callback-1"}] [(Object.)]]]
        (expect (nil? (:argument-key (argument-start (event/context) args)))))))

(defdescribe argument-key-validation-test
             (it "rejects malformed equality keys rather than trusting display text"
                 (let [valid (argument-start (event/context) [])]
                   (doseq [key [nil "" "guess" (apply str (repeat 64 "A")) 42]]
                     (expect (= "malformed argument key"
                                (event/event-error (assoc valid :argument-key key))))))))

(defdescribe
  custom-presentation-redaction-test
  (it "redacts custom headlines, summaries, code and table content"
      (let [ctx
            (event/context)

            public
            (event/content-event
              ctx
              (event/invocation ctx nil)
              {:operation :sample :presenter :generic}
              {:headline "password=fixture-headline"
               :summary "token=fixture-summary"
               :content [{:type :code :text "api_key=fixture-code" :language "python"}
                         {:type :table :columns ["Value"] :rows [["secret=fixture-cell"]]}]})]

        (doseq [secret ["fixture-headline" "fixture-summary" "fixture-code" "fixture-cell"]]
          (expect (not (string/includes? (pr-str public) secret))))
        (expect (nil? (event/event-error public)))))
  (it "never publishes a presentation whose redaction exceeds the content budget"
      (let [ctx
            (event/context)

            presentation
            {:headline (str (apply str (repeat 502 "x")) " token=a") :summary "" :content []}

            refusal
            (try (event/content-event ctx
                                      (event/invocation ctx nil)
                                      {:operation :sample :presenter :generic}
                                      presentation)
                 nil
                 (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]

        (expect (= :activity/invalid-content refusal)))))

(defdescribe
  presentation-secret-regression-test
  (it
    "redacts labelled credentials and embedded handles on every Activity text surface"
    (let [ctx
          (event/context)

          invocation
          (event/invocation ctx nil)

          text
          "password=fixture-password token=fixture-token handle vis-secret:fixture-handle"

          details
          {:operation :shell
           :presenter :generic
           :args [text]
           :label text
           :phrase text
           :group-head text}

          start
          (event/start-event ctx invocation details)

          terminal
          (event/terminal-event ctx
                                invocation
                                (assoc details
                                  :started-at-ms 0
                                  :outcome :succeeded
                                  :result text))

          failed
          (event/terminal-event ctx
                                invocation
                                (assoc details
                                  :started-at-ms 0
                                  :outcome :failed
                                  :error (ex-info text {})))]

      (doseq [public [start terminal failed]]
        (let [rendered (pr-str public)]
          (expect (not (string/includes? rendered "fixture-password")))
          (expect (not (string/includes? rendered "fixture-token")))
          (expect (not (string/includes? rendered "fixture-handle")))
          (expect (string/includes? rendered "[REDACTED]"))))))
  (it "hides private key fields and callable transport references without changing raw data"
      (let [ctx
            (event/context)

            raw
            {"private_key" "fixture-private-key"
             "passphrase" "fixture-passphrase"
             "callback" {"__vis_callable__" "fixture-callable-reference"}
             "nested" [{"__vis_callback__" "fixture-callback-reference"}]
             "count" 3}

            terminal
            (event/terminal-event ctx
                                  (event/invocation ctx nil)
                                  {:operation :sample
                                   :presenter :generic
                                   :started-at-ms 0
                                   :outcome :succeeded
                                   :result raw})

            public
            (:result-summary terminal)]

        (doseq [secret ["fixture-private-key" "fixture-passphrase" "fixture-callable-reference"
                        "fixture-callback-reference" "__vis_"]]
          (expect (not (string/includes? public secret))))
        (expect (string/includes? public "3"))
        (expect (= "fixture-private-key" (get raw "private_key")))
        (expect (= "fixture-callable-reference" (get-in raw ["callback" "__vis_callable__"]))))))

(defdescribe
  python-transport-presentation-test
  (it
    "shows public fields, not Python object envelopes, in arguments and outcomes"
    (let [nested
          {"__vis_object__" "Job"
           "__vis_attrs__" {"status" "success" "api_key" "fixture-credential"}
           "__vis_object_ref__" "fixture-job-reference"}

          result
          {"__vis_object__" "WatchOutcome"
           "__vis_attrs__" {"jobs" [nested]}
           "__vis_object_ref__" "fixture-outcome-reference"}

          ctx
          (event/context)

          invocation
          (event/invocation ctx nil)

          details
          {:operation :gh.watch :presenter :generic :args [result]}

          start
          (event/start-event ctx invocation details)

          terminal
          (event/terminal-event ctx
                                invocation
                                (assoc details
                                  :started-at-ms 0
                                  :outcome :succeeded
                                  :result result))]

      (doseq [summary [(:argument-summary start) (:result-summary terminal)]]
        (expect (string/includes? summary "success"))
        (expect (string/includes? summary "[REDACTED]"))
        (expect (not (string/includes? summary "__vis_")))
        (expect (not (string/includes? summary "fixture-credential")))
        (expect (not (string/includes? summary "fixture-job-reference")))
        (expect (not (string/includes? summary "fixture-outcome-reference"))))
      (expect (= "fixture-outcome-reference" (get result "__vis_object_ref__")))))
  (it "keeps the traversal budget when public object fields contain an unbounded sequence"
      (let [result
            {"__vis_object__" "WatchOutcome"
             "__vis_attrs__" {"jobs" (concat (range 200)
                                             (lazy-seq (throw (ex-info "traversed too far" {}))))}}

            ctx
            (event/context)

            terminal
            (event/terminal-event ctx
                                  (event/invocation ctx nil)
                                  {:operation :gh.watch
                                   :presenter :generic
                                   :started-at-ms 0
                                   :outcome :succeeded
                                   :result result})]

        (expect (:result-truncated terminal))
        (expect (not (string/includes? (:result-summary terminal) "__vis_")))))
  (it "does not rewrite source text that mentions a transport marker"
      (let [ctx
            (event/context)

            result
            "return {'__vis_object__': 'Example'}"

            terminal
            (event/terminal-event ctx
                                  (event/invocation ctx nil)
                                  {:operation :cat
                                   :presenter :observation
                                   :started-at-ms 0
                                   :outcome :succeeded
                                   :result result})]

        (expect (= (pr-str result) (:result-summary terminal))))))

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
    "carries a patch WHOLE, classified and redacted"
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
          (first (:diff-evidence terminal))]

      (expect (= :diff (:kind evidence)))
      (expect (= [:hunk :context :deletion :addition] (mapv :kind (take 4 (:lines evidence)))))
      ;; Regression, T121: the KIND is the sign. Leaving `+`/`-` on the text too made
      ;; every renderer, which draws its own marker column, print `+ +` and `- -`.
      (expect (not-any? #(re-find #"^[-+]" (str (:text %)))
                        (filter (comp #{:addition :deletion :context} :kind) (:lines evidence))))
      (expect (= ["[REDACTED]" "[REDACTED]"] (mapv :text (take 2 (drop 2 (:lines evidence))))))
      (expect (:is-redacted evidence))
      ;; Regression, T131: a patch stopped after 120 lines and the receipt printed
      ;; `N more lines`, so the reader had to leave the axis to read the rest of it.
      (expect (not (:is-truncated evidence)))
      (expect (= (count (string/split-lines diff)) (count (:lines evidence)))
              "every line of the patch is carried")
      (expect (<= (event/utf8-bytes (wire/json-str terminal)) event/max-event-bytes))))
  ;; Regression, T120: one write touching eleven files shipped ONE diff evidence carrying
  ;; hand-made `--- (path)` headers, so the reader scrolled through ten files to reach theirs.
  (it
    "answers one named diff per file when a result carries several"
    (let [ctx
          (event/context)

          invocation
          (event/invocation ctx nil)

          hunk
          (fn [path]
            {:diff (str "@@ -1,1 +1,2 @@\n context\n+" path
                        "\n" (apply str (repeat 400 (str "+" (apply str (repeat 400 "y")) "\n"))))
             :lines {"added" 401 "removed" 0 "modified" 0}
             :target {:resolved path}})

          paths
          ["/tmp/one.clj" "/tmp/two.clj" "/tmp/three.clj"]

          terminal
          (event/terminal-event ctx
                                invocation
                                {:operation :fs-write
                                 :presenter :patch
                                 :started-at-ms (System/currentTimeMillis)
                                 :outcome :succeeded
                                 :result "wrote 3 files"
                                 :result-envelope {:metadata
                                                   {:lines {"added" 1203 "removed" 0 "modified" 0}
                                                    :diffs (mapv hunk paths)}}})

          evidence
          (:diff-evidence terminal)]

      (expect (= 3 (count evidence)))
      (expect (= [:diff :diff :diff] (mapv :kind evidence)))
      (expect (= paths (mapv :text evidence)))
      ;; Regression, T131: the row's byte budget was divided between the files and each
      ;; diff was cut where its share ran out, so three files arrived as three half-patches.
      (expect (<= (event/utf8-bytes (wire/json-str terminal)) event/max-event-bytes)
              "the transport's ceiling still holds")
      (expect (some #(seq (:lines %)) evidence) "what fits is carried")
      (expect (some #(and (empty? (:lines %)) (:is-truncated %)) evidence)
              "and what does not is dropped WHOLE, named and marked")
      (expect (every? (fn [d]
                        (or (empty? (:lines d))
                            (= (count (string/split-lines (:diff (hunk (:text d)))))
                               (count (:lines d)))))
                      evidence)
              "no file arrives as half a patch")))
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
      (expect (<= (event/utf8-bytes (:result-summary terminal)) event/max-detail-bytes))))
  (it "shows nothing for a call with no arguments whose result only declares resources"
      (let [ctx
            (event/context)

            invocation
            (event/invocation ctx nil)

            start
            (event/start-event ctx invocation {:operation :write :presenter :generic :args []})

            terminal
            (event/terminal-event ctx
                                  invocation
                                  {:operation :write
                                   :presenter :generic
                                   :started-at-ms (System/currentTimeMillis)
                                   :outcome :succeeded
                                   :result {:activity/resources [{:type :file :id "/w/one.txt"}]}})]

        ;; a declaration is addressed to Activity, never to the reader: it names the row's
        ;; resources and leaves no argument or result text behind
        (expect (not (contains? start :argument-summary)))
        (expect (not (contains? terminal :result-summary)))
        (expect (= [{:type :file :id "/w/one.txt"}] (:resources terminal))))))
