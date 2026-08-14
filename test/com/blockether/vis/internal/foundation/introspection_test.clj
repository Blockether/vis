(ns com.blockether.vis.internal.foundation.introspection-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.introspection :as introspection]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.env-python :as env-python]
            [com.blockether.vis.ext.persistance-sqlite.test-helpers :as h]
            [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis.internal.prompt]
            [com.blockether.vis.internal.foundation.core]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe struct-patch-diagnosis-contract-test
             (let
               [classify
                @#'introspection/classify-expression-failure

                advice
                @#'introspection/advice-for-classification]

               (it "classifies unbalanced struct_patch code and says how to re-emit it"
                   (expect (= :struct-patch-invalid-code
                              (classify "struct_patch({...})" "unmatched delimiter in code")))
                   (let [message (advice :struct-patch-invalid-code)]
                     (expect (str/includes? message "triple-quoted"))
                     (expect (not (str/includes? message "anchor")))))))

;; Regression: grep gained `is_regex` and started answering ONE anchored TEXT
;; block, but this advice still read "Regex is not supported - filter the matches
;; in Python", so a model that hit an escape error was told to index a string.
(defdescribe grep-advice-names-is-regex-and-text-test
             (let [advice @#'introspection/advice-for-classification]
               (it "points at is_regex instead of denying regex"
                   (let [message (advice :regex-unsupported-escape)]
                     (expect (str/includes? message "is_regex"))
                     (expect (not (str/includes? message "Regex is not supported")))))
               (it "tells the model grep answers TEXT, not a keyed map"
                   (let [message (advice :regex-unsupported-escape)]
                     (expect (str/includes? message "anchored TEXT"))
                     (expect (str/includes? message ".splitlines()"))))))

(defdescribe introspection-public-surface-test
             (it
               "exposes the read, the single descriptor and the index under verb_noun names"
               (let
                 [symbols
                  (set (map :ext.symbol/symbol introspection/all-symbols))

                  read-doc
                  (:doc (meta #'introspection/read-session))

                  get-doc
                  (:doc (meta #'introspection/get-session))

                  list-doc
                  (:doc (meta #'introspection/list-sessions))]

                 (expect (= #{'read-session 'get-session 'list-sessions} symbols))
                 ;; The storage noun is gone from the agent surface: `session_state` is a
                 ;; DB TABLE, never a tool, and the bare plural `sessions` is not a verb.
                 (expect (not (contains? symbols 'session-state)))
                 (expect (not (contains? symbols 'sessions)))
                 (expect (not (contains? symbols 'session-usage)))
                 (expect (not (contains? symbols 'session-report-html)))
                 (expect (str/starts-with?
                           read-doc
                           "await read_session(target=None)  # current session by default"))
                 (expect (str/includes? read-doc "\"usage\""))
                 (expect (re-find #"tool rows overlap" read-doc))
                 (expect (re-find #"recovery path for raw folded current-session" read-doc))
                 (expect (re-find #"does not undo fold intents or restore them" read-doc))
                 (expect (str/starts-with? get-doc "await get_session(target=None)"))
                 (expect (str/starts-with? list-doc "await list_sessions(search=None)"))
                 (expect (re-find #"is_in_title" list-doc))
                 ;; engine-symbol-* tools were retired in favour of the bare
                 ;; `doc` / `apropos` engine system calls.
                 (expect (not (contains? symbols 'engine-symbol-documentation)))
                 (expect (not (contains? symbols 'engine-symbol-apropos)))
                 (expect (= 3 (count symbols)))))
             (it "defaults read_session to the current session when no id is passed"
                 (let
                   [inspect-data
                    @#'introspection/foundation-inspect-data

                    data
                    (inspect-data {:session-id "current-session" :db-info nil} nil)]

                   (expect (= "current-session" (:session-id data)))
                   (expect (contains? data :usage)))))

(defdescribe
  session-usage-ledger-test
  (it
    "returns compact per-turn, per-iteration, per-tool usage, and bounded tool errors in one read"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let
          [sid (h/store-session! s {:channel :tui :title "Usage fixture" :model "gpt-4o"})
           turn (vis/db-store-session-turn!
                  s
                  {:parent-session-id sid :user-request "measure" :status :running})
           _ (h/store-iteration!
               s
               {:session-turn-id turn
                :code "(cat \"fixture\")"
                :forms
                [{:vis/tool-name "cat" :success? false :error {:message "fixture is unavailable"}}]
                :tokens {"input" 100 "output" 20 "reasoning" 3 "cached" 30}
                :cache-created-tokens 40
                :cost-usd 0.0042
                :llm-routing {:selected {:provider :anthropic :model "claude-5"}
                              :actual {:provider :openai :model "gpt-5"}
                              :fallback? true
                              :trace [{:event/type :llm.routing/provider-retry
                                       :status 503
                                       :reason :rate-limit
                                       :attempt 1
                                       :delay-ms 25}
                                      {:event/type :llm.routing/provider-fallback :status 503}]}})
           _ (h/store-iteration! s
                                 {:session-turn-id turn
                                  :code "(python_execution \"fixture\")"
                                  :forms [{:vis/tool-name "cat" :success? true}
                                          {:vis/tool-name "python_execution" :timeout? true}]
                                  :tokens {"input" 80 "output" 10 "reasoning" 2 "cached" 20}
                                  :cache-created-tokens 10
                                  :cost-usd 0.0021
                                  :llm-routing {:selected {:provider :openai :model "gpt-5"}
                                                :actual {:provider :openai :model "gpt-5"}}})
           _ (vis/db-update-session-turn! s turn {:status :done})
           _ (persistance/db-create-extension-aggregate!
               s
               {:extension-id "vis"
                :aggregate-key "manual-switch-fixture"
                :kind :session-model-switch
                :session-soul-id sid
                :content {:from {:provider "anthropic" :model "claude-5"}
                          :to {:provider "openai" :model "gpt-5"}
                          :source :tui}})
           inspect @#'introspection/foundation-inspect
           ledger (get (:result (inspect {:session-id sid :db-info s} sid)) "usage")
           missing-ledger (get (:result (inspect {:session-id "missing" :db-info s} "missing"))
                               "usage")
           turn-row (first (get ledger "turns"))
           iteration-row (first (get turn-row "iterations"))
           cat-row (first (filter #(= "cat" (get % "tool")) (get ledger "tools")))
           python-row (first (filter #(= "python_execution" (get % "tool")) (get ledger "tools")))
           errors (get ledger "tool_errors")
           routing (get ledger "routing")
           selected (get routing "selected")
           actual (get routing "actual")
           transition (first (get routing "transitions"))
           manual-switch (first (get routing "manual_switches"))]

          (expect (= "session_usage" (get ledger "scope")))
          (expect (= 1 (get-in ledger ["totals" "turns"])))
          (expect (= 2 (get-in ledger ["totals" "iterations"])))
          (expect (= 180 (get-in ledger ["totals" "tokens" "input"])))
          (expect (= 50 (get-in ledger ["totals" "tokens" "cached"])))
          (expect (= 130 (get-in ledger ["totals" "tokens" "uncached"])))
          (expect (= 50 (get-in ledger ["totals" "tokens" "cache_created"])))
          (expect (= 30 (get-in ledger ["totals" "tokens" "output"])))
          (expect (= 5 (get-in ledger ["totals" "tokens" "reasoning"])))
          (expect (< (Math/abs (- 0.0063 (double (get-in ledger ["totals" "cost_usd"])))) 1.0E-9))
          (expect (= 3 (get-in ledger ["totals" "tool_calls"])))
          (expect (= 2 (get-in ledger ["totals" "tool_errors"])))
          (expect (= {"done" 1 "error" 1 "timeout" 1} (get-in ledger ["totals" "tool_outcomes"])))
          ;; Routing reports selected-versus-actual service, fallback/retry detail,
          ;; and durable manual picker history without prompts or provider payloads.
          (expect (= 1 (get routing "fallbacks")))
          (expect (= 1 (get routing "retries")))
          (expect (= 2 (count (get routing "events"))))
          (expect (= "llm.routing_provider_retry" (get-in routing ["events" 0 "type"])))
          (expect (= 25 (get-in routing ["events" 0 "delay_ms"])))
          (expect (= 2 (get-in actual [0 "iterations"])))
          (expect (= "openai" (get-in actual [0 "provider"])))
          (expect (= 1 (get-in selected [0 "iterations"])))
          (expect (= {"provider" "anthropic" "model" "claude-5"} (get transition "from")))
          (expect (= {"provider" "openai" "model" "gpt-5"} (get transition "to")))
          (expect (= 1 (get transition "count")))
          (expect (= "tui" (get manual-switch "source")))
          (expect (integer? (get manual-switch "at_ms")))
          ;; Turn totals make per-turn comparisons one read; they must equal the only turn.
          (expect (= 2 (get turn-row "iteration_count")))
          (expect (= 180 (get-in turn-row ["tokens" "input"])))
          (expect (= 50 (get-in turn-row ["tokens" "cached"])))
          (expect (= 130 (get-in turn-row ["tokens" "uncached"])))
          (expect (< (Math/abs (- 0.0063 (double (get turn-row "cost_usd")))) 1.0E-9))
          (expect (= 3 (get turn-row "tool_calls")))
          (expect (= 2 (get turn-row "tool_errors")))
          ;; Per-iteration data retains usage and outcome counts, but never raw errors.
          (expect (= 1 (get iteration-row "tool_call_count")))
          (expect (= 1 (get iteration-row "tool_error_count")))
          (expect (= {"done" 0 "error" 1 "timeout" 0} (get iteration-row "tool_outcomes")))
          (expect (not (contains? iteration-row "tool_call_statuses")))
          ;; Tool rows overlap by iteration: cat receives both costs, python only its own.
          (expect (= 2 (get cat-row "calls")))
          (expect (= 1 (get cat-row "errors")))
          (expect (= {"done" 1 "error" 1 "timeout" 0} (get cat-row "outcomes")))
          (expect (= 2 (get cat-row "iterations")))
          (expect (< (Math/abs (- 0.0063 (double (get cat-row "cost_usd")))) 1.0E-9))
          (expect (= 1 (get python-row "calls")))
          (expect (= 1 (get python-row "errors")))
          (expect (= {"done" 0 "error" 0 "timeout" 1} (get python-row "outcomes")))
          (expect (= 1 (get python-row "iterations")))
          (expect (= 80 (get-in python-row ["tokens" "input"])))
          (expect (= 0.0021 (get python-row "cost_usd")))
          ;; Errors are bounded, positioned, and message-only: no call code or result leaks.
          (expect (= 2 (count errors)))
          (expect (= {"tool" "cat"
                      "turn" 1
                      "iteration" 1
                      "form" 1
                      "status" "error"
                      "message" "fixture is unavailable"}
                     (first errors)))
          (expect (= "timeout" (get (second errors) "status")))
          (expect (= "Tool execution timed out" (get (second errors) "message")))
          (expect (false? (get ledger "tool_errors_truncated")))
          ;; Unknown sessions are a safe, zero-valued ledger rather than an exception.
          (expect (nil? (get missing-ledger "session")))
          (expect (= {"iterations" 0
                      "tokens"
                      {"input" 0 "cached" 0 "uncached" 0 "cache_created" 0 "output" 0 "reasoning" 0}
                      "cost_usd" 0.0
                      "tool_calls" 0
                      "tool_errors" 0
                      "tool_outcomes" {"done" 0 "error" 0 "timeout" 0}
                      "turns" 0}
                     (get missing-ledger "totals")))
          (expect (= [] (get missing-ledger "turns")))
          (expect (= [] (get missing-ledger "tools")))
          (expect (= [] (get missing-ledger "tool_errors")))
          (expect (false? (get missing-ledger "tool_errors_truncated"))))
        (finally (vis/db-dispose-connection! s))))))

(defdescribe session-usage-tool-error-cap-test
             (it "bounds error samples while reporting their truncation"
                 (let
                   [summarize
                    @#'introspection/usage-tool-errors

                    result
                    (summarize [{:tool-call-statuses (mapv (fn [index]
                                                             {:tool "cat"
                                                              :turn 1
                                                              :iteration 1
                                                              :form index
                                                              :status :error
                                                              :error {:message "excess error"}})
                                                           (range 21))}])]

                   (expect (= 20 (count (:tool-errors result))))
                   (expect (:tool-errors-truncated? result))
                   (expect (= "excess error" (get-in result [:tool-errors 0 :message]))))))

(defdescribe read-session-envelope-test
             (it "returns one canonical envelope with the compact usage ledger embedded"
                 (let
                   [inspect
                    @#'introspection/foundation-inspect

                    result
                    (inspect {:session-id nil :db-info nil})

                    data
                    (:result result)]

                   (expect (extension/tool-result? result))
                   ;; Envelope key stays keyword — internal, unwrapped before the boundary.
                   (expect (= :read-session (:symbol result)))
                   (expect (map? data))
                   (expect (contains? data "usage"))
                   (expect (map? (get data "usage"))))))

(defdescribe
  read-session-strings-only-test
  ;; read_session is a MODEL-FACING verb: its `:result` crosses the strings-only
  ;; Clojure->Python boundary, which throws on any keyword/symbol key OR value.
  ;; The verb stringifies once at egress (deep-stringify), so the whole surface —
  ;; including the embedded diagnosis / failures / transcript sub-maps — reads
  ;; snake_case exactly like the old boundary rendered it.
  (it "returns a fully string-keyed result with no keyword keys/values at any depth"
      (let
        [inspect
         @#'introspection/foundation-inspect

         data
         (:result (inspect {:session-id nil :db-info nil}))]

        ;; Top-level keys are snake_case strings (`:schema-version` -> "schema_version").
        (expect (every? string? (keys data)))
        (expect (contains? data "schema_version"))
        ;; Keyword enum VALUES stringify (`:session` -> "session").
        (expect (= "session" (get data "scope")))
        ;; Predicate `?` stripped + kebab->snake (`:repetition-loop?` -> "repetition_loop").
        (expect (contains? (get data "diagnosis") "repetition_loop"))
        ;; The strings-only boundary mirror passes it through WITHOUT throwing —
        ;; the exact contract (a keyword anywhere would throw here).
        (expect (map? (env-python/boundary-view data)))))
  (it
    "a POPULATED read_session (turns/calls/timeline/diagnosis + string-keyed llm maps) crosses the boundary with no keyword leak"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let
          [cid (h/store-session!
                 s
                 {:channel :tui :title "Boundary fixture" :provider :openai :model "gpt-4o"})
           turn (vis/db-store-session-turn!
                  s
                  {:parent-session-id cid :user-request "run a tool" :status :running})
           _ (h/store-iteration!
               s
               {:session-turn-id turn
                :code "(v/tool \"echo hi\")"
                ;; nippy `:forms` (keyword-keyed) alongside `<-json`
                ;; llm maps (string-keyed) — the mixed shape the verb
                ;; must fully stringify at egress.
                :forms [{:scope "t1/i1/f1"
                         :tag :observation
                         :src "(v/tool \"echo hi\")"
                         :result {:success? true
                                  :result {:exit 0 :command "echo hi"}
                                  :info {:op :v/tool
                                         :tool {:symbol 'tool :call "v/tool"}
                                         :command "echo hi"}
                                  :error nil}}]
                :answer "done"
                :llm-messages [{:role "system" :content "SYS"} {:role "user" :content "hi"}]
                :llm-executable-blocks [{:lang "clojure" :source "(v/tool \"echo hi\")"}]
                :duration-ms 10})
           _ (vis/db-update-session-turn! s turn {:status :done :answer-markdown "done"})
           data (:result (@#'introspection/foundation-inspect {:session-id cid :db-info s} cid))]

          ;; The whole model-facing surface survives the strings-only boundary
          ;; mirror — keyword enum values like `:op :v/tool`, `:kind :code`,
          ;; `:role :user`, `:status :done` all had to stringify at egress.
          (expect (map? (env-python/boundary-view data)))
          (expect (every? string? (keys data))))
        (finally (vis/db-dispose-connection! s))))))

(defdescribe list-sessions-envelope-test
             ;; Regression (session 9c829d10): the index verb was the ONE introspection
             ;; verb without the `session-envelope` wrap — it returned the raw vector,
             ;; so `assert-symbol-envelope!` rejected EVERY call ("Symbol 'sessions'
             ;; must return a canonical :envelope map").
             (it "no-arg arity returns a canonical envelope (empty index without a db)"
                 (let
                   [list-sessions
                    @#'introspection/foundation-sessions

                    result
                    (list-sessions {:session-id nil :db-info nil})]

                   (expect (extension/tool-result? result))
                   (expect (= :list-sessions (:symbol result)))
                   (expect (= [] (:result result)))))
             (it "the search arity is enveloped too"
                 (let
                   [list-sessions
                    @#'introspection/foundation-sessions

                    result
                    (list-sessions {:db-info nil} "anything")]

                   (expect (extension/tool-result? result))
                   (expect (= :list-sessions (:symbol result)))
                   (expect (= [] (:result result))))))

(defdescribe
  introspection-toggle-gate-test
  "Session introspection is NOT core policy: symbols AND prompt hang off the
   `introspection` toggle, which is OFF by default."
  (it "registers an `introspection` toggle that defaults OFF"
      (let [spec (first (filter #(= "introspection" (:id %)) (vis/registered-toggles)))]
        (expect (some? spec))
        (expect (false? (boolean (:default spec))))
        (expect (false? (vis/toggle-enabled? "introspection")))))
  (it "owns the session symbols behind an activation-fn bound to that toggle"
      (let
        [ext
         introspection/vis-extension

         activation
         (:ext/activation-fn ext)]

        (expect (= "foundation-introspection" (:ext/name ext)))
        (expect (= (set introspection/all-symbols) (set (:ext.engine/symbols (:ext/engine ext)))))
        (expect (false? (boolean (activation {}))))
        (with-redefs [vis/toggle-enabled? (constantly true)]
          (expect (true? (boolean (activation {})))))))
  (it "keeps gateway-event / read_session guidance out of core, in its own prompt"
      (let
        [text
         ((:ext/prompt-fn introspection/vis-extension) {})

         core
         (var-get #'com.blockether.vis.internal.prompt/CORE_SYSTEM_PROMPT)]

        (expect (str/includes? text "~/.vis/gateway/events/<id>.ndjson"))
        (expect (str/includes? text "await read_session()"))
        (expect (str/includes? text "list_sessions(search="))
        (expect (not (str/includes? core "gateway/events")))
        (expect (not (str/includes? core "read_session")))))
  (it "is not bundled into foundation-core's symbol set"
      (let
        [core-symbols (set (:ext.engine/symbols
                             (:ext/engine
                               com.blockether.vis.internal.foundation.core/vis-extension)))]
        (expect (empty? (filter core-symbols introspection/all-symbols))))))

(defdescribe introspection-env-injection-test
             (it "uses declarative env injection rather than a before middleware shim"
                 (doseq [symbol introspection/all-symbols]
                   (expect (true? (:ext.symbol/inject-env? symbol)))
                   (expect (nil? (:ext.symbol/before-fn symbol))))))

(defdescribe
  inspect-other-session-current-turn-test
  ;; Regression (session 227812d4): `read_session("<other-id>")` built
  ;; `:current-turn` from `env` alone, so inspecting ANOTHER session
  ;; answered with the CALLER's own live turn — its user request, its
  ;; attempts and its cost, filed under the other session's id.
  (it "reports the INSPECTED session's latest turn, not the caller's own"
      (let [s (vis/db-create-connection! :memory)]
        (try (let
               [mine (h/store-session! s {:channel :tui :title "Caller"})
                other (h/store-session! s {:channel :tui :title "Inspected"})
                _ (vis/db-store-session-turn!
                    s
                    {:parent-session-id mine :user-request "my own live request" :status :running})
                other-turn (vis/db-store-session-turn! s
                                                       {:parent-session-id other
                                                        :user-request "their finished request"
                                                        :status :done})
                _ (h/store-iteration! s
                                      {:session-turn-id other-turn
                                       :code "(+ 1 1)"
                                       :forms [{:src "(+ 1 1)" :result 2}]
                                       :duration-ms 1})
                data (:result
                       (@#'introspection/foundation-inspect {:session-id mine :db-info s} other))
                current (get data "current_turn")]

               (expect (map? current))
               (expect (= "their finished request" (get current "user_request")))
               (expect (= (str other-turn) (str (get current "id"))))
               ;; the live iteration pointer is env-local runtime state:
               ;; it describes the caller, so it must not ride along.
               (expect (not (contains? current "iteration"))))
             (finally (vis/db-dispose-connection! s)))))
  (it "still carries the live iteration pointer when inspecting the CURRENT session"
      (let [s (vis/db-create-connection! :memory)]
        (try
          (let
            [cid (h/store-session! s {:channel :tui :title "Caller"})
             turn (vis/db-store-session-turn!
                    s
                    {:parent-session-id cid :user-request "my own live request" :status :running})
             _ (h/store-iteration! s
                                   {:session-turn-id turn
                                    :code "(+ 1 1)"
                                    :forms [{:src "(+ 1 1)" :result 2}]
                                    :duration-ms 1})
             env {:session-id cid :db-info s :turn-state-atom (atom {:iteration {:position 3}})}
             current (get (:result (@#'introspection/foundation-inspect env cid)) "current_turn")]

            (expect (= "my own live request" (get current "user_request")))
            (expect (= 3 (get-in current ["iteration" "current"]))))
          (finally (vis/db-dispose-connection! s))))))

(defdescribe
  read-session-live-turn-ledger-test
  ;; Regression (session e95e1cb2): `read_session()` removed the live turn from
  ;; `session.turns`, and a foreign live turn reported the turn row's stale zero
  ;; instead of the iteration rows already persisted for it.
  (it "returns the live turn and its persisted iterations consistently"
      (let [s (vis/db-create-connection! :memory)]
        (try (let
               [cid (h/store-session! s {:channel :tui :title "Live fixture"})
                turn (vis/db-store-session-turn!
                       s
                       {:parent-session-id cid :user-request "still working" :status :running})
                _ (doseq [position (range 1 3)]
                    (h/store-iteration! s
                                        {:session-turn-id turn
                                         :code (str "(+ " position " 1)")
                                         :forms [{:src (str "(+ " position " 1)")
                                                  :result (inc position)}]
                                         :duration-ms 1}))
                env {:session-id cid
                     :db-info s
                     :turn-state-atom (atom {:session-turn-id turn :iteration {:position 3}})}
                data (:result (@#'introspection/foundation-inspect env cid))
                session (get data "session")
                summary-turn (first (get session "turns"))
                transcript-turn (first (get-in data ["transcript" "turns"]))]

               (expect (= 1 (get session "turn_count")))
               (expect (= (str turn) (str (get summary-turn "id"))))
               (expect (= 2 (get summary-turn "iteration_count")))
               (expect (= 2 (get transcript-turn "iteration_count"))))
             (finally (vis/db-dispose-connection! s))))))

(defdescribe
  iteration-form-error-ledger-test
  ;; Regression (session 227812d4): `:failures`, `:diagnosis` and every
  ;; attempt's `:error` were read off the ITERATION row (`(:error iteration)`),
  ;; a key the projection never produces — errors and results live on the
  ;; iteration's `:forms`. So a session full of blown-up python blocks still
  ;; reported `failure_count 0` and `"error": null` on every attempt.
  (it "surfaces an error carried on an iteration form"
      (let [s (vis/db-create-connection! :memory)]
        (try (let
               [cid (h/store-session! s {:channel :tui :title "Failure fixture"})
                turn (vis/db-store-session-turn!
                       s
                       {:parent-session-id cid :user-request "read the palette" :status :running})
                code "mm = r[\"matches\"][\"palettes.ts\"]"
                _ (h/store-iteration! s
                                      {:session-turn-id turn
                                       :code code
                                       :forms [{:src code
                                                :vis/tool-name "python_execution"
                                                :error {:message "KeyError: 'palettes.ts'"}}]
                                       :duration-ms 5})
                data (:result
                       (@#'introspection/foundation-inspect {:session-id cid :db-info s} cid))
                failure (first (get data "failures"))
                attempt (first (get-in data ["current_turn" "attempts"]))]

               (expect (= 1 (get-in data ["diagnosis" "failure_count"])))
               (expect (= "code" (get failure "source")))
               (expect (= code (get failure "code")))
               (expect (str/includes? (get failure "message") "KeyError"))
               (expect (= "python_execution" (get failure "tool")))
               ;; and the raw attempt keeps its error, so the documented
               ;; [a for a in attempts if a["error"]] derivation works.
               (expect (some? (get attempt "error")))
               (expect (= code (get attempt "code"))))
             (finally (vis/db-dispose-connection! s))))))

;; Regression, issue #130: a cancel interrupt was attributed to the form that
;; happened to be on the stack, making a user stop look like broken agent code.
(defdescribe cancel-interrupt-classification-test
             (let
               [classify
                @#'introspection/classify-expression-failure

                advice
                @#'introspection/advice-for-classification]

               (it "reports cancellation fallout instead of a code failure"
                   (expect (= :turn-cancelled
                              (classify "python_execution(...)" "java.lang.InterruptedException")))
                   (expect (= :turn-cancelled
                              (classify "grep(...)" "java.util.concurrent.CancellationException")))
                   (expect (str/includes? (advice :turn-cancelled) "cancelled")))
               (it "keeps genuine agent failures classified as code errors"
                   (expect (= :code-execution-error
                              (classify "python_execution(...)" "NameError: name 'x'"))))))

;; ---------------------------------------------------------------------------
;; The renamed surface's two new behaviours: `list_sessions(search=…)` is the
;; SAME ranked search the TUI and the companion app run, and `get_session` is
;; the single-row read that used to be missing between the index and the
;; whole transcript.
;; ---------------------------------------------------------------------------

(defdescribe
  list-sessions-search-test
  (it "answers `search` in the SERVER's ranked order, tagged with where it hit"
      (let [s (vis/db-create-connection! :memory)]
        (try (let
               [titled (h/store-session! s {:channel :tui :title "the needle title"})
                spoken (h/store-session! s {:channel :tui :title "something else"})
                _ (vis/db-store-session-turn!
                    s
                    {:parent-session-id spoken :user-request "find the needle" :status :done})
                _ (h/store-session! s {:channel :tui :title "unrelated"})
                rows (@#'introspection/foundation-sessions-data {:db-info s} "needle")
                by-id (into {} (map (juxt #(str (:id %)) identity)) rows)
                titled-row (get by-id (str titled))
                spoken-row (get by-id (str spoken))]

               ;; Title band (0) before the request band (1); the third session
               ;; never matched, so it is simply absent.
               (expect (= [(str titled) (str spoken)] (mapv #(str (:id %)) rows)))
               (expect (= 0 (:rank titled-row)))
               (expect (true? (:is-in-title titled-row)))
               (expect (= 1 (:rank spoken-row)))
               (expect (true? (:is-in-request spoken-row)))
               (expect (false? (:is-in-title spoken-row)))
               ;; A search row is still an INDEX row - same keys, no transcript.
               (expect (= "the needle title" (:title titled-row)))
               (expect (= 1 (:turn-count spoken-row)))
               (expect (not (contains? titled-row :transcript))))
             (finally (vis/db-dispose-connection! s)))))
  (it "a blank or absent search is the plain newest-first index"
      (let [s (vis/db-create-connection! :memory)]
        (try (let
               [_ (h/store-session! s {:channel :tui :title "one"})
                _ (h/store-session! s {:channel :tui :title "two"})
                index (@#'introspection/foundation-sessions-data {:db-info s})]

               (expect (= 2 (count index)))
               (expect (= index (@#'introspection/foundation-sessions-data {:db-info s} nil)))
               (expect (= index (@#'introspection/foundation-sessions-data {:db-info s} "   ")))
               ;; No search means no ranking keys to paint.
               (expect (every? #(not (contains? % :rank)) index)))
             (finally (vis/db-dispose-connection! s))))))

(defdescribe
  get-session-descriptor-test
  (it "answers ONE row - identity, counts and the last turn - with no transcript"
      (let [s (vis/db-create-connection! :memory)]
        (try (let
               [cid (h/store-session!
                      s
                      {:channel :tui :title "Descriptor fixture" :provider :openai :model "gpt-4o"})
                _ (vis/db-store-session-turn!
                    s
                    {:parent-session-id cid :user-request "first ask" :status :done})
                _ (vis/db-store-session-turn!
                    s
                    {:parent-session-id cid :user-request "second ask" :status :running})
                row
                (@#'introspection/foundation-session-descriptor {:db-info s :session-id cid} cid)]

               (expect (= (str cid) (str (:id row))))
               (expect (= "Descriptor fixture" (:title row)))
               (expect (= 2 (:turn-count row)))
               (expect (true? (:is-current row)))
               (expect (= "openai/gpt-4o" (:provider-model row)))
               (expect (= "second ask" (get-in row [:last-turn :user-request])))
               ;; The whole point of the middle read: no transcript, no per-turn roll-up.
               (expect (not (contains? row :transcript)))
               (expect (not (contains? row :turns))))
             (finally (vis/db-dispose-connection! s)))))
  (it "defaults to the current session and answers nil for an unknown target"
      (let [s (vis/db-create-connection! :memory)]
        (try (let
               [cid (h/store-session! s {:channel :tui :title "Default fixture"})
                env {:db-info s :session-id cid}]

               (expect (= (str cid)
                          (str (:id (@#'introspection/foundation-session-descriptor env)))))
               (expect (false? (:is-current (@#'introspection/foundation-session-descriptor
                                             {:db-info s :session-id nil}
                                             cid))))
               (expect (nil? (@#'introspection/foundation-session-descriptor
                              env
                              "00000000-0000-0000-0000-000000000000"))))
             (finally (vis/db-dispose-connection! s))))))

(defdescribe session-verb-keyword-argument-test
             ;; A Python KEYWORD call crosses as ONE trailing dict whose keys are verbatim
             ;; STRINGS, so `read_session(target=…)` / `list_sessions(search=…)` must bind
             ;; exactly like the positional call rather than treating the dict as an id.
             (let
               [target-arg
                @#'introspection/target-arg

                search-arg
                @#'introspection/search-arg]

               (it "unwraps the trailing kwargs dict for both reads"
                   (expect (= "abc" (target-arg "abc")))
                   (expect (= "abc" (target-arg {"target" "abc"})))
                   (expect (= "abc" (target-arg {"session_id" "abc"})))
                   (expect (= "abc" (target-arg {"id" "abc"})))
                   (expect (nil? (target-arg nil)))
                   (expect (= "needle" (search-arg "needle")))
                   (expect (= "needle" (search-arg {"search" "needle"})))
                   (expect (= "needle" (search-arg {"query" "needle"}))))
               (it "takes the marked id a copy affordance puts on the clipboard"
                   ;; The TUI header chip and the companion app copy
                   ;; `vis_session_id#<uuid>` so the id says what it addresses;
                   ;; pasting that verbatim must read the session, not miss.
                   (expect (= "abc" (target-arg "vis_session_id#abc")))
                   (expect (= "abc" (target-arg {"target" "vis_session_id#abc"})))
                   (expect (= "abc" (target-arg {"session_id" "vis_session_id#abc"}))))
               (it "binds a keyword search end-to-end through the enveloped verb"
                   (let [s (vis/db-create-connection! :memory)]
                     (try (let
                            [cid (h/store-session! s {:channel :tui :title "kwarg needle"})
                             rows (:result (@#'introspection/foundation-sessions
                                            {:db-info s}
                                            {"search" "needle"}))]

                            (expect (= [(str cid)] (mapv #(str (get % "id")) rows))))
                          (finally (vis/db-dispose-connection! s)))))))
