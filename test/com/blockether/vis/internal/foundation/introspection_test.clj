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

(defdescribe patch-diagnosis-contract-test
             (let
               [classify
                @#'introspection/classify-expression-failure

                advice
                @#'introspection/advice-for-classification]

               (it "classifies stale anchors and recommends the anchor-only contract"
                   (expect (= :patch-stale-anchor
                              (classify "patch([...])" "anchors no longer match the file")))
                   (let [message (advice :patch-stale-anchor)]
                     (expect (str/includes? message "lineno:hash"))
                     (expect (not (str/includes? message "SEARCH")))))))

(defdescribe
  introspection-public-surface-test
  (it "exposes only the session introspection symbols (symbol docs moved to engine `doc`/`apropos`)"
      (let [symbols (set (map :ext.symbol/symbol introspection/all-symbols))]
        (expect (contains? symbols 'session-state))
        (expect (contains? symbols 'session-usage))
        (expect (contains? symbols 'session-report-html))
        (expect (contains? symbols 'sessions))
        (let
          [session-state-doc (:doc (meta #'introspection/session-state))
           session-usage-doc (:doc (meta #'introspection/session-usage))]

          (expect (str/starts-with?
                    session-state-doc
                    "await session_state(session_id=None)  # current session by default"))
          (expect (re-find #"recovery path for raw folded current-session" session-state-doc))
          (expect (re-find #"does not undo fold intents or restore them" session-state-doc))
          (expect (str/starts-with? session-usage-doc "await session_usage(session_id=None)"))
          (expect (re-find #"Tool rows overlap" session-usage-doc)))
        ;; engine-symbol-* tools were retired in favour of the bare
        ;; `doc` / `apropos` engine system calls.
        (expect (not (contains? symbols 'engine-symbol-documentation)))
        (expect (not (contains? symbols 'engine-symbol-apropos)))
        (expect (= 4 (count symbols)))))
  (it "defaults session_state to the current session when no id is passed"
      (let
        [inspect-data
         @#'introspection/foundation-inspect-data

         data
         (inspect-data {:session-id "current-session" :db-info nil} nil)]

        (expect (= "current-session" (:session-id data))))))

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
           usage @#'introspection/foundation-usage
           ledger (:result (usage {:session-id sid :db-info s} sid))
           missing-ledger (:result (usage {:session-id "missing" :db-info s} "missing"))
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

(defdescribe session-state-envelope-test
             (it "returns a canonical envelope so observed symbol wrapping can unwrap it"
                 (let
                   [inspect
                    @#'introspection/foundation-inspect

                    result
                    (inspect {:session-id nil :db-info nil})]

                   (expect (extension/tool-result? result))
                   ;; Envelope key stays keyword — internal, unwrapped before the boundary.
                   (expect (= :session-state (:symbol result)))
                   (expect (map? (:result result))))))

(defdescribe
  session-state-strings-only-test
  ;; session_state is a MODEL-FACING verb: its `:result` crosses the strings-only
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
    "a POPULATED session_state (turns/calls/timeline/diagnosis + string-keyed llm maps) crosses the boundary with no keyword leak"
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

(defdescribe sessions-envelope-test
             ;; Regression (session 9c829d10): `sessions()` was the ONE introspection
             ;; verb without the `session-envelope` wrap — it returned the raw vector,
             ;; so `assert-symbol-envelope!` rejected EVERY call ("Symbol 'sessions'
             ;; must return a canonical :envelope map").
             (it "no-arg arity returns a canonical envelope (empty index without a db)"
                 (let
                   [sessions
                    @#'introspection/foundation-sessions

                    result
                    (sessions {:session-id nil :db-info nil})]

                   (expect (extension/tool-result? result))
                   (expect (= :sessions (:symbol result)))
                   (expect (= [] (:result result)))))
             (it "channel-filtered arity is enveloped too"
                 (let
                   [sessions
                    @#'introspection/foundation-sessions

                    result
                    (sessions {:db-info nil} :tui)]

                   (expect (extension/tool-result? result))
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
  (it "keeps gateway-event / session_state guidance out of core, in its own prompt"
      (let
        [text
         ((:ext/prompt-fn introspection/vis-extension) {})

         core
         (var-get #'com.blockether.vis.internal.prompt/CORE_SYSTEM_PROMPT)]

        (expect (str/includes? text "~/.vis/gateway/events/<id>.ndjson"))
        (expect (str/includes? text "await session_state()"))
        (expect (not (str/includes? core "gateway/events")))
        (expect (not (str/includes? core "session_state")))))
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
