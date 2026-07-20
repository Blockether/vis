(ns com.blockether.vis.internal.foundation.introspection-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.introspection :as introspection]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.env-python :as env-python]
            [com.blockether.vis.ext.persistance-sqlite.test-helpers :as h]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe patch-diagnosis-contract-test
             (let [classify
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
        (expect (contains? symbols 'session-report-html))
        (expect (contains? symbols 'sessions))
        (expect (re-find #"recovery path for a folded current-session"
                         (:doc (meta #'introspection/session-state))))
        ;; engine-symbol-* tools were retired in favour of the bare
        ;; `doc` / `apropos` engine system calls.
        (expect (not (contains? symbols 'engine-symbol-documentation)))
        (expect (not (contains? symbols 'engine-symbol-apropos)))
        (expect (= 3 (count symbols))))))

(defdescribe session-state-envelope-test
             (it "returns a canonical envelope so observed symbol wrapping can unwrap it"
                 (let [inspect
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
      (let [inspect
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
        (let [cid (h/store-session!
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
                 (let [sessions
                       @#'introspection/foundation-sessions

                       result
                       (sessions {:session-id nil :db-info nil})]

                   (expect (extension/tool-result? result))
                   (expect (= :sessions (:symbol result)))
                   (expect (= [] (:result result)))))
             (it "channel-filtered arity is enveloped too"
                 (let [sessions
                       @#'introspection/foundation-sessions

                       result
                       (sessions {:db-info nil} :tui)]

                   (expect (extension/tool-result? result))
                   (expect (= [] (:result result))))))
