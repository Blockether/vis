(ns com.blockether.vis.internal.diagnose-test
  "Coverage for `internal/diagnose` \u2014 the renderer behind the
   `vis diagnose <conv-id>` CLI subcommand. Drives a real in-memory
   SQLite store via the persistance facade so the test exercises the
   same join the CLI does end-to-end. No mocks, no network, no
   filesystem."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.diagnose :as diagnose]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- seed-conversation!
  "Build a conversation that mirrors the real-world REPRODUCTION.md
   case: two turns, one with a successful iteration, one with a
   block-level failure. Returns the conversation id."
  [s]
  (let [cid (vis/db-store-conversation! s {:channel :tui
                                           :title "Diagnose fixture"
                                           :model "gpt-4o"
                                           :system-prompt "sys"})
        q1  (vis/db-store-query! s {:parent-conversation-id cid
                                    :query "First turn"
                                    :status :running})]
    ;; Turn 1: one clean iteration with token + cost data.
    (vis/db-store-iteration! s {:query-id q1
                                :blocks [{:code "(+ 1 1)" :result 2 :execution-time-ms 5}]
                                :duration-ms 12
                                :tokens   {:input 100 :output 20 :reasoning 0 :cached 30}
                                :cost-usd 0.0042})
    (vis/db-update-query! s q1 {:status :done})
    ;; Turn 2: one iteration with a failing block (mirrors the
    ;; unresolved-symbol class from REPRODUCTION.md).
    (let [q2 (vis/db-store-query! s {:parent-conversation-id cid
                                     :query "Second turn that fails"
                                     :status :running})]
      (vis/db-store-iteration! s {:query-id q2
                                  :blocks [{:code "Let"
                                            :error "ExceptionInfo: Unable to resolve symbol: Let"}
                                           {:code "(+ 1 1)" :result 2}]
                                  :duration-ms 9
                                  :tokens   {:input 80 :output 10 :reasoning 0 :cached 20}
                                  :cost-usd 0.0021})
      (vis/db-update-query! s q2 {:status :error}))
    cid))

(defdescribe diagnose-render-test
  (it "returns a 'not found' line for a missing conversation id"
    (let [s   (vis/db-create-connection! :memory)
          out (try (diagnose/render s "00000000-0000-0000-0000-000000000000")
                (finally (vis/db-dispose-connection! s)))]
      (expect (string? out))
      (expect (str/includes? out "Conversation not found"))))

  (it "renders a header with conversation metadata"
    (let [s   (vis/db-create-connection! :memory)
          cid (seed-conversation! s)
          out (try (diagnose/render s cid)
                (finally (vis/db-dispose-connection! s)))]
      (expect (str/includes? out (str "conversation `" cid "`")))
      (expect (str/includes? out "Title:** Diagnose fixture"))
      (expect (str/includes? out "Channel:** tui"))
      (expect (str/includes? out "Total turns:** 2"))
      (expect (str/includes? out "Total iterations:** 2"))))

  (it "renders one turn block per query_soul, with status + iter count"
    (let [s   (vis/db-create-connection! :memory)
          cid (seed-conversation! s)
          out (try (diagnose/render s cid)
                (finally (vis/db-dispose-connection! s)))]
      ;; Both goals appear in their own turn block.
      (expect (str/includes? out "Goal:** First turn"))
      (expect (str/includes? out "Goal:** Second turn that fails"))
      ;; Status label is the literal status keyword name.
      (expect (str/includes? out "Status:** done"))
      (expect (str/includes? out "Status:** error"))
      ;; Iteration counter is populated, not stuck at 0 (the
      ;; REPRODUCTION.md regression).
      (expect (str/includes? out "Iterations:** 1"))))

  (it "renders the per-iteration token + cost summary on each turn"
    (let [s   (vis/db-create-connection! :memory)
          cid (seed-conversation! s)
          out (try (diagnose/render s cid)
                (finally (vis/db-dispose-connection! s)))]
      (expect (str/includes? out "Tokens (in/out):** 100/20"))
      (expect (str/includes? out "Tokens (in/out):** 80/10"))
      (expect (str/includes? out "$0.0042"))
      (expect (str/includes? out "$0.0021"))))

  (it "lists block-level failures in the per-turn Failures table"
    (let [s   (vis/db-create-connection! :memory)
          cid (seed-conversation! s)
          out (try (diagnose/render s cid)
                (finally (vis/db-dispose-connection! s)))]
      ;; The failing block from turn 2 must surface, code + error.
      (expect (str/includes? out "Unable to resolve symbol: Let"))
      ;; The Failures count for the failing turn is 1, the clean turn
      ;; reports 0.
      (expect (str/includes? out "Failures:** 1"))
      (expect (str/includes? out "Failures:** 0"))))

  (it "appends a global Failure log table when any block-level error happened"
    (let [s   (vis/db-create-connection! :memory)
          cid (seed-conversation! s)
          out (try (diagnose/render s cid)
                (finally (vis/db-dispose-connection! s)))]
      (expect (str/includes? out "## Failure log"))
      (expect (str/includes? out "Unable to resolve symbol: Let"))))

  (it "omits the Failure log when every iteration was clean"
    (let [s   (vis/db-create-connection! :memory)
          cid (vis/db-store-conversation! s {:channel :tui :title "Clean"
                                             :model "gpt-4o"})
          q   (vis/db-store-query! s {:parent-conversation-id cid
                                      :query "all good"
                                      :status :running})
          _   (vis/db-store-iteration! s {:query-id q
                                          :blocks [{:code "(+ 1 1)" :result 2}]
                                          :duration-ms 1
                                          :tokens   {:input 10 :output 5}
                                          :cost-usd 0.0001})
          _   (vis/db-update-query! s q {:status :done})
          out (try (diagnose/render s cid)
                (finally (vis/db-dispose-connection! s)))]
      (expect (string? out))
      (expect (not (str/includes? out "## Failure log"))))))
