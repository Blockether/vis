(ns seed
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.foundation.introspection :as inspect]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.core :as sql]
            [taoensso.nippy :as nippy]))

(def sid "10200000-0000-4000-8000-000000000001")
(def peer "10200000-0000-4000-8000-000000000002")
(def ws "10200000-0000-4000-8000-000000000003")
(def st "10200000-0000-4000-8000-000000000004")
(def now 1746139200000)

(defn- ident [tag n]
  (format "10200000-0000-4000-8%03d-%012d" (long tag) (long n)))

(defn- row! [db table row]
  (sql/execute! db {:insert-into table :values [row]}))

(defn- settled? [kind n]
  (not (and (= n 2) (#{"wait" "log"} kind))))

(defn- seed! [db root kind]
  (row! db :workspace {:id ws :repo_id "benchmark" :root root :created_at now})
  (row! db :session_soul {:id sid :created_at now :claimed_at now})
  (row! db :session_state {:id st :session_soul_id sid :workspace_id ws
                           :title "Synthetic release history" :version 0 :created_at now})
  (when (#{"batch" "origin"} kind)
    (row! db :session_soul {:id peer :created_at now :claimed_at now})
    (doseq [[id message] [[11 "Begin the replay inspection."]
                          [12 "Replay inspection complete."]]]
      (row! db :council_entry {:id id :group_id sid :author_sid peer :activation_id "benchmark"
                               :title (str "Replay report " id) :content message
                               :source "host" :created_at (+ now id)
                               :fingerprint (str id) :idempotency_key (str id)})))
  (doseq [n (range 1 17)
          :let [soul (ident 1 n) run (ident 2 n)
                batch? (and (= n 2) (#{"batch" "origin"} kind))
                status (if (settled? kind n) "done" "error")
                final (if (= n 2) "approved-after-recheck" "checks complete")]]
    (row! db :session_turn_soul (cond-> {:id soul :session_state_id st :position n
                                         :created_at (+ now (* n 10000)) :user_request "Check the release."}
                                   batch? (assoc :request_kind "council" :council_entry_id 11)))
    (when batch?
      (row! db :session_turn_council_entry {:session_turn_soul_id soul :position 2 :council_entry_id 12}))
    (row! db :session_turn_state {:id run :session_turn_soul_id soul :version 0
                                  :status status :prior_outcome (if (= status "done") "complete" "error")
                                  :iteration_count 3 :duration_ms 50000 :input_tokens 1500
                                  :input_regular_tokens 1200 :input_cache_read_tokens 300
                                  :output_tokens 120 :llm_root_provider "openai" :llm_root_model "demo-large"
                                  :content_json (wire/json-str [{:type "text" :text final}])
                                  :created_at (+ now (* n 10000))})
    (doseq [i (range 1 4)
            :let [inserted (+ now (* n 10000) (* i 1000))
                  fallback? (and (= kind "route") (= n 2))
                  stdout (if (and (= kind "block") (= n 2) (= i 2))
                           "Verification code: GLASS-713\n"
                           (str "Verification code: DECOY-" n "-" i "\n"))
                  body (str stdout (when (and (= kind "block") (not= n 2))
                                     (str/join "\n" (for [k (range 350)] (format "sample-%04d|count=%03d" k (mod (* k n) 53))))))]]
      (row! db :session_turn_iteration
            {:id (ident 3 (+ (* n 10) i)) :session_turn_state_id run :position i
             :status (if (= i 3) status "done") :created_at inserted :finished_at (+ inserted 100)
             :llm_selected_provider "openai" :llm_selected_model "demo-large"
             :llm_actual_provider (if fallback? "anthropic" "openai")
             :llm_actual_model (if fallback? "demo-mini" "demo-large") :is_llm_fallback (if fallback? 1 0)
             :llm_assistant_prose (if (= i 3) final "Draft verdict: provisional-reject")
             :input_tokens 500 :input_regular_tokens 400 :input_cache_read_tokens 100 :output_tokens 40
             :request_health (when (and (= kind "block") (= n 2) (= i 2))
                               (nippy/freeze {:counted-projection :prepared-request
                                              :estimated-input-tokens 490
                                              :breakdown [{:label "Vis core system prompt" :tokens 300}
                                                          {:label "Built-in tools and rules" :tokens 190}]}))
             :code (if (= i 3) "" "print('verification generated')")
             :tool_calls (when (< i 3) (nippy/freeze [{:src "print('verification generated')"
                                                       :tool-name "py3" :stdout body :success? true}]))}))))

(defn- seed-handoff! [db root]
  (row! db :workspace {:id ws :repo_id "benchmark" :root root :created_at now})
  (row! db :session_soul {:id sid :created_at now :claimed_at now})
  (row! db :session_state {:id st :session_soul_id sid :workspace_id ws
                            :title "Release ledger" :version 0 :created_at now})
  (doseq [n (range 1 25)
          :let [turn (ident 1 n) run (ident 2 n) created (+ now (* n 10000))
                request (if (= n 1) "Which transfer value did the release board approve?"
                          (str "Summarize inventory batch " n ". Keep the rows in their original order."))
                answer (if (= n 1) "The release board approved the transfer value: VIOLET-9231-ORBIT."
                         (str/join "\n" (for [i (range 24)]
                                            (format "Inventory batch %d row %d: sealed container, inspection complete, no change." n i))))]]
    (row! db :session_turn_soul {:id turn :session_state_id st :position n
                                 :created_at created :user_request request})
    (row! db :session_turn_state {:id run :session_turn_soul_id turn :version 0 :status "done"
                                  :prior_outcome "complete" :iteration_count 1
                                  :content_json (wire/json-str [(content/prose (str "bench-answer-" n) answer)])
                                  :created_at created})
    (row! db :session_turn_iteration {:id (ident 3 n) :session_turn_state_id run :position 1
                                      :status "done" :code "" :created_at created :finished_at (+ created 100)})))

(defn- answer [db kind]
  (let [env {:db-info db :session-id sid}
        initial (#'inspect/foundation-inspect-data env sid)
        scoped? (= "index" (:view initial))
        view (case kind
               "final" "full"
               "batch" "council"
               ("wait" "origin" "task") "events"
               "blocks")
        result (#'inspect/deep-stringify
                 (if scoped?
                   (#'inspect/foundation-inspect-data env sid (cond-> {"view" view}
                                                                (= view "blocks") (assoc "turn" 2 "iteration" 2)
                                                                (#{"council" "events"} view) (assoc "turn" 2)))
                   initial))
        turn (get-in result ["transcript" "turns" (if (and scoped? (= view "blocks")) 0 1)])
        iter (get-in turn ["iterations" (if (and scoped? (= view "blocks")) 0 1)])
        events (get result "events")]
    (case kind
      "route" (select-keys (or (get iter "llm_actual") iter) ["provider" "model"])
      "block" {"verification_code" (second (re-find #"Verification code: ([A-Z]+-[0-9]+)"
                                            (get-in iter ["blocks" 0 "stdout"])))}
      "final" {"verdict" (get-in turn ["content" 0 "text"])}
      "batch" (let [entries (get-in (if scoped? result turn) ["council" "entries"])]
                {"author_session_id" (get-in entries [0 "author_session_id"])
                 "entry_count" (count entries) "second_message" (get-in entries [1 "content"])})
      "wait" (when scoped?
               (select-keys (last (filter #(and (= "turn.progress" (get % "type"))
                                                (= 2 (get % "iteration"))) events))
                            ["connection" "silent_ms"]))
      "origin" (when scoped?
                 (select-keys (first (filter #(= "turn.started" (get % "type")) events)) ["request_kind"]))
      "task" (when scoped?
               (let [row (first (filter #(= "task.ended" (get % "type")) events))]
                 (select-keys (or (get row "task") row) ["state"])))
      nil)))

(let [database (System/getenv "VIS_BENCH_DB")
      root (System/getenv "VIS_BENCH_ROOT")
      kind (System/getenv "VIS_BENCH_KIND")
      db (ps/db-create-connection! database)]
  (try
    (do (if (= kind "handoff") (seed-handoff! db root) (seed! db root kind))
        (when (System/getenv "VIS_BENCH_CHECK_SEED")
          (when-let [proof (if (= kind "handoff")
                             (let [trailer (transcript/prior-turn-trailer {:db-info db :session-id sid} nil 25 [])
                                   early (get-in (some #(when (= "t1/closing" (first %)) %) trailer)
                                                 [1 :turn/messages 0 :content] "")
                                   later (str/join "\n" (for [[_ entry] trailer
                                                               :when (> (:turn/position entry 0) 1)
                                                               msg (:turn/messages entry)]
                                                           (:content msg)))]
                               {:early_has_value (str/includes? early "VIOLET-9231-ORBIT")
                                :later_has_value (str/includes? later "VIOLET-9231-ORBIT")
                                :later_tokens (svar/count-tokens "gpt-4o" later)})
                             (answer db kind))]
            (println (wire/json-str (wire/->wire proof))))))
    (finally (ps/db-dispose-connection! db))))
(shutdown-agents)
