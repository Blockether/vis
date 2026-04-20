(ns com.blockether.vis.loop.runtime.query.subquery
  "Sub-RLM iterated execution. Wraps iteration-loop with a constructed
   system prompt (from skill bodies) and a FORKED SCI context — sub-RLM
   defs don't leak back to the parent sandbox. Sub-queries are parented
   to the invoking iteration in the DB so the full recursion tree is
   recoverable:

     :conversation
       └ :query (main)
           └ :iteration
               └ :query (sub-RLM)          ← this file's doing
                   └ :iteration
                       └ :iteration-var"
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [com.blockether.vis.loop.knowledge.skills :as rlm-skills]
   [com.blockether.vis.loop.storage.db :as rlm-db]
   [sci.core :as sci]
   [taoensso.trove :as trove]))

(defn- read-sandbox-var
  "Read a var out of the given rlm-env's SCI sandbox. Returns the
   value (deref'd if it's an IDeref) or nil."
  [rlm-env sym]
  (try
    (let [sandbox-map (get-in @(:env (:sci-ctx rlm-env)) [:namespaces 'sandbox])
          v (get sandbox-map sym)]
      (when v
        (if (instance? clojure.lang.IDeref v) @v v)))
    (catch Throwable _ nil)))

(defn build-sub-rlm-handoff
  "Assemble the `[parent handoff]` block prefixed to a sub-RLM's
   system prompt. The agent inside the sub-RLM sees, at the top of
   every turn:

     [parent handoff]
       parent *query*     : <what the main/enclosing RLM was asked>
       parent *reasoning* : <its latest thinking>
       parent *answer*    : <set only when the parent just finalized>
     [sub-task]
       <your prompt>

   Without this block the sub-RLM has no idea WHY it's being invoked
   or what context it inherits. Returns nil when the parent env has
   no readable SCI state (fresh env, testing mode)."
  [parent-env sub-prompt]
  (let [parent-query     (read-sandbox-var parent-env '*query*)
        parent-reasoning (read-sandbox-var parent-env '*reasoning*)
        parent-answer    (read-sandbox-var parent-env '*answer*)
        lines (cond-> []
                parent-query     (conj (str "  parent *query*     : "
                                         (str/trim (str parent-query))))
                parent-reasoning (conj (str "  parent *reasoning* : "
                                         (str/trim (str parent-reasoning))))
                parent-answer    (conj (str "  parent *answer*    : "
                                         (str/trim (let [a parent-answer]
                                                     (if (map? a) (str (:result a)) (str a)))))))]
    (when (seq lines)
      (str "[parent handoff]\n"
        (str/join "\n" lines)
        "\n[sub-task]\n  "
        (str/trim (str sub-prompt))))))

(defn fork-rlm-env-for-sub
  "Return a sub-RLM env derived from `parent-env`:

   - `:sci-ctx` is forked via `sci/fork` — writes go to a private SCI
     namespace; the parent's sandbox stays untouched when the sub-RLM
     mutates vars or defines new ones.
   - `:var-index-atom` is fresh — the fork has its own var-index cache
     state so parent's cached rendering isn't invalidated by sub-RLM's
     activity.
   - `:parent-iteration-ref` is set when supplied so the iteration-loop
     can parent the sub-query entity to the invoking iteration rather
     than the conversation.

   Everything else (`:router`, `:db-info`, `:conversation-ref`,
   `:tool-registry-atom`, `:sub-rlm-query-fn`) is shared — these are
   process-scoped resources, not per-turn state."
  [parent-env parent-iteration-ref]
  (cond-> (assoc parent-env
            :sci-ctx (sci/fork (:sci-ctx parent-env))
            :var-index-atom (atom {:index nil :revision -1 :current-revision 0}))
    parent-iteration-ref (assoc :parent-iteration-ref parent-iteration-ref)))

(defn- auto-refine-async!
  "Fires an async skill refinement pass after a sub-rlm-query with skills.
   Non-blocking — runs in a future, never delays the response."
  [iteration-loop-fn rlm-env skills-loaded sub-result]
  (when (and iteration-loop-fn
          (seq skills-loaded)
          (:skill-registry-atom rlm-env)
          (:db-info rlm-env))
    (let [status (:status sub-result)
          confidence (get-in sub-result [:result :confidence])
          needs-refine? (or (some? status)
                          (= confidence :low)
                          (and (nil? (:content sub-result))
                            (nil? (:result sub-result))))]
      (when needs-refine?
        (future
          (try
            (let [skill-name (first skills-loaded)
                  skill-registry-atom (:skill-registry-atom rlm-env)
                  skill (get @skill-registry-atom skill-name)
                  skill-manage-fn rlm-skills/skill-manage
                  trace-summary (cond-> {:status status
                                         :iterations (:iter sub-result)
                                         :confidence confidence}
                                  (:content sub-result)
                                  (assoc :answer-preview
                                    (let [c (:content sub-result)]
                                      (if (> (count c) 200) (subs c 0 200) c))))
                  make-sub-rlm iteration-loop-fn
                  refine-prompt (str "A skill named :" (name skill-name)
                                  " was used and produced this outcome:\n"
                                  (pr-str trace-summary)
                                  "\n\nCurrent skill abstract: " (pr-str (:abstract skill))
                                  "\n\nCurrent skill body (procedure):\n" (:body skill)
                                  "\n\n---\nAnalyze the outcome. Return a JSON-like map with TWO keys:\n"
                                  "1. \"abstract\" — better abstract (<=200 chars). More accurate for what the skill does/when it works. Note limitations if it failed.\n"
                                  "2. \"body-patch\" — if the procedure needs fixing, return {\"old\": \"exact text to replace\", \"new\": \"replacement\"}. If procedure is fine, return null.\n"
                                  "Example: {\"abstract\": \"OCR image PDFs, fails on encrypted\", \"body-patch\": {\"old\": \"step 3 text\", \"new\": \"revised step 3\"}}\n"
                                  "Return ONLY the map, no explanation.")
                  refine-result (make-sub-rlm
                                  rlm-env
                                  refine-prompt
                                  {:max-iterations 1
                                   :cancel-atom (atom false)
                                   :max-consecutive-errors 1
                                   :max-restarts 0})
                  raw-answer (when-let [a (:answer refine-result)]
                               (str (if (map? a) (:result a) a)))
                  parsed (when (and raw-answer (not (str/blank? raw-answer)))
                           (try (edn/read-string raw-answer)
                             (catch Exception e
                               (trove/log! {:level :debug :id ::skill-refine-edn-parse-fallback
                                            :data {:error (ex-message e)}
                                            :msg "Skill refine answer not valid EDN, skipping patch"})
                               nil)))
                  new-abstract (when-let [a (get parsed "abstract")]
                                 (when (and (string? a) (not (str/blank? a)))
                                   (let [trimmed (str/trim a)]
                                     (if (> (count trimmed) 200)
                                       (str (subs trimmed 0 197) "...")
                                       trimmed))))
                  body-patch (get parsed "body-patch")]
              (when new-abstract
                (skill-manage-fn (:db-info rlm-env) skill-registry-atom
                  :refine {:name skill-name :abstract new-abstract})
                (trove/log! {:level :info :id ::skill-auto-refined-abstract
                             :data {:skill skill-name
                                    :old-abstract (:abstract skill)
                                    :new-abstract new-abstract
                                    :trigger (or status :low-confidence)}
                             :msg "Skill abstract auto-refined"}))
              (when (and (map? body-patch) (get body-patch "old") (get body-patch "new"))
                (skill-manage-fn (:db-info rlm-env) skill-registry-atom
                  :patch {:name skill-name
                          :old (get body-patch "old")
                          :new (get body-patch "new")})
                (trove/log! {:level :info :id ::skill-auto-refined-body
                             :data {:skill skill-name
                                    :trigger (or status :low-confidence)}
                             :msg "Skill body auto-patched"})))
            (catch Exception e
              (trove/log! {:level :warn :id ::skill-auto-refine-failed
                           :data {:error (ex-message e)
                                  :skills skills-loaded}
                           :msg "Skill auto-refine failed — non-blocking, swallowed"}))))))))

(defn run-sub-rlm
  "Runs an iterated sub-RLM query using the existing iteration-loop machinery.

   Forwards the caller's `:routing` + `:reasoning` into `iteration-loop` so
   `(sub-rlm-query \"q\" {:routing {:optimize :cost} :reasoning :deep})` actually
   steers every sub-iteration (not just the first). The LLM inside the sub-RLM
   can still override per-turn via `:next.model` / `:next.reasoning`; those
   overrides win for that one iteration but don't sticky past it."
  [iteration-loop-fn rlm-env prompt {:keys [system-prompt max-iter cancel-atom
                                            include-trace skills-loaded
                                            routing reasoning parent-iteration-ref]
                                     :or   {max-iter 5}}]
  (when-not iteration-loop-fn
    (throw (ex-info "run-sub-rlm requires iteration-loop-fn — caller must inject it"
             {:type :rlm/missing-iteration-loop})))
  ;; Fork the SCI env so sub-RLM defs don't pollute the parent sandbox,
  ;; and thread the invoking iteration ref through so the sub-query
  ;; entity gets parented to it in the DB (see this file's docstring
  ;; for the tree shape).
  (let [effective-parent-iter (or parent-iteration-ref
                                (:parent-iteration-ref rlm-env))
        sub-env (fork-rlm-env-for-sub rlm-env effective-parent-iter)
        ;; Prepend a `[parent handoff]` block so the sub-RLM's first
        ;; iteration sees WHY it was invoked and inherits the parent's
        ;; current thinking state. Without this the sub would start
        ;; cold and often re-derive context the parent already owns.
        sub-prompt-with-handoff (or (build-sub-rlm-handoff rlm-env prompt)
                                  prompt)]
    (trove/log! {:level :info :id ::sub-rlm-start
                 :data {:prompt-len (count prompt)
                        :max-iter max-iter
                        :has-system-prompt (some? system-prompt)
                        :routing routing
                        :reasoning reasoning
                        :parent-iteration-ref effective-parent-iter
                        :skills-loaded skills-loaded}
                 :msg "Sub-RLM iterated query starting"})
    ;; Create a :query entity for the sub-RLM BEFORE running the loop
    ;; so its iterations, vars, and final answer persist under the
    ;; invoking iteration (see docstring). Without this, sub-iterations
    ;; would orphan in the DB with no parent query.
    (let [sub-query-ref (rlm-db/store-query! (:db-info sub-env)
                          {:conversation-ref (:conversation-ref sub-env)
                           :parent-ref       effective-parent-iter
                           :text             sub-prompt-with-handoff
                           :status           :running})
          result (iteration-loop-fn
                   sub-env
                   sub-prompt-with-handoff
                 (cond-> {:system-prompt          system-prompt
                          :max-iterations         max-iter
                          :cancel-atom            (or cancel-atom (atom false))
                          :max-consecutive-errors 3
                          :max-restarts           1
                          :query-ref              sub-query-ref}
                   routing   (assoc :routing routing)
                   reasoning (assoc :reasoning-default reasoning)))
        answer-data (:answer result)
        answer-str (when answer-data
                     (if (map? answer-data)
                       (str (:result answer-data))
                       (str answer-data)))
        last-iter-execs (when-let [trace (:trace result)]
                          (:executions (last trace)))
        code (when (seq last-iter-execs)
               (let [blocks (mapv :code last-iter-execs)]
                 (when (seq blocks) blocks)))
         ;; Seal the sub-query entity with final answer + counters so
         ;; a trace-inspecting caller can pull a fully-populated row by
         ;; the returned `:query-id`.
         _ (rlm-db/update-query! (:db-info sub-env) sub-query-ref
             {:answer      answer-data
              :iterations  (or (:iterations result) 0)
              :duration-ms 0
              :status      (or (:status result) :unknown)
              :tokens      (:tokens result)
              :cost        (:cost result)})
         sub-query-id (second sub-query-ref)
         sub-result (cond-> {:content       answer-str
                             :query-id      sub-query-id
                             :code          code
                             :result        (when answer-data
                                              {:answer     answer-str
                                               :confidence (:confidence result)
                                               :sources    (:sources result)
                                               :reasoning  (:reasoning result)})
                             :iter          (or (:iterations result) 0)
                             :tokens        (:tokens result)
                             :cost          (:cost result)
                             :status        (:status result)
                             :skills-loaded skills-loaded}
                      include-trace (assoc :trace (:trace result)))]
     (trove/log! {:level :info :id ::sub-rlm-done
                  :data {:iter (:iterations result)
                         :status (:status result)
                         :has-answer (some? answer-str)
                         :code-blocks (count code)
                         :query-id sub-query-id}
                  :msg "Sub-RLM iterated query done"})
     ;; Auto-refine reads/writes the SHARED skill registry on the
     ;; parent env; pass parent, not sub-env.
     (auto-refine-async! iteration-loop-fn rlm-env skills-loaded sub-result)
     sub-result)))
