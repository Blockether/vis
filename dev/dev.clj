(ns dev
  "Unified post-mortem debugger for stored vis conversations.

   Purpose
   -------
   When a user reports 'this conversation went sideways' (like
   6f832df0-6762-402b-8ca0-275f9aeb54a4 — 96 iterations, 0 answers,
   52 re-reads of the same file), you open a REPL and use these
   functions to see exactly what the LLM saw at every step without
   grepping raw SQL. The whole namespace is read-only against
   `~/.vis/vis.mdb/rlm.db` — no mutation, safe to run while the web
   server is up.

   Usage
   -----
     clojure -M:dev -r
     user=> (require '[dev :as d])
     user=> (d/conversation \"6f832df0-6762-402b-8ca0-275f9aeb54a4\")
     user=> (d/turns         \"6f832df0-6762-402b-8ca0-275f9aeb54a4\")
     user=> (d/iterations    <query-uuid>)
     user=> (d/iteration     <query-uuid> 21)   ; zero-indexed
     user=> (d/iteration-context <query-uuid> 21)
     user=> (d/quality-report \"6f832df0-6762-402b-8ca0-275f9aeb54a4\")

   Hierarchy
   ---------
   conversation → query (turn) → iteration → iteration-var
                 (N per conv)  (N per query) (N per iter)

   Every navigator takes UUID or string; UUIDs are coerced internally.
   Every reconstructor returns plain Clojure maps — no live env, no
   SCI sandbox, nothing to dispose. The DB handle is shared via a
   memoized atom and released automatically at JVM shutdown.

   What this does NOT reconstruct
   ------------------------------
   - The exact system prompt at iteration-time. The persisted
     `:conversation/system-prompt` is the persona only; the full
     prompt is rebuilt per turn from live env state (tools active,
     skills loaded, corpus revision). Showing it here would lie. Use
     `dev/token_count.clj` if you need a live prompt render.
   - Token counts. Add jtokkit if you need them — intentionally
     excluded to keep this namespace dep-light.
   - The live `<var_index>` renderer. That pulls from the SCI
     sandbox map. We render a DB-view instead: every `(def x v)`
     written through iter N, ordered by iter-of-last-write.

   What this DOES reconstruct (DB-only, historic)
   ----------------------------------------------
   - The `<journal>` block at iter N. Built from iter N-1's
     persisted executions via the real `format-execution-results`
     — byte-identical to what the LLM saw.
   - Nudges that would have fired at iter N: budget-warning,
     var-index-overflow, repetition-warning. The repetition counter
     is rebuilt from iters 0..N-1 so you see the SAME warning the
     LLM got (or the one it should have gotten, pre-fix)."
  (:refer-clojure :exclude [iteration])
  (:require
    [clojure.edn :as edn]
    [clojure.string :as str]
    [com.blockether.vis.config :as config]
    [com.blockether.vis.loop.core :as rlm-core]
    [com.blockether.vis.loop.nudges :as nudges]
    [com.blockether.vis.loop.storage.db :as rlm-db])
  (:import
    [com.knuddels.jtokkit Encodings]
    [com.knuddels.jtokkit.api EncodingType]))

;; -----------------------------------------------------------------------------
;; DB handle — memoized, process-lifetime. Readers only.
;; -----------------------------------------------------------------------------

(defonce ^:private db-atom (atom nil))

(defn db
  "Shared read-only db-info handle. Opens `~/.vis/vis.mdb/rlm.db` on
   first call; subsequent calls reuse the pool. The same shared
   connection vis itself uses, so you won't deadlock when the web
   server is up."
  []
  (or @db-atom
    (swap! db-atom (fn [cur] (or cur (rlm-db/create-rlm-conn config/db-path))))))

(defn close-db!
  "Release the memoized pool. Safe to call at REPL shutdown; next
   `(db)` reopens. You rarely need this — the pool is shared."
  []
  (when-let [d @db-atom]
    (try (rlm-db/dispose-rlm-conn! d) (catch Exception _ nil))
    (reset! db-atom nil)))

(.addShutdownHook (Runtime/getRuntime)
  (Thread. ^Runnable close-db!))

;; -----------------------------------------------------------------------------
;; Coercion
;; -----------------------------------------------------------------------------

(defn- ->uuid [x]
  (cond
    (uuid? x)   x
    (string? x) (java.util.UUID/fromString x)
    :else       (throw (ex-info "Expected UUID or UUID string"
                         {:input x :type (class x)}))))

(defn- conv-ref [conv-id] [:id (->uuid conv-id)])
(defn- query-ref [q-id]   [:id (->uuid q-id)])
(defn- iter-ref  [i-id]   [:id (->uuid i-id)])

(defn- read-edn-safe [s fallback]
  (if (or (nil? s) (= "" s))
    fallback
    (try (edn/read-string s) (catch Exception _ fallback))))

;; -----------------------------------------------------------------------------
;; Token counting (jtokkit)
;; -----------------------------------------------------------------------------
;;
;; Approximation by design. O200K is exact for gpt-5 / gpt-4o / o-series;
;; CL100K is the Anthropic rough-match. For providers outside those two
;; tokenizer families (glm, gemini, mistral, qwen) we still use O200K —
;; off by ~5-15 % but consistent within a conversation, which is what we
;; need to see iteration-over-iteration blowup.

(def ^:private jtokkit-registry
  (delay (Encodings/newDefaultEncodingRegistry)))

(defn- pick-encoding [^String model-str]
  (cond
    (str/includes? model-str "gpt-5")   (.getEncoding ^com.knuddels.jtokkit.api.EncodingRegistry @jtokkit-registry EncodingType/O200K_BASE)
    (str/includes? model-str "gpt-4o")  (.getEncoding ^com.knuddels.jtokkit.api.EncodingRegistry @jtokkit-registry EncodingType/O200K_BASE)
    (str/includes? model-str "o1")      (.getEncoding ^com.knuddels.jtokkit.api.EncodingRegistry @jtokkit-registry EncodingType/O200K_BASE)
    (str/includes? model-str "o3")      (.getEncoding ^com.knuddels.jtokkit.api.EncodingRegistry @jtokkit-registry EncodingType/O200K_BASE)
    (str/includes? model-str "claude")  (.getEncoding ^com.knuddels.jtokkit.api.EncodingRegistry @jtokkit-registry EncodingType/CL100K_BASE)
    :else                                (.getEncoding ^com.knuddels.jtokkit.api.EncodingRegistry @jtokkit-registry EncodingType/O200K_BASE)))

(defn count-tokens
  "Tokenize `text` for `model`. Returns 0 for nil/empty. Safe to call
   on very long strings — jtokkit is streaming. Encoding choice is
   approximate (see ns comment)."
  [model text]
  (if (or (nil? text) (= "" text))
    0
    (.countTokens (pick-encoding (or model "")) ^String text)))

(defn- conv-model
  "Lookup the persisted model string for a conversation so the token
   helpers match the runtime encoding when possible."
  [db-info cref]
  (or (:model (rlm-db/db-get-conversation db-info cref)) ""))

(defn- model-for-query
  "Resolve the conversation that owns `q-id` and return its model
   string via the public facade (`fetch-entity` on the query →
   parent-id → conversation row). Returns '' on any lookup failure;
   token fns degrade to O200K which is accurate enough for relative
   sizing within a turn."
  [db-info q-id]
  (try
    (let [fetch-entity (requiring-resolve
                         'com.blockether.vis.loop.storage.sqlite.core/fetch-entity)
          q-row        (fetch-entity db-info (->uuid q-id))
          conv-id      (:parent-id q-row)]
      (if conv-id
        (conv-model db-info [:id conv-id])
        ""))
    (catch Exception _ "")))

;; -----------------------------------------------------------------------------
;; Navigators
;; -----------------------------------------------------------------------------

(defn conversation
  "Summary for one conversation: system prompt, model, turn count,
   total iterations, status of the latest turn. First call when
   triaging."
  [conv-id]
  (let [db-info (db)
        cref    (conv-ref conv-id)
        conv    (rlm-db/db-get-conversation db-info cref)
        queries (rlm-db/db-list-conversation-queries db-info cref)
        iter-counts (mapv (fn [q]
                            (count (rlm-db/db-list-query-iterations db-info [:id (:id q)])))
                      queries)]
    {:id              (:id conv)
     :system-prompt   (:system-prompt conv)
     :model           (:model conv)
     :turns           (count queries)
     :total-iterations (reduce + iter-counts)
     :latest-status   (:status (last queries))
     :latest-text     (some-> (last queries) :text (#(subs % 0 (min 100 (count %)))))}))

(defn turns
  "One entry per user turn. Wraps `db-query-history` so you get the
   compact summary (status, iteration count, answer preview, vars
   defined) in one call."
  [conv-id]
  (rlm-db/db-query-history (db) (conv-ref conv-id)))

;; -----------------------------------------------------------------------------
;; Execution shape reconstruction
;; -----------------------------------------------------------------------------

(defn- pair->execution
  "Turn a persisted [code-string, pr-str'd-result-map] pair back into
   the `{:code :result :error :stdout :stderr :execution-time-ms
   :timeout? :repaired?}` shape consumed by
   `format-execution-results` and the repetition detector. Legacy
   rows that stored the raw result (not wrapped in `:result`) get
   coerced to the new shape."
  [code-str result-str]
  (let [raw (read-edn-safe result-str {})
        m   (if (and (map? raw) (contains? raw :result))
              raw
              {:result raw})]
    (cond-> {:code code-str
             :result (:result m)}
      (:error m)    (assoc :error (:error m))
      (:stdout m)   (assoc :stdout (:stdout m))
      (:stderr m)   (assoc :stderr (:stderr m))
      (:timeout? m)  (assoc :timeout? true)
      (:repaired? m) (assoc :repaired? true)
      (:time-ms m)   (assoc :execution-time-ms (:time-ms m)))))

(defn- reconstruct-executions
  "DB iteration row → [execution] vector. Returns empty vector on
   malformed rows (rare — only happens on rows from before the
   result-map migration if any exist)."
  [iter-row]
  (let [code-strs   (read-edn-safe (:code iter-row) [])
        result-strs (read-edn-safe (:results iter-row) [])
        n           (min (count code-strs) (count result-strs))]
    (mapv (fn [i]
            (pair->execution (nth code-strs i) (nth result-strs i)))
      (range n))))

;; -----------------------------------------------------------------------------
;; Iteration-level views
;; -----------------------------------------------------------------------------

(defn iterations
  "Per-iteration view for a query: `[{:pos :iteration-id :duration-ms
   :thinking :answer :error :final? :executions :vars-defined}]`.

   `:executions` is the reconstructed vector ready to hand to
   `format-execution-results` or the repetition detector.
   `:vars-defined` is exactly what was persisted via `store-iteration!`
   (so `(restore-var 'name)` can reclaim these values in a future
   turn)."
  [q-id]
  (let [db-info (db)
        qref    (query-ref q-id)
        raw     (rlm-db/db-list-query-iterations db-info qref)]
    (mapv (fn [i it]
            {:pos          i
             :iteration-id (:id it)
             :created-at   (:created-at it)
             :duration-ms  (:duration-ms it)
             :thinking     (:thinking it)
             :answer       (:answer it)
             :error        (:error it)
             :final?       (some? (:answer it))
             :executions   (reconstruct-executions it)
             :vars-defined (rlm-db/db-list-iteration-vars db-info [:id (:id it)])})
      (range) raw)))

(defn iteration
  "Single iteration by zero-indexed `pos`. Returns nil when out of
   range. Shorthand for `(nth (iterations q-id) pos nil)`."
  [q-id pos]
  (nth (iterations q-id) pos nil))

;; -----------------------------------------------------------------------------
;; Context reconstruction
;; -----------------------------------------------------------------------------

(defn- cumulative-vars-through
  "Walk iters 0..up-to-pos-1 and return `{sym {:value :code
   :source-iter :version}}` — last-write-wins. Pure DB view. No
   sandbox dependency so this is safe to run against any stored
   conversation, including running ones."
  [db-info qref up-to-pos]
  (let [iters (take up-to-pos (rlm-db/db-list-query-iterations db-info qref))]
    (reduce
      (fn [acc [iter-pos it]]
        (reduce
          (fn [acc2 {:keys [name value code]}]
            (if name
              (let [sym     (symbol name)
                    prev-v  (get-in acc2 [sym :version] 0)]
                (assoc acc2 sym
                  {:value       value
                   :code        code
                   :source-iter iter-pos
                   :version     (inc prev-v)}))
              acc2))
          acc
          (rlm-db/db-list-iteration-vars db-info [:id (:id it)])))
      {}
      (map vector (range) iters))))

(defn- render-var-index-view
  "DB-view rendering of `<var_index>`. Not byte-identical to the
   live SCI-backed renderer — intentionally simpler — but shows
   every var the model could have restored at iteration N, the iter
   it was last written at, and a 100-char value preview."
  [var-view]
  (when (seq var-view)
    (->> var-view
      (sort-by key)
      (mapv (fn [[sym {:keys [value source-iter version]}]]
              (let [preview (pr-str value)
                    preview (if (> (count preview) 100)
                              (str (subs preview 0 100) "...")
                              preview)]
                (str "  " sym " := " preview
                  " ;; iter " source-iter
                  (when (> version 1) (str " (v" version ")"))))))
      (str/join "\n"))))

(defn iteration-context
  "Reconstruct what the LLM saw as its user-message at iteration
   `pos` of the given query. Returns a map with each slice broken
   out so you can inspect individually, plus `:approx-full` — the
   concatenated best-effort reconstruction.

   Slices:
     :iter-marker       '[iter N/M]' header the loop always emits.
                        M is the live iter count at rest, so it's
                        accurate for completed queries; for running
                        queries it's the count at the moment you
                        queried. Matches what the LLM saw only if
                        no budget extensions were in flight.

     :prior-thinking    Iter (pos-1)'s persisted thinking, or nil.

     :journal           Output of the REAL `format-execution-results`
                        over iter (pos-1)'s reconstructed executions.
                        Byte-identical to the live journal (modulo
                        any LLM-side formatting done on the final
                        message envelope).

     :var-index         DB view: a map {sym {:value :code
                        :source-iter :version}} accumulated over
                        iters 0..pos-1.
     :var-index-preview Readable one-line-per-var rendering for eyeball
                        inspection — NOT what the LLM saw (see
                        namespace doc).

     :nudges            Vector of nudges that would have fired given
                        the reconstructed state. Uses the real
                        composers (budget-warning, var-index-overflow,
                        repetition-warning) so a fix to any of those
                        shows up here automatically. The repetition
                        counter is rebuilt from iters 0..pos-1 to
                        match what the LLM saw.

     :prev-executions   Reconstructed executions from iter (pos-1).
                        Handy to pass into nudges manually for
                        what-if analysis.

   Gotcha: the live loop caps the journal at the PREVIOUS iteration
   only, never N-2. This reconstruction respects that — iter 0's
   journal is always nil even though iters 1..pos-1 are visible in
   the var index."
  [q-id pos]
  (let [db-info       (db)
        qref          (query-ref q-id)
        all-iters     (rlm-db/db-list-query-iterations db-info qref)
        total         (count all-iters)
        prev-it       (when (pos? pos) (nth all-iters (dec pos) nil))
        prev-execs    (when prev-it (reconstruct-executions prev-it))
        var-view      (cumulative-vars-through db-info qref pos)
        ;; Rebuild the repetition counter state by replaying every
        ;; iteration BEFORE `prev-it` through the real detector. At
        ;; iter N the live atom holds counts from iters 0..(N-2);
        ;; iter (N-1)'s executions are fed IN by the call below,
        ;; matching what `nudges/repetition-warning` does at
        ;; iteration-context build time. Earlier drafts used `(take
        ;; pos …)` which double-counted prev-execs and produced
        ;; spurious "repetition fired" warnings for first-time calls.
        rep-state     (reduce (fn [counts it]
                                (first (nudges/bump-and-detect-repetition
                                         counts (reconstruct-executions it))))
                        {}
                        (take (max 0 (dec pos)) all-iters))
        ;; Final repetition call: feeds prev-execs against the
        ;; pre-prev counter, returning the warning string (or nil).
        rep-warn      (when (seq prev-execs)
                        (second (nudges/bump-and-detect-repetition rep-state prev-execs)))
        budget-msg    (nudges/budget-warning
                        {:iteration pos :current-max-iterations total})
        overflow-msg  (nudges/var-index-overflow (count var-view))
        journal       (when (seq prev-execs)
                        ;; format-execution-results is private; resolve
                        ;; via its var so the dev ns doesn't force a
                        ;; public API change just for post-mortems.
                        (let [f (requiring-resolve
                                  'com.blockether.vis.loop.core/format-execution-results)]
                          (f prev-execs (dec pos))))
        nudge-strings (vec (keep identity [budget-msg overflow-msg rep-warn]))
        approx-full   (->> [(str "[iter " (inc pos) "/" total "]")
                            (when (and prev-it (not (str/blank? (:thinking prev-it))))
                              (str "<prior_thinking>\n" (:thinking prev-it) "\n</prior_thinking>"))
                            journal
                            (when-let [vi (render-var-index-view var-view)]
                              (str "<var_index>\n" vi "\n</var_index>"))
                            (when (seq nudge-strings) (str/join "\n" nudge-strings))]
                        (keep identity)
                        (remove str/blank?)
                        (str/join "\n"))]
    {:iter-marker        (str "[iter " (inc pos) "/" total "]")
     :prior-thinking     (some-> prev-it :thinking (#(when-not (str/blank? %) %)))
     :prev-executions    prev-execs
     :journal            journal
     :var-index          var-view
     :var-index-preview  (render-var-index-view var-view)
     :nudges             nudge-strings
     :repetition-state   rep-state
     :approx-full        approx-full
     :tokens             (let [model (model-for-query db-info q-id)]
                           {:prior-thinking (count-tokens model (or (some-> prev-it :thinking) ""))
                            :journal        (count-tokens model (or journal ""))
                            :var-index      (count-tokens model (or (render-var-index-view var-view) ""))
                            :nudges         (count-tokens model (str/join "\n" nudge-strings))
                            :approx-full    (count-tokens model approx-full)})}))

(defn query-context
  "Reconstruct the cross-query handover block + the user's raw text
   for turn `query-pos` (zero-indexed) within a conversation.

   The live loop emits a `[prior turn]` handover at iteration 0 of
   every query beyond the first. This function returns the data you
   need to understand what the model started the turn with:

     {:turn-pos      N
      :query-text    '...'       ; what the user typed
      :prior-turn    {:text ...  ; the PREVIOUS query's text
                      :answer ...; the PREVIOUS query's final answer
                      :iterations K}
      :vars-on-entry {sym {:value ...}} ; var registry inherited
                                         ; from earlier turns}

   For iteration-level context see `iteration-context`."
  [conv-id query-pos]
  (let [db-info (db)
        cref    (conv-ref conv-id)
        queries (rlm-db/db-list-conversation-queries db-info cref)
        q       (nth queries query-pos nil)]
    (when q
      (let [prior        (when (pos? query-pos) (nth queries (dec query-pos) nil))
            prior-iters  (when prior
                           (rlm-db/db-list-query-iterations db-info [:id (:id prior)]))
            inherited    (rlm-db/db-latest-var-registry db-info cref
                           {:max-scan-queries query-pos})]
        {:turn-pos      query-pos
         :query-id      (:id q)
         :query-text    (:text q)
         :status        (:status q)
         :prior-turn    (when prior
                          {:query-id   (:id prior)
                           :text       (:text prior)
                           :answer     (:answer prior)
                           :iterations (count prior-iters)})
         :vars-on-entry inherited}))))

;; -----------------------------------------------------------------------------
;; Quality assessment
;; -----------------------------------------------------------------------------

(defn- signature
  "Tool-call signature: strip `def NAME` wrappers and `:content`
   destructuring so `(read-file X)`, `(def a (read-file X))`, and
   `(def b (:content (read-file X)))` all normalize to the same key.

   Heuristic and regex-based on purpose: keeping it simple means it
   never throws on malformed code. The repetition detector now has
   a result-only key covering cloaked loops; this signature is a
   complementary quality metric, not a runtime guard."
  [code-str]
  (-> (or code-str "")
    (str/replace #"^\s*\(\s*def(n|n-|once)?\s+[\w\-\?\!\*\+\.\<\>]+\s+" "(... ")
    (str/replace #"\(:content\s+" "(")
    (str/replace #"\(:text\s+" "(")
    str/trim))

(defn quality-report
  "Per-turn quality metrics for a whole conversation. Flags the
   patterns that correlate with stuck loops:

     :iterations        total iterations
     :final-answer?     did the turn emit :final at any iter
     :error-iterations  count of iters with at least one failing execution
     :budget-extensions count of request-more-iterations calls
     :repetition-fires  how many times the repetition detector would
                        have warned (based on reconstruction)
     :distinct-signatures
                        unique tool-call signatures across the turn —
                        low number relative to :iterations is a
                        strong stuck-loop signal
     :redundant-exec-ratio
                        1 - (distinct-sig / total-execs) — 0.0 means
                        every execution explored something new, 1.0
                        means total repetition
     :duration-ms-total sum of iter durations
     :slowest-iter      worst iter and its time

   Run this first on any suspect conversation — the numbers usually
   tell you which turn to dig into with `iteration-context`."
  [conv-id]
  (let [db-info (db)
        cref    (conv-ref conv-id)
        queries (rlm-db/db-list-conversation-queries db-info cref)]
    (vec
      (for [q queries]
        (let [qref         [:id (:id q)]
              iters        (rlm-db/db-list-query-iterations db-info qref)
              per-iter     (mapv reconstruct-executions iters)
              all-execs    (vec (mapcat identity per-iter))
              signatures   (mapv #(signature (:code %)) all-execs)
              distinct-sigs (count (distinct signatures))
              extensions   (count (filter #(re-find #"request-more-iterations" (or % ""))
                             (map :code all-execs)))
              error-iters  (count (filter (fn [execs] (some :error execs)) per-iter))
              rep-fires    (atom 0)
              _            (reduce (fn [counts execs]
                                     (let [[counts' w] (nudges/bump-and-detect-repetition
                                                         counts execs)]
                                       (when w (swap! rep-fires inc))
                                       counts'))
                             {} per-iter)
              iter-times   (keep :duration-ms iters)
              slowest      (when (seq iters)
                             (apply max-key (fn [it] (or (:duration-ms it) 0)) iters))]
          {:query-id          (:id q)
           :text              (let [t (or (:text q) "")]
                                (subs t 0 (min 100 (count t))))
           :status            (:status q)
           :iterations        (count iters)
           :final-answer?     (some? (some :answer iters))
           :error-iterations  error-iters
           :budget-extensions extensions
           :repetition-fires  @rep-fires
           :distinct-signatures distinct-sigs
           :redundant-exec-ratio (if (pos? (count signatures))
                                   (double (- 1 (/ distinct-sigs (count signatures))))
                                   0.0)
           :duration-ms-total (reduce + 0 iter-times)
           :slowest-iter      (when slowest
                                {:iteration-id (:id slowest)
                                 :duration-ms  (:duration-ms slowest)})})))))
