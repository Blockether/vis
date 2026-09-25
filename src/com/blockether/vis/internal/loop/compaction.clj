(ns com.blockether.vis.internal.loop.compaction
  "Context compaction verbs and the durable context snapshot.

   Implements the verbs that fold settled work, checkpoint and rebase a session's
   context, and the folded-context view they produce, so a session keeps working
   once its transcript outgrows the context window."
  (:require [charred.api :as json]
            [clojure.set :as set]
            [clojure.string :as str]
            [com.blockether.vis.internal.context.engine :as ctx-engine]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.context.renderer :as ctx-renderer]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [taoensso.telemere :as tel]))

;; Freeze each iteration result as an append-only message so the prefix cache remains
;; reusable. A semantic fold deliberately rewrites those messages once under pressure.

(def ^:private SESSION_REBASE_RECLAIMED_TOKENS 200000)

(defn rebase-session-context!
  "Materialize `cur` as both the same-turn full delta and next-turn standing
   snapshot when a fold crossed the rebase threshold. Returns nil otherwise."
  [standing-ctx-atom session-rebase-atom cur]
  (when (and cur
             (true? (:pending? (some-> session-rebase-atom
                                       deref))))
    (reset! standing-ctx-atom {:block (ctx-renderer/render-ctx-map cur) :baseline cur})
    (reset! session-rebase-atom {:reclaimed-tokens 0 :pending? false})
    (ctx-renderer/render-ctx-delta {} cur)))

(defn durable-context-snapshot
  "Normalize both in-turn checkpoints and terminal context snapshots for resume."
  [environment ctx]
  (-> (ctx-loop/stamp-cursor environment ctx)
      ctx-engine/gc-pass
      (dissoc "session_scope")
      ctx-engine/strip-ephemeral))

(defn checkpoint-fold!
  "A fold is acknowledged only after its current turn version has saved the checkpoint."
  [environment ctx]
  (let [{:keys [session-turn-id session-turn-state-id]} (ctx-loop/read-turn-state environment)]
    (when-not (persistance/db-checkpoint-session-turn-ctx! (:db-info environment)
                                                           session-turn-id
                                                           session-turn-state-id
                                                           (durable-context-snapshot environment
                                                                                     ctx))
      (throw (ex-info "fold_session could not save its checkpoint; the turn changed or ended."
                      {:type :vis/fold-checkpoint-rejected}))))
  nil)

(defn- folded-context
  "Record a bounded selector and supersede older intents without changing the live context."
  [ctx intent]
  (let [candidates
        (conj (vec (get ctx "session_summaries"))
              (assoc intent "at_turn" (get intent "issued_turn")))

        ;; Include scopes named by earlier folds, even when absent from the live trailer.
        universe
        (into (vec (get ctx "engine_iter_universe"))
              (comp (mapcat #(get % "scopes")) (filter ctx-engine/scope-key))
              candidates)

        tagged
        (mapv #(assoc %2 "__record_idx" %1) (range) candidates)

        winners
        (-> tagged
            (ctx-engine/expand-through universe (keys (get ctx "engine_turn_weights")))
            ctx-engine/supersede-summaries)

        kept
        (into #{} (map #(get % "__record_idx")) winners)]

    (assoc ctx
      "session_summaries_revision" (inc (long (get ctx "session_summaries_revision" 0)))
      "engine_fold_count" (inc (long (or (get ctx "engine_fold_count") 0)))
      "engine_fold_measurement" (if (= "pending" (get-in ctx ["engine_fold_measurement" "status"]))
                                  (update (get ctx "engine_fold_measurement") "fold_count" inc)
                                  (let [sample (get ctx "engine_provider_input")]
                                    (merge (select-keys sample ["turn" "provider" "model"])
                                           {"status" "pending"
                                            "fold_count" 1
                                            "before_input_tokens" (when (= (get sample "turn")
                                                                           (get ctx "session_turn"))
                                                                    (get sample "input_tokens"))})))
      ;; Preserve the original selector shape, not its expanded scope list.
      "session_summaries" (into [] (keep-indexed #(when (contains? kept %1) %2)) candidates))))

(defn compaction-verbs
  "Build the model-facing compaction verb bound into the sandbox as
   `fold_session`, closing over `ctx-atom`. It records a `:session/summaries`
   intent the wire applies via `apply-summaries`, and returns a visible
   confirmation string for stdout.

    The verb takes exactly TWO arguments: a KEY and an optional GIST. The key is
    a STRING in the `ctx-engine/fold-key` grammar — \"t2/i5\" one step, \"t2\" a
    whole turn, \"t2/i1-i56\" a range, \"-t2/i56\"/\"t2/i5-\" an open one, commas
    to union several — disjoint RANGES included (a list of key strings works
     too). Anything that is not a step key, or that resolves to neither settled
     steps nor a turn recap, is refused BY NAME with the grammar. The gist is
     OPTIONAL: pass it to KEEP a one-line takeaway; OMIT it
    to discard the step with no summary line. Recorded intents are string-keyed
    because they persist inside the ctx blob; `ctx-engine/expand-through` owns
    their shape and `apply-summaries` renders them."
  [ctx-atom & [session-rebase-atom checkpoint!]]
  (let [normalize-key
        (fn [value]
          ;; Some Python call shapes hand a LIST of keys across as one JSON string;
          ;; decode it so "[\"t1/i2\", \"t1/i3\"]" binds like the list itself.
          (if (and (string? value) (re-matches #"\s*\[.*" value))
            (try (let [parsed (json/read-json value)]
                   (if (sequential? parsed) parsed value))
                 (catch Throwable _ value))
            value))

        freeze
        (fn [intent]
          ;; Unbounded-above selectors (`since`, or `from` without `to`) would
          ;; otherwise re-resolve against the GROWING universe on every send and
          ;; silently swallow iterations created AFTER the fold — a standing
          ;; subscription to future work the model never chose to fold. Freeze
          ;; the ceiling NOW: resolve to concrete scopes against the current
          ;; universe so the fold captures only what existed at fold time.
          ;; Bounded selectors (`through`, `from`+`to`) are already safe and
          ;; pass through untouched (still re-resolved raw, but their upper
          ;; bound blocks any new scope).
          (let [unbounded?
                (boolean (some (fn [r]
                                 (or (contains? r "since")
                                     (and (contains? r "from") (not (contains? r "to")))))
                               (ctx-engine/intent-ranges intent)))

                ctx
                (some-> ctx-atom
                        deref)

                universe
                (get ctx "engine_iter_universe")

                turns
                (keys (get ctx "engine_turn_weights"))]

            (if (and unbounded? (or (seq universe) (seq turns)))
              (first (ctx-engine/expand-through [intent] universe turns))
              intent)))

        parse-key
        (fn [k]
          (when-let [parsed (ctx-engine/fold-key (normalize-key k))]
            (when-let [error (:error parsed)]
              (throw (ex-info error {:type :vis/fold-session-key :key k})))
            [(freeze (:intent parsed)) (:label parsed)]))

        current-turn
        (fn []
          (let [v (some-> ctx-atom
                          deref
                          (get "session_turn"))]
            (cond (integer? v) (long v)
                  (string? v) (parse-long (str/trim v))
                  :else nil)))

        record!
        (fn [intent]
          (when ctx-atom
            ;; Serialize fold writers, but leave unrelated atom updates intact. Never do
            ;; IO inside swap!: a retry could commit a checkpoint twice.
            (locking ctx-atom
              (let [after (folded-context @ctx-atom intent)]
                (when checkpoint! (checkpoint! after))
                (swap! ctx-atom merge
                  (select-keys after
                               ["session_summaries" "session_summaries_revision" "engine_fold_count"
                                "engine_fold_measurement"]))))))

        fmt-tok
        (fn [t]
          (let [t (long t)]
            (cond (>= t 1000000) (str (/ (Math/round (/ (double t) 100000.0)) 10.0) "M")
                  (>= t 1000) (str (long (Math/round (/ (double t) 1000.0))) "k")
                  :else (str t))))

        ;; Keep tokenizer removal estimates separate from provider-measured input.
        ;; Diagnostic enrichment must not break the fold receipt.
        priced
        (fn [base]
          (try
            (let [ctx
                  (some-> ctx-atom
                          deref)

                  universe
                  (get ctx "engine_iter_universe")

                  turns
                  (keys (get ctx "engine_turn_weights"))

                  weights
                  (get ctx "engine_iter_weights")

                  util
                  (get ctx "engine_utilization")

                  ;; Price the DELTA this intent still removes, not its entire selector.
                  ;; A model can fold repeatedly before the next provider projection has
                  ;; re-stamped visible weights; the earlier summary already hid its raw
                  ;; payload even though its old weight is still present in this ctx.
                  expanded
                  (ctx-engine/expand-through [base] (or universe []) turns)

                  existing
                  (ctx-engine/expand-through (get ctx "session_summaries") (or universe []) turns)

                  already-scopes
                  (into #{} (mapcat #(get % "scopes")) existing)

                  scopes
                  (set/difference (into #{} (mapcat #(get % "scopes")) expanded) already-scopes)

                  ;; Whole-turn intent also removes the turn's Q/A recap from the
                  ;; prior-turn context. Apply the same delta rule: a wider re-fold
                  ;; must not recharge a recap an earlier whole-turn fold removed.
                  already-turns
                  (into #{} (mapcat #(get % "turns")) existing)

                  new-turns
                  (set/difference (into #{} (mapcat #(get % "turns")) expanded) already-turns)

                  qa-toks
                  (let [tw (get ctx "engine_turn_weights")]
                    (reduce + 0 (keep #(get tw %) new-turns)))

                  toks
                  (if-let [estimate (get ctx "engine_fold_estimator")]
                    (estimate ctx (folded-context ctx base))
                    (when (or (map? weights) (map? (get ctx "engine_turn_weights")))
                      (+ (long (reduce + 0 (keep #(get weights %) scopes))) (long qa-toks))))

                  ;; A fold can legitimately cover scopes that already left the wire:
                  ;; iterations of a turn that COMPLETED normally replay no results at
                  ;; all (`off-wire-seed?` above), so only that turn's Q/A recap still
                  ;; resides — and ONLY a whole-turn token (`tN`) charges and removes a
                  ;; recap. Folding those `tN/iM` ids is an honest no-op, but a card
                  ;; listing 44 folded scopes beside a small `saved ~` reads as broken
                  ;; accounting (issue #88). So name the weightless share and point at
                  ;; the shape that WOULD reclaim it. A scope MISSING from `weights`
                  ;; is merely unsent (never priced yet), not off-wire.
                  off-wire
                  (if (map? weights)
                    (filterv #(and (contains? scopes %)
                                   (contains? weights %)
                                   (zero? (long (get weights %))))
                      (or universe []))
                    [])

                  recap-turns
                  (let [tw
                        (get ctx "engine_turn_weights")

                        folded?
                        (into already-turns new-turns)]

                    (into []
                          (comp (keep #(some-> (second (re-matches #"t(\d+)/i\d+" %))
                                               parse-long))
                                (distinct)
                                (remove folded?)
                                (filter #(pos? (long (get tw % 0)))))
                          off-wire))

                  ;; Only advise when a whole-turn fold really would reclaim something:
                  ;; recap-less or already-whole-turn-folded scopes need no nudge.
                  off-wire-note
                  (when (and (nil? (get ctx "engine_fold_estimator")) (seq recap-turns))
                    (str " · "
                         (count off-wire)
                         "/"
                         (count scopes)
                         " scopes already off-wire — fold "
                         (str/join ", " (map #(str "t" %) recap-turns))
                         " to drop their recaps"))

                  removed
                  (cond (nil? toks) (when util " · removal estimate unavailable")
                        (pos? (long toks)) (str " · estimated removal ~" (fmt-tok toks) " tokens")
                        (neg? (long toks))
                        (str " · estimated growth ~" (fmt-tok (- (long toks))) " tokens")
                        :else " · estimated removal ~0 tokens")

                  ;; Removal uses a local tokenizer; input is provider usage. Subtracting
                  ;; them cannot establish the remaining context or a non-foldable floor.
                  ;; The next provider request measures the actual post-fold total.
                  input
                  (long (or (get util "last_request_tokens") 0))

                  budget
                  (long (or (get util "auto_compress_above") 0))

                  limit
                  (long (or (get util "model_input_limit") 0))

                  measured
                  (str (when (pos? input) (str " · last input " (fmt-tok input) " measured tokens"))
                       (when (pos? budget) (str " · operating budget " (fmt-tok budget)))
                       (when (pos? limit) (str " · model limit " (fmt-tok limit))))]

              {:note (str removed measured off-wire-note)
               :reclaimed-tokens (max 0 (long (or toks 0)))})
            (catch Throwable _ {:note " · removal estimate unavailable" :reclaimed-tokens 0})))]

    {'fold-session
     (fn fold-session [fold-key & [gist]]
       ;; Python kwargs cross as ONE trailing dict (`__vis_direct_kwargs__`):
       ;; `fold_session(k, gist="…")` arrives as (k {"gist" "…"}) and a fully
       ;; keyword call as ({"key" … "gist" …}). Unwrap both so keyword and
       ;; positional calls bind identically; anything else that spreads at the
       ;; top level is not a key, so it travels on to `ctx-engine/fold-key` and
       ;; is refused by name with the grammar instead of folding nothing.
       (let [kwargs?
             (fn [m]
               (and (map? m) (or (contains? m "key") (contains? m "gist"))))

             [fold-key gist]
             (cond (kwargs? gist) [(if (contains? gist "key") (get gist "key") fold-key)
                                   (get gist "gist")]
                   (and (nil? gist) (kwargs? fold-key)) [(or (get fold-key "key")
                                                             (not-empty (dissoc fold-key "gist")))
                                                         (get fold-key "gist")]
                   :else [fold-key gist])]

         (if-let [[base label] (parse-key fold-key)]
           (let [turn (current-turn)
                 ctx (some-> ctx-atom
                             deref)
                 uni (get ctx "engine_iter_universe")
                 universe (set uni)
                 ;; Resolve the selector against the SETTLED wire. `universe` is every
                 ;; iteration already on THIS request's trailer: all prior turns PLUS
                 ;; the current turn's COMPLETED iterations. Bare-turn / range / cursor
                 ;; selectors are universe-bounded, so they can only ever name settled
                 ;; steps; an EXPLICIT `tN/iN` literal is the one shape that survives
                 ;; resolution verbatim, so it is the only way to point at the live
                 ;; iteration still being emitted (present on no trailer, absent here).
                 resolved (first (ctx-engine/expand-through [base]
                                                            (or uni [])
                                                            (keys (get ctx "engine_turn_weights"))))
                 ;; The live iteration is any CURRENT-turn (or future) scope not yet
                 ;; settled. Prior turns are always foldable, AND so is every finished
                 ;; iteration of the current turn — only the in-flight iteration is
                 ;; off-limits, because folding it would collapse steps this very turn
                 ;; is still producing.
                 live-scopes (when turn
                               (into (sorted-set)
                                     (filter (fn [sc]
                                               (when-let [k (ctx-engine/scope-key sc)]
                                                 (and (>= (long (first k)) (long turn))
                                                      (not (contains? universe sc))))))
                                     (get resolved "scopes")))]

             (when-not turn
               (throw (ex-info "fold_session cannot prove the current turn; folding is blocked."
                               {:type :vis/fold-session-turn-unknown})))
             (when (seq live-scopes)
               (throw (ex-info
                        (str "fold_session blocked: " (str/join ", " live-scopes)
                             " name the live iteration you are emitting right now — not yet a "
                             "settled wire step. Fold only COMPLETED steps: every prior turn AND "
                             "the current turn's finished iterations (e.g. \"-tN/iK\" up to the "
                             "last settled iteration). Do not retry THESE scopes this turn.")
                        {:type :vis/fold-session-active-turn
                         :current-turn turn
                         :blocked-scopes live-scopes})))
             ;; A target that resolves to NO settled wire step folds nothing: the ack
             ;; would read `saved ~0 tokens` and the gist would anchor to an id the
             ;; wire never held (a mistyped range, a turn that does not exist yet).
             ;; Refuse it by name instead of recording a silent no-op fold. Whole-turn
             ;; intent (`turns`) at or before the live turn stays legal even with no
             ;; iteration on this trailer — it also removes that turn's Q/A recap,
             ;; which resides outside the iteration universe.
             (let [named (get resolved "scopes")
                   settled-turns (filter (fn [tn]
                                           (<= (long tn) (long turn)))
                                         (get resolved "turns"))
                   ordered (sort-by ctx-engine/scope-key (filter ctx-engine/scope-key uni))]

               (when (and (seq ordered) (empty? settled-turns) (not-any? universe named))
                 (throw (ex-info (str "fold_session: " label
                                      " matches no settled step — this session's wire holds "
                                      (first ordered)
                                      " … " (last ordered)
                                      ". " ctx-engine/fold-key-grammar)
                                 {:type :vis/fold-session-unknown-key
                                  :key label
                                  :scopes (into (sorted-set) named)}))))
             (let [g (some-> gist
                             str
                             str/trim
                             not-empty)
                   ;; Stamp the ISSUING turn so `previous-turn-context` never lets
                   ;; a whole-turn fold recorded DURING turn N erase turn N's own
                   ;; Q/A recap next request (the answer is produced after the fold;
                   ;; the gist can't summarize it). `turn` is always non-nil here —
                   ;; the guard above throws when it can't prove the current turn.
                   base (cond-> (assoc base "issued_turn" turn)
                          g
                          (assoc "gist" g))
                   {:keys [note reclaimed-tokens]} (priced base)
                   intent (cond-> base
                            (not (str/blank? note))
                            (assoc "note" note))]

               (record! intent)
               (when (and session-rebase-atom (pos? (long reclaimed-tokens)))
                 (swap! session-rebase-atom
                   (fn [{accumulated :reclaimed-tokens :as state}]
                     (let [total (+ (long (or accumulated 0)) (long reclaimed-tokens))]
                       (assoc state
                         :reclaimed-tokens total
                         :pending? (>= (long total) (long SESSION_REBASE_RECLAIMED_TOKENS)))))))
               (tel/log! {:level :info :id ::fold-session :data {:intent intent}}
                         "model folded scopes")
               (str "folded "
                    label
                    note
                    (when (get ctx "engine_provider_input")
                      " · provider net change pending in request health after the next response")
                    (when g (str " → " g)))))
           (str "fold_session: nothing to fold — " ctx-engine/fold-key-grammar))))}))
