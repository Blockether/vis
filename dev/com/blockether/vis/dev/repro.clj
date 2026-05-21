(ns com.blockether.vis.dev.repro
  "Replay-from-DB harness for vis loop debugging.

   The vis loop assembles a per-iteration provider request by combining
   the system prompt, the original user message, and a `trailer-iters`
   vector of `[iteration-position {:thinking :blocks :assistant-message
   :preserved-thinking/replay? ...}]` entries built from persisted
   iterations. Most regressions show up at this seam:
     - preserved-thinking-replay drops messages it should have kept
     - cached_tokens stop growing because the prompt prefix changed
     - the model re-derives the same scratch state for every iteration
     - tool-result orphans leak across turns

   `(replay-session session-id)` and `(replay-session-turn session-id
   turn-position)` reconstruct the exact request shape the live loop
   would have sent for each iteration of a real persisted session,
   without spending a single provider token. The returned report carries
   per-iteration metrics (replay-count, cache hit, reasoning chars
   emitted vs replayed, etc.) so before/after fix comparisons are a
   diff, not a guess.

   Use cases:
     1. (replay-session #uuid \"...\") -> verify a fix landed (e.g. that
        `preserved-thinking-replay-messages` now returns N items per
        iteration instead of 1).
     2. Capture a `summary` baseline before changing loop assembly, then
        compare after \u2014 every iteration that lost a replay message is a
        regression candidate.
     3. Quick smoke after svar/router changes (`:reasoning-style`,
        catalog-key resolver) without round-tripping through the TUI.

   This namespace is dev-only; it intentionally does NOT reach into
   private vars of `internal.loop` except through their canonical fns
   to stay sensitive to behavior changes (the assembly path is what
   we're verifying)."
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.loop :as lp]))

;; -----------------------------------------------------------------------------
;; Trailer reconstruction (mirrors the live-loop's `:trailer-iters` shape)
;; -----------------------------------------------------------------------------

(defn- iteration->trailer-entry
  "Project one persisted iteration row to the canonical `[position
   iter-map]` shape `compatible-preserved-thinking-trailer-iters`
   consumes. Same fields the live loop attaches when it accumulates
   trailer-iters during a running turn."
  [iter]
  [(:position iter)
   (cond-> {:thinking                  (:thinking iter)
            :blocks                    (or (:llm-executable-blocks iter) [])
            :llm-provider              (:llm-actual-provider iter)
            :llm-model                 (:llm-actual-model iter)
            ;; Persisted iterations are durable evidence; the live loop
            ;; never sets `:preserved-thinking/replay? false` on them
            ;; until a NEW user turn opens, at which point trailer-iters
            ;; is reseeded with the flag flipped. Replay tests reason
            ;; per turn, so within one turn we treat everything as
            ;; replay-eligible.
            :preserved-thinking/replay? true}
     (:llm-assistant-message iter)
     (assoc :assistant-message (:llm-assistant-message iter)))])

(defn- replay-target
  "Build the `target` map that `compatible-preserved-thinking-trailer-iters`
   uses to gate cross-provider replay. The live loop derives this from
   the resolved-model on every iteration; replays should target the
   same provider+model the iteration was originally served by, which is
   stored on the persisted row."
  [iter]
  {:provider (:llm-actual-provider iter)
   :model    (some-> (:llm-actual-model iter) str)})

(defn- replay-counts-for-iter
  "Reconstruct the prefix of trailer-iters that the live loop would
   have had IN HAND when it dispatched iteration `idx`. Returns the
   number of `:assistant-message`s the (current) preserved-thinking
   replay fn would have appended to the wire request."
  [iters idx]
  (let [prefix (take idx iters)
        trailer (mapv iteration->trailer-entry prefix)
        target  (replay-target (nth iters idx))
        compatible (#'lp/compatible-preserved-thinking-trailer-iters trailer target)
        replays    (#'lp/preserved-thinking-replay-messages compatible)]
    {:trailer-size   (count trailer)
     :compatible     (count compatible)
     :replay-msgs    (count replays)
     :thinking-chars (reduce + 0
                       (map (fn [m]
                              (->> (:content m)
                                (filter #(= "thinking" (:type %)))
                                (map #(count (or (:thinking-signature %) (:thinking %) "")))
                                (reduce + 0)))
                         replays))}))

(defn- iter-summary
  "Per-iteration metric row. Combines the persisted iteration's own
   token counts with the replay metrics the assembly path would have
   used for THIS iteration's outbound request."
  [iters idx]
  (let [iter (nth iters idx)
        replay (replay-counts-for-iter iters idx)]
    (merge
      {:position       (:position iter)
       :status         (:status iter)
       :input-tokens   (:input-tokens iter)
       :output-tokens  (:output-tokens iter)
       :cached-tokens  (:cached-tokens iter)
       :reasoning-tokens (:reasoning-tokens iter)
       :error-type     (some-> iter :error :type)
       :llm-model      (:llm-actual-model iter)}
      replay)))

;; -----------------------------------------------------------------------------
;; Public API
;; -----------------------------------------------------------------------------

(defn replay-turn
  "Per-iteration replay metrics for a single persisted turn. Callers
   must pass the parent `session-id` so the turn metadata lookup does
   not depend on the somewhat slippery `db-resolve-session-id` reverse
   lookup (which silently returns nil for a bare turn UUID).

   Returns:
     {:turn-position N
      :turn-status   :done|:interrupted|...
      :provider :foo  :model \"bar\"
      :iteration-count N
      :iterations [ {:position 1 :input-tokens ... :replay-msgs N
                     :compatible N :thinking-chars N ...} ...]
      :totals     { :input-tokens :output-tokens :cached-tokens
                    :replay-msgs :thinking-chars ... }}"
  [session-id turn-id]
  (let [db    (vis/db-info)
        turn  (->> (vis/db-list-session-turns db session-id)
                (filter #(= turn-id (:id %)))
                first)
        iters (vis/db-list-session-turn-iterations db turn-id)
        rows  (mapv #(iter-summary iters %) (range (count iters)))
        totals (reduce (fn [acc r]
                         (-> acc
                           (update :input-tokens     + (or (:input-tokens r) 0))
                           (update :output-tokens    + (or (:output-tokens r) 0))
                           (update :cached-tokens    + (or (:cached-tokens r) 0))
                           (update :reasoning-tokens + (or (:reasoning-tokens r) 0))
                           (update :replay-msgs      + (or (:replay-msgs r) 0))
                           (update :thinking-chars   + (or (:thinking-chars r) 0))))
                 {:input-tokens 0 :output-tokens 0 :cached-tokens 0
                  :reasoning-tokens 0 :replay-msgs 0 :thinking-chars 0}
                 rows)]
    {:turn-position   (:position turn)
     :turn-status     (:status turn)
     :provider        (:provider turn)
     :model           (:model turn)
     :iteration-count (count iters)
     :iterations      rows
     :totals          totals}))

(defn replay-session
  "Per-turn replay metrics for an entire session.

   Returns:
     {:session-id #uuid \"...\"
      :title :provider :model
      :turns [<replay-turn-result>+]}"
  [session-id]
  (let [db    (vis/db-info)
        sess  (vis/db-get-session db session-id)
        turns (vis/db-list-session-turns db session-id)]
    {:session-id  session-id
     :title       (:title sess)
     :provider    (:provider sess)
     :model       (:model sess)
     :turns       (mapv #(replay-turn session-id (:id %)) turns)}))

(defn replay-session-turn
  "Targeted replay of one (session-id, turn-position) pair. Handy when
   only one turn out of many looped \u2014 most regressions show up on the
   third or later turn, so cherry-picking is the common case."
  [session-id turn-position]
  (let [db   (vis/db-info)
        turn (->> (vis/db-list-session-turns db session-id)
               (filter #(= turn-position (:position %)))
               first)]
    (when turn
      (replay-turn session-id (:id turn)))))

(defn loop-symptom-summary
  "Heuristic flags for the looping pattern observed in sessions 52983a42
   / 831cedee / 3102ad16. Computes per-turn:

     :cache-plateau? - `cached-tokens` stayed within 5% across the last
                       5 iterations (preserved-thinking didn't grow the
                       cache prefix; classic GLM regression).
     :replay-flatlined? - `replay-msgs` per iteration is constant 0 or 1
                       across the turn even though the turn ran multiple
                       iterations (the conservative last-only policy at
                       work).
     :reasoning-spiral? - reasoning-tokens monotonically grew past 2x
                       the median (model is re-deriving scratch state).

   Returns `{:turn-position N :flags #{...}}` per turn."
  [session-id]
  (let [{:keys [turns]} (replay-session session-id)]
    (mapv (fn [{:keys [turn-position iterations]}]
            (let [n (count iterations)
                  tail (take-last 5 iterations)
                  cache-vals (map (fnil identity 0) (map :cached-tokens tail))
                  cache-spread (when (seq cache-vals)
                                 (- (reduce max cache-vals) (reduce min cache-vals)))
                  cache-min (when (seq cache-vals) (reduce min cache-vals))
                  cache-plateau? (and (>= n 5)
                                   (pos? (or cache-min 0))
                                   (< (or cache-spread 0)
                                     (max 1 (* 0.05 (or cache-min 1)))))
                  replay-counts (set (map :replay-msgs iterations))
                  replay-flatlined? (and (>= n 3)
                                      (or (= #{0} replay-counts)
                                        (= #{1} replay-counts)))
                  rt (vec (sort (map (fnil identity 0) (map :reasoning-tokens iterations))))
                  med (when (seq rt) (nth rt (quot (count rt) 2)))
                  rt-last (some-> (last iterations) :reasoning-tokens (or 0))
                  reasoning-spiral? (and (>= n 5)
                                      (pos? (or med 0))
                                      (>= (long rt-last) (* 2 (long med))))]
              {:turn-position turn-position
               :iterations    n
               :flags         (cond-> #{}
                                cache-plateau?    (conj :cache-plateau)
                                replay-flatlined? (conj :replay-flatlined)
                                reasoning-spiral? (conj :reasoning-spiral))}))
      turns)))
