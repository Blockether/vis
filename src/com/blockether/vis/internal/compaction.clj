(ns com.blockether.vis.internal.compaction
  "Side-effect compaction wrappers around the pure engine helpers in
   `ctx-engine`. The engine owns SHAPE decisions
   (`pick-oldest-batch-for-compaction`, `make-compact-stub`,
   `compact-trailer-with-summarizer`); this namespace owns THREADING
   decisions: which companion model to call, with what timeout, what
   fallback if it fails.

   Why split: keeping side-effects out of `ctx-engine` lets the pure
   helpers stay 100% test-friendly (no svar dependency, no thread /
   future juggling) and concentrates HTTP/timeout policy in one place
   where the loop layer can replace it (tests, dry-runs, future
   per-provider tweaks).

   Companion provider routing is conservative:
     - per-main-provider default mapping below
     - opt-in override via `:vis/companion-provider` / `-model` in
       config.edn (TODO: not wired yet)
     - if neither matches OR companion call fails/times out → engine
       dummy summary; no exception bubbles out. Compaction never
       breaks the turn."
  (:require
   [clojure.string :as str]
   [com.blockether.svar.core :as svar]
   [com.blockether.vis.internal.ctx-engine :as ctx-engine]
   [com.blockether.vis.internal.tokens :as tokens]
   [taoensso.telemere :as tel]))

;; =============================================================================
;; Companion model selection
;; =============================================================================

(def DEFAULT_COMPANION_MAPPING
  "Per-main-provider companion model defaults. Each entry MUST resolve
   to a real provider/model pair the router already knows. Cheap +
   fast: companion calls run synchronously inside the turn boundary,
   so seconds matter.

   Map key   = main provider keyword (the one driving the iteration).
   Map value = {:provider <kw> :model <string>}.

   Users can override globally via `:vis/companion-provider` /
   `:vis/companion-model` in config.edn (TODO: wire). Falls back to
   this table when missing."
  {:anthropic-coding-plan     {:provider :anthropic-coding-plan
                               :model    "claude-haiku-4-5"}
   :anthropic                 {:provider :anthropic
                               :model    "claude-haiku-4-5"}
   :openai-codex              {:provider :openai-codex
                               :model    "gpt-5.5-mini"}
   :openai                    {:provider :openai
                               :model    "gpt-4o-mini"}
   :zai-coding-plan           {:provider :zai-coding-plan
                               :model    "glm-4.6"}
   :zai                       {:provider :zai
                               :model    "glm-4.6"}
   :github-copilot-individual {:provider :github-copilot-individual
                               :model    "gpt-4o-mini"}
   :github-copilot-business   {:provider :github-copilot-business
                               :model    "gpt-4o-mini"}})

(defn companion-for
  "Resolve companion routing for `main-provider`. Returns nil when
   neither config override nor default mapping applies — caller should
   skip companion call and fall back to dummy summary."
  [main-provider]
  (when main-provider
    (get DEFAULT_COMPANION_MAPPING (keyword main-provider))))

;; =============================================================================
;; Prompt assembly
;; =============================================================================

(def ^:private COMPACT_PROMPT_HEADER
  "Summarize the coding-agent activity below into 2-3 sentences (under
~150 tokens). Cover what tools were called, what was observed, what
was concluded. Preserve identifiers (file paths, function names,
shas, branch names) verbatim. Plain prose; no headings, no bullets,
no code fences.

ACTIVITY:
")

(defn- batch->pretty
  "Render one compaction batch into a deterministic plain-text body the
   companion LLM can summarise. Avoids EDN noise (key colons, braces)
   for tokens; keeps scopes + src + result/error labelled."
  [batch]
  (let [sb (StringBuilder.)]
    (doseq [pin batch]
      (.append sb "ITERATION ")
      (.append sb (str (or (:scope pin)
                         (str (:scope-start pin) ".." (:scope-end pin)))))
      (.append sb "\n")
      (when-let [summary (:summary pin)]
        (.append sb "  summary: ") (.append sb (str summary)) (.append sb "\n"))
      (doseq [form (:forms pin [])]
        (.append sb "  FORM ") (.append sb (str (:scope form))) (.append sb "\n")
        (when-let [src (:src form)]
          (.append sb "    src: ") (.append sb (str/trim (str src))) (.append sb "\n"))
        (when (contains? form :result)
          (.append sb "    result: ")
          (.append sb (pr-str (:result form)))
          (.append sb "\n"))
        (when (contains? form :error)
          (.append sb "    error: ")
          (.append sb (pr-str (:error form)))
          (.append sb "\n"))))
    (.toString sb)))

;; =============================================================================
;; Companion call — sync with hard timeout, cascade to dummy
;; =============================================================================

(def ^:private COMPANION_TIMEOUT_MS
  "Maximum wall time we wait for the companion LLM to produce a
   compaction summary. The user accepted 15 seconds: at the high end
   of what we can block a turn for, but companion models typically
   resolve in 1-3 s; the cushion only matters for cold start / queue."
  15000)

(defn- extract-first-text-block
  "Pull plain text out of an `ask-code!` response. svar returns
   `{:blocks [...]}` with each block carrying `:source`; with `:lang
   \"text\"` the source is the literal model body. Join all blocks
   so providers that fenced over multiple sections still round-trip."
  [resp]
  (let [blocks (:blocks resp)
        texts  (->> blocks
                 (keep #(some-> % :source str str/trim not-empty))
                 vec)]
    (when (seq texts)
      (str/trim (str/join "\n\n" texts)))))

(defn- summarize-with-companion!
  "Side-effect: ask the companion model for a free-text summary of
   `batch`. Synchronous with `COMPANION_TIMEOUT_MS` hard wall.
   Returns `{:summary str :source :companion-llm}` on success, nil on
   any kind of failure (caller cascades to dummy)."
  [{:keys [router]} main-provider batch]
  (when-let [comp (companion-for main-provider)]
    (when router
      (let [content (str COMPACT_PROMPT_HEADER (batch->pretty batch))
            opts    {:lang              "text"
                     :messages          [{:role    "user"
                                          :content content}]
                     :routing           comp
                     :timeout-ms        COMPANION_TIMEOUT_MS
                     :ttft-timeout-ms   10000
                     :idle-timeout-ms   10000
                     :code-tail-pointer? false}
            fut    (future
                     (try
                       (extract-first-text-block (svar/ask-code! router opts))
                       (catch Throwable t
                         (tel/log! {:level :debug
                                    :id    ::companion-call-failed
                                    :data  {:provider (:provider comp)
                                            :model    (:model comp)
                                            :error    (ex-message t)}})
                         nil)))
            result (deref fut COMPANION_TIMEOUT_MS ::timeout)]
        (when (and (string? result) (seq result))
          {:summary result :source :companion-llm})))))

;; =============================================================================
;; Public API
;; =============================================================================

(defn compact-trailer-now!
  "Run one round of trailer compaction. Mutates `env`'s `:ctx-atom` in
   place. Pure decisions go through
   `ctx-engine/compact-trailer-with-summarizer`; this fn wires in the
   companion summariser and the side-effect ctx-atom swap.

   `opts`:
     :target-tokens   trailer-total target after compaction (caller
                      typically sets this BELOW the budget so we leave
                      headroom for next iter)
     :born-scope      `tN/iM/fK` stamp on the new summary stub
     :main-provider   keyword of the provider driving the iteration;
                      used to pick the companion model. Nil → engine
                      dummy summary.

   Returns the same map `ctx-engine/compact-trailer-with-summarizer`
   returns, plus `:duration-ms`. Never throws."
  [{:keys [ctx-atom] :as env} {:keys [target-tokens born-scope main-provider]}]
  (if-not ctx-atom
    {:compacted? false :reason :no-ctx-atom}
    (let [summarizer (fn [batch]
                       (summarize-with-companion! env main-provider batch))
          start-ms   (System/currentTimeMillis)
          result-box (atom nil)
          _          (swap! ctx-atom
                       (fn [ctx]
                         (let [r (ctx-engine/compact-trailer-with-summarizer
                                   ctx
                                   {:target-tokens target-tokens
                                    :born-scope    born-scope
                                    :summarizer-fn summarizer})]
                           (reset! result-box (dissoc r :ctx))
                           (:ctx r))))
          r           @result-box
          duration-ms (- (System/currentTimeMillis) start-ms)
          source      (when (:compacted? r)
                        (or (some-> r :warnings first :data
                              (#(if (re-find #"companion-LLM" (str %)) :companion-llm :engine-dummy)))
                          :engine-dummy))]
      (when (:compacted? r)
        (tel/log! {:level :info
                   :id    ::trailer-auto-compacted
                   :data  {:scope-range  (:scope-range r)
                           :batch-size   (:batch-size r)
                           :tokens-freed (:tokens-freed r)
                           :source       source
                           :duration-ms  duration-ms}}
          (str "Trailer auto-compacted: " (:batch-size r)
            " pin(s) freed ~" (:tokens-freed r) " tokens in "
            duration-ms "ms.")))
      (merge r {:duration-ms duration-ms :source source}))))

;; =============================================================================
;; Render-time budget enforcement
;; =============================================================================

(def DEFAULT_PROMPT_BUDGET_TOKENS
  "Total prompt-token ceiling for a single provider call. Defaults to
   72% of a typical 200k-token window — the user accepted this
   threshold explicitly. Leaves ~56k tokens of headroom for the model's
   own output plus margin for tokenizer disagreement (cl100k vs
   provider-native)."
  144000)

(def ^:private MAX_COMPACTION_ROUNDS
  "Hard cap on how many compaction round-trips a single render can
   trigger. 3 rounds * 15s companion timeout = 45s worst case before
   we ship the prompt anyway. In practice rounds 2 and 3 hit dummy
   fallback after the first companion call satisfied (or didn't), so
   real-world latency is one network round-trip plus a few ms."
  3)

(def ^:private COMPACTION_SAFETY_MARGIN_TOKENS
  "Trailer compaction frees AT LEAST (delta + this) tokens so a single
   long-tail iteration's growth does not immediately push the prompt
   back over budget on the next turn. 4 000 tok keeps room for one
   medium tool result before the next compaction has to run."
  4000)

(defn- iter-ctx-msg
  "Wrap the rendered ctx string into the user-role message shape that
   the loop hands off to svar. Mirrors the assembly site in loop.clj
   — if that ever changes shape, this helper must follow."
  [ctx-rendered]
  {:role "user" :content (or ctx-rendered "")})

(defn estimate-prompt-tokens
  "Sum the tokens for the full message vec that will go to the
   provider this iteration: stable system prompts + initial user
   message + per-iter ctx. Uses jtokkit cl100k_base — approximation
   only, ~10-30% margin per provider; precision is sufficient for
   compaction triggers."
  [stable-msgs ctx-rendered]
  (+ (tokens/count-prompt-tokens (or stable-msgs []))
    (tokens/count-prompt-tokens [(iter-ctx-msg ctx-rendered)])))

(defn ensure-prompt-under-budget!
  "Render the per-iter ctx; if the resulting prompt total exceeds
   `:budget-tokens`, run trailer compaction round-trips until either
   the prompt fits or `MAX_COMPACTION_ROUNDS` is hit. Returns:

     {:rendered     <ctx-string>            ; final rendered ctx
      :total-tokens <long>                   ; final estimated total
      :over-budget? <bool>                   ; true if still over after all rounds
      :rounds       [<compact-result> ...]}  ; per-round detail (engine summary)

   `opts`:
     :render-fn      `(fn [env] -> ctx-string)` — typically a partial
                     over `ctx-loop/render-block!` + the engine renderer.
     :stable-msgs    system + user + previous-turn fixed prefix; used
                     for the token estimate (not mutated).
     :budget-tokens  total prompt cap (default DEFAULT_PROMPT_BUDGET_TOKENS).
     :main-provider  drives companion model selection.
     :born-scope     `tN/iM/fK` stamped onto auto-summary stubs.

   Compaction policy: each round targets a trailer total small enough
   that the freed delta plus a safety margin brings the prompt under
   budget. If compaction returns `:compacted? false` (already empty /
   only one pin left), the loop exits early — trailer can't shrink any
   further and the panic path / facts compaction (TODO) takes over.

   Never throws."
  [env {:keys [render-fn stable-msgs budget-tokens main-provider born-scope]
        :or   {budget-tokens DEFAULT_PROMPT_BUDGET_TOKENS}}]
  (let [render-once (fn [] (render-fn env))
        measure     (fn [r] (estimate-prompt-tokens stable-msgs r))]
    (loop [round    0
           rendered (render-once)
           total    (measure (render-once))
           rounds   []]
      (cond
        (<= total budget-tokens)
        {:rendered rendered :total-tokens total :over-budget? false :rounds rounds}

        (>= round MAX_COMPACTION_ROUNDS)
        (do
          (tel/log! {:level :warn :id ::compaction-rounds-exhausted
                     :data {:total-tokens total :budget-tokens budget-tokens
                            :rounds       (count rounds)}}
            (str "Compaction exhausted " MAX_COMPACTION_ROUNDS
              " rounds; prompt still " total " tok (> " budget-tokens
              "). Sending anyway — provider may reject."))
          {:rendered rendered :total-tokens total :over-budget? true :rounds rounds})

        :else
        (let [;; how many tokens we need to drop from trailer:
              ;; (current_total - budget) + safety_margin.
              ;; That delta MUST come from compactable surface; we feed
              ;; the trailer pure helper a "target" that is (current
              ;; trailer total - required-drop).
              ctx-atom-snap (some-> (:ctx-atom env) deref)
              trailer       (or (:session/trailer ctx-atom-snap) [])
              trailer-toks  (ctx-engine/trailer-total-tokens trailer)
              required-drop (+ (- total budget-tokens) COMPACTION_SAFETY_MARGIN_TOKENS)
              target        (max 0 (- trailer-toks required-drop))
              r             (compact-trailer-now! env
                              {:target-tokens target
                               :born-scope    born-scope
                               :main-provider main-provider})]
          (if-not (:compacted? r)
            (do
              (tel/log! {:level :warn :id ::compaction-cannot-shrink
                         :data {:total-tokens total :budget-tokens budget-tokens
                                :trailer-tokens trailer-toks}}
                "Trailer compaction returned no-op (one pin left or already empty). Stopping cascade.")
              {:rendered     rendered
               :total-tokens total
               :over-budget? true
               :rounds       (conj rounds r)})
            (let [rendered' (render-once)
                  total'    (measure rendered')]
              (recur (inc round)
                rendered'
                total'
                (conj rounds r)))))))))
