(ns com.blockether.vis.ext.foundation.transcript
  "Full conversation transcript — DATA first, presentation second.

   `(v/transcript [conversation-id])` returns one canonical Clojure
   map with every turn, every iteration, every executed block plus
   the LLM-side context (system prompt, message envelope, reasoning
   trace, top-level provider error, per-iteration vars, answer-form
   pointer, returned-empty-blocks flag) and the per-block forensic
   detail (code, comment, result, error, stdout, stderr, duration,
   timeout?, repaired?). Pure data. The agent can pattern-match on
   it; the CLI renders Markdown on top; a future TUI screen, JSON
   exporter, or analytics extension consumes the same shape.

   Lives in foundation (alongside `v/turn`, `v/conversation`,
   `v/diagnose`) because it's an introspection surface, not host
   plumbing.

   Public surface:

     `(transcript    db-info conv-id)`  → data map (canonical shape)
     `(transcript-md db-info conv-id)`  → Markdown string

     `register-cli!` mounts `vis diagnose <CONVERSATION-ID>` at the
     top of the command tree, mirroring the `vis doctor` pattern.

   Canonical data shape:

     {:conversation {:id :title :channel :model :provider :created-at}
      :totals       {:turns N :iterations N
                     :tokens {:input :output :reasoning :cached}
                     :cost-usd D}
      :turns
       [{:id :goal :status :prior-outcome :provider :model
         :iteration-count :failure-count
         :tokens :cost-usd :answer
         :iterations
          [{:id :position :status :duration-ms
            :provider :model :thinking :error
            :tokens :cost-usd
            :answer-form-idx :returned-empty-blocks?
            :llm-system-prompt :llm-user-prompt :metadata
            :vars
            [{:name :code :value :version}]
            :blocks
            [{:idx :code :comment :result :error
              :stdout :stderr :duration-ms
              :timeout? :repaired?}]}]}]}

   The Markdown renderer renders thinking, iteration-level errors,
   vars, the per-block forensic dump, and the final answer text.
   `:llm-system-prompt` and `:llm-user-prompt` are intentionally NOT
   rendered by default — they're multi-KB × N iterations and would
   blow up the file; they live in DATA so programmatic readers can
   opt in (e.g. `(:llm-system-prompt (first (:iterations turn)))`)."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis])
  (:import
   [java.util Locale]))

;; =============================================================================
;; Data layer.
;; =============================================================================

(defn- iteration-rollup
  "Sum the per-iteration token + cost columns across a vec of
   iteration maps. Every value is numeric on the read side (the
   persistance layer defaults NULL columns to 0 / 0.0), so this is
   a clean reduce."
  [iterations]
  (reduce (fn [a it]
            (-> a
              (update-in [:tokens :input]     + (long   (or (:input-tokens it) 0)))
              (update-in [:tokens :output]    + (long   (or (:output-tokens it) 0)))
              (update-in [:tokens :reasoning] + (long   (or (:reasoning-tokens it) 0)))
              (update-in [:tokens :cached]    + (long   (or (:cached-tokens it) 0)))
              (update    :cost-usd            + (double (or (:cost-usd it) 0.0)))))
    {:tokens {:input 0 :output 0 :reasoning 0 :cached 0}
     :cost-usd 0.0}
    iterations))

(defn- enrich-iteration
  "Attach `:blocks` and `:vars` to one iteration row.

     `:blocks` — every executed form (Nippy-encoded inline log).
     `:vars`   — every `(def …)` this iteration produced; reads from
                the separate `expression_soul` / `expression_state`
                facade. Each entry is `{:name :code :value :version}`.

   Both reads degrade silently to `[]` so the renderer never
   throws on a partial DB."
  [db-info iter]
  (let [blocks (try (vec (vis/db-list-iteration-blocks db-info (:id iter)))
                 (catch Throwable _ []))
        vars   (try (vec (vis/db-list-iteration-vars db-info (:id iter)))
                 (catch Throwable _ []))]
    (-> iter
      (assoc :blocks blocks)
      (assoc :vars   vars)
      (assoc :failure-count (count (filter :error blocks))))))

(defn- build-turn
  "Pure projection: one query_soul row + its iterations → the
   turn-shaped data map the public `transcript` returns."
  [db-info query]
  (let [raw-iters (try (vis/db-list-query-iterations db-info (:id query))
                    (catch Throwable _ []))
        iters     (mapv (partial enrich-iteration db-info) raw-iters)
        totals    (iteration-rollup iters)
        provider  (some #(some-> % :provider name) iters)
        model     (some :model iters)]
    (cond-> {:id              (:id query)
             :goal            (or (:text query) (:goal query) "")
             :status          (:status query)
             :prior-outcome   (:prior-outcome query)
             :iteration-count (count iters)
             :failure-count   (reduce + 0 (map :failure-count iters))
             :iterations      iters
             :tokens          (:tokens totals)
             :cost-usd        (:cost-usd totals)}
      provider        (assoc :provider provider)
      model           (assoc :model model)
      (:answer query) (assoc :answer (:answer query)))))

(defn- conversation-totals
  "Sum tokens + cost + iteration counts across every turn."
  [turns]
  (reduce (fn [a t]
            (-> a
              (update :iterations + (long (or (:iteration-count t) 0)))
              (update-in [:tokens :input]     + (long   (or (:input     (:tokens t)) 0)))
              (update-in [:tokens :output]    + (long   (or (:output    (:tokens t)) 0)))
              (update-in [:tokens :reasoning] + (long   (or (:reasoning (:tokens t)) 0)))
              (update-in [:tokens :cached]    + (long   (or (:cached    (:tokens t)) 0)))
              (update :cost-usd               + (double (or (:cost-usd  t) 0.0)))))
    {:turns      (count turns)
     :iterations 0
     :tokens     {:input 0 :output 0 :reasoning 0 :cached 0}
     :cost-usd   0.0}
    turns))

(defn transcript
  "Full conversation transcript as one Clojure data map. See ns
   docstring for the canonical shape. Returns nil when the
   conversation id does not resolve.

   Pure with respect to the database — no writes, no logging.
   `(:db-info env)` is the standard handle; the SCI-bound symbol
   variant uses the live env automatically."
  [db-info conversation-id]
  (when-let [conv (try (vis/db-get-conversation db-info conversation-id)
                    (catch Throwable _ nil))]
    (let [queries (try (vis/db-list-conversation-queries db-info conversation-id)
                    (catch Throwable _ []))
          turns   (mapv (partial build-turn db-info) queries)
          totals  (conversation-totals turns)]
      {:conversation (cond-> {:id         conversation-id
                              :title      (:title conv)
                              :channel    (:channel conv)
                              :model      (:model conv)
                              :created-at (:created-at conv)}
                       (:provider conv) (assoc :provider (:provider conv)))
       :totals       totals
       :turns        turns})))

;; =============================================================================
;; Markdown renderer. Pure transformation over `transcript`'s data
;; shape — no DB calls, no side effects.
;; =============================================================================

(defn- truncate
  [s n]
  (let [s (str s)]
    (if (<= (count s) n) s (str (subs s 0 n) "…"))))

(defn- one-line
  [s]
  (-> (or s "") str (str/replace #"\s+" " ") str/trim))

(defn- format-cost-usd
  "Locale-stable USD formatter — always a dot separator (`$0.0042`),
   never a locale comma. nil collapses to `$0.0000` so callers don't
   `or`-pad."
  [c]
  (let [v (double (or c 0.0))]
    (String/format Locale/US "$%.4f" (object-array [v]))))

(defn- format-tokens
  [{:keys [input output reasoning cached]}]
  (let [base (str (long (or input 0)) "/" (long (or output 0)))
        suff (cond-> []
               (and reasoning (pos? (long reasoning)))
               (conj (str "r=" reasoning))
               (and cached (pos? (long cached)))
               (conj (str "c=" cached)))]
    (if (seq suff)
      (str base " (" (str/join ", " suff) ")")
      base)))

(defn- render-fenced
  [lang body]
  (let [s (str body)]
    (if (str/blank? s)
      ""
      (str "```" (or lang "") "\n" s "\n```\n"))))

(defn- render-collapsible
  "GitHub-flavored Markdown `<details>` block. Default-collapsed so
   the file stays scannable; users (and viewers like mdBook / VS Code
   / GitHub) expand on demand. The summary line shows what's inside +
   its size so the reader knows whether to click. Empty `body` yields
   the empty string so the block disappears entirely."
  [summary body]
  (let [s (str body)]
    (if (str/blank? s)
      ""
      (str "<details><summary>" summary "</summary>\n\n"
        s
        "</details>\n\n"))))

(defn- render-block-section
  "Per-block forensic dump: status header, optional comment, full code
   in a fenced ```clojure block, result line, fenced stdout/stderr,
   fenced error. `answer?` flips on the block the iteration's
   `:answer-form-idx` points at — the form that called `(answer …)` —
   so the reader spots the terminal block at a glance.

   Truncation budgets stay generous (4KB stdout / stderr, 800 chars
   on the pr-str of result) so the report is forensic, not a
   one-pager."
  [idx answer? {:keys [code comment result error stdout stderr] :as block}]
  (let [marker      (if error "✗" "✓")
        flags       (cond-> []
                      answer?            (conj "answer")
                      (:timeout? block)  (conj "timeout")
                      (:repaired? block) (conj "repaired")
                      error              (conj "error"))
        suffix      (if (seq flags) (str " [" (str/join ", " flags) "]") "")
        has-result? (and (not error) (contains? block :result))]
    (str "##### Block " idx " — " marker " "
      (long (or (:duration-ms block) 0)) "ms" suffix "\n"
      (when (not (str/blank? comment)) (str comment "\n"))
      (render-fenced "clojure" code)
      (when has-result?
        (str "\nResult: `" (truncate (pr-str result) 800) "`\n"))
      (when (not (str/blank? stdout))
        (str "\n_stdout:_\n" (render-fenced "text" (truncate stdout 4096))))
      (when (not (str/blank? stderr))
        (str "\n_stderr:_\n" (render-fenced "text" (truncate stderr 4096))))
      (when error
        (str "\n_error:_\n" (render-fenced "text" error)))
      "\n")))

(defn- render-thinking
  "Render the LLM's reasoning trace as a fenced text block. Many
   models stream multi-KB reasoning per iteration; preserved
   verbatim. Hard cap at 16 KB so a runaway thinking trace doesn't
   blow up the file."
  [thinking]
  (when (not (str/blank? thinking))
    (str "_thinking:_\n"
      (render-fenced "text" (truncate thinking 16384))
      "\n")))

(defn- render-iter-error
  "Render an iteration-level error — the provider call failed before
   any block could run. Persisted on `iteration.llm_error` as JSON.
   nil/blank → nothing emitted."
  [error]
  (when (and error (not (str/blank? (str error))))
    (str "_iteration error:_\n"
      (render-fenced "text" (str error))
      "\n")))

(defn- render-vars
  "Compact list of `(def …)` rows produced by this iteration. One
   bullet per var with truncated code preview + truncated value
   pr-str. Empty / nil → nothing emitted."
  [vars]
  (when (seq vars)
    (str "_vars defined this iteration:_\n\n"
      (str/join "\n"
        (map (fn [{:keys [name code value version]}]
               (str "- `" name "`"
                 (when version (str " (v" version ")"))
                 (when (and code (not (str/blank? code)))
                   (str " — `" (truncate (one-line code) 80) "`"))
                 (when (some? value)
                   (str " → `" (truncate (pr-str value) 80) "`"))))
          vars))
      "\n\n")))

(defn- render-system-prompt
  "Collapsible `<details>` block carrying the full assembled system
   prompt for this iteration. `prev-system-prompt` is the system
   prompt of the previous iteration in the same turn (nil on the
   first iteration); when both match we render a tiny
   \"unchanged from iteration N-1\" stub so a 16-iteration turn
   doesn't carry 16 identical 8KB system-prompt copies inline."
  [system-prompt prev-system-prompt prev-pos]
  (when (not (str/blank? system-prompt))
    (let [size  (count system-prompt)
          same? (and prev-system-prompt (= system-prompt prev-system-prompt))]
      (if same?
        (render-collapsible
          (str "System prompt (" size " chars, unchanged from iteration " prev-pos ")")
          "_(identical to the previous iteration's system prompt)_\n")
        (render-collapsible
          (str "System prompt (" size " chars)")
          (render-fenced "text" system-prompt))))))

(defn- render-llm-messages
  "Collapsible `<details>` block carrying the full LLM message
   envelope for this iteration: every `[{:role :content}]` pair the
   provider was called with. Each message renders as its own
   `_role:_` fenced text block. Empty / nil envelope → no output."
  [messages]
  (when (seq messages)
    (let [body (apply str
                 (map (fn [{:keys [role content]}]
                        (str "_" (or role "?") ":_\n"
                          (render-fenced "text" (str content))
                          "\n"))
                   messages))]
      (render-collapsible
        (str "LLM messages (" (count messages) " messages, "
          (reduce + 0 (map #(count (str (:content %))) messages)) " chars total)")
        body))))

(defn- render-iteration-section [iter prev-iter]
  (let [pos     (:position iter)
        status  (or (some-> (:status iter) name) "—")
        dur     (or (:duration-ms iter) 0)
        in      (or (:input-tokens iter) 0)
        out     (or (:output-tokens iter) 0)
        cost    (or (:cost-usd iter) 0.0)
        blocks  (:blocks iter)
        ;; Index of the block that called `(answer …)`. nil for
        ;; non-terminal iterations — the marker only fires on the
        ;; right block.
        ans-idx (:answer-form-idx iter)]
    (str "\n#### Iteration " pos " — " status
      " (" in "/" out " tokens, " (format-cost-usd cost) ", " (long dur) "ms)\n\n"
      (render-thinking (:thinking iter))
      (render-iter-error (:error iter))
      (render-system-prompt (:llm-system-prompt iter)
        (:llm-system-prompt prev-iter)
        (:position prev-iter))
      (render-llm-messages (:llm-user-prompt iter))
      (render-vars (:vars iter))
      (cond
        (empty? blocks)
        "_No code blocks (LLM returned an empty response)._\n"

        :else
        (apply str (map-indexed (fn [idx block]
                                  (render-block-section idx (= idx ans-idx) block))
                     blocks))))))

(defn- render-final-answer
  "Final answer text the turn settled on, persisted on
   `query_state.metadata.answer`. Rendered after every iteration so
   the reader sees the trajectory that led to it. nil/blank → nothing
   emitted."
  [answer]
  (when (not (str/blank? (str answer)))
    (str "\n#### Final answer\n\n"
      (render-fenced "text" answer))))

(defn- render-turn-block
  [{:keys [id goal status prior-outcome provider model
           iteration-count failure-count
           iterations tokens cost-usd answer]}]
  (str
    "### Turn `" id "`\n"
    "- **Goal:** " (one-line goal) "\n"
    "- **Status:** " (or (some-> status name) "—")
    (when prior-outcome (str " (" (name prior-outcome) ")")) "\n"
    "- **Provider/model:** "
    (or (cond
          (and provider model) (str provider "/" model)
          model                model
          provider             provider)
      "—") "\n"
    "- **Iterations:** " iteration-count "\n"
    "- **Failures:** " failure-count "\n"
    "- **Tokens (in/out):** " (format-tokens tokens) "\n"
    "- **Cost:** " (format-cost-usd cost-usd) "\n"
    ;; Pair each iteration with the previous one in the same turn so
    ;; the renderer can dedupe an unchanged system prompt instead of
    ;; emitting the same 8 KB block 16 times.
    (apply str (map render-iteration-section iterations (cons nil iterations)))
    (render-final-answer answer)
    "\n"))

(defn- render-header [{:keys [conversation totals]}]
  (str
    "# Diagnostic report — conversation `" (:id conversation) "`\n"
    "\n"
    "- **Title:** "    (or (:title    conversation) "—") "\n"
    "- **Channel:** "  (or (some-> (:channel conversation) name) "—") "\n"
    "- **Model:** "    (or (:model    conversation) "—") "\n"
    "- **Created:** "  (or (:created-at conversation) "—") "\n"
    "- **Total turns:** "      (:turns totals) "\n"
    "- **Total iterations:** " (:iterations totals) "\n"
    "- **Total cost (USD):** " (format-cost-usd (:cost-usd totals)) "\n"
    "- **Total tokens (in/out):** " (format-tokens (:tokens totals)) "\n"
    "\n"))

(defn transcript-md
  "Render the conversation as Markdown. Single transformation over
   `transcript`'s data. Returns a string; returns
   `\"Conversation not found: <id>\\n\"` (no throw) on a missing id so
   shell pipelines stay clean."
  [db-info conversation-id]
  (if-let [data (transcript db-info conversation-id)]
    (str (render-header data)
      "## Turn-by-turn breakdown\n\n"
      (apply str (map render-turn-block (:turns data))))
    (str "Conversation not found: " conversation-id "\n")))

;; =============================================================================
;; Sandbox symbol — hooked into foundation's `:ext/symbols` vec via
;; `core.clj`.
;; =============================================================================

(defn- sandbox-transcript
  "SCI-bound entry point. Pulls the env-injected `:db-info` and
   defaults `conversation-id` to the current conversation when the
   agent calls `(v/transcript)` with no arg."
  ([env]
   (sandbox-transcript env (:conversation-id env)))
  ([env conversation-id]
   (when (and (:db-info env) conversation-id)
     (transcript (:db-info env) conversation-id))))

(def transcript-symbol
  (vis/symbol 'transcript sandbox-transcript
    {:doc       (str "Full conversation transcript as ONE Clojure data map: "
                  "{:conversation :totals :turns}. Each turn carries every "
                  "iteration; each iteration carries thinking, top-level "
                  "error, vars, answer-form-idx, returned-empty-blocks?, "
                  ":llm-system-prompt, :llm-user-prompt, :metadata, plus "
                  "every executed block (:code :comment :result :error "
                  ":stdout :stderr :duration-ms :timeout? :repaired?) and "
                  "the per-iteration :tokens / :cost-usd. Default = "
                  "current conversation; pass an id (or unambiguous "
                  "prefix) to inspect another. Pure data — the CLI's "
                  "`vis diagnose` renders Markdown on top of this.")
     :arglists  '([] [conversation-id])
     :examples  ["(v/transcript)"
                 "(map :goal (:turns (v/transcript)))"
                 "(:cost-usd (:totals (v/transcript)))"
                 "(map :thinking (mapcat :iterations (:turns (v/transcript))))"
                 "(filter :error (mapcat :blocks (mapcat :iterations (:turns (v/transcript)))))"]
     :before-fn (fn [args env _ctx] (cons env args))}))

;; =============================================================================
;; CLI command — `vis diagnose <CONVERSATION-ID>`. Foundation owns it
;; (mirrors the `vis doctor` pattern in doctor.clj).
;; =============================================================================

(defn- println-original!
  [^String s]
  (.println ^java.io.PrintStream vis/original-stdout s)
  (.flush ^java.io.PrintStream vis/original-stdout))

(defn- cli-diagnose-run!
  [_parsed residual]
  (vis/init-cli!)
  (let [cid-input (some-> (first residual) str/trim not-empty)]
    (cond
      (nil? cid-input)
      (do (println-original! "Usage: vis diagnose <CONVERSATION-ID>")
        (println-original! "")
        (println-original! "List conversations with:  vis conversations")
        (System/exit 1))

      :else
      (let [d        (vis/db-info)
            resolved (try (vis/db-resolve-conversation-id d cid-input)
                       (catch Throwable _ nil))]
        (if (nil? resolved)
          (do (println-original! (str "Conversation not found: " cid-input))
            (System/exit 1))
          (do (println-original! (transcript-md d resolved))
            (System/exit 0)))))))

(defn register-cli! []
  (vis/register-cmd!
    {:cmd/name  "diagnose"
     :cmd/doc   "Print a forensic Markdown report for a conversation: every turn, every iteration, every executed code block (code, comment, result, stdout, stderr, error, duration), every (def ...) var, plus the LLM reasoning trace and the final answer. Resolves an unambiguous id prefix the same way `vis conversations --fork` does."
     :cmd/usage "vis diagnose <CONVERSATION-ID>"
     :cmd/args  [{:name "conversation-id" :kind :positional :type :string
                  :doc  "Conversation id (full UUID or unambiguous prefix)."}]
     :cmd/examples ["vis diagnose eeaf9651-06c7-4dda-9e97-877fcef06337"
                    "vis diagnose eeaf9651"
                    "vis diagnose eeaf9651 > REPRODUCTION.md"]
     :cmd/run-fn cli-diagnose-run!}))
