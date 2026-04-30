(ns com.blockether.vis.internal.diagnose
  "Render a Markdown diagnostic report for a single conversation.

   Pulls every turn (`query_soul`), every retry (`query_state`), and
   every iteration (`iteration` row + decoded `iteration.blocks` Nippy
   blob) for the conversation, then summarises:

     * Per-turn rollup: status, attempts, iteration count, total cost,
       failure count, error messages.
     * Per-iteration block-level failure log (the same shape the
       agent-authored REPRODUCTION.md tried to produce by hand).

   This is the data path behind the `vis diagnose <conv-id>` CLI
   subcommand. Lives next to `parse_diagnose.clj` so the two
   diagnostic surfaces share a directory — one for live iteration-
   loop hints (parse-diagnose), one for post-mortem conversation
   reports (this ns).

   Public surface:

     `(render db-info conversation-id)` → Markdown string.

   The renderer is total: a missing conversation returns a one-line
   \"Conversation not found\" string instead of throwing, so the CLI
   can shell-redirect cleanly even on bad input."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.persistance :as persistance])
  (:import
   [java.util Locale]))

;; ---------------------------------------------------------------------------
;; Aggregation helpers — cheap pure fns over the maps the persistance
;; facade returns. Kept here (not in introspection.clj) because the
;; CLI needs them without dragging the SCI / RLM environment in.
;; ---------------------------------------------------------------------------

(defn- truncate
  "Trim `s` to at most `n` chars; if it had to be cut, append an
   ellipsis. nil-safe."
  [s n]
  (let [s (str s)]
    (if (<= (count s) n) s (str (subs s 0 n) "…"))))

(defn- one-line
  "Squash whitespace runs (incl. newlines) to single spaces. Used for
   cells where the original goal / message may span multiple lines."
  [s]
  (-> (or s "") str (str/replace #"\s+" " ") str/trim))

(defn- pad-cell
  "Markdown table cell escape: convert pipe to its HTML entity so the
   embedded text doesn't break the table boundary. Keep backticks
   intact — they belong to inline code we render elsewhere."
  [s]
  (-> (or s "") str (str/replace "|" "\\|") (str/replace "\n" " ")))

(defn- format-cost-usd
  "Render a USD amount with four-decimal precision. ALWAYS uses
   `Locale/US` so the decimal separator is a dot (`$0.0042`), never
   a locale-dependent comma (`$0,0042`). nil is treated as $0.0000
   so callers don't need to `or`-pad. The four-decimal precision is
   intentional: real per-iteration costs land in the
   $0.0001 — $0.50 band, where four digits matter."
  [c]
  (let [v (double (or c 0.0))]
    (String/format Locale/US "$%.4f" (object-array [v]))))

(defn- format-tokens
  "`\"input/output\"` pair, with cached/reasoning sub-fields when
   non-zero. Tokens are always numeric on the read side (the
   persistance layer defaults NULL columns to 0), so this fn just
   reads them — no `or`-padding here either."
  [{:keys [input-tokens output-tokens reasoning-tokens cached-tokens]}]
  (let [base (str (long (or input-tokens 0)) "/" (long (or output-tokens 0)))
        suff (cond-> []
               (and reasoning-tokens (pos? (long reasoning-tokens)))
               (conj (str "r=" reasoning-tokens))
               (and cached-tokens (pos? (long cached-tokens)))
               (conj (str "c=" cached-tokens)))]
    (if (seq suff)
      (str base " (" (str/join ", " suff) ")")
      base)))

(defn- iteration-totals
  "Sum the cost / token columns across a vector of iteration maps.
   Every value is already numeric on the read side (the persistance
   layer defaults NULL columns to `0` / `0.0`), so this is a clean
   reduce — no nil handling, no \"$0.0000 vs —\" branching. Returns a
   map with :input-tokens :output-tokens :reasoning-tokens
   :cached-tokens :cost-usd, all guaranteed numeric."
  [iterations]
  (reduce (fn [a it]
            (-> a
              (update :input-tokens     + (long   (or (:input-tokens it) 0)))
              (update :output-tokens    + (long   (or (:output-tokens it) 0)))
              (update :reasoning-tokens + (long   (or (:reasoning-tokens it) 0)))
              (update :cached-tokens    + (long   (or (:cached-tokens it) 0)))
              (update :cost-usd         + (double (or (:cost-usd it) 0.0)))))
    {:input-tokens 0 :output-tokens 0 :reasoning-tokens 0
     :cached-tokens 0 :cost-usd 0.0}
    iterations))

;; ---------------------------------------------------------------------------
;; Per-turn projection — the row that fills the `Turn` block in the
;; rendered Markdown. The fields here MUST mirror REPRODUCTION.md's
;; intended shape so future diff-against-snapshot tests stay stable.
;; ---------------------------------------------------------------------------

(defn- turn-row
  "Project one `query_soul` row + its iterations into the rollup map
   used by the renderer."
  [db-info query]
  (let [iters (try (persistance/db-list-query-iterations db-info (:id query))
                (catch Throwable _ []))
        totals (iteration-totals iters)
        block-failures (vec
                         (mapcat
                           (fn [it]
                             (let [blocks (try (persistance/db-list-iteration-blocks db-info (:id it))
                                            (catch Throwable _ []))]
                               (->> blocks
                                 (keep-indexed (fn [idx b]
                                                 (when-let [err (:error b)]
                                                   {:iteration-position (:position it)
                                                    :iteration-id       (:id it)
                                                    :block-idx          idx
                                                    :code               (:code b)
                                                    :error              err}))))))
                           iters))]
    (cond-> {:id            (:id query)
             :goal          (or (:text query) (:goal query) "")
             :status        (:status query)
             :prior-outcome (:prior-outcome query)
             :iteration-count (count iters)
             :failure-count  (count block-failures)
             :failures       block-failures
             :tokens         totals
             :cost-usd       (:cost-usd totals)}
      (:answer query) (assoc :answer (:answer query)))))

;; ---------------------------------------------------------------------------
;; Markdown rendering. Plain string concatenation — no templating
;; library, no dependency on the prompt / md helpers (those carry SCI
;; baggage that the CLI does not need).
;; ---------------------------------------------------------------------------

(defn- render-header [conv turn-rows]
  (let [created     (:created-at conv)
        total-cost  (reduce + 0.0 (map (fn [t] (double (or (:cost-usd t) 0.0))) turn-rows))
        total-iters (reduce + 0   (map :iteration-count turn-rows))]
    (str
      "# Diagnostic report — conversation `" (:id conv) "`\n"
      "\n"
      "- **Title:** " (or (:title conv) "—") "\n"
      "- **Channel:** " (or (some-> (:channel conv) name) "—") "\n"
      "- **Model:** " (or (:model conv) "—") "\n"
      "- **Created:** " (or created "—") "\n"
      "- **Total turns:** " (count turn-rows) "\n"
      "- **Total iterations:** " total-iters "\n"
      "- **Total cost (USD):** " (format-cost-usd total-cost) "\n"
      "\n")))

(defn- render-turn-block [{:keys [id goal status prior-outcome
                                  iteration-count failure-count
                                  failures tokens cost-usd]}]
  (str
    "### Turn `" id "`\n"
    "- **Goal:** " (one-line goal) "\n"
    "- **Status:** " (or (some-> status name) "—")
    (when prior-outcome (str " (" (name prior-outcome) ")")) "\n"
    "- **Iterations:** " iteration-count "\n"
    "- **Failures:** " failure-count "\n"
    "- **Tokens (in/out):** " (format-tokens tokens) "\n"
    "- **Cost:** " (format-cost-usd cost-usd) "\n"
    (when (seq failures)
      (str "\n#### Failures\n\n"
        "| Iter | Block | Error |\n"
        "|------|-------|-------|\n"
        (str/join "\n"
          (map (fn [{:keys [iteration-position block-idx code error]}]
                 (str "| " iteration-position
                   " | " block-idx
                   " — `" (pad-cell (truncate (one-line code) 50)) "`"
                   " | " (pad-cell (truncate (one-line error) 80)) " |"))
            failures))
        "\n"))
    "\n"))

(defn- render-failure-log [turn-rows]
  (let [all (->> turn-rows
              (mapcat (fn [t]
                        (map #(assoc % :turn-id (:id t)
                                :goal (:goal t)) (:failures t))))
              vec)]
    (when (seq all)
      (str "## Failure log (every block-level error in this conversation)\n\n"
        "| # | Turn | Iter | Code | Error |\n"
        "|---|------|------|------|-------|\n"
        (str/join "\n"
          (map-indexed
            (fn [i {:keys [turn-id iteration-position code error]}]
              (str "| " (inc i)
                " | `" (pad-cell (truncate turn-id 8)) "`"
                " | " iteration-position
                " | `" (pad-cell (truncate (one-line code) 60)) "`"
                " | " (pad-cell (truncate (one-line error) 80)) " |"))
            all))
        "\n"))))

(defn render
  "Render the full Markdown diagnostic for `conversation-id`. Returns
   a string. Returns `\"Conversation not found: <id>\\n\"` (no throw)
   when the id does not resolve, so the CLI can shell-redirect even
   on bad input."
  [db-info conversation-id]
  (let [conv (try (persistance/db-get-conversation db-info conversation-id)
               (catch Throwable _ nil))]
    (if (nil? conv)
      (str "Conversation not found: " conversation-id "\n")
      (let [queries (try (persistance/db-list-conversation-queries db-info conversation-id)
                      (catch Throwable _ []))
            turn-rows (mapv (partial turn-row db-info) queries)]
        (str
          (render-header conv turn-rows)
          "## Turn-by-turn breakdown\n\n"
          (apply str (map render-turn-block turn-rows))
          (render-failure-log turn-rows))))))
