(ns com.blockether.vis.internal.limits-format
  "Channel-neutral `{:dynamic {:limits [...]}}` row formatters.

   Hoisted from the TUI extension (`channel_tui/limits_fmt.clj`) so
   every channel — TUI footer, TUI provider cards — renders the SAME
   compact account-quota summary from a provider's normalized limits
   report. The TUI namespace now aliases these vars; other channels
   consume them through `vis.core`.

   The interesting account-level rows (`:zai-coding-plan-5h`,
   `:codex-7d`, `:premium_interactions`, ...) live under
   `[:dynamic :limits]`; static `:rpm`/`:tpm` are svar catalog
   defaults, identical for every provider, useful only as fallback."
  (:require [clojure.string :as str])
  (:import [java.util Locale]))

(defn format-limit-number
  "Render a numeric usage/limit/remaining value with a single-decimal
   suffix when the value is non-integral, else as a clean integer.
   `Locale/ROOT` keeps the JVM locale from injecting a comma decimal
   separator next to English suffix text."
  [n]
  (when (number? n)
    (let [d (double n)]
      (if (== d (Math/floor d))
        (String/format Locale/ROOT "%.0f" (object-array [d]))
        (String/format Locale/ROOT "%.1f" (object-array [d]))))))

(defn- ->kw
  "Coerce a wire value back to a keyword. The gateway JSON hop stringifies
   keyword VALUES (`:rate` -> \"rate\", `:claude-5h` -> \"claude-5h\") while
   only KEYS are keywordized on parse, so a limits report read back through
   the daemon carries string `:id`/`:kind` where an in-process report carries
   keywords. Normalizing here lets the row formatters treat both shapes
   identically (see `wire/canonical`)."
  [x]
  (cond (keyword? x) x
        (string? x) (keyword x)
        :else x))


(defn generic-limit-label
  "Human label for a dynamic-limit row. Hand-rolled overrides for the
   widely-known plan rows; fallback derives a label from `:label` or
   `:id`, trimming the redundant ` Quota` / ` Quota (%)` suffixes the
   raw provider rows ship with. `:id` is coerced via `->kw` so the
   overrides match whether the report came in-process (keyword ids) or
   across the gateway wire (string ids)."
  [row]
  (case (->kw (:id row))
    :premium_interactions
    "Premium interactions"

    :premium-interactions
    "Premium interactions"

    :codex-5h
    "Codex 5h"

    :codex-7d
    "Codex 7d"

    :zai-coding-plan-5h
    "Z.ai coding plan 5h"

    :zai-coding-plan-7d
    "Z.ai coding plan 7d"

    (let [label (or (:label row)
                    (some-> (:id row)
                            name
                            (str/replace #"[_-]" " ")
                            str/capitalize)
                    "Limit")]
      (-> label
          (str/replace #"(?i)\s+quota\s*\(%\)" "")
          (str/replace #"(?i)\s+quota$" "")))))

(def account-plan-window-ids
  "Known account-level rolling plan windows (Codex / Z.ai 5h + 7d). These are
   surfaced as a PAIR: when a provider omits data for one window, its companion
   row carries no usage signal but must STILL render so both windows stay
   visible (the whole point of the provider keeping a placeholder row)."
  #{:codex-5h :codex-7d :zai-coding-plan-5h :zai-coding-plan-7d})

(defn account-plan-window-row?
  "True when the row is one of the known account plan windows (Codex / Z.ai
   5h + 7d), REGARDLESS of whether it currently carries usage signal. `:id` is
   coerced via `->kw` so a report that crossed the gateway wire (string ids)
   matches the same as an in-process one (keyword ids)."
  [row]
  (contains? account-plan-window-ids (->kw (:id row))))

(defn percentage-limit-row?
  "True when the row is best displayed as a percent-remaining (the
   provider reports a 0-100 percentage rather than raw token counts).
   The ID allowlist covers the Codex / Z.ai plan windows; the
   `:rate` + `:limit 100` heuristic catches generic percentage rows
   (the Anthropic Claude windows). `id`/`kind` are coerced via `->kw`
   so a report that crossed the gateway wire (string values) matches
   the same as an in-process one (keyword values)."
  [{:keys [id kind limit remaining]}]
  (let [id
        (->kw id)

        kind
        (->kw kind)]

    (and (number? remaining)
         (or (contains? account-plan-window-ids id)
             (and (= :rate kind) (number? limit) (== 100.0 (double limit)))))))

(defn format-limit-usage
  "Render the usage/remaining portion of a row as a short string,
   choosing the most informative shape the row's numbers allow:

     - explicit `unlimited?` flag         -> \"unlimited\"
     - percentage-style row               -> \"47% left\"
     - used + limit + remaining           -> \"3/5 used (2 left)\"
     - used + limit                       -> \"3/5 used\"
     - remaining + limit                  -> \"2/5 left\"
     - remaining only                     -> \"2 left\"
     - used only                          -> \"3 used\"
     - none of the above                  -> nil

   Returns nil only when the row carries no usage signal at all, so
   callers can `(when usage ...)` to skip empty cells."
  [{:keys [used limit remaining unlimited?] :as row}]
  (cond unlimited? "unlimited"
        (percentage-limit-row? row) (str (long (Math/round (double remaining))) "% left")
        (and (number? used) (number? limit) (number? remaining)) (str (format-limit-number used)
                                                                      "/"
                                                                      (format-limit-number limit)
                                                                      " used ("
                                                                      (format-limit-number
                                                                        remaining)
                                                                      " left)")
        (and (number? used) (number? limit))
        (str (format-limit-number used) "/" (format-limit-number limit) " used")
        (and (number? remaining) (number? limit))
        (str (format-limit-number remaining) "/" (format-limit-number limit) " left")
        (number? remaining) (str (format-limit-number remaining) " left")
        (number? used) (str (format-limit-number used) " used")
        :else nil))

(defn generic-limit-has-signal?
  "True when the row has usage or reset signal worth surfacing. Used to
   prefer informative rows when the visible area is tight. A reset timestamp
   is signal even when the provider reports zero remaining and omits a limit:
   that's exactly when the user needs to know when credits come back."
  [row]
  (or (:unlimited? row)
      (some? (get-in row [:window :resets-at-ms]))
      (pos? (double (or (:limit row) (:remaining row) (:used row) 0)))))

(defn label+usage
  "Compose `\"<label> <usage>\"` for a single row, or `\"<label>\"`
   when the row has no usage signal. Returns nil when both are
   blank/absent."
  [row]
  (let [label
        (generic-limit-label row)

        usage
        (format-limit-usage row)]

    (cond (and (seq label) usage) (str label " " usage)
          (seq label) label
          :else nil)))

(defn dynamic-summary
  "Compact one-line summary of the most informative `:dynamic :limits`
   rows for a provider's normalized limits report.

   Picks rows with signal first, falls back to all rows when nothing
   has signal yet (so a fresh, all-zero report still surfaces SOMETHING
   rather than collapsing to empty). Takes up to `max-rows` (default 2)
   and joins them with ` · `.

   Returns nil when there's nothing to render."
  ([limits] (dynamic-summary limits 2))
  ([limits max-rows]
   (let [rows
         (get-in limits [:dynamic :limits])

         pick
         (or (seq (filter #(or (generic-limit-has-signal? %) (account-plan-window-row? %)) rows))
             (seq rows))

         lines
         (->> pick
              (keep label+usage)
              (take max-rows))]

     (when (seq lines) (str/join " · " lines)))))
