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

    (let
      [label (or (:label row)
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
  (let
    [id
     (->kw id)

     kind
     (->kw kind)]

    (and (number? remaining)
         (or (contains? account-plan-window-ids id)
             (and (= :rate kind) (number? limit) (== 100.0 (double limit)))))))

(defn format-limit-usage
  "Render the usage/remaining portion of a row as a short string,
   choosing the most informative shape the row's numbers allow:

     - explicit `is-unlimited` flag         -> \"unlimited\"
     - percentage-style row               -> \"47% left\"
     - used + limit + remaining           -> \"3/5 used (2 left)\"
     - used + limit                       -> \"3/5 used\"
     - remaining + limit                  -> \"2/5 left\"
     - remaining only                     -> \"2 left\"
     - used only                          -> \"3 used\"
     - none of the above                  -> nil

   Returns nil only when the row carries no usage signal at all, so
   callers can `(when usage ...)` to skip empty cells."
  [{:keys [used limit remaining is-unlimited] :as row}]
  (cond is-unlimited "unlimited"
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
  (or (:is-unlimited row)
      (some? (get-in row [:window :resets-at-ms]))
      (pos? (double (or (:limit row) (:remaining row) (:used row) 0)))))

(defn limit-row-exhausted?
  "True when a metered row has nothing left: not unlimited, a numeric
   `:remaining` at (or below) zero, and enough context to know that the
   zero is a WALL — either a positive `:limit` or positive `:used`.

   A brand new all-zero row (`{:remaining 0 :limit 0}`) is NOT exhausted;
   it simply has not been filled in yet."
  [{:keys [is-unlimited remaining limit used]}]
  (boolean (and (not is-unlimited)
                (number? remaining)
                (not (pos? (double remaining)))
                (or (and (number? limit) (pos? (double limit)))
                    (and (number? used) (pos? (double used)))))))

(defn limit-row-pressure
  "Sort key ranking a row by how much it constrains the user RIGHT NOW:

     0. exhausted metered rows (0 left, requests are being rejected)
     1. metered rows, tightest remaining fraction first
     2. rows with no tank at all (`:is-unlimited`)

   Without this, a provider that reports its unlimited buckets first
   (GitHub Copilot lists `chat`, `completions`, then `premium_interactions`)
   summarises as \"Chat unlimited · Completions unlimited\" while the ONE
   bucket that actually rejects requests sits silently at 0 remaining."
  [{:keys [is-unlimited remaining limit] :as row}]
  (cond (limit-row-exhausted? row) [0 0.0]
        is-unlimited [2 0.0]
        (and (number? remaining) (number? limit) (pos? (double limit)))
        [1 (/ (double remaining) (double limit))]
        :else [1 1.0]))

(def ^:private window-unit-ms
  "Milliseconds per `[:window :unit]` keyword."
  {:second 1000 :minute 60000 :hour 3600000 :day 86400000 :week 604800000 :month 2592000000})

(defn limit-window-ms
  "Duration of a row's rolling window in milliseconds, or nil when the row is
   not a window row. Read from `[:window :unit]` + `[:window :size]` when the
   provider supplied them, else from the id's own `-5h` / `-7d` suffix (the
   only spelling every provider uses), so a report that crossed the gateway
   wire — where a placeholder row may carry nothing but its id — still orders."
  [row]
  (let
    [{:keys [unit size]}
     (:window row)

     unit-ms
     (get window-unit-ms (->kw unit))]

    (or (when (and unit-ms (number? size)) (long (* (long unit-ms) (long size))))
        (when-let
          [[_ n u] (some->> (:id row)
                            name
                            (re-find #"-([0-9]+)([hdwm])$"))]
          (* (long (parse-long n))
             (long (get window-unit-ms
                        (case u
                          "h"
                          :hour

                          "d"
                          :day

                          "w"
                          :week

                          "m"
                          :minute))))))))

(defn limit-window-order
  "Sort key putting the SHORTER rolling window first — 5h before 7d, always,
   whichever one happens to be tighter right now. Non-window rows fall back to
   `limit-row-pressure` and keep sorting behind the plan windows they belong to."
  [row]
  (if-let [ms (limit-window-ms row)]
    [0 ms 0.0]
    (let [[category fraction] (limit-row-pressure row)]
      [1 (double category) (double fraction)])))

(defn prioritize-limit-rows
  "Stable reorder of limit rows: rolling plan windows first, SHORTEST window
   leading (5h before 7d, never the other way round because the weekly bucket
   happens to be tighter today), then everything else by `limit-row-pressure`
   so whatever is blocking leads any truncated rendering."
  [rows]
  (vec (sort-by limit-window-order rows)))

(def ^:private short-label-rewrites
  "Label shortenings applied to every compact cell. A group prints its plan
   name ONCE (`compact-limit-cells`), so what a cell still spells out is only
   what the user could not infer from the provider already on screen."
  [[#"(?i)^premium interactions$" "Premium"] [#"(?i)^z\.ai coding plan " "Z.ai "]])

(defn short-limit-label
  "`generic-limit-label` trimmed for a one-line summary: \"Premium\",
   \"Z.ai 5h\", \"Codex 7d\", \"OpenCode Go 30d\"."
  [row]
  (reduce (fn [label [pattern replacement]]
            (str/replace label pattern replacement))
          (generic-limit-label row)
          short-label-rewrites))

(defn compact-limit-usage
  "`format-limit-usage` with the English words dropped: \"47% left\" -> \"47%\",
   \"3/5 used (2 left)\" -> \"3/5 (2)\", \"2 left\" -> \"2\". The words cost one
   repetition per window, and a plan with three windows spends the whole line
   on them."
  [row]
  (some-> (format-limit-usage row)
          (str/replace #"% left$" "%")
          (str/replace #" used \(([^)]+) left\)$" " ($1)")
          (str/replace #" used$" "")
          (str/replace #" left$" "")))

(defn limit-label-parts
  "Split a row's short label into `{:label :prefix :window}` when it ends in a
   window suffix (`5h`, `7d`, `30d`), else `{:label ...}` alone. `:prefix` is
   the plan name a family of windows shares."
  [row]
  (let
    [label
     (short-limit-label row)

     parts
     (str/split label #"\s+")

     window
     (last parts)]

    (if (and (< 1 (count parts)) (re-matches #"[0-9]+[hdwm]" window))
      {:label label :prefix (str/join " " (butlast parts)) :window window}
      {:label label})))

(defn compact-limit-cells
  "Compact a group of rows into
   `{:prefix <shared plan name or nil> :cells [{:row <row> :text \"5h 100%\"}]}`.

   When every row's label ends in a window suffix and they all carry the SAME
   plan name, that name is hoisted OUT of the cells into `:prefix`, so a
   three-window plan reads \"OpenCode Go 5h 100% · 7d 100% · 30d 99%\" instead
   of spelling \"OpenCode Go\" three times and \"left\" three times. Otherwise
   `:prefix` is nil and each cell keeps its own label.

   Channels render the pieces themselves (the TUI footer joins with ` / ` and
   stamps a reset on one cell) but never re-derive them."
  [rows]
  (let
    [rows
     (vec rows)

     parts
     (mapv limit-label-parts rows)

     prefix
     (when (and (< 1 (count rows)) (every? :prefix parts) (apply = (map :prefix parts)))
       (:prefix (first parts)))]

    {:prefix prefix
     :cells (mapv (fn [row {:keys [label window]}]
                    (let
                      [head
                       (if prefix window label)

                       usage
                       (compact-limit-usage row)]

                      {:row row :text (if usage (str head " " usage) head)}))
                  rows
                  parts)}))

(defn label+usage
  "Compact cell for a SINGLE row — its own short label plus its compact usage
   (\"Codex 5h 47%\"), or the label alone when the row carries no usage signal.
   `generic-limit-label` bottoms out at \"Limit\", so a row always renders."
  [row]
  (:text (first (:cells (compact-limit-cells [row])))))

(defn dynamic-summary
  "Compact one-line summary of the most informative `:dynamic :limits`
   rows for a provider's normalized limits report.

   Picks rows with signal first, falls back to all rows when nothing
   has signal yet (so a fresh, all-zero report still surfaces SOMETHING
   rather than collapsing to empty). Takes up to `max-rows` (default 3 — the
   widest plan family shipped is 5h + 7d + 30d), compacts them through
   `compact-limit-cells` so the plan name is written once, and joins the cells
   with ` · `:

     OpenCode Go 5h 100% · 7d 100% · 30d 99%

   Returns nil when there's nothing to render."
  ([limits] (dynamic-summary limits 3))
  ([limits max-rows]
   (let
     [rows
      (get-in limits [:dynamic :limits])

      pick
      (or (seq (filter #(or (generic-limit-has-signal? %) (account-plan-window-row? %)) rows))
          (seq rows))

      {:keys [prefix cells]}
      (compact-limit-cells (take max-rows (prioritize-limit-rows pick)))

      body
      (str/join " · " (keep :text cells))]

     (when (seq body) (if prefix (str prefix " " body) body)))))
