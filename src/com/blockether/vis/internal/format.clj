(ns com.blockether.vis.internal.format
  "Format helpers — leaf module.

   Small, dependency-light formatters used by the SDK facade, the
   TUI footer, the CLI status output, and the Telegram bot. Each one
   is a pure transform over basic Clojure / Java values.

     `format-date`      — `java.util.Date` to `dd-MM-yyyy HH:mm` (local TZ)
     `format-clojure`   — pretty-print a Clojure source string with zprint
     `format-duration`  — millisecond duration to `2.3s`, `1m 15s`, etc.
     `format-tokens`    — `:input`/`:output` token counts to '↑11461 ↓35'
     `format-cost`      — dollar cost to '~$0.006954'
     `format-iterations`— iteration count to '1 iter' / '3 iters'
     `format-meta-line` — canonical ' · '-joined turn-summary line
                          (used identically by CLI / TUI / Telegram)

   zprint is a hard dep; `format-clojure` calls it directly. The
   namespace is otherwise free of state — safe to require from any
   layer."
  (:require
   [clojure.string :as str]
   [zprint.core :as zprint])
  (:import
   [java.util Locale]))

(defn format-date
  "Format a `java.util.Date` as `dd-MM-yyyy HH:mm` in local timezone."
  [^java.util.Date d]
  (when d
    (.format (doto (java.text.SimpleDateFormat. "dd-MM-yyyy HH:mm")
               (.setTimeZone (java.util.TimeZone/getDefault)))
      d)))

(defn format-clojure
  "Pretty-print a Clojure source string with zprint. Falls back to
   the original string on any error."
  [code-str width]
  (try
    (let [formatted (zprint/zprint-str code-str width
                      {:parse-string? true :style :community})]
      (if (str/blank? formatted) code-str (str/trimr formatted)))
    (catch Exception _ code-str)))

(defn format-duration
  "Human-readable millisecond duration. e.g. `2.3s`, `1m 15s`. Always
   uses Locale/US so the decimal separator is a dot regardless of
   the JVM default locale. Coerces the input to long up-front because
   callers routinely pass a double from `(/ ns 1e6)`."
  [ms]
  (when (and ms (pos? ms))
    (let [ms (long ms)]
      (cond
        (< ms 1000)  (str ms "ms")
        (< ms 60000) (String/format Locale/US "%.1fs"
                       (into-array Object [(double (/ ms 1000.0))]))
        :else        (let [m (quot ms 60000)
                           s (quot (mod ms 60000) 1000)]
                       (str m "m " s "s"))))))

;; =============================================================================
;; Turn-summary helpers (CLI / TUI / Telegram all share these)
;;
;; Three different surfaces used to format the same data three
;; different ways: "ctx-in: N, ctx-out: M" (CLI), "↑N · ↓M" (TUI),
;; "ctx N→M" (Telegram). Inconsistent, and every surface duplicated
;; its own `String/format` for the cost. These helpers are the single
;; source of truth for the canonical surface form:
;;
;;   tokens   →  "↑11461 ↓35"            (input arrows up, output down)
;;   cost     →  "~$0.006954"            (six decimal places, US locale)
;;   iters    →  "1 iter" / "3 iters"    (unit auto-pluralized)
;;   line     →  "<iters> · <tokens> · ~$<cost> · <duration>"
;;
;; Surfaces compose around this output — the TUI prepends the model
;; name; the CLI wraps in `[…]`; Telegram wraps in italics. The
;; INNER content is identical so screenshots / pastes / chat history
;; all read the same way.
;; =============================================================================

(defn format-tokens
  "Render token counts in the canonical compact form: '↑<input> ↓<output>'.
   Up arrow = tokens fed INTO the model (prompt); down arrow = tokens
   the model produced. Returns nil when neither field carries a number."
  [{:keys [input output]}]
  (let [parts (cond-> []
                (number? input)  (conj (str "↑" input))
                (number? output) (conj (str "↓" output)))]
    (when (seq parts) (str/join " " parts))))

(defn format-cost
  "Render a dollar cost as '~$0.006954' (six decimal places, US
   locale). Returns nil when `cost` is nil, zero, negative, or
   non-numeric. Accepts either the bare number or a `:total-cost`
   map; the map form lets call sites pass `:cost` straight through."
  [cost]
  (let [n (cond
            (number? cost)              cost
            (and (map? cost)
              (number? (:total-cost cost))) (:total-cost cost)
            :else                        nil)]
    (when (and n (pos? n))
      (String/format Locale/US "~$%.6f" (into-array Object [(double n)])))))

(defn format-iterations
  "Render an iteration count as '1 iter' or '3 iters'. Returns nil
   when `n` is nil or non-numeric."
  [n]
  (when (number? n)
    (str n (if (= 1 n) " iter" " iters"))))

(defn- normalize-provider
  "Coerce a provider id to a short string. Accepts keyword (`:openai`),
   string (`\"openai\"`), or nil. Strips a leading colon so a stringified
   keyword still renders as `openai`, not `:openai`. Returns nil for
   blank or non-string values — the caller treats nil as 'no provider
   prefix' and renders the bare model."
  [p]
  (cond
    (keyword? p)        (name p)
    (and (string? p)
      (str/starts-with? p ":")) (subs p 1)
    (and (string? p)
      (not (str/blank? p)))     p
    :else nil))

(defn- extract-model
  "Pull the model identity off a result map and render it as
   `provider/model` when both are present — e.g. `openai/gpt-4o`,
   `blockether/glm-5.1`. Falls back to bare `model` when only the
   model is known (older persisted rows, mid-flight chunks). The
   iteration runtime stores both fields on `:cost`
   (`(:cost result) => {:total-cost N :provider :openai :model
   \"gpt-4o\"}`); channels sometimes lift `:model` / `:provider` to
   top-level on the result map. Returns nil when no model is known."
  [result]
  (let [model    (or (when-let [m (:model result)]       (when (string? m) m))
                   (when-let [m (:model (:cost result))] (when (string? m) m)))
        provider (or (normalize-provider (:provider result))
                   (normalize-provider (:provider (:cost result))))]
    (when model
      (if provider (str provider "/" model) model))))

(defn format-meta-line
  "Compose the canonical ' · '-joined turn-summary line shared by
   the CLI bracket, the TUI per-message footer, and the Telegram
   reply tagline. Skips slots whose value is nil so a partial result
   (no cost yet, no duration on a cancelled turn) renders cleanly.

   Slot order:
     <model> · <iterations> · <tokens> · <cost> · <duration>

   The model slot renders `provider/model` when both are known
   (e.g. `openai/gpt-4o`, `blockether/glm-5.1`); falls back to bare
   `model` when only the model name is on the result. Both fields
   are auto-extracted from `:provider` / `:model` (top-level) or
   `:cost :provider` / `:cost :model` (where the iteration runtime
   persists them). Pass `:model` in `opts` to override the rendered
   slot directly; pass `:model false` to suppress.

   `result` is the iteration runtime's result map: `{:iteration-count
   :duration-ms :tokens {:input :output} :cost {:total-cost :provider
   :model}}`. `opts` accepts
   `{:model <string|false> :prefix [...] :suffix [...]}` —
   `:prefix`/`:suffix` are lists of arbitrary extra slots prepended /
   appended to the line (rare; mostly for channels with non-result
   chrome)."
  ([result] (format-meta-line result nil))
  ([{:keys [iteration-count duration-ms tokens cost] :as result}
    {:keys [model prefix suffix]}]
   (let [model* (cond
                  (false? model) nil
                  (string? model) model
                  :else (extract-model result))
         parts  (concat
                  (vec prefix)
                  [model*
                   (format-iterations iteration-count)
                   (format-tokens tokens)
                   (format-cost cost)
                   (format-duration duration-ms)]
                  (vec suffix))]
     (str/join " · " (remove nil? parts)))))
