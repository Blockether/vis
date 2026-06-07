(ns com.blockether.vis.internal.format
  "Format helpers - leaf module.

   Small, dependency-light formatters used by the SDK facade, the
   TUI footer, the CLI status output, and the Telegram bot. Each one
   is a pure transform over basic Clojure / Java values.

     `format-date`      - `java.util.Date` to `dd-MM-yyyy HH:mm` (local TZ)
     `format-clojure`   - pass-through (code is shown as written, not reformatted)
     `format-duration`  - millisecond duration to `2.3s`, `1m 15s`, etc.
     `format-tokens`    - `:input`/`:output` token counts to 'tok 11461→35'
     `format-cost`      - dollar cost to '~$0.006954'
     `format-iterations`- iteration count to '1 iter' / '3 iters'
     `format-meta-line` - canonical ' / '-joined turn-summary line
                          (used identically by CLI / TUI / Telegram)

   No external pretty-printer: data renders through `clojure.pprint`
   (built-in) and source code is shown verbatim. The namespace is free of
   state - safe to require from any layer."
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [edamame.core :as edamame])
  (:import
   [java.util Locale]))

(defn format-date
  "Format a `java.util.Date` as `dd-MM-yyyy HH:mm` in local timezone."
  [^java.util.Date d]
  (when d
    (.format (doto (java.text.SimpleDateFormat. "dd-MM-yyyy HH:mm")
               (.setTimeZone (java.util.TimeZone/getDefault)))
      d)))

(defn safe-zprint-str
  "Pretty-print a runtime DATA value to a string via `clojure.pprint`.
   Kept under the historical name so existing data-render call sites
   (ctx trailer, bounded-value previews) need no change. The optional
   trailing args (width / opts) accepted by the old zprint wrapper are
   ignored; pprint reads `*print-length*`/`*print-level*` from the
   dynamic bindings the caller sets."
  [v & _ignored]
  (str/trimr (with-out-str (pprint/pprint v))))

(defn safe-zprint-file-str
  "Source-formatting seam. Vis no longer reformats code — source is shown
   as written — so this returns the input verbatim. Retained so callers
   (e.g. the Clojure language extension) keep a stable entry point."
  [source & _ignored]
  (str source))

(defn format-clojure
  "Source is shown as written — no reformatting. Returns `code-str`
   trimmed of trailing whitespace (or unchanged when not a string)."
  [code-str _width]
  (if (string? code-str) (str/trimr code-str) code-str))

(def ^:private DEF_HEADS_TO_STRIP
  "Def-shaped heads whose 2nd-arg docstring slot should be stripped
   when rendering source for human consumption (channels: TUI,
   Telegram, transcript). Mirrors `env/DEF_HEADS_FOR_RESTORE` but
   self-contained so `format` stays a leaf module with zero engine
   deps."
  '#{def defn defn- defmacro defonce defmulti
     clojure.core/def clojure.core/defn clojure.core/defn-
     clojure.core/defmacro clojure.core/defonce clojure.core/defmulti})

(defn- strip-doc-from-form
  "If `form` is `(HEAD NAME doc-string …)` with HEAD in
   `DEF_HEADS_TO_STRIP` and the 3rd element a string, return the form
   with the docstring removed: `(HEAD NAME …)`. Otherwise return
   `form` unchanged.

   Channel-side rendering only — the persisted source keeps the
   docstring so var meta and restore continue to work."
  [form]
  (if (and (seq? form)
        (>= (count form) 4)
        (symbol? (first form))
        (contains? DEF_HEADS_TO_STRIP (first form))
        (symbol? (second form))
        (string? (nth form 2 nil)))
    (concat (list (first form) (second form)) (drop 3 form))
    form))

(defn strip-def-docstrings
  "Parse `source` as one or more Clojure forms and return the source
   with the docstring slot removed from every def-shaped form.
   Forms that are not def-shapes pass through untouched.

   Channel renderers call this before `format-clojure` /
   `format-clojure-ansi` so the human-visible code does not carry the
   docstring noise the model is forced to emit (every var requires a
   docstring — the live-vars line already surfaces the description by
   name, so repeating it inside every code block is pure clutter).

   Parse failures fall through with the original `source` — a
   rendered form should never break because of a doc-stripping
   refinement."
  [source]
  (if-not (string? source)
    source
    (try
      (let [forms    (edamame/parse-string-all source {:all true})
            stripped (map strip-doc-from-form forms)]
        (str/join "\n" (map pr-str stripped)))
      (catch Throwable _ source))))

(defn format-clojure-ansi
  "Source is shown as written — no reformatting or syntax coloring.
   Returns `code-str` trimmed of trailing whitespace."
  [code-str _width]
  (if (string? code-str) (str/trimr code-str) (str code-str)))

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
;; different ways: "ctx-in: N, ctx-out: M" (CLI), "↑N / ↓M" (TUI),
;; "ctx N->M" (Telegram). Inconsistent, and every surface duplicated
;; its own `String/format` for the cost. These helpers are the single
;; source of truth for the canonical surface form:
;;
;;   tokens   ->  "tok 11461→35 (cached 4096)" (input, output, cached input)
;;              or "tok 11461→35" when cached input is zero / unknown
;;   cost     ->  "~$0.006954"             (six decimal places, US locale)
;;   iters    ->  "1 iter" / "3 iters"    (unit auto-pluralized)
;;   line     ->  "<iters> / <tokens> / ~$<cost> / <duration>"
;;
;; Surfaces compose around this output - the TUI prepends the model
;; name; the CLI wraps in `[...]`; Telegram wraps in italics. The
;; INNER content is identical so screenshots / pastes / chat history
;; all read the same way.
;; =============================================================================

(defn format-tokens
  "Render token counts in the canonical compact grouped form:
   'tok <input>→<output> (cached <cached-input>)' when cached input is
   positive, otherwise 'tok <input>→<output>'.

   The arrow reads 'prompt produced completion'. Cached is cached
   input tokens, parenthesized because provider APIs report cache
   hits inside prompt usage. `:cached` is the provider field;
   `:cached-input` / `:input-cached` are accepted aliases so usage
   maps can name the direction explicitly.

   Cache visibility: the `(cached N)` segment renders only when N is
   positive. Zero / missing cache info stays hidden so meta lines do
   not show noisy `(cached 0)` decorations.

   Returns nil when no known field carries a number."
  [{:keys [input output] :as tokens}]
  (letfn [(first-number [ks]
            (some (fn [k]
                    (let [v (get tokens k)]
                      (when (number? v) v)))
              ks))]
    (let [cached-input (or (first-number [:cached-input :input-cached :cached]) 0)
          cache-created (or (first-number [:cache-created :cache-created-input :cache-creation :cache-write]) 0)
          in-n  (when (number? input) input)
          out-n (when (number? output) output)]
      (when (or in-n out-n (pos? cached-input) (pos? cache-created))
        (let [head (str "tok " (or in-n 0) "→" (or out-n 0))
              parts (cond-> []
                      (pos? cached-input) (conj (str "cached " cached-input))
                      (pos? cache-created) (conj (str "cache-write " cache-created)))]
          (if (seq parts)
            (str head " (" (str/join ", " parts) ")")
            head))))))

(defn format-cost
  "Render a dollar cost as '~$0.006954' (six decimal places, US
   locale). Returns nil when `cost` is nil, zero, negative, or
   non-numeric. Accepts either the bare number or a `:total-cost`
   map. Detailed cost maps render the total first and the breakdown
   parenthesized, in order: in, cached, write, out — e.g.
   '~$0.006954 (in ~$0.001200, cached ~$0.000400, out ~$0.005354)'.
   The parenthesized breakdown renders only when at least two of
   those slots carry a positive value; otherwise just the total."
  [cost]
  (letfn [(cost-number [k]
            (let [v (get cost k)]
              (when (number? v) v)))
          (positive-cost-number [k]
            (let [v (cost-number k)]
              (when (and v (pos? v)) v)))
          (format-cost-number [n]
            (String/format Locale/US "~$%.6f" (into-array Object [(double n)])))
          (detail [label k]
            (when-let [v (positive-cost-number k)]
              (str label " " (format-cost-number v))))]
    (let [n (cond
              (number? cost) cost
              (and (map? cost)
                (number? (:total-cost cost))) (:total-cost cost)
              :else nil)]
      (when (and n (pos? n))
        (if (map? cost)
          (let [details (cond-> []
                          (positive-cost-number :input-uncached-cost)
                          (conj (detail "in" :input-uncached-cost))

                          (positive-cost-number :input-cached-cost)
                          (conj (detail "cached" :input-cached-cost))

                          (positive-cost-number :input-cache-write-cost)
                          (conj (detail "write" :input-cache-write-cost))

                          (positive-cost-number :output-cost)
                          (conj (detail "out" :output-cost)))
                total   (format-cost-number n)]
            (if (> (count details) 1)
              (str total " (" (str/join ", " details) ")")
              total))
          (format-cost-number n))))))

(defn format-iterations
  "Render an iteration count as '1 iter' or '3 iters'. Returns nil
   when `n` is nil or non-numeric. Optional `:silent-count` appends
   hidden/silent bookkeeping count, e.g. '3 iters (2 silent)'."
  ([n] (format-iterations n nil))
  ([n {:keys [silent-count]}]
   (when (number? n)
     (str n (if (= 1 n) " iter" " iters")
       (when (and (number? silent-count) (pos? silent-count))
         (str " (" silent-count " silent)"))))))

(defn- normalize-provider
  "Coerce a provider id to a short string. Accepts keyword (`:openai`),
   string (`\"openai\"`), or nil. Strips a leading colon so a stringified
   keyword still renders as `openai`, not `:openai`. Returns nil for
   blank or non-string values - the caller treats nil as 'no provider
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
   `provider/model` when both are present - e.g. `openai/gpt-4o`,
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
  "Compose the canonical ' / '-joined turn-summary line shared by
   the CLI bracket, the TUI per-message footer, and the Telegram
   reply tagline. Skips slots whose value is nil so a partial result
   (no cost yet, no duration on a cancelled turn) renders cleanly.

   Slot order:
     <model> / <iterations> / <tokens> / <cost> / <duration>

   The model slot renders `provider/model` when both are known
   (e.g. `openai/gpt-4o`, `blockether/glm-5.1`); falls back to bare
   `model` when only the model name is on the result. Both fields
   are auto-extracted from `:provider` / `:model` (top-level) or
   `:cost :provider` / `:cost :model` (where the iteration runtime
   persists them). Pass `:model` in `opts` to override the rendered
   slot directly; pass `:model false` to suppress.

   `result` is the iteration runtime's result map: `{:iteration-count
   :duration-ms :tokens {:input :output :cached ...} :cost {:total-cost
   :provider :model}}`. `opts` accepts
   `{:model <string|false> :prefix [...] :suffix [...]}` -
   `:prefix`/`:suffix` are lists of arbitrary extra slots prepended /
   appended to the line (rare; mostly for channels with non-result
   chrome)."
  ([result] (format-meta-line result nil))
  ([{:keys [iteration-count duration-ms tokens cost silent-count] :as result}
    {:keys [model prefix suffix]}]
   (let [model* (cond
                  (false? model) nil
                  (string? model) model
                  :else (extract-model result))
         parts  (concat
                  (vec prefix)
                  [model*
                   (format-iterations iteration-count {:silent-count silent-count})
                   (format-tokens tokens)
                   (format-cost cost)
                   (format-duration duration-ms)]
                  (vec suffix))]
     (str/join " / " (remove nil? parts)))))

;; =============================================================================
;; Bounded value rendering
;;
;; UI-level helper: stringify a plain Clojure value with size + nesting
;; caps so non-tool progress chunks, history-restore previews, and trailer
;; `;; => …` lines never dump multi-megabyte payloads into a render buffer.
;; Tool results must use symbol-specific renderers in `internal.extension`.
;; Renders *runtime values* via `clojure.pprint` for data shapes and
;; `pr-str` for scalar fallback.
;; =============================================================================

(def ^:const MAX_RESULT_DISPLAY_CHARS
  "Default char cap on bounded plain-value output when no `:max-chars`
   override is passed. TUI progress chunks and history-restore previews
   use this. Tape rendering passes its own `JOURNAL_RESULT_MAX_CHARS` and
   bypasses the default."
  1500)

(defn- strip-sandbox-ns [s]
  (str/replace (str s) #"\bsandbox/|\buser/" ""))

(defn- pprintable-data?
  [v]
  (or (map? v)
    (vector? v)
    (set? v)
    (instance? clojure.lang.IPersistentList v)))

(defn- value-pr-str
  [v bounded-print?]
  (if (and (pprintable-data? v) (not bounded-print?))
    (safe-zprint-str v)
    (pr-str v)))

(defn bounded-value-str
  "Bounded Clojure data rendering for plain working-memory previews
   (TUI progress, history-restore, trailer `;; => …` lines). Caps output
   at `MAX_RESULT_DISPLAY_CHARS` chars by default; callers that want
   tighter or looser bounds pass `:max-chars`. Do not use for tool results;
   tools must render through their symbol-specific renderers."
  ([v] (bounded-value-str v {}))
  ([v {:keys [max-chars print-length print-level] :as opts
       :or {max-chars MAX_RESULT_DISPLAY_CHARS
            print-length 64
            print-level 6}}]
   (try
     (binding [*print-length* print-length
               *print-level*  print-level]
       (let [bounded-print? (or (contains? opts :print-length)
                              (contains? opts :print-level))
             s (strip-sandbox-ns (value-pr-str v bounded-print?))]
         (if (> (count s) max-chars)
           (str (subs s 0 max-chars) " ...<+" (- (count s) max-chars) " chars>")
           s)))
     (catch Throwable t
       (str "<unprintable: " (.getMessage t) ">")))))
