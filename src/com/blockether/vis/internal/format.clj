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
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str])
  (:import [java.util Locale]))

(def ^:private turn-key-re #"turn_(\d+)")

(defn humanize-fact-key
  "Human-facing label for a fact/entity key. A `turn_<N>` key reads as
   `Turn <N>` for DISPLAY. Every other key is shown with
   underscores/hyphens normalized to SPACES and the first letter capitalized
   (`api_key` -> `Api key`, `clj_eval_render` -> `Clj eval render`).
   DISPLAY ONLY — the stored key stays verbatim, so restore still
   matches. Canonical across the context panel and every channel (TUI, web).

   Fact/entity keys are model-authored strings (strings-only boundary),
   so `(str k)` is total here — no keyword branch."
  [k]
  (let [s (str k)]
    (cond (str/blank? s) s
          :else (if-let [[_ n] (re-matches turn-key-re s)]
                  (str "Turn " n)
                  (let [s (str/replace s #"[-_]+" " ")]
                    (str (str/upper-case (subs s 0 1)) (subs s 1)))))))

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
  (when (and ms (pos? (double ms)))
    (let [ms (long ms)]
      (cond (< ms 1000) (str ms "ms")
            (< ms 60000)
            (String/format Locale/US "%.1fs" (into-array Object [(double (/ ms 1000.0))]))
            :else (let [m (quot ms 60000)
                        s (quot (long (mod ms 60000)) 1000)]

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
    (let [cached-input
          (or (first-number [:cached-input :input-cached :cached]) 0)

          cache-created
          (or (first-number [:cache-created :cache-created-input :cache-creation :cache-write]) 0)

          in-n
          (when (number? input) input)

          out-n
          (when (number? output) output)]

      (when (or in-n out-n (pos? (long cached-input)) (pos? (long cache-created)))
        (let [head
              (str "tok " (or in-n 0) "→" (or out-n 0))

              parts
              (cond-> []
                (pos? (long cached-input))
                (conj (str "cached " cached-input))

                (pos? (long cache-created))
                (conj (str "cache-write " cache-created)))]

          (if (seq parts) (str head " (" (str/join ", " parts) ")") head))))))

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
              (when (and v (pos? (double v))) v)))
          (format-cost-number [n]
            (String/format Locale/US "~$%.6f" (into-array Object [(double n)])))
          (detail [label k]
            (when-let [v (positive-cost-number k)]
              (str label " " (format-cost-number v))))]
    (let [n (cond (number? cost) cost
                  (and (map? cost) (number? (:total-cost cost))) (:total-cost cost)
                  :else nil)]
      (when (and n (pos? (double n)))
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
                total (format-cost-number n)]

            (if (> (count details) 1) (str total " (" (str/join ", " details) ")") total))
          (format-cost-number n))))))

(defn format-iterations
  "Render an iteration count as '1 iter' or '3 iters'. Returns nil
   when `n` is nil or non-numeric. Optional `:silent-count` appends
   hidden/silent bookkeeping count, e.g. '3 iters (2 silent)'."
  ([n] (format-iterations n nil))
  ([n {:keys [silent-count]}]
   (when (number? n)
     (str n
          (if (= 1 n) " iter" " iters")
          (when (and (number? silent-count) (pos? (long silent-count)))
            (str " (" silent-count " silent)"))))))

(defn- normalize-provider
  "Coerce a provider id to a short string. Accepts keyword (`:openai`),
   string (`\"openai\"`), or nil. Strips a leading colon so a stringified
   keyword still renders as `openai`, not `:openai`. Returns nil for
   blank or non-string values - the caller treats nil as 'no provider
   prefix' and renders the bare model."
  [p]
  (cond (keyword? p) (name p)
        (and (string? p) (str/starts-with? p ":")) (subs p 1)
        (and (string? p) (not (str/blank? p))) p
        :else nil))

(defn display-model-name
  "DISPLAY-ONLY normalization of a model id: path-style ids
   (`google/gemma-4-12b-qat`, `org/model` as LM Studio / HF name them)
   render with the slashes flattened to dashes (`google-gemma-4-12b-qat`),
   so a `provider/model` label never reads as three ambiguous segments.
   The wire/config id keeps its slashes — never feed this back to a
   router or provider. nil-safe; non-strings and blanks return nil."
  [m]
  (when (and (string? m) (not (str/blank? m))) (str/replace m "/" "-")))

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
  (let [model
        (display-model-name (or (when-let [m (:model result)]
                                  (when (string? m) m))
                                (when-let [m (:model (:cost result))]
                                  (when (string? m) m))))

        provider
        (or (normalize-provider (:provider result))
            (normalize-provider (:provider (:cost result))))]

    (when model (if provider (str provider "/" model) model))))

;; ── Humanized turn-summary line (shared verbatim: CLI / TUI / Telegram) ──────
;;
;; `format-tokens`/`format-cost` above stay PRECISE — other surfaces (the trace
;; run-report, transcript export) want exact counts. The turn-summary FOOTER, by
;; contrast, is a glance: humanized counts, total cost, zero slots suppressed,
;; and the routing fallback told as a short note. These helpers are the single
;; source so every channel reads identically.

(defn- model-pair-label
  "`provider/model` when both are known, bare model / bare provider when only one
   is, nil when neither. Keywords render without the leading colon."
  [{:keys [provider model]}]
  (let [p
        (normalize-provider provider)

        m
        (display-model-name model)]

    (cond (and p m) (str p "/" m)
          m m
          p p
          :else nil)))

(defn- meta-model-label
  "Label for the model that actually ANSWERED: prefer the routing `:llm-actual`,
   then the `:provider`/`:model`/`:cost` pair `extract-model` already knows."
  [result]
  (or (model-pair-label (:llm-actual result)) (extract-model result)))

(defn- humanize-count
  "Compact human count: 35 → \"35\", 11461 → \"11.5k\", 4096 → \"4.1k\",
   2000000 → \"2M\". One decimal, trailing \".0\" dropped (1000 → \"1k\").
   Integer math so the decimal mark never flips on a non-US JVM locale."
  [n]
  (when (number? n)
    (let [n
          (long n)

          tenths
          (fn [scale]
            (Math/round (/ (double n) (/ (double scale) 10.0))))

          render
          (fn [^long t unit]
            (let [whole
                  (quot t 10)

                  frac
                  (rem t 10)]

              (str whole (when (pos? frac) (str "." frac)) unit)))]

      (cond (< n 1000) (str n)
            (< n 1000000) (render (tenths 1000) "k")
            :else (render (tenths 1000000) "M")))))

(defn meta-tokens
  "Humanized token slot — \"11.5k→35\", with \" (cached 4.1k)\" only when the
   cached-input count is positive. Returns nil for a ZERO-usage turn (no input
   AND no output) so a failed / empty provider call never renders a bare
   \"0→0\"."
  [tokens]
  (letfn [(num [ks]
            (some (fn [k]
                    (let [v (get tokens k)]
                      (when (number? v) v)))
                  ks))]
    (let [in
          (num [:input])

          out
          (num [:output])

          cached
          (num [:cached-input :input-cached :cached])]

      (when (or (and in (pos? (long in))) (and out (pos? (long out))))
        (str (humanize-count (or in 0))
             "→"
             (humanize-count (or out 0))
             (when (and cached (pos? (long cached)))
               (str " (cached " (humanize-count cached) ")")))))))

(defn meta-cost
  "Humanized dollar cost — \"~$0.0070\" / \"~$1.23\". nil for zero / missing.
   Extra decimals for sub-cent turns so they don't round down to \"$0\"."
  [cost]
  (let [n (cond (number? cost) cost
                (and (map? cost) (number? (:total-cost cost))) (:total-cost cost)
                :else nil)]
    (when (and n (pos? (double n)))
      (str "~$"
           (String/format Locale/US
                          (cond (>= (double n) 1) "%.2f"
                                (>= (double n) 0.0001) "%.4f"
                                :else "%.6f")
                          (into-array Object [(double n)]))))))

(def meta-separator
  "Calm separator for the shared turn-summary line — a middot ringed by spaces.
   Identical across CLI, TUI, and Telegram so every surface reads the same."
  "  ·  ")

(defn meta-fallback-note
  "Faint routing note, present only when the turn fell back to another model:
     ↳ from <selected-model> — <reason>, retried N×
   `reason` prefers the HTTP status (429) on the fallback event, then the reason
   keyword, then the free-form error. Retries count `:llm.routing/provider-retry`
   events in the trace. Returns nil when there was no fallback. Shared so the TUI
   can float it on its own faint row while CLI/Telegram fold it inline."
  [{:keys [llm-selected llm-fallback? llm-routing-trace]}]
  (when llm-fallback?
    (let [from
          (or (model-pair-label llm-selected) "previous model")

          ev
          (first (filter #(contains? #{:llm.routing/provider-fallback :llm.routing/format-fallback}
                                     (:event/type %))
                         llm-routing-trace))

          retries
          (count (filter #(= :llm.routing/provider-retry (:event/type %)) llm-routing-trace))

          status
          (:status ev)

          why
          (cond (some? status) (str status)
                (some? (:reason ev)) (name (:reason ev))
                (seq (str (:error ev))) (str (:error ev))
                :else nil)

          tail
          (->> [why (when (pos? retries) (str "retried " retries "×"))]
               (remove (fn [s]
                         (or (nil? s) (str/blank? (str s))))))]

      (str "↳ from " from (when (seq tail) (str " — " (str/join ", " tail)))))))

(defn meta-summary-line
  "The canonical, humanized turn-summary MAIN line, shared verbatim by the CLI
   bracket, the TUI bubble footer, and the Telegram tagline:

     <provider/model>  ·  <in→out (cached)>  ·  ~$cost  ·  <duration>

   Zero-usage and zero-cost slots are dropped (no \"0→0\", no \"$0\"), so a turn
   that produced nothing reads as just the model + time. Does NOT include the
   fallback note — that is `meta-fallback-note`, which single-line surfaces fold
   in via `format-meta-line` and the TUI floats on a second row.

   `opts` keeps the legacy override hooks: `{:model <string|false> :prefix [...]
   :suffix [...]}` — `:model false` suppresses the model slot, a string overrides
   it; prefix/suffix are extra slots spliced in around the standard ones."
  ([result] (meta-summary-line result nil))
  ([{:keys [tokens cost duration-ms] :as result} {:keys [model prefix suffix]}]
   (let [model*
         (cond (false? model) nil
               (string? model) model
               :else (meta-model-label result))

         parts
         (->> (concat (vec prefix)
                      [model* (meta-tokens tokens) (meta-cost cost) (format-duration duration-ms)]
                      (vec suffix))
              (remove nil?))]

     (when (seq parts) (str/join meta-separator parts)))))

(defn format-meta-line
  "Single-line turn summary for plain-text surfaces (the CLI `[...]` bracket and
   the Telegram tagline): the shared `meta-summary-line` with the fallback note
   folded inline. The TUI instead uses `meta-summary-line` + `meta-fallback-note`
   directly so it can float the note on its own faint row — same words, same
   numbers, just two rows. Returns \"\" when there's nothing to show."
  ([result] (format-meta-line result nil))
  ([result opts]
   (let [main
         (meta-summary-line result opts)

         note
         (meta-fallback-note result)]

     (cond (and main note) (str main meta-separator note)
           main main
           note note
           :else ""))))

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

(defn- strip-sandbox-ns [s] (str/replace (str s) #"\bsandbox/|\buser/" ""))

(defn- pprintable-data?
  [v]
  (or (map? v) (vector? v) (set? v) (instance? clojure.lang.IPersistentList v)))

(defn- value-pr-str
  [v bounded-print?]
  (if (and (pprintable-data? v) (not bounded-print?)) (safe-zprint-str v) (pr-str v)))

(defn bounded-value-str
  "Bounded Clojure data rendering for plain working-memory previews
   (TUI progress, history-restore, trailer `;; => …` lines). Caps output
   at `MAX_RESULT_DISPLAY_CHARS` chars by default; callers that want
   tighter or looser bounds pass `:max-chars`. Do not use for tool results;
   tools must render through their symbol-specific renderers."
  ([v] (bounded-value-str v {}))
  ([v
    {:keys [max-chars print-length print-level]
     :as opts
     :or {max-chars MAX_RESULT_DISPLAY_CHARS print-length 64 print-level 6}}]
   (try (binding [*print-length*
                  print-length

                  *print-level*
                  print-level]

          (let [bounded-print?
                (or (contains? opts :print-length) (contains? opts :print-level))

                s
                (strip-sandbox-ns (value-pr-str v bounded-print?))]

            (if (> (count s) (long max-chars))
              (str (subs s 0 max-chars) " ...<+" (- (count s) (long max-chars)) " chars>")
              s)))
        (catch Throwable t (str "<unprintable: " (.getMessage t) ">")))))
