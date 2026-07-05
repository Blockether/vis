(ns com.blockether.vis.internal.form
  "The canonical per-form DISPLAY contract — ONE source of truth for the fields a
   channel reads to render an executed form, live (via the gateway) and restored
   (via the DB).

   Why this exists: the SAME field set used to be hand-listed in ~7 independent
   allowlists — the loop chunk, the persisted block, the gateway `block.output`
   wire payload, the gateway→chunk client projection, the live progress form, the
   DB-restored form, and the restored display form. A new field was invisible
   until every one was edited, and whichever layer forgot it silently dropped the
   field (the gateway dropping `:tool-color-role` so the live badge vanished was
   exactly that). Now every layer projects the WHOLE set through `->display`
   (outbound) / `<-wire` (inbound, tolerant of the wire's snake_case + stringified
   keyword values), so a new display field is a ONE-line change to `display-keys`
   and `form-roundtrip-test` fails if a boundary stops carrying it.

   Transformed fields (`:stdout`/`:error` bounded, `:silent`/`:duration_ms`
   renamed) stay as explicit gateway overrides — they are not carried verbatim, so
   they are NOT in this set."
  (:require [clojure.string :as str]))

(def display-keys
  "Every field carried VERBATIM from the loop to a channel to render a form — the
   complete passthrough set, NOT the handful the gateway computes/renames itself
   (`:stdout`/`:error` are bounded, `:silent`/`:duration_ms` are derived; those
   stay explicit gateway overrides). Add a new verbatim display field HERE and
   `->display`/`<-wire` flow it across every boundary.

   Grouped: the source the model wrote, the result surfaces (raw + the
   pre-rendered op-card), the native-tool badge identity (label + colour), the
   per-form display projections, the tool-call linkage, and the repair/timeout
   flags channels surface."
  [;; source
   :code :comment :scope :started-at-ms
   ;; result surfaces — the raw value, the pre-rendered op-card body, and the
   ;; op-card HEADLINE (a tool-authored summary like "5 hits in 1 file", never a
   ;; first-line slice of the body)
   :result :result-render :result-summary
   ;; MULTI-card: a python block that print()ed several tool results carries one
   ;; CANONICAL MINI-FORM per result here (each shaped like a single form's display
   ;; fields), so `result-cards`/`result-card` render each and `<-wire` round-trips
   ;; them by recursing over this vector. nil/absent for a single-result form.
   :cards
   ;; native-tool op-card badge identity
   :vis/tool-name :tool-color-role
   ;; display projections
   :render-segments :result-kind :result-detail :tag
   ;; tool-call linkage + status flags channels surface
   :svar/tool-call-id :timeout? :repaired? :auto-repaired?])

(def ^:private label-overrides
  "Native-tool WIRE name → a nicer op-card LABEL. Most tools read fine uppercased
   (RG, CAT, PATCH); a few don't — `python_execution` is the model writing/running
   code, but its card surfaces what that run produced, so it reads `RESULT`."
  {"python_execution" "RESULT"})

(def tool-color-roles
  "The canonical set of native-tool op-card BADGE colour roles — the ONE list both
   channels colour against (TUI maps each to a lanterna fg, the web to a `--tool-*`
   CSS var). Hand-maintained per-channel maps were free to drift; a guard test in
   each channel asserts its map covers every role here, so a new role can't be
   added in one channel and silently forgotten in the other."
  [:tool-color/read :tool-color/search :tool-color/preview :tool-color/edit
   :tool-color/create :tool-color/delete :tool-color/move :tool-color/shell
   :tool-color/meta :tool-color/test])

(defn tool-label
  "The op-card badge LABEL for a native tool's wire name: the name uppercased,
   except the few `label-overrides` rename. ONE place both channels derive it from
   so the TUI badge and the web label never drift. nil for a non-tool form."
  [wire-name]
  (when (some? wire-name)
    (let [n (name wire-name)]
      (or (label-overrides n) (str/upper-case n)))))
(defn result-card
  "Canonical tool-result CARD descriptor — the ONE place the op-card / collapse
   decision is made, so the TUI and web AGREE on `tool?`/label/colour/summary/
   collapsible instead of each re-deriving it from the raw form. Given an executed
   form map, returns the op-card shape for a NATIVE TOOL result:

     {:tool?        true
      :label        \"RG\"                — op-card badge label (`tool-label`)
      :color-role   :tool-color/search   — badge colour role (keyword-normalized,
                                           since a JSON wire hop stringifies it)
      :summary      \"5 hits in 1 file\"  — the HEADLINE (`:result-summary`), nil
                                           when the tool authored none
      :body         \"…markdown…\"        — the detail body (`:result-render`), nil
                                           for a summary-only tool (move/delete)
      :collapsible? true}                — true ⇔ there's a body to fold under
                                           the summary (a chevron/`<details>`)

   `nil` for a NON-tool form (no `:vis/tool-name`) — its result rendering stays
   channel-specific (raw value / stdout). The badge is whatever the tool's
   `:summary` already produced; no first-line-of-body heuristic."
  [{:keys [tool-color-role result-summary result-render], tool-name :vis/tool-name}]
  (when (some? tool-name)
    (let [summary (some-> result-summary str str/trim not-empty)
          body    (some-> result-render str str/trimr not-empty)]
      {:tool?        true
       :label        (tool-label tool-name)
       :color-role   (cond (keyword? tool-color-role) tool-color-role
                       (string? tool-color-role)  (keyword tool-color-role)
                       :else                      nil)
       :summary      summary
       :body         body
       :collapsible? (boolean body)})))

(defn result-cards
  "The op-card descriptor(s) a form renders — the ONE place a channel asks \"what
   cards does this form show?\" so the TUI and web never re-derive it differently.

   A python block that print()ed several tool results carries a `:cards` vector of
   canonical mini-forms; each becomes its OWN op-card via `result-card`. Any other
   form yields its single `result-card` (or none). Always a vector — channels just
   iterate. Empty when the form has no tool card at all (plain value / stdout)."
  [form]
  (if-let [cards (seq (:cards form))]
    (into [] (keep result-card) cards)
    (if-let [c (result-card form)] [c] [])))

(defn native-tool-form?
  "True when `form` is a NATIVE tool call (cat/rg/patch/…): it carries a
   `:vis/tool-name` and therefore renders as an op-card via `result-card`."
  [{tool-name :vis/tool-name}]
  (some? tool-name))

(defn hide-tool-code?
  "Should a channel DROP a form's synthesized invocation source (the `name(args)`
   code block) instead of showing it as code? YES for a SUCCESSFUL native tool
   call (cat/rg/patch/…) that is NOT `python_execution`: the op-card
   (`result-card`) already says what ran, so the source is redundant chrome.
   `python_execution` — the model's OWN program — always keeps its code; an
   ERRORED form keeps it too (the inline error needs the surrounding source).
   The ONE predicate the TUI and web both consult so the code-chrome decision
   can't drift between channels."
  [{:keys [error success?] :as form}]
  (let [errored? (or (some? error)
                   (and (some? success?) (not success?)))]
    (boolean (and (not errored?)
               (native-tool-form? form)
               (not= (name (:vis/tool-name form)) "python_execution")))))

(def ^:private coalescable-tools
  "Native tools whose ADJACENT same-file op-cards fold into ONE card within an
   iteration: repeated reads (`cat`) and repeated edits (`patch`) of the SAME
   path read as a single multi-span / multi-diff card instead of a stack of
   look-alike siblings. Only these two carry a stable file identity and a
   summary whose leading `` `path` `` chip survives a DB round-trip."
  #{"cat" "patch"})

(defn- coalesce-error-form?
  "A form the gateway marked failed (`:success? false`) — never folded into a run
   (a failed read/edit is its own event and keeps its inline error)."
  [form]
  (and (some? (:success? form)) (not (:success? form))))

(defn- form-file-path
  "The file PATH a coalescable op-card (`cat`/`patch`) acted on — from the raw
   `:result` `:path` when present, else parsed out of the first `` `path` `` chip
   of the summary (which survives a DB round-trip that flattens `:result` to a
   string). nil for a non-coalescable / pathless form."
  [form]
  (when (contains? coalescable-tools (some-> (:vis/tool-name form) str))
    (or (get-in form [:result :path])
      (some-> (:result-summary form) str
        (->> (re-find #"`([^`]+)`")) second))))

(defn- merge-summaries
  "Fold N op-card summaries for the SAME path into ONE. Splits each on ` · `,
   keeps the shared leading chip (`` `path` `` for cat, `` update `path` `` for
   patch), then appends every DISTINCT tail — for cat the read's LINE SPANS plus
   the SUMMED line count (only when every merged read carried one). So two reads
   render `` `a.clj` · L1-10 · L40-50 `` and two edits stay `` update `a.clj` ``
   instead of two look-alike cards."
  [summaries]
  (let [parts    (map #(str/split (str %) #" · ") summaries)
        chip     (ffirst parts)
        tails    (mapcat rest parts)
        count-re #"^(\d+) lines?$"
        counts   (keep #(some-> (re-matches count-re %) second parse-long) tails)
        spans    (remove #(re-matches count-re %) tails)
        total    (reduce + 0 counts)
        span-str (str/join " · " (distinct spans))
        count-str (when (and (seq counts) (= (count counts) (count summaries)))
                    (str total " line" (when (not= 1 total) "s")))
        tail     (str/join " · " (remove str/blank? [span-str count-str]))]
    (if (str/blank? tail) chip (str chip " · " tail))))

(defn- merge-run
  "Collapse a RUN of adjacent op-cards on the same file into one synthesized form:
   the combined multi-span/multi-diff summary plus every card's body slice stacked
   under the single headline, so a channel renders it as ONE collapsible card."
  [forms]
  (let [f0      (first forms)
        summary (merge-summaries (map :result-summary forms))
        body    (str/join "\n" (keep (comp not-empty str :result-render) forms))
        anchors (reduce merge {} (map #(get-in % [:result :anchors]) forms))
        r0      (:result f0)]
    (cond-> (assoc f0 :result-summary summary :result-render body)
      ;; Only merge anchors onto a genuine MAP result. After a DB round-trip
      ;; `:result` comes back as the rendered string (anchors flattened) and the
      ;; path/spans already live in the merged summary, so a non-map result
      ;; carries through untouched (assoc'ing onto a string throws).
      (map? r0) (assoc :result (assoc r0 :anchors anchors)))))

(defn coalesce-forms
  "Merge each maximal run of ADJACENT, successful op-cards of the SAME coalescable
   tool reading/editing the SAME path into a single card; every other form passes
   through untouched. The ONE projection both channels apply before rendering an
   iteration's forms, so two `cat`s or two `patch`es of one file never render as
   look-alike siblings. Always returns a vector."
  [forms]
  (let [key-fn (fn [f]
                 (if-let [p (and (not (coalesce-error-form? f)) (form-file-path f))]
                   [::run (some-> (:vis/tool-name f) str) p]
                   [::solo (gensym)]))]
    (into []
      (map (fn [grp] (if (next grp) (merge-run grp) (first grp))))
      (partition-by key-fn (vec forms)))))

(def ^:private keyword-valued
  "Display fields whose VALUE is a keyword (`:tool-color/search`), which a JSON
   wire stringifies — `<-wire` coerces them back so a channel's keyword dispatch
   doesn't miss."
  #{:tool-color-role})

(defn ->display
  "Project the canonical display fields off a source map (loop chunk/block, a
   restored row) — the ONE projection every form builder + the gateway uses
   instead of hand-listing keys. Drops nils so a merge never stamps empty keys."
  [m]
  (reduce (fn [acc k] (if (some? (get m k)) (assoc acc k (get m k)) acc))
    {} display-keys))

(defn- spellings
  "Every keyword + string spelling of a base name, snake_case and kebab-case."
  [base]
  (let [snake (str/replace base "-" "_")
        kebab (str/replace base "_" "-")]
    [(keyword base) (keyword snake) (keyword kebab) base snake kebab]))

(defn- wire-key-variants
  "The on-the-wire spellings a display key may arrive as after serialization.
   CRUCIAL for namespaced keys: `(name :vis/tool-name)` drops the namespace, so a
   wire key like `\"vis/tool_name\"` would be missed — include the FULL `ns/name`
   spelling alongside the bare name so the round-trip survives however the wire
   encodes it."
  [k]
  (let [n  (name k)
        ns (namespace k)]
    (distinct (concat [k]
                (when ns (spellings (str ns "/" n)))
                (spellings n)))))

(defn- wire-get
  "Read one display key off a wire event, tolerant of snake_case / namespaced /
   string keys."
  [event k]
  (some #(let [v (get event %)] (when (some? v) v)) (wire-key-variants k)))

(defn <-wire
  "Read the canonical display fields back off a gateway WIRE event into a form,
   tolerant of snake_case keys and re-keywording the keyword-valued fields. The
   single inbound projection the channels use — the mirror of `->display`."
  [event]
  (reduce (fn [acc k]
            (let [v (wire-get event k)]
              (cond
                (nil? v)               acc
                (keyword-valued k)     (assoc acc k (keyword v))
                ;; `:cards` is a vector of canonical MINI-FORMS — each crossed the
                ;; JSON wire with snake_case keys + a stringified `:tool-color-role`,
                ;; exactly what `<-wire` itself normalizes. Recurse so the nested
                ;; colour keyword survives the hop the same way the top-level one does.
                (= k :cards)           (assoc acc k (mapv <-wire v))
                :else                  (assoc acc k v))))
    {} display-keys))
