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

(def ^:private display-fields
  "Every field carried VERBATIM from the loop to a channel to render a form,
   paired with its literal gateway wire key. This is the complete passthrough set,
   NOT the handful the gateway computes/renames itself (`:stdout`/`:error` are
   bounded, `:silent`/`:duration_ms` are derived; those stay explicit gateway
   overrides). Add a new verbatim display field HERE; `->display`/`<-wire` then
   flow it across every boundary without runtime key rewriting.

   Grouped: the source the model wrote, the result surfaces (raw + the
   pre-rendered op-card), the native-tool badge identity (label + colour), the
   per-form display projections, the tool-call linkage, and the repair/timeout
   flags channels surface."
  [;; source
   [:code "code"]
   [:comment "comment"]
   [:scope "scope"]
   [:started-at-ms "started_at_ms"]
   ;; result surfaces — the raw value, the pre-rendered op-card body, and the
   ;; op-card HEADLINE (a tool-authored summary, never a first-line body slice)
   [:result "result"]
   [:result-render "result_render"]
   [:result-summary "result_summary"]
   ;; MULTI-card: canonical MINI-FORMS, recursively normalized by `<-wire`.
   [:cards "cards"]
   ;; native-tool op-card badge identity; wire keys intentionally drop namespaces
   [:vis/tool-name "tool_name"]
   [:tool-color-role "tool_color_role"]
   ;; display projections
   [:render-segments "render_segments"]
   [:result-kind "result_kind"]
   [:result-detail "result_detail"]
   [:tag "tag"]
   ;; tool-call linkage + status flags channels surface
   [:svar/tool-call-id "tool_call_id"]
   [:timeout? "is_timeout"]
   [:repaired? "is_repaired"]
   [:auto-repaired? "is_auto_repaired"]])

(def display-keys
  "The canonical engine keys projected by `->display` and recovered by `<-wire`."
  (mapv first display-fields))

(def ^:private label-overrides
  "Native-tool WIRE name → a nicer op-card LABEL. Most tools read fine uppercased
   (RG, CAT, PATCH); a few don't — `python_execution` is the model writing/running
   code, but its card surfaces what that run produced, so it reads `RESULT`;
   `repl_eval` reads as the terse `REPL` badge its collapsed/expanded card wants;
   the `!`/`!&` shell-sugar tools read as prose `SHELL RUN` / `SHELL BACKGROUND`
   instead of the raw wire names `SHELL_RUN` / `SHELL_BG`."
  {"python_execution" "RESULT"
   "repl_eval" "REPL"
   "shell_run" "SHELL RUN"
   "shell_bg" "SHELL BACKGROUND"})

(def tool-color-roles
  "The canonical set of native-tool op-card BADGE colour roles — the ONE list both
   channels colour against (TUI maps each to a lanterna fg, the web to a `--tool-*`
   CSS var). Hand-maintained per-channel maps were free to drift; a guard test in
   each channel asserts its map covers every role here, so a new role can't be
   added in one channel and silently forgotten in the other."
  [:tool-color/read :tool-color/search :tool-color/preview :tool-color/edit :tool-color/create
   :tool-color/delete :tool-color/move :tool-color/shell :tool-color/meta :tool-color/test])

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
  [{:keys [tool-color-role result-summary result-render] tool-name :vis/tool-name}]
  (when (some? tool-name)
    (let [summary
          (some-> result-summary
                  str
                  str/trim
                  not-empty)

          body
          (some-> result-render
                  str
                  str/trimr
                  not-empty)]

      {:tool? true
       :label (tool-label tool-name)
       :color-role (cond (keyword? tool-color-role) tool-color-role
                         (string? tool-color-role) (keyword tool-color-role)
                         :else nil)
       :summary summary
       :body body
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
    (if-let [c (result-card form)]
      [c]
      [])))

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
  (let [errored? (or (some? error) (and (some? success?) (not success?)))]
    (boolean (and (not errored?)
                  (native-tool-form? form)
                  (not= (name (:vis/tool-name form)) "python_execution")))))

(def ^:private same-path-coalescable-tools
  "Native tools whose ADJACENT op-cards fold only when they target the SAME file."
  #{"cat" "patch"})

(def ^:private same-tool-coalescable-tools
  "Native tools whose ADJACENT op-cards fold by tool name, even across files.
   `format_code` often runs once per file from the agent workflow; two
   back-to-back no-op format acks should read as ONE FORMAT_CODE roll-up, not a
   pile of identical sibling bubbles."
  #{"format_code"})

(defn- tool-name-s
  [form]
  (some-> (:vis/tool-name form)
          str))

(defn- coalesce-error-form?
  "A form the gateway marked failed (`:success? false`) — never folded into a run
   (a failed read/edit/format is its own event and keeps its inline error)."
  [form]
  (and (some? (:success? form)) (not (:success? form))))

(defn- result-field
  "Read a result field from a live Clojure map (`:path`) or a JSON/DB-restored map
   (`\"path\"`). nil for non-map results."
  [form k]
  (let [r (:result form)]
    (when (map? r)
      (cond (contains? r k) (get r k)
            (contains? r (name k)) (get r (name k))))))

(defn- same-path-form-path
  "The file PATH a same-path coalescable op-card (`cat`/`patch`) acted on — from
   the raw `:result` `:path` when present, else parsed out of the first
   `` `path` `` chip of the summary (which survives a DB round-trip that flattens
   `:result` to a string). nil for a non-coalescable / pathless form."
  [form]
  (when (contains? same-path-coalescable-tools (tool-name-s form))
    (or (result-field form :path)
        (some-> (:result-summary form)
                str
                (->> (re-find #"`([^`]+)`"))
                second))))

(defn- split-summary-parts [summary] (str/split (str summary) #" · "))

(defn- merge-same-path-summaries
  "Fold N same-file op-card summaries into ONE. Splits each on ` · `, keeps the
   shared leading chip (`` `path` `` for cat, `` update `path` `` for patch), then
   appends every DISTINCT tail — for cat the read's LINE SPANS plus the SUMMED
   line count (only when every merged read carried one). So two reads render
   `` `a.clj` · L1-10 · L40-50 `` and two edits stay `` update `a.clj` `` instead
   of two look-alike cards."
  [summaries]
  (let [parts
        (map split-summary-parts summaries)

        chip
        (ffirst parts)

        tails
        (mapcat rest parts)

        count-re
        #"^(\d+) lines?$"

        counts
        (keep #(some-> (re-matches count-re %)
                       second
                       parse-long)
              tails)

        spans
        (remove #(re-matches count-re %) tails)

        total
        (reduce + 0 counts)

        span-str
        (str/join " · " (distinct spans))

        count-str
        (when (and (seq counts) (= (count counts) (count summaries)))
          (str total " line" (when (not= 1 total) "s")))

        tail
        (str/join " · " (remove str/blank? [span-str count-str]))]

    (if (str/blank? tail) chip (str chip " · " tail))))

(defn- format-summary-entry
  "Parse a single-path format_code summary like `` `src/x.clj` (no change) ``.
   Falls back to the raw summary so already-aggregated/restored cards still remain
   visible instead of disappearing."
  [form]
  (let [summary (str (:result-summary form))]
    (if-let [[_ path status] (re-matches #"`([^`]+)`\s+(.*)" summary)]
      {:path path :status status}
      {:path (or (result-field form :path) summary)
       :status (if (result-field form :changed) "(changed)" "")})))

(defn- merge-format-forms
  "Turn adjacent per-file `format_code` acks into the same shape as one
   `format_code {\"paths\": [...]}` call: one headline plus a collapsible per-file
   body."
  [forms]
  (let [entries
        (map format-summary-entry forms)

        n
        (count entries)

        changed
        (count (filter #(str/includes? (str (:status %)) "(changed") entries))

        body
        (str/join "\n"
                  (for [{:keys [path status]} entries]
                    (str path (when (seq status) (str " " status)))))]

    {:summary (str n " file" (when (not= 1 n) "s") " — " changed " changed")
     :body (when (seq body) (str "```\n" body "\n```"))}))

(defn- merge-run
  "Collapse a RUN of adjacent op-cards into one synthesized form: for same-file
   read/edit tools, the combined multi-span/multi-diff summary plus every card's
   body slice; for per-file format acks, one format roll-up body. Channels render
   the synthesized form as ONE card/bubble."
  [forms]
  (let [f0
        (first forms)

        tool
        (tool-name-s f0)

        merged
        (if (= "format_code" tool)
          (merge-format-forms forms)
          {:summary (merge-same-path-summaries (map :result-summary forms))
           :body (str/join "\n" (keep (comp not-empty str :result-render) forms))})

        anchors
        (reduce merge {} (map #(result-field % :anchors) forms))

        r0
        (:result f0)]

    (cond-> (assoc f0
              :result-summary (:summary merged)
              :result-render (:body merged))
      ;; Only merge anchors onto a genuine MAP result. After a DB round-trip
      ;; `:result` comes back as the rendered string (anchors flattened) and the
      ;; path/spans already live in the merged summary, so a non-map result
      ;; carries through untouched (assoc'ing onto a string throws).
      (map? r0)
      (assoc :result (assoc r0 :anchors anchors)))))

(defn- coalesce-key
  [form]
  (when-not (coalesce-error-form? form)
    (let [tool (tool-name-s form)]
      (cond (contains? same-path-coalescable-tools tool) (when-let [p (same-path-form-path form)]
                                                           [::same-path tool p])
            (contains? same-tool-coalescable-tools tool) [::same-tool tool]))))

(defn coalesce-forms
  "Merge each maximal run of ADJACENT, successful coalescable native op-cards into
   a single card. `cat`/`patch` coalesce only for the SAME file; `format_code`
   coalesces adjacent per-file acks into one roll-up across files. Every other
   form passes through untouched. The ONE projection both channels apply before
   rendering an iteration's forms, so repeated tool acks never render as a stack
   of look-alike sibling bubbles. Always returns a vector."
  [forms]
  (let [key-fn (fn [f]
                 (or (coalesce-key f) [::solo (gensym)]))]
    (into []
          (map (fn [grp]
                 (if (next grp) (merge-run grp) (first grp))))
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
  (reduce (fn [acc k]
            (if (some? (get m k)) (assoc acc k (get m k)) acc))
          {}
          display-keys))



(defn <-wire
  "Read the canonical display fields back off a gateway WIRE event into a form,
   using the literal wire spelling declared beside each engine key in
   `display-fields`, and re-keywording keyword-valued fields. The single inbound
   projection channels use — the mirror of `->display`."
  [event]
  (reduce (fn [acc [k wire-k]]
            (let [v (get event wire-k)]
              (cond (nil? v) acc
                    (keyword-valued k) (assoc acc k (keyword v))
                    ;; `:cards` is a vector of canonical MINI-FORMS. Recurse so
                    ;; each nested colour keyword survives the JSON hop too.
                    (= k :cards) (assoc acc k (mapv <-wire v))
                    :else (assoc acc k v))))
          {}
          display-fields))
