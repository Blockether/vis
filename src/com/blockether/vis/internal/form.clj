(ns com.blockether.vis.internal.form
  "The canonical per-form DISPLAY contract — ONE source of truth for the fields a
   channel reads to render an executed form, live (via the gateway) and restored
   (via the DB).

   Why this exists: the SAME field set used to be hand-listed in independent
   allowlists across the loop, persistence, gateway, progress, and restored display
   paths. Now every layer projects the WHOLE set through `->display` (outbound) /
   `<-wire` (inbound), so a new display field is a ONE-line change to
   `display-keys` and `form-roundtrip-test` fails if a boundary stops carrying it.

   Transformed fields (`:stdout`/`:error` bounded, `:silent`/`:duration_ms`
   renamed) stay as explicit gateway overrides — they are not carried verbatim, so
   they are NOT in this set."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.pyfmt :as pyfmt]))

(def ^:private display-fields
  "Every field carried VERBATIM from the loop to a channel to render a form,
   paired with its literal gateway wire key. This is the complete passthrough set,
   NOT the handful the gateway computes/renames itself (`:stdout`/`:error` are
   bounded, `:silent`/`:duration_ms` are derived; those stay explicit gateway
   overrides). Add a new verbatim display field HERE; `->display`/`<-wire` then
   flow it across every boundary without runtime key rewriting.

   Grouped: the source the model wrote, the result surfaces, the native-tool badge
   label, the per-form display projections, the tool-call linkage, and the
   repair/timeout flags channels surface."
  [;; source
   [:code "code"] [:display-code "display_code"] [:display-language "display_language"]
   [:comment "comment"] [:scope "scope"] [:started-at-ms "started_at_ms"]
   ;; result surfaces — the raw value, the pre-rendered op-card body, and the
   ;; op-card HEADLINE (a tool-authored summary, never a first-line body slice)
   [:result "result"] [:result-render "result_render"] [:result-summary "result_summary"]
   ;; the same HEADLINE while the call is still RUNNING, authored by the tool's
   ;; `:render-start-call-fn`. Its own key: a pending card must never look like an outcome,
   ;; so channels still read "running" off the absent `:result-*` fields.
   ;; …and the BODY it paints under that headline while it runs: the tool's own
   ;; card sections, built by the same renderers its finished body uses.
   [:pending-summary "pending_summary"] [:pending-render "pending_render"]
   ;; MULTI-card: canonical MINI-FORMS, recursively normalized by `<-wire`.
   [:cards "cards"]
   ;; native-tool op-card badge identity
   [:vis/tool-name "tool_name"]
   ;; display projections
   [:render-segments "render_segments"] [:result-kind "result_kind"]
   [:result-detail "result_detail"] [:tag "tag"]
   ;; tool-call linkage + status flags channels surface
   [:svar/tool-call-id "tool_call_id"] [:timeout? "is_timeout"] [:repaired? "is_repaired"]
   [:auto-repaired? "is_auto_repaired"]])

(def display-keys
  "The canonical engine keys projected by `->display` and recovered by `<-wire`."
  (mapv first display-fields))

(def ^:private label-overrides
  "Native-tool WIRE name → a nicer op-card LABEL. Most tools read fine uppercased
   (RG, CAT, PATCH); a few don't — `python_execution` is the model writing/running
   code, but its card surfaces what that run produced, so it reads `RESULT`;
   `repl_eval` reads as the terse `REPL` badge its collapsed/expanded card wants.
   `shell` needs no override: it is ONE tool whose op-card already says which op
   ran, so its badge is the plain uppercased `SHELL`."
  {"python_execution" "RESULT" "native_call" "NATIVE CALL" "repl_eval" "REPL"})


(defn tool-label
  "The op-card badge LABEL for a native tool's wire name: the name uppercased,
   except the few `label-overrides` rename. ONE place both channels derive it from
   so the TUI badge and the web label never drift. nil for a non-tool form."
  [wire-name]
  (when (some? wire-name)
    (let [n (name wire-name)]
      (or (label-overrides n) (str/upper-case n)))))

(def ^:private compact-path-summary-tools #{"patch" "struct_patch" "write"})

(defn- compact-tool-summary
  "Remove mutation verbs made redundant by the tool badge. This also normalizes
   persisted pre-change summaries, so restored TUI and gateway traces render like
   new ones. Other tool-authored summaries remain untouched."
  [summary tool-name]
  (if (contains? compact-path-summary-tools (name tool-name))
    (-> summary
        (str/replace #"(^| · )(?:(?:update|add|delete|replace|overwrite)\s+|\(no change\)\s+)" "$1")
        (str/replace " · " ", "))
    summary))

(defn result-card
  "Canonical tool-result CARD descriptor — the ONE place the op-card / collapse
   decision is made, so the TUI and web AGREE on `tool?`/label/summary/collapsible
   instead of each re-deriving it from the raw form. Given an executed
   form map, returns the op-card shape for a NATIVE TOOL result:

     {:tool?        true
      :label        RG                  — op-card badge label (`tool-label`)
      :summary      5 hits in 1 file    — the HEADLINE (`:result-summary`), nil
                                           when the tool authored none
      :body         …markdown…          — the detail body (`:result-render`), nil
                                           for a summary-only tool (move/delete)
      :collapsible? true}               — true ⇔ there's a body to fold under
                                           the summary (a chevron/`<details>`)

   A call still RUNNING has no result yet, so the headline falls back to the
   tool-authored `:pending-summary` (`shell`'s `$ npm test (running)`): the SAME
   card, in its awaiting state, rather than a bare unlabeled code band.

   `nil` for a NON-tool form (no `:vis/tool-name`) — its result rendering stays
   channel-specific (raw value / stdout). The badge is whatever the tool's
   `:summary` already produced; no first-line-of-body heuristic."
  [{:keys [result-summary pending-summary result-render pending-render] tool-name :vis/tool-name}]
  (when (some? tool-name)
    (let
      [summary
       (or (some-> result-summary
                   str
                   str/trim
                   not-empty
                   (compact-tool-summary tool-name))
           (some-> pending-summary
                   str
                   str/trim
                   not-empty))

       ;; A RUNNING call has no result body yet, so the card shows the tool's own
       ;; PENDING body — the same sections its result renderer builds, minus the
       ;; outcome — and simply swaps it for the real one when the call lands.
       body
       (or (some-> result-render
                   str
                   str/trimr
                   not-empty)
           (some-> pending-render
                   str
                   str/trimr
                   not-empty))]

      {:tool? true
       :label (tool-label tool-name)
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
  "Should a channel DROP a form's invocation source instead of showing it as a
   separate code block? Successful native tools already have a result card, so
   their synthesized invocation is redundant. Failed native tools keep source
   context. `python_execution` is different: its program is user-relevant
   evidence and remains visible on both success and failure. This is the shared
   TUI/channel policy; web mirrors it at the wire boundary."
  [{:keys [error success?] :as form}]
  (let
    [errored?
     (or (some? error) (and (some? success?) (not success?)))

     tool-name
     (some-> (:vis/tool-name form)
             name)]

    (boolean (and (native-tool-form? form) (not errored?) (not= tool-name "python_execution")))))

(def ^:private coalescable-tools
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


(defn- format-summary-entries
  "Recover every per-file row from a `format_code` form. Current language packs
   return an aggregate headline plus structured `:files`; older/restored records
   may instead carry a single `` `path` (status) `` headline. Never use an
   aggregate headline as a fake file path."
  [form]
  (let
    [files
     (result-field form :files)

     from-files
     (when (sequential? files)
       (keep (fn [file]
               (when (map? file)
                 (let
                   [path
                    (or (get file :path) (get file "path"))

                    changed?
                    (if (contains? file :changed) (get file :changed) (get file "changed"))]

                   (when (seq (str path))
                     {:path (str path) :status (if changed? "(changed)" "(no change)")}))))
             files))

     summary
     (str (:result-summary form))]

    (or (seq from-files)
        (when-let [[_ path status] (re-matches #"`([^`]+)`\s+(.*)" summary)]
          [{:path path :status status}])
        (when-let [path (result-field form :path)]
          [{:path path :status (if (result-field form :changed) "(changed)" "(no change)")}])
        [])))

(defn- merge-format-forms
  "Turn adjacent per-file `format_code` acks into the same shape as one
   `format_code {\"paths\": [...]}` call: one headline plus a collapsible per-file
   body."
  [forms]
  (let
    [entries
     (mapcat format-summary-entries forms)

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
  "Collapse a RUN of adjacent per-file `format_code` acks into one synthesized
   form carrying a single roll-up headline and body. Channels render the
   synthesized form as ONE card/bubble."
  [forms]
  (let [merged (merge-format-forms forms)]
    (assoc (first forms)
      :result-summary (compact-tool-summary (:summary merged) (tool-name-s (first forms)))
      :result-render (:body merged))))

(defn- coalesce-key
  [form]
  (when-not (coalesce-error-form? form)
    (let [tool (tool-name-s form)]
      (when (contains? coalescable-tools tool) [::same-tool tool]))))

(defn coalesce-forms
  "Merge each maximal run of ADJACENT, successful coalescable native op-cards into
   a single card: `format_code` folds adjacent per-file acks into one roll-up
   across files. Every other form passes through untouched — batching tools such
   as cat, patch and grep already carry every path of one call in ONE card, so
   two adjacent cards are two genuinely distinct calls. The ONE projection both
   channels apply before rendering an iteration's forms, so repeated tool acks
   never render as a stack of look-alike sibling bubbles. Always returns a vector."
  [forms]
  (let
    [key-fn (fn [f]
              (or (coalesce-key f) [::solo (gensym)]))]
    (into []
          (map (fn [grp]
                 (if (next grp) (merge-run grp) (first grp))))
          (partition-by key-fn (vec forms)))))


(defn with-display-code
  "Attach the canonical cached ruff rendering of a form's Python source.
   Channels render `:display-code` verbatim; local callers without it may use
   the same formatter. Nested result cards are normalized recursively.

   An AUTHORED `:display-code` is never overwritten: a native tool may render
   its own PENDING call (`:render-start-call-fn` — `shell` ships the bash it is about to
   run instead of the raw invocation JSON), and that surface, paired with its
   `:display-language`, is the one the channels must paint."
  [form]
  (cond-> form
    (and (str/blank? (str (:display-code form))) (not (str/blank? (str (:code form)))))
    (assoc :display-code (pyfmt/beautify-python (:code form)))

    (seq (:cards form))
    (update :cards #(mapv with-display-code %))))

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
   `display-fields`. The single inbound projection channels use — the mirror of
   `->display`."
  [event]
  (reduce (fn [acc [k wire-k]]
            (let [v (get event wire-k)]
              (cond (nil? v) acc
                    ;; `:cards` is a vector of canonical MINI-FORMS.
                    (= k :cards) (assoc acc k (mapv <-wire v))
                    :else (assoc acc k v))))
          {}
          display-fields))
