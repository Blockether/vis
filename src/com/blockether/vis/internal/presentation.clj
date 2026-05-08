(ns com.blockether.vis.internal.presentation
  "Shared internal presentation/render contract.

   This namespace owns the data shapes and pure rendering helpers
   that every Vis surface (TUI, CLI, Telegram, transcript export,
   audit reports) projects through. It is intentionally:

   - HOST-ONLY. Pure Clojure, no Lanterna/graphical/SCI dependency.
     Can be loaded under headless terminals, CI, SSH, REPL, tests.
   - DATA-FIRST. Builders return plain maps tagged with
     `:vis.presentation/kind`. Renderers consume those maps.
   - MARKDOWN-CANONICAL. Every renderer emits Markdown. Channels then
     run the Markdown through their own projection (TUI markdown
     marker tokeniser, Telegram MarkdownV2 escaper, plain-text
     export).

   Recognized kinds:

   - `:vis.presentation/markdown`     - Markdown block, body is raw markdown
   - `:vis.presentation/details`      - <details>/<summary> block with
                                         optional `:collapsed?` flag, body
                                         is itself a markdown string OR a
                                         seq of presentation maps
   - `:vis.presentation/tool-call`    - Tool invocation with op/args/result
   - `:vis.presentation/system-call`  - System-call event (vis/system,
                                         vis/needs-input, etc.)
   - `:vis.presentation/provider-error` - Provider/schema/parse failure
   - `:vis.presentation/mermaid`      - Mermaid fenced block (source +
                                         optional rendered ASCII)
   - `:vis.presentation/audit-report` - Audit/proof report with
                                         events/bundles/attestations/
                                         gates/plans/intents and an
                                         optional `:mermaid` graph

   The contract is small on purpose: every channel can either dispatch
   on `:vis.presentation/kind` or call `present` to get Markdown back.
   Proof, intents, and audit semantics are preserved end-to-end:
   the audit report carries every counted slot, blocker refs are
   highlighted, and `:guards`/`:violations` survive collapsed AND
   expanded summaries."
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [com.blockether.vis.internal.markdown :as md]))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; Kinds + predicate helpers
;; ---------------------------------------------------------------------------

(def presentation-kinds
  "Closed set of recognized `:vis.presentation/kind` values. Renderers
   that don't know how to handle a kind fall back to `pr-str` of the
   value so unknown shapes still surface as Markdown text instead of
   throwing."
  #{:vis.presentation/markdown
    :vis.presentation/details
    :vis.presentation/tool-call
    :vis.presentation/system-call
    :vis.presentation/provider-error
    :vis.presentation/mermaid
    :vis.presentation/audit-report})

(defn presentation?
  "True when `x` is a presentation map (carries a recognized
   `:vis.presentation/kind`)."
  [x]
  (and (map? x) (contains? presentation-kinds (:vis.presentation/kind x))))

(defn- ->str
  ^String [x]
  (cond
    (nil? x) ""
    (string? x) x
    :else (str x)))

(defn- non-blank? [s]
  (and (string? s) (not (str/blank? s))))

;; ---------------------------------------------------------------------------
;; Builders
;; ---------------------------------------------------------------------------

(defn markdown
  "Wrap a Markdown string as a presentation map. Body is the raw
   Markdown; renderer is identity."
  [body]
  {:vis.presentation/kind :vis.presentation/markdown
   :body (->str body)})

(defn details
  "Collapsible `<details>/<summary>` block. `body` may be a Markdown
   string OR a seq of presentation maps (renderer recurses).
   `:collapsed?` defaults to true."
  ([summary body] (details summary body {}))
  ([summary body {:keys [collapsed?] :or {collapsed? true}}]
   {:vis.presentation/kind :vis.presentation/details
    :summary (->str summary)
    :body body
    :collapsed? (boolean collapsed?)}))

(defn tool-call
  "Tool invocation presentation map. Required keys (any one is
   enough): `:op`, `:tool`, or `:label`. Optional: `:args`, `:result`,
   `:stdout`, `:stderr`, `:error`, `:duration-ms`, `:status`,
   `:provenance`, `:ref`, `:rendering-kind`. The renderer formats the
   call as a self-describing details block."
  [opts]
  (merge {:vis.presentation/kind :vis.presentation/tool-call}
    (select-keys opts [:op :tool :label :args :result :stdout :stderr
                       :error :duration-ms :status :provenance :ref
                       :rendering-kind])))

(defn system-call
  "System-call event (vis/system, vis/needs-input, vis/answer with
   side effect). Same shape family as `tool-call` but renders with
   the `:vis/system` heading."
  [opts]
  (merge {:vis.presentation/kind :vis.presentation/system-call}
    (select-keys opts [:op :tool :label :result :error :status
                       :rendering-kind :provenance :ref :body :args])))

(defn provider-error
  "Provider/schema/parse failure presentation map. Required:
   `:message`. Optional: `:type`, `:reason`, `:received-type`,
   `:raw-preview`, `:advice`, `:iteration`, `:iteration-id`,
   `:status`, `:provider`, `:model`, `:classification`."
  [opts]
  (merge {:vis.presentation/kind :vis.presentation/provider-error}
    (select-keys opts [:message :type :reason :received-type :raw-preview
                       :advice :iteration :iteration-id :status :provider
                       :model :classification])))

(defn mermaid
  "Mermaid fenced block. Required `:source`. Optional `:rendered-lines`
   (vector of pre-rendered ASCII lines). When `:rendered-lines` is
   present, the renderer emits both the source fence (so other
   surfaces can re-render it) AND a details block with the ASCII art."
  [opts]
  (merge {:vis.presentation/kind :vis.presentation/mermaid}
    (select-keys opts [:source :rendered-lines :diagram-type])))

(defn audit-report
  "Audit/proof report presentation map. The renderer projects this
   into a coherent Markdown report with sections for
   events/bundles/attestations/gates/plans/intents, an optional
   Mermaid graph, a collapsed summary line, and an expanded body.

   Recognized keys (every section optional):

   - `:title`              human-readable title (default `\"Audit\"`)
   - `:counts`             `{:events n :bundles n :attestations n
                              :gates n :plans n :intents n :violations n}`
   - `:events`             coll of provenance timeline events
   - `:bundles`            coll of evidence bundles
   - `:attestations`       coll of attestations
   - `:gates`              coll of gate snapshots
   - `:plans`              coll of plan snapshots
   - `:intents`            coll of intent snapshots
   - `:guards`             `{:success? bool :violations [...]}`
   - `:mermaid`            either a Mermaid source string OR a map
                            `{:source ... :rendered-lines [...]}`
   - `:summary`            override the collapsed one-line summary
   - `:collapsed?`         default true (renders summary + details)"
  [opts]
  (merge {:vis.presentation/kind :vis.presentation/audit-report
          :title "Audit"
          :collapsed? true}
    (select-keys opts [:title :counts :events :bundles :attestations
                       :gates :plans :intents :guards :mermaid
                       :summary :collapsed? :conversation-id])))

;; ---------------------------------------------------------------------------
;; Pure helpers shared between renderers
;; ---------------------------------------------------------------------------

(defn- preview
  ([text] (preview text 220))
  ([text limit]
   (when (some? text)
     (let [s (str text)]
       (if (> (count s) limit)
         (str (subs s 0 limit) "…")
         s)))))

(defn- kw-name
  "`name`-equivalent that preserves the namespace component when
   present. `:svar.spec/schema-rejected` -> `\"svar.spec/schema-rejected\"`,
   `:running` -> `\"running\"`. Used everywhere the renderer surfaces
   semantic keywords back as Markdown."
  ^String [k]
  (cond
    (nil? k) ""
    (keyword? k) (str (some-> k namespace (str "/")) (name k))
    :else (str k)))

(defn- format-status
  ^String [status]
  (cond
    (nil? status) "·"
    (keyword? status) (kw-name status)
    :else (str status)))

(defn- format-op
  ^String [op]
  (cond
    (nil? op) "?"
    (keyword? op) (str (some-> op namespace (str "/")) (name op))
    :else (str op)))

(defn- format-duration-ms
  ^String [ms]
  (when (number? ms)
    (cond
      (< (long ms) 1) "<1ms"
      (< (long ms) 1000) (str (long ms) "ms")
      :else (format "%.1fs" (/ (double ms) 1000.0)))))

(defn- as-markdown-fence
  ^String [lang body]
  (md/code-block (or lang "text") (str body)))

(defn- pretty-args
  ^String [args]
  (cond
    (nil? args) ""
    (string? args) args
    (map? args) (with-out-str (pp/pprint args))
    :else (pr-str args)))

;; ---------------------------------------------------------------------------
;; Renderers (all return Markdown strings; never throw)
;; ---------------------------------------------------------------------------

(defn markdown->md
  "Render a `:vis.presentation/markdown` map. Identity over body
   (after coercion / whitespace trim)."
  [{:keys [body]}]
  (str/trimr (->str body)))

(declare present)

(defn details->md
  "Render a `:vis.presentation/details` block. When `:body` is a
   string, embed verbatim. When it's a seq of presentation maps, run
   each through `present` and join with blank lines."
  [{:keys [summary body collapsed?]}]
  (let [body-text (cond
                    (string? body) body
                    (presentation? body) (present body)
                    (sequential? body) (->> body
                                         (map #(if (presentation? %) (present %) (->str %)))
                                         (remove str/blank?)
                                         (str/join "\n\n"))
                    :else (->str body))
        body-text (str/trim body-text)
        sum (md/summary (or summary "Details"))]
    (cond
      ;; A "collapsed" view in pure markdown still shows summary + body
      ;; collapsed under <details>. Channels that own real toggle state
      ;; (TUI) own the actual show/hide; the markdown shape preserves
      ;; both halves so static exports stay faithful.
      (str/blank? body-text)
      (md/details (md/summary (or summary "Details")))

      collapsed?
      (md/details sum body-text)

      :else
      (md/details sum body-text))))

(defn tool-call->md
  "Render a `:vis.presentation/tool-call` block.

   Layout:

       **TOOL** `op` · status · 12ms
       _label or short summary_
       <args fenced>
       <result fenced or details>
       <stdout/stderr details if non-blank>
       <error block if present>"
  [{:keys [op tool label args result stdout stderr error duration-ms
           status rendering-kind provenance ref]}]
  (let [op-str (format-op (or op tool))
        head-bits (cond-> [(md/bold "TOOL")
                           (md/code op-str)]
                    status (conj (str " · " (format-status status)))
                    duration-ms (conj (str " · " (format-duration-ms duration-ms)))
                    ref (conj (str " · " (md/code (str ref)))))
        head (str/join "" head-bits)
        sub (when (non-blank? label) (md/italic label))
        args-block (when (some? args)
                     (let [s (pretty-args args)]
                       (when (non-blank? s)
                         (md/details (md/summary "args")
                           (as-markdown-fence "edn" (str/trim s))))))
        result-block (cond
                       (nil? result) nil
                       (string? result)
                       (if (str/blank? result)
                         nil
                         ;; Result text is already markdown by convention
                         ;; (extension render-fns return markdown).
                         (md/details (md/summary "result")
                           (str/trim result)))
                       :else
                       (md/details (md/summary "result")
                         (as-markdown-fence "edn" (pr-str result))))
        stdout-block (when (non-blank? stdout)
                       (md/details (md/summary "stdout")
                         (as-markdown-fence "text" (str/trim stdout))))
        stderr-block (when (non-blank? stderr)
                       (md/details (md/summary "stderr")
                         (as-markdown-fence "text" (str/trim stderr))))
        error-block (when error
                      (let [text (cond
                                   (string? error) error
                                   (map? error) (or (:message error) (pr-str error))
                                   :else (str error))]
                        (str (md/bold "Error: ") (md/code (preview text 240)))))
        kind-line (when rendering-kind
                    (md/italic (str "rendering-kind: " (kw-name rendering-kind))))]
    (->> [head
          sub
          kind-line
          args-block
          result-block
          stdout-block
          stderr-block
          error-block
          (when provenance
            (md/details (md/summary "provenance")
              (as-markdown-fence "edn"
                (with-out-str (pp/pprint provenance)))))]
      (remove nil?)
      (remove str/blank?)
      (str/join "\n\n"))))

(defn system-call->md
  "Render a `:vis.presentation/system-call` block. Same data shape as
   tool-call; uses `**SYSTEM**` heading and a slightly different
   default body — system events often carry no args, just a status
   + body text."
  [opts]
  (let [{:keys [op label body args result error rendering-kind]} opts
        op-str (format-op (or op (:tool opts)))
        head (str (md/bold "SYSTEM") " " (md/code op-str)
               (when (:status opts) (str " · " (format-status (:status opts)))))
        sub (when (non-blank? label) (md/italic label))
        body-text (when body
                    (let [s (->str body)]
                      (when (non-blank? s)
                        (str/trim s))))
        args-block (when (some? args)
                     (let [s (pretty-args args)]
                       (when (non-blank? s)
                         (md/details (md/summary "args")
                           (as-markdown-fence "edn" (str/trim s))))))
        result-block (when (and result (not (str/blank? (->str result))))
                       (md/details (md/summary "result")
                         (if (string? result)
                           (str/trim result)
                           (as-markdown-fence "edn" (pr-str result)))))
        error-block (when error
                      (str (md/bold "Error: ") (md/code (preview (->str error) 240))))
        kind-line (when rendering-kind
                    (md/italic (str "rendering-kind: " (kw-name rendering-kind))))]
    (->> [head sub kind-line body-text args-block result-block error-block]
      (remove nil?)
      (remove str/blank?)
      (str/join "\n\n"))))

(defn provider-error->md
  "Render a `:vis.presentation/provider-error` block. Surfaces type,
   reason, raw preview, and advice in a stable shape so downstream
   judging (and the user) always see the failure cause and the
   recommended action."
  [{:keys [message type reason received-type raw-preview advice
           iteration provider model classification status]}]
  (let [head (str (md/bold "PROVIDER ERROR")
               (when classification (str " · " (md/code (kw-name classification))))
               (when iteration (str " · iteration " iteration)))
        meta-bits (cond-> []
                    provider (conj (str "provider: " (md/code (kw-name provider))))
                    model (conj (str "model: " (md/code (str model))))
                    type (conj (str "type: " (md/code (kw-name type))))
                    reason (conj (str "reason: " (md/code (kw-name reason))))
                    received-type (conj (str "received: " (md/code (str received-type))))
                    status (conj (str "status: " (md/code (format-status status)))))
        meta-line (when (seq meta-bits) (str/join " · " meta-bits))
        msg (when (non-blank? message)
              (str (md/bold "Message:") " " (->str message)))
        raw (when (non-blank? raw-preview)
              (md/details (md/summary "raw")
                (as-markdown-fence "text" (str/trim (str raw-preview)))))
        adv (when (non-blank? advice)
              (str (md/bold "Advice:") " " (->str advice)))]
    (->> [head meta-line msg raw adv]
      (remove nil?)
      (remove str/blank?)
      (str/join "\n\n"))))

(defn mermaid->md
  "Render a `:vis.presentation/mermaid` block. Always emits the
   ```mermaid``` source fence (so any surface can re-render with a
   real Mermaid engine). When `:rendered-lines` is present, an extra
   collapsed details block carries the pre-rendered ASCII diagram."
  [{:keys [source rendered-lines diagram-type]}]
  (let [src (str/trim (->str source))
        fence (md/code-block "mermaid" src)
        ascii (when (seq rendered-lines)
                (md/details (md/summary
                              (str "Mermaid diagram"
                                (when diagram-type
                                  (str " (" (name diagram-type) ")"))))
                  (md/code-block "text"
                    (str/join "\n" (mapv ->str rendered-lines)))))]
    (->> [fence ascii]
      (remove nil?)
      (str/join "\n\n"))))

;; ---------------------------------------------------------------------------
;; Audit report builders
;; ---------------------------------------------------------------------------

(defn- safe-coll [x]
  (cond
    (nil? x) []
    (sequential? x) (vec x)
    :else [x]))

(defn- summarise-counts
  "Auto-derive `:counts` if not supplied. Lossless: returns the union
   of `:counts` and any auto-derived counts."
  [{:keys [counts events bundles attestations gates plans intents guards]}]
  (let [auto {:events (count (safe-coll events))
              :bundles (count (safe-coll bundles))
              :attestations (count (safe-coll attestations))
              :gates (count (safe-coll gates))
              :plans (count (safe-coll plans))
              :intents (count (safe-coll intents))
              :violations (count (safe-coll (:violations guards)))}]
    (merge auto (or counts {}))))

(defn- audit-summary-line
  "One-line collapsed summary for an audit report.

       Audit · 7 events · 3 attestations · 2 intents (1 fulfilled) · ✓ guards"
  [{:keys [title counts intents guards]}]
  (let [counts (or counts {})
        intent-statuses (frequencies (keep :status (safe-coll intents)))
        fulfilled (get intent-statuses :fulfilled 0)
        bits (cond-> []
               (pos? (or (:events counts) 0))
               (conj (str (:events counts) " events"))

               (pos? (or (:attestations counts) 0))
               (conj (str (:attestations counts) " attestations"))

               (pos? (or (:gates counts) 0))
               (conj (str (:gates counts) " gates"))

               (pos? (or (:plans counts) 0))
               (conj (str (:plans counts) " plans"))

               (pos? (or (:intents counts) 0))
               (conj (str (:intents counts) " intents"
                       (when (pos? fulfilled)
                         (str " (" fulfilled " fulfilled)")))))
        guard-str (cond
                    (nil? guards) nil
                    (:success? guards) "✓ guards"
                    :else (str "✗ " (count (or (:violations guards) [])) " guard violations"))]
    (str (or title "Audit")
      (when (seq bits)
        (str " · " (str/join " · " bits)))
      (when guard-str
        (str " · " guard-str)))))

(defn- render-event
  [{:keys [ref parent-ref kind op status duration-ms error]}]
  (str "- " (md/code (or ref "?")) " "
    (kw-name (or kind :event)) " " (md/code (format-op op))
    (when parent-ref (str " parent " (md/code parent-ref)))
    " → " (format-status status)
    (when duration-ms (str ", " (format-duration-ms duration-ms)))
    (when error (str " — " (preview (cond
                                      (string? error) error
                                      (map? error) (or (:message error) (pr-str error))
                                      :else (str error))
                             180)))))

(defn- render-bundle
  [{:keys [id kind subject-ref attester decision status]}]
  (str "- " (md/code (str (or id "?"))) " "
    (when kind (str (kw-name kind) " "))
    (when subject-ref (str "→ " (md/code subject-ref) " "))
    (when attester (str "by " (md/code (str attester)) " "))
    (when decision (str (kw-name decision) " "))
    (when status (md/italic (kw-name status)))))

(defn- render-attestation
  [{:keys [id kind subject-id decision status reason]}]
  (str "- " (md/code (str (or id "?"))) " "
    (when kind (str (kw-name kind) " "))
    (when subject-id (str "→ " (md/code (str subject-id)) " "))
    (when decision (str (kw-name decision) " "))
    (when status (md/italic (kw-name status)))
    (when (non-blank? reason) (str " — " (preview reason 180)))))

(defn- render-gate
  [{:keys [id proposition question status]}]
  (str "- " (md/code (str (or id "?"))) " "
    (md/italic (or proposition question "Gate"))
    (when status (str " · " (format-status status)))))

(defn- render-plan
  [{:keys [id summary status]}]
  (str "- " (md/code (str (or id "?"))) " "
    (when (non-blank? summary) summary)
    (when status (str " · " (format-status status)))))

(defn- render-intent
  [{:keys [id title status rationale]}]
  (str "- " (md/code (str (or id "?"))) " "
    (md/bold (or title "Intent"))
    (when status (str " · " (format-status status)))
    (when (non-blank? rationale) (str " — " (preview rationale 180)))))

(defn- render-section
  "Render one audit section as a heading + bullet list, or `nil` when
   the coll is empty."
  [title coll renderer]
  (when (seq coll)
    (str (md/h3 title) "\n\n"
      (str/join "\n" (map renderer coll)))))

(defn audit-report->md
  "Render a `:vis.presentation/audit-report` map as Markdown.

   When `:collapsed?` is true (default), the output is:

       <details>
         <summary>Audit · 7 events · ...</summary>
         {full body}
       </details>

   Otherwise the full body is emitted directly. The body always
   contains:

   1. counts table (when any counts are non-zero),
   2. guard status block,
   3. events / bundles / attestations / gates / plans / intents
      sections (skipped when empty),
   4. optional Mermaid graph (always rendered as the source fence;
      ASCII art is added when `:rendered-lines` is present).

   This shape is stable across surfaces so the TUI, transcripts, and
   judges can all consume the same Markdown."
  [{:keys [title counts events bundles attestations gates plans intents
           guards mermaid summary collapsed?]
    :as report}]
  (let [counts (summarise-counts (assoc report :counts counts))
        title (or title "Audit")
        summary-line (or summary (audit-summary-line (assoc report :counts counts)))
        counts-table (when (some pos? (vals counts))
                       (md/table ["section" "n"]
                         (mapv (fn [[k v]] [(name k) v])
                           (sort-by first counts))))
        guards-block (cond
                       (nil? guards) nil
                       (:success? guards)
                       (str (md/bold "Guards:") " ok")
                       :else
                       (str (md/bold "Guards:") " "
                         "failed " (count (or (:violations guards) []))
                         " check(s)"
                         (when (seq (:violations guards))
                           (str "\n\n"
                             (str/join "\n"
                               (map (fn [v]
                                      (str "- "
                                        (md/code (kw-name (or (:type v) :unknown)))
                                        (when (:ref v) (str " " (md/code (:ref v))))
                                        (when (:message v) (str " — " (:message v)))))
                                 (:violations guards)))))))
        mermaid-block (when mermaid
                        (mermaid->md
                          (cond
                            (string? mermaid) {:vis.presentation/kind :vis.presentation/mermaid
                                               :source mermaid}
                            (map? mermaid) (merge {:vis.presentation/kind :vis.presentation/mermaid}
                                             mermaid))))
        sections (->> [(render-section "Events" events render-event)
                       (render-section "Bundles" bundles render-bundle)
                       (render-section "Attestations" attestations render-attestation)
                       (render-section "Gates" gates render-gate)
                       (render-section "Plans" plans render-plan)
                       (render-section "Intents" intents render-intent)]
                   (remove nil?))
        body (str/join "\n\n"
               (remove nil?
                 (concat [(md/h2 title)
                          counts-table
                          guards-block]
                   sections
                   [mermaid-block])))]
    (if collapsed?
      (md/details (md/summary summary-line) body)
      body)))

(defn audit-report->summary
  "One-line collapsed summary for an audit report. Useful for status
   bars / inline citations."
  [report]
  (audit-summary-line (assoc report :counts (summarise-counts report))))

;; ---------------------------------------------------------------------------
;; Dispatch
;; ---------------------------------------------------------------------------

(defn present
  "Project a presentation map to Markdown. Returns the input
   verbatim when it's already a string. Returns `pr-str` of the
   value when the kind is unrecognized so unknown shapes still
   render as something readable."
  [x]
  (cond
    (nil? x) ""
    (string? x) x
    (presentation? x)
    (case (:vis.presentation/kind x)
      :vis.presentation/markdown        (markdown->md x)
      :vis.presentation/details         (details->md x)
      :vis.presentation/tool-call       (tool-call->md x)
      :vis.presentation/system-call     (system-call->md x)
      :vis.presentation/provider-error  (provider-error->md x)
      :vis.presentation/mermaid         (mermaid->md x)
      :vis.presentation/audit-report    (audit-report->md x))
    :else (pr-str x)))

(defn markdown-fence?
  "True when `body-text` is a Markdown fenced block (or starts with
   one) whose lang matches `expected-lang`. Used by channels that
   want to detect a Mermaid block before delegating to the Mermaid
   renderer."
  [body-text expected-lang]
  (and (string? body-text)
    (let [trimmed (str/trim body-text)]
      (and (str/starts-with? trimmed "```")
        (let [first-line (first (str/split-lines trimmed))]
          (= (str/lower-case (or expected-lang ""))
            (str/lower-case (str/trim (subs first-line 3)))))))))
