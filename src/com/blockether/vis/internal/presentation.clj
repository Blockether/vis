(ns com.blockether.vis.internal.presentation
  "Shared internal presentation/render contract.

   This namespace owns the data shapes and pure rendering helpers
   that every Vis surface (TUI, CLI, Telegram, transcript export,
   reports) projects through. It is intentionally:

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

   The contract is small on purpose: every channel can either dispatch
   on `:vis.presentation/kind` or call `present` to get Markdown back."
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]))

;; Inline markdown string-builder helpers.
;; All channels render markdown via their registered
;; :channel/messages-renderer-fn; presentation strings stay in pure
;; GFM (no <details>/<summary>/<kbd>/<details> raw HTML — those break
;; Telegram's HTML parse_mode and have no analog in the IR taxonomy).
(defn- md-bold       ^String [s]      (str "**" s "**"))
(defn- md-italic     ^String [s]      (str "*" s "*"))
(defn- md-code       ^String [s]      (str "`" s "`"))
(defn- md-code-block
  (^String [body]      (str "```\n" body "\n```"))
  (^String [lang body] (str "```" (or lang "") "\n" body "\n```")))
(defn- md-section
  "Render `summary` as a bold label, body indented underneath.
   Replaces the prior `<details><summary>...</summary>...</details>`
   pattern. Visual collapsibility is gone; channels that owned real
   toggle state (TUI) can re-introduce it via styled rendering later."
  ^String [summary body]
  (let [s (str (or summary ""))
        b (str (or body ""))]
    (cond
      (and (str/blank? s) (str/blank? b)) ""
      (str/blank? b)                       (str "**" s "**")
      (str/blank? s)                       b
      :else                                (str "**" s "**\n\n" b))))

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
    :vis.presentation/mermaid})

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
   `:info`, `:ref`, `:role`. The renderer formats the
   call as a self-describing details block."
  [opts]
  (merge {:vis.presentation/kind :vis.presentation/tool-call}
    (select-keys opts [:op :tool :label :args :result :stdout :stderr
                       :error :duration-ms :status :info :ref
                       :role])))

(defn system-call
  "System-call event (vis/system, vis/needs-input, vis/answer with
   side effect). Same shape family as `tool-call` but renders with
   the `:vis/system` heading."
  [opts]
  (merge {:vis.presentation/kind :vis.presentation/system-call}
    (select-keys opts [:op :tool :label :result :error :status
                       :role :info :ref :body :args])))

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

;; ---------------------------------------------------------------------------
;; Pure helpers shared between renderers
;; ---------------------------------------------------------------------------

(defn- preview
  ([text] (preview text 220))
  ([text limit]
   (when (some? text)
     (let [s (str text)]
       (if (> (count s) limit)
         (str (subs s 0 limit) "...")
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
    (nil? status) "/"
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
  (md-code-block (or lang "text") (str body)))

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
  "Render a `:vis.presentation/details` block as `**summary**\n\nbody`.
   Visual collapsibility (the prior `<details>` HTML) is gone;
   channels that own toggle state can re-introduce it via styled
   rendering. The `:collapsed?` flag is accepted for API compat but
   no longer affects output."
  [{:keys [summary body]}]
  (let [body-text (cond
                    (string? body) body
                    (presentation? body) (present body)
                    (sequential? body) (->> body
                                         (map #(if (presentation? %) (present %) (->str %)))
                                         (remove str/blank?)
                                         (str/join "\n\n"))
                    :else (->str body))]
    (md-section (or summary "Details") (str/trim body-text))))

(defn tool-call->md
  "Render a `:vis.presentation/tool-call` block.

   Layout:

       **TOOL** `op` / status / 12ms
       _label or short summary_
       <args fenced>
       <result fenced or details>
       <stdout/stderr details if non-blank>
       <error block if present>"
  [{:keys [op tool label args result stdout stderr error duration-ms
           status rendering-kind info ref]}]
  (let [op-str (format-op (or op tool))
        head-bits (cond-> [(md-bold "TOOL")
                           (md-code op-str)]
                    status (conj (str " / " (format-status status)))
                    duration-ms (conj (str " / " (format-duration-ms duration-ms)))
                    ref (conj (str " / " (md-code (str ref)))))
        head (str/join "" head-bits)
        sub (when (non-blank? label) (md-italic label))
        args-block (when (some? args)
                     (let [s (pretty-args args)]
                       (when (non-blank? s)
                         (md-section "args"
                           (as-markdown-fence "edn" (str/trim s))))))
        result-block (cond
                       (nil? result) nil
                       (string? result)
                       (if (str/blank? result)
                         nil
                         ;; Result text is already markdown by convention
                         ;; (extension render-fns return markdown).
                         (md-section "result"
                           (str/trim result)))
                       :else
                       (md-section "result"
                         (as-markdown-fence "edn" (pr-str result))))
        stdout-block (when (non-blank? stdout)
                       (md-section "stdout"
                         (as-markdown-fence "text" (str/trim stdout))))
        stderr-block (when (non-blank? stderr)
                       (md-section "stderr"
                         (as-markdown-fence "text" (str/trim stderr))))
        error-block (when error
                      (let [text (cond
                                   (string? error) error
                                   (map? error) (or (:message error) (pr-str error))
                                   :else (str error))]
                        (str (md-bold "Error: ") (md-code (preview text 240)))))
        kind-line (when rendering-kind
                    (md-italic (str "rendering-kind: " (kw-name rendering-kind))))]
    (->> [head
          sub
          kind-line
          args-block
          result-block
          stdout-block
          stderr-block
          error-block
          (when info
            (md-section "info"
              (as-markdown-fence "edn"
                (with-out-str (pp/pprint info)))))]
      (remove nil?)
      (remove str/blank?)
      (str/join "\n\n"))))

(defn system-call->md
  "Render a `:vis.presentation/system-call` block. Same data shape as
   tool-call; uses `**SYSTEM**` heading and a slightly different
   default body - system events often carry no args, just a status
   + body text."
  [opts]
  (let [{:keys [op label body args result error rendering-kind]} opts
        op-str (format-op (or op (:tool opts)))
        head (str (md-bold "SYSTEM") " " (md-code op-str)
               (when (:status opts) (str " / " (format-status (:status opts)))))
        sub (when (non-blank? label) (md-italic label))
        body-text (when body
                    (let [s (->str body)]
                      (when (non-blank? s)
                        (str/trim s))))
        args-block (when (some? args)
                     (let [s (pretty-args args)]
                       (when (non-blank? s)
                         (md-section "args"
                           (as-markdown-fence "edn" (str/trim s))))))
        result-block (when (and result (not (str/blank? (->str result))))
                       (md-section "result"
                         (if (string? result)
                           (str/trim result)
                           (as-markdown-fence "edn" (pr-str result)))))
        error-block (when error
                      (str (md-bold "Error: ") (md-code (preview (->str error) 240))))
        kind-line (when rendering-kind
                    (md-italic (str "rendering-kind: " (kw-name rendering-kind))))]
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
  (let [head (str (md-bold "PROVIDER ERROR")
               (when classification (str " / " (md-code (kw-name classification))))
               (when iteration (str " / iteration " iteration)))
        meta-bits (cond-> []
                    provider (conj (str "provider: " (md-code (kw-name provider))))
                    model (conj (str "model: " (md-code (str model))))
                    type (conj (str "type: " (md-code (kw-name type))))
                    reason (conj (str "reason: " (md-code (kw-name reason))))
                    received-type (conj (str "received: " (md-code (str received-type))))
                    status (conj (str "status: " (md-code (format-status status)))))
        meta-line (when (seq meta-bits) (str/join " / " meta-bits))
        msg (when (non-blank? message)
              (str (md-bold "Message:") " " (->str message)))
        raw (when (non-blank? raw-preview)
              (md-section "raw"
                (as-markdown-fence "text" (str/trim (str raw-preview)))))
        adv (when (non-blank? advice)
              (str (md-bold "Advice:") " " (->str advice)))]
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
        fence (md-code-block "mermaid" src)
        ascii (when (seq rendered-lines)
                (md-section (str "Mermaid diagram"
                              (when diagram-type
                                (str " (" (name diagram-type) ")")))
                  (md-code-block "text"
                    (str/join "\n" (mapv ->str rendered-lines)))))]
    (->> [fence ascii]
      (remove nil?)
      (str/join "\n\n"))))

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
      :vis.presentation/mermaid         (mermaid->md x))
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
