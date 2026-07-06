(ns com.blockether.vis.internal.foundation.transcript
  "Full session transcript - DATA first, presentation second.

   `transcript` returns one canonical Clojure map with every turn,
   every iteration, every executed block plus the LLM-side context
   (system prompt, message envelope, reasoning trace, top-level
   provider error, per-iteration vars, answer-form pointer,
   returned-empty-blocks flag) and the per-block forensic detail
   (code, comment, result, error, duration, timeout?, repaired?).
   Pure data. The agent can pattern-match on it; the CLI
   renders Markdown on top; a future TUI screen, JSON exporter, or
   analytics extension consumes the same shape.

   Lives in foundation because it's an introspection surface, not host
   plumbing. The sandbox-visible public surface is `(session-state)`
   for data and `(session-report-md)` for Markdown; this namespace
   owns the transcript portion behind that deeper interface.

   Public Clojure surface:

     `(transcript      db-info session-id)`  -> transcript data map
     `(transcript->md  data)`             -> Markdown string
     `(transcript-md   db-info session-id)`  -> DB lookup + Markdown string

   Canonical data shape:

     {:session {:id :title :channel :model :provider :created-at}
      :totals       {:turns N :iterations N
                     :tokens {:input :output :reasoning :cached}
                     :cost-usd D}
      :dialog      [{:role :turn-id :content}]
      :calls       [{:kind :ref :parent-ref :turn-id :iteration-id :op :tool
                     :var :code :status :duration-ms :command :target
                     :result :result-summary :info}]
      :timeline    [{:kind :ref :turn-id :iteration-id :content :code
                     :status :duration-ms :result-summary}]
      :turns
       [{:id :user-request :status :prior-outcome :provider :model
         :iteration-count :failure-count
         :tokens :cost-usd :answer
         :iterations
          [{:id :position :status :duration-ms
            :provider :model :thinking :error
            :tokens :cost-usd
            :answer-position :returned-empty-blocks?
            :vars
            [{:name :code :value :version}]
            :blocks
            [{:position :code :comment :result :error
              :duration-ms :timeout? :repaired?}]}]}]}

   The Markdown renderer renders thinking, iteration-level errors,
   vars, per-block forensic previews, and final answer text. Large
   fields are bounded so reports stay safe to open."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis])
  (:import
   [java.util Locale]))

;; =============================================================================
;; Data layer.
;; =============================================================================

(def ^:private encrypted-reasoning-placeholder
  "[provider returned encrypted reasoning; plaintext reasoning is unavailable]")

(defn- visible-thinking [thinking]
  (let [s (some-> thinking str)]
    (when-not (or (str/blank? (or s "")) (= encrypted-reasoning-placeholder s))
      s)))

(defn- iteration-rollup
  "Sum the per-iteration token + cost columns across a vec of
   iteration maps. Every value is numeric on the read side (the
   persistance layer defaults NULL columns to 0 / 0.0), so this is
   a clean reduce."
  [iterations]
  (reduce (fn [a it]
            (-> a
              ;; Phase B canonical iteration keys. `:input-tokens` is
              ;; TOTAL (Anthropic-additive raw values summed at the
              ;; canonical-normalizer boundary); details obey the
              ;; invariant on a per-row basis.
              (update-in [:tokens :input]         + (long   (or (:input-tokens it) 0)))
              (update-in [:tokens :output]        + (long   (or (:output-tokens it) 0)))
              (update-in [:tokens :reasoning]     + (long   (or (:output-reasoning-tokens it) 0)))
              (update-in [:tokens :cached]        + (long   (or (:input-cache-read-tokens it) 0)))
              (update-in [:tokens :cache-created] + (long   (or (:input-cache-write-tokens it) 0)))
              (update    :cost-usd                + (double (or (:cost-usd it) 0.0)))))
    {:tokens {:input 0 :output 0 :reasoning 0 :cached 0 :cache-created 0}
     :cost-usd 0.0}
    iterations))

(defn- form-envelope->block
  "Project one per-form envelope from `:forms` into the transcript's
   `:blocks` shape. Each envelope carries `:scope :tag :src :result :error`
   so the block surfaces all of them, plus a 0-based `:position` derived
   from the form's index in the iter's `:forms` vec."
  [position envelope]
  (cond-> {:position position
           :code (or (:src envelope) "")}
    (:scope envelope)              (assoc :scope (:scope envelope))
    (:tag envelope)                (assoc :tag (:tag envelope))
    (contains? envelope :result)   (assoc :result (:result envelope))
    (contains? envelope :error)    (assoc :error (:error envelope))))

(defn- enrich-iteration
  "Attach `:blocks` to one iteration row.

     `:blocks` - one entry per top-level form the iter executed,
                derived from the iter's `:forms` envelope vec on
                `session_turn_iteration.forms`. Cross-turn def
                rehydration is gone, so there is no separate `:vars`
                slice — every form lives on the iteration row.

   Degrades silently to `[]` so the renderer never throws on a
   partial DB."
  [_db-info iter]
  (let [forms  (or (:forms iter) [])
        blocks (vec (map-indexed form-envelope->block forms))]
    (-> iter
      (update :thinking visible-thinking)
      (assoc :blocks blocks)
      (assoc :failure-count (count (filter :error blocks))))))

(defn- build-turn
  "Pure projection: one session_turn_soul row + its iterations -> the
   turn-shaped data map the public `transcript` returns."
  [db-info turn]
  (let [raw-iters (try (vis/db-list-session-turn-iterations db-info (:id turn))
                    (catch Throwable _ []))
        iters     (mapv (partial enrich-iteration db-info) raw-iters)
        totals    (iteration-rollup iters)
        provider  (some #(some-> % :provider name) iters)
        model     (some :model iters)]
    (cond-> {:id              (:id turn)
             :position        (:position turn)
             :user-request    (or (:user-request turn) "")
             :status          (:status turn)
             :prior-outcome   (:prior-outcome turn)
             :iteration-count (count iters)
             :failure-count   (reduce + 0 (map :failure-count iters))
             :iterations      iters
             :tokens          (:tokens totals)
             :cost-usd        (:cost-usd totals)}
      provider                 (assoc :provider provider)
      model                    (assoc :model model)
      (:answer-markdown turn)  (assoc :answer (:answer-markdown turn)))))

(defn- session-totals
  "Sum tokens + cost + iteration counts across every turn."
  [turns]
  (reduce (fn [a t]
            (-> a
              (update :iterations + (long (or (:iteration-count t) 0)))
              (update-in [:tokens :input]     + (long   (or (:input     (:tokens t)) 0)))
              (update-in [:tokens :output]    + (long   (or (:output    (:tokens t)) 0)))
              (update-in [:tokens :reasoning]     + (long   (or (:reasoning     (:tokens t)) 0)))
              (update-in [:tokens :cached]        + (long   (or (:cached        (:tokens t)) 0)))
              (update-in [:tokens :cache-created] + (long   (or (:cache-created (:tokens t)) 0)))
              (update :cost-usd                   + (double (or (:cost-usd      t) 0.0)))))
    {:turns      (count turns)
     :iterations 0
     :tokens     {:input 0 :output 0 :reasoning 0 :cached 0 :cache-created 0}
     :cost-usd   0.0}
    turns))

(def ^:private transcript-known-channels
  ;; Channels scanned when resolving a session by short PREFIX (full UUIDs and
  ;; `db-resolve-session-id` hits don't need this). MUST include every channel
  ;; that persists sessions, or a prefix on a missing channel silently resolves
  ;; to nothing — e.g. `:api` (the gateway/web channel) must be included so a
  ;; web-session prefix resolves.
  [:tui :telegram :cli :api :web])

(defn- resolve-session-ref
  "Resolve one transcript/session reference to the canonical UUID.

   Accepted shapes:
     - UUID           => returned only when it exists
     - full UUID str  => parsed, then existence-checked
     - unique prefix  => scanned across every channel and expanded

   Returns nil on miss or ambiguous prefix. Unlike
   `db-resolve-session-id`, this helper is existence-aware - a
   well-formed but unknown UUID string must not masquerade as a real
   session."
  [db-info session-ref]
  (letfn [(existing-id [id]
            (when (and id (try (vis/db-get-session db-info id)
                            (catch Throwable _ nil)))
              id))]
    (cond
      (nil? session-ref) nil
      (uuid? session-ref) (existing-id session-ref)
      :else
      (let [s (str session-ref)]
        (or (existing-id (try (vis/db-resolve-session-id db-info s)
                           (catch Throwable _ nil)))
          (let [matches (->> transcript-known-channels
                          (mapcat #(or (vis/db-list-sessions db-info %) []))
                          (filter (fn [session]
                                    (str/starts-with? (str (:id session)) s)))
                          vec)]
            (when (= 1 (count matches))
              (:id (first matches)))))))))

(defn- preview-string
  [s n]
  (let [s (str s)]
    (if (<= (count s) n) s (str (subs s 0 n) "..."))))

(defn- preview-value
  [v n]
  (preview-string (pr-str v) n))

(defn- runtime-ref?
  [v]
  (and (map? v) (= :expr (:vis/ref v))))

(defn- op-slug
  [op]
  (let [s (cond
            (keyword? op) (if (namespace op)
                            (str (namespace op) "." (name op))
                            (name op))
            (symbol? op)  (if (namespace op)
                            (str (namespace op) "." (name op))
                            (name op))
            :else        (str op))]
    (-> s
      (str/replace #"/" ".")
      (str/replace #"[^A-Za-z0-9_.:-]" "-"))))

(defn- form-index
  [block]
  (or (:position block) (:idx block) (:id block) 0))

(defn- block-ref
  [turn iteration block]
  (or (get-in block [:envelope :ref])
    (:scope block)
    ;; Canonical model/CTX scope. Avoid `turn/<uuid-prefix>` refs: they
    ;; look like impossible turn numbers (`turn/75797678/...`).
    (str "t" (:position turn)
      "/i" (:position iteration)
      "/f" (inc (long (form-index block))))))

(defn- envelope-duration-ms
  [envelope]
  (when (and (map? envelope)
          (nat-int? (:started-at-ms envelope))
          (nat-int? (:finished-at-ms envelope)))
    (max 0 (- (long (:finished-at-ms envelope))
             (long (:started-at-ms envelope))))))

(defn- block-duration-ms
  [block]
  (or (envelope-duration-ms (:envelope block))
    (:duration-ms block)
    0))

(defn- tool-result-envelope?
  [value]
  (and (map? value)
    (contains? value :success?)
    (contains? value :info)))

(defn- result-summary
  "Bounded, data-first result preview for timeline/call rows. The full
   values remain available where they were persisted (`:blocks` and
   `:calls :result`); this summary makes the timeline useful without
   forcing callers to inspect provider/tool-specific payloads."

  [result]
  (cond
    (runtime-ref? result)
    {:type :runtime-ref :preview "<runtime value; see matching var/call row>"}

    (map? result)
    (cond-> {:type :map
             :keys (vec (take 16 (keys result)))
             :preview (preview-value result 400)}
      (contains? result :exit)            (assoc :exit (:exit result))
      (contains? result :timed-out?)      (assoc :timed-out? (:timed-out? result))
      (contains? result :command)         (assoc :command (:command result))
      (contains? result :duration-ms)     (assoc :duration-ms (:duration-ms result)))

    :else
    {:type (cond
             (nil? result) :nil
             (string? result) :string
             (keyword? result) :keyword
             (number? result) :number
             (coll? result) :collection
             :else :value)
     :preview (preview-value result 400)}))

(defn- event-status
  [error success? timeout?]
  (cond
    timeout? :timeout
    error :error
    (false? success?) :error
    :else :done))

(defn- tool-call-row
  [turn iteration block var-row envelope]
  (let [tool-meta       (or (:metadata envelope) (:info envelope))
        result          (if (contains? envelope :result) (:result envelope) (:result envelope))
        success?        (if (contains? envelope :success?) (:success? envelope) (:success? envelope))
        error           (if (contains? envelope :error) (:error envelope) (:error envelope))
        op              (or (:symbol envelope) (:op tool-meta) :tool)
        parent-ref      (when block (block-ref turn iteration block))
        ref             (when parent-ref (str parent-ref "/tool/" (op-slug op)))
        status          (event-status error success?
                          (or (:timed-out? result) (:timeout? tool-meta)))
        tool            (:tool tool-meta)]
    (cond-> {:kind           :tool-call
             :ref            ref
             :parent-ref     parent-ref
             :turn-id        (:id turn)
             :iteration-id   (:id iteration)
             :iteration      (:position iteration)
             :op             op
             :tool           (or (:symbol tool) (:call tool) tool)
             :status         status
             :success?       success?
             :duration-ms    (or (:duration-ms tool-meta)
                               (:duration-ms result)
                               0)
             :code           (:code block)
             :result         result
             :result-summary (result-summary result)
             :info           tool-meta}
      var-row              (assoc :var (:name var-row))
      (:command tool-meta) (assoc :command (:command tool-meta))
      (:command result)    (assoc :command (:command result))
      (:target tool-meta)  (assoc :target  (:target tool-meta))
      error                (assoc :error error))))

(defn- block-by-code
  [iteration]
  (reduce (fn [acc block]
            (if (contains? acc (:code block))
              acc
              (assoc acc (:code block) block)))
    {}
    (:blocks iteration)))

(defn- iteration-tool-calls
  [turn iteration]
  (let [_blocks-by-code (block-by-code iteration)
        direct-calls    (keep (fn [block]
                                (when (tool-result-envelope? (:result block))
                                  (tool-call-row turn iteration block nil (:result block))))
                          (:blocks iteration))
        {:keys [order rows]}
        (reduce (fn [{:keys [order rows] :as acc} call]
                  (let [dedupe-key (or (:ref call)
                                     [(:parent-ref call) (:op call) (:code call)])]
                    (if (contains? rows dedupe-key)
                      (assoc acc :rows (update rows dedupe-key merge call))
                      {:order (conj order dedupe-key)
                       :rows  (assoc rows dedupe-key call)})))
          {:order [] :rows {}}
          direct-calls)]
    (mapv rows order)))

(defn- transcript-calls
  [turns]
  (vec
    (mapcat (fn [turn]
              (mapcat #(iteration-tool-calls turn %) (:iterations turn)))
      turns)))

(defn- dialog-events
  [turns]
  (vec
    (mapcat (fn [turn]
              (cond-> [{:role :user
                        :turn-id (:id turn)
                        :content (:user-request turn)}]
                (:answer turn) (conj {:role :assistant
                                      :turn-id (:id turn)
                                      :content (:answer turn)})))
      turns)))

(defn- code-event
  [turn iteration block]
  (let [error (:error block)]
    (cond-> {:kind           :code
             :ref            (block-ref turn iteration block)
             :turn-id        (:id turn)
             :iteration-id   (:id iteration)
             :iteration      (:position iteration)
             :form-position  (inc (long (form-index block)))
             :role (:role block)
             :status         (event-status error true (:timeout? block))
             :duration-ms    (block-duration-ms block)
             :code           (:code block)}
      (contains? block :result) (assoc :result-summary (result-summary (:result block)))
      error                     (assoc :error error))))

(defn- transcript-timeline
  [turns calls]
  (let [calls-by-parent (group-by :parent-ref calls)]
    (vec
      (mapcat (fn [turn]
                (concat
                  [{:kind :user-message
                    :turn-id (:id turn)
                    :content (:user-request turn)}]
                  (mapcat (fn [iteration]
                            (mapcat (fn [block]
                                      (let [ref (block-ref turn iteration block)]
                                        (cons (code-event turn iteration block)
                                          (get calls-by-parent ref))))
                              (:blocks iteration)))
                    (:iterations turn))
                  (when (:answer turn)
                    [{:kind :assistant-message
                      :turn-id (:id turn)
                      :content (:answer turn)}])))
        turns))))

(defn transcript
  "Full session transcript as one Clojure data map. See ns
   docstring for the canonical shape. Returns nil when the
   session id does not resolve.

   `session-id` accepts either the canonical UUID or an
   unambiguous string prefix.

   Pure with respect to the database - no writes, no logging.
   `(:db-info env)` is the standard handle; the sandbox-bound symbol
   variant uses the live env automatically."
  [db-info session-id]
  (when-let [resolved-id (resolve-session-ref db-info session-id)]
    (when-let [session (try (vis/db-get-session db-info resolved-id)
                         (catch Throwable _ nil))]
      (let [turn-rows (try (vis/db-list-session-turns db-info resolved-id)
                        (catch Throwable _ []))
            turns     (mapv (partial build-turn db-info) turn-rows)
            totals    (session-totals turns)
            calls     (transcript-calls turns)]
        {:session     (cond-> {:id         resolved-id
                               :title      (:title session)
                               :channel    (:channel session)
                               :model      (:model session)
                               :created-at (:created-at session)}
                        (:provider session) (assoc :provider (:provider session)))
         :totals           totals
         :dialog           (dialog-events turns)
         :calls            calls
         :timeline         (transcript-timeline turns calls)
         :turns            turns}))))

;; =============================================================================
;; Markdown renderer. Pure transformation over `transcript`'s data
;; shape - no DB calls, no side effects.
;; =============================================================================

(defn- one-line
  [s]
  (-> (or s "") str (str/replace #"\s+" " ") str/trim))

(defn- format-cost-usd
  "Locale-stable USD formatter - always a dot separator (`$0.0042`),
   never a locale comma. nil collapses to `$0.0000` so callers don't
   `or`-pad."
  [c]
  (let [v (double (or c 0.0))]
    (String/format Locale/US "$%.4f" (object-array [v]))))

(defn- format-tokens
  [{:keys [input output reasoning cached cache-created]}]
  (let [base (str (long (or input 0)) "/" (long (or output 0)))
        suff (cond-> []
               (and reasoning (pos? (long reasoning)))
               (conj (str "r=" reasoning))
               (and cached (pos? (long cached)))
               (conj (str "c=" cached))
               (and cache-created (pos? (long cache-created)))
               (conj (str "w=" cache-created)))]
    (if (seq suff)
      (str base " (" (str/join ", " suff) ")")
      base)))

(defn- render-fenced
  [lang body]
  (let [s (str body)]
    (if (str/blank? s)
      ""
      (str "```" (or lang "") "\n" s "\n```\n"))))

(defn- display-result [result]
  (if (and (map? result) (= :expr (:vis/ref result)))
    "<runtime value; re-evaluate expression to restore>"
    (pr-str result)))

(defn- render-block-code-segments
  [code render-segments]
  (if (seq render-segments)
    (let [body (apply str
                 (keep (fn [{:keys [kind source value]}]
                         (case kind
                           :code (when-not (str/blank? (str source))
                                   (render-fenced "clojure" source))
                           :title (str "_session title:_ `" (or value "") "`\n")
                           :answer-ref nil
                           nil))
                   render-segments))]
      (when-not (str/blank? body) body))
    (render-fenced "clojure" code)))

(defn- render-block-section
  "Per-block forensic dump: status header, optional comment, full code
   in a fenced ```clojure block, result line, fenced error. `answer?`
   flips on the block the iteration's `:answer-position` points at —
   the block that called `done(...)` — so the reader spots the
   terminal block at a glance.

   Verbatim: result strings, error blobs, and code segments are
   rendered without truncation. Forensic reports are useless when the
   first place you look has been clipped."
  [idx answer? {:keys [code comment render-segments result error] :as block}]
  (let [marker      (if error "✗" "✓")
        flags       (cond-> []
                      answer?            (conj "answer")
                      (:timeout? block)  (conj "timeout")
                      (:repaired? block) (conj "repaired")
                      error              (conj "error"))
        suffix      (if (seq flags) (str " [" (str/join ", " flags) "]") "")
        has-result? (and (not error) (contains? block :result))]
    (str "##### Block " idx " - " marker " "
      (long (block-duration-ms block)) "ms" suffix "\n"
      (when (not (str/blank? comment)) (str comment "\n"))
      (render-block-code-segments code render-segments)
      (when has-result?
        (str "\nResult: `" (display-result result) "`\n"))
      (when error
        (str "\n_error:_\n" (render-fenced "text" error)))
      "\n")))

(defn- render-thinking
  "Render the LLM's reasoning trace as a fenced text block. The
   transcript carries the reasoning verbatim — no truncation — so the
   reader has a forensic record of what the model thought before it
   acted."
  [thinking]
  (when (not (str/blank? thinking))
    (str "_thinking:_\n"
      (render-fenced "text" thinking)
      "\n")))

(defn- render-iter-error
  "Render an iteration-level error - the provider call failed before
   any block could run. Persisted on `iteration.llm_error` as JSON.
   nil/blank -> nothing emitted."
  [error]
  (when (and error (not (str/blank? (str error))))
    (str "_iteration error:_\n"
      (render-fenced "text" (str error))
      "\n")))

(defn- render-iteration-section [iter]
  (let [pos     (:position iter)
        status  (or (some-> (:status iter) name) "-")
        dur     (or (:duration-ms iter) 0)
        in      (or (:input-tokens iter) 0)
        out     (or (:output-tokens iter) 0)
        cost    (or (:cost-usd iter) 0.0)
        blocks  (:blocks iter)
        ;; Index of the block that called `done(...)`. nil for
        ;; non-terminal iterations - the marker only fires on the
        ;; right block.
        ans-idx (:answer-position iter)]
    (str "\n#### Iteration " pos " - " status
      " (" in "/" out " tokens, " (format-cost-usd cost) ", " (long dur) "ms)\n\n"
      (render-thinking (:thinking iter))
      (render-iter-error (:error iter))
      (cond
        (empty? blocks)
        "_No code blocks (LLM returned an empty response)._\n"

        :else
        (apply str (map-indexed (fn [idx block]
                                  (render-block-section idx (= idx ans-idx) block))
                     blocks))))))

(defn- render-final-answer
  "Final answer text the turn settled on, persisted in the
   `session_turn_state.answer_markdown` TEXT column. The model wrote
   raw Markdown via `done(...)`; the transcript echoes the
   source verbatim. nil/blank -> nothing emitted."
  [answer]
  (when (not (str/blank? (str answer)))
    (str "\n#### Final answer\n\n" answer "\n")))

(defn- render-turn-block
  [{:keys [position user-request status prior-outcome provider model
           iteration-count failure-count
           iterations tokens cost-usd answer]}]
  ;; Render `position` (int), never `:id` (uuid). UUID stays in
  ;; introspection responses for programmatic callers,
  ;; never in user/LLM-facing surfaces.
  (str
    "### Turn " (or position "?") "\n"
    "- **User request:** " (one-line user-request) "\n"
    "- **Status:** " (or (some-> status name) "-")
    (when prior-outcome (str " (" (name prior-outcome) ")")) "\n"
    "- **Provider/model:** "
    (or (cond
          (and provider model) (str provider "/" model)
          model                model
          provider             provider)
      "-") "\n"
    "- **Iterations:** " iteration-count "\n"
    "- **Failures:** " failure-count "\n"
    "- **Tokens (in/out):** " (format-tokens tokens) "\n"
    "- **Cost:** " (format-cost-usd cost-usd) "\n"
    (apply str (map render-iteration-section iterations))
    (render-final-answer answer)
    "\n"))

(defn- render-header [{:keys [session totals]}]
  (str
    "# Diagnostic report" (when-let [t (:title session)] (str " - " t)) "\n"
    "\n"
    "- **Title:** "    (or (:title    session) "-") "\n"
    "- **Channel:** "  (or (some-> (:channel session) name) "-") "\n"
    "- **Model:** "    (or (:model    session) "-") "\n"
    "- **Created:** "  (or (:created-at session) "-") "\n"
    "- **Total turns:** "      (:turns totals) "\n"
    "- **Total iterations:** " (:iterations totals) "\n"
    "- **Total cost (USD):** " (format-cost-usd (:cost-usd totals)) "\n"
    "- **Total tokens (in/out):** " (format-tokens (:tokens totals)) "\n"
    "\n"))

(defn- render-dialog-message
  [{:keys [role content]}]
  (str "### " (case role
                :user "User"
                :assistant "Assistant"
                (name role))
    "\n\n"
    (render-fenced "markdown" content)
    "\n"))

(defn- render-dialog-md
  [{:keys [session dialog]}]
  (str "# Dialog" (when-let [t (:title session)] (str " - " t)) "\n\n"
    (if (seq dialog)
      (apply str (map render-dialog-message dialog))
      "_No dialog messages._\n")))

(defn- render-full-md
  [data]
  (str (render-header data)
    "## Turn-by-turn breakdown\n\n"
    (apply str (map render-turn-block (:turns data)))))

(defn transcript->md
  "Render transcript data as Markdown. Pure transformation over
   `transcript`'s canonical data shape. Returns a string.

   Modes:
   - `:full`               - bounded diagnostic report (default).
   - `:dialog`             - user/assistant dialog only."
  ([data]
   (transcript->md data {:mode :full}))
  ([data {:keys [mode] :or {mode :full}}]
   (case mode
     :dialog         (render-dialog-md data)
     :full           (render-full-md data)
     (render-full-md data))))

(defn transcript-md
  "Render the session as Markdown. Single transformation over
   `transcript`'s data. Returns a string; returns
   `\"Session not found: <id>\\n\"` (no throw) on a missing id so
   shell pipelines stay clean."
  ([db-info session-id]
   (transcript-md db-info session-id {:mode :full}))
  ([db-info session-id opts]
   (if-let [data (transcript db-info session-id)]
     (transcript->md data opts)
     (str "Session not found: " session-id "\n"))))
