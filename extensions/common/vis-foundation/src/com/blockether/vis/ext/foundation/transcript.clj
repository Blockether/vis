(ns com.blockether.vis.ext.foundation.transcript
  "Full conversation transcript - DATA first, presentation second.

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
   plumbing. The sandbox-visible public surface is `(v/conversation-state)`
   for data and `(v/conversation-report)` for Markdown; this namespace
   owns the transcript portion behind that deeper interface.

   Public Clojure surface:

     `(transcript      db-info conv-id)`  -> transcript data map
     `(transcript->md  data)`             -> Markdown string
     `(transcript-md   db-info conv-id)`  -> DB lookup + Markdown string

     `cli-command` mounts `vis extensions reproduction <CONVERSATION-ID>`
     through `:ext/cli`, keeping extension-owned commands under
     `vis extensions`.

   Canonical data shape:

     {:conversation {:id :title :channel :model :provider :created-at}
      :totals       {:turns N :iterations N
                     :tokens {:input :output :reasoning :cached}
                     :cost-usd D}
      :dialog      [{:role :turn-id :content}]
      :calls       [{:kind :ref :parent-ref :turn-id :iteration-id :op :tool
                     :var :code :status :duration-ms :command :target
                     :result :result-summary :info}]
      :timeline    [{:kind :ref :turn-id :iteration-id :content :code
                     :status :duration-ms :result-summary}]
      :llm-diagnostics [{:turn-id :iteration-id :raw-response}]
      :turns
       [{:id :user-request :status :prior-outcome :provider :model
         :iteration-count :failure-count
         :tokens :cost-usd :answer
         :iterations
          [{:id :position :status :duration-ms
            :provider :model :thinking :error
            :tokens :cost-usd
            :answer-position :returned-empty-blocks?
            :llm-system-prompt :llm-user-prompt
            :llm-raw-response :llm-raw-response-preview :llm-raw-response-length
            :llm-raw-response-sha256
            :llm-executable-blocks
            :metadata
            :vars
            [{:name :code :value :version}]
            :blocks
            [{:position :code :comment :result :error
              :duration-ms :timeout? :repaired?}]}]}]}

   The Markdown renderer renders thinking, iteration-level errors,
   vars, per-block forensic previews, final answer text, plus compact
   raw-response diagnostics. Prompt bodies render only in prompt/debug
   modes. Large fields are bounded so reports stay safe to open."
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
              (update-in [:tokens :input]     + (long   (or (:input-tokens it) 0)))
              (update-in [:tokens :output]    + (long   (or (:output-tokens it) 0)))
              (update-in [:tokens :reasoning] + (long   (or (:reasoning-tokens it) 0)))
              (update-in [:tokens :cached]    + (long   (or (:cached-tokens it) 0)))
              (update    :cost-usd            + (double (or (:cost-usd it) 0.0)))))
    {:tokens {:input 0 :output 0 :reasoning 0 :cached 0}
     :cost-usd 0.0}
    iterations))

(defn- enrich-iteration
  "Attach `:blocks` and `:vars` to one iteration row.

     `:blocks` - every executed form (Nippy-encoded inline log).
     `:vars`   - every `(def ...)` this iteration produced; reads from
                the separate `definition_soul` / `definition_state`
                facade. Each entry is `{:name :code :value :version}`.

   Both reads degrade silently to `[]` so the renderer never
   throws on a partial DB."
  [db-info iter]
  (let [block (cond-> {:position 0
                       :code (or (:code iter) "")}
                (contains? iter :result) (assoc :result (:result iter))
                (contains? iter :error) (assoc :error (:error iter))
                (contains? iter :duration-ms)
                (assoc :duration-ms (:duration-ms iter)))
        blocks [block]
        vars   (try (vec (vis/db-list-iteration-vars db-info (:id iter)))
                 (catch Throwable _ []))]
    (-> iter
      (update :thinking visible-thinking)
      (assoc :blocks blocks)
      (assoc :vars   vars)
      (assoc :failure-count (count (filter :error blocks))))))

(defn- build-turn
  "Pure projection: one conversation_turn_soul row + its iterations -> the
   turn-shaped data map the public `transcript` returns."
  [db-info turn]
  (let [raw-iters (try (vis/db-list-conversation-turn-iterations db-info (:id turn))
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
      provider       (assoc :provider provider)
      model          (assoc :model model)
      (:answer turn) (assoc :answer (:answer turn)))))

(defn- conversation-totals
  "Sum tokens + cost + iteration counts across every turn."
  [turns]
  (reduce (fn [a t]
            (-> a
              (update :iterations + (long (or (:iteration-count t) 0)))
              (update-in [:tokens :input]     + (long   (or (:input     (:tokens t)) 0)))
              (update-in [:tokens :output]    + (long   (or (:output    (:tokens t)) 0)))
              (update-in [:tokens :reasoning] + (long   (or (:reasoning (:tokens t)) 0)))
              (update-in [:tokens :cached]    + (long   (or (:cached    (:tokens t)) 0)))
              (update :cost-usd               + (double (or (:cost-usd  t) 0.0)))))
    {:turns      (count turns)
     :iterations 0
     :tokens     {:input 0 :output 0 :reasoning 0 :cached 0}
     :cost-usd   0.0}
    turns))

(def ^:private transcript-known-channels
  [:tui :telegram :cli])

(defn- resolve-conversation-ref
  "Resolve one transcript/conversation reference to the canonical UUID.

   Accepted shapes:
     - UUID           => returned only when it exists
     - full UUID str  => parsed, then existence-checked
     - unique prefix  => scanned across every channel and expanded

   Returns nil on miss or ambiguous prefix. Unlike
   `db-resolve-conversation-id`, this helper is existence-aware - a
   well-formed but unknown UUID string must not masquerade as a real
   conversation."
  [db-info conversation-ref]
  (letfn [(existing-id [id]
            (when (and id (try (vis/db-get-conversation db-info id)
                            (catch Throwable _ nil)))
              id))]
    (cond
      (nil? conversation-ref) nil
      (uuid? conversation-ref) (existing-id conversation-ref)
      :else
      (let [s (str conversation-ref)]
        (or (existing-id (try (vis/db-resolve-conversation-id db-info s)
                           (catch Throwable _ nil)))
          (let [matches (->> transcript-known-channels
                          (mapcat #(or (vis/db-list-conversations db-info %) []))
                          (filter (fn [conversation]
                                    (str/starts-with? (str (:id conversation)) s)))
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

(defn- block-index
  [block]
  (or (:position block) (:idx block) (:id block) 0))

(defn- block-ref
  [turn iteration block]
  (or (get-in block [:envelope :ref])
    (str "turn/" (subs (str (:id turn)) 0 8)
      "/iteration/" (:position iteration)
      "/block/" (inc (long (block-index block))))))

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
   values remain available where they were persisted (`:blocks`,
   `:vars`, and `:calls :result`); this summary makes the timeline
   useful without forcing callers to inspect provider/tool-specific
   payloads."
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
        op              (or (:symbol envelope) (:op tool-meta) :v/tool)
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
  (let [blocks-by-code (block-by-code iteration)
        direct-calls   (keep (fn [block]
                               (when (tool-result-envelope? (:result block))
                                 (tool-call-row turn iteration block nil (:result block))))
                         (:blocks iteration))
        var-calls      (keep (fn [var-row]
                               (when (tool-result-envelope? (:value var-row))
                                 (tool-call-row turn iteration
                                   (get blocks-by-code (:code var-row))
                                   var-row
                                   (:value var-row))))
                         (:vars iteration))
        {:keys [order rows]}
        (reduce (fn [{:keys [order rows] :as acc} call]
                  (let [dedupe-key (or (:ref call)
                                     [(:parent-ref call) (:op call) (:code call)])]
                    (if (contains? rows dedupe-key)
                      (assoc acc :rows (update rows dedupe-key merge call))
                      {:order (conj order dedupe-key)
                       :rows  (assoc rows dedupe-key call)})))
          {:order [] :rows {}}
          (concat direct-calls var-calls))]
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
             :form-position  (inc (long (block-index block)))
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

(defn- raw-response-map
  [iteration]
  (let [blocks (vec (:llm-executable-blocks iteration))]
    (cond-> {}
      (some? (:llm-raw-response-preview iteration))
      (assoc :preview (:llm-raw-response-preview iteration))
      (some? (:llm-raw-response-length iteration))
      (assoc :length (:llm-raw-response-length iteration))
      (some? (:llm-raw-response-sha256 iteration))
      (assoc :sha256 (:llm-raw-response-sha256 iteration))
      (seq blocks)
      (assoc :executable-blocks blocks
        :block-count (count blocks)
        :block-langs (mapv :lang blocks)))))

(defn- llm-diagnostic-row
  [turn iteration]
  (let [raw-response (raw-response-map iteration)]
    (when (seq raw-response)
      (cond-> {:turn-id      (:id turn)
               :user-request (:user-request turn)
               :iteration-id (:id iteration)
               :iteration    (:position iteration)
               :status       (:status iteration)
               :raw-response raw-response}
        (:provider iteration) (assoc :provider (:provider iteration))
        (:model iteration)    (assoc :model (:model iteration))))))

(defn- transcript-llm-diagnostics
  [turns]
  (vec
    (mapcat (fn [turn]
              (keep #(llm-diagnostic-row turn %) (:iterations turn)))
      turns)))

(defn transcript
  "Full conversation transcript as one Clojure data map. See ns
   docstring for the canonical shape. Returns nil when the
   conversation id does not resolve.

   `conversation-id` accepts either the canonical UUID or an
   unambiguous string prefix.

   Pure with respect to the database - no writes, no logging.
   `(:db-info env)` is the standard handle; the SCI-bound symbol
   variant uses the live env automatically."
  [db-info conversation-id]
  (when-let [resolved-id (resolve-conversation-ref db-info conversation-id)]
    (when-let [conv (try (vis/db-get-conversation db-info resolved-id)
                      (catch Throwable _ nil))]
      (let [turn-rows (try (vis/db-list-conversation-turns db-info resolved-id)
                        (catch Throwable _ []))
            turns     (mapv (partial build-turn db-info) turn-rows)
            totals    (conversation-totals turns)
            calls     (transcript-calls turns)]
        {:conversation     (cond-> {:id         resolved-id
                                    :title      (:title conv)
                                    :channel    (:channel conv)
                                    :model      (:model conv)
                                    :created-at (:created-at conv)}
                             (:provider conv) (assoc :provider (:provider conv)))
         :totals           totals
         :dialog           (dialog-events turns)
         :calls            calls
         :timeline         (transcript-timeline turns calls)
         :llm-diagnostics  (transcript-llm-diagnostics turns)
         :turns            turns}))))

;; =============================================================================
;; Markdown renderer. Pure transformation over `transcript`'s data
;; shape - no DB calls, no side effects.
;; =============================================================================

(def ^:private markdown-code-preview-chars 20000)
(def ^:private markdown-raw-preview-chars 1200)
(def ^:private markdown-executable-blocks-preview-chars 4000)
(def ^:private markdown-answer-preview-chars 12000)

(defn- truncate
  [s n]
  (let [s (str s)]
    (if (<= (count s) n) s (str (subs s 0 n) "..."))))

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
  [{:keys [input output reasoning cached]}]
  (let [base (str (long (or input 0)) "/" (long (or output 0)))
        suff (cond-> []
               (and reasoning (pos? (long reasoning)))
               (conj (str "r=" reasoning))
               (and cached (pos? (long cached)))
               (conj (str "c=" cached)))]
    (if (seq suff)
      (str base " (" (str/join ", " suff) ")")
      base)))

(defn- render-fenced
  [lang body]
  (let [s (str body)]
    (if (str/blank? s)
      ""
      (str "```" (or lang "") "\n" s "\n```\n"))))

(defn- render-collapsible
  "GitHub-flavored Markdown `<details>` block. Default-collapsed so
   the file stays scannable; users (and viewers like mdBook / VS Code
   / GitHub) expand on demand. The summary line shows what's inside +
   its size so the reader knows whether to click. Empty `body` yields
   the empty string so the block disappears entirely."
  [summary body]
  (let [s (str body)]
    (if (str/blank? s)
      ""
      (str "<details><summary>" summary "</summary>\n\n"
        s
        "</details>\n\n"))))

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
                                   (render-fenced "clojure"
                                     (truncate source markdown-code-preview-chars)))
                           :title (str "_conversation title:_ `" (or value "") "`\n")
                           :answer-ref nil
                           nil))
                   render-segments))]
      (when-not (str/blank? body) body))
    (render-fenced "clojure" (truncate code markdown-code-preview-chars))))

(defn- render-block-section
  "Per-block forensic dump: status header, optional comment, full code
   in a fenced ```clojure block, result line, fenced error. `answer?`
   flips on the block the iteration's `:answer-position` points at -
   the block that called `(done ...)` - so the reader spots the
   terminal block at a glance.

   Result truncation cap is 800 chars on the display string so the
   report is forensic, not a one-pager."
  [idx answer? {:keys [code comment render-segments result error] :as block}]
  (let [marker      (if error "✗" "✓")
        flags       (cond-> []
                      answer?            (conj "answer")
                      (:timeout? block)  (conj "timeout")
                      (:repaired? block) (conj "repaired")
                      error              (conj "error"))
        suffix      (if (seq flags) (str " [" (str/join ", " flags) "]") "")
        has-result? (and (not error) (contains? block :result) (not= :vis/answer result))]
    (str "##### Block " idx " - " marker " "
      (long (block-duration-ms block)) "ms" suffix "\n"
      (when (not (str/blank? comment)) (str comment "\n"))
      (render-block-code-segments code render-segments)
      (when has-result?
        (str "\nResult: `" (truncate (display-result result) 800) "`\n"))
      (when error
        (str "\n_error:_\n" (render-fenced "text" error)))
      "\n")))

(defn- render-thinking
  "Render the LLM's reasoning trace as a fenced text block. Many
   models stream multi-KB reasoning per iteration; preserved
   verbatim. Hard cap at 16 KB so a runaway thinking trace doesn't
   blow up the file."
  [thinking]
  (when (not (str/blank? thinking))
    (str "_thinking:_\n"
      (render-fenced "text" (truncate thinking 16384))
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

(defn- render-vars
  "Compact list of `(def ...)` rows produced by this iteration. One
   bullet per var with truncated code preview + truncated value
   pr-str. Empty / nil -> nothing emitted."
  [vars]
  (when (seq vars)
    (str "_vars defined this iteration:_\n\n"
      (str/join "\n"
        (map (fn [{:keys [name code value version]}]
               (str "- `" name "`"
                 (when version (str " (v" version ")"))
                 (when (and code (not (str/blank? code)))
                   (str " - `" (truncate (one-line code) 80) "`"))
                 (when (some? value)
                   (str " -> `" (truncate (pr-str value) 80) "`"))))
          vars))
      "\n\n")))

(defn- render-system-prompt
  "Collapsible `<details>` block carrying the full assembled system
   prompt for this iteration. `prev-system-prompt` is the system
   prompt of the previous iteration in the same turn (nil on the
   first iteration); when both match we render a tiny
   \"unchanged from iteration N-1\" stub so a 16-iteration turn
   doesn't carry 16 identical 8KB system-prompt copies inline."
  [system-prompt prev-system-prompt prev-pos]
  (when (not (str/blank? system-prompt))
    (let [size  (count system-prompt)
          same? (and prev-system-prompt (= system-prompt prev-system-prompt))]
      (if same?
        (render-collapsible
          (str "System prompt (" size " chars, unchanged from iteration " prev-pos ")")
          "_(identical to the previous iteration's system prompt)_\n")
        (render-collapsible
          (str "System prompt (" size " chars)")
          (render-fenced "text" system-prompt))))))

(defn- prompt-message-purpose
  [role content]
  (let [role (str role)
        content (str content)]
    (cond
      (= "system" role) "stable system prompt"
      (and (= "user" role)
        (or (str/includes? content ";; ctx =")
          (str/includes? content "<current_turn_context>"))) "per-iteration trailer"
      (and (= "user" role)
        (or (str/includes? content ";; -- CURRENT-USER-MESSAGE --")
          (str/includes? content "<current_user_message>"))) "current user message"
      (= "assistant" role) "assistant optional replay"
      :else nil)))

(defn- render-prompt-message
  [idx {:keys [role content]}]
  (let [role (or role "?")
        purpose (prompt-message-purpose role content)]
    (str "[" idx "] role=" role
      (when purpose (str " - " purpose))
      " (" (count (str content)) " chars):\n"
      (render-fenced "text" content)
      "\n")))

(defn- render-raw-provider-messages
  [messages]
  (str "RAW provider messages sent to LLM (exact persisted vector):\n"
    (render-fenced "clojure" (pr-str (vec messages)))
    "\n"))

(defn- render-llm-messages
  "Collapsible `<details>` block carrying the full LLM message
   envelope for this iteration: the exact persisted vector passed to
   the provider, followed by an indexed readability view. Empty / nil
   envelope -> no output."
  [messages]
  (when (seq messages)
    (let [body (str (render-raw-provider-messages messages)
                 "Indexed view:\n\n"
                 (apply str (map-indexed render-prompt-message messages)))]
      (render-collapsible
        (str "LLM messages (" (count messages) " messages, "
          (reduce + 0 (map #(count (str (:content %))) messages)) " chars total)")
        body))))

(defn- raw-diagnostic-rows
  "Flatten transcript turns/iterations into compact raw-response
   diagnostic rows. Rows exist only when the iteration has persisted
   raw-response or executable-block forensic data. This is presentation
   support for `v/conversation-report`; `v/conversation-state` builds its public convenience
   view from the same underlying transcript fields."
  [turns]
  (vec
    (mapcat (fn [turn]
              (keep (fn [iter]
                      (when (or (some? (:llm-raw-response-preview iter))
                              (some? (:llm-raw-response-length iter))
                              (some? (:llm-raw-response-sha256 iter))
                              (seq (:llm-executable-blocks iter)))
                        (let [executable-blocks (vec (:llm-executable-blocks iter))]
                          {:turn-id           (:id turn)
                           :turn-position     (:position turn)
                           :iteration         (:position iter)
                           :status            (:status iter)
                           :raw-preview       (:llm-raw-response-preview iter)
                           :raw-length        (:llm-raw-response-length iter)
                           :raw-sha256        (:llm-raw-response-sha256 iter)
                           :executable-blocks executable-blocks
                           :block-count       (count executable-blocks)
                           :block-langs       (mapv :lang executable-blocks)})))
                (:iterations turn)))
      turns)))

(defn- sha-prefix
  [s]
  (when (not (str/blank? (str s)))
    (truncate s 12)))

(defn- render-raw-diagnostic-row
  [{:keys [turn-position iteration status raw-length raw-sha256 block-count block-langs]}]
  (str "| " (or turn-position "?") " | " iteration " | " (or (some-> status name) "-")
    " | " (or raw-length "-")
    " | `" (or (sha-prefix raw-sha256) "-") "`"
    " | " (or block-count "-")
    " | " (if (seq block-langs) (str/join ", " block-langs) "-")
    " |\n"))

(defn- render-raw-diagnostic-details
  [{:keys [turn-position iteration raw-preview raw-length executable-blocks]}]
  (render-collapsible
    (str "Raw LLM response preview for turn " (or turn-position "?")
      " / iteration " iteration
      (when raw-length (str " (" raw-length " chars total)")))
    (str
      (render-fenced "text" (truncate raw-preview markdown-raw-preview-chars))
      (when (seq executable-blocks)
        (str "\n\nExecutable Markdown code blocks selected by svar:\n\n"
          (render-fenced "clojure"
            (truncate (pr-str executable-blocks)
              markdown-executable-blocks-preview-chars)))))))

(defn- render-raw-diagnostics
  "Compact raw LLM response diagnostics for the whole report. The
   table is always small; bounded previews live in collapsed details
   blocks so `v/conversation-report` answers the first diagnostic question without
   forcing users into logs or SQLite."
  [turns]
  (let [rows (raw-diagnostic-rows turns)]
    (when (seq rows)
      (str "## Raw LLM response diagnostics\n\n"
        "| Turn | Iter | Status | Raw chars | SHA-256 | Blocks | Langs |\n"
        "|---|---:|---|---:|---|---:|---|\n"
        (apply str (map render-raw-diagnostic-row rows))
        "\n"
        (apply str (keep render-raw-diagnostic-details rows))))))

(defn prompt-snapshots
  "Flatten persisted provider prompt state from transcript data.

   This is the DB-backed forensic surface for prompt investigation:
   each row corresponds to one iteration and carries the assembled
   system prompt plus the exact provider message envelope persisted on
   the iteration row. The last user message in `:messages` is the
   per-iteration trailer/bindings snapshot the model saw."
  [data]
  (vec
    (mapcat (fn [turn]
              (map (fn [iter]
                     {:turn-id       (:id turn)
                      :iteration-id  (:id iter)
                      :iteration     (:position iter)
                      :status        (:status iter)
                      :provider      (:provider iter)
                      :model         (:model iter)
                      :system-prompt (:llm-system-prompt iter)
                      :messages      (vec (:llm-user-prompt iter))})
                (:iterations turn)))
      (:turns data))))

(defn- prompt-report-heading
  [data report-title]
  ;; Per PLAN §2.9 + §5.1: header NEVER prints the conversation UUID.
  ;; Title is the user-facing identifier; UUID stays programmatic-only
  ;; (introspection callers read `:id` directly from the data map).
  (let [{:keys [channel provider model created-at]} (:conversation data)
        conv-title (get-in data [:conversation :title])]
    (str "# " report-title (when conv-title (str " - " conv-title)) "\n\n"
      "- **Channel:** " (or (some-> channel name) "-") "\n"
      "- **Provider/model:** " (or (some-> provider name) "-") "/" (or model "-") "\n"
      "- **Created:** " (or created-at "-") "\n\n")))

(defn- render-prompt-row-table
  [rows]
  (str "| Turn | Iter | Status | Provider/model | System chars | Messages | Message chars |\n"
    "|---|---:|---|---|---:|---:|---:|\n"
    (apply str
      (map (fn [{:keys [turn-id iteration status provider model system-prompt messages]}]
             (str "| `" turn-id "` | " iteration " | " (or (some-> status name) "-")
               " | " (or (some-> provider name) "-") "/" (or model "-")
               " | " (count (str system-prompt))
               " | " (count messages)
               " | " (reduce + 0 (map #(count (str (:content %))) messages))
               " |\n"))
        rows))
    "\n"))

(defn- render-system-prompt-snapshot-row
  [prev-system-prompt {:keys [turn-id iteration system-prompt]}]
  (let [same? (and prev-system-prompt (= prev-system-prompt system-prompt))]
    (if same?
      (render-collapsible
        (str "System prompt turn " turn-id " / iteration " iteration
          " (" (count (str system-prompt)) " chars, unchanged)")
        "_(identical to the previous prompt snapshot)_\n")
      (render-collapsible
        (str "System prompt turn " turn-id " / iteration " iteration
          " (" (count (str system-prompt)) " chars)")
        (render-fenced "text" system-prompt)))))

(defn- render-system-prompts-report
  [data]
  (let [rows (prompt-snapshots data)]
    (str (prompt-report-heading data "System prompt snapshots")
      (render-prompt-row-table rows)
      (first
        (reduce (fn [[acc prev] row]
                  [(str acc (render-system-prompt-snapshot-row prev row))
                   (:system-prompt row)])
          ["" nil]
          rows)))))

(defn- render-provider-prompt-snapshot-row
  [{:keys [turn-id iteration system-prompt messages]}]
  (render-collapsible
    (str "Provider prompt turn " turn-id " / iteration " iteration
      " (system " (count (str system-prompt)) " chars, "
      (count messages) " messages)")
    (str "_system prompt snapshot:_\n"
      (render-fenced "text" system-prompt)
      "\n_full provider message envelope:_\n\n"
      (render-raw-provider-messages messages)
      "Indexed view:\n\n"
      (apply str (map-indexed render-prompt-message messages)))))

(defn- render-provider-prompts-report
  [data]
  (let [rows (prompt-snapshots data)]
    (str (prompt-report-heading data "Provider prompt snapshots")
      (render-prompt-row-table rows)
      (apply str (map render-provider-prompt-snapshot-row rows)))))

(defn- render-iteration-section [include-prompts? iter prev-iter]
  (let [pos     (:position iter)
        status  (or (some-> (:status iter) name) "-")
        dur     (or (:duration-ms iter) 0)
        in      (or (:input-tokens iter) 0)
        out     (or (:output-tokens iter) 0)
        cost    (or (:cost-usd iter) 0.0)
        blocks  (:blocks iter)
        ;; Index of the block that called `(done ...)`. nil for
        ;; non-terminal iterations - the marker only fires on the
        ;; right block.
        ans-idx (:answer-position iter)]
    (str "\n#### Iteration " pos " - " status
      " (" in "/" out " tokens, " (format-cost-usd cost) ", " (long dur) "ms)\n\n"
      (render-thinking (:thinking iter))
      (render-iter-error (:error iter))
      (when include-prompts?
        (str
          (render-system-prompt (:llm-system-prompt iter)
            (:llm-system-prompt prev-iter)
            (:position prev-iter))
          (render-llm-messages (:llm-user-prompt iter))))
      (render-vars (:vars iter))
      (cond
        (empty? blocks)
        "_No code blocks (LLM returned an empty response)._\n"

        :else
        (apply str (map-indexed (fn [idx block]
                                  (render-block-section idx (= idx ans-idx) block))
                     blocks))))))

(defn- render-final-answer
  "Final answer text the turn settled on, persisted on
   `conversation_turn_state.metadata.answer`. Rendered after every iteration so
   the reader sees the trajectory that led to it. nil/blank -> nothing
   emitted."
  [answer]
  (when (not (str/blank? (str answer)))
    (str "\n#### Final answer\n\n"
      (render-fenced "text" (truncate answer markdown-answer-preview-chars)))))

(defn- render-turn-block
  [include-prompts?
   {:keys [position user-request status prior-outcome provider model
           iteration-count failure-count
           iterations tokens cost-usd answer]}]
  ;; Per PLAN §2.9 + §5.1: render `position` (int), never `:id` (uuid).
  ;; UUID stays in introspection responses for programmatic callers,
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
    ;; Pair each iteration with the previous one in the same turn so
    ;; prompt-debug mode can dedupe an unchanged system prompt instead
    ;; of emitting the same block 16 times.
    (apply str (map (partial render-iteration-section include-prompts?)
                 iterations
                 (cons nil iterations)))
    (render-final-answer answer)
    "\n"))

(defn- render-header [{:keys [conversation totals]}]
  (str
    "# Diagnostic report" (when-let [t (:title conversation)] (str " - " t)) "\n"
    "\n"
    "- **Title:** "    (or (:title    conversation) "-") "\n"
    "- **Channel:** "  (or (some-> (:channel conversation) name) "-") "\n"
    "- **Model:** "    (or (:model    conversation) "-") "\n"
    "- **Created:** "  (or (:created-at conversation) "-") "\n"
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
  [{:keys [conversation dialog]}]
  (str "# Dialog" (when-let [t (:title conversation)] (str " - " t)) "\n\n"
    (if (seq dialog)
      (apply str (map render-dialog-message dialog))
      "_No dialog messages._\n")))

(defn- render-full-md
  ([data]
   (render-full-md data {:include-prompts? true}))
  ([data {:keys [include-prompts?] :or {include-prompts? false}}]
   (str (render-header data)
     (render-raw-diagnostics (:turns data))
     "## Turn-by-turn breakdown\n\n"
     (apply str (map (partial render-turn-block include-prompts?) (:turns data))))))

(defn transcript->md
  "Render transcript data as Markdown. Pure transformation over
   `transcript`'s canonical data shape. Returns a string.

   Modes:
   - `:full`               - bounded diagnostic report (default).
   - `:debug`              - diagnostic report plus prompt bodies (still bounded).
   - `:dialog`             - user/assistant dialog only.
   - `:system-prompts`     - persisted system prompt snapshots only.
   - `:prompts`            - exact persisted provider prompt envelopes."
  ([data]
   (transcript->md data {:mode :full}))
  ([data {:keys [mode] :or {mode :full}}]
   (case mode
     :dialog         (render-dialog-md data)
     :system-prompts (render-system-prompts-report data)
     :prompts        (render-provider-prompts-report data)
     :full           (render-full-md data)
     :debug          (render-full-md data {:include-prompts? true})
     (render-full-md data))))

(defn transcript-md
  "Render the conversation as Markdown. Single transformation over
   `transcript`'s data. Returns a string; returns
   `\"Conversation not found: <id>\\n\"` (no throw) on a missing id so
   shell pipelines stay clean."
  ([db-info conversation-id]
   (transcript-md db-info conversation-id {:mode :full}))
  ([db-info conversation-id opts]
   (if-let [data (transcript db-info conversation-id)]
     (transcript->md data opts)
     (str "Conversation not found: " conversation-id "\n"))))

;; =============================================================================
;; CLI command - `vis extensions reproduction <CONVERSATION-ID>`. Foundation owns
;; it, mounted through `:ext/cli` rather than direct global registration.
;; =============================================================================

(defn- println-original!
  [^String s]
  (.println ^java.io.PrintStream vis/original-stdout s)
  (.flush ^java.io.PrintStream vis/original-stdout))

(defn- reproduction-usage! []
  (println-original! "Usage: vis extensions reproduction <CONVERSATION-ID>")
  (println-original! "")
  (println-original! "Prints one bounded Markdown diagnostic artifact:")
  (println-original! "  every turn, iteration, executed code preview,")
  (println-original! "  vars, reasoning trace, final answer, and raw LLM diagnostics with bounded previews.")
  (println-original! "")
  (println-original! "No mode flags are supported. The artifact is bounded by default.")
  (println-original! "")
  (println-original! "List conversations with:  vis conversations"))

(defn- parse-reproduction-residual
  [residual]
  (reduce (fn [acc token]
            (let [s (str token)]
              (cond
                (:error acc) acc
                (str/starts-with? s "--") (assoc acc :error (str "Flags are not supported: " s))
                (:conversation-id acc) (assoc acc :error (str "Unexpected extra argument: " s))
                :else (assoc acc :conversation-id (str/trim s)))))
    {}
    residual))

(defn- cli-reproduction-run!
  [_parsed residual]
  (vis/init-cli!)
  (let [{:keys [conversation-id error]} (parse-reproduction-residual residual)
        cid-input (some-> conversation-id str/trim not-empty)]
    (cond
      error
      (do (println-original! error)
        (println-original! "")
        (reproduction-usage!)
        (System/exit 1))

      (nil? cid-input)
      (do (reproduction-usage!)
        (System/exit 1))

      :else
      ;; `resolve-conversation-ref` is prefix-aware and existence-checks the
      ;; result; `vis/db-resolve-conversation-id` only accepts full UUIDs and
      ;; would silently fail on the very prefixes the help text advertises.
      (let [d        (vis/db-info)
            resolved (resolve-conversation-ref d cid-input)]
        (if (nil? resolved)
          (do (println-original! (str "Conversation not found: " cid-input))
            (System/exit 1))
          (do (println-original! (transcript-md d resolved))
            (System/exit 0)))))))

(defn cli-command []
  {:cmd/name  "reproduction"
   :cmd/doc   "Print a flag-free Markdown diagnostic artifact for a conversation. It is bounded by default so huge transcripts stay safe to open: every turn, iteration, executed code preview, var summary, reasoning preview, final answer preview, and raw LLM diagnostic summary. Resolves an unambiguous id prefix the same way `vis conversations --fork` does."
   :cmd/usage "vis extensions reproduction <CONVERSATION-ID>"
   :cmd/args  [{:name "conversation-id" :kind :positional :type :string
                :doc  "Conversation id (full UUID or unambiguous prefix)."}]
   :cmd/examples ["vis extensions reproduction eeaf9651-06c7-4dda-9e97-877fcef06337"
                  "vis extensions reproduction eeaf9651"
                  "vis extensions reproduction eeaf9651 > REPRODUCTION.md"]
   :cmd/run-fn cli-reproduction-run!})
