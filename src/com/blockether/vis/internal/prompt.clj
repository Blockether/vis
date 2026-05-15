(ns com.blockether.vis.internal.prompt
  "Prompt assembly.

   Provider messages are explicit blocks in send order: core system rules,
   extension fragments, then the current user message. Extension prompts
   stay OUT of `CORE_SYSTEM_PROMPT`; active extensions own their
   model-facing blocks (including foundation's `<environment>`) inside
   the extension message.

   Per-iteration user-role context is assembled by
   `build-iteration-context`: rich-comment
   `;; system-vars` / `;; live-vars` lines plus a journal entry per
   recent iteration, wrapped by zero or more `<iteration_hint>` XML
   blocks from active extensions. The legacy XML pipeline and its token-budgeting
   helpers were retired in the same pass."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.format :as fmt]
   [taoensso.telemere :as tel]))

;; =============================================================================
;; Iteration context assembly
;; =============================================================================

;; `safe-pr-str` + `MAX_RESULT_DISPLAY_CHARS` moved to `format.clj`
;; (the right home for a bounded value-render helper — same neighborhood
;; as `safe-zprint-str` it delegates to). All consumers (tape, TUI
;; progress, history restore, chat extension) require it via the
;; `fmt` alias on this ns or the `vis.core` re-export.

(defn- prompt-block
  [tag body]
  (when (and (string? body) (not (str/blank? body)))
    (str "<" tag ">\n"
      body
      (when-not (str/ends-with? body "\n") "\n")
      "</" tag ">")))

(defn- attr-str
  [v]
  (-> (str v)
    (str/replace "&" "&amp;")
    (str/replace "\"" "&quot;")
    (str/replace "<" "&lt;")
    (str/replace ">" "&gt;")))

(defn- attr-name
  [v]
  (attr-str (if (keyword? v) (name v) v)))

(defn- normalize-system-nudge
  [default-importance nudge]
  (let [entry (cond
                (string? nudge)
                {:importance default-importance
                 :text       nudge}

                (map? nudge)
                {:importance (or (:importance nudge) default-importance)
                 :text       (:text nudge)}

                :else nil)
        text  (some-> (:text entry) str str/trim)]
    (when (and text (not (str/blank? text)))
      {:importance (attr-name (or (:importance entry) default-importance))
       :text       text})))

(defn- format-iteration-hint
  [{:keys [importance text]}]
  (str "<iteration_hint importance=\"" importance "\">\n"
    (attr-str text)
    "\n</iteration_hint>"))

(defn- iteration-hints-block
  [hints]
  (when-let [hints (seq (keep identity hints))]
    (str "<iteration_hints>\n"
      (str/join "\n" (map format-iteration-hint hints))
      "\n</iteration_hints>")))

(defn- call-extension-callback
  [ext f & args]
  (binding [extension/*current-extension* ext
            extension/*current-symbol* nil]
    (apply f args)))

;; =============================================================================
;; Initial messages
;; =============================================================================

(defn previous-turn-context-block
  "Full previous exchange context for follow-up turns.

   Vis deliberately does not replay the whole chat transcript; prior work
   flows through persisted iterations. But one-turn follow-ups like `A`,
   `yes`, or `do it` need the complete immediately previous answer as their
   referent. Do not truncate this block: provider/context management owns the
   final context budget."
  [{:keys [user-request answer]}]
  (let [answer (some-> answer str str/trim)]
    (when (and answer (not (str/blank? answer)))
      (prompt-block
        "previous_turn_context"
        (str
          (when-not (str/blank? (str user-request))
            (str (prompt-block "previous_user_request" user-request)
              "\n\n"))
          (prompt-block "previous_assistant_answer" answer))))))

(defn assemble-initial-messages
  "Initial provider messages for one turn. Deliberately excludes full prior
   dialog transcript: Vis state flows through persisted iterations,
   defs, and DB-backed tools. The current user message is tagged as
   `current_user_message`.

   One full previous-turn context block may be prepended so short follow-ups
   can inspect the prior exchange without replaying the whole conversation."
  [{:keys [stable-prompt-messages initial-user-content previous-turn-context]}]
  (let [previous-block (previous-turn-context-block previous-turn-context)
        user-block     (when initial-user-content
                         (prompt-block "current_user_message" initial-user-content))]
    (vec
      (concat
        (or stable-prompt-messages [])
        (when user-block
          [{:role "user" :content (str/join "\n\n" (keep identity [previous-block user-block]))}])))))

;; =============================================================================
;; System prompt
;; =============================================================================

(def ^:private CORE_SYSTEM_PROMPT
  ;; Each line is its OWN short string, joined at runtime. cljfmt
  ;; only mangles embedded `\"` in the channel-rendered tool-call rowlong multi-line docstrings;
  ;; short single-line strings round-trip cleanly through every
  ;; reformat pass. Keeps the prompt inline (one ns, no resource
  ;; load) while still letting code examples use real double
  ;; quotes.
  (str/join "\n"
    ["Vis -- Clojure SCI eval loop. No chat prose; you reply with code."
     ""
     "LOOP"
     "  Iteration: ONE ```clojure``` block. Many top-level forms inside;"
     "             value of the LAST form is the iteration result."
     "             No (do ...) wrap."
     "  Journal:      prior iteration renders below as commented Clojure"
     "             source. Errors: ;; ! ERROR ... -- correct next iter."
     "  Vars:      every (def ...) persists across iterations and turns."
     "             ;; system-vars and ;; live-vars lists are above the journal."
     "             Bind a probe once; reference by name; do not re-probe."
     "  Tools:     extensions register them (v/cat, v/rg, v/ls, v/patch, ...)."
     "             Tool calls return Handles. @h materializes; (view h :op ...)"
     "             gives a bounded window; (summary h) is a fact map;"
     "             (kind h), (handle? v) work on any value."
     ""
     "ENV"
     "  Aliases: walk str set pp edn s"
     "  Banned : slurp, spit, clojure.java.io -- all I/O via extensions."
     ""
     "DEF DISCIPLINE"
     "  Docstring REQUIRED as second arg, real \"double-quote\" strings:"
     "    (def NAME \"doc\" VAL)"
     "    (defn NAME \"doc\" [args] body)"
     "  Allowed heads: def, defn, defn-, defonce, defmulti, defmacro."
     "  Banned  heads: defrecord, deftype, defprotocol, gen-class,"
     "                 extend-type, extend-protocol, definterface, reify."
     "  Use multimethods for polymorphism. Functions / lazy seqs / runtime"
     "  objects persist as {:vis/ref :expr} and rebuild via re-eval."
     ""
     "ANSWER"
     "  (done IR) terminates the turn iff the block runs without throwing."
     "  Probe + answer in one iteration is fine:"
     ""
     "    (def h \"README handle\" (v/cat \"README.md\"))"
     "    (done [:ir [:p (str \"lines: \" (:line-count (summary h)))]])"
     ""
     "  IR = EDN hiccup [:ir & blocks]. Blocks: :p :h :code :ul :ol :li"
     "  :quote :table :tr :th :td. Inline: :span :br :strong :em :c :a"
     "  :img :kbd :mark :sup :sub. :h {:level 1-6}; :ol {:start N};"
     "  :code {:lang \"...\"}; :a {:href \"...\"}; :img {:src \"...\" :alt \"...\"}."]))

(defn build-system-prompt
  "Core system prompt: CORE_SYSTEM_PROMPT plus optional caller addendum."
  [{:keys [system-prompt]}]
  (str CORE_SYSTEM_PROMPT
    (when (and (string? system-prompt) (not (str/blank? system-prompt)))
      (str "\n\n" system-prompt))))

(defn active-extensions
  "Returns the seq of registered extensions whose `:ext/activation-fn` returns
   truthy for `environment`, in registration order. Single source of truth for
   activation; call ONCE at the top of a turn."
  [environment]
  (when-let [exts (some-> (:extensions environment) deref seq)]
    (vec
      (filter (fn [ext]
                (try
                  (boolean (call-extension-callback ext (:ext/activation-fn ext) environment))
                  (catch Throwable t
                    (tel/log! {:level :error :id ::ext-activation-error
                               :data {:ext (:ext/namespace ext)
                                      :error (ex-message t)}}
                      (str "Extension '" (:ext/namespace ext) "' activation-fn threw"))
                    false)))
        exts))))

(defn extensions-snapshot
  "Build the value of the `TURN_ACTIVE_EXTENSIONS` SYSTEM var from a precomputed
   active-extensions vec.

   Returns a vec of compact, fully-realized data maps - NO functions,
   NO atoms, NO opaque runtime objects. The model walks this with
   `filter` / `keep` / `some` exactly like any other Clojure data
   structure; never has to reach into `(v/extensions)` just to
   discover what's loaded.

   Per element:
     :alias     - short symbol the model calls under (`'v`, `'z`,
                  `'git`, ...). nil when the extension didn't declare
                  an `:ext/alias`.
     :namespace - fully-qualified ns symbol of the extension.
     :doc       - one-line LLM description from `:ext/doc` (when set).
     :kind      - categorical bucket (providers, channels, foundation,
                  languages, persistance, ...) used as the section
                  label both in this snapshot and in `vis extensions
                  list` (when set).
     :registry-id - canonical manifest id, usually the alias symbol.
     :symbols   - vec of bare symbol names the extension intern'd into
                  the sandbox.

   The vec is bound ONCE at turn start (see `iteration-loop`) and
   stays frozen for the rest of the turn - every iteration sees the
   same value."
  [active-extensions]
  (->> (or active-extensions [])
    (mapv (fn [ext]
            (let [info (extension/extension-info ext)
                  registry-id (:registry-id info)]
              (cond-> {:namespace   (:namespace info)
                       :alias       (:alias info)
                       :doc         (:doc info)
                       :kind        (:kind info)
                       :registry-id registry-id
                       :symbols     (mapv :ext.symbol/symbol (:ext/symbols ext))}
                (nil? (:alias info)) (dissoc :alias)
                (nil? (:doc info)) (dissoc :doc)
                (nil? (:kind info)) (dissoc :kind)
                (nil? registry-id) (dissoc :registry-id)))))))

(defn- extension-prompt-id
  [ext]
  (str (or (get-in ext [:ext/alias :alias])
         (:ext/namespace ext)
         "unknown")))

(defn- extension-prompt-fragment
  [ext body]
  (str "<extension id=\"" (attr-str (extension-prompt-id ext)) "\">\n"
    body
    (when-not (str/ends-with? body "\n") "\n")
    "</extension>"))

(defn- extensions-prompt-block
  "Collect `<extensions>` from every active extension that declares
   `:ext/prompt`. Each prompt is `(fn [env] -> string)` (normalized at
   registration). Non-blank results are wrapped as an extension element
   with an id attribute, then joined in the channel-rendered tool-call rowone `<extensions>...</extensions>`
   block."
  [environment active-extensions]
  (let [fragments (keep (fn [ext]
                          (when-let [f (:ext/prompt ext)]
                            (try
                              (let [result (call-extension-callback ext f environment)]
                                (when (and (string? result) (not (str/blank? result)))
                                  (extension-prompt-fragment ext result)))
                              (catch Throwable t
                                (tel/log! {:level :warn
                                           :id ::extension-prompt-error
                                           :data {:ext (:ext/namespace ext)
                                                  :error (ex-message t)}}
                                  "Extension :ext/prompt fn threw")
                                nil))))
                    active-extensions)]
    (when (seq fragments)
      (prompt-block "extensions" (str/join "\n\n" fragments)))))

(defn- turn-system-context-block
  "Turn-scoped system context that can be rebuilt/replaced as runtime
   capabilities change.

   Keep this as ONE provider system message. Extension prompts belong here,
   not in every per-iteration trailer. When a future
   reload path recomputes active extensions mid-turn, it should replace this
   message in the rebuilt stateless provider message vector rather than append
   a second extension/context message."
  [environment active-extensions]
  (when-let [extensions-block (extensions-prompt-block environment active-extensions)]
    (prompt-block "turn_system_context" extensions-block)))

(defn- stable-prompt-message
  [content]
  (when (and (string? content) (not (str/blank? content)))
    {:role "system" :content content}))

(defn stable-prompt-text
  "Join stable prompt message contents for token budgeting and debug bindings only.
   Provider sends the original message vector; this is not a send path."
  [messages]
  (str/join "\n\n" (keep :content messages)))

(defn assemble-stable-prompt-messages
  "Assemble provider-prefix messages.

   Send order is explicit and tested:
     `<system_prompt>`       - CORE_SYSTEM_PROMPT + caller addendum
     `<turn_system_context>` - turn-scoped runtime capability context. Today it
                               contains the single `<extensions>` block; future
                               extension reloads should replace this one message,
                               never append a second extension context.

   Extension fragments are separate from the core system prompt and are not
   repeated in per-iteration trailers.

   Required opts:
     `:active-extensions` - vec from `(active-extensions env)`. Drives
        environment, extension prompt, and nudge collection.

   Optional opts:
     `:system-prompt`            - caller addendum appended to CORE."
  [environment {:keys [system-prompt active-extensions] :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "assemble-stable-prompt-messages requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [core-block (prompt-block "system_prompt"
                     (build-system-prompt {:system-prompt system-prompt}))
        turn-system-block (turn-system-context-block environment active-extensions)]
    (vec
      (keep stable-prompt-message
        [core-block turn-system-block]))))

;; =============================================================================
;; REPL journal rendering primitives
;;
;; A pure-Clojure journal replaces the legacy XML scaffolding: prior
;; iteration source + `;; => results` rendered like a REPL transcript.
;; Wired into the live engine path via `build-iteration-context`.
;; =============================================================================

(def ^:const JOURNAL_RESULT_MAX_CHARS
  "Per-result truncation cap when rendering `;; =>` lines on the journal."
  1500)

(defn journal-iteration-header
  "One-line `;; --- iteration N | turn=K conv=… state=… | status=… ---`
   header that prefixes each rendered iteration's code on the journal.
   `status` is :done | :error | :current. Optional ids: nil values are
   elided so test contexts can render with partial coordinates."
  [{:keys [iteration-position turn-position conv-id state-id status]}]
  (let [parts (cond-> []
                iteration-position (conj (str "iteration " iteration-position))
                turn-position      (conj (str "turn=" turn-position))
                conv-id            (conj (str "conv=" conv-id))
                state-id           (conj (str "state=" state-id))
                status             (conj (str "status=" (name status))))]
    (str ";; --- " (str/join " | " parts) " ---")))

(defn journal-result-line
  "Render `;; => <pr-str of result>` honoring `JOURNAL_RESULT_MAX_CHARS`.
   Returns nil for `:vis/no-result` so a thrown form skips the result
   line entirely (the error annotation carries the meaning)."
  [result]
  (when-not (= :vis/no-result result)
    (let [s (fmt/safe-pr-str result {:max-chars JOURNAL_RESULT_MAX_CHARS})]
      (str ";; => " s))))

(defn journal-side-effect-line
  "Render a side-effect annotation: `;; ! <kind>> <text>` for
   `:stdout` / `:stderr` / `:error` / `:timeout`. Empty / blank text
   returns nil so the renderer can drop it."
  [kind text]
  (when (and text (not (str/blank? (str text))))
    (let [tag (case kind
                :stdout  "stdout> "
                :stderr  "stderr> "
                :error   "ERROR "
                :timeout "TIMEOUT ")]
      (str ";; ! " tag (str/trim (str text))))))

(defn format-journal-iteration
  "Render one iteration as commented Clojure source for the journal.

   Input shape:
     {:iteration-position N
      :turn-position      K          ; optional, for cross-turn rendering
      :conv-id            \"…\"      ; optional, short id
      :state-id           \"…\"      ; optional, short id
      :status             :done|:error|:current
      :code               \"(def …)\"
      :result             <value>    ; or :vis/no-result on throw
      :error              {:message…} ; structured map or nil
      :stdout             \"…\"
      :stderr             \"…\"
      :timeout?           bool}

   Output (newline-joined):
     ;; --- iteration N | turn=K conv=… state=… | status=done ---
     <code text>
     ;; ! stdout> <captured>
     ;; ! stderr> <captured>
     ;; => <pr-str result>           ; omitted on error
     ;; ! ERROR <message>            ; only on error
     ;; ! TIMEOUT <message>          ; only on timeout

   Side-effect lines render in stdout / stderr / => / ERROR / TIMEOUT
   order. The :code body is preserved verbatim — the model's own `;;`
   thinking comments in the channel-rendered tool-call rowthe form survive untouched."
  [{:keys [code result error stdout stderr timeout?] :as iter}]
  (let [header     (journal-iteration-header iter)
        body       (when (string? code) (str/trim-newline code))
        result-ln  (when-not (or error timeout?)
                     (journal-result-line result))
        stdout-ln  (journal-side-effect-line :stdout stdout)
        stderr-ln  (journal-side-effect-line :stderr stderr)
        error-ln   (when error
                     (journal-side-effect-line :error (or (:message error) (str error))))
        timeout-ln (when timeout?
                     (journal-side-effect-line :timeout
                       (or (:message error) "iteration timed out")))]
    (str/join "\n"
      (keep identity [header body stdout-ln stderr-ln result-ln error-ln timeout-ln]))))

(defn format-system-vars-block
  "Render the `;; system-vars:` block for the live-vars discovery
   surface. `entries` is a vec of `{:name :doc}` maps. UPPERCASE
   convention; engine-managed (USER_REQUEST, etc.). Returns nil when
   no entries — caller can omit the block entirely."
  [entries]
  (when (seq entries)
    (str/join "\n"
      (cons ";; system-vars:"
        (map (fn [{:keys [name doc]}]
               (str ";;   " name "  " (pr-str (or doc ""))))
          entries)))))

(defn format-live-vars-block
  "Render the `;; live-vars (N/30):` block for the discovery surface.
   `entries` is a vec of `{:name :doc}` maps. `cap` defaults to 30.
   Returns nil for an empty vec."
  ([entries] (format-live-vars-block entries 30))
  ([entries cap]
   (when (seq entries)
     (str/join "\n"
       (cons (str ";; live-vars (" (count entries) "/" cap "):")
         (map (fn [{:keys [name doc]}]
                (str ";;   " name "  " (pr-str (or doc ""))))
           entries))))))

(defn format-journal
  "Render a sequence of iterations as the full N-1 (or N-1 + N) tape
   the model sees in the user-role message. `iters` is a vec of
   per-iteration maps (see `format-journal-iteration` for the shape) in
   chronological order. Iterations render top-to-bottom separated by
   a blank line so the iteration-header lines visually anchor each
   block.

   Tape-window policy lives in the caller; this fn renders whatever
   it's given. Pass `[N-1]` on a clean run, `[N-1 N]` on an error
   recovery iteration."
  [iters]
  (when (seq iters)
    (str/join "\n\n" (map format-journal-iteration iters))))

(defn format-user-role-journal-message
  "Assemble the full user-role message body: optional system-vars
   header + optional live-vars header + the rendered journal. Each
   section is joined with a blank line so the journal header lines stay
   visually distinct.

   Phase 7 main will wire this through `build-iteration-context` in
   place of the legacy XML
   blocks. Until then the engine still uses the old assembly path
   and this fn is exercised only by tests."
  [{:keys [system-vars live-vars iters]}]
  (let [sys-block  (format-system-vars-block system-vars)
        live-block (format-live-vars-block live-vars)
        tape-block (format-journal iters)
        parts      (keep identity [sys-block live-block tape-block])]
    (when (seq parts)
      (str/join "\n\n" parts))))

(defn iteration->journal-iter
  "Adapt an engine iteration map (the multi-block shape from the legacy
   loop) into the journal-iter shape `format-journal-iteration` expects.

   Engine input:
     {:position N
      :blocks   [{:code :result :error :stdout :stderr
                  :execution-time-ms :timeout?} …]
      :answer   \"…\"          ; optional}

   Optional `coords` map carries per-iteration telemetry that does not
   live on the iteration row itself: turn-position, conv-id, state-id,
   status. Engine-side caller fills these from env / state.

   Joins block fields conservatively: code by `\\n`; stdout/stderr
   joined with `\\n` after dropping blanks; result is the LAST
   successful block's value (model's intended return); error is the
   FIRST error encountered (root cause); timeout? true if any block
   timed out. Status defaults to :error if any block errored, :done
   otherwise — caller can override via `coords`.

   Phase 7 main wires this in `build-iteration-context`. Single-form
   iterations (post-Phase-4) round-trip identity-preserved through
   the same adapter — the join over a 1-element blocks vec is still
   the same single block."
  ([iteration]
   (iteration->journal-iter iteration {}))
  ([iteration {:keys [iteration-position turn-position conv-id state-id status]}]
   (let [blocks (or (:blocks iteration) [])
         non-blank #(when (and (string? %) (not (str/blank? %))) %)
         joined-code (->> blocks (keep :code) (str/join "\n"))
         joined-stdout (->> blocks (keep :stdout) (keep non-blank) (str/join "\n"))
         joined-stderr (->> blocks (keep :stderr) (keep non-blank) (str/join "\n"))
         first-error  (some :error blocks)
         last-success (->> blocks (remove :error) last)
         last-result  (when last-success (:result last-success))
         any-timeout? (boolean (some :timeout? blocks))
         derived-status (cond
                          first-error  :error
                          any-timeout? :error
                          :else        :done)]
     (cond-> {:iteration-position (or iteration-position (:position iteration))
              :status             (or status derived-status)
              :code               joined-code
              :result             (if first-error :vis/no-result last-result)
              :error              first-error
              :stdout             joined-stdout
              :stderr             joined-stderr
              :timeout?           any-timeout?}
       turn-position (assoc :turn-position turn-position)
       conv-id       (assoc :conv-id conv-id)
       state-id      (assoc :state-id state-id)))))

(defn build-iteration-context
  "Phase 7 build-iteration-context replacement: assembles the
   per-iteration user-role trailer — XML
   `<iteration_hints>` (preserved per design) followed by pure
   commented-Clojure sections (system-vars / live-vars / journal).

   Lives alongside the legacy `build-iteration-context`. Phase 7 main
   flips the engine call site in loop.clj from the legacy fn to this
   one. Until then the engine still uses the legacy assembly.

   Required opts:
     `:active-extensions` - vec from `(active-extensions env)`. Drives
        `:turn.iteration/start` hook collection for `<iteration_hints>`.

   Optional opts:
     `:blocks-by-iteration` - carried iterations as
        `[iteration-position {:blocks …}]` pairs. Fed through
        `iteration->journal-iter` to produce journal entries.
     `:iteration`            - current iteration position (1-based).
     `:current-status`       - status to stamp on the LATEST iteration
        in the rendered journal (default :done; pass :current for the
        in-flight iteration that has not yet executed).
     `:system-vars`          - vec of `{:name :doc}` for engine-managed
        UPPERCASE vars. Phase 7 main fills this from sandbox introspection.
     `:live-vars`            - vec of `{:name :doc}` for user vars
        within the LRU window. Phase 7 main fills from per-env LRU map.
     `:model` / `:context-limit` - unused for now; reserved so the
        signature matches `build-iteration-context` for a clean swap."
  [environment {:keys [blocks-by-iteration active-extensions iteration
                       current-status system-vars live-vars]
                :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "build-iteration-context requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [turn-position (long (or (some-> environment :current-turn-position-atom deref) 1))
        conv-id       (some-> environment :conversation-id str
                        (subs 0 (min 8 (count (str (:conversation-id environment))))))
        last-pos      (some-> blocks-by-iteration last first)
        journal-iters    (mapv (fn [[pos iter-data]]
                                 (iteration->journal-iter
                                   iter-data
                                   (cond-> {:iteration-position pos
                                            :turn-position turn-position
                                            :conv-id conv-id}
                                     (and last-pos (= pos last-pos))
                                     (assoc :status (or current-status :done)))))
                           (or blocks-by-iteration []))
        body (format-user-role-journal-message
               {:system-vars (or system-vars [])
                :live-vars   (or live-vars [])
                :iters       journal-iters})
        ;; Reuse the legacy nudge-ctx + hint-collection path so existing
        ;; `:turn.iteration/start` hook contracts keep working unchanged.
        nudge-ctx {:environment environment
                   :iteration (if (some? iteration) (inc (long iteration)) 1)
                   :turn-position turn-position
                   :previous-blocks (some-> blocks-by-iteration last second :blocks)
                   :model nil
                   :context-limit nil
                   :input-tokens 0
                   :title-refresh? false
                   :conversation-title (some-> (:conversation-title-atom environment) deref str str/trim not-empty)
                   :user-request nil}
        all-hints (into []
                    (mapcat
                      (fn [ext]
                        (for [{:keys [id phase fn]} (or (:ext/hooks ext) [])
                              :when (= :turn.iteration/start phase)
                              :let [hit (try (call-extension-callback ext fn
                                               (assoc nudge-ctx :phase phase))
                                          (catch Throwable t
                                            (tel/log! {:level :warn
                                                       :id ::hook-threw
                                                       :data {:ext (:ext/namespace ext)
                                                              :hook id
                                                              :phase phase
                                                              :error (ex-message t)}})
                                            nil))]
                              :when (and (map? hit) (string? (:hint hit)) (not (str/blank? (:hint hit))))]
                          (normalize-system-nudge
                            (or (:importance hit) :normal)
                            (:hint hit)))))
                    (or active-extensions []))
        hints-block (iteration-hints-block all-hints)
        parts (keep (fn [p] (when (and (string? p) (not (str/blank? p))) p))
                [hints-block body])]
    (when (seq parts)
      (str/join "\n\n" parts))))
