(ns com.blockether.vis.internal.ctx-renderer
  "Pure-fn renderer: turn `{:ctx :warnings :progression :next-actions}` into
   the bare-EDN text block embedded under `;; ctx` in every user message.

   Design rules:

   1. **ONE printer.** Every Clojure VALUE the renderer emits goes through
      `zp` — a single locked entry into `safe-zprint-str` with a sealed
      `ZP_OPTS` config (no commas, sorted keys, width 80). No `pr-str`, no
      `pprint`, no ad-hoc string concat for value bodies.

   2. **Top-level structure is hand-assembled.** The renderer interleaves
      `:session/X` headers with their zp'd values and per-section annotation
      tails. That structural layer is the only thing the renderer itself
      writes; it never tries to inject text inside a zp'd value.

   3. **Annotations live in section tails.** Warnings and progression hints
      anchored to a top-level subtree (`:session/specs`, `:session/tasks`,
      `:session/facts`) follow that section's value as `;; ⚠ …` / `;;
      progression …` lines, indented to the section body's column. Model
      reads the data first, then the issues immediately below — clear
      locality without polluting the EDN.

   Output skeleton:

     ;; ctx
     {:session/id        \"01HXYZ\"
      :session/turn      7
      :session/scope     {:turn 7 :iter 3 :next-form 1}

      :session/workspace {…}
      :session/symbols   {…}

      :session/specs
      {:auth {…}}
      ;; ⚠ spec :auth req :r1 refs nonexistent fact :foo
      ;; progression :auth 2/3 :partial; missing [:r2]

      :session/tasks {…}
      ;; ⚠ task :t1 :done but dep :t2 is :doing

      :session/facts   {…}
      :session/trailer […]
      :session/next-actions […]}"
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.format :as fmt]
   [com.blockether.vis.internal.safe-guards :as safe-guards]
   [com.blockether.vis.internal.tokens :as tokens]))

;; =============================================================================
;; Knobs
;; =============================================================================

;; Trailer rendering is INTENTIONALLY untruncated.
;;
;; Earlier this file capped trailer entries to TRAILER_BUDGET (16) and
;; per-form result strings to TRAILER_FORM_RESULT_MAX_CHARS (1200).
;; Both caps were removed: when the model binds a value via `(def x
;; …)` and the trailer rendered only a {:preview … :truncated? true}
;; map, the model perceived its own data as missing and reached for
;; the only escape hatch the sandbox doesn't allow — `(println x)` —
;; which then surfaced as `I/O side-effect fns are banned in the
;; sandbox.` See conversation ccee2e1f-16ee-4acf-8d93-b4505034c0de
;; (iters 9, 10, 12, 15, 17) for the failure mode.
;;
;; The fix is structural: the trailer prints the full value the form
;; produced. Upstream (loop/form-results) controls how many forms get
;; pinned and whether a value is even storable; once a form is in the
;; trailer, its `:result` and the trailer's entry count are passed
;; through to the provider verbatim. If a payload is genuinely too
;; large to fit a prompt, that must be addressed at the source — not
;; by silently dropping bytes here.
(def ^:private NEXT_ACTIONS_BUDGET 5)
(def ^:private WIDTH 80)

;; ---------------------------------------------------------------------------
;; Safe-guards
;; ---------------------------------------------------------------------------
;; Engine renders the prompt; engine measures size; engine clips the
;; VIEW (not the data). Full payloads always survive in CTX + DB and
;; remain reachable through `introspect-*` verbs.
;;
;; Per-entry guards use `safe-guards/clip-value` which produces a
;; `{:vis/head :vis/tail :vis/size :vis/full}` head+tail preview.
;; No companion call, no AI summarization — deterministic mechanical
;; clip. Engine-driven trailer fold (when total prompt > budget) lives
;; in `safe-guards/ensure-prompt-under-budget!` and uses the same
;; never-LLM policy.
;;
;; Limits in tokens (jtokkit cl100k_base, ~10-30% margin vs native
;; provider tokenizers). Sized for a 200k-token provider window:
;;   form-result   2 000 tok   (~1% window)
;;   fact-content    800 tok   (~0.4% window)

(def ^:private FORM_RESULT_TOKEN_LIMIT 2000)
(def ^:private FACT_CONTENT_TOKEN_LIMIT 800)

(defn- bound-fact-content
  "Replace `(:content fact)` with a safe-guard head+tail stub when its
   token weight exceeds `FACT_CONTENT_TOKEN_LIMIT`. Other keys pass
   through. Mirrors the engine's `:fact-content-too-large` write-time
   warning. Full content stays in CTX + DB; `(introspect-fact :K)`
   recovers it verbatim."
  [fact-k fact]
  (if-let [content (:content fact)]
    (assoc fact :content
      (safe-guards/clip-value content
        FACT_CONTENT_TOKEN_LIMIT
        (str "(introspect-fact " fact-k ")")))
    fact))

(defn- bound-facts
  "Walk every fact entry in `facts` and apply `bound-fact-content`.
   Preserves key order via `into (array-map)` so a session with many
   facts renders deterministically."
  [facts]
  (if (empty? facts)
    {}
    (into (array-map)
      (map (fn [[k v]] [k (bound-fact-content k v)]))
      facts)))

(defn- bound-form-result
  "Replace `(:result form)` with a head+tail safe-guard stub when its
   token weight exceeds `FORM_RESULT_TOKEN_LIMIT`. `:error`,
   `:channel`, `:src`, `:scope`, `:tag` pass through. Full result
   stays on the envelope (CTX) and in DB (`session_turn_iteration.forms`);
   `(introspect-form \"<scope>\")` returns the original payload.

   This is THE fix for the c8dc39b1 / 1a9a61ee trailer-bloat class:
   `(def x (v/cat huge-file))` no longer rides every later prompt
   verbatim — the model sees head + tail + the handle."
  [form]
  (if (contains? form :result)
    (assoc form :result
      (safe-guards/clip-value (:result form)
        FORM_RESULT_TOKEN_LIMIT
        (str "(introspect-form " (pr-str (:scope form)) ")")))
    form))

;; =============================================================================
;; The single value printer
;; =============================================================================

(def ^:private ZP_OPTS
  "Sealed zprint config — every value the renderer emits flows through this.
   No commas (bare EDN), deterministic key order (sort?), preserve namespace
   keywords (no shortening)."
  {:width WIDTH
   :map   {:comma? false :sort? true}
   :style :community})

(defn- zp
  "Render any Clojure value to a multi-line bare-EDN string. Trim trailing
   newline. This is the renderer's ONLY printer surface — see ns docstring."
  [v]
  (str/trim-newline (fmt/safe-zprint-str v ZP_OPTS)))

;; =============================================================================
;; Indent helpers (no value-string surgery — only structural padding)
;; =============================================================================

(defn- pad [n] (apply str (repeat n \space)))

(defn- indent-rest
  "Prefix every line AFTER the first with `n` spaces. First line stays
   verbatim so the caller can position it under its key. Single-line
   input is a no-op; built off a single `str/replace` so the regex
   matches newlines and inserts indentation in one pass."
  [s n]
  (str/replace s #"\n" (str "\n" (pad n))))

;; =============================================================================
;; Annotation lines (the only renderer output that isn't EDN data)
;; =============================================================================

(defn- warning-line [indent w]
  (str (pad indent) ";; ⚠ " (:message w)))

(defn- progression-line [indent spec-id p]
  (let [missing (some-> p :missing seq sort vec)]
    (str (pad indent) ";; progression " (zp spec-id) " "
      (:proven p) "/" (:total p) " " (zp (:state p))
      (when missing (str "; missing " (zp missing))))))

;; =============================================================================
;; Routing warnings to their owning section
;; =============================================================================

(defn- anchors-by-subtree
  "Group warnings by which top-level CTX subtree they anchor to. The :anchor
   first element is the routing key:
     - if it's a key in :session/specs → goes to :session/specs section
     - in :session/tasks → :session/tasks section
     - in :session/facts → :session/facts section
     - otherwise → :other (rendered after :session/next-actions)
   Returns {subtree-key [warnings…]}."
  [ctx warnings]
  (let [spec-keys (set (keys (or (:session/specs ctx) {})))
        task-keys (set (keys (or (:session/tasks ctx) {})))
        fact-keys (set (keys (or (:session/facts ctx) {})))]
    (reduce
      (fn [acc w]
        (let [a (first (:anchor w))
              bucket (cond
                       (contains? spec-keys a) :session/specs
                       (contains? task-keys a) :session/tasks
                       (contains? fact-keys a) :session/facts
                       :else :other)]
          (update acc bucket (fnil conj []) w)))
      {} warnings)))

(defn- section-annotations
  "Build the annotation block (a string of newline-joined ;; lines) for one
   subtree section. Returns nil when there is nothing to say."
  [warnings progression-entries indent]
  (let [lines (concat
                (map #(warning-line indent %) warnings)
                (map (fn [[k p]] (progression-line indent k p)) progression-entries))]
    (when (seq lines) (str/join "\n" lines))))

;; =============================================================================
;; Section + bounded subtrees
;; =============================================================================

(defn- render-section
  "One top-level `:session/X` key plus its value, with an optional annotation
   tail underneath. `value-text` is already zp'd (or hand-built for trailer /
   next-actions). Output column rules:

     :session/X
     <value-text indented by 1 space>
     ;; ⚠ …   (also indented by 1 space)
  "
  [k value-text annotation-block]
  (let [body (str " " (zp k) "\n " (indent-rest value-text 1))]
    (if annotation-block
      (str body "\n " annotation-block)
      body)))

(def ^:private IMPORTANCE_RANK
  {:critical 0 :warn 1 :info 2 nil 3})

(def ^:private TASK_STATUS_RANK
  {:todo 0 :doing 1 :blocked 2 :done 3 :cancelled 4})

(defn- task-sort-key
  "Comparator key for a [task-id task] pair. Order:
     1. hook-source tasks first (so the model sees pending hook work
        before its own task list)
     2. within source, by :importance (critical < warn < info < none)
     3. within importance, by :status (todo < doing < blocked < done <
        cancelled) so live work is on top
     4. tiebreak by string-comparable task id for determinism."
  [[task-id task]]
  [(if (= :hook (:source task)) 0 1)
   (get IMPORTANCE_RANK (:importance task) 3)
   (get TASK_STATUS_RANK (:status task) 5)
   (str task-id)])

(defn- rank-tasks
  "Return an `array-map` of `:session/tasks` entries ranked for render.
   `array-map` preserves insertion order through zprint so the model
   reads hook-tasks first. Returns `{}` unchanged when the input is
   nil/empty."
  [tasks]
  (if (empty? tasks)
    {}
    (into (array-map) (sort-by task-sort-key tasks))))

(defn- hook-task-annotation-lines
  "One `;; via hook` annotation line per unresolved hook-task. Lets the
   model spot pending engine-driven work that's mixed into the same
   :session/tasks map. Resolved (:status :done :validated? true) hooks
   are silent."
  [tasks indent]
  (let [unresolved (->> tasks
                     (filter (fn [[_ t]] (and (= :hook (:source t))
                                           (not (and (= :done (:status t))
                                                  (:validated? t))))))
                     (sort-by task-sort-key))]
    (mapv
      (fn [[k t]]
        (str (pad indent) ";; via hook " (:importance t) " "
          (zp k) "  status=" (:status t)
          (when (= :done (:status t))
            (str " :validated? " (boolean (:validated? t))))))
      unresolved)))

;; ---------------------------------------------------------------------------
;; Trailer pins — render :src verbatim, NOT as a Clojure-escaped string.
;;
;; Why: the trailer's :src field holds the model's prior form source as a
;; plain string. When the whole pin is zp'd, that string round-trips through
;; Clojure's print-method and the inner quote chars become `\"`. The model
;; reads the rendered trailer as informational text (CTX is NOT re-parsed
;; back into EDN — see ns docstring), then copy-pastes the visible source
;; into the next iter. With `\"` in the rendered text, the model copies the
;; literal backslash + quote and emits invalid Clojure (`(str \" / \")`),
;; producing EOF-quote / unbalanced-quote errors at the very next iter.
;;
;; Fix: render each form's :src as a raw code block annotation (multi-line
;; comment with verbatim source — no escaping), and emit the form map with
;; :src omitted. Tag + result + error stay machine-shaped. The trailer is
;; presentation surface only, so dropping pure-EDN parseability is safe.
;; ---------------------------------------------------------------------------

(defn- src-lines-as-block
  "Render `src` as a multi-line block whose lines are prefixed with `;;   `,
   preceded by a `;; src <scope> (<tag>):` header. `src` may be multi-line
   or even the literal `<unparseable trailing form>` marker — printed as-is
   with zero quote-escaping."
  [scope tag src indent]
  (let [prefix (pad indent)
        text   (or src "")
        lines  (str/split-lines text)
        lines  (if (seq lines) lines [""])]
    (str prefix ";; src " scope " (" (name (or tag :form)) "):\n"
      (str/join "\n"
        (map #(str prefix ";;   " %) lines)))))

(defn- presentation-form
  "Prompt-facing copy of one trailer form. `:src` is rendered separately
   as a verbatim block (see `src-lines-as-block`), so the map view drops
   `:src` to avoid printing the same source twice. `:result` is passed
   through `bound-form-result` so an oversized payload renders as the
   `{:vis/preview :vis/size :vis/full}` safe-guard stub instead of
   riding into every subsequent prompt verbatim. Other keys pass
   through unchanged."
  [form]
  (-> form
    bound-form-result
    (dissoc :src)))

(defn- render-form-pin
  "Render one `:forms` entry: verbatim src block, then the form map with
   `:src` stripped (since the block carries the source unescaped)."
  [form indent]
  (let [no-src    (presentation-form form)
        map-text  (zp no-src)
        map-line  (str (pad indent) (indent-rest map-text indent))]
    (str (src-lines-as-block (:scope form) (:tag form) (:src form) indent)
      "\n" map-line)))

(defn- render-trailer-pin
  "Render one top-level trailer entry. Summary pins (carry `:summary`) zp
   straight through — their text has no `:src` field. Form pins
   (`:forms` vec) are rendered with `render-form-pin` per envelope so the
   raw source survives unescaped."
  [pin indent]
  (cond
    ;; summary pin — no :src, safe to zp directly
    (contains? pin :summary)
    (str (pad indent) (indent-rest (zp pin) indent))

    ;; forms pin — :scope + :forms
    (vector? (:forms pin))
    (let [pad-i  (pad indent)
          inner  (+ indent 2)
          forms-text (str/join "\n" (map #(render-form-pin % inner) (:forms pin)))]
      (str pad-i "{:scope " (zp (:scope pin)) "\n"
        pad-i " :forms\n"
        pad-i " [\n" forms-text "\n"
        pad-i "  ]}"))

    :else
    (str (pad indent) (indent-rest (zp pin) indent))))

(defn- render-trailer-value
  "Render the full trailer vector verbatim. No entry cap, no per-form
   payload cap — see the trailer truncation note at the top of this
   file. Each pin is rendered with `render-trailer-pin` so form-source
   survives without quote-escape corruption. Returns a multi-line
   string positioned under the trailer key (caller indents the first
   line)."
  [trailer]
  (if (empty? trailer)
    "[]"
    (str "[\n"
      (str/join "\n" (map #(render-trailer-pin % 1) trailer))
      "\n ]")))

(defn- render-next-actions-value
  "Next-actions vec capped to NEXT_ACTIONS_BUDGET with an overflow hint."
  [actions]
  (let [n (count actions)
        capped (vec (take NEXT_ACTIONS_BUDGET actions))
        body (if (empty? capped) "[]" (zp capped))
        suffix (when (> n NEXT_ACTIONS_BUDGET)
                 (str "\n;; " (- n NEXT_ACTIONS_BUDGET)
                   " more action(s) suppressed"))]
    (cond-> body suffix (str suffix))))

;; =============================================================================
;; Top-level
;; =============================================================================

(defn render-ctx
  "Render the engine view as the bare-EDN text block embedded under `;; ctx`
   in the user message.

   Input map keys:
     :ctx           full ::cs/ctx (validated upstream)
     :warnings      vec of {:code :anchor :message} from derive-warnings
     :progression   {spec-id {:total :proven :ratio :state :missing}}
     :next-actions  vec from derive-next-actions

   Output: a string starting with `;; ctx\\n{` and ending with `}`."
  [{:keys [ctx warnings progression next-actions]}]
  (let [by-sub        (anchors-by-subtree ctx (or warnings []))
        prog-entries  (vec (or progression {}))
        specs-tail    (section-annotations
                        (get by-sub :session/specs [])
                        prog-entries 1)
        ranked-tasks  (rank-tasks (or (:session/tasks ctx) {}))
        hook-lines    (hook-task-annotation-lines (or (:session/tasks ctx) {}) 1)
        tasks-tail-w  (section-annotations (get by-sub :session/tasks []) [] 1)
        tasks-tail    (cond
                        (and tasks-tail-w (seq hook-lines))
                        (str tasks-tail-w "\n" (str/join "\n" hook-lines))
                        (seq hook-lines) (str/join "\n" hook-lines)
                        :else tasks-tail-w)
        facts-tail    (section-annotations (get by-sub :session/facts []) [] 1)
        other-tail    (section-annotations (get by-sub :other []) [] 1)]
    (str
      ";; ctx\n"
      "{" (zp :session/id)    "        " (zp (:session/id ctx))    "\n"
      " " (zp :session/turn)  "      "   (zp (:session/turn ctx))  "\n"
      " " (zp :session/scope) "     "    (zp (:session/scope ctx)) "\n"
      "\n"
      (render-section :session/workspace
        (zp (or (:session/workspace ctx) {})) nil)                "\n\n"
      (when-let [env-block (:session/env ctx)]
        (str (render-section :session/env (zp env-block) nil) "\n\n"))
      (render-section :session/symbols
        (zp (or (:session/symbols ctx) {})) nil)                  "\n\n"
      (render-section :session/specs
        (zp (or (:session/specs ctx) {})) specs-tail)             "\n\n"
      (render-section :session/tasks
        (zp ranked-tasks) tasks-tail)                             "\n\n"
      (render-section :session/facts
        (zp (bound-facts (or (:session/facts ctx) {}))) facts-tail) "\n\n"
      (render-section :session/trailer
        (render-trailer-value (or (:session/trailer ctx) [])) nil) "\n\n"
      (render-section :session/next-actions
        (render-next-actions-value (or next-actions []))
        other-tail)
      "}")))
