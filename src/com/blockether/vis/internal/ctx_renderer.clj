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
   [com.blockether.vis.internal.ctx-engine :as eng]
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
   `:src`, `:scope`, `:tag` pass through. Full result stays on the
   envelope (CTX) and in DB (`session_turn_iteration.forms`);
   `(introspect-form \"<scope>\")` returns the original payload.

   This is THE fix for the c8dc39b1 / 1a9a61ee trailer-bloat class:
   `(def x (v/cat huge-file))` no longer rides every later prompt
   verbatim — the model sees head + tail + the handle.

   Phase G (trailer-noise): `:channel` is NOT in the pass-through set
   anymore — it carries channel-render IR Hiccup ([:p {} [:strong ...] …])
   meant for the TUI, never for the model. `presentation-form` strips
   it (and `:form` / `:form-idx` / `:position` / `:success?` /
   `:symbol`) so the trailer pin stays model-relevant."
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

(defn- archived-proofs-annotation-lines
  "One `;; rejected-proofs …` line per task carrying a non-empty
   `:archived-proofs` vec. Surfaces validator rejections the engine
   stamped at end-of-iter so the model sees the history without an
   explicit `(introspect-failed-proofs :K)` call. Includes the count
   and the most-recent rejection's `:reason` + `:proof` so a single
   line carries enough signal to decide whether to swap the proof or
   change strategy."
  [tasks indent]
  (let [with-archive (->> tasks
                       (filter (fn [[_ t]] (seq (:archived-proofs t))))
                       (sort-by task-sort-key))]
    (mapv
      (fn [[k t]]
        (let [archive (:archived-proofs t)
              latest  (peek archive)
              n       (count archive)
              reason  (some-> (:reason latest) (str))]
          (str (pad indent) ";; rejected-proofs " (zp k)
            "  count=" n
            "  latest=" (pr-str (:proof latest))
            (when reason (str "  reason=" reason))
            "  — (introspect-failed-proofs " (pr-str k) ")")))
      with-archive)))

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

(def ^:private prompt-trailer-form-noise-keys
  "Keys on a trailer-form envelope that are useless to the model and
   belong only to channel UI / engine internals. Stripped in
   `presentation-form` before the envelope reaches the prompt.

   - `:src`        rendered separately as `;; src <scope>:` block above
                   the map (would print the source twice if kept here)
   - `:channel`    channel-render IR Hiccup (TUI / UI surface only —
                   `[:p {} [:strong {} ...]]` trees; ~60 lines per form)
   - `:form`       parsed form (list of symbols/literals) — redundant
                   with `:src` which the model already sees verbatim
   - `:form-idx`   engine positional, model has no use
   - `:position`   engine positional, model has no use
   - `:success?`   derivable from `:error` (nil = success); redundant
   - `:symbol`     first symbol of the form (head) — redundant with
                   `:src` block"
  [:src :channel :form :form-idx :position :success? :symbol])

(defn- presentation-form
  "Prompt-facing copy of one trailer form. Strips noise keys (see
   `prompt-trailer-form-noise-keys`) so the model sees only `:scope`,
   `:tag`, `:result`, `:error`, and any other engine-emitted
   forensic fields — not the channel-render IR Hiccup or duplicate
   source/symbol/position bookkeeping. `:result` is passed through
   `bound-form-result` so an oversized payload renders as the
   `{:vis/preview :vis/size :vis/full}` safe-guard stub instead of
   riding into every subsequent prompt verbatim."
  [form]
  (apply dissoc (bound-form-result form) prompt-trailer-form-noise-keys))

(defn- consult-pin-summary-line
  "One-line preview header for a synthetic consult-resolution pin.
   Reads :id + :result keys (the entry map) and emits a
   compact ;; consult line so the model scans resolved consults
   without parsing the full pr-str entry.

   Active entries:  ;; consult :K :high — <citation-title or content-head>
   Failed entries:  ;; consult :K :failed (:timeout)"
  [form indent]
  (let [id     (:id form)
        entry  (or (:result form) {})
        status (:status entry)]
    (case status
      :failed
      (str (pad indent) ";; consult " id " :failed (" (:error entry) ")")

      :active
      (let [confidence (or (:confidence entry) :medium)
            first-cite (first (:citations entry))
            content    (or (:content entry) "")
            tag        (or (some-> first-cite :title)
                         (some-> first-cite :url)
                         (when (string? content)
                           (subs content 0 (min 60 (count content)))))]
        (str (pad indent) ";; consult " id " " confidence " — " tag))

      (str (pad indent) ";; consult " id " " status))))

(defn- consult-pin?
  [form]
  (= :consult (:tag form)))

(defn- render-consult-form-pin
  "Render a synthetic consult-resolution pin with a leading one-line
   summary, then the projected entry map. Drops the synthetic `:src`
   `(consult-resolved :K)` line — it isn't real model-emitted source."
  [form indent]
  (let [no-src    (presentation-form form)
        map-text  (zp no-src)
        map-line  (str (pad indent) (indent-rest map-text indent))]
    (str (consult-pin-summary-line form indent) "\n" map-line)))

(defn- render-form-pin
  "Render one `:forms` entry: verbatim src block, then the form map with
   `:src` stripped (since the block carries the source unescaped).

   Consult-resolution pins (`:tag :consult`) get a one-line summary
   preview instead of the synthetic `(consult-resolved :K)` src block."
  [form indent]
  (if (consult-pin? form)
    (render-consult-form-pin form indent)
    (let [no-src    (presentation-form form)
          map-text  (zp no-src)
          map-line  (str (pad indent) (indent-rest map-text indent))]
      (str (src-lines-as-block (:scope form) (:tag form) (:src form) indent)
        "\n" map-line))))

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

(defn- pct-string [ratio]
  (cond
    (nil? ratio) "0%"
    :else (str (Math/round (* 100.0 (double ratio))) "%")))

(defn- project-fact
  "LLM-facing projection of a fact. Drops engine-internal flags; keeps
   `:content`, `:status`, `:born`, `:depends-on`, `:contradicts` (vec).
   Raw shape stays in storage; `(introspect-fact :K)` returns it."
  [[k f]]
  [k (cond-> {:status (or (:status f) :active)}
       (some? (:content f))      (assoc :content (:content f))
       (:born f)                 (assoc :born (:born f))
       (seq (:depends-on f))     (assoc :depends-on (:depends-on f))
       (seq (:contradicts f))    (assoc :contradicts (vec (sort (:contradicts f)))))])

(defn- project-spec
  "LLM-facing projection of a spec. Drops `:validator-fn` source from
   each requirement (engine remembers; `(introspect-spec :K)` returns).
   Adds derived: `:progress`, `:missing`, `:validators`. Reqs become
   flat `{:req-id {:title}}` map."
  [progression [k s]]
  (let [reqs (or (:requirements s) [])
        p    (get progression k)]
    [k (cond-> {:title (:title s) :status (or (:status s) :draft)}
         (:born s)                 (assoc :born (:born s))
         (:done-born s)            (assoc :done-born (:done-born s))
         (seq (:depends-on s))     (assoc :depends-on (:depends-on s))
         (seq reqs)                (assoc :reqs
                                     (into {}
                                       (for [r reqs]
                                         [(:id r) {:title (:title r)}])))
         (some? p)                 (assoc :progress
                                     (str (:proven p) "/" (:total p)
                                       " (" (pct-string (:ratio p)) ") "
                                       (name (:state p))))
         (some? p)                 (assoc :missing (vec (sort (:missing p))))
         (seq reqs)                (assoc :validators
                                     (str (count (filter :validator-fn reqs))
                                       "/" (count reqs))))]))

(defn- task-proof-shape
  "Flat proof view per task: `{:spec/req <scope-or-compose>}`. Compose
   proofs surface as the full sub-scope vec under the qualified key."
  [t]
  (into {}
    (for [[spec-k proof-vec] (or (:specs t) {})
          proof              proof-vec
          :let [k (keyword (name spec-k) (name (:requirement proof)))]]
      [k (cond
           (vector? (:proof-compose proof))
           (vec (:proof-compose proof))
           (string? (:proof proof))
           (:proof proof)
           :else nil)])))

(defn- project-task
  "LLM-facing projection of a task. Drops `:validated?`, drops the raw
   `:archived-proofs` vec (replaced by `:rejected-count`), flattens
   `:specs {spec-K [proofs]}` to `:proofs {:spec/:req scope}`."
  [[k t]]
  (let [proofs (task-proof-shape t)
        rejected (count (or (:archived-proofs t) []))]
    [k (cond-> {:title (:title t) :status (or (:status t) :todo)}
         (:born t)                 (assoc :born (:born t))
         (:done-born t)            (assoc :done-born (:done-born t))
         (:source t)               (assoc :source (:source t))
         (:hook-id t)              (assoc :hook-id (:hook-id t))
         (:importance t)           (assoc :importance (:importance t))
         (seq (:depends-on t))     (assoc :depends-on (:depends-on t))
         (seq proofs)              (assoc :proofs proofs)
         (pos? rejected)           (assoc :rejected-count rejected))]))

(defn- alive?
  "Entity is `alive` (rendered in timeline/orphans) when not archived.
   Archived entities go to introspect-archived; live ctx hides them."
  [entity]
  (not= :archived (:status entity)))

(defn- task-roots
  "Tasks that no OTHER task points at via :depends-on. They head the
   timeline; their dep tree expands beneath."
  [tasks dep-graph]
  (let [referenced (reduce-kv
                     (fn [acc [kind _] edges]
                       (cond-> acc
                         (= kind :task)
                         (into (keep (fn [[k id]] (when (= :task k) id)))
                           edges)))
                     #{} dep-graph)]
    (vec (sort (remove #(contains? referenced %) (keys tasks))))))

(defn- build-timeline
  "Walk each task root through the typed dep-graph collecting the
   spec/fact refs it reaches (BFS, dedupe across roots). Returns a vec
   of timeline entries shaped:
     {:task :K :title :status :born :depends-on :proofs :rejected-count
      :specs [{projected-spec}] :facts [{projected-fact}]}
   Tasks whose status is :archived are skipped."
  [ctx dep-graph progression]
  (let [tasks (or (:session/tasks ctx) {})
        specs (or (:session/specs ctx) {})
        facts (or (:session/facts ctx) {})
        live-tasks (into {} (filter #(alive? (val %)) tasks))
        roots (task-roots live-tasks dep-graph)
        seen-spec  (atom #{})
        seen-fact  (atom #{})
        ;; Per-visit dedupe atoms. The dep-graph CAN cycle when ctx
        ;; snapshots predate the Phase B write-time cycle check, and
        ;; even acyclic diamond shapes re-enqueue the same node
        ;; through multiple inbound edges. Without an enqueue-time
        ;; seen check, the BFS loops forever on cycles and explodes
        ;; exponentially on diamonds. The seen-spec / seen-fact atoms
        ;; up in the closure dedupe ACROSS roots (so a node hit by
        ;; two roots is rendered once); these per-visit `enqueued`
        ;; sets dedupe WITHIN one root walk so we never push the same
        ;; node onto the queue twice.
        visit
        (fn [task-k]
          (let [enq-spec (atom #{})
                enq-fact (atom #{})
                enqueue? (fn [[kind' k']]
                           (case kind'
                             :spec (and (contains? specs k')
                                     (alive? (get specs k'))
                                     (not (@enq-spec k'))
                                     (do (swap! enq-spec conj k') true))
                             :fact (and (contains? facts k')
                                     (alive? (get facts k'))
                                     (not (@enq-fact k'))
                                     (do (swap! enq-fact conj k') true))
                             :task false))]
            (loop [queue [[:task task-k]]
                   specs-out []
                   facts-out []]
              (if (empty? queue)
                {:specs specs-out :facts facts-out}
                (let [[kind k :as node] (peek queue)
                      rest             (pop queue)
                      edges            (get dep-graph node #{})
                      spec-here        (and (= :spec kind)
                                         (alive? (get specs k))
                                         (not (@seen-spec k))
                                         k)
                      fact-here        (and (= :fact kind)
                                         (alive? (get facts k))
                                         (not (@seen-fact k))
                                         k)]
                  (when spec-here (swap! seen-spec conj spec-here))
                  (when fact-here (swap! seen-fact conj fact-here))
                  (recur (into rest (filter enqueue?) edges)
                    (cond-> specs-out
                      spec-here (conj (second (project-spec progression
                                                [spec-here (get specs spec-here)]))))
                    (cond-> facts-out
                      fact-here (conj (second (project-fact
                                                [fact-here (get facts fact-here)]))))))))))]
    (vec
      (for [root roots
            :let [task (get live-tasks root)
                  {:keys [specs facts]} (visit root)]]
        (let [[_ projected] (project-task [root task])]
          (cond-> projected
            (seq specs) (assoc :specs (vec specs))
            (seq facts) (assoc :facts (vec facts))
            true        (assoc :task root)))))))

(defn- build-orphans
  "Entities (live, non-archived) NOT pulled into any task tree. Returns
   `{:facts {} :specs {} :tasks {}}`. Tasks here are roots without any
   connected specs/facts AND themselves point at nothing."
  [ctx timeline _dep-graph progression]
  ;; project-* puts the key in position 0 of each returned pair; the
  ;; timeline `:specs` / `:facts` keep that shape, so we walk them as
  ;; map entries to collect the visible keys.
  (let [in-timeline-spec (set (mapcat #(map first (or (:specs %) [])) timeline))
        in-timeline-fact (set (mapcat #(map first (or (:facts %) [])) timeline))
        in-timeline-task (set (map :task timeline))
        live-facts (filter (fn [[_ f]] (alive? f)) (or (:session/facts ctx) {}))
        live-specs (filter (fn [[_ s]] (alive? s)) (or (:session/specs ctx) {}))
        live-tasks (filter (fn [[_ t]] (alive? t)) (or (:session/tasks ctx) {}))
        orphan-fact? (fn [[k _]] (not (contains? in-timeline-fact k)))
        orphan-spec? (fn [[k _]] (not (contains? in-timeline-spec k)))
        orphan-task? (fn [[k _]] (not (contains? in-timeline-task k)))]
    {:facts (into {} (map (fn [pair] (let [[k v] (project-fact pair)] [k v])))
              (filter orphan-fact? live-facts))
     :specs (into {} (map (fn [pair] (let [[k v] (project-spec progression pair)] [k v])))
              (filter orphan-spec? live-specs))
     :tasks (into {} (map (fn [pair] (let [[k v] (project-task pair)] [k v])))
              (filter orphan-task? live-tasks))}))

(defn- render-timeline-value
  "Render :session/timeline. Vec of task-rooted entries, each carrying
   inline projected specs/facts."
  [timeline]
  (if (empty? timeline)
    "[]"
    (zp (vec timeline))))

(defn- render-orphans-value
  "Render :session/orphans. One map with three sub-maps."
  [{:keys [facts specs tasks]}]
  (let [m (cond-> {}
            (seq facts) (assoc :facts facts)
            (seq specs) (assoc :specs specs)
            (seq tasks) (assoc :tasks tasks))]
    (if (empty? m) "{}" (zp m))))

(defn- render-next-actions-value
  "Next-actions vec capped to NEXT_ACTIONS_BUDGET with an overflow hint.
   When the head of the list carries `:blocking? true` (priority 1 fix-
   consistency actions), prepend a `;; ⛔ BLOCKING ...` banner so the
   model cannot miss it even when scanning the ctx fast. Without the
   banner the action is just another entry in the EDN list."
  [actions]
  (let [n         (count actions)
        capped    (vec (take NEXT_ACTIONS_BUDGET actions))
        blocking  (filterv :blocking? capped)
        body      (if (empty? capped) "[]" (zp capped))
        prefix    (when (seq blocking)
                    (str ";; ⛔ " (count blocking)
                      " BLOCKING action(s) — resolve before generating new work"
                      (str/join ""
                        (map (fn [a] (str "\n;;   → " (:hint a))) blocking))
                      "\n"))
        suffix    (when (> n NEXT_ACTIONS_BUDGET)
                    (str "\n;; " (- n NEXT_ACTIONS_BUDGET)
                      " more action(s) suppressed (raise NEXT_ACTIONS_BUDGET or close work)"))]
    (str prefix (cond-> body suffix (str suffix)))))

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
  (let [warnings*     (or warnings [])
        actions*      (or next-actions [])
        blocking-n    (count (filter :blocking? actions*))
        warn-n        (count warnings*)
        banner        (when (or (pos? blocking-n) (pos? warn-n))
                        (str ";; ⚠ ctx-summary: "
                          (when (pos? warn-n) (str warn-n " warning(s)"))
                          (when (and (pos? warn-n) (pos? blocking-n)) ", ")
                          (when (pos? blocking-n) (str blocking-n " blocking action(s)"))
                          " — read :session/next-actions first\n"))
        ;; Phase G': projection over raw entities. Storage stays full-
        ;; fidelity in :session/{specs,tasks,facts}; the renderer
        ;; rebuilds a coherent task-rooted timeline + orphans for the
        ;; LLM. introspect-* fns still return raw shape for full audit.
        dep-graph     (or (:dep-graph (eng/build-indexes ctx)) {})
        timeline      (build-timeline ctx dep-graph (or progression {}))
        orphans       (build-orphans ctx timeline dep-graph (or progression {}))
        by-sub        (anchors-by-subtree ctx warnings*)
        other-tail    (section-annotations
                        (concat (get by-sub :session/specs [])
                          (get by-sub :session/tasks [])
                          (get by-sub :session/facts [])
                          (get by-sub :other []))
                        [] 1)]
    (str
      ";; ctx\n"
      banner
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
      (render-section :session/timeline
        (render-timeline-value timeline) nil)                     "\n\n"
      (render-section :session/orphans
        (render-orphans-value orphans) nil)                       "\n\n"
      (render-section :session/trailer
        (render-trailer-value (or (:session/trailer ctx) [])) nil) "\n\n"
      (render-section :session/next-actions
        (render-next-actions-value actions*)
        other-tail)
      "}")))
