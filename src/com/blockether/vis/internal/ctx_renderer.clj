(ns com.blockether.vis.internal.ctx-renderer
  "Pure-fn renderer: turn `{:ctx :warnings :progression :stages}` into
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
;;   form-result  10 000 tok   (~5% window; enough for code slices / rg context)
;;   fact-content    800 tok   (~0.4% window)

(def ^:private FORM_RESULT_TOKEN_LIMIT 10000)
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
   verbatim once it crosses the generous 10k-token evidence window —
   the model sees head + tail + the handle.

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
;; Phase G fix: keep `:form` (the NATIVE Clojure list) in the projected
;; map and drop the `:src` string. The model reads code as code, no
;; verbatim comment-block prefix needed, and zero quote-escape
;; corruption since lists are printed by zprint structurally.

(def ^:private prompt-trailer-form-noise-keys
  "Keys on a trailer-form envelope that are useless to the model and
   belong only to channel UI / engine internals. Stripped in
   `presentation-form` before the envelope reaches the prompt.

   - `:src`        string copy of the form — redundant with `:form`
                   which is the native Clojure list. Model reads code
                   natively; comment-block prelude was clutter.
   - `:channel`    channel-render IR Hiccup (TUI / UI surface only —
                   `[:p {} [:strong {} ...]]` trees; ~60 lines per form)
   - `:form-idx`   engine positional, model has no use
   - `:position`   engine positional, model has no use
   - `:success?`   derivable from `:error` (nil = success); redundant
   - `:symbol`     first symbol of the form (head) — redundant with
                   `:form` head"
  [:src :channel :form-idx :position :success? :symbol])

(defn- presentation-form
  "Prompt-facing copy of one trailer form. Strips noise keys (see
   `prompt-trailer-form-noise-keys`) so the model sees only `:scope`,
   `:tag`, `:form` (native Clojure list), `:result`, `:error`, and
   any engine-emitted forensic fields — not the channel-render IR
   Hiccup or duplicate `:src` string. `:result` is passed through
   `bound-form-result` so an oversized payload renders as the
   `{:vis/preview :vis/size :vis/full}` safe-guard stub instead of
   riding into every subsequent prompt verbatim.

   No more `;; src <scope>:` verbatim comment block prefix — the
   `:form` field carries the native list which the model reads as
   first-class code."
  [form]
  (apply dissoc (bound-form-result form) prompt-trailer-form-noise-keys))

(defn- render-form-pin
  "Render one `:forms` entry as a single Clojure map. ALL pins are pure
   EDN data — model-emitted forms, consult-resolution pins, summary
   pins, every kind. No `;; src` verbatim block. No `;; consult` summary
   line. The model reads the map directly: `:scope :tag :form :result`
   (or `:scope :tag :id :result` for consult pins). Picking out what
   matters is the model's job, not the renderer's."
  [form indent]
  (let [proj     (presentation-form form)
        map-text (zp proj)]
    (str (pad indent) (indent-rest map-text indent))))

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

(defn- render-stages-value
  "Phase H: render `:session/stages` — vec-of-vecs from
   `eng/derive-stages`. Outer index = stage number; inner vec holds
   parallel-safe entries `{:kind :id :status :reason :remedy}` the
   model emits in ONE fence.

   Caps total entry count at NEXT_ACTIONS_BUDGET, trimming from the
   DEEPEST stage backwards so blockers + leaf work always survive.
   Overflow is announced as a final stage `[{:overflow N :hint \"...\"}]`
   so the data shape stays uniform (vec-of-vecs)."
  [stages]
  (let [stages (vec (or stages []))
        sizes  (mapv count stages)
        total  (apply + 0 sizes)]
    (if (empty? stages)
      "[]"
      (let [budget    NEXT_ACTIONS_BUDGET
            trim      (fn [stages overflow]
                        (loop [s   stages
                               rem overflow]
                          (if (or (zero? rem) (empty? s))
                            s
                            (let [n (count (peek s))]
                              (cond
                                (zero? n)   (recur (pop s) rem)
                                (<= n rem)  (recur (pop s) (- rem n))
                                :else       (conj (pop s)
                                              (vec (take (- n rem) (peek s)))))))))
            overflow  (max 0 (- total budget))
            trimmed   (if (pos? overflow) (trim stages overflow) stages)
            with-tail (cond-> trimmed
                        (pos? overflow)
                        (conj [{:overflow overflow
                                :hint     "raise NEXT_ACTIONS_BUDGET or close work"}]))]
        (zp with-tail)))))

;; =============================================================================
;; Top-level
;; =============================================================================

(defn render-ctx
  "Render the engine view as the bare-EDN text block embedded under `;; ctx`
   in the user message.

   Phase G: the prompt-side derived view is ONE field — `:session/stages`
   — a flat ordered vec of `{:kind :id :status :reason :remedy}` entries.
   Replaces the legacy triplet (`:session/timeline` + `:session/orphans`
   + `:session/next-actions`) AND the trailing `;; ⚠ ...` line-comment
   annotations. The model reads pure EDN data, no embedded prose.

   Raw entity subtrees (`:session/specs` / `:session/tasks` /
   `:session/facts`) ride directly into the prompt when non-empty so
   `(introspect-fact :K)` etc. have their canonical sources visible.

   Input map keys:
     :ctx           full ::cs/ctx (validated upstream)
     :progression   {spec-id {:total :proven :ratio :state :missing}}
     :stages        vec-of-vecs from `eng/derive-stages`

   Output: a string starting with `;; ctx\\n{` and ending with `}`.
   No trailing line-comments anywhere."
  [{:keys [ctx stages]}]
  (let [stages* (or stages [])]
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
      (when-let [symbols (not-empty (:session/symbols ctx))]
        (str (render-section :session/symbols (zp symbols) nil) "\n\n"))
      (when-let [specs (not-empty (:session/specs ctx))]
        (str (render-section :session/specs (zp specs) nil) "\n\n"))
      (when-let [tasks (not-empty (:session/tasks ctx))]
        (str (render-section :session/tasks (zp tasks) nil) "\n\n"))
      (when-let [facts (not-empty (:session/facts ctx))]
        (str (render-section :session/facts (zp facts) nil) "\n\n"))
      (render-section :session/trailer
        (render-trailer-value (or (:session/trailer ctx) [])) nil) "\n\n"
      (render-section :session/stages
        (render-stages-value stages*) nil)
      "}")))
