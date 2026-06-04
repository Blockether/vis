(ns com.blockether.vis.internal.ctx-renderer
  "Pure-fn renderer: turn `{:ctx :warnings}` into the bare-EDN text block
   embedded under `;; ctx` in every user message.

   Design rules:

   1. **ONE printer.** Every Clojure VALUE the renderer emits goes through
      `zp` — a single locked entry into `safe-zprint-str` with a sealed
      `ZP_OPTS` config (no commas, sorted keys, width 100). No `pr-str`, no
      `pprint`, no ad-hoc string concat for value bodies.

   2. **Top-level structure is hand-assembled.** The renderer interleaves
      `:session/X` headers with their zp'd values. That structural layer is
      the only thing the renderer itself writes; it never tries to inject
      text inside a zp'd value.

   3. **Warnings are their own subtree.** The engine's `derive-warnings`
      yields a vec of short strings; the renderer prints them under
      `:session/warnings` (only when non-empty) as the single \"what needs
      attention\" surface. No `;; ⚠` line-comments inside any EDN value.

   Output skeleton:

     ;; ctx
     {:session/id        \"01HXYZ\"
      :session/turn      7
      :session/scope     {:turn 7 :iter 3 :next-form 1}

      :session/workspace {…}
      :session/symbols   {…}

      :session/tasks {…}
      :session/facts   {…}
      :session/trailer […]
      :session/warnings [\"task :t1 :done but dep :t2 is :doing\"]}"
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
(def ^:private WIDTH 100)

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
   warning. Full content stays in CTX + DB; `(recall :K)` windows it."
  [fact-k fact]
  (if-let [content (:content fact)]
    (assoc fact :content
      (safe-guards/clip-value content
        FACT_CONTENT_TOKEN_LIMIT
        (str "(recall " fact-k ")")))
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
   `(recall \"<scope>\")` windows the original payload (scroll via
   :vis/next).

   This is THE fix for the c8dc39b1 / 1a9a61ee trailer-bloat class:
   `(def x (cat huge-file))` no longer rides every later prompt
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
        (str "(recall " (pr-str (:scope form)) ")")))
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

(defn- compact-form-head-opts
  "Return zprint opts that classify this trailer form's head as `:none`.
   zprint otherwise hangs many unknown/namespaced calls (`v/*`, `clj/*`,
   `git/*`, extension aliases) even when they fit. Derive from actual form,
   never from a stale global allowlist."
  [form]
  (if-let [head (and (seq? form) (symbol? (first form)) (first form))]
    (let [heads (cond-> [(str head)]
                  (namespace head) (conj (name head)))]
      (update ZP_OPTS :fn-map merge (zipmap heads (repeat :none))))
    ZP_OPTS))

(defn- zp
  "Render any Clojure value to a multi-line bare-EDN string. Trim trailing
   newline. This is the renderer's ONLY printer surface — see ns docstring."
  ([v] (zp v ZP_OPTS))
  ([v opts]
   (str/trim-newline (fmt/safe-zprint-str v opts))))

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
        map-text (zp proj (compact-form-head-opts (:form proj)))]
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

(defn- project-fact
  "LLM-facing projection of a fact. Drops engine-internal flags; keeps
   `:content`, `:status`, `:born`, `:depends-on`, `:contradicts` (vec),
   and `:files` (durable file regions — path + verbatim src + hash anchors).
   Raw shape stays in storage; `(recall :K)` windows it. `:id` is the
   stable turn-qualified handle the model passes to `(recall {:ids …})`;
   `:recalled` shows it was brought back + why."
  [[k f]]
  [k (cond-> {:status (or (:status f) :active)}
       (:id f)                   (assoc :id (:id f))
       (some? (:content f))      (assoc :content (:content f))
       (:born f)                 (assoc :born (:born f))
       (:recalled f)             (assoc :recalled (:recalled f))
       (seq (:depends-on f))     (assoc :depends-on (:depends-on f))
       (seq (:files f))          (assoc :files (:files f))
       (seq (:contradicts f))    (assoc :contradicts (vec (sort (:contradicts f)))))])

(defn- render-warnings-value
  "Render `:session/warnings` — the vec of short warning STRINGS from
   `eng/derive-warnings`. This is the single \"what needs attention\"
   surface (replaces the legacy `:session/stages` derived view and the
   trailing `;; warn` line-comments). Pure EDN data; the model reads the
   strings and acts on them. Caller only renders this section when the
   vec is non-empty, so an empty vec never reaches here."
  [warnings]
  (zp (vec warnings)))

;; =============================================================================
;; Top-level
;; =============================================================================

(defn render-ctx
  "Render the engine view as the bare-EDN text block embedded under `;; ctx`
   in the user message.

   `:session/warnings` is the only derived field: a flat vec of short
   warning STRINGS from `eng/derive-warnings` (dep target missing,
   contradicting facts, rebind loop, task :done with a non-terminal dep).
   It is the single 'what needs attention' surface and replaces the legacy
   `:session/stages` view plus the trailing `;; warn ...` line-comments.
   The model reads pure EDN data, no embedded prose.

   Raw entity subtrees (`:session/tasks` / `:session/facts`) ride directly
   into the prompt when non-empty so `(recall :K)` etc. have their
   canonical sources visible.

   Input map keys:
     :ctx       full ::cs/ctx (validated upstream)
     :warnings  vec of short strings from `eng/derive-warnings`

   Output: a string starting with `;; ctx\\n{` and ending with `}`.
   No trailing line-comments anywhere."
  [{:keys [ctx warnings]}]
  (let [warnings* (vec (or warnings []))]
    (str
      ";; ctx\n"
      "{" (zp :session/id)    "        " (zp (:session/id ctx))    "\n"
      " " (zp :session/turn)  "      "   (zp (:session/turn ctx))  "\n"
      " " (zp :session/scope) "     "    (zp (:session/scope ctx)) "\n"
      (when-let [util (:engine/utilization ctx)]
        (str " " (zp :session/utilization) " " (zp util) "\n"))
      "\n"
      (render-section :session/workspace
        (zp (or (:session/workspace ctx) {})) nil)                "\n\n"
      (when-let [env-block (:session/env ctx)]
        (str (render-section :session/env (zp env-block) nil) "\n\n"))
      (when-let [symbols (not-empty (:session/symbols ctx))]
        (str (render-section :session/symbols (zp symbols) nil) "\n\n"))
      (when-let [tasks (not-empty (:session/tasks ctx))]
        (str (render-section :session/tasks (zp tasks) nil) "\n\n"))
      (when-let [facts (not-empty (:session/facts ctx))]
        (str (render-section :session/facts (zp facts) nil) "\n\n"))
      (render-section :session/trailer
        (render-trailer-value (or (:session/trailer ctx) [])) nil)
      (when (seq warnings*)
        (str "\n\n"
          (render-section :session/warnings
            (render-warnings-value warnings*) nil)))
      "}")))

