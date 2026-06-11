(ns com.blockether.vis.internal.ctx-renderer
  "Pure-fn renderer: turn `{:ctx :warnings}` into the agent-facing `ctx`
   snapshot — a real PYTHON DICT under a `# ctx` comment, in every user message.

   Design rules:

   1. **Python makes the string.** `render-ctx` builds the projected map
      (`project-ctx`) and hands it to `env/ctx->python-str`, which JSON-bridges it
      into GraalPy where `__vis_pp__` (Python) stringifies it. The text is THE
      canonical Python representation — same JSON, same printer — so it matches
      the live `ctx` dict (bound via `env/bind-ctx!`) byte-for-byte.

   2. **Top-level structure is an ordered map.** `project-ctx` builds an array-map
      in canonical key order and omits empty entity/hint subtrees.

   3. **Hints are their own subtree.** `eng/session-view` conjoins engine
      structural advisories + extension hook hints into `session_hints` — a list
      of `{source, content, importance}` dicts; rendered only when non-empty.

   Output skeleton:

     # ctx
     {
      \"session_id\": \"01HXYZ\",
      \"session_turn\": 7,
      \"session_scope\": {\"turn\": 7, \"iter\": 3, \"next_form\": 1},
      \"session_workspace\": {...},
      \"session_tasks\": {...},
      \"session_facts\": {...},
      \"session_trailer\": [...],
      \"session_hints\": [{\"source\": \"engine\", \"content\": \"...\", \"importance\": \"medium\"}]
     }"
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.ctx-engine :as eng]
   [com.blockether.vis.internal.env-python :as env]
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
   warning. Full content stays in CTX + DB; `recall(\"K\")` windows it."
  [fact-k fact]
  (if-let [content (:content fact)]
    (assoc fact :content
      (safe-guards/clip-value content
        FACT_CONTENT_TOKEN_LIMIT
        (eng/recall-call fact-k)))
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
   `recall(\"<scope>\")` windows the original payload (scroll via
   vis_next).

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
        (eng/recall-call (:scope form))))
    form))

;; =============================================================================
;; The single value printer
;;
;; The `<context>` STRING is produced canonically by GraalPy: `render-ctx` builds
;; the projected Clojure map, and `env/ctx->python-str` JSON-bridges it into a
;; DEDICATED printer Context where `__vis_pp__` (Python) stringifies it — separate
;; from the eval sandbox Context. So the printed text and the live `context` dict
;; (bound via `env/bind-ctx!` from the SAME projection) cannot drift.
;; =============================================================================

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

(defn- project-trailer-pin
  "Prompt-facing copy of one trailer entry. Form pins (`:forms` vec) get each
   envelope passed through `presentation-form` to strip channel/engine noise
   keys; summary pins pass through untouched. The result is plain data that
   `env/ctx->python-str` renders as a Python dict."
  [pin]
  (if (vector? (:forms pin))
    (update pin :forms #(mapv presentation-form %))
    pin))

;; =============================================================================
;; Top-level
;; =============================================================================

(defn project-ctx
  "THE canonical projection of a `session-view` into the agent-facing ordered map
   — the SINGLE source of truth for both the rendered `# ctx` text AND the live
   `ctx` dict bound in the sandbox (loop/execute-code binds this same shape via
   `env/bind-ctx!`). array-map fixes canonical key order; empty entity/hint
   subtrees are omitted; trailer pins are noise-stripped via `project-trailer-pin`.

   Takes a `session-view` map (from `eng/session-view`). The bound-ctx path and
   the render path differ only in whether `:session/hints` is present (the light
   per-block snapshot skips `derive-warnings`), which falls out of `(seq hints)`.

   Opts: `:include-trailer?` (default true). The BOUND dict always keeps
   the trailer; the prompt-RENDER path excludes it (`render-ctx-mutable`)
   because form results ride the conversation as frozen `<results>`
   messages — the one deliberate, documented divergence between the
   bound dict and the rendered text (prefix-cache economics, see
   `frozen-trailer-messages` in internal/loop.clj)."
  ([view] (project-ctx view nil))
  ([view {:keys [include-trailer?] :or {include-trailer? true}}]
   (let [hints (vec (:session/hints view))]
     (cond-> (array-map
               :session/id    (:session/id view)
               :session/turn  (:session/turn view)
               :session/scope (:session/scope view))
       (:session/utilization view)         (assoc :session/utilization (:session/utilization view))
       true                                (assoc :session/workspace (or (:session/workspace view) {}))
       (:session/env view)                 (assoc :session/env (:session/env view))
       (not-empty (:session/routing view)) (assoc :session/routing (:session/routing view))
       (not-empty (:session/resources view)) (assoc :session/resources (:session/resources view))
       (not-empty (:session/symbols view)) (assoc :session/symbols (:session/symbols view))
       (not-empty (:session/tasks view))   (assoc :session/tasks (:session/tasks view))
       (not-empty (:session/facts view))   (assoc :session/facts (:session/facts view))
       include-trailer?                    (assoc :session/trailer
                                             (mapv project-trailer-pin (or (:session/trailer view) [])))
       (:session/archive-digest view)      (assoc :session/archive-digest (:session/archive-digest view))
       (seq hints)                         (assoc :session/hints hints)))))

(defn render-ctx
  "Render the engine view as the agent-facing `ctx` snapshot — a real PYTHON
   DICT under a `# ctx` comment, STRINGIFIED BY PYTHON (GraalPy). Keys + keyword
   values are snake_case, literals are Python (True/False/None, \"strings\",
   [lists], {dicts}), so the block reads EXACTLY like the `ctx` dict the agent
   holds in the sandbox — by construction, since both come from `project-ctx` and
   the same `env/...->python` path.

   `session_hints` is the derived advisory field: a list of
   `{source, content, importance}` dicts (engine structural advisories +
   extension hook hints) — the single 'what needs attention' surface.

   Input map keys:
     :ctx       full ::cs/ctx (validated upstream)
     :warnings  vec of short engine-advisory strings; `session-view` wraps
                them into `{source: engine}` hint dicts

   Wrapped in a `<context>` tag (with a one-line lead-in) so the model can't skim
   past it — XML delimiters are a strong salience signal. The body is the live
   value of the `context` Python dict.

   Output: `<context>\\n<lead-in>\\n{ …python dict… }\\n</context>`."
  [{:keys [ctx warnings]}]
  (str "<context>\n"
    "# Live read-only snapshot of your `context` dict (rebuilt each turn — read it, never reassign it):\n"
    (env/ctx->python-str (project-ctx (eng/session-view ctx warnings)))
    "\n</context>"))

(defn- form-render-index
  "True f-index of a form for the `[fK]` marker, from its `:scope`
   (`t2/i1/f3` → 3) — survives done()/silent forms being filtered out,
   so recall addresses stay correct. Positional fallback when the
   scope doesn't parse."
  [form fallback-idx]
  (or (some->> (:scope form) str (re-find #"/f(\d+)") second parse-long)
    (inc (long fallback-idx))))

(defn- form-render-body
  "One form's rendered body, escape-tax free where possible:
     - errors      → `error: {python dict}`
     - string      → RAW text (the quoted-dict form paid ~a token per
                     line in \\n/quote escaping); falls back to the
                     quoted printer only when the text could break the
                     `</results>` wrapper
     - other value → the canonical Python printer (`:op` stripped from
                     top-level maps — the call is visible in the
                     assistant replay; stamping it again per result was
                     a self-introduction on every tool call)
   nil when the form has neither result nor error (already pruned to
   src-only — nothing worth prompt bytes; the DB keeps the full form)."
  [form]
  (cond
    (:error form)
    (str "error: " (env/ctx->python-str (:error form)))

    (string? (:result form))
    (let [s (:result form)]
      (if (str/includes? s "</results>")
        (env/ctx->python-str s)
        s))

    (contains? form :result)
    (env/ctx->python-str
      (let [v (:result form)]
        (if (map? v) (dissoc v :op) v)))

    :else nil))

(defn render-trailer-pin
  "Render ONE trailer pin as a standalone FROZEN block — the body of a
   permanent `<results>` user message in the conversation. Deterministic
   for equal pin data (same projection + same canonical Python printer
   as the ctx render), which is what makes the frozen messages
   byte-stable across iterations and therefore prefix-cacheable.

   LEAN format (every byte rides cached re-bills + full-price re-buys
   at each turn start):
     - scope lives ONLY in the tag attribute; a single-form pin uses
       the form's FULL scope (`t2/i2/f1`) so recall addresses read
       straight off the tag
     - multi-form pins mark each output with its true `[fK]` index —
       no per-form scope strings, no `forms` wrapper dict
     - string results render RAW (no \\n/quote escaping)
     - summary pins render their summary text raw (the engine
       bookkeeping — born / auto? / summary-source — never ships)"
  [pin]
  (if (:summary pin)
    (str "<results scope=\"" (:scope-start pin) ".." (:scope-end pin) "\" folded>\n"
      (str (:summary pin))
      "\n</results>")
    (let [forms    (mapv presentation-form (or (:forms pin) []))
          rendered (vec (keep-indexed
                          (fn [i f]
                            (when-let [body (form-render-body f)]
                              {:idx   (form-render-index f i)
                               :scope (some-> (:scope f) str not-empty)
                               :body  body}))
                          forms))
          single?  (= 1 (count rendered))
          scope    (if single?
                     ;; the ONE rendered form's full scope reads straight
                     ;; off the tag as a recall address
                     (or (:scope (first rendered))
                       (str (:scope pin) "/f" (:idx (first rendered))))
                     (:scope pin))
          body     (cond
                     (empty? rendered) "(no output)"
                     single? (:body (first rendered))
                     :else (str/join "\n"
                             (mapcat (fn [{:keys [idx body]}]
                                       [(str "[f" idx "]") body])
                               rendered)))]
      (str "<results" (when scope (str " scope=\"" scope "\"")) ">\n"
        body
        "\n</results>"))))

(defn render-ctx-mutable
  "`render-ctx` minus the trailer: the regenerated per-iteration TAIL
   carries only the MUTABLE ctx (tasks, facts, cursor, utilization,
   routing, env, hints). Past form results ride the conversation as
   frozen `<results>` messages instead — re-rendering them inside this
   tail re-billed the whole trailer uncached on EVERY provider call
   (the prefix cache ends at the first changed byte). The BOUND
   `context` dict still carries `session_trailer` for programmatic
   access; the lead-in says so."
  [{:keys [ctx warnings]}]
  ;; ONE stable lead-in line — the <results>-message contract lives in
  ;; the SYSTEM prompt (cached once), not here (re-billed every call).
  (str "<context>\n"
    "# Live read-only snapshot of your `context` dict (rebuilt each turn — read it, never reassign it):\n"
    (env/ctx->python-str (project-ctx (eng/session-view ctx warnings) {:include-trailer? false}))
    "\n</context>"))

