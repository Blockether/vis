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
      structural advisories + extension hook hints into `hints` — a list
      of `{source, content, importance}` dicts; rendered only when non-empty.

   Output skeleton:

     # ctx
     {
      \"id\": \"01HXYZ\",
      \"turn\": 7,
      \"scope\": {\"turn\": 7, \"iter\": 3, \"next_form\": 1},
      \"workspace\": {...},
      \"tasks\": {...},
      \"facts\": {...},
      \"trailer\": [...],
      \"hints\": [{\"source\": \"engine\", \"content\": \"...\", \"importance\": \"medium\"}]
     }"
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.ctx-engine :as eng]
   [com.blockether.vis.internal.env-python :as env]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.safe-guards :as safe-guards]
   [com.blockether.vis.internal.foundation.editing.patch :as patch]))

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

(def ^:private OBS_FORM_RESULT_TOKEN_LIMIT
  "Observation-SHAPED results (file windows, rg hits — re-derivable
   reads, recall-windowable) clip TIGHTER than the universal limit:
   ~6k tokens ≈ 250+ gutter lines, beyond what a single patch flow
   needs. Mutation evidence and unrecognized shapes keep the 10k
   backstop. ONE clip site (here) — the pin/persisted copies stay
   verbatim; this is a render-time view."
  6000)

(declare file-window-result? rg-hits-result?)

(defn- form-result-token-limit
  [v]
  (if (or (file-window-result? v) (rg-hits-result? v))
    OBS_FORM_RESULT_TOKEN_LIMIT
    FORM_RESULT_TOKEN_LIMIT))
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
        ;; shape-aware: observation shapes (file windows, rg hits)
        ;; clip at the tighter OBS limit — see form-result-token-limit
        (form-result-token-limit (:result form))
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

(defn- evidence-refs
  [evidence task-k]
  (mapv :id (get evidence (str task-k))))

(defn- compress-tasks
  "Index-not-Content: truncate/strip bulky fields on completed/terminal tasks
   to stabilize Zone C prefix cache when rendering the prompt, keeping the full
   content available inside the sandbox's local context."
  ([tasks] (compress-tasks tasks nil))
  ([tasks evidence]
   (into {}
     (map (fn [[k t]]
            (let [refs (evidence-refs evidence k)
                  t'   (if (seq refs)
                         (assoc (dissoc t :evidence) :evidence_refs refs)
                         t)]
              [k (if (#{:done :failed :cancelled :rejected :archived} (:status t'))
                   (cond-> (dissoc t' :evidence :acceptance :reason)
                     (and (some? (:evidence t')) (empty? refs))
                     (assoc :evidence "<contained in history>")
                     (some? (:acceptance t')) (assoc :acceptance "<contained in history>")
                     (some? (:reason t')) (assoc :reason "<contained in history>"))
                  ;; Keep active task info, but evidence bodies become refs when possible.
                   t')]))
       tasks))))

(defn- compress-facts
  "Index-not-Content: truncate/strip bulky fields on facts to stabilize
   Zone C prefix cache when rendering the prompt, keeping the full content
   available inside the sandbox's local context."
  [facts]
  (into {}
    (map (fn [[k f]]
           [k (if-let [c (:content f)]
                (assoc f :content
                  (if (> (count (str c)) 120)
                    (str (subs (str c) 0 120) "...<contained in history>")
                    c))
                f)])
      facts)))

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
   ;; Keys are UNQUALIFIED on purpose: the engine view's `:session/*`
   ;; namespace folded into a `session_` prefix on every dict key
   ;; (`context["session_tasks"]`), paying the same 8 chars per key per
   ;; prompt for zero information — the dict IS the session context.
   ;; The model reads `context["tasks"]`, `context["trailer"]`, ….
   (let [hints (vec (:session/hints view))
         tasks (:session/tasks view)
         facts (:session/facts view)
         observations (:session/observations view)
         evidence (:session/evidence view)]
     (cond-> (array-map
               :id    (:session/id view)
               :turn  (:session/turn view)
               :scope (:session/scope view))
       (:session/utilization view)         (assoc :utilization (:session/utilization view))
       true                                (assoc :workspace (or (:session/workspace view) {}))
       (:session/env view)                 (assoc :env (:session/env view))
       (not-empty (:session/routing view)) (assoc :routing (:session/routing view))
       (not-empty (:session/resources view)) (assoc :resources (:session/resources view))
       (not-empty (:session/symbols view)) (assoc :symbols (:session/symbols view))
       (not-empty tasks)                   (assoc :tasks (if include-trailer?
                                                           tasks
                                                           (compress-tasks tasks evidence)))
       (not-empty facts)                   (assoc :facts (if include-trailer?
                                                           facts
                                                           (compress-facts facts)))
       (not-empty observations)            (assoc :observations observations)
       (not-empty evidence)                (assoc :evidence evidence)
       include-trailer?                    (assoc :trailer
                                             (mapv project-trailer-pin (or (:session/trailer view) [])))
       (:session/archive-digest view)      (assoc :archive-digest (:session/archive-digest view))
       (seq hints)                         (assoc :hints hints)))))

(defn render-ctx
  "Render the engine view as the agent-facing `ctx` snapshot — a real PYTHON
   DICT under a `# ctx` comment, STRINGIFIED BY PYTHON (GraalPy). Keys + keyword
   values are snake_case, literals are Python (True/False/None, \"strings\",
   [lists], {dicts}), so the block reads EXACTLY like the `ctx` dict the agent
   holds in the sandbox — by construction, since both come from `project-ctx` and
   the same `env/...->python` path.

   `hints` is the derived advisory field: a list of
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

(defn- file-window-result?
  "cat/tail/range-style result: `{:lines [[N text]…] …}`."
  [v]
  (and (map? v)
    (vector? (:lines v))
    (seq (:lines v))
    (every? (fn [t] (and (sequential? t) (= 2 (count t))))
      (take 3 (:lines v)))))

(defn- render-file-window
  "File-window results render as the canonical hash-gutter block
   (`N:hash│ text`) instead of a Python dict: the gutter IS the patch
   address (`:from_anchor` parses it back), the line number isn't paid
   twice (tuples + a separate `:anchors` map), file text carries no dict
   escaping, and ranged reads use the per-range windows instead of
   shipping the same lines twice (flat `:lines` + `:ranges`). The
   paging/guard fields ride ONCE in a header line — `mtime`/`size` stay
   visible because `patch`/`write` take them back as
   `expected_mtime`/`expected_size`."
  [v]
  (let [header (->> [(some->> (:path v) (str "path "))
                     (when-let [[s e] (:range v)] (str "range " s ".." e))
                     (if-let [n (:next-offset v)]
                       (str "next-offset " n)
                       (when (:eof? v) "eof"))
                     (when (:truncated? v) "truncated")
                     (when-let [m (:mtime v)] (str "mtime " m))
                     (when-let [s (:size v)] (str "size " s))]
                 (remove nil?)
                 (str/join " · "))
        body   (if (seq (:ranges v))
                 (patch/render-hashline-range-block (:ranges v))
                 (patch/render-hashline-block (:lines v)))]
    (str header "\n" body)))

(defn- string-list-result?
  "A flat list of strings — `apropos(...)` names, a comprehension of
   paths/identifiers. Rendered one per line instead of a Python list:
   the quotes + commas paid ~2 ceremony tokens per item. Items with
   newlines keep the list print (raw lines would blur item boundaries)."
  [v]
  (and (sequential? v)
    (seq v)
    (every? string? v)
    (not-any? #(str/includes? % "\n") v)))

(defn- render-string-list
  [v]
  (str/join "\n" v))

(defn- rg-hits-result?
  [v]
  (and (map? v)
    (vector? (:hits v))
    (seq (:hits v))
    (map? (first (:hits v)))
    (:path (first (:hits v)))))

(defn- render-rg-hits
  "rg content hits render grouped by path with the SAME hash-gutter
   anchors `cat` shows (`{:anchor}` already carries `lineno:hash`) —
   instead of one `{path line text anchor}` dict per hit (~20 ceremony
   tokens each, path repeated per hit)."
  [v]
  (str
    (->> (:hits v)
      (partition-by :path)
      (map (fn [hits]
             (str (:path (first hits)) "\n"
               (->> hits
                 (map (fn [{:keys [line text anchor]}]
                        (str (or anchor line) patch/hashline-gutter text)))
                 (str/join "\n")))))
      (str/join "\n"))
    (when (= :limit (:truncated-by v))
      "\n… truncated by limit")))

(defonce ^:private op-index-fold-cache
  ;; Memo of op-keyword-index snapshots folded to Python call names:
  ;; `{<index-snapshot> {py-name v}}`. Registration happens at boot /
  ;; extension install, so each registry's snapshot is identical across
  ;; virtually every lookup — the fold runs once per registration burst,
  ;; not per pin/iteration. Bounded: cleared when stale snapshots pile up.
  (atom {}))

(defn fold-op-index
  "Fold an op-keyword index map (`{:shell/run v, …}` — any of the
   extension registries: tags, model-render-fns) to the sandbox call
   names the model writes (`{\"shell_run\" v, …}`), with the SAME fold
   the globals bind under (`env/sym->py-name`). Memoized on snapshot
   identity — the ONE fold site for every head-keyed lookup (trailer
   model renders here, `classify-form-tag`'s tag resolver in loop)."
  [index]
  (or (get @op-index-fold-cache index)
    (let [folded (into {}
                   (map (fn [[op v]] [(env/sym->py-name (symbol op)) v]))
                   index)]
      (swap! op-index-fold-cache
        (fn [m] (assoc (if (> (count m) 8) {} m) index folded)))
      folded)))

(defn- model-render-fn-for-head
  "The registered `:model-render-fn` for a form whose source HEAD is the
   Python call `head-name`, or nil."
  [head-name]
  (when head-name
    (get (fold-op-index (extension/model-render-fn-index)) (str head-name))))

(defn- model-rendered-result
  "Per-tool compressed string render of `(:result form)`, or nil when the
   tool registered no `:model-render-fn`, the result is a clip stub, the
   render threw (fails OPEN to the generic dict render), or the rendered
   text would break the `<results>` wrapper. `src` is the RAW form source
   (presentation-form strips `:src`, so the caller passes it alongside)."
  [form src]
  (let [v (:result form)]
    (when (and (contains? form :result)
            ;; tool results are DATA STRUCTURES; a bare string under a
            ;; tool head (a recalled odd value, a wrapper) must render
            ;; RAW, not be destructured into a garbage header — Clojure
            ;; destructuring on a string nils every key without throwing,
            ;; so the throw-guard below never sees it
            (coll? v)
            ;; a clip stub ({:vis/preview …}) is not the tool's shape —
            ;; the render fn would misread it; the stub prints as a dict
            (not (and (map? v) (:vis/preview v))))
      (when-let [f (model-render-fn-for-head (eng/form-head-name (str (or src ""))))]
        (when-let [s (try (f v) (catch Throwable _ nil))]
          (when (and (string? s)
                  (not (str/blank? s))
                  (not (str/includes? s "</results>")))
            s))))))

(defn- recall-window-result?
  "A `recall(addr)`/`recall(addr, {offset})` WINDOW result —
   `{:vis/recall addr :vis/window [a b] :vis/size N :view \"…\" :vis/next?}`."
  [v]
  (and (map? v) (:vis/recall v) (string? (:view v)) (:vis/window v)))

(defn- render-recall-window
  "recall WINDOW pins render the slice RAW under a one-line cursor header
   — printing the map JSON-escaped every byte of the `:view` (which is
   already the COMPRESSED render of the recalled value)."
  [{:keys [view] :as v}]
  (let [[a b] (:vis/window v)]
    (str "recall " (:vis/recall v) " · chars " a ".." b " of " (:vis/size v)
      (when-let [nxt (:vis/next v)] (str " · next: " nxt))
      "\n" view)))

(defn render-form-value
  "THE compressed model-facing string for one tool/form VALUE — the same
   dispatch trailer pins use, callable by anything that needs to show a
   stored value to the model (recall windows re-render through this so a
   recalled `cat`/`rg`/shell/git payload reads exactly like its original
   pin did, not as a pr-str'd map):
     - per-tool      → the producing call's registered `:model-render-fn`
                       (resolved from `src`'s call head)
     - recall window → raw `:view` under a cursor header
     - file windows  → hash-gutter block (see `render-file-window`)
     - rg hits       → grouped gutter lines (see `render-rg-hits`)
     - string        → RAW text; quoted printer only when the text could
                       break the `</results>` wrapper
     - string list   → one item per line (apropos names, path lists)
     - other value   → the canonical Python printer (`:op` stripped —
                       the call is visible in the assistant replay)"
  ^String [src v]
  (let [;; RAW shape renders (gutters, recall views) must not embed the
        ;; literal closing tag — a cat of a file that itself contains
        ;; the tag text would terminate the frozen block early. Same
        ;; convention the raw-string branch established (tested):
        ;; quoted printer instead, so the tag reads as payload.
        wrapper-safe (fn [^String rendered]
                       (if (and rendered (str/includes? rendered "</results>"))
                         (env/ctx->python-str v)
                         rendered))]
    (if-let [s (model-rendered-result {:result v} src)]
      s
      (cond
        (recall-window-result? v)
        (wrapper-safe (render-recall-window v))

        (file-window-result? v)
        (wrapper-safe (render-file-window v))

        (rg-hits-result? v)
        (wrapper-safe (render-rg-hits v))

        (string? v)
        (if (str/includes? v "</results>")
          (env/ctx->python-str v)
          v)

        ;; flat string lists (apropos names, comprehension-built path
        ;; lists) — one item per line, no list ceremony
        (and (string-list-result? v)
          (not-any? #(str/includes? % "</results>") v))
        (render-string-list v)

        :else
        (env/ctx->python-str (if (map? v) (dissoc v :op) v))))))

(defn- form-render-body
  "One form's rendered body: errors as an `error:`-prefixed dict,
   results via `render-form-value` (the single compressed-value
   dispatch). nil when the form has neither result nor error (already
   pruned to src-only — nothing worth prompt bytes; the DB keeps the
   full form).

   2-arity passes the RAW form source so the per-tool lookup can read the
   call head (`presentation-form` strips `:src` before this point)."
  ([form] (form-render-body form nil))
  ([form src]
   (cond
     (:error form)
     (str "error: " (env/ctx->python-str (:error form)))

     (contains? form :result)
     (render-form-value src (:result form))

     :else nil)))

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
       bookkeeping — born / auto? / summary-source — never ships)

   Opts: `:include-src?` prefixes each output with its (one-line,
   compacted) form source. Used for PRE-TURN pins, whose assistant
   replays never cross the turn boundary — without it those results
   render with no visible calls."
  ([pin] (render-trailer-pin pin nil))
  ([pin {:keys [include-src?]}]
   (if (:summary pin)
     (str "<results scope=\"" (:scope-start pin) ".." (:scope-end pin) "\" folded>\n"
       (str (:summary pin))
       "\n</results>")
     (let [raw-forms (vec (or (:forms pin) []))
           rendered  (vec (keep-indexed
                            (fn [i raw]
                              (let [f (presentation-form raw)]
                                (when-let [body (form-render-body f (:src raw))]
                                  {:idx   (form-render-index f i)
                                   :scope (some-> (:scope f) str not-empty)
                                   :body  (if include-src?
                                            (str (eng/compact-src (:src raw)) "\n" body)
                                            body)})))
                            raw-forms))
           single?   (= 1 (count rendered))
           scope     (if single?
                       ;; the ONE rendered form's full scope reads straight
                       ;; off the tag as a recall address
                       (or (:scope (first rendered))
                         (str (:scope pin) "/f" (:idx (first rendered))))
                       (:scope pin))
           body      (cond
                       (empty? rendered) "(no output)"
                       single? (:body (first rendered))
                       :else (str/join "\n"
                               (mapcat (fn [{:keys [idx body]}]
                                         [(str "[f" idx "]") body])
                                 rendered)))]
       (str "<results" (when scope (str " scope=\"" scope "\"")) ">\n"
         body
         "\n</results>")))))

(defn render-ctx-mutable
  "`render-ctx` minus the trailer: the regenerated per-iteration TAIL
   carries only the MUTABLE ctx (tasks, facts, cursor, utilization,
   routing, env, hints). Past form results ride the conversation as
   frozen `<results>` messages instead — re-rendering them inside this
   tail re-billed the whole trailer uncached on EVERY provider call
   (the prefix cache ends at the first changed byte). The BOUND
   `context` dict still carries `trailer` for programmatic
   access; the lead-in says so."
  [{:keys [ctx warnings]}]
  ;; ONE stable lead-in line — the <results>-message contract lives in
  ;; the SYSTEM prompt (cached once), not here (re-billed every call).
  (str "<context>\n"
    "# Live read-only snapshot of your `context` dict (rebuilt each turn — read it, never reassign it):\n"
    (env/ctx->python-str (project-ctx (eng/session-view ctx warnings) {:include-trailer? false}))
    "\n</context>"))
