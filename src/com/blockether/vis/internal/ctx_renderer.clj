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
   [com.blockether.vis.internal.foundation.editing.patch :as patch]
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

;; =============================================================================
;; Top-level
;; =============================================================================

(defn project-ctx
  "THE canonical projection of a `session-view` into the agent-facing ordered map
   — the SINGLE source of truth for both the rendered `# ctx` text AND the live
   `ctx` dict bound in the sandbox (loop/execute-code binds this same shape via
   `env/bind-ctx!`). array-map fixes canonical key order; empty subtrees are
   omitted.

   Takes a `session-view` map (from `eng/session-view`). `_opts` is ignored
   (the trailer/tasks/facts that the opt once gated are gone)."
  ([view] (project-ctx view nil))
  ([view _opts]
   ;; Keys are UNQUALIFIED on purpose: the engine view's `:session/*`
   ;; namespace folded to a bare dict key — the dict IS the session context.
   ;; There are no tasks/facts/trailer/hints/archive anymore: the model reads
   ;; identity + workspace + env + routing + resources + symbols.
   (cond-> (array-map
             :id    (:session/id view)
             :turn  (:session/turn view)
             :scope (:session/scope view))
     (:session/utilization view)           (assoc :utilization (:session/utilization view))
     true                                  (assoc :workspace (or (:session/workspace view) {}))
     (:session/env view)                   (assoc :env (:session/env view))
     (not-empty (:session/routing view))   (assoc :routing (:session/routing view))
     (not-empty (:session/resources view)) (assoc :resources (:session/resources view))
     (not-empty (:session/symbols view))   (assoc :symbols (:session/symbols view)))))

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

(defn render-ctx-mutable
  "The per-iteration ctx tail: the live MUTABLE ctx snapshot (cursor,
   utilization, workspace, env, routing, resources, symbols). Form
   results ride the conversation as the verbatim message history, not
   inside this tail, so the prefix cache stays intact. Identical to
   `render-ctx` now that there is no trailer to exclude; kept as the
   loop's named call site."
  [{:keys [ctx warnings]}]
  (str "<context>\n"
    "# Live read-only snapshot of your `context` dict (rebuilt each turn — read it, never reassign it):\n"
    (env/ctx->python-str (project-ctx (eng/session-view ctx warnings)))
    "\n</context>"))

