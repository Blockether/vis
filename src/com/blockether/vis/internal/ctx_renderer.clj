(ns com.blockether.vis.internal.ctx-renderer
  "Pure renderer for the standing agent-facing `<context>` snapshot.

   `render-ctx` projects the session view with `project-ctx` and prints it via
   the same Python pretty-printer path used to bind the live sandbox `context`
   dict, so the visible block and runtime value share one canonical shape."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.ctx-engine :as eng]
   [com.blockether.vis.internal.env-python :as env]))

;; =============================================================================
;; Knobs
;; =============================================================================

;; Context rendering is intentionally narrow: stable session identity,
;; workspace/env/routing/resources/symbols, and utilization. Tool outputs are
;; rendered as append-only `<results>` messages by the loop, not projected here.

;; =============================================================================
;; The single value printer
;;
;; The `<context>` STRING is produced canonically by GraalPy: `render-ctx` builds
;; the projected Clojure map, and `env/ctx->python-str` marshals it (via `->py`,
;; an ordered `ProxyHashMap`) into a DEDICATED printer Context where `__vis_pp__`
;; (Python) stringifies it — separate from the eval sandbox Context, no JSON. So
;; the printed text and the live `context` dict (bound via `env/bind-ctx!` from
;; the SAME projection) cannot drift.
;; =============================================================================



;; =============================================================================
;; Top-level
;; =============================================================================

(defn project-ctx
  "THE canonical projection of a `session-view` into the agent-facing ordered map
   — the SINGLE source of truth for both the rendered `<context>` text AND the live
   `context` dict bound in the sandbox (loop/execute-code binds this same shape
   via `env/bind-ctx!`). array-map fixes canonical key order; empty subtrees are
   omitted.

   Takes a `session-view` map (from `eng/session-view`). `_opts` is ignored."
  ([view] (project-ctx view nil))
  ([view _opts]
   ;; Keys are UNQUALIFIED on purpose: the engine view's `:session/*`
   ;; namespace folds to bare Python dict keys.
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

(def ^:private static-context-keys
  "The ambient session keys the model needs as STANDING context — embedded
   once in the (cached) system prompt and re-emitted per iteration ONLY when
   they change. The per-iteration churn (`:id` `:turn` `:scope`
   `:utilization`) is internal bookkeeping and never model-facing."
  [:workspace :env :routing :resources :symbols])

(defn project-ctx-static
  "`project-ctx` limited to `static-context-keys`, canonical order preserved.
   The host clock (`[:env :host :clock]`) is stripped: it ticks every render,
   so leaving it in would make the per-iteration change-diff fire every time."
  [view]
  (let [full (project-ctx view)
        m    (reduce (fn [m k] (if (contains? full k) (assoc m k (get full k)) m))
               (array-map) static-context-keys)]
    (cond-> m
      (get-in m [:env :host :clock]) (update-in [:env :host] dissoc :clock))))

(defn render-ctx-static
  "Render ONLY the standing session context (workspace / env / routing /
   resources / symbols) as a `<context>` block — embedded once in the system
   prompt, then re-emitted in the conversation ONLY when it changed mid-turn
   (the diff). Returns nil when there is nothing to show."
  [{:keys [ctx warnings]}]
  (let [m (project-ctx-static (eng/session-view ctx warnings))]
    (when (seq m)
      (str "<context>\n"
        "# Standing session context — workspace, environment, routing, available tools.\n"
        "# Embedded in your system prompt; it only reappears here when something changed.\n"
        (env/ctx->python-str m)
        "\n</context>"))))

(defn ctx-static-map
  "The standing ctx as a MAP (`project-ctx-static`) — the data behind
   `render-ctx-static`'s `<context>` block. Baseline + current input to
   `render-ctx-delta`."
  [{:keys [ctx warnings]}]
  (project-ctx-static (eng/session-view ctx warnings)))

(defn- ctx-key->py
  "Keyword/string ctx key → the Python dict key string (snake_case), matching
   how `->py` renders keys, so the delta's `ctx[\"k\"]` path hits the live dict."
  [k]
  (-> k (#(if (keyword? %) (name %) (str %))) (str/replace "-" "_")))

(defn- ctx-path-str
  "A key path `[:env :host :os]` → the Python subscript chain
   `[\"env\"][\"host\"][\"os\"]`."
  [path]
  (apply str (map #(str "[\"" (ctx-key->py %) "\"]") path)))

(defn- ctx-delta-ops
  "RECURSIVE structural diff of `prev`→`cur` under `path`, as a seq of Python op
   strings. Descends into nested maps so only the leaf (or subtree) that actually
   moved is emitted — `ctx[\"env\"][\"os\"] = \"linux\"` rather than re-sending the
   whole `env`. Added key → assign its value; removed → `del`; changed map →
   recurse; changed leaf → assign."
  [path prev cur]
  (if (and (map? prev) (map? cur))
    (mapcat (fn [k]
              (let [pv (get prev k ::absent), cv (get cur k ::absent)]
                (cond
                  (= cv ::absent)           [(str "del ctx" (ctx-path-str (conj path k)))]
                  (= pv cv)                 nil
                  (and (map? pv) (map? cv)) (ctx-delta-ops (conj path k) pv cv)
                  :else                     [(str "ctx" (ctx-path-str (conj path k))
                                              " = " (env/ctx->python-str cv))])))
      (distinct (concat (keys prev) (keys cur))))
    (when (not= prev cur)
      [(str "ctx" (ctx-path-str path) " = " (env/ctx->python-str cur))])))

(defn render-ctx-delta
  "Structural Python delta of the standing ctx between the previously-sent map
   `prev` and the current `cur` (both `ctx-static-map` shape). RECURSIVE: emits
   the minimal `ctx[\"a\"][\"b\"] = <repr>` / `del ctx[\"a\"][\"b\"]` ops for exactly
   the keys that moved, at any depth — never the whole context. nil when
   nothing changed. Append-only + cache-safe."
  [prev cur]
  (let [ops (ctx-delta-ops [] prev cur)]
    (when (seq ops) (str/join "\n" ops))))

(defn render-ctx
  "Render the session view as the agent-facing Python-shaped context snapshot.
   Keys + keyword values are snake_case, literals are Python (True/False/None,
   strings, lists, dicts), and both this text and the live sandbox `context`
   dict come from `project-ctx` plus the same `env/...->python` path.

   Input map keys:
     :ctx       full context map
     :warnings  legacy compatibility slot, ignored by `session-view`

   Wrapped in a `<context>` tag (with a one-line lead-in) so the model can't skim
   past it — XML delimiters are a strong salience signal. The body is the live
   value of the `context` Python dict.

   Output: `<context>\\n<lead-in>\\n{ …python dict… }\\n</context>`."
  [{:keys [ctx warnings]}]
  (str "<context>\n"
    "# Live read-only snapshot of your `context` dict (rebuilt each turn — read it, never reassign it):\n"
    (env/ctx->python-str (project-ctx (eng/session-view ctx warnings)))
    "\n</context>"))

(defonce ^:private op-index-fold-cache
  ;; Memo of op-keyword-index snapshots folded to Python call names:
  ;; `{<index-snapshot> {py-name v}}`. Registration happens at boot /
  ;; extension install, so each registry's snapshot is identical across
  ;; virtually every lookup — the fold runs once per registration burst,
  ;; not per pin/iteration. Bounded: cleared when stale snapshots pile up.
  (atom {}))

(defn fold-op-index
  "Fold an op-keyword index map (`{:shell/run v, …}` — e.g. the tag
   registry) to the sandbox call names the model writes (`{\"shell_run\"
   v, …}`), with the SAME fold the globals bind under
   (`env/sym->py-name`). Memoized on snapshot identity — the ONE fold
   site for head-keyed lookups (`classify-form-tag`'s tag resolver in
   loop)."
  [index]
  (or (get @op-index-fold-cache index)
    (let [folded (into {}
                   (map (fn [[op v]] [(env/sym->py-name (symbol op)) v]))
                   index)]
      (swap! op-index-fold-cache
        (fn [m] (assoc (if (> (count m) 8) {} m) index folded)))
      folded)))

(defn render-form-value
  "THE model-facing string for one tool/form VALUE: the canonical
   STRUCTURED serialization of the result, and nothing else. Tools return
   maps or vectors and the model reads them as DATA — there is NO per-tool
   rendering, no hash-gutter file views, no rg grouping, no recall windows.

   `:op` (the call head) is stripped from maps since the call is already
   visible in the assistant replay; `src` is accepted for call-site
   compatibility but no longer affects the output. File reads return `:anchors`
   as an ordered `{\"ln:hash\" text}` map — the key IS the `patch :from_anchor`
   — so editing resolves straight off this structured data."
  ^String [_src v]
  (env/ctx->python-str (if (map? v) (dissoc v :op) v)))

