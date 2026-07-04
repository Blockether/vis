(ns com.blockether.vis.internal.ctx-renderer
  "Pure renderer for the standing agent-facing `session` snapshot.

   `render-ctx-static` projects the session view with `project-ctx-static` and
   prints it via the same Python pretty-printer path used to bind the live
   sandbox `session` dict, emitting a fenced Python `session = {…}` block — so the
   visible embed and the runtime value share one canonical shape, and
   `render-ctx-delta` mutates that same `session` with `session[...] = …` lines."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.ctx-engine :as eng]
   [com.blockether.vis.internal.env-python :as env]))

;; =============================================================================
;; Knobs
;; =============================================================================

;; Context rendering is intentionally narrow: stable session identity,
;; workspace/env/routing/resources/symbols, and utilization. Tool outputs are
;; rendered as append-only `r["tN/iN/fN"] = …` assignments by the loop, not here.

;; =============================================================================
;; The single value printer
;;
;; The `session = {…}` STRING is produced canonically by GraalPy: `render-ctx-static`
;; builds the projected Clojure map, and `env/ctx->python-str` marshals it (via
;; `->py`, an ordered `ProxyHashMap`) into a DEDICATED printer Context where
;; `__vis_pp__` (Python) stringifies it — separate from the eval sandbox Context,
;; no JSON. So the printed text and the live `session` dict (bound via
;; `env/bind-ctx!` from the SAME projection) cannot drift.
;; =============================================================================

;; =============================================================================
;; Top-level
;; =============================================================================

(def ^:private workspace-drop-keys
  "Internal ids the model never uses — dropped from the model-facing `workspace`
   block (the real linkage lives on the session objects, not in ctx). The ctx is
   STRING-KEYED end to end; producers own the naming, this is only a defensive
   drop of internal id fields."
  #{"id" "workspace_id" "state_id" "session_state_id" "session_id"})

(defn- normalize-workspace
  "Model-facing `workspace` map: drop internal UUID ids. Producers build the
   map STRING-KEYED (`\"root\"`, `\"sandbox\"`, `\"vcs_kind\"`, ...) — no key
   rewriting happens here anymore. Source iteration order is preserved
   (array-map) so the ctx-delta stays stable."
  [ws]
  (if (map? ws)
    (reduce-kv
      (fn [m k v]
        (if (contains? workspace-drop-keys k) m (assoc m k v)))
      (array-map) ws)
    (or ws {})))

(defn project-ctx
  "THE canonical projection of a `session-view` into the agent-facing ordered map
   — the SINGLE source of truth for both the rendered `<context>` text AND the live
   `session` dict bound in the sandbox (loop/execute-code binds this same shape
   via `env/bind-ctx!`). array-map fixes canonical key order; empty subtrees are
   omitted.

   Takes a `session-view` map (from `eng/session-view`). `_opts` is ignored."
  ([view] (project-ctx view nil))
  ([view _opts]
   ;; Keys are BARE on purpose: the engine view's `session_*` prefix folds
   ;; to bare Python dict keys. Strings-only — this map crosses the boundary.
   (cond-> (array-map
             "id"    (get view "session_id")
             "turn"  (get view "session_turn")
             "scope" (get view "session_scope"))
     (get view "session_utilization")
     (assoc "utilization" (get view "session_utilization"))
     true (assoc "workspace" (normalize-workspace (get view "session_workspace")))
     (get view "session_env") (assoc "env" (get view "session_env"))
     (not-empty (get view "session_routing")) (assoc "routing" (get view "session_routing"))
     (not-empty (get view "session_resources")) (assoc "resources" (get view "session_resources"))
     (not-empty (get view "session_symbols")) (assoc "symbols" (get view "session_symbols")))))

(def ^:private static-context-keys
  "The ambient session keys the model needs as STANDING context — embedded
   once in the (cached) system prompt and re-emitted per iteration ONLY when
   they change. The per-iteration churn (`\"id\"` `\"turn\"` `\"scope\"`
   `\"utilization\"`) is internal bookkeeping and never model-facing."
  ["workspace" "env" "routing" "resources" "symbols"])

(defn project-ctx-static
  "`project-ctx` limited to `static-context-keys`, canonical order preserved.
   The host clock (`[:env :host :clock]`) is stripped: it ticks every render,
   so leaving it in would make the per-iteration change-diff fire every time."
  [view]
  (let [full (project-ctx view)
        m    (reduce (fn [m k] (if (contains? full k) (assoc m k (get full k)) m))
               (array-map) static-context-keys)]
    (cond-> m
      (get-in m ["env" "host" "clock"]) (update-in ["env" "host"] dissoc "clock"))))

(defn render-ctx-static
  "Render the standing session context (workspace / env / routing / resources /
   symbols) as a FENCED PYTHON block that binds `session` to its initial value —
   embedded once in the system prompt. The same `session` dict is live in the
   sandbox; mid-session changes arrive as `session[...] = …` / `del session[...]` delta
   lines (`render-ctx-delta`), so the embed and the deltas are one coherent
   Python story. Returns nil when there is nothing to show."
  [{:keys [ctx warnings]}]
  (let [m (project-ctx-static (eng/session-view ctx warnings))]
    (when (seq m)
      (str "```python\n"
        "# Your live session context (read-only — never reassign `session`).\n"
        "# The host keeps it current; mid-session changes arrive as later\n"
        "# `session[...] = …` / `del session[...]` lines. Tool results live in `r`, not here.\n"
        "session = " (env/ctx->python-str m) "\n"
        "```"))))

(defn ctx-static-map
  "The standing ctx as a MAP (`project-ctx-static`) — the data behind
   `render-ctx-static`'s FROZEN `<context>` block, and the delta BASELINE seed.
   Utilization-free so the cached system prefix never churns."
  [{:keys [ctx warnings]}]
  (project-ctx-static (eng/session-view ctx warnings)))

(defn ctx-delta-map
  "Per-iteration CURRENT map for the structural ctx delta: `ctx-static-map`
   PLUS `:utilization`. The frozen system block stays utilization-free (cache
   stability); live token usage instead rides as a cheap appended
   `session[\"utilization\"] = …` delta against the frozen baseline."
  [{:keys [ctx warnings]}]
  (let [view (eng/session-view ctx warnings)
        m    (project-ctx-static view)]
    (cond-> m
      (get view "session_utilization")
      (assoc "utilization" (get view "session_utilization")))))

(defn- ctx-path-str
  "A key path `[\"env\" \"host\" \"os\"]` → the Python subscript chain
   `[\"env\"][\"host\"][\"os\"]`. Ctx keys are canonical STRINGS already —
   no key transform exists between the delta path and the live dict."
  [path]
  (apply str (map #(str "[\"" % "\"]") path)))

(defn- ctx-delta-ops
  "RECURSIVE structural diff of `prev`→`cur` under `path`, as a seq of Python op
   strings. Descends into nested maps so only the leaf (or subtree) that actually
   moved is emitted — `session[\"env\"][\"os\"] = \"linux\"` rather than re-sending the
   whole `env`. Added key → assign its value; removed → `del`; changed map →
   recurse; changed leaf → assign."
  [path prev cur]
  (if (and (map? prev) (map? cur))
    (mapcat (fn [k]
              (let [pv (get prev k ::absent), cv (get cur k ::absent)]
                (cond
                  (= cv ::absent)           [(str "del session" (ctx-path-str (conj path k)))]
                  (= pv cv)                 nil
                  (and (map? pv) (map? cv)) (ctx-delta-ops (conj path k) pv cv)
                  :else                     [(str "session" (ctx-path-str (conj path k))
                                               " = " (env/ctx->python-str cv))])))
      (distinct (concat (keys prev) (keys cur))))
    (when (not= prev cur)
      [(str "session" (ctx-path-str path) " = " (env/ctx->python-str cur))])))

(defn render-ctx-delta
  "Structural Python delta of the standing ctx between the previously-sent map
   `prev` and the current `cur` (both `ctx-static-map` shape). RECURSIVE: emits
   the minimal `session[\"a\"][\"b\"] = <repr>` / `del session[\"a\"][\"b\"]` ops for exactly
   the keys that moved, at any depth — never the whole session. nil when
   nothing changed. Append-only + cache-safe."
  [prev cur]
  (let [ops (ctx-delta-ops [] prev cur)]
    (when (seq ops) (str/join "\n" ops))))

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
   rendering, no hash-gutter file views, no rg grouping.

   `:op` (the call head) is stripped from maps since the call is already
   visible in the assistant replay; `src` is accepted for call-site
   compatibility but no longer affects the output. File reads return `:anchors`
   as an ordered `{\"ln:hash\" text}` map — the key IS the `patch from_anchor`
   — so editing resolves straight off this structured data."
  ^String [_src v]
  (env/ctx->python-str (if (map? v) (dissoc v "op") v)))

