(ns com.blockether.vis.internal.ctx-renderer
  "Pure renderer for the standing agent-facing `<context>` snapshot.

   `render-ctx` projects the session view with `project-ctx` and prints it via
   the same Python pretty-printer path used to bind the live sandbox `context`
   dict, so the visible block and runtime value share one canonical shape."
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

;; Context rendering is intentionally narrow: stable session identity,
;; workspace/env/routing/resources/symbols, and utilization. Tool outputs are
;; rendered as append-only `<results>` messages by the loop, not projected here.

;; =============================================================================
;; The single value printer
;;
;; The `<context>` STRING is produced canonically by GraalPy: `render-ctx` builds
;; the projected Clojure map, and `env/ctx->python-str` JSON-bridges it into a
;; DEDICATED printer Context where `__vis_pp__` (Python) stringifies it — separate
;; from the eval sandbox Context. So the printed text and the live `context` dict
;; (bound via `env/bind-ctx!` from the SAME projection) cannot drift.
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
  "THE compressed model-facing string for one tool/form VALUE, callable by
   anything that needs to show a stored value to the model (recall windows
   re-render through this so a recalled `cat`/`rg`/shell/git payload reads
   exactly like its original result, not as a pr-str'd map):
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

