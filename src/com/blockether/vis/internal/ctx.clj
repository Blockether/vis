(ns com.blockether.vis.internal.ctx
  "Engine-owned `ctx` snapshot bound under sandbox name `ctx` before every
   model call.

   Domain keys, every nil/blank field stripped:

     :conversation {:id :title :turn-id
                    :iteration {:id :position}
                    :hints [{:id :importance :text :satisfy-with}]}
     :llm-provider {:selected :actual :routing :error}
     :project      {:root :warnings}
     :extensions   [{:namespace :alias :doc :kind :registry-id :symbols}]
     :defs         {sym {:doc <string?> :shape <malli|fn-shape>}}

   `:iteration` and `:hints` are nested under `(:conversation ctx)`. The
   current user request is intentionally not duplicated in ctx; it is already
   delivered as the current user message.

   `:defs` is an ordered map; newest sym first. History (prior turns,
   iterations, code, errors) is reachable via foundation tools,
   not embedded in ctx.

   No previews. No raw values. Real values live in the SCI sandbox; the
   model derefs symbols when it needs them.

   Shape inference uses `malli.provider/provide` cached by
   `System/identityHashCode` of the value."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.format :as fmt]
   [malli.provider :as mp]))

(def ^:private hidden-syms
  '#{ctx done set-conversation-title! satisfy-hint!})

(defonce ^:private shape-cache-atom
  ;; { env-id {sym {:identity int :shape <schema> :order long}} }
  (atom {}))

(defonce ^:private order-counter-atom
  ;; { env-id long }
  (atom {}))

(defn- next-order!
  [env-id]
  (-> (swap! order-counter-atom update env-id (fnil inc 0))
    (get env-id)))

(defn- non-blank? [v]
  (and (string? v) (not (str/blank? v))))

(defn- prune
  "Drop keys whose value is nil, empty string, empty coll."
  [m]
  (into {} (remove (fn [[_ v]]
                     (or (nil? v)
                       (and (string? v) (str/blank? v))
                       (and (coll? v) (empty? v))))
             m)))

(defn error-shape
  "Normalize an error map / Throwable to `{:message :trace :data :hint}`.
   Drops blank/empty fields."
  [err]
  (let [base (cond
               (map? err) {:message (or (:message err) "")
                           :trace   (:trace err)
                           :data    (:data err)
                           :hint    (:hint err)}
               (instance? Throwable err)
               (let [^Throwable t err]
                 {:message (or (.getMessage t) (.. t getClass getSimpleName))
                  :data    (ex-data t)})
               :else {:message (str err)})]
    (prune base)))

(defn- provide-shape [value]
  (try (mp/provide [value])
    (catch Throwable _ nil)))

(defn value-shape
  [cache-key sym value]
  (let [id     (System/identityHashCode value)
        cached (get-in @shape-cache-atom [cache-key sym])]
    (if (= id (:identity cached))
      (:shape cached)
      (let [shape (provide-shape value)]
        (swap! shape-cache-atom assoc-in [cache-key sym]
          (merge cached {:identity id :shape shape}))
        shape))))

(defn- safe-meta [v]
  (try (meta v) (catch Throwable _ nil)))

(defn- safe-deref [v]
  (cond
    (instance? clojure.lang.IDeref v) (try @v (catch Throwable _ nil))
    :else v))

(defn- fn-shape [v]
  (let [al (some-> (safe-meta v) :arglists)]
    (cond-> {:type :fn}
      (seq al) (assoc :arglists (vec al)))))

(defn- compute-shape
  [cache-key sym v]
  (let [val (safe-deref v)]
    (cond
      (nil? val) nil
      (fn?  val) (fn-shape v)
      :else      (value-shape cache-key sym val))))

(defn- ensure-order!
  [cache-key sym]
  (or (get-in @shape-cache-atom [cache-key sym :order])
    (let [n (next-order! cache-key)]
      (swap! shape-cache-atom assoc-in [cache-key sym :order] n)
      n)))

(defn- def-entry
  [cache-key [sym v]]
  (let [order (ensure-order! cache-key sym)
        doc   (:doc (safe-meta v))
        shape (compute-shape cache-key sym v)]
    [order sym (prune {:doc   (when (non-blank? doc) doc)
                       :shape shape})]))

(defn- visible-def?
  [initial-ns-keys [sym _]]
  (and (symbol? sym)
    (not (contains? hidden-syms sym))
    (not (contains? initial-ns-keys sym))))

(defn- build-defs
  [sci-ctx initial-ns-keys]
  (when sci-ctx
    (let [cache-key (System/identityHashCode (:env sci-ctx))
          sandbox   (get-in @(:env sci-ctx) [:namespaces 'sandbox])
          entries   (into []
                      (comp
                        (filter (partial visible-def? (or initial-ns-keys #{})))
                        (map (partial def-entry cache-key)))
                      sandbox)]
      ;; newest first by insertion order, then build an array-map so
      ;; iteration preserves order on render.
      (apply array-map
        (mapcat (fn [[_ sym entry]] [sym entry])
          (sort-by first > entries))))))

(defn build
  "Build the engine ctx snapshot.

   `:conversation` — pre-pruned map `{:id :title :turn-id}`, with nested
      `:iteration {:id :position}` and `:hints [...]` when supplied.
      `:user-request` is stripped; current user text lives in the provider
      message, not duplicated in ctx.
   `:llm-provider` — optional provider/model routing and last-error state.
   `:project`      — optional project facts and warnings.
   `:extensions`   — compact active extension summary from prompt/extensions-snapshot."
  [{:keys [environment conversation iteration hints llm-provider project extensions]}]
  (let [iteration* (prune (or iteration {}))
        hints* (vec (or hints []))
        conversation* (cond-> (dissoc (or conversation {}) :user-request)
                        (seq iteration*) (assoc :iteration iteration*)
                        (seq hints*)     (assoc :hints hints*))]
    (prune
      {:conversation (prune conversation*)
       :llm-provider (prune (or llm-provider {}))
       :project      (prune (or project {}))
       :extensions   (vec (or extensions []))
       :defs         (or (build-defs (:sci-ctx environment)
                           (:initial-ns-keys environment))
                       {})})))

(defn forget!
  "Drop the shape + order caches for a sci-ctx."
  [sci-ctx]
  (when sci-ctx
    (let [env-id (System/identityHashCode (:env sci-ctx))]
      (swap! shape-cache-atom dissoc env-id)
      (swap! order-counter-atom dissoc env-id))))

;; =============================================================================
;; REPL-style iteration outcome rendering
;;
;; Each prior iteration of the current turn is rendered as a REPL transcript
;; (code lines, per-form `;; => shape`, `;; ! ERROR ...`). The whole block
;; forms one user-message trailer the model sees between its own assistant
;; replays, plus the fresh `ctx` snapshot at the end.
;; =============================================================================

(defn- error-lines
  [err]
  (when err
    (let [shape   (error-shape err)
          message (:message shape)
          trace   (:trace shape)
          data    (:data shape)
          ;; Diagnostic catalogue can attach a :hint to a parse / eval error
          ;; map (parse-diagnose). Lift the hint into its own line so the
          ;; model sees the precise correction.
          hint    (or (when (map? err) (:hint err)) (:hint shape))]
      (cond-> []
        (non-blank? message) (conj (str ";; ! ERROR " message))
        (non-blank? hint)    (conj (str ";; ! hint " hint))
        (non-blank? trace)
        (into (mapv #(str ";;   " %) (str/split-lines trace)))
        (some? data)
        (conj (str ";; ! data " (pr-str data)))))))

(def ^:private TRAILER_VALUE_MAX_CHARS
  "Per-form bounded value preview in the REPL trailer. The shape is always
   shown; the value is shown up to this cap, then truncated with a marker that
   points the model at the binding for the full value. Generous on purpose:
   model needs to read its own observations."
  20000)

(def ^:private TRAILER_ZPRINT_OPTS
  ;; Width 120 is the trailer wrap point. The trailer never lands in a
  ;; terminal — it goes into a user message read by the model — so width
  ;; only trades "more breaks" against "denser lines". 120 fits typical
  ;; long file paths (e.g. extensions/.../foundation/editing/core.clj) on
  ;; one line and keeps nested maps/vecs readable. Same EDN escape
  ;; semantics as pr-str.
  {:width 120})

(defn- bounded-pr-str
  "Render `v` for the REPL trailer.

   zprint is used so nested maps/vecs land readable; escape semantics match
   pr-str (single edn escape, `\\n` inside strings). Falls back to pr-str
   when zprint throws (rare; cyclic structures or eval-time-only objects).
   Caps at `TRAILER_VALUE_MAX_CHARS`; the marker tells the model to re-deref
   the binding when it needs the full value."
  [v]
  (let [s (try (fmt/safe-zprint-str v TRAILER_ZPRINT_OPTS)
            (catch Throwable _
              (try (pr-str v) (catch Throwable _ ""))))
        ;; zprint appends a trailing newline on top-level values; strip so the
        ;; `;; => ` prefix line concatenation does not break across the boundary.
        s (cond-> s (str/ends-with? s "\n") (subs 0 (dec (count s))))
        n (count s)]
    (if (<= n TRAILER_VALUE_MAX_CHARS)
      s
      (str (subs s 0 TRAILER_VALUE_MAX_CHARS)
        "…<+" (- n TRAILER_VALUE_MAX_CHARS) " chars—re-deref the binding for the full value>"))))

(defn- prefix-continuation-lines
  "Prefix the first line of `s` with `head`, every subsequent line with
   `cont`. Keeps multi-line zprint output inside the `;;` comment column so
   the REPL trailer stays a valid commented transcript."
  [head cont s]
  (let [lines (str/split-lines (str s))]
    (str/join "\n"
      (cons (str head (first lines))
        (map #(str cont %) (rest lines))))))

(defn- form-result-lines
  "Render `;; => <value>` (zprint-pretty, multi-line aware).

   No `;; => shape` line: the actual value sits right below, so a shape
   inferred from that same value duplicates information the model already
   has. Shapes still live on `:defs` in ctx, where the value is intentionally
   not shown (sandbox-state directory)."
  [value]
  (cond-> []
    (some? value) (conj (prefix-continuation-lines ";; => " ";;    "
                          (bounded-pr-str value)))))

(defn- per-form-block-lines
  "Render lines for a single per-form entry (:source :result :error).
   Each form gets its own value and error so the model never loses
   sight of what each form produced."
  [_cache-key _position _idx {:keys [source result error]}]
  (concat
    [source]
    (form-result-lines (when (nil? error) result))
    (error-lines error)))

(defn- iteration->repl-text
  "Render one prior iteration as a REPL transcript block. When the engine
   captured per-form outcomes (`:forms`), each form is rendered with its own
   value/shape/error. Falls back to whole-block render when the parser
   rejected the source."
  [cache-key {:keys [position blocks]}]
  (let [block    (first blocks)
        forms    (:forms block)
        repaired (:repaired-source block)
        header   (cond-> [(str ";; iter " position)]
                   repaired (conj ";; ⚠ engine auto-repaired delimiters with parinferish (see iteration code below; verify the fix matches your intent)"))
        body     (if (seq forms)
                   (mapcat (fn [idx form-entry]
                             (per-form-block-lines cache-key position idx form-entry))
                     (range) forms)
                ;; Fallback: model emitted code the parser rejected. Show whole
                ;; block source + iteration-level outcome.
                   (let [code   (or (:code block) "")
                         result (when (and (not (:error block))
                                        (not= :vis/no-result (:result block)))
                                  (:result block))]
                     (concat
                       [code]
                       (form-result-lines result)
                       (error-lines (:error block)))))]
    (str/join "\n" (concat header body))))

(defn render-iteration-trailer
  "Build the REPL-style user-message body for a model call.

   `:trailer-iters` — vec of `[position {:blocks [...]}]` ordered ascending.
   `:ctx`          — the engine ctx snapshot map for the upcoming iteration,
                    including any conversation `:hints` for that iteration.

   Returns the full trailer string: prior iterations as REPL transcripts,
   followed by `;; ctx = <edn>` for the fresh state. Hint data lives inside
   `(get-in ctx [:conversation :hints])`; there is no XML hint side-channel."
  [{:keys [environment trailer-iters ctx]}]
  (let [cache-key (System/identityHashCode (:env (:sci-ctx environment)))
        prior     (mapv (fn [[pos data]] (iteration->repl-text cache-key
                                           {:position pos :blocks (:blocks data)}))
                    (or trailer-iters []))
        ctx-block (str ";; ctx =\n" (pr-str ctx))]
    (str/join "\n\n" (conj prior ctx-block))))
