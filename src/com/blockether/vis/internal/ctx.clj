(ns com.blockether.vis.internal.ctx
  "Engine-owned `ctx` snapshot bound under sandbox name `ctx` before every
   model call.

   Three keys, every nil/blank field stripped:

     :conversation {:id :title :turn-id :user-request}
     :iteration    {:id :position}
     :defs         {sym {:doc <string?> :shape <malli|fn-shape>}}

   `:iteration` lives at the top level so `(:iteration ctx)` works directly
   and the per-iteration scope is not buried inside the conversation.

   `:defs` is an ordered map; newest sym first. History (prior turns,
   iterations, code, errors, stdout/stderr) is reachable via foundation
   tools, not embedded in ctx.

   No previews. No raw values. Real values live in the SCI sandbox; the
   model derefs symbols when it needs them.

   Shape inference uses `malli.provider/provide` cached by
   `System/identityHashCode` of the value."
  (:require
   [clojure.string :as str]
   [malli.provider :as mp]))

(def ^:private hidden-syms
  '#{ctx done set-conversation-title!})

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

   `:conversation` — pre-pruned map `{:id :title :turn-id :user-request}`.
   `:iteration`    — pre-pruned map `{:id :position}`."
  [{:keys [environment conversation iteration]}]
  (prune
    {:conversation (prune (or conversation {}))
     :iteration    (prune (or iteration {}))
     :defs         (or (build-defs (:sci-ctx environment)
                         (:initial-ns-keys environment))
                     {})}))

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
;; (code lines, per-form `;; => shape`, `;; ! stdout> ...`, `;; ! ERROR ...`).
;; The whole block forms one user-message trailer the model sees between its
;; own assistant replays, plus the fresh `ctx` snapshot at the end.
;; =============================================================================

(defn- side-effect-lines
  [{:keys [stdout stderr]}]
  (cond-> []
    (non-blank? stdout)
    (into (mapv #(str ";; ! stdout> " %) (str/split-lines stdout)))
    (non-blank? stderr)
    (into (mapv #(str ";; ! stderr> " %) (str/split-lines stderr)))))

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

(defn- bounded-pr-str
  [v]
  (let [s (try (pr-str v) (catch Throwable _ ""))
        n (count s)]
    (if (<= n TRAILER_VALUE_MAX_CHARS)
      s
      (str (subs s 0 TRAILER_VALUE_MAX_CHARS)
        "…<+" (- n TRAILER_VALUE_MAX_CHARS) " chars—re-deref the binding for the full value>"))))

(defn- form-result-lines
  "Render `;; => <pr-str of value>` plus a `;; => shape <malli>` line so the
   model sees both the data it just produced and a structural view of it."
  [value shape]
  (cond-> []
    (some? shape) (conj (str ";; => shape " (pr-str shape)))
    (some? value) (conj (str ";; => " (bounded-pr-str value)))))

(defn- per-form-block-lines
  "Render lines for a single per-form entry (:source :result :stdout :stderr
   :error). Each form gets its own value preview, shape, stdout, stderr, and
   error so the model never loses sight of what each form produced."
  [cache-key position idx {:keys [source result stdout stderr error]}]
  (let [shape (when (and (some? result) (nil? error))
                (value-shape cache-key (str "__iter" position "__form" idx) result))]
    (concat
      [source]
      (form-result-lines (when (nil? error) result) shape)
      (side-effect-lines {:stdout stdout :stderr stderr})
      (error-lines error))))

(defn- iteration->repl-text
  "Render one prior iteration as a REPL transcript block. When the engine
   captured per-form outcomes (`:forms`), each form is rendered with its own
   value/shape/stdout/stderr/error. Falls back to whole-block render when the
   parser rejected the source."
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
                                  (:result block))
                         shape  (when result
                                  (value-shape cache-key
                                    (str "__iter" position "__") result))]
                     (concat
                       [code]
                       (form-result-lines result shape)
                       (side-effect-lines block)
                       (error-lines (:error block)))))]
    (str/join "\n" (concat header body))))

(defn render-iteration-trailer
  "Build the REPL-style user-message body for a model call.

   `:trailer-iters` — vec of `[position {:blocks [...]}]` ordered ascending.
   `:ctx`            — the engine ctx snapshot map for the upcoming iteration.

   Returns the full trailer string: prior iterations as REPL transcripts,
   followed by `;; ctx = <edn>` for the fresh state. When no prior iterations
   exist, only the ctx block is emitted."
  [{:keys [environment trailer-iters ctx]}]
  (let [cache-key (System/identityHashCode (:env (:sci-ctx environment)))
        prior     (mapv (fn [[pos data]] (iteration->repl-text cache-key
                                           {:position pos :blocks (:blocks data)}))
                    (or trailer-iters []))
        ctx-block (str ";; ctx =\n" (pr-str ctx))]
    (str/join "\n\n" (conj prior ctx-block))))
