(ns com.blockether.vis.internal.ctx
  "Engine-owned `ctx` snapshot bound under sandbox name `ctx` before every
   model call.

   Two keys, every nil/blank field stripped:

     :conversation {:id :title :turn-id :iteration-id :user-request}
     :defs         {sym {:doc <string?> :shape <malli|fn-shape>}}

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
  "Normalize an error map / Throwable to `{:message :trace :data}`. Drops
   blank/empty fields."
  [err]
  (let [base (cond
               (map? err) {:message (or (:message err) "")
                           :trace   (:trace err)
                           :data    (:data err)}
               (instance? Throwable err)
               (let [^Throwable t err]
                 {:message (or (.getMessage t) (.. t getClass getSimpleName))
                  :data    (ex-data t)})
               :else {:message (str err)})]
    (prune base)))

(defn- provide-shape [value]
  (try (mp/provide [value])
    (catch Throwable _ nil)))

(defn- value-shape
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

   `:conversation` — pre-pruned map `{:id :title :turn-id :iteration-id :user-request}`."
  [{:keys [environment conversation]}]
  (prune
    {:conversation (prune (or conversation {}))
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
