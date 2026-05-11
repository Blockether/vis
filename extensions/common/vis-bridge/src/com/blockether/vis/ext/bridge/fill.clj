(ns com.blockether.vis.ext.bridge.fill
  "Bridge fill layer: normalized extraction facts -> extension aggregates.

   Language extractors must stop at normalized facts. This namespace owns the
   storage mapping so every backend persists nodes, edges, indexes, and summaries
   consistently."
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.bridge.schema :as schema]))

(defn- non-blank-string? [x]
  (and (string? x) (not (str/blank? x))))

(s/def ::key non-blank-string?)
(s/def ::kind #{:bridge/node :bridge/edge :bridge/index :bridge/summary})
(s/def ::scope #{:global})
(s/def ::metadata map?)
(s/def ::content map?)
(s/def ::row
  (s/keys :req-un [::key ::kind ::scope ::metadata ::content]))
(s/def ::rows (s/coll-of ::row :kind vector?))

(defn edge-key
  "Stable aggregate key for a Bridge edge. One row per source/kind/target.
   Repeated call-sites should be merged into content metadata before storage."
  [{:keys [source edge-kind target]}]
  (str "edge:" source "::" (name edge-kind) "::" target))

(defn node-key [{:keys [qualified-name]}]
  (str "node:" qualified-name))

(defn index-key [path]
  (str "idx:" path))

(defn- node-metadata [{node-kind :kind node-name :name
                       :keys [path language visibility metadata]}]
  (cond-> {:path path
           :language language
           :kind (name node-kind)
           :name node-name}
    visibility (assoc :visibility (name visibility))
    (map? metadata) (merge (select-keys metadata [:symbol-kind :doc-kind :relevance :layer]))))

(defn- edge-metadata [{:keys [path language edge-kind source target resolved? metadata]}]
  (cond-> {:path path
           :edge-kind (name edge-kind)
           :source source
           :target target}
    language (assoc :language language)
    (some? resolved?) (assoc :resolved? resolved?)
    (map? metadata) (merge (select-keys metadata [:syntax :backend :relevance]))))

(defn node-row
  "Map one normalized Bridge node to an extension aggregate row."
  [node]
  {:key (node-key node)
   :kind :bridge/node
   :scope :global
   :metadata (node-metadata node)
   :content node})

(defn edge-row
  "Map one normalized Bridge edge to an extension aggregate row."
  [edge]
  {:key (edge-key edge)
   :kind :bridge/edge
   :scope :global
   :metadata (edge-metadata edge)
   :content edge})

(defn index-row
  "Build the per-path index aggregate row for an extraction result."
  [path result]
  {:key (index-key path)
   :kind :bridge/index
   :scope :global
   :metadata {:path path
              :language (get-in result [:stats :language])}
   :content {:path path
             :language (get-in result [:stats :language])
             :node-count (count (:nodes result))
             :edge-count (count (:edges result))
             :stats (:stats result)}})

(defn rows-for-result
  "Pure storage mapping. Validates extraction result and returns aggregate rows.
   Does not touch the DB."
  ([result] (rows-for-result result nil))
  ([result {:keys [path]}]
   (schema/assert-extract-result! result)
   (let [rows (vec (concat
                     (map node-row (:nodes result))
                     (map edge-row (:edges result))
                     (when-let [p (or path (get-in result [:stats :path]))]
                       [(index-row p result)])))]
     (when-not (s/valid? ::rows rows)
       (throw (ex-info "Invalid Bridge aggregate rows"
                {:type :bridge.fill/invalid-rows
                 :explain (s/explain-data ::rows rows)})))
     rows)))

(defn fill!
  "Persist an extraction result into Bridge extension aggregates. Must be called
   from a Bridge extension symbol/hook context so `vis/ext-put!` can supply the
   extension id. Returns storage stats."
  ([env result] (fill! env result nil))
  ([env result opts]
   (let [rows (rows-for-result result opts)]
     (doseq [row rows]
       (vis/ext-put! env row))
     {:rows (count rows)
      :nodes (count (:nodes result))
      :edges (count (:edges result))
      :indexes (count (filter #(= :bridge/index (:kind %)) rows))})))
