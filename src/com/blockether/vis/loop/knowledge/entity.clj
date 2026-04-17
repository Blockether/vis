(ns com.blockether.vis.loop.knowledge.entity
  "Entity extraction from PageIndex documents using LLM."
  (:require
   [clojure.string :as str]
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.vis.loop.storage.schema :refer [bytes->base64]]
   [taoensso.trove :as trove]))

(def ^:private ENTITY_EXTRACTION_SPEC
  {:entities {:type :array}
   :relationships {:type :array}})

(def ^:private ENTITY_EXTRACTION_OBJECTIVE
  "Extract entities and relationships from the provided content.")

(defn extract-entities-from-page!
  "Extracts entities from a page's text nodes using LLM.

   Params:
   `text-content` - String. Combined text from page nodes.
   `rlm-router` - Router from llm/make-router.

   Returns:
   Map with :entities and :relationships keys (empty if extraction fails)."
  [text-content rlm-router]
  (try
    (let [truncated (if (> (count text-content) 8000) (subs text-content 0 8000) text-content)
          response (llm/ask! rlm-router {:spec ENTITY_EXTRACTION_SPEC
                                         :messages [(llm/system ENTITY_EXTRACTION_OBJECTIVE)
                                                    (llm/user truncated)]
                                         :routing {:optimize :cost}})]
      (or (:result response) {:entities [] :relationships []}))
    (catch Exception e
      (trove/log! {:level :warn :data {:error (ex-message e)} :msg "Entity extraction failed for page"})
      {:entities [] :relationships []})))

(defn extract-entities-from-visual-node!
  "Extracts entities from a visual node (image/table) using vision or text.

   Params:
   `node` - Map. Page node with :type, :image-data, :description.
   `rlm-router` - Router from llm/make-router.

   Returns:
   Map with :entities and :relationships keys (empty if extraction fails)."
  [node rlm-router]
  (try
    (let [image-data (:image-data node)
          description (:description node)]
      (cond
        image-data
        (let [b64 (bytes->base64 image-data)
              response (llm/ask! rlm-router {:spec ENTITY_EXTRACTION_SPEC
                                             :messages [(llm/system ENTITY_EXTRACTION_OBJECTIVE)
                                                        (llm/user (or description "Extract entities from this image")
                                                          (llm/image b64 "image/png"))]
                                             :routing {:optimize :cost}})]
          (or (:result response) {:entities [] :relationships []}))
        description
        (let [response (llm/ask! rlm-router {:spec ENTITY_EXTRACTION_SPEC
                                             :messages [(llm/system ENTITY_EXTRACTION_OBJECTIVE)
                                                        (llm/user description)]
                                             :routing {:optimize :cost}})]
          (or (:result response) {:entities [] :relationships []}))
        :else
        (do (trove/log! {:level :warn :msg "Visual node has no image-data or description, skipping"})
          {:entities [] :relationships []})))
    (catch Exception e
      (trove/log! {:level :warn :data {:error (ex-message e)} :msg "Visual node extraction failed"})
      {:entities [] :relationships []})))

(defn extract-entities-from-document!
  "Extracts entities from all pages of a document.

   Params:
   `db-info` - Map. Database info with :store key.
   `document` - Map. PageIndex document.
   `rlm-router` - Router from llm/make-router.
   `opts` - Map. Options with :max-extraction-pages, :max-vision-rescan-nodes.

   Returns:
   Map with extraction statistics."
  [_db-info document rlm-router opts]
  (let [max-pages (or (:max-extraction-pages opts) 50)
        max-vision (or (:max-vision-rescan-nodes opts) 10)
        pages (take max-pages (:pages document))
        entities-atom (atom [])
        relationships-atom (atom [])
        errors-atom (atom 0)
        vision-count-atom (atom 0)]
    (doseq [page pages]
      (let [nodes (:nodes page)
            text-nodes (filter #(not (#{:image :table} (:type %))) nodes)
            visual-nodes (filter #(#{:image :table} (:type %)) nodes)]
        (when (seq text-nodes)
          (let [text (str/join "\n" (keep :content text-nodes))]
            (when (not (str/blank? text))
              (try
                (let [result (extract-entities-from-page! text rlm-router)]
                  (swap! entities-atom into (:entities result))
                  (swap! relationships-atom into (:relationships result)))
                (catch Exception e
                  (trove/log! {:level :warn :data {:page (:index page) :error (ex-message e)}
                               :msg "Entity extraction failed for page"})
                  (swap! errors-atom inc))))))
        (doseq [vnode visual-nodes]
          (when (< @vision-count-atom max-vision)
            (try
              (let [result (extract-entities-from-visual-node! vnode rlm-router)]
                (swap! vision-count-atom inc)
                (swap! entities-atom into (:entities result))
                (swap! relationships-atom into (:relationships result)))
              (catch Exception e
                (trove/log! {:level :warn :data {:node-type (:type vnode) :error (ex-message e)}
                             :msg "Entity extraction failed for visual node"})
                (swap! errors-atom inc)))))))
    (let [entities @entities-atom
          relationships @relationships-atom]
      {:entities-extracted (count entities)
       :relationships-extracted (count relationships)
       :pages-processed (count pages)
       :extraction-errors @errors-atom
       :visual-nodes-scanned @vision-count-atom})))
