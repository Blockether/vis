(ns com.blockether.vis.loop.knowledge.ontology
  "Cross-document concept graph — BridgeRAG-style two-phase architecture.

   Phase 1 (index time): extract-page-concepts!
     Per-page concept extraction grounded to actual page nodes + content SHA.
     Called during PageIndex ingestion. Results go to `page_concept` table.

   Phase 2 (bridge time): build-concept-graph!
     Reads grounded page_concept rows, cross-links across documents,
     persists to concept/concept_edge tables. Skips user_edited concepts.

   User refinement:
     set-concept-status! — mark concepts as removed or user_edited.
     update-concept! — change definition/group (marks user_edited).
     Rebuild preserves user_edited concepts, hides removed ones."
  (:require
   [clojure.string :as str]
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.vis.loop.storage.db :as db]
   [com.blockether.svar.internal.util :as util]
   [taoensso.trove :as trove])
  (:import
   [java.security MessageDigest]))

;; =============================================================================
;; Content hashing
;; =============================================================================

(defn content-sha
  "SHA-256 of a string. Returns \"sha256:hex\" or nil for blank input."
  [^String s]
  (when (and s (not (str/blank? s)))
    (let [digest (MessageDigest/getInstance "SHA-256")
          bytes  (.getBytes s "UTF-8")]
      (.update digest bytes)
      (str "sha256:" (apply str (map #(format "%02x" %) (.digest digest)))))))

;; =============================================================================
;; Phase 1: Per-page concept extraction (index time)
;; =============================================================================

(def ^:private PAGE_CONCEPTS_SPEC
  {:concepts
   {:type :array
    :items {:term       {:type :string}
            :definition {:type :string}
            :group      {:type :string}
            :excerpt    {:type :string}}}})

(defn extract-page-concepts!
  "Extract concepts from a single page's content. Called at index time.

   Params:
   `rlm-router`  — LLM router.
   `db-info`     — DB handle.
   `document-id` — Document ID (from page table).
   `page-id`     — Page ID (from page table).
   `node-id`     — Best representative node ID for this page (first text node).
   `page-content`— Combined text content of the page.

   Persists to page_concept table with content SHA for change detection.
   Returns count of concepts extracted."
  [rlm-router db-info document-id page-id node-id page-content]
  (when (and rlm-router (not (str/blank? page-content)))
    (let [sha (content-sha page-content)
          truncated (if (> (count page-content) 6000)
                      (subs page-content 0 6000) page-content)]
      (try
        (let [response (llm/ask! rlm-router
                         {:spec     PAGE_CONCEPTS_SPEC
                          :messages [(llm/system
                                       "Extract key domain concepts from this page content.
For each concept: term (canonical name), definition (ONE sentence),
group (cluster like 'Core Theory', 'Assessment', 'Treatment'),
excerpt (short grounding quote from the text).
Only domain-specific terms. Skip generic words. 3-10 concepts max.")
                                    (llm/user truncated)]
                          :routing  {:optimize :cost}})
              concepts (or (:concepts (:result response)) [])]
          ;; Store page SHA for change detection
          (db/update-page-content-sha! db-info page-id sha)
          ;; Store each concept
          (doseq [c concepts]
            (db/store-page-concept! db-info
              {:document-id document-id
               :page-id     page-id
               :node-id     node-id
               :term        (:term c)
               :definition  (:definition c)
               :excerpt     (:excerpt c)
               :page-sha    sha}))
          (count concepts))
        (catch Exception e
          (trove/log! {:level :warn
                       :data {:page-id page-id :error (ex-message e)}
                       :msg "Page concept extraction failed"})
          0)))))

;; =============================================================================
;; Phase 2: Cross-document linking (bridge time)
;; =============================================================================

(def ^:private CROSS_LINK_SPEC
  {:concepts
   {:type :array
    :items {:term       {:type :string}
            :definition {:type :string}
            :group      {:type :string}
            :aliases    {:type :array :items {:type :string}}}}
   :relationships
   {:type :array
    :items {:from        {:type :string}
            :to          {:type :string}
            :type        {:type :string}
            :description {:type :string}
            :cardinality {:type :string}}}
   :ambiguities
   {:type :array
    :items {:term       {:type :string}
            :issue      {:type :string}
            :resolution {:type :string}}}})

(defn- cross-link-concepts!
  "Merge page concepts across documents: deduplicate, find relationships, flag ambiguities."
  [rlm-router all-page-concepts]
  (try
    (let [;; Group by term, summarize
          by-term (group-by :term all-page-concepts)
          summary (str/join "\n"
                    (map-indexed
                      (fn [i [term cs]]
                        (let [docs (distinct (map :document_id cs))
                              def1 (or (:definition (first cs)) "no definition")]
                          (str (inc i) ". **" term "** — " def1
                            " [" (count docs) " doc(s)]")))
                      (take 100 by-term)))
          response (llm/ask! rlm-router
                     {:spec     CROSS_LINK_SPEC
                      :messages [(llm/system
                                   "You are building a unified domain ontology from concepts extracted across multiple document pages.

1. MERGE duplicates — pick one canonical term, list others as aliases
2. DEFINE relationships — types: related_to, part_of, produces, belongs_to, is_a, contrasts_with, requires
   Include cardinality: one-to-one, one-to-many, many-to-many
3. FLAG ambiguities — same term meaning different things

Output 15-50 canonical concepts. Be opinionated — pick the BEST term.")
                                 (llm/user summary)]
                      :routing  {:optimize :intelligence}})]
      (or (:result response) {:concepts [] :relationships [] :ambiguities []}))
    (catch Exception e
      (trove/log! {:level :warn :data {:error (ex-message e)} :msg "Cross-linking failed"})
      {:concepts [] :relationships [] :ambiguities []})))

(defn- persist-concept-graph!
  "Write the merged concept graph to the DB.
   Clears existing auto-generated concepts but preserves user_edited ones."
  [db-info merged-graph page-concepts]
  ;; Delete only auto-generated concepts (status=active), keep user_edited
  (let [existing (db/list-concepts db-info)
        user-edited (set (map :id (filter #(= "user_edited" (:status %)) existing)))]
    ;; Clear non-user-edited concepts and their dependents
    (doseq [c existing
            :when (not (user-edited (:id c)))]
      (db/set-concept-status! db-info (:id c) "removed"))
    ;; Actually delete removed
    (when-let [ds (:datasource (:db-info db-info))]
      ;; Just clear and rebuild non-user-edited
      nil))
  ;; Simpler: clear all non-user-edited, re-insert
  (let [existing    (db/list-concepts db-info)
        user-terms  (set (map :term (filter #(= "user_edited" (:status %)) existing)))
        concept-ids (atom {})]
    ;; Keep user_edited concepts in the map
    (doseq [c existing
            :when (= "user_edited" (:status c))]
      (swap! concept-ids assoc (:term c) (:id c)))
    ;; Clear auto-generated
    (doseq [c existing
            :when (not= "user_edited" (:status c))]
      ;; Delete via cascade
      (when (db/set-concept-status! db-info (:id c) "removed")
        nil))
    ;; Store new concepts (skip user_edited terms)
    (doseq [c (:concepts merged-graph)
            :when (not (user-terms (:term c)))]
      (let [cid (db/store-concept! db-info
                  {:term       (:term c)
                   :definition (:definition c)
                   :group-name (:group c)})]
        (swap! concept-ids assoc (:term c) cid)
        ;; Aliases
        (doseq [a (or (:aliases c) [])]
          (db/store-concept-alias! db-info
            {:concept-id cid :alias a :reason "Alias to avoid"}))
        ;; Sources from grounded page concepts
        (let [sources (filter #(= (:term %) (:term c)) page-concepts)]
          (doseq [s sources]
            (db/store-concept-source! db-info
              {:concept-id  cid
               :document-id (:document_id s)
               :page-index  nil
               :excerpt     (:excerpt s)})))))
    ;; Edges
    (doseq [r (:relationships merged-graph)]
      (let [src-id (get @concept-ids (:from r))
            tgt-id (get @concept-ids (:to r))]
        (when (and src-id tgt-id)
          (db/store-concept-edge! db-info
            {:source-concept-id src-id
             :target-concept-id tgt-id
             :relationship-type (:type r)
             :description       (:description r)
             :cardinality       (:cardinality r)}))))
    {:concepts-stored  (count (remove #(user-terms (:term %)) (:concepts merged-graph)))
     :user-preserved   (count user-terms)
     :edges-stored     (count (filter #(and (get @concept-ids (:from %))
                                         (get @concept-ids (:to %)))
                                (:relationships merged-graph)))
     :ambiguities      (count (or (:ambiguities merged-graph) []))}))

;; =============================================================================
;; Markdown (read-only rendering)
;; =============================================================================

(defn concept-graph->markdown
  "Render a concept graph (from DB) as DDD ubiquitous language markdown."
  [{:keys [concepts edges]} & [{:keys [ambiguities]}]]
  (let [sb (StringBuilder.)]
    (.append sb "# Concept Graph\n\n")
    (let [groups (group-by :group_name concepts)
          sorted-groups (sort-by key groups)]
      (doseq [[group-name group-concepts] sorted-groups]
        (.append sb (str "## " (or group-name "Uncategorized") "\n\n"))
        (.append sb "| Term | Definition | Aliases to avoid |\n")
        (.append sb "| ---- | ---------- | ---------------- |\n")
        (doseq [c (sort-by :term group-concepts)]
          (let [aliases (str/join ", " (map :alias (:aliases c)))]
            (.append sb (str "| **" (:term c) "** | "
                          (or (:definition c) "") " | "
                          (if (str/blank? aliases) "" aliases) " |\n"))))
        (.append sb "\n")))
    (when (seq edges)
      (.append sb "## Relationships\n\n")
      (doseq [e edges]
        (.append sb (str "- **" (:source_term e) "** "
                      (str/replace (or (:relationship_type e) "") "_" " ")
                      " **" (:target_term e) "**"
                      (when (:cardinality e) (str " (" (:cardinality e) ")"))
                      (when (:description e) (str " — " (:description e)))
                      "\n")))
      (.append sb "\n"))
    (when (seq ambiguities)
      (.append sb "## Flagged Ambiguities\n\n")
      (doseq [a ambiguities]
        (.append sb (str "- **" (:term a) "**: " (:issue a)
                      (when (:resolution a) (str " -> " (:resolution a)))
                      "\n")))
      (.append sb "\n"))
    (str sb)))

(defn export-concept-graph-md!
  "Load the concept graph from DB and write to `output-path`."
  [db-info output-path & [ambiguities]]
  (let [graph (db/load-full-concept-graph db-info)
        md    (concept-graph->markdown graph {:ambiguities ambiguities})]
    (spit output-path md)
    {:path output-path :concepts (count (:concepts graph)) :edges (count (:edges graph))}))

;; =============================================================================
;; System prompt
;; =============================================================================

(defn concept-graph-for-prompt
  "Format the concept graph as a compact system prompt section.
   Groups list terms (~75 tokens). Cross-group edges shown as bridges.
   Within-group relationships discoverable via (concept-info \"Term\").
   Returns nil if no concepts exist."
  [db-info]
  (when-let [concepts (seq (db/list-concepts db-info))]
    (let [groups    (group-by :group_name concepts)
          term->grp (into {} (map (juxt :term :group_name)) concepts)
          edges     (db/list-concept-edges db-info)
          bridges   (filter #(not= (term->grp (:source_term %))
                               (term->grp (:target_term %)))
                      edges)
          sb        (StringBuilder.)]
      (.append sb "<concept-graph>\n")
      (.append sb "Domain terms. (concept-info \"Term\") for definition.\n")
      (doseq [[group-name cs] (sort-by key groups)]
        (.append sb (str (or group-name "Other") ": "
                      (str/join ", " (map :term (sort-by :term cs))) "\n")))
      (when (seq bridges)
        (.append sb (str "Bridges: "
                      (str/join " | "
                        (map #(str (:source_term %)
                                " ->" (str/replace (or (:relationship_type %) "") "_" "-")
                                "-> " (:target_term %))
                          (take 25 bridges)))
                      "\n")))
      (.append sb "</concept-graph>")
      (str sb))))

;; =============================================================================
;; SCI sandbox tool
;; =============================================================================

(defn make-concept-graph-bindings
  "Return SCI sandbox binding: concept-info (go-to-definition)."
  [db-info]
  (when db-info
    {'concept-info
     (fn concept-info [term]
       (let [concepts (db/list-concepts db-info)
             match    (or (first (filter #(= (:term %) term) concepts))
                       (let [all (db/load-full-concept-graph db-info)]
                         (first (filter (fn [c]
                                          (some #(= (:alias %) term)
                                            (:aliases c)))
                                  (:concepts all)))))]
         (when match
           (let [cid (:id match)]
             {:term       (:term match)
              :definition (:definition match)
              :group      (:group_name match)
              :status     (:status match)
              :aliases    (mapv :alias (db/list-concept-aliases db-info cid))
              :sources    (mapv (fn [s]
                                 (cond-> {:document-id (:document_id s)}
                                   (:page_index s) (assoc :page (:page_index s))
                                   (:excerpt s)    (assoc :excerpt (:excerpt s))))
                            (db/list-concept-sources db-info cid))}))))}))

;; =============================================================================
;; Public API
;; =============================================================================

(defn build-concept-graph!
  "Build a cross-document concept graph from grounded page concepts.

   Reads page_concept rows (extracted at index time by extract-page-concepts!),
   cross-links them across documents, persists to concept/concept_edge tables.
   Preserves user_edited concepts — rebuild won't overwrite them.

   Params:
   `env`  — RLM environment with documents ingested.
   `opts` — Optional map (reserved for future options).

   Returns:
   {:concepts-stored N :edges-stored N :ambiguities N :user-preserved N}"
  [env & [_opts]]
  (let [db-info    (:db-info env)
        rlm-router (:router env)]
    (when-not db-info
      (throw (ex-info "No DB in environment" {:type :rlm/invalid-env})))
    (when-not rlm-router
      (throw (ex-info "No router in environment" {:type :rlm/invalid-env})))
    (let [page-concepts (db/list-page-concepts db-info)
          _             (trove/log! {:level :info :id ::build-start
                                     :data {:page-concepts (count page-concepts)}
                                     :msg "Building concept graph from grounded page concepts"})
          merged        (if (seq page-concepts)
                          (cross-link-concepts! rlm-router page-concepts)
                          {:concepts [] :relationships [] :ambiguities []})
          stats         (persist-concept-graph! db-info merged page-concepts)]
      (trove/log! {:level :info :id ::build-done :data stats
                   :msg "Concept graph built"})
      stats)))
