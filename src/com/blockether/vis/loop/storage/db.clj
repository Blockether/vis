(ns com.blockether.vis.loop.storage.db
  "Public persistence contract over the SQLite store. Preserves stable fn
   names + namespaced return shapes so callers across adapters and runtime
   modules can depend on one boundary.

   db-info map layout:
     {:datasource javax.sql.DataSource
      :path        \"/path/to.db\"  ;; nil for :external
      :owned?      bool
      :mode        :temp|:persistent|:external}

   Delegates to the sqlite.* domain namespaces."
  (:require
   [clojure.edn :as edn]
   [com.blockether.vis.loop.runtime.shared :as rt-shared]
   [com.blockether.vis.loop.storage.sqlite.concept-graph :as cg]
   [com.blockether.vis.loop.storage.sqlite.conversations :as conv]
   [com.blockether.vis.loop.storage.sqlite.core :as core]
   [com.blockether.vis.loop.storage.sqlite.corpus :as corpus]
   [com.blockether.vis.loop.storage.sqlite.git :as git]
   [com.blockether.vis.loop.storage.sqlite.search :as search]
   [com.blockether.vis.loop.storage.sqlite.vitality :as vit]
   [edamame.core :as edamame]))

(def ->id core/->id)
(def ->kw core/->kw)
(def ->epoch-ms core/->epoch-ms)

(defn- read-edn-safe [s fallback]
  (if (or (nil? s) (= "" s))
    fallback
    (try (edn/read-string s)
      (catch Exception _ fallback))))

;; -----------------------------------------------------------------------------
;; Connection lifecycle
;; -----------------------------------------------------------------------------

(def create-rlm-conn
  "Open a SQLite RLM store. See sqlite.core/open-store for db-spec forms."
  core/open-store)

(def dispose-rlm-conn!
  "Close an RLM store. Idempotent. See sqlite.core/close-store."
  core/close-store)

;; -----------------------------------------------------------------------------
;; Corpus metadata
;; -----------------------------------------------------------------------------

(def get-corpus-revision    corpus/get-corpus-revision)
(def bump-corpus-revision!  corpus/bump-corpus-revision!)

;; -----------------------------------------------------------------------------
;; Entity / Conversation / Query / Iteration
;; -----------------------------------------------------------------------------

(def store-entity!     core/store-entity!)

(def store-conversation!              conv/store-conversation!)
(def db-get-conversation              conv/db-get-conversation)
(def db-resolve-conversation-ref      conv/db-resolve-conversation-ref)

(def store-query!   conv/store-query!)
(def update-query!  conv/update-query!)

(def store-iteration!        conv/store-iteration!)
(def db-list-iteration-vars  conv/db-list-iteration-vars)

(def delete-entity-tree!               core/delete-entity-tree!)

(def db-list-conversation-queries  conv/db-list-conversation-queries)
(def db-list-query-iterations      conv/db-list-query-iterations)
(def db-list-queries               corpus/db-list-queries)
(def db-document-page-nodes-full   corpus/db-document-page-nodes-full)

;; -----------------------------------------------------------------------------
;; Derived views over conversations / queries / iterations
;; -----------------------------------------------------------------------------

(def ^:private DEF_LIKE_OPS '#{def defn defn- defonce defmulti defmacro})

(defn- parse-forms-safe [code]
  (try (edamame/parse-string-all (or code "") {:all true})
    (catch Exception _ [])))

(defn- form->defined-symbol [form]
  (when (seq? form)
    (let [[op sym & _] form]
      (when (and (contains? DEF_LIKE_OPS op) (symbol? sym))
        sym))))

(defn- iteration->defined-symbols [iteration]
  (->> (read-edn-safe (:code iteration) [])
    (mapcat parse-forms-safe)
    (keep form->defined-symbol)
    distinct
    vec))

(defn db-query-history
  "Ordered query history for a conversation with compact summaries."
  [db-info conversation-ref]
  (let [queries (db-list-conversation-queries db-info conversation-ref)]
    (mapv (fn [idx query]
            (let [qref [:id (:id query)]
                  iterations (db-list-query-iterations db-info qref)
                  key-vars (->> iterations (mapcat iteration->defined-symbols) distinct vec)
                  answer-preview (rt-shared/truncate
                                   (or (some-> (:answer query) (read-edn-safe nil) str)
                                     (:answer query)
                                     (some-> iterations last :answer)
                                     "")
                                   160)]
              {:query-pos idx
               :query-ref qref
               :query-id (:id query)
               :created-at (:created-at query)
               :text (:text query)
               :status (:status query)
               :iterations (count iterations)
               :answer-preview answer-preview
               :key-vars key-vars}))
      (range)
      queries)))

(defn db-query-code
  "Ordered code blocks for a query with metadata."
  [db-info query-ref]
  (->> (db-list-query-iterations db-info query-ref)
    (map-indexed (fn [iter-pos iter]
                   {:iteration-pos iter-pos
                    :created-at (:created-at iter)
                    :code (read-edn-safe (:code iter) [])
                    :answer (:answer iter)}))
    vec))

(defn db-query-results
  "Ordered result blocks for a query with vars + answer."
  [db-info query-ref]
  (->> (db-list-query-iterations db-info query-ref)
    (map-indexed (fn [iter-pos iter]
                   (let [iref [:id (:id iter)]]
                     {:iteration-pos iter-pos
                      :created-at (:created-at iter)
                      :results (read-edn-safe (:results iter) [])
                      :vars (db-list-iteration-vars db-info iref)
                      :answer (:answer iter)})))
    vec))

(defn db-latest-var-registry
  "Builds latest restorable var registry for a conversation (last write wins)."
  ([db-info conversation-ref] (db-latest-var-registry db-info conversation-ref {}))
  ([db-info conversation-ref {:keys [max-scan-queries]}]
   (let [ordered-queries (sort-by :created-at (db-list-conversation-queries db-info conversation-ref))
         queries (if max-scan-queries
                   (take-last (max 0 (long max-scan-queries)) ordered-queries)
                   ordered-queries)]
     (reduce (fn [acc query]
               (let [qref [:id (:id query)]]
                 (reduce (fn [m iter]
                           (reduce (fn [m2 {:keys [name value code]}]
                                     (if name
                                       (let [sym (symbol name)
                                             prev-version (get-in m2 [sym :version] 0)]
                                         (assoc m2 sym
                                           {:value value :code code
                                            :query-id (:id query)
                                            :query-ref qref
                                            :iteration-id (:id iter)
                                            :created-at (:created-at iter)
                                            :version (inc prev-version)}))
                                       m2))
                             m
                             (db-list-iteration-vars db-info [:id (:id iter)])))
                   acc
                   (db-list-query-iterations db-info qref))))
       {}
       queries))))

(defn diffable-value?
  "True when `v` is a collection type where editscript produces a useful
   structural diff (maps, vectors, sets, lists).  Primitives (strings,
   numbers, keywords, booleans, nil) yield only whole-value replacement
   so they are NOT diffable."
  [v]
  (or (map? v) (vector? v) (set? v) (sequential? v)))

(defn db-var-history
  "Returns all versions of a named var across a conversation, oldest first.
   Each entry: {:version N :value <edn> :code str :diffable? bool
                :query-id uuid :created-at inst}."
  [db-info conversation-ref var-sym]
  (let [var-name (str var-sym)
        queries  (sort-by :created-at (db-list-conversation-queries db-info conversation-ref))]
    (->> queries
      (mapcat (fn [query]
                (let [qref [:id (:id query)]]
                  (->> (db-list-query-iterations db-info qref)
                    (mapcat (fn [iter]
                              (->> (db-list-iteration-vars db-info [:id (:id iter)])
                                (filter #(= (:name %) var-name))
                                (map (fn [{:keys [value code]}]
                                       {:query-id   (:id query)
                                        :created-at (:created-at iter)
                                        :value      value
                                        :code       code
                                        :diffable?  (diffable-value? value)})))))))))
      (map-indexed (fn [i entry] (assoc entry :version (inc i))))
      vec)))

;; -----------------------------------------------------------------------------
;; Documents + TOC + raw documents
;; -----------------------------------------------------------------------------

(def db-list-documents             corpus/db-list-documents)
(def store-document!               corpus/store-document!)
(def db-store-pageindex-document!  corpus/db-store-pageindex-document!)

(def db-store-toc-entry!    corpus/db-store-toc-entry!)
(def db-search-toc-entries  search/db-search-toc-entries)
(def db-get-toc-entry       corpus/db-get-toc-entry)
(def db-list-toc-entries    corpus/db-list-toc-entries)

;; -----------------------------------------------------------------------------
;; Pages + page nodes (search / list / get)
;; -----------------------------------------------------------------------------

(def db-get-page-node        corpus/db-get-page-node)
(def db-list-page-nodes      corpus/db-list-page-nodes)
(def db-search-page-nodes    search/db-search-page-nodes)
(def db-search-batch         search/db-search-batch)

;; -----------------------------------------------------------------------------
;; Git / repos / commits
;; -----------------------------------------------------------------------------

(def db-store-repo!        git/db-store-repo!)
(def db-list-repos         git/db-list-repos)
(def db-repo-stats         git/db-repo-stats)
(def db-get-repo-by-name   git/db-get-repo-by-name)

(def db-search-commits     git/db-search-commits)
(def db-commit-shas        git/db-commit-shas)
(def db-commit-by-sha      git/db-commit-by-sha)
(def store-commit-entity!  git/store-commit-entity!)

;; -----------------------------------------------------------------------------
;; Vitality / Q-values / Cooccurrence
;; -----------------------------------------------------------------------------

(def get-page-vitality      vit/get-page-vitality)

(def get-page-q-value       corpus/get-page-q-value)
(def pages-accessed-since   corpus/pages-accessed-since)
(def finalize-q-updates!    corpus/finalize-q-updates!)

(def record-page-access!     vit/record-page-access!)

(def record-cooccurrence!        vit/record-cooccurrence!)
(def record-cooccurrences!       vit/record-cooccurrences!)
(def batch-cooccurrence-boosts   vit/batch-cooccurrence-boosts)
(def get-cooccurrence-boost      vit/get-cooccurrence-boost)
(def recently-accessed-page-ids  vit/recently-accessed-page-ids)

;; -----------------------------------------------------------------------------
;; Document certainty (Bayesian)
;; -----------------------------------------------------------------------------

(def document-certainty           vit/document-certainty)
(def record-document-access!      vit/record-document-access!)
(def decay-document-certainty!    vit/decay-document-certainty!)
(def reindex-certainty-jump!      vit/reindex-certainty-jump!)

;; -----------------------------------------------------------------------------
;; Skills
;; -----------------------------------------------------------------------------

(def ingest-skills!        corpus/ingest-skills!)
(def delete-skill-entity!  corpus/delete-skill-entity!)

;; -----------------------------------------------------------------------------
;; Claims (citation verification)
;; -----------------------------------------------------------------------------

(def db-cited-page-ids     corpus/db-cited-page-ids)

;; -----------------------------------------------------------------------------
;; Refinement / stats helpers consumed by core.clj + query.clj
;; -----------------------------------------------------------------------------

(def db-stored-docs-for-refinement  corpus/db-stored-docs-for-refinement)
(def db-count-document-pages        corpus/db-count-document-pages)
(def db-entity-type-counts          corpus/db-entity-type-counts)

;; -----------------------------------------------------------------------------
;; QA manifest corpus helpers
;; -----------------------------------------------------------------------------

(def qa-corpus-documents    corpus/qa-corpus-documents)
(def qa-corpus-toc-entries  corpus/qa-corpus-toc-entries)
(def qa-corpus-page-nodes   corpus/qa-corpus-page-nodes)

;; -----------------------------------------------------------------------------
;; Concept graph
;; -----------------------------------------------------------------------------

(def clear-concept-graph!     cg/clear-concept-graph!)
(def store-concept!           cg/store-concept!)
(def store-concept-alias!     cg/store-concept-alias!)
(def store-concept-source!    cg/store-concept-source!)
(def store-concept-edge!      cg/store-concept-edge!)
(def list-concepts            cg/list-concepts)
(def list-concept-aliases     cg/list-concept-aliases)
(def list-concept-sources     cg/list-concept-sources)
(def list-concept-edges       cg/list-concept-edges)
(def load-full-concept-graph  cg/load-full-concept-graph)
(def store-page-concept!      cg/store-page-concept!)
(def list-page-concepts       cg/list-page-concepts)
(def clear-page-concepts-for-page! cg/clear-page-concepts-for-page!)
(def get-page-content-sha    cg/get-page-content-sha)
(def update-page-content-sha! cg/update-page-content-sha!)
(def set-concept-status!      cg/set-concept-status!)
(def update-concept!          cg/update-concept!)
