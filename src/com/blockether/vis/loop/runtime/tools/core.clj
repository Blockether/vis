(ns com.blockether.vis.loop.runtime.tools.core
  "Core tool factories for the SCI sandbox — always bound when a DB exists.

   Contains:
   - Document tools: search-documents, fetch-document-content, format-docs
   - History tools: conversation-history, conversation-code, conversation-results
   - Restore tools: restore-var, restore-vars"
  (:require
   [clojure.string :as str]
   [com.blockether.vis.loop.storage.db :as db
    :refer [db-get-page-node db-get-toc-entry
            db-search-page-nodes
            db-search-toc-entries record-page-access!]]))

;; =============================================================================
;; Document tools
;; =============================================================================

(def ^:private FETCH_CONTENT_PAGE_SIZE 4000)

(defn- chunk-text [text]
  (let [page-size FETCH_CONTENT_PAGE_SIZE]
    (loop [remaining (str/split text #"\n\n+")
           current [] current-size 0 result []]
      (if (empty? remaining)
        (if (seq current) (conj result (str/join "\n\n" current)) result)
        (let [para (first remaining)
              para-size (count para)]
          (if (and (> current-size 0) (> (+ current-size para-size) page-size))
            (recur remaining [] 0 (conj result (str/join "\n\n" current)))
            (recur (rest remaining) (conj current para) (+ current-size para-size) result)))))))

(defn- append-pages-md [^StringBuilder sb pages]
  (let [by-page (group-by :page-id pages)]
    (doseq [[pid nodes] (sort-by key by-page)]
      (.append sb (str "## " (or pid "unknown") "\n"))
      (doseq [n nodes]
        (let [t (some-> (:type n) name)
              zone (some-> (:vitality-zone n) name)
              preview (or (:preview n) "")]
          (.append sb (str "- **" t "**" (when zone (str " [" zone "]")) " " preview "\n"))))
      (.append sb "\n"))))

(defn- append-toc-md [^StringBuilder sb toc]
  (.append sb "## TOC\n")
  (doseq [e toc]
    (.append sb (str "- " (or (:level e) "") " " (or (:title e) "")
                  (when-let [p (:target-page e)] (str " (p." p ")")) "\n")))
  (.append sb "\n"))

(defn- page-node? [x] (and (map? x) (some? (:id x))))
(defn- toc-entry? [x] (and (map? x) (some? (:id x))))

(defn format-docs
  "Format arbitrary document data into compact markdown."
  [data]
  (cond
    (string? data) data
    (nil? data) ""
    (map? data)
    (cond
      (page-node? data) (format-docs [data])
      (toc-entry? data) (format-docs [data])
      :else
      (let [sb (StringBuilder.)]
        (when (seq (:pages data)) (append-pages-md sb (:pages data)))
        (when (seq (:toc data)) (append-toc-md sb (:toc data)))
        (str sb)))
    (coll? data)
    (let [groups (group-by (fn [x]
                             (cond (page-node? x) :pages
                               (toc-entry? x) :toc
                               :else :unknown)) data)
          sb (StringBuilder.)]
      (when (seq (:pages groups)) (append-pages-md sb (:pages groups)))
      (when (seq (:toc groups)) (append-toc-md sb (:toc groups)))
      (str sb))
    :else (str data)))

(defn make-search-documents-fn
  "Creates search-documents — unified search across pages and TOC."
  [db-info]
  (fn search-documents
    ([query] (search-documents query {}))
    ([query {:keys [in top-k document-id type] :or {top-k 10}}]
     (if db-info
       (let [do-pages #(let [results (db-search-page-nodes db-info query
                                       (cond-> {:top-k top-k}
                                         document-id (assoc :document-id document-id)
                                         type (assoc :type type)))]
                         (let [page-ids (distinct (keep :page-id results))]
                           (doseq [page-id page-ids] (record-page-access! db-info page-id 0.2))
                           (db/record-cooccurrences! db-info page-ids))
                         results)
             do-toc #(db-search-toc-entries db-info query {:top-k top-k})]
         (format-docs
           (case in
             :pages {:pages (do-pages)}
             :toc {:toc (do-toc)}
             {:pages (do-pages) :toc (do-toc)})))
       ""))))

(defn make-fetch-document-content-fn
  "Creates fetch-document-content — fetches content using lookup ref syntax."
  [db-info]
  (fn fetch-document-content [lookup-ref]
    (when db-info
      (when (and (vector? lookup-ref) (= 2 (count lookup-ref)))
        (let [[attr id] lookup-ref]
          (case attr
            :node/id
            (when-let [node (db-get-page-node db-info id)]
              (when-let [page-id (:page-id node)] (record-page-access! db-info page-id 1.0))
              (or (:content node) (:description node) ""))
            :doc/id
            (let [nodes (db/db-document-page-nodes-full db-info id)
                  page-ids (distinct (keep :page-id nodes))]
              (doseq [pid page-ids] (record-page-access! db-info pid 1.0))
              (db/record-cooccurrences! db-info page-ids)
              (when (seq nodes)
                (chunk-text (str/join "\n" (keep :content nodes)))))
            :toc/id
            (when-let [toc (db-get-toc-entry db-info id)]
              (or (:description toc) (:title toc) ""))
            (throw (ex-info (str "fetch-document-content unknown lookup attribute: " attr
                              ". Use :node/id, :doc/id, or :toc/id")
                     {:type :svar/invalid-lookup-ref :attr attr :id id}))))))))

;; =============================================================================
;; History tools
;; =============================================================================

(defn make-conversation-history-fn
  "Creates conversation-history for browsing prior query summaries."
  [db-info conversation-ref]
  (fn conversation-history
    ([] (conversation-history nil))
    ([n]
     (if db-info
       (let [history (db/db-query-history db-info conversation-ref)
             selected (if (some? n) (take-last (max 0 (long n)) history) history)]
         (mapv #(select-keys % [:query-pos :query-id :text :answer-preview :status :iterations :key-vars :created-at])
           selected))
       []))))

(defn- resolve-query-ref [db-info conversation-ref query-selector]
  (let [history (db/db-query-history db-info conversation-ref)]
    (cond
      (nil? query-selector) (some-> history last :query-ref)
      (integer? query-selector) (some->> history (filter #(= (:query-pos %) query-selector)) first :query-ref)
      (and (vector? query-selector) (= :id (first query-selector))) [:id (second query-selector)]
      (uuid? query-selector) [:id query-selector]
      :else nil)))

(defn make-conversation-code-fn
  "Creates conversation-code for browsing prior query code blocks."
  [db-info conversation-ref]
  (fn conversation-code [query-selector]
    (if db-info
      (if-let [query-ref (resolve-query-ref db-info conversation-ref query-selector)]
        (db/db-query-code db-info query-ref) [])
      [])))

(defn make-conversation-results-fn
  "Creates conversation-results for browsing prior query results and restorable vars."
  [db-info conversation-ref]
  (fn conversation-results [query-selector]
    (if db-info
      (if-let [query-ref (resolve-query-ref db-info conversation-ref query-selector)]
        (db/db-query-results db-info query-ref) [])
      [])))

;; =============================================================================
;; Restore tools
;; =============================================================================

(defn make-restore-var-fn
  "Creates restore-var for fetching the latest persisted data var from prior iterations."
  [db-info conversation-ref]
  (fn restore-var
    ([sym] (restore-var sym {}))
    ([sym opts]
     (if db-info
       (let [sym (if (symbol? sym) sym (symbol (str sym)))
             registry (db/db-latest-var-registry db-info conversation-ref
                        (select-keys (or opts {}) [:max-scan-queries]))]
         (if-let [{:keys [value]} (get registry sym)]
           value
           (throw (ex-info (str "No restorable var found for " sym)
                    {:type :rlm/restore-var-missing :symbol sym}))))
       (throw (ex-info "No DB available for restore-var" {:type :rlm/no-db}))))))

(defn make-restore-vars-fn
  "Creates restore-vars for batch fetching latest persisted data vars."
  [restore-var-fn]
  (fn restore-vars
    ([syms] (restore-vars syms {}))
    ([syms opts]
     (into {}
       (map (fn [sym]
              (try [sym (restore-var-fn sym opts)]
                (catch Exception e
                  [sym {:error {:type (:type (ex-data e))
                                :symbol sym
                                :message (ex-message e)}}]))))
       syms))))
