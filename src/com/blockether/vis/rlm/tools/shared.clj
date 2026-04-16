(ns com.blockether.vis.rlm.tools.shared
  "Shared helpers for the rlm.tools subsystem: SCI ns-map builder,
   EXTRA_BINDINGS, lazy-seq realization, date helpers, document-rendering
   helpers, and the unified document/citation tool factories.

   No cross-tool deps (no conversation / git / restore / sci). Safe to load
   first inside the tools/* bounded submodule."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.rlm.persistence.db :as db
    :refer [db-get-entity db-get-page-node db-get-toc-entry
            db-list-relationships db-search-entities db-search-page-nodes
            db-search-toc-entries record-page-access! str-truncate]]
   [com.blockether.svar.internal.util :as util]))

(defn ns->sci-map
  "Builds an SCI :namespaces entry map from a Clojure namespace's public vars.
   Pulls the entire ns-publics surface so models can use everything a real
   namespace offers without us enumerating fns manually.
   NOTE: prefer sci/copy-ns for standard namespaces (preserves doc/arglists).
   Use this only for namespaces where copy-ns fails (e.g. charred.api)."
  [ns-sym]
  (require ns-sym)
  (into {} (for [[sym v] (ns-publics (the-ns ns-sym))
                 :when (and (var? v) (not (:macro (meta v))))]
             [sym @v])))

(def EXTRA_BINDINGS
  "Extra bindings beyond what SCI provides by default.
   SCI already ships with all of clojure.core. We only add
   Clojure 1.11/1.12 additions that SCI doesn't have yet.
   Models use str/join, set/union etc. via namespace aliases."
  {'abs abs, 'parse-long parse-long, 'parse-double parse-double,
   'parse-boolean parse-boolean, 'parse-uuid parse-uuid,
   'infinite? infinite?, 'NaN? NaN?,
   'url-encode (fn ^String url-encode [^String s] (java.net.URLEncoder/encode s "UTF-8")),
   'url-decode (fn ^String url-decode [^String s] (java.net.URLDecoder/decode s "UTF-8"))})

(def ^:private REALIZE_LAZY_LIMIT
  "Upper bound on elements pulled from a lazy sequence by `realize-value`.
   Prevents OOM when models feed infinite seqs like `(range)` or
   `(iterate inc 0)` into the result path. Kept low (100) by default -
   the model can explicitly `(vec (take n my-seq))` for larger slices."
  100)

(defn realize-value
  "Recursively realizes lazy sequences in a value to prevent opaque LazySeq@hash.
   Converts lazy seqs to vectors (bounded), walks into maps/vectors/sets.
   Lazy seqs longer than REALIZE_LAZY_LIMIT are truncated with a trailing
   marker so downstream code can still serialize them safely."
  [v]
  (cond
    (instance? clojure.lang.LazySeq v)
    (let [head (take (inc REALIZE_LAZY_LIMIT) v)
          realized (mapv realize-value (take REALIZE_LAZY_LIMIT head))]
      (if (> (count head) REALIZE_LAZY_LIMIT)
        (conj realized (str "...<truncated lazy seq at " REALIZE_LAZY_LIMIT " elements>"))
        realized))
    (map? v) (persistent! (reduce-kv (fn [m k val] (assoc! m k (realize-value val))) (transient {}) v))
    (vector? v) (mapv realize-value v)
    (set? v) (into #{} (map realize-value) v)
    :else v))

;; =============================================================================
;; Date Helper Functions
;; =============================================================================

(defn parse-date
  "Parses an ISO-8601 date string (YYYY-MM-DD)."
  [date-str]
  (try
    (when date-str
      (str (java.time.LocalDate/parse date-str)))
    (catch Exception _
      nil)))

(defn date-before?
  [date1 date2]
  (try
    (when (and date1 date2)
      (.isBefore (java.time.LocalDate/parse date1)
        (java.time.LocalDate/parse date2)))
    (catch Exception _
      false)))

(defn date-after?
  [date1 date2]
  (try
    (when (and date1 date2)
      (.isAfter (java.time.LocalDate/parse date1)
        (java.time.LocalDate/parse date2)))
    (catch Exception _
      false)))

(defn days-between
  [date1 date2]
  (try
    (when (and date1 date2)
      (.between java.time.temporal.ChronoUnit/DAYS
        (java.time.LocalDate/parse date1)
        (java.time.LocalDate/parse date2)))
    (catch Exception _
      nil)))

(defn date-plus-days
  [date-str days]
  (try
    (when date-str
      (str (.plusDays (java.time.LocalDate/parse date-str) days)))
    (catch Exception _
      nil)))

(defn date-minus-days
  [date-str days]
  (try
    (when date-str
      (str (.minusDays (java.time.LocalDate/parse date-str) days)))
    (catch Exception _
      nil)))

(defn date-format
  [date-str pattern]
  (try
    (when (and date-str pattern)
      (let [formatter (java.time.format.DateTimeFormatter/ofPattern pattern)
            date (java.time.LocalDate/parse date-str)]
        (.format date formatter)))
    (catch Exception _
      nil)))

(defn today-str
  []
  (str (java.time.LocalDate/now)))

;; =============================================================================
;; Document Storage — unified search / fetch / cite
;; =============================================================================

(def ^:private FETCH_CONTENT_PAGE_SIZE
  "Characters per chunk when fetch-document-content returns a document as a vector of pages."
  4000)

(defn- chunk-text
  [text]
  (let [page-size FETCH_CONTENT_PAGE_SIZE]
    (loop [remaining (str/split text #"\n\n+")
           current [] current-size 0 result []]
      (if (empty? remaining)
        (if (seq current)
          (conj result (str/join "\n\n" current))
          result)
        (let [para (first remaining)
              para-size (count para)]
          (if (and (> current-size 0) (> (+ current-size para-size) page-size))
            (recur remaining [] 0 (conj result (str/join "\n\n" current)))
            (recur (rest remaining) (conj current para)
              (+ current-size para-size) result)))))))

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
                  (when-let [p (:target-page e)] (str " (p." p ")"))
                  "\n")))
  (.append sb "\n"))

(defn- append-entities-md [^StringBuilder sb entities]
  (.append sb "## Entities\n")
  (doseq [e entities]
    (.append sb (str "- **" (or (:name e) "") "** ("
                  (some-> (:type e) name) ") "
                  (str-truncate (or (:description e) "") 80) "\n")))
  (.append sb "\n"))

(defn- append-relationships-md [^StringBuilder sb rels]
  (.append sb "## Relationships\n")
  (doseq [r rels]
    (let [rtype (some-> (:type r) name)
          src (:source-id r)
          tgt (:target-id r)]
      (.append sb (str "- **" rtype "** " src " → " tgt "\n"))))
  (.append sb "\n"))

(defn- page-node? [x] (and (map? x) (some? (:id x))))
(defn- toc-entry? [x] (and (map? x) (some? (:id x))))
(defn- entity? [x] (and (map? x) (some? (:id x))))
(defn- relationship? [x] (and (map? x) (some? (:type x))))

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
      (entity? data) (format-docs [data])
      (relationship? data) (format-docs [data])
      :else
      (let [sb (StringBuilder.)]
        (when (seq (:pages data)) (append-pages-md sb (:pages data)))
        (when (seq (:toc data)) (append-toc-md sb (:toc data)))
        (when (seq (:entities data)) (append-entities-md sb (:entities data)))
        (when (seq (:relationships data)) (append-relationships-md sb (:relationships data)))
        (str sb)))
    (coll? data)
    (let [groups (group-by (fn [x]
                             (cond (page-node? x) :pages
                               (toc-entry? x) :toc
                               (entity? x) :entities
                               (relationship? x) :relationships
                               :else :unknown))
                   data)
          sb (StringBuilder.)]
      (when (seq (:pages groups)) (append-pages-md sb (:pages groups)))
      (when (seq (:toc groups)) (append-toc-md sb (:toc groups)))
      (when (seq (:entities groups)) (append-entities-md sb (:entities groups)))
      (when (seq (:relationships groups)) (append-relationships-md sb (:relationships groups)))
      (str sb))
    :else (str data)))

(defn make-search-documents-fn
  "Creates search-documents — unified search across pages, TOC, and entities.
   Returns markdown directly."
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
                           (doseq [page-id page-ids]
                             (record-page-access! db-info page-id 0.2))
                           (db/record-cooccurrences! db-info page-ids))
                         results)
             do-toc #(db-search-toc-entries db-info query {:top-k top-k})
             do-ents #(db-search-entities db-info query
                        (cond-> {:top-k top-k}
                          document-id (assoc :document-id document-id)
                          type (assoc :type type)))]
         (format-docs
           (case in
             :pages {:pages (do-pages)}
             :toc {:toc (do-toc)}
             :entities {:entities (do-ents)}
             {:pages (do-pages) :toc (do-toc) :entities (do-ents)})))
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
              (when-let [page-id (:page-id node)]
                (record-page-access! db-info page-id 1.0))
              (or (:content node) (:description node) ""))

            :doc/id
            (let [nodes (db/db-document-page-nodes-full db-info id)
                  page-ids (distinct (keep :page-id nodes))]
              (doseq [pid page-ids]
                (record-page-access! db-info pid 1.0))
              (db/record-cooccurrences! db-info page-ids)
              (when (seq nodes)
                (let [full-text (->> nodes
                                  (keep :content)
                                  (str/join "\n"))]
                  (chunk-text full-text))))

            :toc/id
            (when-let [toc (db-get-toc-entry db-info id)]
              (or (:description toc) (:title toc) ""))

            :id
            (when-let [entity (db-get-entity db-info id)]
              {:entity entity
               :relationships (db-list-relationships db-info id {})})

            (throw (ex-info (str "fetch-document-content unknown lookup attribute: " attr
                              ". Use :node/id, :doc/id, :toc/id, or :id")
                     {:type :svar/invalid-lookup-ref :attr attr :id id}))))))))

(defn make-cite-fn
  "Creates CITE function for the LLM to cite claims with sources."
  [claims-atom]
  (fn CITE
    ([claim-text document-id page section quote]
     (CITE claim-text document-id page section quote 1.0))
    ([claim-text document-id page section quote confidence]
     (let [claim {:id (util/uuid)
                  :text claim-text
                  :document-id document-id
                  :page (long (if (string? page) (Long/parseLong page) page))
                  :section section
                  :quote quote
                  :confidence (float (if (string? confidence) (Double/parseDouble confidence) confidence))
                  :created-at (java.util.Date.)}]
       (swap! claims-atom conj claim)
       {:cited true :claim-id (:id claim) :claim-text claim-text}))))

(defn make-cite-unverified-fn
  "Creates CITE-UNVERIFIED function for claims without source verification."
  [claims-atom]
  (fn CITE-UNVERIFIED
    [claim-text]
    (let [claim {:id (util/uuid)
                 :text claim-text
                 :document-id nil
                 :page nil
                 :section nil
                 :quote nil
                 :confidence 0.5
                 :verified? false
                 :created-at (java.util.Date.)}]
      (swap! claims-atom conj claim)
      {:cited true :verified? false :claim-id (:id claim) :claim-text claim-text})))

(defn make-list-claims-fn
  "Creates list-claims function to retrieve accumulated claims."
  [claims-atom]
  (fn list-claims
    []
    (vec @claims-atom)))
