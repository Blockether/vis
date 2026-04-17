(ns com.blockether.vis.loop.runtime.shared
  "Shared helpers and core tool factories for the SCI sandbox.

   Contains:
   - General helpers: truncate, realize-value, ns->sci-map, EXTRA_BINDINGS
   - Date tool bindings: parse-date, date-before?, date-after?, etc.
   - Document tools: search-documents, fetch-document-content, format-docs
   - History tools: conversation-history, conversation-code, conversation-results
   - Restore tools: restore-var, restore-vars
   - Git tools: in runtime/tools/git.clj (separate to avoid cyclic dep)

   All tool factories follow the pattern `(make-*-fn db-info ...)` → sandbox fn.
   Safe to load first — no cross-deps on runtime/core."
  )

;; =============================================================================
;; General helpers
;; =============================================================================

(defn truncate
  "Return `s` capped at `n` chars, preserving nil."
  [s n]
  (when s
    (if (> (count s) n)
      (subs s 0 n)
      s)))

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

(def ^:private REALIZE_LAZY_LIMIT 100)

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
;; Date tool bindings
;; =============================================================================

(defn parse-date
  "Parses an ISO-8601 date string (YYYY-MM-DD)."
  [date-str]
  (try
    (when date-str
      (str (java.time.LocalDate/parse date-str)))
    (catch Exception _ nil)))

(defn date-before? [date1 date2]
  (try
    (when (and date1 date2)
      (.isBefore (java.time.LocalDate/parse date1) (java.time.LocalDate/parse date2)))
    (catch Exception _ false)))

(defn date-after? [date1 date2]
  (try
    (when (and date1 date2)
      (.isAfter (java.time.LocalDate/parse date1) (java.time.LocalDate/parse date2)))
    (catch Exception _ false)))

(defn days-between [date1 date2]
  (try
    (when (and date1 date2)
      (.between java.time.temporal.ChronoUnit/DAYS
        (java.time.LocalDate/parse date1) (java.time.LocalDate/parse date2)))
    (catch Exception _ nil)))

(defn date-plus-days [date-str days]
  (try
    (when date-str (str (.plusDays (java.time.LocalDate/parse date-str) days)))
    (catch Exception _ nil)))

(defn date-minus-days [date-str days]
  (try
    (when date-str (str (.minusDays (java.time.LocalDate/parse date-str) days)))
    (catch Exception _ nil)))

(defn date-format [date-str pattern]
  (try
    (when (and date-str pattern)
      (let [formatter (java.time.format.DateTimeFormatter/ofPattern pattern)
            date (java.time.LocalDate/parse date-str)]
        (.format date formatter)))
    (catch Exception _ nil)))

(defn today-str [] (str (java.time.LocalDate/now)))
