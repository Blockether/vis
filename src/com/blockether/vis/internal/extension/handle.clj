(ns com.blockether.vis.internal.extension.handle
  "Handle: protocol-driven, deref-able reference to bounded payload data.

   `PHandle` is the contract every handle kind implements:
     - (kind h)              → kind tag (`:v.cat`, `:v.rg`, `:v.ls`, ...)
     - (summary h)           → map of one-line metadata for prompt-side
                               rendering; includes `:kind` and `:views`
     - (view h op [a [b]])   → materialize a bounded view of the payload;
                               op is a kind-specific keyword

   Each handle kind is its own defrecord (`CatHandle` here; `RgHandle`
   and `LsHandle` follow when those tools convert). All handle records
   implement `IDeref` so `@h` returns the full payload from the
   process-wide store, and all of them ship a single-line `print-method`.

   The store is a defonce LRU map bounded by `MAX_STORE_BYTES`. Per-env
   wiring follows once slice/view tools land and need conversation
   isolation."
  (:require
   [clojure.string :as str])
  (:import
   (java.security MessageDigest)))

(def ^:const MAX_STORE_BYTES
  "Upper bound on total live payload bytes. LRU on insert."
  (* 16 1024 1024))

;; =============================================================================
;; Store
;; =============================================================================

(defonce ^:private store
  (atom {:entries {} :insert-order [] :total-bytes 0}))

(defn- estimate-bytes
  "Cheap UTF-8 byte estimate. Strings count their actual bytes; vectors
   recurse on members; everything else falls back to `pr-str` length."
  ^long [v]
  (cond
    (nil? v) 0
    (string? v) (long (alength (.getBytes ^String v "UTF-8")))
    (vector? v) (long (reduce (fn [^long acc x] (+ acc (estimate-bytes x))) 0 v))
    :else (long (alength (.getBytes ^String (pr-str v) "UTF-8")))))

(defn- evict-until-fits
  "Drop oldest entries until adding `incoming` bytes fits under the cap.
   Empties the store if a single payload exceeds the cap (the new entry
   still goes in)."
  [s ^long incoming]
  (loop [s s]
    (if (and (pos? (count (:insert-order s)))
          (> (+ (long (:total-bytes s)) incoming) (long MAX_STORE_BYTES)))
      (let [[oldest & rest-order] (:insert-order s)
            old-bytes (long (or (get-in s [:entries oldest :bytes]) 0))]
        (recur (-> s
                 (update :entries dissoc oldest)
                 (assoc :insert-order (vec rest-order))
                 (update :total-bytes - old-bytes))))
      s)))

(defn- gen-store-key []
  (str "h_" (System/nanoTime)))

(defn- lookup-payload
  "Resolve a store-key to its payload. Throws :vis.handle/evicted when
   the entry has been pushed out under LRU pressure."
  [store-key kind]
  (if-let [entry (get-in @store [:entries store-key])]
    (:payload entry)
    (throw (ex-info "Handle payload evicted from store"
             {:type :vis.handle/evicted
              :kind kind
              :store-key store-key}))))

(defn- intern-payload
  "Stash `payload` in the store; return the generated store-key."
  [payload]
  (let [k (gen-store-key)
        bytes (estimate-bytes payload)]
    (swap! store
      (fn [s]
        (let [s* (evict-until-fits s bytes)]
          (-> s*
            (assoc-in [:entries k] {:payload payload :bytes bytes})
            (update :insert-order (fnil conj []) k)
            (update :total-bytes + bytes)))))
    k))

;; =============================================================================
;; Protocol
;; =============================================================================

(defprotocol PHandle
  "Polymorphic handle interface. Each kind implements its own view
   operations; the prompt renderer reads `summary` to draw the one-line
   #vis/handle form."
  (kind [h]
    "Kind tag, e.g. :v.cat / :v.rg / :v.ls.")
  (summary [h]
    "Map of one-line metadata for prompt-side rendering. Always
     contains :kind and :views; other keys are kind-specific.")
  (view
    [h op]
    [h op a]
    [h op a b]
    "Materialize a bounded view of the handle's payload. Op is a
     kind-specific keyword. Throws :vis.handle/unsupported-view on
     unknown ops."))

(defn handle?
  "True for any value implementing the PHandle protocol."
  [v]
  (satisfies? PHandle v))

;; =============================================================================
;; CatHandle — file content reads
;; =============================================================================

(def ^:private cat-views
  "View ops supported by CatHandle. Surface in summary so the model
   discovers them without external docs."
  #{:peek :lines :at})

(def ^:const ^:private CAT_PEEK_LINES 50)

(defn- unsupported-view!
  [kind op arity]
  (throw (ex-info (str "Unsupported view op for " kind " handle")
           {:type :vis.handle/unsupported-view
            :kind kind
            :op op
            :arity arity})))

(defrecord CatHandle [store-key info]
  clojure.lang.IDeref
  (deref [_] (lookup-payload store-key :v.cat))
  PHandle
  (kind [_] :v.cat)
  (summary [_] (assoc info :kind :v.cat :views (vec cat-views)))
  (view [h op]
    (case op
      :peek (let [lines @h]
              (subvec lines 0 (min CAT_PEEK_LINES (count lines))))
      (unsupported-view! :v.cat op 0)))
  (view [h op a]
    (case op
      :at (nth @h (dec a) nil)
      (unsupported-view! :v.cat op 1)))
  (view [h op a b]
    (case op
      :lines (let [lines @h
                   n     (count lines)
                   start (max 0 (min n a))
                   end   (max start (min n b))]
               (subvec lines start end))
      (unsupported-view! :v.cat op 2))))

(defn- sha8
  "Short SHA-256 prefix over the joined line content. Cheap fingerprint
   for prompt rendering — collision-safe in practice for handle ids."
  ^String [^String s]
  (let [md (MessageDigest/getInstance "SHA-256")
        digest (.digest md (.getBytes s "UTF-8"))
        hex (apply str (map #(format "%02x" (bit-and % 0xff)) digest))]
    (subs hex 0 8)))

(defn make-cat
  "Construct a CatHandle from a `read-file`-style result map. Stashes
   `:lines` into the store; the handle's `:info` carries pagination
   metadata (offset / next-offset / eof? / truncated-by) plus the
   one-line summary keys (line-count / sha / first-line / last-line)."
  [{:keys [path lines offset next-offset eof? truncated-by]}]
  (let [lines (vec lines)
        line-count (count lines)
        first-line (first lines)
        last-line  (last lines)
        sha-hex    (when (pos? line-count)
                     (sha8 (str/join "\n" lines)))
        store-key  (intern-payload lines)]
    (->CatHandle store-key
      {:path         path
       :offset       offset
       :line-count   line-count
       :next-offset  next-offset
       :eof?         eof?
       :truncated-by truncated-by
       :sha          sha-hex
       :first-line   first-line
       :last-line    last-line})))

;; =============================================================================
;; RgHandle — literal-search hits
;; =============================================================================

(def ^:private rg-views
  "View ops supported by RgHandle. :peek surfaces the first window of
   hits; :hit fetches a single 1-based hit; :all returns the whole vec."
  #{:peek :hit :all})

(def ^:const ^:private RG_PEEK_HITS 10)

(defrecord RgHandle [store-key info]
  clojure.lang.IDeref
  (deref [_] (lookup-payload store-key :v.rg))
  PHandle
  (kind [_] :v.rg)
  (summary [_] (assoc info :kind :v.rg :views (vec rg-views)))
  (view [h op]
    (case op
      :peek (let [hits @h]
              (subvec hits 0 (min RG_PEEK_HITS (count hits))))
      :all  @h
      (unsupported-view! :v.rg op 0)))
  (view [h op a]
    (case op
      :hit (nth @h (dec a) nil)
      (unsupported-view! :v.rg op 1)))
  (view [_ op _ _] (unsupported-view! :v.rg op 2)))

(defn make-rg
  "Construct an RgHandle from a `grep-files`-style result map. Stashes
   `:hits` into the store; the handle's `:info` carries hit-count,
   truncated-by, the first hit's `path:line` summary, plus the original
   spec for traceability."
  [{:keys [hits truncated-by]} {:keys [spec paths]}]
  (let [hits      (vec hits)
        hit-count (count hits)
        first-hit (when (pos? hit-count)
                    (let [{:keys [path line]} (nth hits 0)]
                      (str path ":" line)))
        store-key (intern-payload hits)]
    (->RgHandle store-key
      {:hit-count    hit-count
       :truncated-by truncated-by
       :first-hit    first-hit
       :paths        paths
       :spec         spec})))

;; =============================================================================
;; LsHandle — directory tree
;; =============================================================================

(def ^:private ls-views
  "View ops supported by LsHandle. :peek lists top-level entry names;
   :children returns the immediate children entries; :tree returns the
   full tree."
  #{:peek :children :tree})

(def ^:const ^:private LS_PEEK_ENTRIES 20)

(defrecord LsHandle [store-key info]
  clojure.lang.IDeref
  (deref [_] (lookup-payload store-key :v.ls))
  PHandle
  (kind [_] :v.ls)
  (summary [_] (assoc info :kind :v.ls :views (vec ls-views)))
  (view [h op]
    (case op
      :peek     (let [tree @h
                      kids (vec (or (:children tree) []))]
                  (mapv :name (subvec kids 0 (min LS_PEEK_ENTRIES (count kids)))))
      :children (let [tree @h] (vec (or (:children tree) [])))
      :tree     @h
      (unsupported-view! :v.ls op 0)))
  (view [_ op _] (unsupported-view! :v.ls op 1))
  (view [_ op _ _] (unsupported-view! :v.ls op 2)))

(defn make-ls
  "Construct an LsHandle from a `list-files` tree node. Stashes the
   tree (root + children) into the store; the handle's `:info` carries
   the path, top-level entry count, and tree? flag."
  [tree]
  (let [{:keys [path type children]} tree
        store-key (intern-payload tree)]
    (->LsHandle store-key
      {:path        path
       :type        type
       :entry-count (count (or children []))
       :tree?       (boolean (seq children))})))

;; =============================================================================
;; print-method (single-line summary)
;; =============================================================================

(defn- summary-str
  "Render a summary map as a single line. Caps depth/length defensively
   in case a handle's meta grows large."
  [m]
  (binding [*print-length* 16
            *print-level* 4]
    (str/replace (pr-str m) #"\s+" " ")))

(defn- print-handle
  [h ^java.io.Writer w]
  (.write w "#vis/handle ")
  (.write w (summary-str (summary h))))

(defmethod print-method CatHandle [h ^java.io.Writer w] (print-handle h w))
(defmethod print-dup    CatHandle [h ^java.io.Writer w] (print-handle h w))
(defmethod print-method RgHandle  [h ^java.io.Writer w] (print-handle h w))
(defmethod print-dup    RgHandle  [h ^java.io.Writer w] (print-handle h w))
(defmethod print-method LsHandle  [h ^java.io.Writer w] (print-handle h w))
(defmethod print-dup    LsHandle  [h ^java.io.Writer w] (print-handle h w))

;; -----------------------------------------------------------------------------
;; pprint dispatch
;;
;; Handle records implement BOTH IPersistentMap (defrecord default) AND
;; IDeref. clojure.pprint/simple-dispatch has methods on both interfaces;
;; without an explicit preference, pprint throws "Multiple methods ...
;; match dispatch value". Prefer the IPersistentMap branch (which falls
;; through to print-method, picking up our custom one-line render).
;; -----------------------------------------------------------------------------

(require 'clojure.pprint)
(prefer-method
  @(resolve 'clojure.pprint/simple-dispatch)
  clojure.lang.IPersistentMap
  clojure.lang.IDeref)

;; =============================================================================
;; Diagnostics
;; =============================================================================

(defn store-stats
  "Inspect the store. For tests / diagnostics."
  []
  (let [s @store]
    {:entry-count (count (:entries s))
     :total-bytes (:total-bytes s)
     :max-bytes   MAX_STORE_BYTES}))

(defn clear-store!
  "Reset the store to empty. Intended for test fixtures only."
  []
  (reset! store {:entries {} :insert-order [] :total-bytes 0}))
