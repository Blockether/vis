(ns com.blockether.vis.internal.env.handle
  "Handle: protocol-driven, deref-able reference to bounded payload data.

   `PHandle` is the contract every handle implements:
     - (op h)                → tool op tag (`:v/cat`, `:v/rg`, `:v/ls`, ...)
     - (summary h)           → map of one-line metadata for prompt-side
                               rendering; includes `:op` and `:views`
     - (view h op [a [b]])   → materialize a bounded view of the payload;
                               op is a handle-specific view keyword

   Each handle type is its own defrecord (`CatHandle` here; `RgHandle`
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
  [store-key handle-op]
  (if-let [entry (get-in @store [:entries store-key])]
    (:payload entry)
    (throw (ex-info "Handle payload evicted from store"
             {:type :vis.handle/evicted
              :op handle-op
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
  "Polymorphic handle interface. Each handle implements its own view
   operations; the prompt renderer reads `summary` to draw the one-line
   #vis/handle form."
  (op [h]
    "Tool op tag, e.g. :v/cat / :v/rg / :v/ls.")
  (summary [h]
    "Map of one-line metadata for prompt-side rendering. Always
     contains :op and :views; other keys are op-specific.")
  (view
    [h op]
    [h op a]
    [h op a b]
    "Materialize a bounded view of the handle's payload. Op is a
     handle-specific view keyword. Throws :vis.handle/unsupported-view on
     unknown ops."))

(defn handle?
  "True only for values backed by a Handle defrecord (concrete op
   implementing PHandle WITHOUT relying on the Object fallback below).
   Cheap class check — avoids `satisfies?` matching the Object impl
   which extends to literally everything."
  [v]
  (and (instance? clojure.lang.IRecord v)
    (satisfies? PHandle v)
    (not= :not-a-handle (op v))))

;; -----------------------------------------------------------------------------
;; Fallback: PHandle on every value
;;
;; Calling (view 42 :peek) used to throw IllegalArgumentException because
;; nothing implemented PHandle for Long. Per the engine-primitive contract
;; (`view` / `summary` / `op` are always available in the sandbox),
;; non-handle values get a structured `:not-a-handle` answer instead of
;; a stack trace. Cheap correction signal — the model sees the shape it
;; got and the hint that points back at v/cat / v/rg / v/ls / handle?.
;; -----------------------------------------------------------------------------

(def ^:private NOT_A_HANDLE_HINT
  "View / summary / op operate on Handle records. Use (handle? v) to test, or call v/cat / v/rg / v/ls to get one.")

(extend-protocol PHandle
  nil
  (op [_] :not-a-handle)
  (summary [_] {:op :not-a-handle :value nil :hint NOT_A_HANDLE_HINT})
  (view
    ([_ view-op] {:op :not-a-handle :view-op view-op :hint NOT_A_HANDLE_HINT})
    ([_ view-op a] {:op :not-a-handle :view-op view-op :args [a] :hint NOT_A_HANDLE_HINT})
    ([_ view-op a b] {:op :not-a-handle :view-op view-op :args [a b] :hint NOT_A_HANDLE_HINT}))

  Object
  (op [_] :not-a-handle)
  (summary [v] {:op :not-a-handle :value v :hint NOT_A_HANDLE_HINT})
  (view
    ([v view-op] {:op :not-a-handle :value v :view-op view-op :hint NOT_A_HANDLE_HINT})
    ([v view-op a] {:op :not-a-handle :value v :view-op view-op :args [a] :hint NOT_A_HANDLE_HINT})
    ([v view-op a b] {:op :not-a-handle :value v :view-op view-op :args [a b] :hint NOT_A_HANDLE_HINT})))

;; =============================================================================
;; CatHandle — file content reads
;; =============================================================================

(def ^:private cat-views
  "View ops supported by CatHandle. Surface in summary so the model
   discovers them without external docs."
  #{:peek :lines :at})

(def ^:const ^:private CAT_PEEK_LINES 50)

(defn- unsupported-view!
  [handle-op view-op arity]
  (throw (ex-info (str "Unsupported view op for " handle-op " handle")
           {:type :vis.handle/unsupported-view
            :op handle-op
            :view-op view-op
            :arity arity})))

(defrecord CatHandle [store-key info]
  clojure.lang.IDeref
  (deref [_] (lookup-payload store-key :v/cat))
  PHandle
  (op [_] :v/cat)
  (summary [_] (assoc info :op :v/cat :views (vec cat-views)))
  (view [h view-op]
    (case view-op
      :peek (let [lines @h]
              (subvec lines 0 (min CAT_PEEK_LINES (count lines))))
      (unsupported-view! :v/cat view-op 0)))
  (view [h view-op a]
    (case view-op
      :at (nth @h (dec a) nil)
      (unsupported-view! :v/cat view-op 1)))
  (view [h view-op a b]
    (case view-op
      :lines (let [lines @h
                   n     (count lines)
                   start (max 0 (min n a))
                   end   (max start (min n b))]
               (subvec lines start end))
      (unsupported-view! :v/cat view-op 2))))

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
  (deref [_] (lookup-payload store-key :v/rg))
  PHandle
  (op [_] :v/rg)
  (summary [_] (assoc info :op :v/rg :views (vec rg-views)))
  (view [h view-op]
    (case view-op
      :peek (let [hits @h]
              (subvec hits 0 (min RG_PEEK_HITS (count hits))))
      :all  @h
      (unsupported-view! :v/rg view-op 0)))
  (view [h view-op a]
    (case view-op
      :hit (nth @h (dec a) nil)
      (unsupported-view! :v/rg view-op 1)))
  (view [_ view-op _ _] (unsupported-view! :v/rg view-op 2)))

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
  (deref [_] (lookup-payload store-key :v/ls))
  PHandle
  (op [_] :v/ls)
  (summary [_] (assoc info :op :v/ls :views (vec ls-views)))
  (view [h view-op]
    (case view-op
      :peek     (let [tree @h
                      kids (vec (or (:children tree) []))]
                  (mapv :name (subvec kids 0 (min LS_PEEK_ENTRIES (count kids)))))
      :children (let [tree @h] (vec (or (:children tree) [])))
      :tree     @h
      (unsupported-view! :v/ls view-op 0)))
  (view [_ view-op _] (unsupported-view! :v/ls view-op 1))
  (view [_ view-op _ _] (unsupported-view! :v/ls view-op 2)))

(defn make-ls
  "Construct an LsHandle from a `list-files` tree node. Stashes the
   tree (root + children) into the store; the handle's `:info` carries
   the relative path, absolute path, top-level entry count, and tree? flag."
  [tree]
  (let [{:keys [path absolute-path type children]} tree
        store-key (intern-payload tree)]
    (->LsHandle store-key
      {:path          path
       :absolute-path absolute-path
       :type          type
       :entry-count   (count (or children []))
       :tree?         (boolean (seq children))})))

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
