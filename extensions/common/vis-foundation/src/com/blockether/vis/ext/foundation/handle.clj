(ns com.blockether.vis.ext.foundation.handle
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

(defrecord CatHandle [store-key path line-count sha first-line last-line]
  clojure.lang.IDeref
  (deref [_] (lookup-payload store-key :v.cat))
  PHandle
  (kind [_] :v.cat)
  (summary [_]
    {:kind        :v.cat
     :path        path
     :line-count  line-count
     :sha         sha
     :first-line  first-line
     :last-line   last-line
     :views       (vec cat-views)})
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
   `:lines` into the store; returns a CatHandle whose summary captures
   path / line-count / sha / first-line / last-line."
  [{:keys [path lines]}]
  (let [lines (vec lines)
        line-count (count lines)
        first-line (first lines)
        last-line  (last lines)
        sha-hex    (when (pos? line-count)
                     (sha8 (str/join "\n" lines)))
        store-key  (intern-payload lines)]
    (->CatHandle store-key path line-count sha-hex first-line last-line)))

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
