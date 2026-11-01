;; The ring type, its interfaces and its unchecked index arithmetic belong together.
(ns com.blockether.vis.internal.gateway.replay-ring
  "The bounded replay ring of one session: frames in ascending seq, oldest first.

   It behaves like a `PersistentQueue`: `conj` appends, `peek` and `pop` take the
   oldest frame. Two operations make it cheap to keep one materialized frame per key:

   - Each frame can have a replace key (`key-fn`). The ring counts the frames of each key.
   - [[remove-key]] scans back from the newest frame only until it has found every
     frame of that key. A replaced frame is almost always near the tail.

   The frames are a persistent vector and an offset of popped slots. A pop clears its
   slot, so an evicted frame never stays reachable. The ring copies the live frames to
   a new vector once the offset reaches [[compact-at]].

   A `subvec` of the whole history is not used: it retains the full backing vector."
  (:import [clojure.lang IFn IPersistentMap PersistentVector]))

(set! *warn-on-reflection* true)

(def ^:private ^:const compact-at
  "The popped slots that the backing vector can hold before the ring copies it."
  32)

(defn- count-key
  ^IPersistentMap [^IPersistentMap counts k ^long delta]
  (if (nil? k)
    counts
    (let [n (+ (long (.valAt counts k 0)) delta)]
      (if (zero? n) (.without counts k) (.assoc counts k (Long/valueOf n))))))

(declare ->Ring)

(defn- live-vector
  "The live frames of `v` from index `off` as a new vector."
  ^PersistentVector [^PersistentVector v ^long off]
  (let [end (.count v)]
    (loop [i off
           out (transient [])]

      (if (< i end) (recur (inc i) (conj! out (.nth v (int i)))) (persistent! out)))))

(deftype Ring [^PersistentVector v ^long off ^IPersistentMap counts ^IFn key-fn]
  clojure.lang.Counted
    (count [_] (- (.count v) off))
  clojure.lang.Sequential
  clojure.lang.Seqable
    (seq [_] (when (< off (.count v)) (seq (subvec v off))))
  clojure.lang.IPersistentCollection
    (cons [_ frame] (Ring. (.cons v frame) off (count-key counts (key-fn frame) 1) key-fn))
    (empty [_] (Ring. [] 0 {} key-fn))
    (equiv [this other]
      (and (or (instance? clojure.lang.Sequential other) (instance? java.util.List other))
           (clojure.lang.Util/equiv (seq this) (seq other))))
  clojure.lang.IPersistentStack
    (peek [_] (when (< off (.count v)) (.nth v (int off))))
    (pop [this]
      (if (>= off (.count v))
        this
        (let [oldest
              (.nth v (int off))

              counts
              (count-key counts (key-fn oldest) -1)

              next-off
              (inc off)]

          (if (>= next-off (long compact-at))
            (Ring. (live-vector v next-off) 0 counts key-fn)
            ;; Clear the slot: an evicted frame must not stay reachable.
            (Ring. (.assocN v (int off) nil) next-off counts key-fn)))))
  Iterable
    (iterator [_]
      ;; A sub-vector of a PersistentVector iterates by chunk; `listIterator` calls `nth`.
      (.iterator ^Iterable (subvec v off)))
  clojure.lang.IReduceInit
    (reduce [_ f init]
      (let [end (.count v)]
        (loop [i off
               acc init]

          (cond (reduced? acc) @acc
                (< i end) (recur (inc i) (f acc (.nth v (int i))))
                :else acc))))
  clojure.lang.IHashEq
    (hasheq [this] (hash-ordered-coll this))
  Object
    (hashCode [this] (hash-ordered-coll this))
    (equals [this other] (.equiv this other)))

(defmethod print-method Ring [ring ^java.io.Writer w] (print-method (or (seq ring) ()) w))

(defn ring
  "`frames` as a ring that keys its frames with `key-fn`. A ring with this `key-fn`
   comes back unchanged."
  [frames key-fn]
  (if (and (instance? Ring frames) (identical? key-fn (.-key-fn ^Ring frames)))
    frames
    (reduce conj (Ring. [] 0 {} key-fn) (or frames []))))

(defn- rebuild-from
  "`v` with the frames from index `start` on replaced by those that `keep?` accepts."
  ^PersistentVector [^PersistentVector v ^long start keep?]
  (let [end
        (.count v)

        tail
        (loop [i
               start

               kept
               (transient [])]

          (if (< i end)
            (let [frame (.nth v (int i))]
              (recur (inc i) (if (keep? frame) (conj! kept frame) kept)))
            (persistent! kept)))

        base
        (loop [w
               v

               n
               (- end start)]

          (if (pos? n) (recur (.pop w) (dec n)) w))]

    (reduce conj base tail)))

(defn remove-key
  "`[ring removed]`: `ring` without its frames of replace key `k`, and those frames in
   ring order. Scans back from the newest frame until it has seen all of them."
  [^Ring ring k]
  (let [n (long (.valAt ^IPersistentMap (.-counts ring) k 0))]
    (if (or (nil? k) (zero? n))
      [ring []]
      (let [^PersistentVector v (.-v ring)
            off (.-off ring)
            key-fn (.-key-fn ring)
            start (loop [i (dec (.count v))
                         found 0]

                    (let [found (if (= k (key-fn (.nth v (int i)))) (inc found) found)]
                      (if (or (= found n) (<= i off)) i (recur (dec i) found))))
            removed (into [] (filter #(= k (key-fn %))) (subvec v start))]

        [(Ring. (rebuild-from v start #(not= k (key-fn %)))
                off
                (.without ^IPersistentMap (.-counts ring) k)
                key-fn) removed]))))

(defn remove-frame
  "`ring` without the frame identical to `frame`."
  [^Ring ring frame]
  (let [^PersistentVector v
        (.-v ring)

        off
        (.-off ring)

        end
        (.count v)

        at
        (loop [i off]
          (cond (>= i end) nil
                (identical? frame (.nth v (int i))) i
                :else (recur (inc i))))]

    (if (nil? at)
      ring
      (Ring. (rebuild-from v at #(not (identical? frame %)))
             off
             (count-key (.-counts ring) ((.-key-fn ring) frame) -1)
             (.-key-fn ring)))))
