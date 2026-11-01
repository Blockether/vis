(ns com.blockether.vis.internal.gateway.event-store
  "Process-local replay frames retain locally assigned cursors, never shared-bus sequence numbers.
   A small frame stays in its descriptor as frozen bytes; a larger one goes to a file."
  (:require [com.blockether.vis.internal.gateway.turn-archive :as archive]
            [taoensso.nippy :as nippy]
            [taoensso.telemere :as tel])
  (:import [java.nio.file Files Path]
           [java.util.concurrent.atomic AtomicLong]))

(defonce ^:private locks (vec (repeatedly 64 #(Object.))))

(defn lock-for [sid] (nth locks (mod (hash (str sid)) (count locks))))

(def ^:dynamic *max-bytes* (* 16 1024 1024))

(def ^:dynamic *inline-max-bytes*
  "The largest frozen frame that stays in its descriptor. A file costs about 0.1 to 0.2 ms
   to create and delete, and most frames freeze to less than 1 KB."
  4096)

(def ^:dynamic *inline-budget-bytes*
  "The frozen bytes that all descriptors of the process can hold. Past it, frames go to
   files again, so many full rings cannot grow the heap without bound."
  (* 64 1024 1024))

(defonce ^:private inline-bytes (AtomicLong. 0))

(defn inline-total
  "The frozen bytes that live descriptors hold now."
  ^long []
  (.get ^AtomicLong inline-bytes))

(defn- claim-inline!
  "Reserve `n` bytes of the inline budget. False when the budget has no room."
  [^long n]
  (loop []

    (let [held (.get ^AtomicLong inline-bytes)]
      (cond (< (long *inline-budget-bytes*) (+ held n)) false
            (.compareAndSet ^AtomicLong inline-bytes held (+ held n)) true
            :else (recur)))))

(defn write-event!
  "Persist before publishing the descriptor. Failed writes declare a replay gap."
  [event]
  (let [block-id
        (or (get event "block_id") (get-in event ["block" "id"]))

        descriptor
        (cond-> (select-keys event ["seq" "type" "turn_id" "iteration" "form_index"])
          block-id
          (assoc "block_id" block-id))]

    (try (let [^bytes frozen
               (nippy/freeze event)

               size
               (alength frozen)]

           (if (and (<= size (long *inline-max-bytes*)) (claim-inline! size))
             (assoc descriptor
               ::frozen frozen
               ::bytes size)
             (let [file (archive/write! event)]
               (assoc descriptor
                 ::file file
                 ::bytes (Files/size (Path/of ^String file (make-array String 0)))))))
         (catch Exception e
           (tel/log! {:level :error :id ::write-failed :data {:error-class (.getName (class e))}})
           (assoc descriptor ::missing? true)))))

(defn read-event
  [descriptor]
  (if-let [frozen (::frozen descriptor)]
    (nippy/thaw frozen)
    (archive/read-turn (::file descriptor))))

(defn delete-event!
  "Release what `descriptor` holds: its file, or its share of the inline budget."
  [descriptor]
  (if-let [^bytes frozen (::frozen descriptor)]
    (.addAndGet ^AtomicLong inline-bytes (- (alength frozen)))
    (when-let [file (::file descriptor)]
      (archive/delete! file))))

(defn- dropped-in-order
  "The descriptors that `after` dropped from `before`, plus `extra` when `after`
   does not keep it. Answers only when `after` is `before` in the same order with
   some descriptors dropped and at most `extra` appended, which is the shape of
   every append. nil for any other shape.

   One walk of both rings by identity, so an append no longer builds a set of
   the whole ring to find the one frame it evicted."
  [before after extra]
  (let [^Iterable before
        (or before [])

        ^Iterable after
        (or after [])

        bs
        (.iterator before)

        as
        (.iterator after)

        next-of
        (fn [^java.util.Iterator it]
          (when (.hasNext it) (.next it)))]

    (loop [a
           (next-of as)

           dropped
           (transient [])]

      (if (.hasNext bs)
        (let [b (.next bs)]
          (if (identical? b a) (recur (next-of as) dropped) (recur a (conj! dropped b))))
        (cond (and (nil? a) extra) (persistent! (conj! dropped extra))
              (nil? a) (persistent! dropped)
              (and (identical? a extra) (not (.hasNext as))) (persistent! dropped)
              :else nil)))))

(defn- held
  "What `descriptor` holds: its file name, or its frozen array, which compares by identity."
  [descriptor]
  (or (::file descriptor) (::frozen descriptor)))

(defn discard-replaced!
  "Release retired frames after the registry transition, never within a CAS retry."
  [before after extra]
  (if-let [dropped (dropped-in-order before after extra)]
    (run! delete-event! dropped)
    (let [retained (into #{} (keep held) after)]
      (doseq [descriptor (cond-> (vec before)
                           extra
                           (conj extra))
              :when (not (contains? retained (held descriptor)))]

        (delete-event! descriptor)))))
