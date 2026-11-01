(ns com.blockether.vis.internal.gateway.event-store
  "Process-local replay files retain locally assigned cursors, never shared-bus sequence numbers."
  (:require [com.blockether.vis.internal.gateway.turn-archive :as archive]
            [taoensso.telemere :as tel])
  (:import [java.nio.file Files Path]))

(defonce ^:private locks (vec (repeatedly 64 #(Object.))))

(defn lock-for [sid] (nth locks (mod (hash (str sid)) (count locks))))

(def ^:dynamic *max-bytes* (* 16 1024 1024))

(defn write-event!
  "Persist before publishing the descriptor. Failed writes declare a replay gap."
  [event]
  (let [block-id
        (or (get event "block_id") (get-in event ["block" "id"]))

        descriptor
        (cond-> (select-keys event ["seq" "type" "turn_id" "iteration" "form_index"])
          block-id
          (assoc "block_id" block-id))]

    (try (let [file (archive/write! event)]
           (assoc descriptor
             ::file file
             ::bytes (Files/size (Path/of ^String file (make-array String 0)))))
         (catch Exception e
           (tel/log! {:level :error :id ::write-failed :data {:error-class (.getName (class e))}})
           (assoc descriptor ::missing? true)))))

(defn read-event [descriptor] (archive/read-turn (::file descriptor)))

(defn delete-event!
  [descriptor]
  (when-let [file (::file descriptor)]
    (archive/delete! file)))

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

(defn discard-replaced!
  "Delete retired files after the registry transition, never within a CAS retry."
  [before after extra]
  (if-let [dropped (dropped-in-order before after extra)]
    (run! delete-event! dropped)
    (let [retained (into #{} (keep ::file) after)]
      (doseq [descriptor (cond-> (vec before)
                           extra
                           (conj extra))
              :when (not (contains? retained (::file descriptor)))]

        (delete-event! descriptor)))))
