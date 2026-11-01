(ns com.blockether.vis.internal.gateway.replay-ring-test
  (:require [com.blockether.vis.internal.gateway.replay-ring :as replay-ring]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- model-step
  "One operation on the old ring model: a `PersistentQueue` filtered and rebuilt."
  [queue [op frame]]
  (case op
    :conj
    (conj queue frame)

    :pop
    (pop queue)

    :replace
    (conj (into clojure.lang.PersistentQueue/EMPTY (remove #(= (:k frame) (:k %))) queue) frame)

    :remove
    (into clojure.lang.PersistentQueue/EMPTY (remove #(identical? frame %)) queue)))

(defn- ring-step
  [ring [op frame]]
  (case op
    :conj
    (conj ring frame)

    :pop
    (pop ring)

    :replace
    (conj (first (replay-ring/remove-key ring (:k frame))) frame)

    :remove
    (replay-ring/remove-frame ring frame)))

(defn- operations
  "A seeded run of appends, evictions, replacements and removals of one frame."
  [seed n]
  (let [rnd (java.util.Random. seed)]
    (loop [i 0
           live []
           ops []]

      (if (= i n)
        ops
        (let [roll (.nextInt rnd 10)
              frame {:seq i :k (when (< (.nextInt rnd 3) 2) (.nextInt rnd 4))}
              op (cond (and (= roll 0) (seq live)) [:pop nil]
                       (and (= roll 1) (seq live)) [:remove (nth live (.nextInt rnd (count live)))]
                       (and (< roll 5) (:k frame)) [:replace frame]
                       :else [:conj frame])]

          (recur (inc i)
                 (case (first op)
                   :pop
                   (subvec live 1)

                   :remove
                   (filterv #(not (identical? (second op) %)) live)

                   :replace
                   (conj (filterv #(not= (:k frame) (:k %)) live) frame)

                   :conj
                   (conj live frame))
                 (conj ops op)))))))

(defdescribe replay-ring-test
             ;; A replaced Activity snapshot that stays in the ring replays a stale form state.
             (it "keeps the frames of the queue model after every operation"
                 (doseq [seed (range 20)]
                   (let [ops (operations seed 400)
                         models (reductions model-step clojure.lang.PersistentQueue/EMPTY ops)
                         rings (reductions ring-step (replay-ring/ring [] :k) ops)]

                     (expect (every? true? (map #(= (seq %1) (seq %2)) models rings)))
                     (expect (= (map peek models) (map peek rings)))
                     (expect (= (map count models) (map count rings))))))
             (it "returns the removed frames in ring order"
                 (let [ring
                       (into (replay-ring/ring [] :k)
                             [{:seq 1 :k :a} {:seq 2 :k :b} {:seq 3 :k :a}])

                       [kept removed]
                       (replay-ring/remove-key ring :a)]

                   (expect (= [{:seq 2 :k :b}] (vec kept)))
                   (expect (= [1 3] (mapv :seq removed)))
                   (expect (= [ring []] (replay-ring/remove-key ring :c))))))
