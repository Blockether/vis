(ns com.blockether.vis.tui.scene-motion
  "Motion of a scene between two patches, without a terminal.

   A scene patch comes about ten times a second. The terminal paints faster, so
   each object eases from where it is to its new pose, as in the Companion
   (`scene.ts`): a turn takes the short way round, and in a wrapping field an
   object crosses the near edge. Patches never come exactly one tick apart, so a
   move lasts as long as the measured gap between them, with a margin."
  (:require [com.blockether.vis.contract.view :as spec]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* :warn-on-boxed)

(def ^:private pose-keys [:x :y :z :rx :ry :rz :scale])

(def ^:private ease-slack
  "How much longer than the gap between two patches a move lasts. A move that
   ends a little after the next patch arrives never stands still."
  1.15)

(defn- modulo
  ^double [^double v ^double size]
  (let [m (rem v size)]
    (if (neg? m) (+ m size) m)))

(defn- shortest
  "The shortest signed way from `from` to `to` on a loop of `size`."
  ^double [^double from ^double to ^double size]
  (- (modulo (+ (- to from) (/ size 2.0)) size) (/ size 2.0)))

(defn- value ^double [o k] (double (or (get o k) (get (:object spec/scene-defaults) k) 0)))

(defn ease-pose
  "The pose at `t` (0..1) between poses `from` and `to` in `scene`."
  [from to ^double t {:keys [width height is-wrapping]}]
  (let [k
        (min 1.0 (max 0.0 t))

        along
        (fn [key size]
          (let [a
                (value from key)

                b
                (value to key)]

            (if size
              (modulo (+ a (* k (shortest a b (double size)))) (double size))
              (+ a (* k (- b a))))))

        turn
        (fn [key]
          (let [a (value from key)]
            (+ a (* k (shortest a (value to key) 360.0)))))]

    {:x (along :x (when is-wrapping width))
     :y (along :y (when is-wrapping height))
     :z (along :z nil)
     :rx (turn :rx)
     :ry (turn :ry)
     :rz (turn :rz)
     :scale (along :scale nil)}))

(defn scene-gap
  "The smoothed gap between two patches in ms. One late patch moves it only a
   little, and a pause longer than three ticks does not count."
  ^double [^double gap ^double sample ^double tick]
  (if (or (<= sample 0.0) (> sample (* 3.0 tick))) gap (+ (* gap 0.75) (* sample 0.25))))

(defn ease-duration
  "How long one move lasts: the gap between patches and a margin, from one to two ticks."
  ^double [^double tick ^double gap]
  (if (<= tick 0.0) 0.0 (min (* tick 2.0) (* (max tick gap) (double ease-slack)))))

(defn ease-at
  "The pose of `ease` at `now` in `scene`."
  [{:keys [from to start duration]} ^double now scene]
  (let [duration (double duration)]
    (ease-pose from to (if (pos? duration) (/ (- now (double start)) duration) 1.0) scene)))

(defn retarget
  "The ease after a patch gives an object the pose `to`. It starts where the
   object is now. A pose that did not change keeps its ease, so a patch for other
   objects does not slow this one."
  [ease to now duration scene]
  (cond (nil? ease) {:from to :to to :start now :duration duration}
        (every? #(== (value (:to ease) %) (value to %)) pose-keys) ease
        :else {:from (ease-at ease (double now) scene) :to to :start now :duration duration}))

(defn step
  "The motion of scene `node` at `now` ms, after the motion `held` (nil at first).
   Answers `{:held motion :node node-to-draw :is-moving bool}`."
  [held node ^double now]
  (let [{:keys [objects tick-ms]}
        node

        tick
        (double (or tick-ms (:tick-ms spec/scene-defaults) 0))

        changed?
        (not= objects (:objects held))

        gap
        (cond (nil? held) tick
              changed? (scene-gap (double (:gap held)) (- now (double (:at held))) tick)
              :else (double (:gap held)))

        duration
        (ease-duration tick gap)

        eases
        (if changed?
          (into {}
                (map (fn [o]
                       [(:id o) (retarget (get-in held [:eases (:id o)]) o now duration node)]))
                objects)
          (:eases held))

        drawn
        (mapv (fn [o]
                (if-let [ease (get eases (:id o))]
                  (merge o (ease-at ease now node))
                  o))
              objects)]

    {:held {:objects objects :eases eases :gap gap :at (if changed? now (:at held))}
     :node (assoc node :objects drawn)
     :is-moving (boolean (some (fn [{:keys [start duration]}]
                                 (< (- now (double start)) (double duration)))
                               (vals eases)))}))
