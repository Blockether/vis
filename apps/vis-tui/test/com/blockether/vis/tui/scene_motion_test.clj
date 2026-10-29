(ns com.blockether.vis.tui.scene-motion-test
  (:require [com.blockether.vis.tui.scene-motion :as motion]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defn- scene
  "A 16 x 9 scene with a 100 ms tick and `objects`."
  [& objects]
  {:id "field" :width 16 :height 9 :tick-ms 100 :objects (vec objects)})

(defn- at
  "The object `id` of the scene that `step` draws."
  [{:keys [node]} id]
  (some #(when (= id (:id %)) %) (:objects node)))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1.0e-9))

(defn- pose [x rz] {:x x :y 4 :z 0 :rx 0 :ry 0 :rz rz :scale 1})

(defdescribe
  motion-test
  (describe "one object between two ticks"
            ;; A ship that leaves the right edge of a wrapping field must slide out over
            ;; that edge, not fly back through the whole field.
            (it "crosses the near edge of a wrapping field"
                (expect (< (Math/abs (double (:x (motion/ease-pose (pose 15.5 0)
                                                                   (pose 0.5 0)
                                                                   0.5
                                                                   (assoc (scene)
                                                                     :is-wrapping true)))))
                           1.0e-9))
                (expect (== 8.0 (:x (motion/ease-pose (pose 15.5 0) (pose 0.5 0) 0.5 (scene))))))
            (it "turns the short way round"
                (expect (== 360.0 (:rz (motion/ease-pose (pose 0 350) (pose 0 10) 0.5 (scene)))))
                (expect (== 0.0 (:rz (motion/ease-pose (pose 0 10) (pose 0 350) 0.5 (scene)))))))
  (describe
    "a scene over several patches"
    ;; The terminal drew each patch as it came, so objects jumped ten times a
    ;; second while the Companion moved them smoothly.
    (it "draws an object partway to its new pose"
        (let [first-step
              (motion/step nil (scene {:id "rock" :x 0}) 0.0)

              moved
              (motion/step (:held first-step) (scene {:id "rock" :x 10}) 100.0)

              half
              (motion/step (:held moved) (scene {:id "rock" :x 10}) 157.5)]

          (expect (close? 5.0 (:x (at half "rock"))))
          (expect (:is-moving half))
          (expect (not (:is-moving (motion/step (:held half) (scene {:id "rock" :x 10}) 300.0))))))
    (it "keeps the speed of an object that a patch does not move"
        (let [first-step
              (motion/step nil (scene {:id "rock" :x 0} {:id "shot" :x 0}) 0.0)

              moved
              (motion/step (:held first-step) (scene {:id "rock" :x 10} {:id "shot" :x 0}) 100.0)

              other
              (motion/step (:held moved) (scene {:id "rock" :x 10} {:id "shot" :x 4}) 157.5)

              later
              (motion/step (:held other) (scene {:id "rock" :x 10} {:id "shot" :x 4}) 186.25)]

          (expect (close? 7.5 (:x (at later "rock"))))))
    ;; Patches of an SDK owner come a little more than one tick apart. A move
    ;; that ended after exactly one tick made every object stop and start again.
    (it "keeps moving until a late patch arrives"
        (let [gap (reduce (fn [gap _]
                            (motion/scene-gap gap 120.0 100.0))
                          100.0
                          (range 12))]
          (expect (> (motion/ease-duration 100.0 gap) 120.0))
          (expect (== gap (motion/scene-gap gap 5000.0 100.0)))))))
