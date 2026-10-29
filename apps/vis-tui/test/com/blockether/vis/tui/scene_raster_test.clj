(ns com.blockether.vis.tui.scene-raster-test
  (:require [com.blockether.vis.contract.view :as spec]
            [com.blockether.vis.tui.scene-raster :as raster]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defn- pixel
  "The `#rrggbb` of pixel `x`, `y` of a raster."
  [{:keys [width pixels]} x y]
  (format "#%06x" (aget ^ints pixels (+ (long x) (* (long y) (long width))))))

(defn- drawn
  "A 10 x 10 orthographic scene with `objects`, drawn at 20 x 20 pixels."
  [scene objects]
  (raster/raster (merge {:width 10 :height 10 :fov 0 :objects objects} scene) 20 20))

(def ^:private box {:id "box" :mesh "box" :x 5 :y 5 :scale 2})

(def ^:private bar
  "A box stretched along y: it shows which way a turn sent the y axis."
  (let [{:keys [faces]} (get spec/scene-meshes "box")]
    {:id "bar"
     :vertices (vec (for [x [-0.3 0.3]
                          y [-4 4]
                          z [-0.3 0.3]]

                      [x y z]))
     :faces faces}))

(defdescribe
  raster-test
  (describe "shading"
            (it "lights a face with the contract light, so a TUI face matches the Companion face"
                (let [{:keys [ambient diffuse direction]}
                      spec/scene-light

                      k
                      (+ (double ambient) (* (double diffuse) (double (nth direction 2))))]

                  (expect (= (format "#%02x0000" (Math/round (* 255.0 k)))
                             (pixel (drawn {} [(assoc box :color "#ff0000")]) 10 10)))))
            (it "keeps the exact colour of an emissive object"
                (expect (= "#123456"
                           (pixel (drawn {}
                                         [(assoc box
                                            :color "#123456"
                                            :is-emissive true)])
                                  10
                                  10)))))
  (describe "visibility"
            (it "lets the nearer object win a pixel in either order"
                (let [far
                      (assoc box
                        :id "far"
                        :scale 3
                        :color "#00ff00"
                        :is-emissive true)

                      near
                      (assoc box
                        :id "near"
                        :z 4
                        :color "#0000ff"
                        :is-emissive true)]

                  (expect (= "#0000ff" (pixel (drawn {} [far near]) 10 10)))
                  (expect (= "#0000ff" (pixel (drawn {} [near far]) 10 10)))))
            (it "draws a face that turns to the camera and drops one that turns away"
                (let [mesh
                      (fn [faces]
                        {:id "tri" :vertices [[-3 -3 0] [3 -3 0] [0 3 0]] :faces faces})

                      tri
                      {:id "t" :mesh "tri" :x 5 :y 5 :color "#ffffff" :is-emissive true}]

                  (expect (= "#ffffff" (pixel (drawn {:meshes [(mesh [[0 1 2]])]} [tri]) 10 10)))
                  (expect (= "#0b0e14" (pixel (drawn {:meshes [(mesh [[0 2 1]])]} [tri]) 10 10)))))
            (it "keeps an object smaller than a pixel on screen"
                (expect (= "#ffffff"
                           (pixel (drawn {}
                                         [{:id "shot"
                                           :mesh "sphere"
                                           :x 5
                                           :y 5
                                           :scale 0.01
                                           :color "#ffffff"
                                           :is-emissive true}])
                                  10
                                  10)))))
  (describe
    "placement"
    (it "wraps a place outside a wrapping field back into it"
        (let [shot {:id "s" :mesh "box" :x 15 :y 15 :scale 0.5 :color "#ffffff" :is-emissive true}]
          (expect (= "#ffffff" (pixel (drawn {:is-wrapping true} [shot]) 10 10)))
          (expect (= "#0b0e14" (pixel (drawn {} [shot]) 10 10)))))
    (it "turns about x, then y, then z, as Companion does"
        ;; x 90 puts the bar along z, then y 90 puts it along x. The other order leaves it along z.
        (let [frame
              (drawn
                {:meshes [bar]}
                [{:id "b" :mesh "bar" :x 5 :y 5 :rx 90 :ry 90 :color "#ffffff" :is-emissive true}])]
          (expect (= "#ffffff" (pixel frame 16 10)))
          (expect (= "#0b0e14" (pixel frame 10 16)))))))

(defdescribe size-test
             (it "keeps the scene aspect and an even pixel height inside the row limit"
                 (expect (= [76 42] (raster/size {:width 16 :height 9} 76 30)))
                 (expect (= [23 40] (raster/size {:width 9 :height 16} 76 20)))))
