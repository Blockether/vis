(ns com.blockether.vis.ext.channel-tui.flex-test
  (:require [com.blockether.vis.ext.channel-tui.flex :as flex]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defn- recording-graphics
  "A headless TextGraphics that records every `putString col row text` so a
   layout can be asserted by WHERE its children landed, not painted cells."
  [sink]
  (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
    (clearModifiers [] this)
    (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" _] this)
    (getActiveModifiers [] (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR))
    (setForegroundColor [_] this)
    (setBackgroundColor [_] this)
    (fillRectangle [_ _ _] this)
    (putString ([col row text] (swap! sink conj [col row text]) this))))

(defn- painted
  [node col row]
  (let
    [sink
     (atom [])

     w
     (flex/render! (recording-graphics sink) col row node)]

    {:width w :calls @sink}))

(defdescribe flex-row-test
             (describe
               "row"
               (it "lays children left→right, accumulating width + gap"
                   (let [{:keys [width calls]} (painted (flex/row ["AA" "BBB" "C"] {:gap 1}) 100 5)]
                     (expect (= 8 width)) ; 2+1+3+1+1
                     (expect (= [[100 5 "AA"] [103 5 "BBB"] [107 5 "C"]] calls))))
               (it "with no gap packs children flush"
                   (let [{:keys [calls]} (painted (flex/row ["AA" "BB"]) 0 0)]
                     (expect (= [[0 0 "AA"] [2 0 "BB"]] calls))))
               (it "right-aligns the cluster inside a fixed width"
                   (let
                     [{:keys [width calls]}
                      (painted (flex/row ["AA" "BBB"] {:gap 1 :width 20 :align :right}) 100 5)]
                     (expect (= 20 width))
                     (expect (= [[114 5 "AA"] [117 5 "BBB"]] calls))))
               (it "center-aligns the cluster inside a fixed width"
                   (let [{:keys [calls]} (painted (flex/row ["AB"] {:width 10 :align :center}) 0 0)]
                     (expect (= [[4 0 "AB"]] calls))))
               (it "drops nil children"
                   (let [{:keys [calls]} (painted (flex/row ["A" nil "B"] {:gap 1}) 0 0)]
                     (expect (= [[0 0 "A"] [2 0 "B"]] calls))))))

(defdescribe flex-col-test
             (describe "col"
                       (it "stacks children top→bottom by their own height + gap"
                           (let [{:keys [calls]} (painted (flex/col ["x" "yy" "zzz"] {:gap 1}) 3 7)]
                             (expect (= [[3 7 "x"] [3 9 "yy"] [3 11 "zzz"]] calls))))
                       (it "measures height as sum of child heights plus gaps"
                           (expect (= 5 (flex/height (flex/col ["a" "b" "c"] {:gap 1}))))
                           (expect (= 3 (flex/width (flex/col ["a" "bb" "ccc"])))))))

(defdescribe flex-node-test
             (describe "node bridge"
                       (it "wraps an existing width + paint thunk unchanged"
                           (let
                             [{:keys [width calls]} (painted (flex/node 5
                                                                        (fn [g c r]
                                                                          (.putString g c r "hi")))
                                                             2
                                                             4)]
                             (expect (= 5 width))
                             (expect (= [[2 4 "hi"]] calls))))
                       (it "render! returns 0 and paints nothing for nil"
                           (expect (= {:width 0 :calls []} (painted nil 0 0))))))
