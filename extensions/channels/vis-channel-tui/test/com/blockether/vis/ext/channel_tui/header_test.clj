(ns com.blockether.vis.ext.channel-tui.header-test
  (:require [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.header :as header]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private right-block-text
  (deref #'header/right-block-text))

(defn- dummy-text-graphics
  []
  (let [active (atom #{})]
    (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
      (clearModifiers []
        (reset! active #{})
        this)
      (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr]
        (swap! active into (seq arr))
        this)
      (getActiveModifiers []
        (if (empty? @active)
          (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
          (java.util.EnumSet/copyOf ^java.util.Collection @active)))
      (setForegroundColor [_] this)
      (setBackgroundColor [_] this)
      (fillRectangle [_ _ _] this)
      (putString
        ([_ _ _] this))
      (setCharacter [_ _ _] this))))

(defdescribe right-block-text-test
  (it "uses the same icon + Copy label as the message bubble"
    (expect (= "4b1ed602 ⧉ Copy" (right-block-text "4b1ed602")))))

(defdescribe draw-header-copy-region-test
  (it "registers a click region covering the short id and the full Copy label"
    (let [uuid          "123e4567-e89b-12d3-a456-426614174000"
          short-id      "123e4567"
          rendered      (right-block-text short-id)
          expected-w    (p/display-width rendered)
          cols          60
          expected-col  (- cols 1 expected-w)
          db            {:title "Chat"
                         :conversation {:id uuid}}]
      (cr/reset!)
      (cr/begin-frame!)
      (header/draw-header! (dummy-text-graphics) db 0 cols)
      (cr/commit-frame!)
      (let [hit (some #(when (= :copy-id (:kind %)) %) (cr/current))]
        (expect (= uuid (:text hit)))
        (expect (= {:row 1 :col expected-col :width expected-w}
                  (:bounds hit)))))))
