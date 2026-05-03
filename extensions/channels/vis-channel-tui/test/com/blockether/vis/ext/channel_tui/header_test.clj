(ns com.blockether.vis.ext.channel-tui.header-test
  (:require [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.header :as header]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private right-block-text
  (deref #'header/right-block-text))

(defn- dummy-text-graphics
  ([] (dummy-text-graphics (atom [])))
  ([writes]
   (let [active (atom #{})
         fg     (atom nil)
         bg     (atom nil)]
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
       (setForegroundColor [color]
         (reset! fg color)
         this)
       (setBackgroundColor [color]
         (reset! bg color)
         this)
       (fillRectangle [_ _ _] this)
       (putString
         ([col row text]
          (swap! writes conj {:col col
                              :row row
                              :text text
                              :fg @fg
                              :bg @bg
                              :modifiers @active})
          this))
       (setCharacter [_ _ _] this)))))

(defdescribe right-block-text-test
  (it "shows separated copy-id and transcript copy actions"
    (expect (= "⧉ 4b1ed602 | Copy Transcript" (right-block-text "4b1ed602")))))

(defdescribe draw-header-copy-region-test
  (it "registers separate click regions for id copy and Markdown copy"
    (let [uuid          "123e4567-e89b-12d3-a456-426614174000"
          short-id      "123e4567"
          rendered      (right-block-text short-id)
          id-rendered   "⧉ 123e4567"
          separator      " | "
          md-rendered   "Copy Transcript"
          expected-w    (p/display-width rendered)
          id-w          (p/display-width id-rendered)
          separator-w   (p/display-width separator)
          md-w          (p/display-width md-rendered)
          cols          60
          expected-col  (- cols 1 expected-w)
          expected-md-col (+ expected-col id-w separator-w)
          db            {:title "Chat"
                         :conversation {:id uuid}}
          writes        (atom [])]
      (cr/reset!)
      (cr/begin-frame!)
      (header/draw-header! (dummy-text-graphics writes) db 0 cols)
      (cr/commit-frame!)
      (let [copy-hit (some #(when (= :copy-id (:kind %)) %) (cr/current))
            md-hit   (some #(when (= :copy-as-markdown (:kind %)) %) (cr/current))]
        (expect (= uuid (:text copy-hit)))
        (expect (= {:row 1 :col expected-col :width id-w}
                  (:bounds copy-hit)))
        (expect (some #(and (= 1 (:row %))
                         (= (+ expected-col id-w) (:col %))
                         (= separator (:text %))
                         (= t/footer-fg-strong (:fg %))
                         (contains? (:modifiers %) p/BOLD))
                  @writes))
        (expect (= uuid (:text md-hit)))
        (expect (= {:row 1 :col expected-md-col :width md-w}
                  (:bounds md-hit)))))))
