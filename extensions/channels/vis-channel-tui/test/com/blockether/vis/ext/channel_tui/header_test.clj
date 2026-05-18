(ns com.blockether.vis.ext.channel-tui.header-test
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
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
       (setCharacter [col row ch]
         (swap! writes conj {:col col
                             :row row
                             :char ch
                             :fg @fg
                             :bg @bg
                             :modifiers @active})
         this)))))

(defdescribe right-block-text-test
  (it "shows the copy-id affordance only"
    (expect (= "⧉ 4b1ed602" (right-block-text "4b1ed602")))))

(defdescribe draw-header-copy-region-test
  (it "registers a single click region for id copy (no Markdown copy)"
    (let [uuid          "123e4567-e89b-12d3-a456-426614174000"
          id-rendered   "⧉ 123e4567"
          id-w          (p/display-width id-rendered)
          cols          60
          expected-col  (- cols 1 id-w)
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
        (expect (nil? md-hit)))))

  (it "can repaint header hover chrome without mutating click-region staging"
    (let [uuid "123e4567-e89b-12d3-a456-426614174000"
          db   {:title "Chat" :conversation {:id uuid}}
          g    (dummy-text-graphics)]
      (cr/reset!)
      (cr/begin-frame!)
      (binding [header/*register-click-regions?* false]
        (header/draw-header! g db 0 80))
      (cr/commit-frame!)
      (expect (= [] (cr/current)))))

  (it "keeps voice channel status on the right without stealing the notification lane"
    (let [uuid          "123e4567-e89b-12d3-a456-426614174000"
          status-text   "● Recording 00:01"
          notification  "✓ Copied!"
          status-w      (p/display-width status-text)
          gap-w         (p/display-width "  ")
          id-rendered   "⧉ 123e4567"
          action-w      (p/display-width (right-block-text "123e4567"))
          id-w          (p/display-width id-rendered)
          cols          80
          expected-right-col (- cols 1 status-w gap-w action-w)
          expected-id-col    (+ expected-right-col status-w gap-w)
          db            {:title "Chat"
                         :conversation {:id uuid}
                         :channel-status {:voice/input {:text status-text
                                                        :level :warn
                                                        :updated-at-ms 1}}}
          writes        (atom [])]
      (cr/reset!)
      (with-redefs-fn {#'header/latest-notification (fn [] {:text notification :level :success})}
        (fn []
          (cr/begin-frame!)
          (header/draw-header! (dummy-text-graphics writes) db 0 cols)
          (cr/commit-frame!)))
      (let [write-by-text (fn [text]
                            (some #(when (= text (:text %)) %) @writes))
            copy-hit      (some #(when (= :copy-id (:kind %)) %) (cr/current))]
        (expect (= 1 (:col (write-by-text notification))))
        (expect (= expected-right-col (:col (write-by-text status-text))))
        (expect (= t/footer-warning-fg (:fg (write-by-text status-text))))
        (expect (= {:row 1 :col expected-id-col :width id-w}
                  (:bounds copy-hit)))))))

(defdescribe draw-header-color-test
  (it "renders placeholder/title, conversation id, and copy icon in header foreground"
    (cr/reset!)
    (let [writes (atom [])
          g      (dummy-text-graphics writes)
          uuid   "123e4567-e89b-12d3-a456-426614174000"
          db     {:title ""
                  :conversation {:id uuid}}]
      (header/draw-header! g db 0 80)
      (let [write-by-text (fn [text]
                            (some #(when (= text (:text %)) %) @writes))]
        (expect (= t/header-fg (:fg (write-by-text "Untitled conversation"))))
        (expect (= t/header-fg (:fg (write-by-text "⧉ 123e4567")))))))

  (it "uses a subtly different foreground for the hovered header copy affordance only"
    (cr/reset!)
    (let [writes (atom [])
          g      (dummy-text-graphics writes)
          uuid   "123e4567-e89b-12d3-a456-426614174000"
          db     {:title "New Conversation"
                  :conversation {:id uuid}}]
      (cr/begin-frame!)
      (header/draw-header! g db 0 80)
      (cr/commit-frame!)
      (let [copy-hit (some #(when (= :copy-id (:kind %)) %) (cr/current))]
        (expect (some? copy-hit))
        (expect (true? (cr/set-hovered! copy-hit)))
        (reset! writes [])
        (cr/begin-frame!)
        (header/draw-header! g db 0 80)
        (cr/commit-frame!)
        (let [write-by-text (fn [text]
                              (some #(when (= text (:text %)) %) @writes))]
          (expect (= t/header-fg (:fg (write-by-text "New Conversation"))))
          (expect (= t/header-hover-fg (:fg (write-by-text "⧉ 123e4567")))))))))

(defdescribe draw-header-workspace-tabs-test
  (it "renders yellow tab top border and normal header top border without spacer"
    (cr/reset!)
    (let [writes (atom [])
          g      (dummy-text-graphics writes)
          db     {:title "Chat"
                  :conversation {:id "123e4567-e89b-12d3-a456-426614174000"}
                  :active-workspace-id :feature
                  :workspace-tabs [{:id :main :label "Main"}
                                   {:id :feature :label "Feature" :dirty? true}
                                   {:id :verify :label "Verify" :state :running}]}]
      (expect (= 3 (header/header-rows (assoc db :workspace-tabs [{:id :main}]))))
      (expect (= 5 (header/header-rows db)))
      (cr/begin-frame!)
      (header/draw-header! g db 0 80)
      (cr/commit-frame!)
      (let [tab-writes (filter #(= 1 (:row %)) @writes)
            title      (some #(when (= "Chat" (:text %)) %) @writes)
            layout     (p/tab-layout (:workspace-tabs db) 0 80 :feature {:gap 0})
            expected   (nth layout 1)
            tab-hit    (some #(when (and (= :workspace-tab (:kind %))
                                      (= 1 (:index %)))
                                %)
                         (cr/current))
            tab-write  (fn [label]
                         (some #(when (= label (str/trim (:text %))) %) tab-writes))
            tab-top-rule (some #(when (and (= 0 (:row %))
                                        (= 0 (:col %))
                                        (= p/BOX_H (:char %)))
                                  %)
                           @writes)
            top-rule   (some #(when (and (= 2 (:row %))
                                      (= 0 (:col %))
                                      (= p/BOX_H (:char %)))
                                %)
                         @writes)
            main-tab   (tab-write "Main")
            active-tab (tab-write "Feature •")
            verify-tab (tab-write "Verify ▶")]
        (expect (= t/footer-warning-fg (:fg tab-top-rule)))
        (expect (= t/footer-fg-muted (:fg top-rule)))
        (expect (some? main-tab))
        (expect (some? active-tab))
        (expect (some? verify-tab))
        (expect (= 0 (:col main-tab)))
        (expect (= (:left expected) (:col active-tab)))
        (expect (= t/header-fg (:fg main-tab)))
        (expect (= t/dialog-bg (:bg main-tab)))
        (expect (contains? (:modifiers main-tab) p/ITALIC))
        (expect (contains? (:modifiers main-tab) p/BORDERED))
        (expect (= t/header-hover-fg (:fg active-tab)))
        (expect (= t/dialog-title-bg (:bg active-tab)))
        (expect (contains? (:modifiers active-tab) p/BOLD))
        (expect (contains? (:modifiers active-tab) p/BORDERED))
        (expect (= t/header-fg (:fg verify-tab)))
        (expect (contains? (:modifiers verify-tab) p/BORDERED))
        (expect (= 3 (:row title)))
        (expect (= {:row 1 :col (:left expected) :width (:width expected)}
                  (:bounds tab-hit)))
        (expect (= :feature (:workspace-id tab-hit)))
        (expect (= tab-hit (cr/lookup (:left expected) 1)))))))
