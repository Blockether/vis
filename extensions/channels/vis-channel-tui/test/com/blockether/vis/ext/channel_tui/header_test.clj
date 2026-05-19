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
                         :session {:id uuid}}
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
          db   {:title "Chat" :session {:id uuid}}
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
          id-rendered   "⧉ 123e4567"
          action-w      (p/display-width (right-block-text "123e4567"))
          id-w          (p/display-width id-rendered)
          cols          80
          right-slot-w  (quot cols 5)
          right-x       (- cols right-slot-w)
          gap-w         (p/display-width "  ")
          status-cap    (max 0 (- right-slot-w 1 action-w gap-w))
          status-shown  (p/truncate-cols status-text status-cap)
          status-w      (p/display-width status-shown)
          expected-right-col (max right-x (- cols 1 status-w gap-w action-w))
          expected-id-col    (+ expected-right-col status-w gap-w)
          db            {:title "Chat"
                         :session {:id uuid}
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
        (expect (= expected-right-col (:col (write-by-text status-shown))))
        (expect (= t/footer-warning-fg (:fg (write-by-text status-shown))))
        (expect (= {:row 1 :col expected-id-col :width id-w}
                  (:bounds copy-hit)))))))

(defdescribe draw-header-color-test
  (it "renders the untitled placeholder inside the active tab, not on the left"
    ;; Fresh session has no `:workspace-tabs` in app-db. The
    ;; header synthesises a single active tab whose label is the
    ;; placeholder; the LEFT slot stays empty.
    (cr/reset!)
    (let [writes (atom [])
          g      (dummy-text-graphics writes)
          uuid   "123e4567-e89b-12d3-a456-426614174000"
          db     {:title ""
                  :session {:id uuid}}]
      (header/draw-header! g db 0 80)
      (let [placeholder-write
            (some #(when (and (string? (:text %))
                           (str/includes? (:text %) "Untitled session"))
                     %)
              @writes)
            left-slot-writes
            (filter #(and (= 1 (:row %))
                       (string? (:text %))
                       (not (str/blank? (:text %)))
                       (< (long (or (:col %) 0)) 16))
              @writes)
            write-by-text (fn [text]
                            (some #(when (= text (:text %)) %) @writes))]
        (expect (some? placeholder-write))
        (expect (= t/header-active-tab-fg (:fg placeholder-write)))
        (expect (= t/dialog-title-bg (:bg placeholder-write)))
        (expect (empty? left-slot-writes))
        (expect (= t/header-fg (:fg (write-by-text "⧉ 123e4567")))))))

  (it "uses a subtly different foreground for the hovered header copy affordance only"
    (cr/reset!)
    (let [writes (atom [])
          g      (dummy-text-graphics writes)
          uuid   "123e4567-e89b-12d3-a456-426614174000"
          db     {:title "New Session"
                  :session {:id uuid}}]
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
        (let [tab-write (some #(when (and (string? (:text %))
                                       (str/includes? (:text %) "New Session"))
                                 %)
                          @writes)
              write-by-text (fn [text]
                              (some #(when (= text (:text %)) %) @writes))]
          ;; Title becomes the active tab label — active style applies.
          ;; Active tab fg switched to the high-contrast yellow so the
          ;; light-theme grey-on-grey contrast bug stays fixed.
          (expect (= t/header-active-tab-fg (:fg tab-write)))
          (expect (= t/header-hover-fg (:fg (write-by-text "⧉ 123e4567")))))))))

(defdescribe draw-header-workspace-tabs-test
  (it "renders tabs in the center header slot without adding rows"
    (cr/reset!)
    (let [writes (atom [])
          g      (dummy-text-graphics writes)
          db     {:title "Chat"
                  :session {:id "123e4567-e89b-12d3-a456-426614174000"}
                  :active-workspace-id :feature
                  :workspace-tabs [{:id :main :label "Main"}
                                   {:id :feature :label "Feature" :dirty? true}
                                   {:id :verify :label "Verify" :state :running}]}]
      (expect (= 3 (header/header-rows (assoc db :workspace-tabs [{:id :main}]))))
      (expect (= 3 (header/header-rows db)))
      (cr/begin-frame!)
      (header/draw-header! g db 0 80)
      (cr/commit-frame!)
      (let [tab-writes (filter #(= 1 (:row %)) @writes)
            ;; LEFT 20% (cols 0..15) stays empty — title lives in the tab now.
            left-slot-writes (filter #(and (= 1 (:row %))
                                        (string? (:text %))
                                        (not (str/blank? (:text %)))
                                        (< (long (or (:col %) 0)) 16))
                               @writes)
            layout     (p/tab-layout (:workspace-tabs db) 16 48 :feature {:gap 0})
            expected   (nth layout 1)
            tab-hit    (some #(when (and (= :workspace-tab (:kind %))
                                      (= 1 (:index %)))
                                %)
                         (cr/current))
            tab-write  (fn [label]
                         (some #(when (= label (str/trim (:text %))) %) tab-writes))
            top-rule   (some #(when (and (= 0 (:row %))
                                      (= 0 (:col %))
                                      (= p/BOX_H (:char %)))
                                %)
                         @writes)
            bottom-rule (some #(when (and (= 2 (:row %))
                                       (= 0 (:col %))
                                       (= p/BOX_H (:char %)))
                                 %)
                          @writes)
            main-tab   (tab-write "Main")
            active-tab (tab-write "Feature •")
            verify-tab (tab-write "Verify ▶")]
        (expect (= t/footer-fg-muted (:fg top-rule)))
        (expect (= t/footer-fg-muted (:fg bottom-rule)))
        (expect (some? main-tab))
        (expect (some? active-tab))
        (expect (some? verify-tab))
        (expect (= 16 (:col main-tab)))
        (expect (= (:left expected) (:col active-tab)))
        (expect (= t/border-fg (:fg main-tab)))
        (expect (= t/dialog-bg (:bg main-tab)))
        (expect (contains? (:modifiers main-tab) p/ITALIC))
        (expect (not (contains? (:modifiers main-tab) p/BORDERED)))
        (expect (= t/header-active-tab-fg (:fg active-tab)))
        (expect (= t/header-active-tab-bg (:bg active-tab)))
        (expect (contains? (:modifiers active-tab) p/BOLD))
        (expect (not (contains? (:modifiers active-tab) p/BORDERED)))
        (expect (= t/border-fg (:fg verify-tab)))
        (expect (not (contains? (:modifiers verify-tab) p/BORDERED)))
        (expect (empty? left-slot-writes))
        (expect (= {:row 1 :col (:left expected) :width (:width expected)}
                  (:bounds tab-hit)))
        (expect (= :feature (:workspace-id tab-hit)))
        (expect (= tab-hit (cr/lookup (:left expected) 1))))))

  (it "shows clickable arrows when tabs overflow the 60 percent center slot"
    (cr/reset!)
    (let [writes (atom [])
          g      (dummy-text-graphics writes)
          tabs   (mapv (fn [i] {:id (keyword (str "tab-" i)) :label (str "Tab " i)})
                   (range 1 9))
          db     {:title "Chat"
                  :session {:id "123e4567-e89b-12d3-a456-426614174000"}
                  :active-workspace-id :tab-5
                  :workspace-tabs tabs}]
      (cr/begin-frame!)
      (header/draw-header! g db 0 50)
      (cr/commit-frame!)
      (let [left-arrow  (some #(when (and (= :workspace-tab (:kind %)) (= :prev (:index %))) %) (cr/current))
            right-arrow (some #(when (and (= :workspace-tab (:kind %)) (= :next (:index %))) %) (cr/current))
            active-hit  (some #(when (and (= :workspace-tab (:kind %)) (= :tab-5 (:workspace-id %))) %) (cr/current))]
        (expect (= {:row 1 :col 10 :width 1} (:bounds left-arrow)))
        (expect (= {:row 1 :col 39 :width 1} (:bounds right-arrow)))
        (expect (some? active-hit))
        (expect (= left-arrow (cr/lookup 10 1)))
        (expect (= right-arrow (cr/lookup 39 1))))))

  (it "pads tab labels with breathing room inside each cell"
    ;; With 3 tabs in a 48-col centre slot each cell is 16 cols wide.
    ;; tab-padding=1 reserves a space on each side, so the rendered text
    ;; starts and ends with a space even when the label is short.
    (cr/reset!)
    (let [writes (atom [])
          g      (dummy-text-graphics writes)
          db     {:title "Chat"
                  :session {:id "123e4567-e89b-12d3-a456-426614174000"}
                  :active-workspace-id :main
                  :workspace-tabs [{:id :main :label "Main"}
                                   {:id :two :label "Two"}
                                   {:id :three :label "Three"}]}]
      (cr/begin-frame!)
      (header/draw-header! g db 0 80)
      (cr/commit-frame!)
      (let [tab-writes (filter #(and (= 1 (:row %)) (string? (:text %))) @writes)
            main-write (some #(when (str/includes? (:text %) "Main") %) tab-writes)]
        (expect (some? main-write))
        ;; First and last visible cells must keep at least one padding cell.
        (expect (str/starts-with? (:text main-write) " "))
        (expect (str/ends-with? (:text main-write) " "))
        ;; The cell paints its full 16-col width as one string.
        (expect (= 16 (p/display-width (:text main-write)))))))

  (it "truncates oversized tab labels with an ellipsis instead of a hard cut"
    ;; Five long-labelled tabs in a 48-col centre slot → cell width 9 (or 10
    ;; for the first three with the +1 remainder). After 2-col padding the
    ;; inner area is < label width, so truncation kicks in with the
    ;; ellipsis glyph.
    (cr/reset!)
    (let [writes (atom [])
          g      (dummy-text-graphics writes)
          db     {:title "Chat"
                  :session {:id "123e4567-e89b-12d3-a456-426614174000"}
                  :active-workspace-id :one
                  :workspace-tabs (mapv (fn [i] {:id (keyword (str "t-" i))
                                                 :label (str "LongTabLabel" i)})
                                    (range 5))}]
      (cr/begin-frame!)
      (header/draw-header! g db 0 80)
      (cr/commit-frame!)
      (let [tab-writes (filter #(and (= 1 (:row %)) (string? (:text %))) @writes)
            ellipsised (some #(when (str/includes? (:text %) "…") %) tab-writes)]
        (expect (some? ellipsised)))))

  (it "clamps visible tab count to at most 8 even when the centre slot is huge"
    ;; cols=400 → centre slot ≈ 240. Without a cap fluid layout would show
    ;; all 12 tabs; the policy caps the visible window at 8 and the rest
    ;; reach via the prev/next arrows.
    (cr/reset!)
    (let [writes (atom [])
          g      (dummy-text-graphics writes)
          tabs   (mapv (fn [i] {:id (keyword (str "big-" i)) :label (str "Big " i)})
                   (range 12))
          db     {:title "Chat"
                  :session {:id "123e4567-e89b-12d3-a456-426614174000"}
                  :active-workspace-id :big-0
                  :workspace-tabs tabs}]
      (cr/begin-frame!)
      (header/draw-header! g db 0 400)
      (cr/commit-frame!)
      (let [tab-hits-by-id (filter #(and (= :workspace-tab (:kind %))
                                      (integer? (:index %)))
                             (cr/current))
            has-arrows? (boolean (and (some #(= :prev (:index %)) (cr/current))
                                   (some #(= :next (:index %)) (cr/current))))]
        (expect (= 8 (count tab-hits-by-id)))
        (expect has-arrows?))))

  (it "keeps the natural-fit count when the slot is too narrow for the min cap"
    ;; cols=50 → centre slot 30, natural fit = quot(30,14) = 2 < min=5,
    ;; so we degrade to the natural fit instead of squeezing five
    ;; unreadable tabs into 30 cols.
    (cr/reset!)
    (let [writes (atom [])
          g      (dummy-text-graphics writes)
          tabs   (mapv (fn [i] {:id (keyword (str "narrow-" i)) :label (str "N" i)})
                   (range 8))
          db     {:title "Chat"
                  :session {:id "123e4567-e89b-12d3-a456-426614174000"}
                  :active-workspace-id :narrow-0
                  :workspace-tabs tabs}]
      (cr/begin-frame!)
      (header/draw-header! g db 0 50)
      (cr/commit-frame!)
      (let [tab-hits-by-id (filter #(and (= :workspace-tab (:kind %))
                                      (integer? (:index %)))
                             (cr/current))]
        (expect (= 2 (count tab-hits-by-id)))))))
