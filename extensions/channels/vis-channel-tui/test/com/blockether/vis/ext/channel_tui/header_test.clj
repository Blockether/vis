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
  (it "shows the short session id only"
    (expect (= "4b1ed602" (right-block-text "4b1ed602")))))

(defdescribe draw-header-copy-region-test
  (it "registers a single click region for id copy (no Markdown copy)"
    (let [uuid          "123e4567-e89b-12d3-a456-426614174000"
          id-rendered   "123e4567"
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

  (it "renders notifications only on the left and suppresses duplicate channel status text"
    (let [uuid          "123e4567-e89b-12d3-a456-426614174000"
          status-text   "● Recording 00:01"
          notification  "✓ Copied!"
          id-rendered   "123e4567"
          id-w          (p/display-width id-rendered)
          cols          80
          expected-id-col (- cols 1 id-w)
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
        (expect (= t/footer-fg-strong (:fg (write-by-text notification))))
        (expect (nil? (write-by-text status-text)))
        (expect (= {:row 1 :col expected-id-col :width id-w}
                  (:bounds copy-hit))))))

  (it "renders channel status on the left when no notification is active"
    (let [uuid          "123e4567-e89b-12d3-a456-426614174000"
          status-text   "● Recording 00:01"
          left-slot-w   16
          status-shown  (p/truncate-cols status-text (- left-slot-w 2))
          db            {:title "Chat"
                         :session {:id uuid}
                         :channel-status {:voice/input {:text status-text
                                                        :level :warn
                                                        :updated-at-ms 1}}}
          writes        (atom [])]
      (cr/reset!)
      (with-redefs-fn {#'header/latest-notification (fn [] nil)}
        (fn []
          (cr/begin-frame!)
          (header/draw-header! (dummy-text-graphics writes) db 0 80)
          (cr/commit-frame!)))
      (let [write-by-text (fn [text]
                            (some #(when (= text (:text %)) %) @writes))]
        (expect (= 1 (:col (write-by-text status-shown))))
        (expect (= t/footer-warning-fg (:fg (write-by-text status-shown)))))))

  (it "does not render stale ready voice status forever"
    (let [uuid   "123e4567-e89b-12d3-a456-426614174000"
          status "Voice response complete 100%"
          writes (atom [])
          db     {:title "Chat"
                  :session {:id uuid}
                  :channel-status {:voice/piper {:text status
                                                 :phase :ready
                                                 :level :info
                                                 :updated-at-ms 1}}}]
      (cr/reset!)
      (with-redefs-fn {#'header/latest-notification (fn [] nil)}
        (fn []
          (cr/begin-frame!)
          (header/draw-header! (dummy-text-graphics writes) db 0 80)
          (cr/commit-frame!)))
      (expect (not-any? #(= status (:text %)) @writes)))))

(defdescribe draw-header-color-test
  (it "renders the lone session as a single active tab in the center"
    ;; Fresh session has no `:tabs` in app-db. The header synthesises one
    ;; workspace and now renders it as a real (active) tab — one consistent
    ;; affordance, no special inert-title path.
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
        ;; A single session now renders as a real (active) tab — not inert
        ;; title text — so it carries the active-tab colors.
        (expect (= t/header-active-tab-fg (:fg placeholder-write)))
        (expect (= t/header-active-tab-bg (:bg placeholder-write)))
        (expect (empty? left-slot-writes))
        (expect (= t/header-fg (:fg (write-by-text "123e4567")))))))

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
        (let [title-write (some #(when (and (string? (:text %))
                                         (str/includes? (:text %) "New Session"))
                                   %)
                            @writes)
              write-by-text (fn [text]
                              (some #(when (= text (:text %)) %) @writes))]
          ;; A single session now renders as an active tab (its label carries
          ;; the active-tab fg), while the copy badge keeps its own hover fg.
          (expect (= t/header-active-tab-fg (:fg title-write)))
          (expect (= t/header-hover-fg (:fg (write-by-text "123e4567")))))))))

(defdescribe draw-header-tab-entries-test
  (it "renders workspace switcher entries in the center header slot without adding rows"
    (cr/reset!)
    (let [writes (atom [])
          g      (dummy-text-graphics writes)
          db     {:title "Chat"
                  :session {:id "123e4567-e89b-12d3-a456-426614174000"}
                  :active-tab-id :feature
                  :tabs [{:id :main :label "Main"}
                               {:id :feature :label "Feature" :dirty? true}
                               {:id :verify :label "Verify" :state :running}]}]
      (expect (= 3 (header/header-rows (assoc db :tabs [{:id :main}]))))
      (expect (= 3 (header/header-rows db)))
      (cr/begin-frame!)
      (header/draw-header! g db 0 80)
      (cr/commit-frame!)
      (let [tab-writes (filter #(= 1 (:row %)) @writes)
            ;; LEFT 20% (cols 0..15) stays empty — title lives in the center slot.
            left-slot-writes (filter #(and (= 1 (:row %))
                                        (string? (:text %))
                                        (not (str/blank? (:text %)))
                                        (< (long (or (:col %) 0)) 16))
                               @writes)
            ;; Tabs now have a 1-col `│` divider between them (gap 1).
            layout     (p/tab-layout (:tabs db) 16 48 :feature {:gap 1})
            expected   (nth layout 1)
            tab-hit    (some #(when (and (= :workspace-entry (:kind %))
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

  (it "shows clickable arrows when workspaces overflow the 60 percent center slot"
    (cr/reset!)
    (let [writes (atom [])
          g      (dummy-text-graphics writes)
          tabs   (mapv (fn [i] {:id (keyword (str "tab-" i)) :label (str "Tab " i)})
                   (range 1 9))
          db     {:title "Chat"
                  :session {:id "123e4567-e89b-12d3-a456-426614174000"}
                  :active-tab-id :tab-5
                  :tabs tabs}]
      (cr/begin-frame!)
      (header/draw-header! g db 0 50)
      (cr/commit-frame!)
      (let [left-arrow  (some #(when (and (= :workspace-entry (:kind %)) (= :prev (:index %))) %) (cr/current))
            right-arrow (some #(when (and (= :workspace-entry (:kind %)) (= :next (:index %))) %) (cr/current))
            active-hit  (some #(when (and (= :workspace-entry (:kind %)) (= :tab-5 (:workspace-id %))) %) (cr/current))]
        (expect (= {:row 1 :col 10 :width 1} (:bounds left-arrow)))
        (expect (= {:row 1 :col 39 :width 1} (:bounds right-arrow)))
        (expect (some? active-hit))
        (expect (= left-arrow (cr/lookup 10 1)))
        (expect (= right-arrow (cr/lookup 39 1))))))

  (it "pads workspace labels with breathing room inside each cell"
    ;; With 3 workspaces in a 48-col centre slot each cell is 16 cols wide.
    ;; tab-entry-padding=1 reserves a space on each side, so the rendered text
    ;; starts and ends with a space even when the label is short.
    (cr/reset!)
    (let [writes (atom [])
          g      (dummy-text-graphics writes)
          db     {:title "Chat"
                  :session {:id "123e4567-e89b-12d3-a456-426614174000"}
                  :active-tab-id :main
                  :tabs [{:id :main :label "Main"}
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
        ;; Each cell now reserves `components/close-button-width` (4) cells on
        ;; the right for the always-visible ✕ close button, so the label cell
        ;; paints 16-4 = 12 cols and a separate "│ ✕ " write covers the rest.
        (expect (= 12 (p/display-width (:text main-write))))
        (expect (some #(str/includes? (str (:text %)) "✕") tab-writes)))))

  (it "omits the ✕ close button when there's only ONE session (the last tab can't be closed)"
    (cr/reset!)
    (let [writes (atom [])
          g      (dummy-text-graphics writes)
          db     {:title "Solo"
                  :session {:id "123e4567-e89b-12d3-a456-426614174000"}
                  :active-tab-id :main
                  :tabs [{:id :main :label "Main"}]}]
      (cr/begin-frame!)
      (header/draw-header! g db 0 80)
      (cr/commit-frame!)
      (let [tab-writes (filter #(and (= 1 (:row %)) (string? (:text %))) @writes)]
        (expect (not-any? #(str/includes? (str (:text %)) "✕") tab-writes))
        (expect (empty? (filter #(= :close-tab (:kind %)) (cr/current)))))))

  (it "truncates oversized workspace labels with an ellipsis instead of a hard cut"
    ;; Five long-labelled workspaces in a 48-col centre slot → cell width 9 (or 10
    ;; for the first three with the +1 remainder). After 2-col padding the
    ;; inner area is < label width, so truncation kicks in with the
    ;; ellipsis glyph.
    (cr/reset!)
    (let [writes (atom [])
          g      (dummy-text-graphics writes)
          db     {:title "Chat"
                  :session {:id "123e4567-e89b-12d3-a456-426614174000"}
                  :active-tab-id :one
                  :tabs (mapv (fn [i] {:id (keyword (str "t-" i))
                                             :label (str "LongTabLabel" i)})
                                (range 5))}]
      (cr/begin-frame!)
      (header/draw-header! g db 0 80)
      (cr/commit-frame!)
      (let [tab-writes (filter #(and (= 1 (:row %)) (string? (:text %))) @writes)
            ellipsised (some #(when (str/includes? (:text %) "…") %) tab-writes)]
        (expect (some? ellipsised)))))

  (it "clamps visible workspace count to at most 8 even when the centre slot is huge"
    ;; cols=400 → centre slot ≈ 240. Without a cap fluid layout would show
    ;; all 12 workspaces; the policy caps the visible window at 8 and the rest
    ;; reach via the prev/next arrows.
    (cr/reset!)
    (let [writes (atom [])
          g      (dummy-text-graphics writes)
          tabs   (mapv (fn [i] {:id (keyword (str "big-" i)) :label (str "Big " i)})
                   (range 12))
          db     {:title "Chat"
                  :session {:id "123e4567-e89b-12d3-a456-426614174000"}
                  :active-tab-id :big-0
                  :tabs tabs}]
      (cr/begin-frame!)
      (header/draw-header! g db 0 400)
      (cr/commit-frame!)
      (let [tab-hits-by-id (filter #(and (= :workspace-entry (:kind %))
                                      (integer? (:index %)))
                             (cr/current))
            has-arrows? (boolean (and (some #(= :prev (:index %)) (cr/current))
                                   (some #(= :next (:index %)) (cr/current))))]
        (expect (= 8 (count tab-hits-by-id)))
        (expect has-arrows?))))

  (it "keeps the natural-fit count when the slot is too narrow for the min cap"
    ;; cols=50 → centre slot 30, natural fit = quot(30,14) = 2 < min=5,
    ;; so we degrade to the natural fit instead of squeezing five
    ;; unreadable workspaces into 30 cols.
    (cr/reset!)
    (let [writes (atom [])
          g      (dummy-text-graphics writes)
          tabs   (mapv (fn [i] {:id (keyword (str "narrow-" i)) :label (str "N" i)})
                   (range 8))
          db     {:title "Chat"
                  :session {:id "123e4567-e89b-12d3-a456-426614174000"}
                  :active-tab-id :narrow-0
                  :tabs tabs}]
      (cr/begin-frame!)
      (header/draw-header! g db 0 50)
      (cr/commit-frame!)
      (let [tab-hits-by-id (filter #(and (= :workspace-entry (:kind %))
                                      (integer? (:index %)))
                             (cr/current))]
        (expect (= 2 (count tab-hits-by-id)))))))
