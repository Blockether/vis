(ns com.blockether.vis.ext.channel-tui.header-test
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.header :as header]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.internal.header :as vh]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private right-block-text (deref #'header/right-block-text))

(defn- dummy-text-graphics
  ([] (dummy-text-graphics (atom [])))
  ([writes]
   (let
     [active
      (atom #{})

      fg
      (atom nil)

      bg
      (atom nil)]

     (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
       (clearModifiers [] (reset! active #{}) this)
       (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr] (swap! active into (seq arr)) this)
       (getActiveModifiers []
         (if (empty? @active)
           (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
           (java.util.EnumSet/copyOf ^java.util.Collection @active)))
       (setForegroundColor [color] (reset! fg color) this)
       (setBackgroundColor [color] (reset! bg color) this)
       (fillRectangle [_ _ _] this)
       (putString
         ([col row text]
          (swap! writes conj {:col col :row row :text text :fg @fg :bg @bg :modifiers @active})
          this))
       (setCharacter [col row ch]
         (swap! writes conj {:col col :row row :char ch :fg @fg :bg @bg :modifiers @active})
         this)))))

(defdescribe right-block-text-test
             (it "shows the short session id only"
                 ;; The id badge is a space-padded COPY BUTTON: the `#`-prefixed short id.
                 ;; The full UUID still lands on the clipboard via the click region; only
                 ;; the visible label changed shape.
                 (expect (= " #4b1ed602 " (right-block-text "4b1ed602")))))

(defdescribe
  draw-header-copy-region-test
  (it
    "registers a single click region for id copy (no Markdown copy)"
    (let
      [uuid
       "123e4567-e89b-12d3-a456-426614174000"

       ;; The badge now renders a space-padded copy BUTTON (` #123e4567 `);
       ;; the click region spans the whole chip and still copies the FULL
       ;; uuid. The chip is right-aligned but never crosses into the centre
       ;; slot, so its col is clamped to the right-slot start.
       id-rendered
       " #123e4567 "

       id-w
       (p/display-width id-rendered)

       cols
       140

       right-x
       (- cols vh/right-slot-cols)

       expected-col
       (max right-x (- cols 1 id-w))

       db
       {:title "Chat" :session {:id uuid}}

       writes
       (atom [])]

      (cr/reset!)
      (cr/begin-frame!)
      (header/draw-header! (dummy-text-graphics writes) db 0 cols)
      (cr/commit-frame!)
      (let
        [copy-hit
         (some #(when (= :copy-id (:kind %)) %) (cr/current))

         md-hit
         (some #(when (= :copy-as-markdown (:kind %)) %) (cr/current))]

        (expect (= uuid (:text copy-hit)))
        (expect (= {:row 1 :col expected-col :width id-w} (:bounds copy-hit)))
        (expect (nil? md-hit)))))
  (it "can repaint header hover chrome without mutating click-region staging"
      (let
        [uuid
         "123e4567-e89b-12d3-a456-426614174000"

         db
         {:title "Chat" :session {:id uuid}}

         g
         (dummy-text-graphics)]

        (cr/reset!)
        (cr/begin-frame!)
        (binding [header/*register-click-regions?* false]
          (header/draw-header! g db 0 80))
        (cr/commit-frame!)
        (expect (= [] (cr/current)))))
  (it
    "renders notifications only on the left and suppresses duplicate channel status text"
    (let
      [uuid
       "123e4567-e89b-12d3-a456-426614174000"

       status-text
       "● Recording 00:01"

       notification
       "✓ Copied!"

       ;; space-padded copy-button badge label (see copy-region test).
       id-rendered
       " #123e4567 "

       id-w
       (p/display-width id-rendered)

       cols
       80

       right-x
       (- cols (quot cols 5))

       expected-id-col
       (max right-x (- cols 1 id-w))

       db
       {:title "Chat"
        :session {:id uuid}
        :channel-status {:voice/input {:text status-text :level :warn :updated-at-ms 1}}}

       writes
       (atom [])]

      (cr/reset!)
      (with-redefs-fn {#'header/latest-notification (fn []
                                                      {:text notification :level :success})}
        (fn []
          (cr/begin-frame!)
          (header/draw-header! (dummy-text-graphics writes) db 0 cols)
          (cr/commit-frame!)))
      (let
        [write-by-text
         (fn [text]
           (some #(when (= text (:text %)) %) @writes))

         copy-hit
         (some #(when (= :copy-id (:kind %)) %) (cr/current))]

        (expect (= 1 (:col (write-by-text notification))))
        (expect (= t/footer-fg-strong (:fg (write-by-text notification))))
        (expect (nil? (write-by-text status-text)))
        (expect (= {:row 1 :col expected-id-col :width id-w} (:bounds copy-hit))))))
  (it
    "renders channel status on the left when no notification is active"
    (let
      [uuid
       "123e4567-e89b-12d3-a456-426614174000"

       status-text
       "● Recording 00:01"

       left-slot-w
       vh/left-slot-cols

       status-shown
       (p/truncate-cols status-text (- left-slot-w 2))

       db
       {:title "Chat"
        :session {:id uuid}
        :channel-status {:voice/input {:text status-text :level :warn :updated-at-ms 1}}}

       writes
       (atom [])]

      (cr/reset!)
      (with-redefs-fn {#'header/latest-notification (fn []
                                                      nil)}
        (fn []
          (cr/begin-frame!)
          (header/draw-header! (dummy-text-graphics writes) db 0 80)
          (cr/commit-frame!)))
      (let
        [write-by-text (fn [text]
                         (some #(when (= text (:text %)) %) @writes))]
        (expect (= 1 (:col (write-by-text status-shown))))
        (expect (= t/footer-warning-fg (:fg (write-by-text status-shown)))))))
  (it "does not render stale ready voice status forever"
      (let
        [uuid
         "123e4567-e89b-12d3-a456-426614174000"

         status
         "Voice response complete 100%"

         writes
         (atom [])

         db
         {:title "Chat"
          :session {:id uuid}
          :channel-status {:voice/piper
                           {:text status :phase :ready :level :info :updated-at-ms 1}}}]

        (cr/reset!)
        (with-redefs-fn {#'header/latest-notification (fn []
                                                        nil)}
          (fn []
            (cr/begin-frame!)
            (header/draw-header! (dummy-text-graphics writes) db 0 80)
            (cr/commit-frame!)))
        (expect (not-any? #(= status (:text %)) @writes)))))

(defdescribe
  draw-header-color-test
  (it "uses a subtly different foreground for the hovered header copy affordance only"
      (cr/reset!)
      (let
        [writes
         (atom [])

         g
         (dummy-text-graphics writes)

         uuid
         "123e4567-e89b-12d3-a456-426614174000"

         db
         {:title "New Session" :session {:id uuid}}]

        (cr/begin-frame!)
        (header/draw-header! g db 0 160)
        (cr/commit-frame!)
        (let [copy-hit (some #(when (= :copy-id (:kind %)) %) (cr/current))]
          (expect (some? copy-hit))
          (expect (true? (cr/set-hovered! copy-hit)))
          (reset! writes [])
          (cr/begin-frame!)
          (header/draw-header! g db 0 160)
          (cr/commit-frame!)
          (let
            [title-write
             (some #(when (and (string? (:text %)) (str/includes? (:text %) "New Session")) %)
                   @writes)
             write-by-text (fn [text]
                             (some #(when (= text (:text %)) %) @writes))]

            ;; A single session now renders as an active tab (its label carries
            ;; the active-tab fg), while the copy badge keeps its own hover fg.
            (expect (= t/header-active-tab-fg (:fg title-write)))
            ;; Badge is a copy BUTTON (` #123e4567 `); hovering it lifts the
            ;; chip to the shared button hover fg (the tab keeps its active-tab fg).
            (expect (= t/header-active-tab-fg (:fg (write-by-text " #123e4567 ")))))))))

(defdescribe
  draw-header-tab-entries-test
  (it
    "shows clickable arrows when workspaces overflow the 60 percent center slot"
    (cr/reset!)
    (let
      [writes
       (atom [])

       g
       (dummy-text-graphics writes)

       tabs
       (mapv (fn [i]
               {:id (keyword (str "tab-" i)) :label (str "Tab " i)})
             (range 1 9))

       db
       {:title "Chat"
        :session {:id "123e4567-e89b-12d3-a456-426614174000"}
        :active-tab-id :tab-5
        :tabs tabs}]

      (cr/begin-frame!)
      (header/draw-header! g db 0 160)
      (cr/commit-frame!)
      (let
        [left-arrow
         (some #(when (and (= :workspace-entry (:kind %)) (= :prev (:index %))) %) (cr/current))

         right-arrow
         (some #(when (and (= :workspace-entry (:kind %)) (= :next (:index %))) %) (cr/current))

         active-hit
         (some #(when (and (= :workspace-entry (:kind %)) (= :tab-5 (:workspace-id %))) %)
               (cr/current))]

        ;; The tab strip is shifted right by 4 cols — the leftmost ` + ` new-session
        ;; button (3 cols) plus a 1-col gap — so the prev arrow moved 51→55. The
        ;; right arrow is unchanged: `+` shrinks the window's width by the same 4 it
        ;; pushed the left edge, so `left+width` (the right arrow's
        ;; anchor) is invariant.
        (expect (= {:row 1 :col 55 :width 3} (:bounds left-arrow)))
        (expect (= {:row 1 :col 106 :width 3} (:bounds right-arrow)))
        (expect (some? active-hit))
        (expect (= left-arrow (cr/lookup 55 1)))
        ;; col 51 is now the ` + ` new-session button, ahead of the tab strip.
        (expect (= :header-new-session (:kind (cr/lookup 51 1))))
        ;; The right nav arrow sits at the centre's right edge, which the
        ;; right-aligned F1/F2/F3 chip cluster paints over - so it is
        ;; registered + correctly bounded but not the topmost click target.
        (expect (some? right-arrow)))))
  (it "pads workspace labels with breathing room inside each cell"
      ;; The 62-col centre slot reserves 4 cols at the left for the ` + `
      ;; new-session button (3) + a 1-col gap, leaving 58 for the tabs. With 3
      ;; workspaces and 2 dividers that's 56 shared → cells of 19/19/18. The first
      ;; cell is 19 wide.
      ;; tab-entry-padding=1 reserves a space on each side, so the rendered text
      ;; starts and ends with a space even when the label is short.
      (cr/reset!)
      (let
        [writes
         (atom [])

         g
         (dummy-text-graphics writes)

         db
         {:title "Chat"
          :session {:id "123e4567-e89b-12d3-a456-426614174000"}
          :active-tab-id :main
          :tabs [{:id :main :label "Main"} {:id :two :label "Two"} {:id :three :label "Three"}]}]

        (cr/begin-frame!)
        (header/draw-header! g db 0 166)
        (cr/commit-frame!)
        (let
          [tab-writes
           (filter #(and (= 1 (:row %)) (string? (:text %))) @writes)

           main-write
           (some #(when (str/includes? (:text %) "Main") %) tab-writes)]

          (expect (some? main-write))
          ;; First and last visible cells must keep at least one padding cell.
          (expect (str/starts-with? (:text main-write) " "))
          (expect (str/ends-with? (:text main-write) " "))
          ;; Each cell now reserves `components/close-button-width` (3) cells on
          ;; the right for the always-visible ✕ close button (` ✕ `, no divider),
          ;; so the first cell (19 cols wide) paints its label over 19-3 = 16 cols
          ;; and a separate " ✕ " write covers the rest.
          (expect (= 16 (p/display-width (:text main-write))))
          (expect (some #(str/includes? (str (:text %)) "✕") tab-writes)))))
  (it "omits the ✕ close button when there's only ONE session (the last tab can't be closed)"
      (cr/reset!)
      (let
        [writes
         (atom [])

         g
         (dummy-text-graphics writes)

         db
         {:title "Solo"
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
      (let
        [writes
         (atom [])

         g
         (dummy-text-graphics writes)

         db
         {:title "Chat"
          :session {:id "123e4567-e89b-12d3-a456-426614174000"}
          :active-tab-id :one
          :tabs (mapv (fn [i]
                        {:id (keyword (str "t-" i)) :label (str "LongTabLabel" i)})
                      (range 5))}]

        (cr/begin-frame!)
        (header/draw-header! g db 0 160)
        (cr/commit-frame!)
        (let
          [tab-writes
           (filter #(and (= 1 (:row %)) (string? (:text %))) @writes)

           ellipsised
           (some #(when (str/includes? (:text %) "…") %) tab-writes)]

          (expect (some? ellipsised)))))
  (it "clamps visible workspace count to at most 8 even when the centre slot is huge"
      ;; cols=400 → centre slot ≈ 240. Without a cap fluid layout would show
      ;; all 12 workspaces; the policy caps the visible window at 8 and the rest
      ;; reach via the prev/next arrows.
      (cr/reset!)
      (let
        [writes
         (atom [])

         g
         (dummy-text-graphics writes)

         tabs
         (mapv (fn [i]
                 {:id (keyword (str "big-" i)) :label (str "Big " i)})
               (range 12))

         db
         {:title "Chat"
          :session {:id "123e4567-e89b-12d3-a456-426614174000"}
          :active-tab-id :big-0
          :tabs tabs}]

        (cr/begin-frame!)
        (header/draw-header! g db 0 400)
        (cr/commit-frame!)
        (let
          [tab-hits-by-id
           (filter #(and (= :workspace-entry (:kind %)) (integer? (:index %))) (cr/current))

           has-arrows?
           (boolean (and (some #(= :prev (:index %)) (cr/current))
                         (some #(= :next (:index %)) (cr/current))))]

          (expect (= 8 (count tab-hits-by-id)))
          (expect has-arrows?))))
  (it "keeps the natural-fit count when the slot is too narrow for the min cap"
      ;; cols=150 -> centre slot 54, natural fit = quot(54,14) = 3 < min=5,
      ;; so we degrade to the natural fit instead of squeezing five
      ;; unreadable workspaces into 54 cols.
      (cr/reset!)
      (let
        [writes
         (atom [])

         g
         (dummy-text-graphics writes)

         tabs
         (mapv (fn [i]
                 {:id (keyword (str "narrow-" i)) :label (str "N" i)})
               (range 8))

         db
         {:title "Chat"
          :session {:id "123e4567-e89b-12d3-a456-426614174000"}
          :active-tab-id :narrow-0
          :tabs tabs}]

        (cr/begin-frame!)
        (header/draw-header! g db 0 150)
        (cr/commit-frame!)
        (let
          [tab-hits-by-id (filter #(and (= :workspace-entry (:kind %)) (integer? (:index %)))
                                  (cr/current))]
          (expect (= 3 (count tab-hits-by-id)))))))
