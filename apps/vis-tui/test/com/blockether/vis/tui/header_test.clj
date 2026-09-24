(ns com.blockether.vis.tui.header-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.header :as header]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as t]
            [com.blockether.vis.tui.header-model :as vh]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna TerminalSize]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(def ^:private right-block-text (deref #'header/right-block-text))

(defn- dummy-text-graphics
  ([] (dummy-text-graphics (atom [])))
  ([writes]
   (let [active
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
       ;; `underline-cell!` reads the painted cell back before folding the
       ;; UNDERLINE in; a nil here makes that a no-op, which is all a colour
       ;; assertion needs from a status border.
       (getCharacter [_col _row] nil)
       (putString
         ([col row text]
          (swap! writes conj {:col col :row row :text text :fg @fg :bg @bg :modifiers @active})
          this))
       (setCharacter [col row ch]
         (swap! writes conj {:col col :row row :char ch :fg @fg :bg @bg :modifiers @active})
         this)))))

(defn- paint-header-grid
  "Paint one real Lanterna back-buffer and return its tab geometry and content row."
  [cols db]
  (Thread/interrupted)
  (let [terminal
        (DefaultVirtualTerminal. (TerminalSize. (int cols) 3))

        screen
        (TerminalScreen. terminal)]

    (try (.startScreen screen)
         (.reset interactions/hit-map)
         (.beginFrame interactions/hit-map)
         (header/draw-header! (.newTextGraphics screen) db 0 cols)
         (.commitFrame interactions/hit-map)
         {:row (apply str
                 (for [x (range cols)]
                   (.getCharacterString (.getBackCharacter screen (int x) 1))))
          :characters (mapv #(.getBackCharacter screen (int %) 1) (range cols))
          :tabs (->> (.current interactions/hit-map)
                     (filter #(and (= :workspace-entry (:kind %)) (integer? (:index %))))
                     (sort-by :index)
                     vec)}
         (finally (.stopScreen screen)))))

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
    (let [uuid
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

      (.reset interactions/hit-map)
      (.beginFrame interactions/hit-map)
      (header/draw-header! (dummy-text-graphics writes) db 0 cols)
      (.commitFrame interactions/hit-map)
      (let [copy-hit
            (some #(when (= :copy-id (:kind %)) %) (.current interactions/hit-map))

            md-hit
            (some #(when (= :copy-as-markdown (:kind %)) %) (.current interactions/hit-map))]

        (expect (= uuid (:text copy-hit)))
        (expect (= {:row 1 :col expected-col :width id-w} (:bounds copy-hit)))
        (expect (nil? md-hit)))))
  (it "can repaint header hover chrome without mutating click-region staging"
      (let [uuid
            "123e4567-e89b-12d3-a456-426614174000"

            db
            {:title "Chat" :session {:id uuid}}

            g
            (dummy-text-graphics)]

        (.reset interactions/hit-map)
        (.beginFrame interactions/hit-map)
        (binding [header/*register-click-regions?* false]
          (header/draw-header! g db 0 80))
        (.commitFrame interactions/hit-map)
        (expect (= [] (.current interactions/hit-map)))))
  (it
    "renders notifications only on the left and suppresses duplicate channel status text"
    (let [uuid
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

      (.reset interactions/hit-map)
      (with-redefs-fn {#'header/latest-notification (fn []
                                                      {:text notification :level :success})}
        (fn []
          (.beginFrame interactions/hit-map)
          (header/draw-header! (dummy-text-graphics writes) db 0 cols)
          (.commitFrame interactions/hit-map)))
      (let [write-by-text
            (fn [text]
              (some #(when (= text (:text %)) %) @writes))

            copy-hit
            (some #(when (= :copy-id (:kind %)) %) (.current interactions/hit-map))]

        (expect (= 1 (:col (write-by-text notification))))
        (expect (= t/footer-fg-strong (:fg (write-by-text notification))))
        (expect (nil? (write-by-text status-text)))
        (expect (= {:row 1 :col expected-id-col :width id-w} (:bounds copy-hit))))))
  (it "renders channel status on the left when no notification is active"
      (let [uuid
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

        (.reset interactions/hit-map)
        (with-redefs-fn {#'header/latest-notification (fn []
                                                        nil)}
          (fn []
            (.beginFrame interactions/hit-map)
            (header/draw-header! (dummy-text-graphics writes) db 0 80)
            (.commitFrame interactions/hit-map)))
        (let [write-by-text (fn [text]
                              (some #(when (= text (:text %)) %) @writes))]
          (expect (= 1 (:col (write-by-text status-shown))))
          (expect (= t/footer-warning-fg (:fg (write-by-text status-shown)))))))
  (it "does not render stale ready voice status forever"
      (let [uuid
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

        (.reset interactions/hit-map)
        (with-redefs-fn {#'header/latest-notification (fn []
                                                        nil)}
          (fn []
            (.beginFrame interactions/hit-map)
            (header/draw-header! (dummy-text-graphics writes) db 0 80)
            (.commitFrame interactions/hit-map)))
        (expect (not-any? #(= status (:text %)) @writes)))))

(defdescribe
  draw-header-color-test
  (it "uses a subtly different foreground for the hovered header copy affordance only"
      (.reset interactions/hit-map)
      (let [writes
            (atom [])

            g
            (dummy-text-graphics writes)

            uuid
            "123e4567-e89b-12d3-a456-426614174000"

            db
            {:title "New Session" :session {:id uuid}}]

        (.beginFrame interactions/hit-map)
        (header/draw-header! g db 0 160)
        (.commitFrame interactions/hit-map)
        (let [copy-hit (some #(when (= :copy-id (:kind %)) %) (.current interactions/hit-map))]
          (expect (some? copy-hit))
          (expect (true? (.setHovered interactions/hit-map copy-hit)))
          (reset! writes [])
          (.beginFrame interactions/hit-map)
          (header/draw-header! g db 0 160)
          (.commitFrame interactions/hit-map)
          (let [title-write
                (some #(when (and (string? (:text %)) (str/includes? (:text %) "New Session")) %)
                      @writes)
                write-by-text (fn [text]
                                (some #(when (= text (:text %)) %) @writes))]

            ;; The title stays legible while the copy badge keeps its own hover fg.
            (expect (= t/header-active-tab-fg (:fg title-write)))
            ;; Badge is a copy BUTTON (` #123e4567 `); hovering fills the chip
            ;; with the shared accent while preserving its inverse foreground.
            (expect (= t/header-active-tab-fg (:fg (write-by-text " #123e4567 "))))
            (expect (= t/header-active-tab-accent (:bg (write-by-text " #123e4567 ")))))))))

;; Regression: removing the numbered strip must leave only the active session title,
;; even when other sessions remain live in the local state.
(defdescribe
  single-session-title-test
  (it "centers one title and registers no tab targets on a wide terminal"
      (let [db
            {:title "Current conversation"
             :session {:id "123e4567-e89b-12d3-a456-426614174000"}
             :active-tab-id :current
             :tabs [{:id :other :label "Other conversation"}
                    {:id :current :label "Outdated label"}]}

            {:keys [row tabs]}
            (paint-header-grid 160 db)

            {:keys [center-x center-w]}
            (vh/slot-layout 160)

            start
            (.indexOf ^String row "Current conversation")]

        (expect (= (+ center-x (quot (- center-w (count "Current conversation")) 2)) start))
        (expect (not (str/includes? row "Other conversation")))
        (expect (not (str/includes? row "Outdated label")))
        (expect (not (str/includes? row " + ")))
        (expect (empty? tabs))
        (expect (not-any? #(#{:workspace-entry :close-tab} (:kind %))
                          (.current interactions/hit-map)))))
  (it "ellipsizes the active title without showing other session names at narrow widths"
      (let [row (:row (paint-header-grid 80
                                         {:title "A current conversation with a very long title"
                                          :session {:id "123e4567-e89b-12d3-a456-426614174000"}
                                          :tabs [{:id :other :label "Other conversation"}]}))]
        (expect (str/includes? row "A current conversation"))
        (expect (str/includes? row "…"))
        (expect (not (str/includes? row "Other conversation")))))
  (it "uses the shared untitled fallback"
      (expect (str/includes? (:row (paint-header-grid
                                     160
                                     {:session {:id "123e4567-e89b-12d3-a456-426614174000"}}))
                             vh/untitled-session-label))))
