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
       (setCharacter [_ _ _] this)))))

(defdescribe right-block-text-test
  (it "shows separated copy-id and transcript copy actions with copy icons"
    (expect (= "⧉ 4b1ed602 | ⧉ Transcript" (right-block-text "4b1ed602")))))

(defdescribe draw-header-copy-region-test
  (it "registers separate click regions for id copy and Markdown copy"
    (let [uuid          "123e4567-e89b-12d3-a456-426614174000"
          short-id      "123e4567"
          rendered      (right-block-text short-id)
          id-rendered   "⧉ 123e4567"
          separator      " | "
          md-rendered   "⧉ Transcript"
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
                         (= t/header-fg (:fg %))
                         (not (contains? (:modifiers %) p/BOLD)))
                  @writes))
        (expect (= uuid (:text md-hit)))
        (expect (= {:row 1 :col expected-md-col :width md-w}
                  (:bounds md-hit))))))

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
  (it "renders placeholder/title, conversation id, transcript label, and copy icons in header foreground"
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
        (expect (= t/header-fg (:fg (write-by-text "⧉ 123e4567"))))
        (expect (= t/header-fg (:fg (write-by-text " | "))))
        (expect (= t/header-fg (:fg (write-by-text "⧉ Transcript")))))))

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
          (expect (= t/header-hover-fg (:fg (write-by-text "⧉ 123e4567"))))
          (expect (= t/header-fg (:fg (write-by-text " | "))))
          (expect (= t/header-fg (:fg (write-by-text "⧉ Transcript")))))))))

(defdescribe draw-header-workspace-tabs-test
  (it "adds one header row and renders workspace tabs only when more than one exists"
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
      (expect (= 4 (header/header-rows db)))
      (cr/begin-frame!)
      (header/draw-header! g db 0 80)
      (cr/commit-frame!)
      (let [tab-writes (filter #(= 0 (:row %)) @writes)
            title      (some #(when (= "Chat" (:text %)) %) @writes)
            layout     (p/tab-layout (:workspace-tabs db) 1 78 :feature)
            expected   (nth layout 1)
            tab-hit    (some #(when (and (= :workspace-tab (:kind %))
                                      (= 1 (:index %)))
                                %)
                         (cr/current))]
        (expect (some #(= "Main" (str/trim (:text %))) tab-writes))
        (expect (some #(= "Feature •" (str/trim (:text %))) tab-writes))
        (expect (some #(= "Verify ▶" (str/trim (:text %))) tab-writes))
        (expect (= 2 (:row title)))
        (expect (= {:row 0 :col (:left expected) :width (:width expected)}
                  (:bounds tab-hit)))
        (expect (= :feature (:workspace-id tab-hit)))
        (expect (= tab-hit (cr/lookup (:left expected) 0)))))))

;; =============================================================================
;; Goal subtitle row (vis-goal extension hook in the header)
;;
;; Goal data lives in the extension_aggregate table; the header reads it
;; via `requiring-resolve` so vis still boots without vis-goal on the
;; classpath. Tests wire a temp in-memory store and the goal extension
;; directly to exercise the full path. Lifted into its own defdescribe so
;; the existing layout tests above stay independent of goal state.
;; =============================================================================

(require '[com.blockether.vis.core :as vis]
  '[com.blockether.vis.ext.goal.core :as goal-ext])

(defn- with-temp-db
  "Bind `vis/db-info` to an in-memory store for one test body, then
   dispose. Header reads goals via `(vis/db-info)` so the binding
   threads through `current-goal`."
  [f]
  (let [store (vis/db-create-connection! :memory)]
    (try
      (with-redefs [vis/db-info (constantly store)]
        (f store))
      (finally
        (vis/db-dispose-connection! store)))))

(defdescribe draw-header-goal-subtitle-test
  (it "header is exactly HEADER_ROWS tall when no goal is set"
    (with-temp-db
      (fn [s]
        (let [cid (vis/db-store-conversation! s {:channel :tui})
              db  {:title "Chat" :conversation {:id (str cid)}}]
          (expect (= header/HEADER_ROWS (header/header-rows db)))))))

  (it "switches the subtitle color to the warn-fg palette when paused"
    (with-temp-db
      (fn [s]
        (let [cid (vis/db-store-conversation! s {:channel :tui})
              _   (goal-ext/set-goal!  s cid {:objective "x" :set-by :user})
              _   (goal-ext/pause-goal! s cid)
              db  {:title "Chat" :conversation {:id (str cid)}}
              writes (atom [])]
          (cr/reset!)
          (cr/begin-frame!)
          (header/draw-header! (dummy-text-graphics writes) db 0 80)
          (cr/commit-frame!)
          (let [goal-row-writes (filter #(and (= 2 (:row %)) (str/includes? (str (:text %)) "goal"))
                                  @writes)]
            (expect (some #(= t/footer-warning-fg (:fg %)) goal-row-writes))))))))
