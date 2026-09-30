(ns com.blockether.vis.tui.session-navigator-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.frame :as frame]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.terminals :as term]
            [com.blockether.vis.tui.theme :as t]
            [lazytest.core :refer [defdescribe describe it expect]])
  (:import [com.googlecode.lanterna SGR TerminalSize]
           [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(def ^:private sessions
  [{"id" "star"
    "title" "Starred session"
    "turn_count" 1
    "modified_at" 1000
    "favorite_rank" 1
    "group_id" "group"
    :work-dir "/first"}
   {"id" "current" "title" "Current session" "turn_count" 1 "modified_at" 2000 :work-dir "/first"}
   {"id" "newest" "title" "Newest session" "turn_count" 1 "modified_at" 4000 :work-dir "/second"}
   {"id" "middle" "title" "Middle session" "turn_count" 1 "modified_at" 3000 :work-dir "/first"}])

(defn- back-line
  [^TerminalScreen screen row]
  (apply str
    (for [column (range (.getColumns (.getTerminalSize screen)))]
      (.getCharacterString (.getBackCharacter screen (int column) (int row))))))

(defn- capture-navigator!
  "Run the real navigator and capture the painted panes before each supplied input."
  [cols rows opts inputs]
  (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
        (term/virtual-screen)

        frames
        (atom [])

        painted
        (atom [])

        preview
        (atom nil)

        inputs
        (atom (vec inputs))

        draw-session
        (var-get #'dlg/draw-navigator-session!)

        draw-preview
        (var-get #'dlg/draw-navigator-preview!)]

    (try (.setTerminalSize terminal (TerminalSize. (int cols) (int rows)))
         (frame/resize! screen)
         (with-redefs-fn
           {#'dlg/draw-navigator-session!
            (fn [g x row width entry selected?]
              (swap! painted conj {:entry entry :selected? selected? :x x :row row :width width})
              (draw-session g x row width entry selected?))
            #'dlg/draw-navigator-preview! (fn [g x row width lines]
                                            (reset! preview
                                              {:x x :row row :width width :lines lines})
                                            (draw-preview g x row width lines))
            #'dlg/read-navigator-key!
            (fn [& _]
              (swap! frames conj {:painted @painted :preview @preview})
              (reset! painted [])
              (let [input (first @inputs)]
                (swap! inputs #(vec (rest %)))
                (if (fn? input) (input terminal) (or input (KeyStroke. KeyType/Escape)))))}
           (fn []
             {:choice (dlg/navigator-dialog! screen opts) :frames @frames}))
         (finally (.stopScreen screen)))))

(defdescribe
  session-navigator-presentation-test
  ;; Regression: C-x s changed its layout with the query and pinned/grouped older rows.
  (describe "one recency list"
            (it "orders across projects, groups, stars and the current session by latest activity"
                (let [rows
                      (#'dlg/navigator-all-rows {:sessions sessions :active-session-id "current"})

                      visible
                      (#'dlg/navigator-visible-rows rows "" {})]

                  (expect (= ["newest" "middle" "current" "star"]
                             (mapv (comp :id :target) visible)))
                  (expect (not-any? :group-start? visible))))
            (it "keeps recency order during search instead of lifting title matches"
                (let [rows
                      (#'dlg/navigator-all-rows {:sessions sessions})

                      matches
                      {"newest" {:rank 2 :kind :reply :reply-snippet "needle"}
                       "star" {:rank 0 :kind :title}}

                      visible
                      (#'dlg/navigator-visible-rows rows "needle" matches)]

                  (expect (= ["newest" "star"] (mapv (comp :id :target) visible))))))
  (describe "compact rows"
            (it "paints a colored bold date, status and title on exactly one line"
                (let [{:keys [^TerminalScreen screen]} (term/virtual-screen)]
                  (try (#'dlg/draw-navigator-session!
                        (.newTextGraphics screen)
                        2
                        4
                        74
                        {:modified "09-30 11:34"
                         :status "idle"
                         :title "Session title"
                         :session "hidden-id"
                         :favorite? true}
                        false)
                       (let [date (.getBackCharacter screen 4 4)]
                         (expect (str/includes? (back-line screen 4)
                                                "09-30 11:34 / idle / Session title"))
                         (expect (str/blank? (back-line screen 5)))
                         (expect (= t/dialog-hint-key (.getForegroundColor date)))
                         (expect (contains? (set (.getModifiers date)) SGR/BOLD))
                         (expect (not (str/includes? (back-line screen 4) "hidden-id")))
                         (expect (not (str/includes? (back-line screen 4) "*"))))
                       (finally (.stopScreen screen)))))
            (it "fits one session per terminal row without group headings or spacer lines"
                (let [rows
                      (#'dlg/navigator-all-rows {:sessions sessions})

                      visible
                      (#'dlg/navigator-visible-rows rows "" {})]

                  (expect (= [1 1 1 1] (#'dlg/navigator-block-heights visible)))
                  (expect (= [0 1 2] (mapv :idx (#'dlg/navigator-visible-blocks visible 0 3)))))))
  (describe
    "opening and resize"
    (it "always keeps the list beside the preview, including a blank query and narrow terminals"
        (doseq [cols [60 80 140]]
          (let [{:keys [frames]} (capture-navigator! cols 32 {:sessions sessions} [])
                {:keys [painted preview]} (first frames)]

            (expect (some? preview))
            (when preview
              (expect (< (+ (:x (first painted)) (:width (first painted))) (:x preview)))
              (expect (= (:row (first painted)) (:row preview)))))))
    (it "opens on the current session in its recency position, not at the top"
        (let [{:keys [choice frames]}
              (capture-navigator! 140
                                  32
                                  {:sessions sessions :active-session-id "current"}
                                  [(KeyStroke. KeyType/Enter)])

              painted
              (:painted (first frames))]

          (expect (= {:action :switch :id "current"} choice))
          (expect (= ["newest" "middle" "current" "star"] (mapv (comp :id :target :entry) painted)))
          (expect (= ["current"] (mapv (comp :id :target :entry) (filter :selected? painted))))))
    (it "scrolls to the current session even when newer sessions fill the viewport"
        (let [rows
              (mapv (fn [n]
                      {"id" (str n) "title" (str "Session " n) "turn_count" 1 "modified_at" n})
                    (range 60))

              {:keys [choice frames]}
              (capture-navigator! 100
                                  30
                                  {:sessions rows :active-session-id "0"}
                                  [(KeyStroke. KeyType/Enter)])

              painted
              (:painted (first frames))]

          (expect (= {:action :switch :id "0"} choice))
          (expect (= ["0"] (mapv (comp :id :target :entry) (filter :selected? painted))))
          (expect (not-any? #(= "59" (:id (:target (:entry %)))) painted)))))
  (describe "gateway paging"
            (it "uses the gateway recents cursor, not starred or grouped windows"
                (let [requests
                      (atom [])

                      page
                      {:sessions (vec (reverse sessions)) :next-cursor "next"}]

                  (with-redefs [vis/gateway-search-sessions
                                (fn [query opts]
                                  (swap! requests conj [query opts])
                                  page)

                                vis/gateway-list-sessions-page
                                (fn [opts]
                                  (swap! requests conj [:navigator opts])
                                  {:sessions sessions :next-cursor "wrong"})]

                    (let [answer (#'screen/tui-session-page {:limit 50 :after "cursor"})]
                      (expect (= ["middle" "newest" "current" "star"]
                                 (mapv #(get % "id") (:sessions answer))))
                      (expect (= "next" (:next-cursor answer)))
                      (expect (= [["" {:limit 50 :after "cursor"}]] @requests))))))))

(defdescribe session-navigator-selection-test
             (it "keeps the current empty session even when recents do not list it"
                 (let [requests (atom [])]
                   (with-redefs [vis/gateway-search-sessions (fn [_ _]
                                                               {:sessions [(last sessions)]
                                                                :next-cursor "next"})
                                 vis/gateway-list-sessions (constantly [])
                                 vis/gateway-soul
                                 (fn [id]
                                   (swap! requests conj id)
                                   {"id" id "title" nil "turn_count" 0 "created_at" 0})]

                     (let [page (#'screen/picker-first-page "current")
                           rows (#'dlg/navigator-all-rows
                                 {:sessions (:sessions page) :active-session-id "current"})]

                       (expect (= ["middle" "current"] (mapv (comp :id :target) rows)))
                       (expect (= 1 (#'dlg/navigator-selected-index rows 0 {:id "current"})))
                       (expect (= ["current"] @requests))
                       (expect (= "next" (:next-cursor page)))))))
             (it "preserves keyboard selection by id when a page changes row positions"
                 (let [rows
                       (#'dlg/navigator-all-rows {:sessions sessions})

                       more
                       (conj sessions {"id" "between" "title" "Between" "modified_at" 2500})

                       next-rows
                       (#'dlg/navigator-all-rows {:sessions more})]

                   (expect (= 3 (#'dlg/navigator-selected-index next-rows 2 {:rows rows})))
                   (expect (= 4 (#'dlg/navigator-selected-index next-rows 3 {:rows rows})))
                   (expect (= 0 (#'dlg/navigator-selected-index [] 3 {:rows rows}))))))

(defdescribe
  session-navigator-interaction-test
  (it "uses modified time, then creation time, for all registered timestamp types"
      (let [rows (#'dlg/navigator-all-rows
                  {:sessions [{"id" "date" "title" "Date" "modified_at" (java.util.Date. 1000)}
                              {"id" "instant"
                               "title" "Instant"
                               "modified_at" (java.time.Instant/ofEpochMilli 2000)}
                              {"id" "number" "title" "Number" "modified_at" 3000}
                              {"id" "created" "title" "Created" "created_at" 4000}
                              {"id" "missing" "title" "Missing"}]})]
        (expect (= ["created" "number" "instant" "date" "missing"]
                   (mapv (comp :id :target) rows)))))
  (it "clips every field and wide title to the list without painting in the preview"
      (doseq [width [8 26 54]]
        (let [{:keys [^TerminalScreen screen]} (term/virtual-screen)]
          (try (let [g (.newTextGraphics screen)]
                 (p/put-str! g 0 4 (apply str (repeat 80 \.)))
                 (#'dlg/draw-navigator-session!
                  g
                  2
                  4
                  width
                  {:modified "09-30 11:34"
                   :status "! input needed ×2"
                   :title (apply str (repeat 40 "界"))}
                  true)
                 (expect (= (apply str (repeat (- 78 width) \.))
                            (subs (back-line screen 4) (+ 2 width)))))
               (finally (.stopScreen screen))))))
  (it "keeps a horizontal split and the chosen row through shrink and grow"
      (let [{:keys [choice frames]} (capture-navigator!
                                      140
                                      32
                                      {:sessions sessions :active-session-id "current"}
                                      [(fn [^DefaultVirtualTerminal terminal]
                                         (.setTerminalSize terminal (TerminalSize. 60 22))
                                         (KeyStroke. KeyType/ArrowDown))
                                       (fn [^DefaultVirtualTerminal terminal]
                                         (.setTerminalSize terminal (TerminalSize. 160 36))
                                         (KeyStroke. KeyType/ArrowUp)) (KeyStroke. KeyType/Enter)])]
        (expect (= {:action :switch :id "current"} choice))
        (expect (= [["current"] ["star"] ["current"]]
                   (mapv (fn [frame]
                           (mapv (comp :id :target :entry) (filter :selected? (:painted frame))))
                         frames)))
        (doseq [{:keys [painted preview]} frames]
          (expect (some? preview))
          (expect (< (+ (:x (first painted)) (:width (first painted))) (:x preview))))))
  (it "restores the current session when a search is cleared"
      (let [{:keys [choice frames]} (capture-navigator!
                                      140
                                      32
                                      {:sessions sessions :active-session-id "current"}
                                      [(term/keystroke \z) (KeyStroke. KeyType/Backspace)
                                       (KeyStroke. KeyType/Enter)])]
        (expect (= {:action :switch :id "current"} choice))
        (expect (empty? (:painted (second frames))))
        (expect (every? :preview frames))))
  (it "keeps keyboard actions on the selected session rather than the newest row"
      (let [{:keys [choice]} (capture-navigator!
                               140
                               32
                               {:sessions sessions :active-session-id "current"}
                               [(KeyStroke. (Character/valueOf \s) true false false)])]
        (expect (= {:action :favorite :id "current" :favorite? true} choice)))))
