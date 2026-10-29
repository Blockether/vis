(ns com.blockether.vis.tui.sidebar-selection-contrast-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.projects :as projects]
            [com.blockether.vis.tui.projects-test :as projects-test]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.shared-theme :as shared-theme]
            [com.blockether.vis.tui.theme :as theme]
            [com.blockether.vis.tui.theme-test :as theme-test]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import [com.googlecode.lanterna TextColor$RGB]
           [com.googlecode.lanterna.screen TerminalScreen]))

(defn- color [[r g b]] (TextColor$RGB. r g b))

(defn- contrast [a b] (#'theme-test/contrast-ratio (color a) (color b)))

(defn- dark-theme-ids
  []
  (filter #(= :dark (:mode (shared-theme/theme %))) (shared-theme/available-theme-ids)))

(defn- sidebar-db
  []
  (->
    (projects-test/fixture-db)
    (assoc-in [:project-sidebar :expanded] #{"a"})
    (assoc-in [:project-sidebar :groups "a"] [projects-test/group-release])
    (assoc-in
      [:project-sidebar :pages "a"]
      {:sessions
       [{"id" "live-session" "title" "Writing tests" "live" true "turn_count" 12 "favorite_rank" 1}
        {"id" "idle-session" "title" "Finished review" "turn_count" 7}]
       :grouped []})))

(defn- capture-sidebar
  [db cols]
  (cap/capture! {:cols cols
                 :rows 36
                 :paint! (fn [{:keys [^TerminalScreen screen]}]
                           (projects/paint! (.newTextGraphics screen) db cols 36))}))

(defn- check-highlight
  [capture entry theme-id]
  (let [hit
        (first (filter #(= (:index entry) (:index %)) (.current projects/hit-map)))

        {:keys [row col width height]}
        (:bounds hit)

        frame
        (first (:frames capture))

        cells
        (mapcat #(subvec (nth frame %) col (+ col width)) (range row (+ row height)))

        background
        (get-in frame [row col :bg])

        surface
        (#'theme-test/rgb-tuple theme/terminal-bg)]

    (expect (nil? (:error capture)))
    (expect (some? hit))
    ;; Text can be readable while a 14% row tint remains nearly invisible.
    (expect (>= (contrast background surface) 3.0) (str theme-id " selection vs sidebar"))
    (doseq [{:keys [ch fg bg]}
            cells

            :when (not (str/blank? ch))]

      (expect (>= (contrast fg bg) 4.5) (str theme-id " selected text: " ch)))))

(defdescribe
  dark-sidebar-selection-contrast-test
  (describe
    "Dark sidebar highlights are distinct and readable"
    (it "highlights focused projects, groups, sections, and complete session rows"
        (let [before
              @theme/active-theme-id

              db
              (sidebar-db)

              entries
              (filter #(#{:project-select :project-group :project-set :project-session} (:kind %))
                      (projects/sidebar-entries db))]

          (try (doseq [theme-id (dark-theme-ids)]
                 (theme/apply-theme! theme-id)
                 (doseq [cols [96 168]
                         entry entries]

                   (check-highlight
                     (capture-sidebar (assoc-in db [:project-sidebar :index] (:index entry)) cols)
                     entry
                     theme-id)))
               (finally (theme/apply-theme! before)))))
    (it "keeps the current session visible after focus returns to chat"
        (let [before
              @theme/active-theme-id

              db
              (-> (sidebar-db)
                  (assoc :session {:id "live-session"})
                  (assoc-in [:project-sidebar :focused?] false))

              entry
              (first (filter #(= "live-session" (get-in % [:session "id"]))
                             (projects/sidebar-entries db)))]

          (try (doseq [theme-id (dark-theme-ids)]
                 (theme/apply-theme! theme-id)
                 (check-highlight (capture-sidebar db 168) entry theme-id))
               (finally (theme/apply-theme! before)))))
    (it
      "keeps attention chips readable on a focused row"
      (let [before
            @theme/active-theme-id

            db
            (projects-test/news-fixture-db)

            entries
            (filter #(#{:project-input :project-unread} (:kind %)) (projects/sidebar-entries db))]

        (expect (seq entries))
        (try (doseq [theme-id (dark-theme-ids)]
               (theme/apply-theme! theme-id)
               (doseq [entry entries]
                 (check-highlight
                   (capture-sidebar (assoc-in db [:project-sidebar :index] (:index entry)) 168)
                   entry
                   theme-id)))
             (finally (theme/apply-theme! before)))))
    (it "keeps multi-selected sessions visible when sidebar focus moves away"
        (let [before
              @theme/active-theme-id

              db
              (-> (sidebar-db)
                  (assoc-in [:project-sidebar :focused?] false)
                  (assoc-in [:project-sidebar :selected "a"] #{"live-session" "idle-session"}))

              entries
              (filter #(= :project-session (:kind %)) (projects/sidebar-entries db))]

          (try (doseq [theme-id (dark-theme-ids)]
                 (theme/apply-theme! theme-id)
                 (doseq [entry entries]
                   (check-highlight (capture-sidebar db 168) entry theme-id)))
               (finally (theme/apply-theme! before)))))))

(defdescribe
  dark-menu-selection-contrast-test
  (it "keeps ordinary list selection readable on dark dialogs and transient surfaces"
      (let [before @theme/active-theme-id]
        (try (doseq [theme-id (dark-theme-ids)]
               (theme/apply-theme! theme-id)
               (doseq [surface [theme/dialog-bg theme/terminal-bg]]
                 (let [capture (binding [theme/dialog-bg surface]
                                 (cap/capture!
                                   {:cols 40
                                    :rows 8
                                    :paint! (fn [{:keys [g]}]
                                              (dlg/draw-selectable-row! g 1 2 30 true "Option")
                                              (dlg/draw-selectable-row! g 1 3 30 false "Option"))}))
                       selected (get-in capture [:frames 0 2 2])
                       unselected (get-in capture [:frames 0 3 2])]

                   (expect (nil? (:error capture)))
                   (expect (:bold selected))
                   (expect (>= (contrast (:bg selected) (:bg unselected)) 3.0))
                   (expect (>= (contrast (:fg selected) (:bg selected)) 4.5)))))
             (finally (theme/apply-theme! before))))))

(defn- capture-queue-focus
  "Paint a focused Queued block whose row `index` is selected, as the chat paints it."
  [index]
  (let [entries (#'render/queued-progress-entries
                 [{:text "first" :turn-id "t1"} {:text "second" :turn-id "t2"}]
                 60
                 nil
                 {:running? true :session-id "s1" :focus {:index index}})]
    (cap/capture! {:cols 64
                   :rows 12
                   :paint! (fn [{:keys [g]}]
                             (render/draw-chat-bubble! g
                                                       {:role :assistant
                                                        :text ""
                                                        :prewrapped-lines (mapv :line entries)
                                                        :line-meta (mapv :meta entries)}
                                                       0 0
                                                       60 {:viewport-h 12}))})))

(defdescribe
  queue-focus-selection-contrast-test
  ;; User report: in vis-light the C-x e selection filled the row with the accent, so the
  ;; accent rail and ordinal vanished and the hint-colored preview was hard to read.
  (it "keeps the selected queue row distinct and its text readable in every theme"
      (let [before @theme/active-theme-id]
        (try (doseq [theme-id (shared-theme/available-theme-ids)]
               (theme/apply-theme! theme-id)
               (let [capture (capture-queue-focus 1)
                     frame (first (:frames capture))
                     row-text (fn [cells]
                                (apply str (map :ch cells)))
                     selected (first (filter #(str/includes? (row-text %) "┃") frame))
                     other (first (filter #(str/includes? (row-text %) "1. first") frame))
                     text-cells (remove #(str/blank? (:ch %)) selected)]

                 (expect (nil? (:error capture)))
                 (expect (str/includes? (row-text selected) "2. second") (str theme-id))
                 (expect (>= (contrast (:bg (first selected)) (:bg (first other))) 3.0)
                         (str theme-id " selected row vs other row"))
                 (expect (= #{"┃" "2" "." "s" "e" "c" "o" "n" "d"} (set (map :ch text-cells)))
                         (str theme-id))
                 (doseq [{:keys [ch fg bg]} text-cells]
                   (expect (>= (contrast fg bg) 4.5) (str theme-id " selected queue text: " ch)))))
             (finally (theme/apply-theme! before))))))
