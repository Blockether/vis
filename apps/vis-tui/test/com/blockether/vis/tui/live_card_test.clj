(ns com.blockether.vis.tui.live-card-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.keymap :as keymap]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.theme :as theme]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.html HtmlTerminal]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(defn live-card-message
  "Deterministic production transcript fixture for #205 and #228, including the adjacent run."
  [width
   {:keys [reason title recorded-only? live? owned?]
    :or {reason :completed title "Jenkins · build pool"}}]
  (let [owner
        {:activity-id "activity-review"}

        run
        (cond-> {:view-id "live-review" :title title :reason reason :lines 13 :elapsed-ms 2200}
          owned?
          (assoc :owner owner))

        iteration
        {:iteration-id "iteration-review"
         :attachments [(cond-> {"source" "tool"
                                "kind" "file"
                                "filename" "jenkins-build-pool.live.ndjson"
                                "media_type" "application/vnd.vis.live+ndjson"
                                "view_id" "live-review"
                                "size" 2048}
                         owned?
                         (assoc "owner" owner))]
         :forms
         [(cond-> {:code "await jenkins.watch()" :success? true :runs (if recorded-only? [] [run])}
            owned?
            (assoc :activity
              {:state "succeeded"
               :history {:id "activity-review"}
               :rows [{:id "watch" :operation "Watch builds" :state "succeeded"}]}))]}

        settings
        {:show-iterations true :show-thinking false}

        options
        {:session-id "live-review" :session-turn-id "turn-review"}

        payload
        (if live?
          (render/progress->lines-data {:iterations [iteration]}
                                       width
                                       settings
                                       (assoc options
                                         :now-ms 3000
                                         :turn-start-ms 1000))
          (render/format-answer-with-thinking-data* nil
                                                    [iteration]
                                                    width
                                                    settings
                                                    nil
                                                    false
                                                    options))]

    {:role :assistant :prewrapped-lines (:lines payload) :line-meta (:line-meta payload)}))

(defn paint-live-card-review!
  "Paint the same transcript on HtmlTerminal and DefaultVirtualTerminal; no copied layout."
  [^TerminalScreen screen {:keys [labels? start-row] :or {start-row 1} :as options}]
  (.doResizeIfNecessary screen)
  (.clear screen)
  (let [cols
        (.getColumns (.getTerminalSize screen))

        rows
        (.getRows (.getTerminalSize screen))

        g
        (.newTextGraphics screen)

        width
        (- cols render/MESSAGE_SIDE_PAD)

        message
        (live-card-message width options)]

    (p/set-colors! g theme/text-fg theme/terminal-bg)
    (.fill g \space)
    (.beginFrame interactions/hit-map)
    (let [consumed (render/draw-chat-bubble! g
                                             message
                                             start-row
                                             render/MESSAGE_MARGIN_LEFT
                                             width
                                             {:viewport-h rows})]
      (.commitFrame interactions/hit-map)
      (when labels? (render/draw-detail-labels! g true nil))
      (.refresh screen)
      {:message message :review-rows (min rows (+ (long start-row) (long consumed) 1))})))

(defn- card-regions [] (filterv :live-card? (.current interactions/hit-map)))

(deftest live-card-status-and-width-test
  ;; #228: the live card uses the full message column, not the inset trace width.
  (doseq [width
          [6 12 24 40 80]

          reason
          [:completed :failed :interrupted :timeout :cancelled]

          live?
          [false true]]

    (let [message
          (live-card-message width
                             {:reason reason :title "界界界 Jenkins · build pool 👩‍💻" :live? live?})

          rows
          (keep-indexed #(when (:live-card-row %2) (nth (:prewrapped-lines message) %1))
                        (:line-meta message))

          text
          (str/join "\n" rows)]

      (is (= 7 (count rows)))
      (is (not (str/includes? text (str \u0000))))
      (is (every? #(= width (p/display-width (subs % 1))) rows))
      (is (not (str/includes? text "Click or")))
      (is (not-any? #(= :hint (:live-card-row %)) (:line-meta message)))
      (when (>= width 24) (is (str/includes? text (str/capitalize (name reason)))))
      (is (not-any? #(= :live-reopen (:kind %)) (:line-meta message)))))
  (let [message
        (live-card-message 80 {:recorded-only? true})

        text
        (str/join "\n" (:prewrapped-lines message))]

    (is (str/includes? text "Recorded"))
    (is (not (str/includes? text "Completed")))
    (is (not (str/includes? text (keymap/label-for :toggle-detail-labels))))))

(deftest live-card-nested-hint-test
  ;; #228: owned recordings keep their target and status without the open instruction.
  (let [artifact
        {:filename "jenkins.live.ndjson" :iteration-id "i" :index 0}

        entries
        (#'render/live-artifact-card-entries artifact "s" 80 {:reason :completed} true)]

    (is (= 2 (count entries)))
    (is (= ["LIVE jenkins" "Completed"] (mapv #(subs (:line %) 1) entries)))
    (is (every? #(= artifact (get-in % [:meta :artifact])) entries))
    (is (every? #(get-in % [:meta :activity-live?]) entries))))

(deftest owned-live-recordings-keep-identity
  ;; #235: persisted recordings must retain the Live View label beside Activity, not become RUN.
  (doseq [width
          [24 40 80]

          reason
          [:completed :failed :interrupted :timeout :cancelled]

          recorded-only?
          [false true]

          live?
          [false true]]

    (let [message
          (live-card-message
            width
            {:owned? true :reason reason :recorded-only? recorded-only? :live? live?})

          entries
          (keep-indexed #(when (:activity-live? %2) [(nth (:prewrapped-lines message) %1) %2])
                        (:line-meta message))

          [[title title-meta] [status]]
          entries]

      (is (= 2 (count entries)))
      (is (str/starts-with? (subs title 1) "LIVE "))
      (is (:run-header? title-meta))
      (is (str/includes? status (if recorded-only? "Recorded" (str/capitalize (name reason)))))
      (is (every? #(= "live-review" (get-in (second %) [:artifact :view-id])) entries))
      (is (every? #(= "live-review" (:session-id (second %))) entries)))))

(deftest live-card-pointer-and-clipping-test
  (binding [interactions/hit-map (interactions/create-hit-map)]
    (doseq [cols [24 40 80]
            start-row [1 -10 14]]

      (let [capture (cap/capture! {:cols cols
                                   :rows 24
                                   :paint! (fn [{:keys [screen]}]
                                             (paint-live-card-review! screen
                                                                      {:start-row start-row}))})
            regions (card-regions)]

        (is (nil? (:error capture)))
        (is (seq regions))
        (is (every? #(= render/MESSAGE_MARGIN_LEFT (get-in % [:bounds :col])) regions))
        (is (every? #(= (- cols render/MESSAGE_SIDE_PAD) (get-in % [:bounds :width])) regions))
        (is (= 1
               (count (filter (comp :live-card? second)
                              (interactions/assign-labels (.current interactions/hit-map))))))
        (when (= start-row 1)
          (let [grid (last (:frames capture))
                top (:bounds (first regions))
                bottom (:bounds (last regions))
                left (long (:col top))
                right (+ left (long (:width top)) -1)]

            (is (= "┌" (get-in grid [(:row top) left :ch])))
            (is (= "┐" (get-in grid [(:row top) right :ch])))
            (is (= "└" (get-in grid [(:row bottom) left :ch])))
            (is (= "┘" (get-in grid [(:row bottom) right :ch])))
            (is (every? #(= "─" (get-in grid [(:row bottom) % :ch])) (range (inc left) right)))))
        (doseq [{:keys [bounds] :as region} regions
                :let [{:keys [row col width]} bounds]
                x (range col (+ col width))]

          (is (<= 0 row 23))
          (is (< x cols))
          (is (= region (.lookup interactions/hit-map (int x) (int row)))))
        (let [{:keys [row col width]} (:bounds (first regions))]
          (is (nil? (.lookup interactions/hit-map (int (dec col)) (int row))))
          (is (nil? (.lookup interactions/hit-map (int (+ col width)) (int row)))))))))

(deftest live-card-keyboard-test
  (binding [interactions/hit-map (interactions/create-hit-map)]
    (cap/capture! {:cols 80
                   :rows 24
                   :paint! (fn [{:keys [screen]}]
                             (paint-live-card-review! screen {}))})
    (let [labels (interactions/assign-labels (.current interactions/hit-map))
          [label card] (first (filter (comp :live-card? second) labels))
          opened (atom [])
          events (atom [])]

      (with-redefs [screen/open-produced-artifact! (fn [& args]
                                                     (swap! opened conj args))
                    state/dispatch #(swap! events conj %)]

        ;; A later repaint cannot reassign the frozen label to another card.
        (.beginFrame interactions/hit-map)
        (.register interactions/hit-map (assoc-in card [:artifact :index] 1))
        (.register interactions/hit-map (assoc-in card [:bounds :row] 20))
        (.commitFrame interactions/hit-map)
        (#'screen/activate-detail-label! nil {:detail-labels labels} (cap/key-stroke (first label)))
        (is (= [["live-review" (:artifact card)]] @opened))
        (is (= [[:set-detail-labels false] [:bump-render-version]] @events))
        (reset! opened [])
        (doseq [key [(KeyStroke. KeyType/Escape) (KeyStroke. \g true false)]]
          (#'screen/activate-detail-label! nil {:detail-labels labels} key))
        (is (empty? @opened))))))

(defn- cell-grid
  [^DefaultVirtualTerminal terminal cols rows]
  (mapv (fn [row]
          (mapv #(.getCharacter terminal (TerminalPosition. (int %) (int row))) (range cols)))
        (range rows)))

(deftest live-card-html-and-terminal-parity-test
  ;; #205: borders, padding, Unicode and keyboard selection must share the real cell layout.
  (binding [interactions/hit-map (interactions/create-hit-map)]
    (doseq [cols [40 80]
            labels? [false true]]

      (with-open [html (-> (HtmlTerminal/builder)
                           (.initialSize (TerminalSize. cols 24))
                           (.defaultForeground theme/text-fg)
                           (.defaultBackground theme/terminal-bg)
                           (.build))
                  terminal (DefaultVirtualTerminal. (TerminalSize. cols 24))
                  hs (doto (TerminalScreen. html) (.startScreen))
                  ts (doto (TerminalScreen. terminal) (.startScreen))]

        (let [options {:labels? labels? :title "界界 Jenkins · build pool"}]
          (paint-live-card-review! hs options)
          (paint-live-card-review! ts options)
          (is (= (cell-grid html cols 24) (cell-grid terminal cols 24)))
          (is (str/includes? (.renderHtml html) "Live view"))
          (let [{:keys [row col]} (:bounds (first (card-regions)))
                cell (.getCharacter terminal (TerminalPosition. (int col) (int row)))]

            (if labels?
              (do (is (= theme/warning-fg (.getBackgroundColor cell))) (is (.isBold cell)))
              (is (= "┌" (.getCharacterString cell))))))))))

(deftest live-card-hover-test
  (binding [interactions/hit-map (interactions/create-hit-map)]
    (with-open [terminal (DefaultVirtualTerminal. (TerminalSize. 80 24))
                screen (doto (TerminalScreen. terminal) (.startScreen))]

      (paint-live-card-review! screen {})
      (.setHovered interactions/hit-map (nth (card-regions) 4))
      (paint-live-card-review! screen {})
      (doseq [{:keys [bounds]} (card-regions)
              :let [{:keys [row col width]} bounds]
              x (range col (+ col width))]

        (is (= theme/link-chrome-hover-bg
               (.getBackgroundColor (.getCharacter terminal
                                                   (TerminalPosition. (int x) (int row))))))))
    (.setHovered interactions/hit-map nil)))

(deftest live-card-label-identity-test
  ;; #205: all painted rows form one target; distinct artifacts stay distinct.
  (let [card
        {:kind :artifact :live-card? true :session-id "s" :artifact {:iteration-id "i" :index 0}}

        second-card
        (assoc-in card [:artifact :index] 1)

        labels
        (interactions/assign-labels [card card (assoc card :live-card? false) second-card
                                     (assoc card :session-id "other")])]

    (is (= 3 (count labels)))
    (is (= ["a" "s" "d"] (mapv first labels)))))
