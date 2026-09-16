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
  ;; #228: the recorded receipt stays one bounded row, even with wide Unicode titles.
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
          (keep-indexed #(when (:live-button? %2) (nth (:prewrapped-lines message) %1))
                        (:line-meta message))

          text
          (str/join "\n" rows)]

      (is (= 1 (count rows)))
      (is (not (str/includes? text (str \u0000))))
      (is (every? #(<= (p/display-width (subs % 1)) width) rows))
      (is (not (str/includes? text "Click or")))
      (is (some #(str/includes? (or (:headline-prefix %) "") (str/capitalize (name reason)))
                (:line-meta message)))
      (is (not-any? #(= :live-reopen (:kind %)) (:line-meta message)))))
  (let [message
        (live-card-message 80 {:recorded-only? true})

        text
        (str/join "\n" (:prewrapped-lines message))]

    (is (some #(= " Recorded " (:right-suffix %)) (:line-meta message)))
    (is (not (str/includes? text "Completed")))
    (is (not (str/includes? text (keymap/label-for :toggle-detail-labels))))))

(deftest recorded-live-card-title-is-bold-test
  ;; Settling or reloading a Live View must not drop its title's emphasis.
  (binding [interactions/hit-map (interactions/create-hit-map)]
    (doseq [cols [40 80]
            owned? [false true]
            live? [false true]
            options [{:reason :completed} {:reason :failed} {:reason :interrupted}
                     {:reason :timeout} {:reason :cancelled} {:recorded-only? true}]]

      (let [capture (cap/capture! {:cols cols
                                   :rows 24
                                   :paint! (fn [{:keys [screen]}]
                                             (paint-live-card-review! screen
                                                                      (assoc options
                                                                        :owned? owned?
                                                                        :live? live?)))})
            row (first (filter #(str/includes? (apply str (map :ch %)) "LIVE ")
                               (last (:frames capture))))
            text (apply str (map :ch row))
            title-col (str/index-of text "LIVE ")
            button-col (str/index-of text " Recorded ")]

        (is (nil? (:error capture)))
        (is (some? title-col))
        (is (some? button-col))
        (when (and title-col button-col)
          (is (every? :bold (remove #(str/blank? (:ch %)) (subvec row title-col button-col))))
          (is (not-any? :bold (subvec row button-col (+ button-col 10)))))))))

(deftest live-card-nested-hint-test
  ;; #228: owned recordings keep their target and status without the open instruction.
  (let [artifact
        {:filename "jenkins.live.ndjson" :iteration-id "i" :index 0}

        entries
        (#'render/live-artifact-receipt-entries artifact "s" 80 {:reason :completed} true)]

    (is (= 1 (count entries)))
    (is (= ["LIVE jenkins · Completed"] (mapv #(subs (:line %) 1) entries)))
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

          [[title title-meta]]
          entries]

      (is (= 1 (count entries)))
      (is (str/starts-with? (subs title 1) "LIVE "))
      (is (:run-header? title-meta))
      (is (= " Recorded " (:right-suffix title-meta)))
      (when-not recorded-only?
        (is (str/includes? (:headline-prefix title-meta) (str/capitalize (name reason)))))
      (is (every? #(= "live-review" (get-in (second %) [:artifact :view-id])) entries))
      (is (every? #(= "live-review" (:session-id (second %))) entries)))))

(deftest live-card-pointer-and-clipping-test
  (binding [interactions/hit-map (interactions/create-hit-map)]
    (doseq [cols [24 40 80]
            start-row [-30 -10 1 14]]

      (let [capture (cap/capture! {:cols cols
                                   :rows 24
                                   :paint! (fn [{:keys [screen]}]
                                             (paint-live-card-review! screen
                                                                      {:start-row start-row}))})
            regions (card-regions)
            grid (last (:frames capture))
            visible-labels (filter #(str/includes? (apply str (map :ch %)) " Recorded ") grid)]

        (is (nil? (:error capture)))
        (is (= (count visible-labels) (count regions)))
        (when (= start-row 1) (is (= 1 (count regions))))
        (is (= (count regions)
               (count (filter (comp :live-card? second)
                              (interactions/assign-labels (.current interactions/hit-map))))))
        (doseq [{:keys [bounds] :as region} regions
                :let [{:keys [row col width]} bounds]]

          (is (= 10 width))
          (is (<= 0 row 23))
          (is (= " Recorded " (apply str (map :ch (subvec (nth grid row) col (+ col width))))))
          (doseq [x (range col (+ col width))]
            (is (< x cols))
            (is (= region (.lookup interactions/hit-map (int x) (int row)))))
          (doseq [x (concat (range col) (range (+ col width) cols))]
            (is (not= :artifact (:kind (.lookup interactions/hit-map (int x) (int row)))))))))))

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
  ;; #205: compact buttons, Unicode and jump navigation share the real cell layout.
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
          (is (str/includes? (.renderHtml html) "LIVE"))
          (let [{:keys [row col]} (:bounds (first (card-regions)))
                cell (.getCharacter terminal (TerminalPosition. (int col) (int row)))]

            (if labels?
              (do (is (= theme/warning-fg (.getBackgroundColor cell))) (is (.isBold cell)))
              (is (= theme/button-bg (.getBackgroundColor cell))))))))))

(deftest live-card-hover-test
  (binding [interactions/hit-map (interactions/create-hit-map)]
    (with-open [terminal (DefaultVirtualTerminal. (TerminalSize. 80 24))
                screen (doto (TerminalScreen. terminal) (.startScreen))]

      (paint-live-card-review! screen {})
      (.setHovered interactions/hit-map (first (card-regions)))
      (paint-live-card-review! screen {})
      (doseq [{:keys [bounds]} (card-regions)
              :let [{:keys [row col width]} bounds]
              x (range col (+ col width))]

        (is (= theme/header-active-tab-accent
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
