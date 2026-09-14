(ns com.blockether.vis.tui.active-live-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.live-view :as lv]
            [com.blockether.vis.tui.live-view-fixture :as fixture]
            [com.blockether.vis.tui.view-materializer :as materializer]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.html-backend-test :as html]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.theme :as theme]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.scroll :as scroll]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.terminal-image :as timg]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(def owner {:invocation-id "11111111-1111-4111-8111-111111111111"})

(defn review-pane
  []
  (lv/opened (-> (fixture/view
                   {:title "Build verification"}
                   (fixture/status "status" "Running the focused tests" {:tone :running})
                   (fixture/log "output"
                                {:label "Output"
                                 :default-expanded true
                                 :lines ["Checking inline Activity rendering"]}))
                 fixture/normalize-live-view
                 materializer/materialize
                 (assoc :id "22222222-2222-4222-8222-222222222222"
                        :seq 0
                        :created-at 0
                        :owner owner))))

(def review-progress
  {:iterations [{:forms [{:code "verify()"
                          :activity {:state "running"
                                     :history owner
                                     :counts {:running 1 :succeeded 0 :failed 0 :cancelled 0}
                                     :rows [{:id (:invocation-id owner)
                                             :operation "Run tests"
                                             :summary "Checking the build"
                                             :state "running"}]
                                     :omitted {:rows 0 :by-classification {}}}}]}]})

(defn review-payload
  [pane width]
  (render/progress->lines-data
    review-progress
    width
    {:show-thinking true :show-iterations true}
    {:session-id "inline-review" :now-ms 1000 :live-runs [(lv/transcript-run pane)]}))

(deftest active-picture-belongs-to-activity
  ;; #222: running live nodes, not only a finished RUN receipt, belong inside Activity.
  (render/invalidate-cache!)
  (let [payload
        (review-payload (review-pane) 90)

        text
        (str/join "\n" (:lines payload))]

    (is (str/includes? text "ACTIVITY"))
    (is (str/includes? text "Running the focused tests"))
    (is (some #(= :activity-live-entry (:kind %)) (:line-meta payload)))))

(defn paint-review!
  "Paint the real transcript inside a clipped terminal viewport for #222 review."
  ([screen pane] (paint-review! screen pane 0))
  ([^TerminalScreen screen pane scroll]
   (.doResizeIfNecessary screen)
   (.clear screen)
   (let [cols
         (.getColumns (.getTerminalSize screen))

         height
         (.getRows (.getTerminalSize screen))

         payload
         (review-payload pane (- cols 4))

         graphics
         (.newTextGraphics screen)

         clip
         (.newTextGraphics graphics (TerminalPosition. 0 3) (TerminalSize. cols (- height 6)))]

     (render/fill-background! graphics cols height)
     (.beginFrame interactions/hit-map)
     (render/draw-chat-bubble! clip
                               {:role :assistant
                                :text ""
                                :prewrapped-lines (:lines payload)
                                :line-meta (:line-meta payload)}
                               (- 1 (long scroll))
                               1
                               (- cols 4)
                               {:viewport-top 3 :viewport-h (- height 6)})
     (.commitFrame interactions/hit-map)
     (.refresh screen)
     payload)))

(defn- grid
  [^DefaultVirtualTerminal terminal cols rows]
  (mapv (fn [row]
          (mapv #(.getCharacter terminal (TerminalPosition. (int %) (int row))) (range cols)))
        (range rows)))

(deftest running-grid-and-clipped-clicks
  ;; #222: actual node pixels and hit rows share the Activity inset and viewport origin.
  (doseq [cols
          [40 80 120]

          scroll
          [0 5]]

    (with-open [ht
                (html/activity-review-terminal cols 44)

                vt
                (DefaultVirtualTerminal. (TerminalSize. cols 44))

                hs
                (doto (TerminalScreen. ht) (.startScreen))

                vs
                (doto (TerminalScreen. vt) (.startScreen))]

      (binding [interactions/hit-map (interactions/create-hit-map)]
        (let [pane (review-pane)]
          (paint-review! hs pane scroll)
          (paint-review! vs pane scroll)
          (is (= (grid ht cols 44) (grid vt cols 44)))
          (let [regions (.current interactions/hit-map)
                disclosure (first (filter #(= :live-expand (:kind %)) regions))
                {:keys [row col]} (:bounds disclosure)
                lines (mapv #(apply str
                               (map (fn [cell]
                                      (.getCharacterString ^com.googlecode.lanterna.TextCharacter
                                                           cell))
                                    %))
                            (grid vt cols 44))]

            (is (some? disclosure))
            (is (= (:view-id disclosure) (lv/view-id pane)))
            (is (= "▾ Output · 1 lines" (str/trim (nth lines row))))
            (let [header-row (first (keep-indexed #(when (str/includes? %2 "ACTIVITY") %1) lines))
                  background-cols
                  (fn [r]
                    (into #{}
                          (keep-indexed (fn [col cell]
                                          (when (= theme/code-block-bg
                                                   (.getBackgroundColor
                                                     ^com.googlecode.lanterna.TextCharacter cell))
                                            col)))
                          (nth (grid vt cols 44) r)))]

              (when header-row
                (is (= [(apply min (background-cols header-row))
                        (apply max (background-cols header-row))]
                       [(apply min (background-cols row)) (apply max (background-cols row))]))))
            (is (= :live-expand (:kind (.lookup interactions/hit-map (int col) (int row)))))
            (is (= theme/code-block-bg
                   (.getBackgroundColor (.getCharacter vt
                                                       (TerminalPosition. (int col) (int row))))))
            (is (every? #(<= 3 (long (get-in % [:bounds :row])) 40)
                        (filter #(contains? #{:live-inline :live-expand :live-minimize} (:kind %))
                                regions)))
            (is (= pane
                   (#'screen/live-band-pane
                    {:live-views [pane]
                     :layout {:cols cols :rows 44 :inline-live-ids #{(lv/view-id pane)}}}
                    row)))))))))

(deftest live-cache-disclosure-and-stop
  ;; #222: each pane transition replaces the existing picture without a duplicate RUN row.
  (let [pane
        (review-pane)

        before
        (review-payload pane 90)

        changed
        (assoc-in pane [:view :nodes 0 :text] "Focused tests passed")

        after
        (review-payload changed 90)

        compact
        (review-payload (lv/minimized changed) 90)

        armed
        (lv/armed changed)]

    (is (str/includes? (str/join "\n" (:lines before)) "Running the focused tests"))
    (is (str/includes? (str/join "\n" (:lines after)) "Focused tests passed"))
    (is (not-any? #(= :live-reopen (:kind %)) (:line-meta after)))
    (is (< (count (:lines compact)) (count (:lines after))))
    (is (some #(= :inline-stop (get-in % [:live-entry :kind]))
              (:line-meta (review-payload armed 90))))
    (is (= :stop (:action (lv/typed armed {:kind :enter}))))))

(deftest ownerless-and-late-owner-placement
  ;; #222: no guessed form index; an owner arriving later invalidates placement.
  (let [pane
        (review-pane)

        project
        (fn [progress pane]
          (render/progress->lines-data
            progress
            90
            {:show-thinking true :show-iterations true}
            {:session-id "owner-test" :now-ms 0 :live-runs [(lv/transcript-run pane)]}))

        missing
        (project review-progress (update pane :view dissoc :owner))

        late
        (project (assoc-in review-progress [:iterations 0 :forms 0 :activity :rows] []) pane)

        found
        (project review-progress pane)

        live?
        #(some (fn [meta]
                 (= :activity-live-entry (:kind meta)))
               (:line-meta %))]

    (is (not (live? missing)))
    (is (not (live? late)))
    (is (live? found))
    (is (seq (:unplaced (#'render/place-run-rows
                         [{:forms [{}]}]
                         [{:anchor {:iteration-index 0 :form-index 0}}]))))))

(deftest empty-history-keeps-owned-live-and-settled-surface
  ;; #222: history ownership remains useful when the bounded row window is empty.
  (let [activity-id
        "44444444-4444-4444-8444-444444444444"

        pane
        (assoc-in (review-pane) [:view :owner] {:activity-id activity-id})

        progress
        (assoc-in review-progress
          [:iterations 0 :forms 0 :activity]
          {:state "running" :history {:id activity-id} :rows []})

        options
        {:session-id "history-test" :now-ms 0}

        live
        (render/progress->lines-data progress
                                     90
                                     {:show-iterations true}
                                     (assoc options :live-runs [(lv/transcript-run pane)]))

        settled
        (render/progress->lines-data progress
                                     90
                                     {:show-iterations true}
                                     (assoc options :runs [(lv/run-row pane)]))]

    (is (some #(= :activity-live-entry (:kind %)) (:line-meta live)))
    (is (str/includes? (str/join "\n" (:lines settled)) "ACTIVITY"))
    (is (some #(= :live-reopen (:kind %)) (:line-meta settled)))))

(deftest full-frame-owns-inline-geometry-and-fallback
  ;; #222: the production frame, not only the node painter, removes the duplicate band.
  (doseq [matched?
          [true false]

          collapsed?
          [true false]

          offscreen?
          [false true]]

    (with-open [terminal
                (DefaultVirtualTerminal. (TerminalSize. 100 44))

                ts
                (doto (TerminalScreen. terminal) (.startScreen))]

      (let [pane
            (cond-> (review-pane)
              (not matched?)
              (update :view dissoc :owner))

            db
            {:session {:id "inline-frame"}
             :messages (into (if offscreen?
                               (mapv (fn [i]
                                       {:id (str i)
                                        :role :user
                                        :text (str/join "\n" (repeat 10 "Earlier conversation"))})
                                     (range 30))
                               [])
                             [{:role :assistant :text ""}])
             :input (input/empty-input)
             :scroll (if offscreen? (scroll/parked 0) scroll/follow)
             :loading? true
             :progress review-progress
             :settings {:show-iterations true}
             :detail-expansions (if collapsed? {:vis.channel-tui/baseline :collapse} {})
             :live-views [pane]
             :pending-sends []
             :channel-status {}
             :tabs []
             :tab-locals {}
             :slash-command-index 0
             :render-version 0}

            events
            (atom [])

            bands
            (atom [])

            painter
            lv/paint!]

        (binding [interactions/hit-map (interactions/create-hit-map)]
          (with-redefs [state/dispatch #(swap! events conj %)
                        timg/images-protocol (constantly nil)
                        client/get-router (constantly nil)
                        lv/paint! (fn [g cols rows panes top prompt now]
                                    (swap! bands conj (mapv lv/view-id panes))
                                    (painter g cols rows panes top prompt now))]

            (let [layout (#'screen/render-frame! ts 100 44 db 1000)
                  id (lv/view-id pane)
                  measured (some #(when (= [:live-view-painted id] (subvec % 0 2)) (nth % 2))
                                 @events)]

              (is (= matched? (contains? (:inline-live-ids layout) id)))
              (is (= (if matched? [] [id]) (last @bands)))
              (when-not (and matched? offscreen?)
                (is (pos? (:visible measured)))
                (is (pos? (:total measured))))
              (is (= (and matched? (not offscreen?))
                     (boolean (some #(= :live-inline (:kind %))
                                    (.current interactions/hit-map))))))))))))

(deftest selectable-rows-use-the-existing-keyboard-and-action-route
  ;; #222: inline pointer and F3 controls name the same authoritative row action.
  (with-open [terminal
              (DefaultVirtualTerminal. (TerminalSize. 100 44))

              ts
              (doto (TerminalScreen. terminal) (.startScreen))]

    (let [pane
          (assoc-in (review-pane)
            [:view :nodes]
            [(fixture/table "jobs"
                            [(fixture/table-column "job" "Job")]
                            {:is-selectable true
                             :rows [(fixture/table-row "focused" ["Focused tests"])]})])

          called
          (promise)]

      (binding [interactions/hit-map (interactions/create-hit-map)]
        (paint-review! ts pane)
        (let [hit (first (filter #(= :live-select (:kind %)) (.current interactions/hit-map)))
              keyboard (first (filter #(= :live-select (:kind %)) (lv/controls [pane])))]

          (is (= (select-keys hit [:view-id :node-id :item-id])
                 (select-keys keyboard [:view-id :node-id :item-id])))
          (with-redefs [client/gateway-view-action! (fn [session-id view-id action]
                                                      (deliver called [session-id view-id action]))]
            (#'screen/select-live-row! {:live-views [pane]} hit)
            (is (= [nil (lv/view-id pane) {:action :select :node-id "jobs" :item-ids ["focused"]}]
                   (deref called 2000 ::timeout)))))))))

(deftest inline-scroll-and-minimize-preserve-the-reading-position
  ;; #222: wheel geometry is bounded, follows patches, and survives minimizing the picture.
  (let [pane
        (assoc-in (review-pane) [:view :nodes 1 :lines] (mapv #(str "Build line " %) (range 50)))

        measure
        #(get-in (first (lv/inline-entries % 80)) [:meta :live-geometry])

        painted
        (lv/painted pane (measure pane))

        scrolled
        (lv/scrolled painted -4)

        geometry
        (measure scrolled)

        scrolled
        (lv/painted scrolled geometry)

        minimized
        (lv/minimized scrolled)

        restored
        (lv/restored (lv/painted minimized (measure minimized)))]

    (is (= 12 (:visible geometry)))
    (is (pos? (:offset geometry)))
    (is (not (:is-following scrolled)))
    (is (= (:offset scrolled) (:offset restored)))
    (is (= (:anchor scrolled) (:anchor restored)))
    (is (= (:visible scrolled) (:visible restored)))))

(deftest live-panes-do-not-take-the-static-scroll-fast-path
  ;; #222: the static scroll path does not project the running progress bubble.
  (let [db
        {:live-views [(review-pane)]}

        flags
        (#'screen/frame-change-flags
         {:last-db db :db db :last-layout {} :cols 100 :same-size? true})]

    (is (every? false? (vals flags)))))
