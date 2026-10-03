(ns com.blockether.vis.tui.active-live-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.live-view :as lv]
            [com.blockether.vis.tui.live-view-fixture :as fixture]
            [com.blockether.vis.tui.view-materializer :as materializer]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.html-backend-test :as html]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.dialogs :as dialogs]
            [com.blockether.vis.tui.theme :as theme]
            [com.blockether.vis.tui.shared-theme :as shared-theme]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.scroll :as scroll]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.terminal-image :as timg]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize TextCharacter]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(def owner {:invocation-id "11111111-1111-4111-8111-111111111111"})

(defn review-pane
  []
  (lv/opened (-> (fixture/view
                   {:title "Build verification" :description "Checking the build before continuing"}
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
  ([pane width] (review-payload pane width {}))
  ([pane width options]
   (render/progress->lines-data
     review-progress
     width
     {:show-thinking true :show-iterations true}
     (-> (merge {:session-id "inline-review"
                 :now-ms 1000
                 :live-runs (when-not (lv/dormant? pane) [(lv/transcript-run pane)])
                 :runs (when (lv/dormant? pane) [(lv/run-row pane)])}
                options)
         ;; Activity lives inside the step digest, so the review opens it.
         (update :detail-expansions assoc ["inline-review" "iteration:i1:digest"] true)))))

(defdescribe
  live-run-is-an-activity-sibling
  (it "live run is an activity sibling"
      ;; #222: LIVE is a sibling section, not a child of the Activity operation.
      (doseq [width
              [40 80 120]

              collapsed?
              [false true]]

        (render/invalidate-cache!)
        (let [payload
              (render/progress->lines-data
                review-progress
                width
                {:show-iterations true}
                {:session-id "inline-review"
                 :now-ms 1000
                 :detail-expansions (assoc (if collapsed? {:vis.channel-tui/baseline :collapse} {})
                                      ["inline-review" "iteration:i1:digest"] true)
                 :live-runs [(lv/transcript-run (review-pane))]})

              lines
              (:lines payload)

              activity-index
              (first (keep-indexed #(when (= :activity-header (:kind %2)) %1) (:line-meta payload)))

              run-index
              (first (keep-indexed #(when (= :inline-title (get-in %2 [:live-entry :kind])) %1)
                                   (:line-meta payload)))

              run-line
              (nth lines run-index)]

          (expect (str/includes? run-line "LIVE Build verification"))
          (expect (zero? (get-in payload [:line-meta activity-index :operation-col])))
          (expect (< activity-index (dec run-index)))
          (expect (str/blank? (subs (nth lines (dec run-index)) 1)))
          (expect (not-any? #(str/includes? % "Running the focused tests") lines))
          (expect (not-any? #(str/includes? % "Checking the build before continuing") lines))
          (expect (= [:inline-title]
                     (keep #(get-in % [:live-entry :kind]) (:line-meta payload))))))))

(defdescribe
  closed-digest-live-button
  (it "folds a live view into one button on the closed digest row"
      ;; A closed digest hides its LIVE rows. The button on its row opens the live view.
      (doseq [[pane label] [[(review-pane) "live"]
                            [(lv/settled (review-pane) {:reason :completed} 2000) "live"]]]
        (render/invalidate-cache!)
        (let [payload (render/progress->lines-data
                        review-progress
                        76
                        {:show-thinking true :show-iterations true}
                        {:session-id "inline-review"
                         :now-ms 1000
                         :live-runs (when-not (lv/dormant? pane) [(lv/transcript-run pane)])
                         :runs (when (lv/dormant? pane) [(lv/run-row pane)])})
              lines (:lines payload)
              row (first (keep-indexed #(when (:digest-live %2) %1) (:line-meta payload)))]

          (expect (= {:view-id (lv/view-id pane) :label label}
                     (select-keys (get-in payload [:line-meta row :digest-live])
                                  [:view-id :label])))
          (expect (str/includes? (nth lines row) (str " · " label)))
          (expect (not-any? #(str/includes? % "Build verification") lines))
          (expect (not-any? #(= :activity-header (:kind %)) (:line-meta payload)))
          (with-open [terminal (DefaultVirtualTerminal. (TerminalSize. 80 12))
                      ts (doto (TerminalScreen. terminal) (.startScreen))]

            (binding [interactions/hit-map (interactions/create-hit-map)]
              (.beginFrame interactions/hit-map)
              (render/draw-chat-bubble!
                (.newTextGraphics ts)
                {:role :assistant :text "" :prewrapped-lines lines :line-meta (:line-meta payload)}
                0 0
                76 {:viewport-top 0 :viewport-h 12})
              (.commitFrame interactions/hit-map)
              (let [regions (.current interactions/hit-map)
                    button (first (filter #(= :live-reopen (:kind %)) regions))
                    toggle (first (filter #(str/ends-with? (str (:node-id %)) ":digest") regions))
                    {:keys [col width] button-row :row} (:bounds button)]

                (expect (= (lv/view-id pane) (:view-id button)))
                (expect (= (count label) width))
                (expect (= (get-in toggle [:bounds :row]) button-row))
                (expect (= button (.lookup interactions/hit-map (int col) (int button-row))))
                ;; Regression, user report: `live` keeps the font of the counts before it.
                (let [cell (fn [c]
                             (.getBackCharacter ts (int c) (int button-row)))
                      ^TextCharacter word (cell col)
                      ^TextCharacter counts (cell (+ 2 (long (get-in toggle [:bounds :col]))))]

                  (expect (= label
                             (apply str
                               (map #(.getCharacterString ^TextCharacter (cell %))
                                    (range col (+ (long col) (long width)))))))
                  (expect (= (.getForegroundColor counts) (.getForegroundColor word)))
                  (expect (= (.getBackgroundColor counts) (.getBackgroundColor word)))
                  (expect (= (.getModifiers counts) (.getModifiers word))))
                (expect (= :toggle-details
                           (:kind (.lookup interactions/hit-map
                                           (int (get-in toggle [:bounds :col]))
                                           (int button-row))))))))))))

(defn paint-review!
  "Paint the real transcript inside a clipped terminal viewport for #222 review."
  ([screen pane] (paint-review! screen pane 0))
  ([screen pane scroll] (paint-review! screen pane scroll {}))
  ([^TerminalScreen screen pane scroll options]
   (.doResizeIfNecessary screen)
   (.clear screen)
   (let [cols
         (.getColumns (.getTerminalSize screen))

         height
         (.getRows (.getTerminalSize screen))

         payload
         (review-payload pane (- cols 4) options)

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

(defdescribe
  inline-live-button-aligns-with-copy
  (it
    "inline live button aligns with copy"
    ;; The clickable LIVE cap shares COPY's inset, button styling and pointer geometry.
    (try
      (doseq [theme-id
              (map keyword (shared-theme/available-theme-ids))

              cols
              [40 80 120]

              scroll
              [0 5]

              pane
              [(review-pane) (lv/minimized (review-pane))
               (lv/settled (review-pane) {:reason :completed} 2000)
               (lv/reopened (lv/settled (review-pane) {:reason :completed} 2000))]]

        (theme/apply-theme! theme-id)
        (with-open [terminal
                    (DefaultVirtualTerminal. (TerminalSize. cols 44))

                    ts
                    (doto (TerminalScreen. terminal) (.startScreen))]

          (binding [interactions/hit-map (interactions/create-hit-map)]
            (let [pane (assoc-in pane [:view :title] (apply str (repeat 12 "Build verification ")))
                  paint! #(paint-review! ts pane scroll)
                  _ (paint!)
                  baseline (grid terminal cols 44)
                  regions (.current interactions/hit-map)
                  copies (filter #(= :copy-disclosure (:kind %)) regions)
                  row (get-in (first (filter #(= :live-reopen (:kind %)) regions)) [:bounds :row])
                  label (if (lv/settled? pane) " Recorded " " LIVE ")
                  button-w (count label)
                  right (let [{:keys [col width]} (:bounds (first copies))]
                          (+ col width))
                  col (- right button-w)
                  cells (subvec (nth baseline row) col right)
                  hit (.lookup interactions/hit-map (int col) (int row))]

              (expect (seq copies))
              (doseq [copy copies]
                (expect (= right (+ (get-in copy [:bounds :col]) (get-in copy [:bounds :width])))))
              (expect (= label (apply str (map #(.getCharacterString ^TextCharacter %) cells))))
              (expect (= {:row row :col col :width button-w} (:bounds hit)))
              (doseq [^TextCharacter cell cells]
                (expect (= theme/button-fg (.getForegroundColor cell)))
                (expect (= theme/button-bg (.getBackgroundColor cell)))
                (expect (not (.isBold cell))))
              (doseq [x (range col right)]
                (let [target (.lookup interactions/hit-map (int x) (int row))]
                  (expect (= :live-reopen (:kind target)))
                  (expect (= (lv/view-id pane) (:view-id target)))
                  (expect (= (:bounds hit) (:bounds target)))))
              (doseq [x (concat (range col) (range right cols))]
                (expect (not= :live-reopen
                              (:kind (.lookup interactions/hit-map (int x) (int row))))))
              (.setHovered interactions/hit-map hit)
              (paint!)
              (let [hovered (grid terminal cols 44)]
                (doseq [^TextCharacter cell (subvec (nth hovered row) col right)]
                  (expect (= theme/header-active-tab-fg (.getForegroundColor cell)))
                  (expect (= theme/header-active-tab-accent (.getBackgroundColor cell)))
                  (expect (.isBold cell)))
                (expect (= (subvec (nth baseline row) 0 col) (subvec (nth hovered row) 0 col)))
                (expect (= (assoc baseline row nil) (assoc hovered row nil))))
              (.setHovered interactions/hit-map nil)
              (paint!)
              (expect (= baseline (grid terminal cols 44)))))))
      (finally (theme/apply-theme! (keyword shared-theme/default-theme-id))))))

(defdescribe
  inline-live-view-has-an-extra-bottom-padding-row
  (it
    "inline live view has an extra bottom padding row"
    ;; A compact receipt retains exactly one blank padding row below its title.
    (doseq [cols
            [40 80 120]

            pane
            [(review-pane) (lv/minimized (review-pane)) (lv/armed (review-pane))
             (lv/reopened (lv/settled (review-pane) {:reason :completed} 2000))]]

      (with-open [terminal
                  (DefaultVirtualTerminal. (TerminalSize. cols 44))

                  ts
                  (doto (TerminalScreen. terminal) (.startScreen))]

        (binding [interactions/hit-map (interactions/create-hit-map)]
          (let [payload (paint-review! ts pane)
                lines (:lines payload)
                index-of (fn [kind]
                           (first (keep-indexed #(when (= kind (get-in %2 [:live-entry :kind])) %1)
                                                (:line-meta payload))))
                title-index (index-of :inline-title)
                padding (nth lines (inc title-index))
                header (first (filter #(= :live-reopen (:kind %)) (.current interactions/hit-map)))
                {:keys [row col width]} (:bounds header)
                padding-row (inc row)]

            (expect (= (subs (nth lines title-index) 0 1) padding))
            (expect (not= padding (nth lines (+ title-index 2))))
            (doseq [^TextCharacter cell
                    (subvec (nth (grid terminal cols 44) padding-row) col (+ col width))]
              (expect (= " " (.getCharacterString cell)))
              (expect (= theme/code-block-bg (.getBackgroundColor cell))))))))))

(defdescribe
  running-grid-and-clipped-clicks
  (it
    "running grid and clipped clicks"
    ;; #222: compact receipts share the Activity inset and clipped viewport origin.
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
            (expect (= (grid ht cols 44) (grid vt cols 44)))
            (let [regions (.current interactions/hit-map)
                  button (first (filter #(= :live-reopen (:kind %)) regions))
                  {:keys [row col]} (:bounds button)
                  lines (mapv #(apply str
                                 (map (fn [cell]
                                        (.getCharacterString ^com.googlecode.lanterna.TextCharacter
                                                             cell))
                                      %))
                              (grid vt cols 44))]

              (expect (some? button))
              (expect (= (:view-id button) (lv/view-id pane)))
              (expect (not-any? #(str/includes? % "Running the focused tests") lines))
              (expect (not-any? #(str/includes? % "Output") lines))
              (let [header-row (first (keep-indexed #(when (str/includes? %2 "ACTIVITY") %1) lines))
                    run-row (first (keep-indexed #(when (str/includes? %2 "LIVE Build") %1) lines))
                    background-cols
                    (fn [r]
                      (into #{}
                            (keep-indexed (fn [col cell]
                                            (when (= theme/code-block-bg
                                                     (.getBackgroundColor
                                                       ^com.googlecode.lanterna.TextCharacter cell))
                                              col)))
                            (nth (grid vt cols 44) r)))]

                (when (and header-row run-row)
                  (expect (= (.indexOf ^String (nth lines header-row) "ACTIVITY")
                             (.indexOf ^String (nth lines run-row) "LIVE")))
                  (expect (= [(apply min (background-cols header-row))
                              (apply max (background-cols header-row))]
                             [(apply min (background-cols run-row))
                              (apply max (background-cols run-row))]))
                  (expect (str/blank? (nth lines (dec run-row))))))
              (expect (= :live-reopen (:kind (.lookup interactions/hit-map (int col) (int row)))))
              (expect (every? #(<= 3 (long (get-in % [:bounds :row])) 40)
                              (filter #(= :live-reopen (:kind %)) regions))))
            (expect (not-any? #(contains? #{:live-inline :live-expand :live-select :live-minimize
                                            :live-log-search}
                                          (:kind %))
                              (.current interactions/hit-map)))
            (doseq [row (range 44)]
              (expect (nil? (#'screen/live-band-pane
                             {:live-views [pane]
                              :layout {:cols cols :rows 44 :inline-live-ids #{(lv/view-id pane)}}}
                             row))))))))))

(defdescribe
  live-cache-disclosure-and-stop
  (it "live cache disclosure and stop"
      ;; #222: each pane transition replaces the existing picture without a duplicate LIVE row.
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

        (expect
          (= (:lines before) (:lines after) (:lines compact) (:lines (review-payload armed 90))))
        (expect (not-any? #(= :live-reopen (:kind %)) (:line-meta after)))
        (expect (= 1 (count (filter #(str/includes? % "Build verification") (:lines compact)))))
        (expect (str/includes? (str/join "\n" (:lines compact)) "LIVE Build verification"))
        (expect (= [:inline-title]
                   (mapv #(get-in % [:live-entry :kind])
                         (filter #(= :activity-live-entry (:kind %))
                                 (:line-meta (review-payload armed 90))))))
        (expect (= :stop (:action (lv/typed armed {:kind :enter})))))))

(defdescribe
  completed-live-receipts-keep-identity
  (it "completed live receipts keep identity"
      ;; #235: settling must keep the Live View label, verdict and reopen target in both projections.
      (doseq [width
              [40 80]

              owned?
              [false true]

              reason
              [:completed :failed :interrupted :timeout :cancelled]

              streaming?
              [false true]]

        (let [pane
              (cond-> (lv/settled (review-pane) {:reason reason} 2000)
                (not owned?)
                (update :view dissoc :owner))

              options
              {:session-id "inline-review"
               :runs [(lv/run-row pane)]
               :now-ms 3000
               :detail-expansions {["inline-review" "iteration:i1:digest"] true}}

              payload
              (if streaming?
                (render/progress->lines-data review-progress width {:show-iterations true} options)
                (render/format-answer-with-thinking-data* nil
                                                          (:iterations review-progress)
                                                          width
                                                          {:show-iterations true}
                                                          nil
                                                          false
                                                          options))

              receipts
              (keep-indexed #(when (= :live-reopen (:kind %2)) [(nth (:lines payload) %1) %2])
                            (:line-meta payload))

              [line meta]
              (first receipts)]

          (expect (= 1 (count receipts)))
          (expect (str/includes? line "LIVE"))
          (expect (str/includes? line "Build verification"))
          (expect (= (lv/view-id pane) (:view-id meta)))
          (expect (= "inline-review" (:session-id meta)))
          (when (= width 80)
            (expect (str/includes? line (name reason)))
            (expect (str/includes? line "1 line")))))))

(defdescribe
  ownerless-and-late-owner-placement
  (it "ownerless and late owner placement"
      ;; #222: no guessed form index; an owner arriving later invalidates placement.
      (let [pane
            (review-pane)

            project
            (fn [progress pane]
              (render/progress->lines-data progress
                                           90
                                           {:show-thinking true :show-iterations true}
                                           {:session-id "owner-test"
                                            :now-ms 0
                                            :live-runs [(lv/transcript-run pane)]
                                            :detail-expansions {["owner-test" "iteration:i1:digest"]
                                                                true}}))

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

        (expect (not (live? missing)))
        (expect (not (live? late)))
        (expect (live? found))
        (expect (seq (:unplaced (#'render/place-run-rows
                                 [{:forms [{}]}]
                                 [{:anchor {:iteration-index 0 :form-index 0}}])))))))

(defdescribe
  empty-history-keeps-owned-live-and-settled-surface
  (it "empty history keeps owned live and settled surface"
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
            {:session-id "history-test"
             :now-ms 0
             :detail-expansions {["history-test" "iteration:i1:digest"] true}}

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

        (expect (some #(= :activity-live-entry (:kind %)) (:line-meta live)))
        (expect (str/includes? (str/join "\n" (:lines settled)) "ACTIVITY"))
        (expect (some #(= :live-reopen (:kind %)) (:line-meta settled))))))

(defdescribe
  live-details-stay-hidden-until-explicitly-opened
  (it
    "live details stay hidden until explicitly opened"
    ;; Owner matching, Activity folding and virtualization must never open a transient.
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

                (expect (= matched? (contains? (:inline-live-ids layout) id)))
                (expect (empty? (last @bands)))
                (expect (nil? measured))
                (expect (not-any? #(contains? #{:live-inline :live-expand :live-select
                                                :live-log-search}
                                              (:kind %))
                                  (.current interactions/hit-map)))))))))))

(defdescribe
  selectable-rows-use-the-existing-keyboard-and-action-route
  (it "selectable rows use the existing keyboard and action route"
      ;; #222: transient pointer and F3 controls name the same authoritative row action.
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
            (.beginFrame interactions/hit-map)
            (lv/paint! (.newTextGraphics ts) 100 44 [pane] 1 3)
            (.commitFrame interactions/hit-map)
            (let [hit (first (filter #(= :live-select (:kind %)) (.current interactions/hit-map)))
                  keyboard (first (filter #(= :live-select (:kind %)) (lv/controls [pane])))]

              (expect (= (select-keys hit [:view-id :node-id :item-id])
                         (select-keys keyboard [:view-id :node-id :item-id])))
              (with-redefs [client/gateway-view-action! (fn [session-id view-id action]
                                                          (deliver called
                                                                   [session-id view-id action]))]
                (#'screen/select-live-row! {:live-views [pane]} hit)
                (expect (= [nil (lv/view-id pane)
                            {:action :select :node-id "jobs" :item-ids ["focused"]}]
                           (deref called 2000 ::timeout))))))))))

(defdescribe transient-scroll-and-minimize-preserve-the-reading-position
             (it "transient scroll and minimize preserve the reading position"
                 ;; #222: only the transient measures node geometry, preserving its viewport when minimized.
                 (with-open [terminal
                             (DefaultVirtualTerminal. (TerminalSize. 80 44))

                             ts
                             (doto (TerminalScreen. terminal) (.startScreen))]

                   (binding [interactions/hit-map (interactions/create-hit-map)]
                     (let [pane (assoc-in (review-pane)
                                  [:view :nodes 1 :lines]
                                  (mapv #(str "Build line " %) (range 50)))
                           measure #(lv/paint! (.newTextGraphics ts) 80 44 [%] 1 3)
                           painted (lv/painted pane (measure pane))
                           scrolled (lv/scrolled painted -4)
                           geometry (measure scrolled)
                           scrolled (lv/painted scrolled geometry)
                           minimized (lv/minimized scrolled)
                           restored (lv/restored (lv/painted minimized (measure minimized)))]

                       (expect (pos? (:visible geometry)))
                       (expect (< (:visible geometry) (:total geometry)))
                       (expect (pos? (:offset geometry)))
                       (expect (not (:is-following scrolled)))
                       (expect (= (:offset scrolled) (:offset restored)))
                       (expect (= (:anchor scrolled) (:anchor restored)))
                       (expect (= (:visible scrolled) (:visible restored))))))))

(defdescribe live-panes-do-not-take-the-static-scroll-fast-path
             (it "live panes do not take the static scroll fast path"
                 ;; #222: the static scroll path does not project the running progress bubble.
                 (let [db
                       {:live-views [(review-pane)]}

                       flags
                       (#'screen/frame-change-flags
                        {:last-db db :db db :last-layout {} :cols 100 :same-size? true})]

                   (expect (every? false? (vals flags))))))

(defdescribe multiple-runs-keep-sibling-separation
             (it "multiple runs keep sibling separation"
                 ;; #222: each owned live pane starts a separate LIVE section, never another nested row.
                 (let [pane
                       (review-pane)

                       second-pane
                       (-> pane
                           (assoc-in [:view :id] "33333333-3333-4333-8333-333333333333")
                           (assoc-in [:view :title] "Release verification"))

                       payload
                       (render/progress->lines-data
                         review-progress
                         90
                         {:show-iterations true}
                         {:session-id "multi-run"
                          :now-ms 0
                          :detail-expansions {["multi-run" "iteration:i1:digest"] true}
                          :live-runs (mapv lv/transcript-run [pane second-pane])})

                       rows
                       (keep-indexed #(when (= :inline-title (get-in %2 [:live-entry :kind])) %1)
                                     (:line-meta payload))]

                   (expect (= 2 (count rows)))
                   (doseq [row rows]
                     (expect (str/blank? (subs (nth (:lines payload) (dec row)) 1)))))))

(defdescribe
  sibling-header-opens-without-folding
  (it "sibling header opens without folding"
      ;; #222: LIVE opens a transient view; the transcript never folds.
      (with-open [terminal
                  (DefaultVirtualTerminal. (TerminalSize. 80 44))

                  ts
                  (doto (TerminalScreen. terminal) (.startScreen))]

        (doseq [pane [(review-pane) (lv/minimized (review-pane))
                      (lv/reopened (lv/settled (review-pane) {:reason :completed} 2000))]]
          (binding [interactions/hit-map (interactions/create-hit-map)]
            (paint-review! ts pane)
            (let [hit (first (filter #(= :live-reopen (:kind %)) (.current interactions/hit-map)))
                  events (atom [])
                  title (:line (first (lv/inline-entries pane 80)))]

              (expect (= "LIVE Build verification" title))
              (expect (some? hit))
              (with-redefs [state/dispatch #(swap! events conj %)]
                (expect (#'screen/activate-live-region! {:live-views [pane]} hit)))
              (expect (some #{[:live-view-reopen (lv/view-id pane)]} @events))))))))

(defdescribe
  mixed-and-saved-runs-keep-sibling-separation
  (it "mixed and saved runs keep sibling separation"
      ;; #222: saved receipts and running panes have the same one-row section boundary.
      (doseq [width
              [40 80]

              mixed?
              [false true]]

        (let [pane
              (review-pane)

              saved
              (assoc (lv/run-row pane)
                :view-id "saved-one"
                :title "Saved one"
                :reason :completed)

              options
              {:session-id "mixed-run"
               :now-ms 0
               :detail-expansions {["mixed-run" "iteration:i1:digest"] true}
               :runs (cond-> [saved]
                       (not mixed?)
                       (conj (assoc saved
                               :view-id "saved-two"
                               :title "Saved two")))
               :live-runs (when mixed? [(lv/transcript-run pane)])}

              payload
              (render/progress->lines-data review-progress width {:show-iterations true} options)

              rows
              (keep-indexed #(when (:run-header? %2) %1) (:line-meta payload))]

          (expect (= 2 (count rows)))
          (doseq [row rows]
            (expect (str/blank? (subs (nth (:lines payload) (dec row)) 1))))))))

(defdescribe
  transient-viewer-keeps-transcript-state
  (it "transient viewer keeps transcript state"
      ;; #222: opening, selecting, settling and closing never fold or duplicate LIVE.
      (let [pane
            (review-pane)

            second-pane
            (assoc-in pane [:view :id] "another-view")

            original
            {:live-views [pane second-pane] :messages [{:runs [(lv/run-row pane)]}]}

            db
            (atom original)]

        (with-redefs [state/app-db db]
          (doseq [selected [pane second-pane]]
            (state/dispatch [:live-view-reopen (lv/view-id selected)])
            (expect (= (lv/view-id selected) (lv/view-id (first (#'screen/viewer-panes @db)))))
            (expect (= (:messages original) (:messages @db)))
            (expect (= (:live-views original) (:live-views @db))))
          (state/dispatch [:live-view-reopen (lv/view-id pane)])
          (swap! db assoc-in [:live-views 0] (lv/settled pane {:reason :completed} 2000))
          (let [frozen @db
                projected (first (#'screen/viewer-panes frozen))]

            (expect (lv/settled? projected))
            (expect (not (lv/dormant? projected)))
            (expect (lv/dormant? (first (:live-views frozen))))
            (expect (every?
                      false?
                      (vals
                        (#'screen/frame-change-flags
                         {:last-db frozen :db frozen :last-layout {} :cols 80 :same-size? true}))))
            (state/dispatch [:live-viewer-close])
            (expect (nil? (#'screen/viewer-panes @db)))
            (expect (= (:live-views frozen) (:live-views @db)))
            (expect (= (:messages original) (:messages @db))))))))

(defn viewer-review-db
  "Deterministic full production frame for the transient Live View viewer."
  [pane]
  {:session {:id "viewer-review"}
   :messages [{:role :assistant :text ""}]
   :input (input/empty-input)
   :scroll scroll/follow
   :loading? true
   :progress review-progress
   :settings {:show-iterations true}
   :live-views [pane]
   :live-viewer-id (lv/view-id pane)
   :pending-sends []
   :channel-status {}
   :tabs []
   :tab-locals {}
   :slash-command-index 0
   :render-version 0})

(defn paint-viewer-review!
  "Paint the production frame, including the selected transient band."
  [ts cols db]
  (with-redefs [timg/images-protocol
                (constantly nil)

                client/get-router
                (constantly nil)]

    (#'screen/render-frame! ts cols 44 db 1000)))

(defdescribe
  transient-viewer-full-frame-open-close
  (it "transient viewer full frame open close"
      ;; #222, #235: the × button targets the selected live/frozen view and clears on dismissal.
      (doseq [cols
              [40 80 120]

              settled?
              [false true]]

        (with-open [terminal
                    (DefaultVirtualTerminal. (TerminalSize. cols 44))

                    ts
                    (doto (TerminalScreen. terminal) (.startScreen))]

          (let [pane
                (cond-> (review-pane)
                  settled?
                  (lv/settled {:reason :completed} 2000))

                db
                (atom (cond-> (dissoc (viewer-review-db pane) :live-viewer-id)
                        settled?
                        (assoc-in [:messages 0 :runs] [(lv/run-row pane)])))

                text
                #(apply str
                   (mapcat (fn [row]
                             (map (fn [^TextCharacter cell]
                                    (.getCharacterString cell))
                                  row))
                           (grid terminal cols 44)))

                details?
                #(str/includes? (str/replace (text) #"[\s│]+" "") "Runningthefocusedtests")]

            (binding [interactions/hit-map (interactions/create-hit-map)]
              (with-redefs [state/app-db db]
                (paint-viewer-review! ts cols @db)
                (expect (not (details?))
                        (str "Closed receipt at " cols " columns; settled=" settled?))
                (expect (not-any? #(= :live-viewer-close (:kind %))
                                  (.current interactions/hit-map)))
                (let [button (first (filter #(= :live-reopen (:kind %))
                                            (.current interactions/hit-map)))]
                  (expect (some? button))
                  (expect (#'screen/activate-live-region! @db button)))
                (expect (= (lv/view-id pane) (:live-viewer-id @db)))
                (paint-viewer-review! ts cols @db)
                (expect (details?))
                (let [close (first (filter #(= :live-viewer-close (:kind %))
                                           (.current interactions/hit-map)))
                      before (:messages @db)]

                  (expect (some? close))
                  (expect (= (lv/view-id pane) (:view-id close)))
                  (expect (#'screen/activate-live-region! @db close))
                  (expect (= before (:messages @db)))
                  (paint-viewer-review! ts cols @db)
                  (expect (not (details?)))
                  (expect (not-any? #(= :live-viewer-close (:kind %))
                                    (.current interactions/hit-map)))))))))))

(defdescribe
  hidden-live-panes-do-not-own-keyboard-controls
  (it
    "hidden live panes do not own keyboard controls"
    (let [pane
          (review-pane)

          newer
          (assoc-in pane [:view :id] "newer-view")

          db
          (atom (assoc (viewer-review-db pane) :live-views [(lv/armed pane) newer]))

          controls
          (atom [])]

      (with-redefs [state/app-db
                    db

                    dialogs/list-dialog!
                    (fn [_screen _title items _options]
                      (swap! controls conj items)
                      nil)]

        (swap! db dissoc :live-viewer-id)
        (#'screen/live-controls! nil @db)
        (expect (empty? @controls))
        (expect (nil? (#'screen/arm-front-live-view! @db)))
        (#'screen/live-stop-key! @db (KeyStroke. \x false false))
        (expect (= "" (lv/stopping (first (:live-views @db)))))
        (expect (nil? (:live-viewer-id @db)))
        (swap! db assoc :live-viewer-id (lv/view-id pane))
        (#'screen/live-controls! nil @db)
        (expect (= 1 (count @controls)))
        (expect (every? #(= (lv/view-id pane) (:view-id %)) (first @controls)))
        (#'screen/live-stop-key! @db (KeyStroke. \x false false))
        (expect (= "x" (lv/stopping (first (:live-views @db)))))
        (expect (nil? (lv/stopping (second (:live-views @db)))))))))

(defdescribe transient-log-search-does-not-open-a-dialog
             (it "transient log search does not open a dialog"
                 ;; #235 follow-up: Search belongs to the existing LIVE transient.
                 (let [pane
                       (review-pane)

                       db
                       (atom (viewer-review-db pane))

                       dialogs-opened
                       (atom 0)]

                   (with-redefs [state/app-db
                                 db

                                 dialogs/text-input-dialog!
                                 (fn [& _]
                                   (swap! dialogs-opened inc)
                                   nil)]

                     (expect
                       (#'screen/activate-live-region!
                        @db
                        {:kind :live-log-search :view-id (lv/view-id pane) :node-id "output"}))
                     (expect (zero? @dialogs-opened))
                     (expect (= "output" (get-in @db [:live-viewer-search :node-id])))
                     (expect (= (lv/view-id pane) (:live-viewer-id @db)))
                     (expect (= (:input (viewer-review-db pane)) (:input @db)))))))

(defdescribe viewer-wheel-stays-inside-band
             (it "viewer wheel stays inside band"
                 ;; #222: an explicit viewer must not capture transcript or header wheel input.
                 (let [pane
                       (review-pane)

                       db
                       (assoc (viewer-review-db pane) :layout {:cols 80 :rows 44})]

                   (binding [interactions/hit-map (interactions/create-hit-map)]
                     (with-redefs [state/band-anchor (constantly {:content-top 4 :prompt-h 3})
                                   lv/band-rows (constantly [10 30])]

                       (expect (nil? (#'screen/live-band-pane db 2)))
                       (expect (nil? (#'screen/live-band-pane db 40)))
                       (expect (= (lv/view-id pane)
                                  (lv/view-id (#'screen/live-band-pane db 15)))))))))

(defn- viewer-lines
  [terminal cols]
  (mapv #(apply str
           (map (fn [cell]
                  (.getCharacterString ^com.googlecode.lanterna.TextCharacter cell))
                %))
        (grid terminal cols 44)))

(defdescribe
  transient-log-search-full-frame-and-pagination
  (it
    "transient log search full frame and pagination"
    ;; #235 follow-up: live and recorded searches share the same band and leave the draft alone.
    (doseq [cols
            [40 80 120]

            settled?
            [false true]]

      (with-open [terminal
                  (DefaultVirtualTerminal. (TerminalSize. cols 44))

                  ts
                  (doto (TerminalScreen. terminal) (.startScreen))]

        (let [pane
              (cond-> (assoc-in (review-pane) [:view :session-id] "viewer-review")
                settled?
                (lv/settled {:reason :completed} 2000))

              pane
              (assoc-in pane [:disclosures "output"] true)

              draft
              (input/paste-text (input/empty-input) "Keep this draft")

              db
              (atom (assoc (viewer-review-db pane) :input draft))

              calls
              (atom [])]

          (binding [interactions/hit-map (interactions/create-hit-map)]
            (with-redefs [state/app-db db
                          client/live-view-log
                          (fn [& args]
                            (swap! calls conj (vec args))
                            {"matched" 205
                             "total" 10000
                             "line_numbers" [(if (zero? (long (nth args 3))) 3 9990)]
                             "lines" [(if (zero? (long (nth args 3)))
                                        "ERROR retained output before the visible tail"
                                        "ERROR final-page")]})]

              (paint-viewer-review! ts cols @db)
              (let [hit (first (filter #(= :live-log-search (:kind %))
                                       (.current interactions/hit-map)))]
                (expect (some? hit))
                (expect (#'screen/activate-live-region! @db hit)))
              (doseq [ch "ERROR"]
                (expect (#'screen/live-log-search-key!
                         @db
                         (KeyStroke. (Character/valueOf (char ch)) false false))))
              (expect (not= ::timeout (deref (#'screen/read-live-log! @db 0) 2000 ::timeout)))
              (expect (= ["viewer-review" (lv/view-id pane) "output" 0 200 "ERROR"] (last @calls)))
              (paint-viewer-review! ts cols @db)
              (let [lines (viewer-lines terminal cols)
                    cursor (.getCursorPosition ts)
                    hit (first (filter #(= :live-log-page (:kind %))
                                       (.current interactions/hit-map)))]

                (expect (some #(str/includes? % "Search Output") lines))
                (expect (some #(str/includes? % "3: ERROR retained") lines))
                (expect (some? cursor))
                (expect (str/includes? (nth lines (.getRow cursor)) "› ERROR"))
                (expect (= 1 (:direction hit)))
                (expect (some #(= :live-viewer-close (:kind %)) (.current interactions/hit-map))))
              (expect (not= ::timeout (deref (#'screen/page-live-log! @db 1) 2000 ::timeout)))
              (expect (= 200 (get-in @db [:live-viewer-search :from])))
              (expect (= ["viewer-review" (lv/view-id pane) "output" 200 200 "ERROR"]
                         (last @calls)))
              (paint-viewer-review! ts cols @db)
              (expect (some #(str/includes? % "9990: ERROR final-page")
                            (viewer-lines terminal cols)))
              (expect (= draft (:input @db)))
              (expect (= (:view pane) (get-in @db [:live-views 0 :view])))
              (expect (#'screen/live-log-search-key! @db (KeyStroke. KeyType/Escape)))
              (expect (nil? (:live-viewer-search @db)))
              (expect (= (lv/view-id pane) (:live-viewer-id @db)))
              (expect (= draft (:input @db)))
              (paint-viewer-review! ts cols @db)
              (expect (some #(= :live-log-search (:kind %)) (.current interactions/hit-map)))
              (expect (not-any? #(= :live-log-page (:kind %))
                                (.current interactions/hit-map))))))))))

(defdescribe
  transient-log-search-cancels-a-pending-read-without-stopping-the-view
  (it
    "transient log search cancels a pending read without stopping the view"
    (let [pane
          (review-pane)

          db
          (atom (viewer-review-db pane))

          started
          (promise)

          release
          (promise)]

      (with-redefs [state/app-db
                    db

                    client/live-view-log
                    (fn [& args]
                      (deliver started args)
                      (deref release 2000 nil)
                      {"matched" 1 "total" 900 "lines" ["old"] "line_numbers" [1]})]

        (#'screen/search-live-log! @db {:view-id (lv/view-id pane) :node-id "output"})
        (let [task (#'screen/read-live-log! @db 0)]
          (try (expect (not= ::timeout (deref started 2000 ::timeout)))
               (expect (get-in @db [:live-viewer-search :loading?]))
               (expect (#'screen/live-log-search-key! @db (KeyStroke. KeyType/Escape)))
               (expect (nil? (:live-viewer-search @db)))
               (expect (not (lv/stopping (first (:live-views @db)))))
               (deliver release true)
               (expect (not= ::timeout (deref task 2000 ::timeout)))
               (expect (nil? (:live-viewer-search @db)))
               (expect (= (lv/view-id pane) (:live-viewer-id @db)))
               (finally (deliver release true))))))))

(defdescribe
  transient-log-search-keyboard-submit-scroll-and-paste
  (it "transient log search keyboard submit scroll and paste"
      (let [pane
            (review-pane)

            db
            (atom (viewer-review-db pane))

            requests
            (atom [])]

        (with-redefs-fn {#'state/app-db db
                         #'screen/read-live-log! (fn [_ from]
                                                   (swap! requests conj from))}
          (fn []
            (state/dispatch [:live-view-search-open (lv/view-id pane) "output"])
            (swap! db update :live-viewer-search assoc :page {"matched" 401} :total 20 :visible 5)
            (#'screen/live-log-search-key! @db (KeyStroke. KeyType/ArrowDown))
            (expect (= 1 (get-in @db [:live-viewer-search :offset])))
            (#'screen/live-log-search-key! @db (KeyStroke. KeyType/Enter))
            (#'screen/live-log-search-key! @db (KeyStroke. KeyType/PageDown))
            (expect (= [0 200] @requests))
            (#'screen/live-log-search-key! @db (KeyStroke. KeyType/PasteStart))
            (#'screen/live-log-search-key! @db (KeyStroke. (Character/valueOf \x) false false))
            (#'screen/live-log-search-key! @db (KeyStroke. KeyType/Enter))
            (#'screen/live-log-search-key! @db (KeyStroke. KeyType/PasteEnd))
            (expect (= [0 200] @requests))
            (expect (= "x"
                       (str/trim (input/input->text (get-in @db [:live-viewer-search :input])))))
            (expect (= (input/empty-input) (:input @db)))
            (state/dispatch [:live-viewer-close])
            (expect (nil? (:live-viewer-search @db))))))))

(defdescribe transient-log-search-result-stays-with-its-tab
             (it "transient log search result stays with its tab"
                 (let [search
                       #(lv/log-search-requested (lv/log-search-opened "output") 0 %)

                       db
                       (atom {:active-tab-id "a"
                              :tabs [{:id "a"} {:id "b"}]
                              :live-viewer-search (search :a)
                              :tab-locals {"b" {:live-viewer-search (search :b)}}})

                       page
                       {"matched" 0 "total" 900 "lines" []}]

                   (with-redefs [state/app-db db]
                     (state/dispatch [:live-view-search-result :b {:page page}])
                     (expect (get-in @db [:live-viewer-search :loading?]))
                     (expect (= page (get-in @db [:tab-locals "b" :live-viewer-search :page])))
                     (state/dispatch [:live-view-search-close])
                     (state/dispatch [:live-view-search-result :a {:page page}])
                     (expect (nil? (:live-viewer-search @db)))))))

(defdescribe
  transient-log-search-retries-in-place
  (it "transient log search retries in place"
      (let [pane
            (review-pane)

            db
            (atom (viewer-review-db pane))

            attempts
            (atom 0)

            empty-page
            {"matched" 0 "total" 900 "lines" [] "line_numbers" []}]

        (with-redefs [state/app-db
                      db

                      client/live-view-log
                      (fn [& _]
                        (if (= 1 (swap! attempts inc))
                          (throw (ex-info "Log unavailable" {}))
                          empty-page))]

          (#'screen/search-live-log! @db {:view-id (lv/view-id pane) :node-id "output"})
          (expect (not= ::timeout (deref (#'screen/read-live-log! @db 0) 2000 ::timeout)))
          (expect (:error (:live-viewer-search @db)))
          (expect (not= ::timeout (deref (#'screen/read-live-log! @db 0) 2000 ::timeout)))
          (expect (not (:error (:live-viewer-search @db))))
          (expect (= empty-page (get-in @db [:live-viewer-search :page])))
          (state/dispatch [:live-view-close (lv/view-id pane) {:reason :completed}])
          (expect (lv/settled? (first (:live-views @db))))
          (expect (= empty-page (get-in @db [:live-viewer-search :page])))
          (expect (= (lv/view-id pane) (:live-viewer-id @db)))))))

(defdescribe
  running-live-receipt-survives-owning-turn-completion
  (it "running live receipt survives owning turn completion"
      ;; #248: an open background view stays below its Activity while later turns run.
      (with-open [terminal
                  (DefaultVirtualTerminal. (TerminalSize. 100 44))

                  ts
                  (doto (TerminalScreen. terminal) (.startScreen))]

        (let [pane
              (review-pane)

              view-id
              (lv/view-id pane)

              db
              (atom (-> (viewer-review-db pane)
                        (dissoc :live-viewer-id)
                        (assoc :live-views [])))

              receipt?
              #(some (fn [hit]
                       (and (= :live-reopen (:kind hit)) (= view-id (:view-id hit))))
                     (.current interactions/hit-map))]

          (binding [interactions/hit-map (interactions/create-hit-map)]
            (with-redefs [state/app-db db]
              (state/dispatch [:live-view-open (assoc (:view pane) :session-id "viewer-review")])
              (paint-viewer-review! ts 100 @db)
              (expect (receipt?) "The running turn exposes a Live receipt")
              (swap! db update
                :messages
                #'state/replace-pending-assistant
                {:role :assistant
                 :text "Background verification continues."
                 :traces (:iterations review-progress)})
              (swap! db assoc :loading? false :progress nil)
              (paint-viewer-review! ts 100 @db)
              (expect (receipt?) "Completing the owning turn must not hide a running Live receipt")
              (expect (= [view-id] (mapv :view-id (get-in @db [:messages 0 :runs]))))
              (swap! db update
                :messages
                conj
                {:role :user :text "Continue"}
                {:role :assistant :text "Next turn"})
              (swap! db assoc :loading? true :progress {:iterations [{:thinking "Next thought"}]})
              (paint-viewer-review! ts 100 @db)
              (expect (= 1
                         (count (filter #(and (= :live-reopen (:kind %)) (= view-id (:view-id %)))
                                        (.current interactions/hit-map))))
                      "A new turn must not duplicate an earlier turn's still-running receipt")
              (state/dispatch [:live-view-close view-id {:reason :completed}])
              (expect (= [:completed] (mapv :reason (get-in @db [:messages 0 :runs])))
                      "A later close updates the original receipt, not the newest turn")
              (expect (empty? (:runs (last (:messages @db)))))))))))

(defn- long-log-pane
  [line-count]
  (lv/opened (-> (fixture/view {:title "Build verification"}
                               (fixture/log "console"
                                            {:label "Output"
                                             :default-expanded true
                                             :lines (mapv #(str "console line " % " output")
                                                          (range line-count))}))
                 fixture/normalize-live-view
                 materializer/materialize
                 (assoc :id "22222222-2222-4222-8222-222222222222"
                        :session-id "viewer-review"
                        :seq 0
                        :created-at 0
                        :owner owner))))

(defdescribe transcript-bar-yields-its-lane-to-the-live-band
             (it "transcript bar yields its lane to the live band"
                 ;; One press, one activation: the transcript's bar lane covers the band's rows,
                 ;; so arming it there jumped the transcript AND fired the band control the
                 ;; release landed on — a click on the transient's close icon activated two areas.
                 (let [db (assoc (viewer-review-db (review-pane)) :layout {:cols 80 :rows 44})]
                   (with-redefs [state/band-anchor (constantly {:content-top 4 :prompt-h 3})
                                 lv/band-rows (constantly [10 30])]

                     (expect (#'screen/transcript-bar-owns-press? db 2 false))
                     (expect (#'screen/transcript-bar-owns-press? db 40 false))
                     (expect (not (#'screen/transcript-bar-owns-press? db 15 false)))
                     (expect (not (#'screen/transcript-bar-owns-press? db 30 false)))
                     ;; A drag that began on the thumb keeps the bar while it crosses the band.
                     (expect (#'screen/transcript-bar-owns-press? db 15 true))))))

(defdescribe
  live-log-search-rides-the-band-heading
  (it "live log search rides the band heading"
      ;; A watched run follows the tail of its log, and that carries the log's own row
      ;; — with the Search control on it — out of the window: the retained output had
      ;; no reachable entrance at all. The heading keeps that control, and the band's
      ;; bar stands left of the close control, clear of the transcript scrollbar.
      (let [cols 120]
        (with-open [terminal (DefaultVirtualTerminal. (TerminalSize. cols 44))
                    ts (doto (TerminalScreen. terminal) (.startScreen))]

          (let [pane (assoc-in (long-log-pane 300) [:disclosures "console"] true)
                db (atom (assoc (viewer-review-db pane) :layout {:cols cols :rows 44}))]

            (binding [interactions/hit-map (interactions/create-hit-map)]
              (with-redefs [state/app-db db]
                (paint-viewer-review! ts cols @db)
                (let [regions (vec (.current interactions/hit-map))
                      search (first (filter #(= :live-log-search (:kind %)) regions))
                      close (first (filter #(= :live-viewer-close (:kind %)) regions))
                      {:keys [row col width]} (:bounds search)
                      lines (viewer-lines terminal cols)
                      body (first (filter #(str/includes? % "console line") lines))
                      rail (long (str/last-index-of body "│"))
                      bar (str/last-index-of (subs body 0 rail) "│")]

                  (expect (some? search) "A long log must still expose its Search control")
                  (expect (= "console" (:node-id search)))
                  (expect (= (lv/view-id pane) (:view-id search)))
                  (expect (= "Search Output" (:label search)))
                  (expect (not-any? #(= :live-expand (:kind %)) regions)
                          "The log's own row is off the window while the band follows its tail")
                  (expect (= (:row (:bounds close)) row) "Search belongs to the heading row")
                  (expect (< (+ (long col) (long width)) (long (:col (:bounds close)))))
                  (expect (= :live-log-search
                             (:kind (.lookup interactions/hit-map (int col) (int row)))))
                  (expect (= :live-log-search
                             (:kind (.lookup interactions/hit-map
                                             (int (+ (long col) (long width) -1))
                                             (int row)))))
                  (expect (= 4 (- bar (+ (long col) (long width))))
                          "Search has four clear columns before the bar")
                  (expect (<= (+ bar 3) (long (get-in close [:bounds :col])))
                          "two clear columns separate the bar from the close button")
                  (expect (= \space (nth body (dec rail))) "the bar stays clear of the right rail")
                  (expect (< bar (- cols (long render/MESSAGE_MARGIN_RIGHT) 1))
                          "…and clear of the columns the transcript's own bar owns")
                  (expect (#'screen/activate-live-region! @db search))
                  (expect (= "console" (get-in @db [:live-viewer-search :node-id]))
                          "The heading control opens the same search the log's row opened")))))))))
