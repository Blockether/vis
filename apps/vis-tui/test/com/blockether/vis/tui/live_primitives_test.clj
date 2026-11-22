(ns com.blockether.vis.tui.live-primitives-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.view :as spec]
            [com.blockether.vis.tui.capture :as capture]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.human-input :as hi]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.live-view :as lv]
            [com.blockether.vis.tui.markers :as markers]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.view-materializer :as materializer]
            [com.blockether.vis.tui.theme :as theme]
            [com.blockether.vis.tui.shared-theme :as shared-theme]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.html HtmlTerminal]))

(defn fixture
  []
  (hi/live-view<-wire (json/read-json (slurp (io/resource
                                               "vis-contract/fixtures/live-primitives.json")))))

(defn- entries [pane] (tree-seq #(seq (:cells %)) :cells {:cells (lv/plan pane 80)}))

(defdescribe
  all-primitives-paint-plan-test
  (it "all primitives paint plan"
      (let [view
            (fixture)

            pane
            (lv/opened view)

            rows
            (entries pane)]

        (expect (nil? (spec/live-view-error view)))
        (expect (= 6 (count (set (map :node-id (filter #(= :heading (:kind %)) rows))))))
        (expect (= 4 (count (filter #(= :spinner (:kind %)) rows))))
        (expect (str/includes? (str/join "\n" (map :text rows)) "# <button> is literal text"))
        (expect (lv/animating? [pane]))
        (expect (= #{"refresh" "target" "token" "notes" "region" "tags" "notify" "risk" "due" "pin"
                     "apply" "plan-small" "details" "table" "canvas"}
                   (set (map :node-id (lv/controls [pane])))))
        (expect (not (lv/animating? [(lv/settled pane {:reason :completed})])))
        (expect (not (lv/animating? [(lv/minimized pane)])))
        (let [markdown (materializer/->markdown view)]
          (expect (= markdown
                     (materializer/->markdown (:view (materializer/parse-markdown markdown)))))))))

(defdescribe
  canvas-rows-test
  (it "animates visible canvas markers by time without moving the text"
      (let [board {:id "board" :type :canvas
                   :cards [{:id "a" :title "First"}
                           {:id "b" :title "Second" :edges [{:from "a" :animation "dot" :is-active true}]}]}
            pane (lv/opened {:nodes [board]})
            idle (lv/opened {:nodes [(update-in board [:cards 1] dissoc :edges)]})
            closed (lv/opened {:nodes [{:id "g" :type :group :is-collapsible true :children [board]}]})]
        (expect (lv/animating? [pane]))
        (expect (not (lv/animating? [idle])))
        (expect (not (lv/animating? [closed])))
        (expect (not (lv/animating? [(lv/minimized pane)])))
        (expect (not (lv/animating? [pane idle])))
        (expect (not (lv/animating? [(lv/painted pane {:canvas-motion-visible? false})])))
        (doseq [width [20 32 40 60 80 120 200]]
          (let [rows #(mapv :text (lv/plan pane width %))
                edge #(some (fn [line] (when (and line (str/includes? line "First")
                                                 (not (str/includes? line "card"))) line)) (rows %))]
            (expect (some? (edge 1000)))
            (expect (not= (edge 1000) (edge 1200)))
            (expect (= (edge 1000) (edge 1099)))
            (expect (= (mapv p/display-width (rows 1000)) (mapv p/display-width (rows 1200))))))))
  (it
    "shows nested items and the details of a resolved comment"
    (let [cards
          [{:id "comment"
            :title "Review"
            :parent "frame"
            :caption "Ready to ship"
            :progress 0.25
            :picture {:alt "Layout"}
            :media {:kind "audio" :url "https://example.com/a" :alt "Notes"}
            :edges [{:from "frame" :line "none" :label "Hidden"} {:from "frame" :label "Shown"}]}
           {:id "frame" :title "Group" :kind "frame"}
           {:id "a" :title "Cycle A" :kind "frame" :parent "b"}
           {:id "b" :title "Cycle B" :kind "frame" :parent "a"}
           {:id "missing" :title "Unknown parent" :kind "note" :parent "absent"}]

          rows
          (filter :item-id
                  (lv/plan (lv/opened {:title "Canvas"
                                       :nodes [{:id "g"
                                                :type :canvas
                                                :cards cards
                                                :threads [{:id "review"
                                                           :item-id "comment"
                                                           :is-resolved true
                                                           :draft "Saved text"
                                                           :messages [{:author {:name "Ada"}
                                                                       :body
                                                                       "Check the layout"}]}]}]})
                           32))

          comment
          (filter #(= "comment" (:item-id %)) rows)

          text
          (str/join "\n" (map :text comment))]

      (expect (= ["frame" "comment" "a" "b" "missing"] (vec (distinct (map :item-id rows)))))
      (expect (str/starts-with? (:text (first comment)) "  "))
      (expect (every? #(not (str/starts-with? (:text %) "  "))
                      (filter #(contains? #{"a" "b" "missing"} (:item-id %)) rows)))
      (doseq [detail ["Ada" "Resolved thread" "Ready to ship" "Check the layout" "Draft: Saved text"
                      "25%" "Layout" "audio" "https://example.com/a" "Notes" "Shown"]]
        (expect (str/includes? text detail) detail))
      (expect (not (str/includes? text "Hidden")))
      (expect (every? #(<= (p/display-width (:text %)) 32) rows))))
  (it "changes the marker for each card animation without shifting the text"
      (doseq [mode (get-in (document/schema-document "view") ["$defs" "canvas_animation" "enum"])]
        (let [pane (lv/opened {:nodes [{:id "board" :type :canvas
                                       :cards [{:id "a" :title "First" :animation mode}]}]})
              line #(some :text (lv/plan pane 20 %))]
          (expect (lv/animating? [pane]))
          (expect (not= (line 1000) (line 1400)))
          (expect (= (p/display-width (line 1000)) (p/display-width (line 1400)))))))
  (it "puts a card under a frame past the depth limit back on the board"
      (let [limit
            (:max-depth spec/canvas-defaults)

            cards
            (mapv (fn [i]
                    {:id (str i) :title (str "Frame " i) :kind "frame" :parent (str (dec i))})
                  (range (+ 2 (long limit))))

            rows
            (filter :item-id
                    (lv/plan (lv/opened {:title "Canvas"
                                         :nodes [{:id "g" :type :canvas :cards cards}]})
                             80))]

        (expect (str/starts-with? (:text (nth rows limit)) "  "))
        (expect (not (str/starts-with? (:text (last rows)) "  ")))))
  (it
    "shows the same Markdown blocks on canvas cards and in the transcript"
    (let [view
          (fixture)

          canvas
          (first (filter #(= "canvas" (:id %))
                         (tree-seq :children :children {:children (:nodes view)})))

          pane
          (lv/expanded (lv/opened (assoc view :nodes [canvas])) "canvas")]

      (doseq [width
              [60 100]

              {:keys [id text]}
              (:cards canvas)

              :when (#{"verify" "ship"} id)]

        (let [[head & body]
              (filter #(= id (:item-id %)) (lv/plan pane width))

              inset
              (+ 2 (count (take-while #{\space} (:text head))))

              transcript
              (render/format-answer-markdown-data* (client/markdown->ast text) (- width inset) nil)

              expected
              (mapv markers/plain-text (rest (:lines transcript)))

              actual
              (mapv #(subs (:text %) (min inset (count (:text %)))) (take (count expected) body))]

          (expect (= expected actual) (str width " columns, card " id))))))
  (it "keeps the line of a canvas card in view when rows arrive above it"
      (let [rows-for
            (fn [first-text]
              (lv/plan (lv/opened {:title "Canvas"
                                   :nodes [{:id "g"
                                            :type :canvas
                                            :cards [{:id "first" :title "First" :text first-text}
                                                    {:id "next"
                                                     :title "Next"
                                                     :text "- One\n- Two\n- Three"}]}]})
                       60))

            before
            (rows-for "Start")

            after
            (rows-for "- Start\n- Continue")

            line
            (fn [rows]
              (first (keep-indexed (fn [i row]
                                     (when (str/includes? (:text row) "Two") i))
                                   rows)))

            anchor
            (lv/anchor-at before (line before))]

        (expect (= (line after) (lv/offset {:anchor anchor} after 1)))))
  (it
    "opens a Markdown link from a canvas thread quote"
    (let [url
          "https://example.com/checks"

          pane
          (lv/opened {:title "Canvas"
                      :nodes [{:id "g"
                               :type :canvas
                               :cards [{:id "check" :title "Check"}]
                               :threads [{:id "review"
                                          :item-id "check"
                                          :messages [{:author {:name "Ada"}
                                                      :body
                                                      (str "> [Read the checks](" url ")")}]}]}]})

          hit-map
          (interactions/create-hit-map)]

      (with-open [terminal
                  (-> (HtmlTerminal/builder)
                      (.initialSize (TerminalSize. 60 32))
                      (.build))

                  screen
                  (TerminalScreen. terminal)]

        (.startScreen screen)
        (binding [interactions/hit-map hit-map]
          (.beginFrame hit-map)
          (lv/paint! (.newTextGraphics screen) 60 32 [pane] 1 3)
          (.commitFrame hit-map))
        (let [rows
              (mapv (fn [row]
                      (apply str
                        (map (fn [col]
                               (.getCharacterString
                                 (.getBackCharacter screen (TerminalPosition. col row))))
                             (range 60))))
                    (range 32))

              [y x]
              (first (keep-indexed (fn [idx row]
                                     (when-let [col (str/index-of row "│ Read the checks")]
                                       [idx (+ col 2)]))
                                   rows))]

          (expect (some? y))
          (expect (= url (:url (.lookup hit-map (int x) (int y)))))))))
  (it "keeps each full media URL in the expanded canvas at 32 columns"
      (let [view
            (fixture)

            canvas
            (first (filter #(= "canvas" (:id %))
                           (tree-seq :children :children {:children (:nodes view)})))

            pane
            (lv/expanded (lv/opened (assoc view :nodes [canvas])) "canvas")

            rows
            (filter :item-id (lv/plan pane 32))]

        (doseq [{:keys [id media]} (filter :media (:cards canvas))]
          (let [text (str/join (map (comp str/trim :text) (filter #(= id (:item-id %)) rows)))]
            (expect (str/includes? text (:url media)) id)))
        (expect (every? #(<= (p/display-width (:text %)) 32) rows)))))

(defdescribe
  badge-list-chart-rows-test
  (it "paints badges, list items and chart values as rows a person can read"
      (let [view
            (fixture)

            texts
            (fn [pane node-id]
              (mapv #(str/trimr (:text %))
                    (filter #(and (= node-id (:node-id %)) (#{:badge :item :chart :axis} (:kind %)))
                            (entries pane))))

            pane
            (lv/opened view)]

        (expect (= ["✓ Stable   ! Beta   • Docs"] (texts pane "labels")))
        (expect (= ["1. Pin the release — After the checks pass" "2. Deploy the playground"]
                   (texts pane "todo")))
        (let [bars (texts pane "load")]
          (expect (= 6 (count bars)))
          (expect (re-matches #"Mon  API  █+[▏▎▍▌▋▊▉]? +12" (first bars)))
          (expect (re-matches #"     Web  █+[▏▎▍▌▋▊▉]? + 4" (second bars)))
          ;; The longest bar belongs to the greatest value, 18.
          (expect (= "Tue  API" (subs (apply max-key #(count (re-seq #"█" %)) bars) 0 8))))
        (let [lines (texts (lv/patched pane
                                       {:view-id (:id view)
                                        :seq 1
                                        :ops [{:op :set :node-id "load" :kind :line}]})
                           "load")]
          (expect (= ["API  ▅▅▅▅████▃▃▃▃  9" "Web  ▁▁▁▁▂▂▂▂▅▅▅▅  11" "     Mon      Wed"]
                     lines))))))

(defdescribe chart-zero-baseline-test
             (it "draws signed bars from zero and leaves zero empty"
                 (let [node
                       {:id "signed"
                        :type :chart
                        :kind :bar
                        :categories ["Loss" "Zero" "Gain"]
                        :series [{:id "delta" :label "Delta" :values [-10 0 10]}]}

                       rows
                       (#'lv/node-rows node {:text-w 40})

                       [loss zero gain]
                       (map :text rows)]

                   (expect (str/includes? loss "█"))
                   (expect (not (str/includes? zero "█")))
                   (expect (str/includes? gain "█"))
                   (expect (every? #(str/includes? % "│") [loss zero gain]))
                   (when (every? #(str/includes? % "│") [loss zero gain])
                     (expect (< (str/index-of loss "█") (str/index-of loss "│")))
                     (expect (< (str/index-of gain "│") (str/index-of gain "█")))))))

(defdescribe narrow-chart-values-test
             (it "keeps values and series names when label columns cannot fit"
                 (let [node
                       {:id "narrow"
                        :type :chart
                        :kind :bar
                        :categories ["Qualification" "Production"]
                        :series [{:id "api" :label "Long API service" :values [1234 5678]}
                                 {:id "web" :label "Long Web service" :values [120 200]}]}

                       rows
                       (#'lv/node-rows node {:text-w 26})

                       text
                       (str/join " " (map :text rows))]

                   (doseq [value ["1234" "5678" "120" "200" "Long API service" "Long Web service"]]
                     (expect (str/includes? text value)))
                   (expect (every? #(<= (p/display-width (:text %)) 26) rows))))
             (it "keeps the latest line values in a narrow terminal"
                 (let [node
                       {:id "line"
                        :type :chart
                        :kind :line
                        :series [{:id "api" :label "API" :values [12 18 9]}
                                 {:id "web" :label "Web" :values [4 7 11]}]}

                       rows
                       (#'lv/node-rows node {:text-w 8})

                       text
                       (str/join " " (map :text rows))]

                   (expect (str/includes? text "9"))
                   (expect (str/includes? text "11"))
                   (expect (every? #(<= (p/display-width (:text %)) 8) rows)))))

(defdescribe list-details-wrap-test
             (it "keeps numbered text and details across narrow rows"
                 (let [node
                       (first (filter #(= "todo" (:id %)) (:nodes (fixture))))

                       rows
                       (#'lv/node-rows node {:text-w 26})

                       text
                       (-> (str/join " " (map :text rows))
                           (str/replace #"\s+" " "))]

                   (expect (str/includes? text "Pin the release"))
                   (expect (str/includes? text "After the checks pass"))
                   (expect (str/includes? text "Deploy the playground"))
                   (expect (= 1 (count (filter #(str/starts-with? (:text %) "1. ") rows))))
                   (expect (= 1 (count (filter #(str/starts-with? (:text %) "2. ") rows))))
                   (expect (every? #(<= (p/display-width (:text %)) 26) rows)))))

(defdescribe
  disclosures-retain-state-and-output-test
  (it "disclosures retain state and output"
      ;; Regression #189: independent folds survive patches and nested parent folds.
      (let [view
            (fixture)

            pane
            (-> (lv/opened view)
                (lv/expanded "details")
                (lv/expanded "a"))

            pane
            (lv/patched
              pane
              {:view-id (:id view) :seq 1 :ops [{:op :append :node-id "a" :lines ["A updated"]}]})]

        (expect (some #(= "A updated" (:text %)) (entries pane)))
        (expect (not-any? #(= "B started" (:text %)) (entries pane)))
        (expect (some #(= "A updated" (:text %))
                      (entries (-> pane
                                   (lv/expanded "details")
                                   (lv/expanded "details")))))
        (let [closed
              (lv/expanded pane "a")

              updated
              (lv/patched
                closed
                {:view-id (:id view) :seq 2 :ops [{:op :append :node-id "a" :lines ["later"]}]})]

          (expect (not-any? #(= :log (:kind %)) (entries updated)))
          (expect (some #(= "later" (:text %)) (entries (lv/expanded updated "a")))))
        (let [receipt (lv/settled pane {:reason :completed})]
          (expect (empty? (:disclosures receipt)))
          (expect (not-any? #(= :log (:kind %)) (entries receipt)))
          (expect (every? :is-disabled (filter #(= :button (:kind %)) (entries receipt))))))))

(defdescribe
  inputs-and-cards-test
  (it "paints every input with the form's field renderer and offers it as a control"
      (let [pane
            (lv/opened (fixture))

            rows
            (entries pane)

            edits
            (filter #(= :live-edit (:kind %)) (lv/controls [pane]))]

        (expect (= ["target" "token" "notes" "region" "tags" "notify" "risk" "due" "pin"]
                   (mapv :node-id edits)))
        (expect (= "Region" (:label (first (filter #(= "region" (:node-id %)) edits)))))
        (expect (some #(and (= :field (:kind %)) (= "EU" (:text %))) rows))
        (expect (some #(and (= :field (:kind %)) (str/includes? (:text %) "(0-10)")) rows))
        (expect (= {"target" "db-2"
                    "notes" "Ship after the checks pass."
                    "region" "eu"
                    "tags" ["api"]
                    "notify" true
                    "risk" 3
                    "due" "2026-03-14"}
                   (lv/input-values pane)))
        (expect (lv/submit-button? pane "apply"))
        (expect (not (lv/submit-button? pane "refresh")))
        (expect (= :multiselect (:type (lv/input-node pane "tags"))))
        (expect (nil? (lv/input-node pane "apply")))))
  (it "paints a card with its title, figure and text, and only a selectable card acts"
      (let [pane
            (lv/opened (fixture))

            cards
            (filter #(= :card (:kind %)) (entries pane))]

        (expect (some #(and (str/includes? (:text %) "Small") (str/includes? (:text %) "2 vCPU"))
                      cards))
        (expect (some #(str/includes? (:text %) "Six hosts, three regions") cards))
        (expect (= [["plan-small" "Small"]]
                   (->> (lv/controls [pane])
                        (filter #(= :live-activate (:kind %)))
                        (filter #(str/starts-with? (:node-id %) "plan-"))
                        (mapv (juxt :node-id :label)))))
        (expect (not-any? :is-selectable (entries (lv/settled pane {:reason :completed})))))))

(defn write-review!
  "Render the shared fixture with the production terminal painter for browser review."
  [path cols]
  (let [rows
        100

        pane
        (assoc (lv/opened (fixture)) :is-following false)]

    (with-open [terminal
                (-> (HtmlTerminal/builder)
                    (.initialSize (TerminalSize. cols rows))
                    (.defaultForeground theme/text-fg)
                    (.defaultBackground theme/terminal-bg)
                    (.title "Live view primitives")
                    (.build))

                screen
                (TerminalScreen. terminal)]

      (.startScreen screen)
      (.setCursorPosition screen nil)
      (doto (.newTextGraphics screen)
        (.setBackgroundColor theme/terminal-bg)
        (.setForegroundColor theme/text-fg)
        (.fill \space))
      (.beginFrame interactions/hit-map)
      (lv/paint! (.newTextGraphics screen) cols rows [pane] 1 3)
      (.commitFrame interactions/hit-map)
      (.refresh screen)
      (.writeHtml terminal (.toPath (io/file path)) (int rows))
      path)))

(defdescribe button-control-routes-through-the-shared-gateway-action-test
             (it "button control routes through the shared gateway action"
                 (let [pane
                       (lv/opened (fixture))

                       control
                       (first (filter #(= "refresh" (:node-id %)) (lv/controls [pane])))

                       called
                       (promise)]

                   (with-redefs [client/gateway-view-action! (fn [& args]
                                                               (deliver called args)
                                                               {:is-accepted true})]
                     (expect (true? (#'screen/activate-live-region! {:live-views [pane]} control)))
                     (expect (= [(get-in pane [:view :session-id]) (lv/view-id pane)
                                 {:action :activate :node-id "refresh"}]
                                (deref called 3000 ::timeout)))))))

(defdescribe human-record-retains-independent-disclosures-test
             (it "human record retains independent disclosures"
                 ;; Regression #189: restore the full human snapshot through the archive reader.
                 (let [raw
                       (json/read-json (slurp (io/resource
                                                "vis-contract/fixtures/live-primitives.json")))

                       source
                       (str/join "\n"
                                 (map json/write-json-str
                                      [{:kind "open" :view raw}
                                       {:kind "close"
                                        :at 20
                                        :result {:reason "completed"
                                                 :is_completed true
                                                 :view (select-keys raw ["title" "nodes"])}}]))

                       pane
                       (lv/recorded-pane source "fixture")

                       open
                       (-> pane
                           (lv/expanded "details")
                           (lv/expanded "a"))]

                   (expect (= #{"details" "canvas"} (set (map :node-id (lv/controls [pane])))))
                   (expect (not-any? #(= :log (:kind %)) (entries pane)))
                   (expect (some #(= "A started" (:text %)) (entries open)))
                   (expect (not-any? #(= "B started" (:text %)) (entries open)))
                   (expect (every? :is-disabled (filter #(= :button (:kind %)) (entries open))))
                   (expect (not (lv/animating? [open]))))))

(defdescribe logs-offer-read-only-search-controls-test
             (it "logs offer read only search controls"
                 (doseq [pane [(lv/opened (fixture))
                               (lv/reopened (lv/settled (lv/opened (fixture))
                                                        {:reason :completed}))]]
                   (let [opened (-> pane
                                    (lv/expanded "details")
                                    (lv/expanded "a"))
                         search (filter #(= :live-log-search (:kind %)) (lv/controls [opened]))]

                     (expect (= ["a"] (mapv :node-id search)))))))

(defdescribe
  log-search-renders-real-terminal-and-receipt-controls-test
  (it "log search renders real terminal and receipt controls"
      ;; Regression #189: search remains reachable in live output and reopened receipts.
      (doseq [cols
              [40 80 120]

              settled?
              [false true]]

        (let [view
              (update (fixture)
                      :nodes
                      #(filterv (fn [n]
                                  (= "details" (:id n)))
                         %))

              pane
              (cond-> (lv/opened view)
                settled?
                (lv/settled {:reason :completed})

                settled?
                lv/reopened)

              pane
              (-> pane
                  (lv/expanded "details")
                  (lv/expanded "a")
                  (assoc :is-following false))

              captured
              (capture/capture!
                {:cols cols
                 :rows 24
                 :paint! (fn [{:keys [screen]}]
                           (.beginFrame interactions/hit-map)
                           (lv/paint! (.newTextGraphics ^TerminalScreen screen) cols 24 [pane] 1 3)
                           (.commitFrame interactions/hit-map)
                           (.refresh ^TerminalScreen screen)
                           {:hits (vec (.current interactions/hit-map))
                            :button-bg [(.getRed theme/button-bg) (.getGreen theme/button-bg)
                                        (.getBlue theme/button-bg)]})})

              lines
              (str/split-lines (capture/frame-text captured))

              search
              (first (filter #(and (= :live-log-search (:kind %)) (= "a" (:node-id %)))
                             (get-in captured [:ret :hits])))

              disclosure
              (first (filter #(and (= :live-expand (:kind %)) (= "a" (:node-id %)))
                             (get-in captured [:ret :hits])))

              {:keys [row col width]}
              (:bounds search)]

          (expect (nil? (:error captured)))
          (expect (some? search))
          ;; #250: Search is a filled header action, not a full-width text row.
          (expect (= (get-in disclosure [:bounds :row]) row) "Search belongs to the log header")
          (expect (= 8 width) "only the padded Search button is clickable")
          (expect (not (str/includes? (capture/frame-text captured) "Search Build A logs")))
          (when search (expect (= " Search " (subs (nth lines row) col (+ col width)))))))))

(defdescribe
  inline-log-search-renders-results-empty-error-and-loading-test
  (it "inline log search renders results empty error and loading"
      ;; #235 moved retained-log search into the viewer; exercise its real frame.
      (doseq [cols
              [40 80 120]

              [response expected]
              [[{:page {"from" 0
                        "total" 503
                        "matched" 2
                        "lines" ["ERROR [disk] cache write failed" "error retry completed"]
                        "line_numbers" [7 9]}}
                ["7: ERROR [disk] cache write failed" "9: error retry completed"]]
               [{:page {"from" 0 "total" 503 "matched" 0 "lines" [] "line_numbers" []}}
                ["No matching lines"]] [{:error "Log unavailable"} ["Could not read log."]]
               [nil ["Searching…"]]]]

        (let [search
              (lv/log-search-requested (lv/log-search-opened "a") 0 "request")

              pane
              (assoc (lv/opened (fixture))
                :is-viewer true
                :log-search (if response (lv/log-search-loaded search "request" response) search))

              captured
              (capture/capture!
                {:cols cols
                 :rows 24
                 :paint! (fn [{:keys [screen]}]
                           (.beginFrame interactions/hit-map)
                           (lv/paint! (.newTextGraphics ^TerminalScreen screen) cols 24 [pane] 1 3)
                           (.commitFrame interactions/hit-map)
                           (.refresh ^TerminalScreen screen))})

              ;; Join wrapped rows without their frame edges or terminal padding.
              text
              (-> (capture/frame-text captured)
                  (str/replace "│" "")
                  (str/replace #"\s+" " "))]

          (expect (nil? (:error captured)))
          (doseq [copy expected]
            (expect (str/includes? text copy)))
          (expect (seq (:frames captured)))
          (expect (every? #(and (= 24 (count %))
                                (every? (fn [row]
                                          (= cols (count row)))
                                        %))
                          (:frames captured)))))))

;; #250: the query must not touch the left gutter, even while horizontally scrolled.
(defdescribe
  log-search-input-aligns-with-content-test
  (it
    "log search input aligns with content"
    (doseq [cols
            [24 40 80 120]

            query
            ["" "ERROR" (apply str (repeat 100 "x"))]]

      (let [pane
            (assoc (lv/opened (fixture))
              :is-viewer true
              :log-search (assoc (lv/log-search-opened "a")
                            :input (input/paste-text (input/empty-input) query)))

            captured
            (capture/capture!
              {:cols cols
               :rows 24
               :paint!
               (fn [{:keys [screen]}]
                 (let [painted
                       (lv/paint! (.newTextGraphics ^TerminalScreen screen) cols 24 [pane] 1 3)]
                   (.refresh ^TerminalScreen screen)
                   painted))})

            lines
            (str/split-lines (capture/frame-text captured))

            heading-col
            (some #(str/index-of % "Search") lines)

            ^TerminalPosition cursor
            (get-in captured [:ret :cursor])]

        (expect (nil? (:error captured)))
        (expect (some? cursor))
        (when cursor
          (let [line
                (nth lines (.getRow cursor))

                prompt-col
                (str/index-of line "> ")]

            (expect (= heading-col prompt-col)
                    "the prompt uses the same left padding as the heading")
            (expect (= \space (get line (dec (long heading-col)))) "leave the gutter clear")
            (expect (<= (+ (long heading-col) 2) (.getColumn cursor) (- cols 4)))
            (when (empty? query) (expect (= (+ (long heading-col) 2) (.getColumn cursor))))))))))

(defdescribe
  log-search-client-encodes-literal-query-test
  (it "log search client encodes literal query"
      ;; A Jenkins job is `folder/job`: the node id rides the query string, where an
      ;; encoded `/` is ordinary text instead of a path separator the gateway refuses.
      (let [called (atom nil)]
        (with-redefs-fn {#'client/send-json! (fn [& args]
                                               (reset! called args)
                                               {"matched" 0})}
          #(client/live-view-log "sid" "view" "jobs/glms#6064 · console" 200 200 "[disk]&Ł"))
        (expect (= ["GET"
                    (str "/v1/sessions/sid/views/live/view/log"
                         "?node=jobs%2Fglms%236064+%C2%B7+console"
                         "&from=200&limit=200&query=%5Bdisk%5D%26%C5%81")]
                   @called)))))

(defdescribe
  styled-log-wire-patches-and-receipts-test
  (it "styled log wire patches and receipts"
      ;; #209: whole-line severity never replaces literal text or changes line order.
      (let [view
            (hi/live-view<-wire {"id" "styled"
                                 "kind" "live"
                                 "version" 1
                                 "seq" 0
                                 "title" "Build output"
                                 "nodes" [{"id" "log"
                                           "type" "log"
                                           "lines" ["WARN before"]
                                           "line_tones" ["warn"]
                                           "window_lines" 2
                                           "total_lines" 1
                                           "default_expanded" true}]})

            pane
            (lv/patched
              (lv/opened view)
              (hi/live-patch<-wire
                {"view_id" "styled"
                 "seq" 1
                 "ops" [{"op" "append" "node_id" "log" "lines" ["ERROR compiler"] "tone" "error"}
                        {"op" "append" "node_id" "log" "lines" ["plain"]}]}))

            logs
            #(filterv (fn [row]
                        (= :log (:kind row)))
               (entries %))]

        (expect (= :warn (get-in view [:nodes 0 :line-tones 0])))
        (expect (= ["WARN before" "ERROR compiler" "plain"] (mapv :text (logs pane))))
        (expect (= [:warn :error nil] (mapv :tone (logs pane))))
        (expect (= (logs pane)
                   (logs (lv/expanded (lv/settled pane {:reason :completed}) "log")))))))

(defdescribe log-keeps-every-line-past-its-window-test
             (it "log keeps every line past its window"
                 ;; A window is what the producer holds hot, never a cut: the pane paints the whole
                 ;; log it was handed, with no "earlier lines" note standing in for dropped text.
                 (let [view
                       (hi/live-view<-wire {"id" "whole"
                                            "kind" "live"
                                            "version" 1
                                            "seq" 0
                                            "title" "Build output"
                                            "nodes" [{"id" "log"
                                                      "type" "log"
                                                      "lines" ["line 1"]
                                                      "window_lines" 2
                                                      "total_lines" 1
                                                      "default_expanded" true}]})

                       pane
                       (lv/patched (lv/opened view)
                                   (hi/live-patch<-wire {"view_id" "whole"
                                                         "seq" 1
                                                         "ops" [{"op" "append"
                                                                 "node_id" "log"
                                                                 "lines" ["line 2" "line 3"
                                                                          "line 4"]}]}))

                       rows
                       (entries pane)]

                   (expect (= ["line 1" "line 2" "line 3" "line 4"]
                              (mapv :text (filterv #(= :log (:kind %)) rows))))
                   (expect (not-any? #(str/includes? (str (:text %)) "earlier lines") rows)))))

(defdescribe
  log-reads-the-record-back-instead-of-counting-earlier-lines-test
  (it "log reads the record back instead of counting earlier lines"
      ;; Attaching mid-run is what the "earlier lines" note used to paper over: the pane
      ;; asks the record for the lines it was never sent, page by page to the first one,
      ;; and paints them in front of the window instead of counting them.
      (let [view
            (hi/live-view<-wire {"id" "late"
                                 "kind" "live"
                                 "version" 1
                                 "seq" 0
                                 "title" "Build output"
                                 "nodes" [{"id" "log"
                                           "type" "log"
                                           "lines" ["line 4499" "line 4500" "line 4501"]
                                           "line_tones" ["warn" "warn" "warn"]
                                           "window_lines" 100
                                           "total_lines" 4501
                                           "default_expanded" true}]})

            page
            (fn [from limit]
              {"lines" (mapv #(str "line " (+ (long from) (long %) 1)) (range limit))
               "line_tones" (vec (repeat limit "error"))})

            read-back
            (fn [pane]
              (loop [pane
                     pane

                     reads
                     []]

                (if-let [{:keys [node-id from limit]} (lv/log-fill-request pane)]
                  (let [request-id (random-uuid)]
                    (recur (lv/log-filled (lv/log-fill-requested pane node-id request-id)
                                          request-id
                                          {:page (page from limit)})
                           (conj reads [from limit])))
                  {:pane pane :reads reads})))

            {:keys [pane reads]}
            (read-back (lv/opened view))

            lines
            #(filterv (fn [row]
                        (= :log (:kind row)))
               (entries %))]

        (expect (= [[2498 2000] [498 2000] [0 498]] reads)
                "the newest missing page first, walking back to the first line")
        (expect (= 4501 (count (lines pane))))
        (expect (= ["line 1" "line 2"] (mapv :text (take 2 (lines pane)))))
        (expect (= ["line 4500" "line 4501"] (mapv :text (take-last 2 (lines pane)))))
        (expect (= [:error :error] (mapv :tone (take 2 (lines pane))))
                "a page read back keeps the tones it was written with")
        (expect (= [:warn :warn] (mapv :tone (take-last 2 (lines pane)))))
        (expect (not-any? #(str/includes? (str (:text %)) "earlier lines") (entries pane)))
        (expect (nil? (lv/log-fill-request pane)) "nothing left to read")
        ;; A page that arrives twice cannot duplicate the head.
        (expect (= 4501
                   (count (lines (lv/log-filled (lv/log-fill-requested pane "log" :again)
                                                :again
                                                {:page (page 0 498)})))))
        ;; While the walk is still running, the pane says what it is doing…
        (expect (some #(str/includes? (str (:text %)) "reading 4498 earlier lines")
                      (entries (lv/opened view))))
        ;; …and a refused read says THAT instead of promising lines nothing is fetching.
        (let [refused (lv/log-filled (lv/log-fill-requested (lv/opened view) "log" :read)
                                     :read
                                     {:error true})]
          (expect (some #(str/includes? (str (:text %)) "reading them failed") (entries refused)))
          (expect (nil? (lv/log-fill-request refused))
                  "a failed read waits for the next line of output")
          (expect (some? (lv/log-fill-request (lv/patched refused
                                                          (hi/live-patch<-wire
                                                            {"view_id" "late"
                                                             "seq" 1
                                                             "ops" [{"op" "append"
                                                                     "node_id" "log"
                                                                     "lines" ["line 4502"]}]}))))
                  "and new output retries it")))))

(defdescribe
  styled-log-narrow-terminal-test
  (it
    "styled log narrow terminal"
    (doseq [cols [24 40 80]]
      (let [view {:id "styled"
                  :title "Build output"
                  :seq 0
                  :nodes [{:id "log"
                           :type :log
                           :lines ["ERROR compiler"]
                           :line-tones [:error]
                           :total-lines 1
                           :window-lines 200
                           :default-expanded true}]}
            captured (capture/capture! {:cols cols
                                        :rows 16
                                        :paint! (fn [{:keys [screen]}]
                                                  (lv/paint! (.newTextGraphics ^TerminalScreen
                                                                               screen)
                                                             cols
                                                             16 [(lv/opened view)]
                                                             1 3))})]

        (expect (nil? (:error captured)))
        (let [frame (last (:frames captured))
              ;; #219: the disclosure inset leaves less room; preserve the visible error tone.
              ;; A narrow band wraps the line and follows its tail, so read the part on screen.
              line (first (filter #(some (fn [part]
                                           (str/includes? (apply str (map :ch %)) part))
                                         ["ERROR" "compil"])
                                  frame))
              start (some #(str/index-of (apply str (map :ch line)) %) ["ERROR" "compil"])
              color theme/footer-error-fg]

          (expect (some? start))
          (expect (= [(.getRed ^com.googlecode.lanterna.TextColor color)
                      (.getGreen ^com.googlecode.lanterna.TextColor color)
                      (.getBlue ^com.googlecode.lanterna.TextColor color)]
                     (:fg (get line start)))))))))

(defdescribe
  styled-log-theme-contrast-test
  (it
    "styled log theme contrast"
    ;; #209: a theme's accent is not necessarily readable as small log text.
    (let [original
          @theme/active-theme-id

          luminance
          (fn [rgb]
            (reduce +
                    0.0
                    (map (fn [c weight]
                           (let [v (/ (double c) 255.0)]
                             (* (double weight)
                                (if (<= v 0.04045)
                                  (/ v 12.92)
                                  (Math/pow (/ (+ v 0.055) 1.055) 2.4)))))
                         rgb
                         [0.2126 0.7152 0.0722])))]

      (try
        (doseq [id
                (shared-theme/available-theme-ids)

                tone
                [:idle :running :ok :warn :error]]

          (theme/apply-theme! id)
          (let [view
                {:id "contrast"
                 :title "Build output"
                 :seq 0
                 :nodes [{:id "log"
                          :type :log
                          :lines ["LEVEL output"]
                          :line-tones [tone]
                          :total-lines 1
                          :window-lines 200
                          :default-expanded true}]}

                captured
                (capture/capture! {:cols 40
                                   :rows 16
                                   :paint! (fn [{:keys [g]}]
                                             (lv/paint! g 40 16 [(lv/opened view)] 1 3))})

                line
                (first (filter #(str/includes? (apply str (map :ch %)) "LEVEL output")
                               (last (:frames captured))))

                start
                (str/index-of (apply str (map :ch line)) "LEVEL output")

                cell
                (get line start)

                foreground
                (double (luminance (:fg cell)))

                background
                (double (luminance (:bg cell)))]

            (expect (some? cell))
            (expect
              (>= (/ (+ (max foreground background) 0.05) (+ (min foreground background) 0.05)) 4.5)
              (str id " " tone))))
        (finally (theme/apply-theme! original))))))

(defdescribe divider-markdown-roundtrip-test
             (it "divider markdown roundtrip"
                 (let [view
                       (assoc (fixture) :nodes [{:id "section-break" :type :divider}])

                       markdown
                       (materializer/->markdown view)

                       parsed
                       (:view (materializer/parse-markdown markdown))]

                   (expect (str/includes? markdown "<!-- vis:divider 1 -->\n---"))
                   (expect (= [:divider] (mapv :type (:nodes parsed))))
                   (expect (= markdown (materializer/->markdown parsed)))
                   (doseq [invalid ["<!-- vis:divider 1 -->\ntext"
                                    "<!-- vis:divider 2 -->\n---\n---"]]
                     (expect (try (materializer/parse-markdown
                                    (str/replace markdown "<!-- vis:divider 1 -->\n---" invalid))
                                  false
                                  (catch clojure.lang.ExceptionInfo _ true)))))))

(def ^:private app-view
  {:id "app"
   :title "Review"
   :seq 1
   :placement :attachment
   :nodes [{:id "d"
            :type :diff
            :patch "--- a/x\n+++ b/x\n@@ -1 +1 @@\n-a\n+b\n"
            :new-revision "r2"
            :threads [{:id "t1" :anchor {:path "x" :side "new" :line 1} :messages [] :draft "d"}]}
           {:id "th"
            :type :thread
            :label "Notes"
            :messages [{:id "m" :author {:kind "agent"} :body "hi\nthere" :created-at 1}]}]})

(defdescribe
  app-ops-test
  (it "adds and replaces a diff thread by id"
      (let [thread
            {:id "t2"
             :anchor {:path "x" :side "old" :line 1}
             :messages [{:id "m2" :author {:kind "human"} :body "q" :created-at 2}]}

            added
            (materializer/apply-patch
              app-view
              {:view-id "app" :seq 2 :ops [{:op :thread :node-id "d" :thread thread}]})

            replaced
            (materializer/apply-patch
              added
              {:view-id "app"
               :seq 3
               :ops [{:op :thread :node-id "d" :thread (assoc thread :draft "more")}]})]

        (expect (= ["t1" "t2"] (mapv :id (get-in added [:nodes 0 :threads]))))
        (expect (= "more" (get-in replaced [:nodes 0 :threads 1 :draft])))))
  (it "removes diff threads by id"
      (let [removed (materializer/apply-patch
                      app-view
                      {:view-id "app" :seq 2 :ops [{:op :remove :node-id "d" :item-ids ["t1"]}]})]
        (expect (= [] (vec (get-in removed [:nodes 0 :threads]))))))
  (it "keeps one receipt per submission id with its newest stage"
      (let [receipt
            {:id "s" :idempotency-key "tui-12345678" :status "accepted" :created-at 1 :updated-at 1}

            patched
            (materializer/apply-patch app-view
                                      {:view-id "app"
                                       :seq 2
                                       :ops [{:op :submission :submission receipt}
                                             {:op :submission
                                              :submission (assoc receipt
                                                            :status "answered"
                                                            :updated-at 2)}]})]

        (expect (= [["s" "answered"]] (mapv (juxt :id :status) (:submissions patched))))))
  (it "refuses a diff thread without an anchor"
      (expect (try (materializer/apply-patch
                     app-view
                     {:view-id "app"
                      :seq 2
                      :ops [{:op :thread :node-id "d" :thread {:id "t9" :messages []}}]})
                   false
                   (catch clojure.lang.ExceptionInfo _ true))))
  (it "keeps the note for the model of an app and removes it with a blank text"
      (let [noted
            (materializer/apply-patch
              app-view
              {:view-id "app" :seq 2 :ops [{:op :agent-note :text " Row 2 "}]})

            cleared
            (materializer/apply-patch noted
                                      {:view-id "app" :seq 3 :ops [{:op :agent-note :text " "}]})]

        (expect (= "Row 2" (:agent-note noted)))
        (expect (not (contains? cleared :agent-note)))
        (expect (try (materializer/apply-patch
                       (assoc app-view :placement :inline)
                       {:view-id "app" :seq 2 :ops [{:op :agent-note :text "Row 2"}]})
                     false
                     (catch clojure.lang.ExceptionInfo _ true)))))
  (it "reads a diff and a thread back from markdown"
      (let [markdown
            (materializer/->markdown app-view)

            parsed
            (:view (materializer/parse-markdown markdown))]

        (expect (str/includes? markdown "```diff\n--- a/x"))
        (expect (str/includes? markdown "> agent: hi there"))
        (expect (= (mapv #(dissoc % :id) (:nodes app-view))
                   (mapv #(dissoc % :id) (:nodes parsed)))))))

(defdescribe
  scene-patch-test
  (it "merges a scene object change onto the object, so a tick keeps what it leaves out"
      (let [view
            (fixture)

            moved
            (materializer/apply-patch view
                                      {:view-id (:id view)
                                       :seq 1
                                       :ops [{:op :append
                                              :node-id "scene"
                                              :objects [{:id "ship" :x 3}
                                                        {:id "new" :mesh "box"}]}]})

            objects
            (->> (:nodes moved)
                 (filter #(= "scene" (:id %)))
                 first
                 :objects
                 (into {} (map (juxt :id identity))))]

        (expect (= [3 "cone" "#7fd4ff"] ((juxt :x :mesh :color) (get objects "ship"))))
        (expect (= ["box" "#cccccc" 1] ((juxt :mesh :color :scale) (get objects "new"))))))
  (it "refuses a scene object that names an unknown mesh"
      (let [view (fixture)]
        (expect (try (materializer/apply-patch
                       view
                       {:view-id (:id view)
                        :seq 1
                        :ops [{:op :append :node-id "scene" :objects [{:id "x" :mesh "nope"}]}]})
                     false
                     (catch clojure.lang.ExceptionInfo _ true))))))

(defdescribe scrollbar-belongs-to-the-open-log-test
             (it "scrollbar belongs to the open log"
                 ;; The band's scrollbar starts on the log's own row — the one carrying Search —
                 ;; and measures the log alone, never the prose and status rows above it.
                 (let [rows [{:node-id "p"} {:node-id "q"} {:node-id "a" :search-label "Search A"}
                             {:node-id "a"} {:node-id "a"} {:node-id "a"} {:node-id "b"}]]
                   (expect (nil? (lv/log-span [{:node-id "p"} {:node-id "q"}])))
                   (expect (= [2 6] (lv/log-span rows)))
                   ;; Four body rows from the top, the body at row 10: the bar starts two rows
                   ;; down and tracks the two log rows on screen out of four.
                   (expect (= {:row 12 :track 2 :total 4 :start 0} (#'lv/bar-shape rows 0 4 10)))
                   ;; Scrolled one row into the log: the bar starts at the body top.
                   (expect (= {:row 10 :track 3 :total 4 :start 1} (#'lv/bar-shape rows 3 3 10)))
                   ;; The whole log fits: no bar at all.
                   (expect (nil? (#'lv/bar-shape rows 0 7 10)))
                   ;; Without an open log the body itself is the scrolled thing.
                   (expect (= {:row 10 :track 1 :total 2 :start 0}
                              (#'lv/bar-shape [{:node-id "p"} {:node-id "q"}] 0 1 10))))))

(defn- timeline-node
  "The shared fixture's timeline node: one ended lane and one running lane."
  []
  (some #(when (= "timeline" (:id %)) %)
        (tree-seq #(seq (or (:children %) (:nodes %))) #(or (:children %) (:nodes %)) (fixture))))

(def ^:private t0 "The fixture's first lane start." 1767225600000)

(defdescribe
  timeline-markdown-test
  (it "prints each lane with UTC times and reads it back with the running lane still open"
      (let [view
            (assoc (fixture) :nodes [(timeline-node)])

            markdown
            (materializer/->markdown view)

            parsed
            (first (:nodes (:view (materializer/parse-markdown markdown))))]

        (expect (str/includes? markdown
                               "- [running] Run tests — 12 of 40 · 2026-01-01T00:00:20.000Z → now"))
        (expect (= markdown (materializer/->markdown (assoc view :nodes [parsed]))))
        (expect (= (mapv #(dissoc % :id) (:lanes (timeline-node)))
                   (mapv #(dissoc % :id) (:lanes parsed))))))
  (it "reads an empty timeline back as a timeline, not as empty steps"
      (let [view (assoc (fixture) :nodes [(assoc (timeline-node) :lanes [])])]
        (expect (= [:timeline []]
                   ((juxt :type :lanes)
                     (first (:nodes (:view (materializer/parse-markdown (materializer/->markdown
                                                                          view)))))))))))

(defdescribe
  timeline-append-test
  (let [view
        (assoc (fixture) :nodes [(timeline-node)])

        append
        (fn [lanes]
          (:lanes (first (:nodes (materializer/apply-patch
                                   view
                                   {:view-id (:id view)
                                    :seq (inc (long (:seq view)))
                                    :ops [{:op :append :node-id "timeline" :lanes lanes}]})))))]

    (it "replaces a lane by id in its slot and appends a new one"
        (expect (= [["build" :error] ["test" :running] ["new" :idle]]
                   (mapv (juxt :id :tone)
                         (append
                           [{:id "build" :label "Build" :tone :error :start t0 :end (+ t0 1000)}
                            {:id "new" :label "New" :tone :idle :start t0}])))))
    (it "refuses a lane that ends before it starts"
        (expect (= "node timeline /lanes/2 - the lane ends before it starts (lane)"
                   (try (append [{:id "bad" :label "Bad" :tone :ok :start (+ t0 10) :end t0}])
                        nil
                        (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))

(defdescribe
  handed-fault-test
  (it "takes a patch that makes a fault from the engine smaller"
      ;; The engine owns a fault that it hands over. A patch that removes one of 40
      ;; stats (the limit is 32) changes only the count of the fault, so it is no new fault.
      (let [stats
            (mapv #(hash-map :id (str "s" %) :label "L" :value-text "1" :tone :ok) (range 40))

            view
            (assoc (fixture) :nodes [{:id "st" :type :stat :label "Counts" :stats stats}])

            patched
            (materializer/apply-patch view
                                      {:view-id (:id view)
                                       :seq (inc (long (:seq view)))
                                       :ops [{:op :remove :node-id "st" :item-ids ["s0"]}]})]

        (expect (= 39 (count (:stats (first (:nodes patched)))))))))

(defdescribe timeline-geometry-test
             (let [[ended running]
                   (:lanes (timeline-node))

                   now
                   (+ t0 100000)

                   scale
                   (lv/lane-span [ended running] now)]

               (it "runs the shared scale from the first start to the clock while a lane runs"
                   (expect (= [t0 now] scale))
                   (expect (= [t0 (+ t0 80000)] (lv/lane-span [ended] now))))
               (it "places an ended lane at its own times and a running lane up to the clock"
                   (expect (= [0 40] (lv/lane-cells ended scale now 50)))
                   (expect (= [10 50] (lv/lane-cells running scale now 50))))
               (it "keeps one cell for a lane that took no time"
                   (expect (= [49 50] (lv/lane-cells {:start now :end now} scale now 50)))))
             (it "marks the axis on the least round step that gives fewer than five marks"
                 (expect (= [0 30000 60000 90000] (lv/axis-marks 100000)))
                 (expect (= [0 10000 20000 30000 40000] (lv/axis-marks 45000)))))

(defdescribe
  timeline-rows-test
  (it "never paints a row wider than the band"
      (doseq [w
              [80 40 20 12]

              :let [pane
                    (lv/opened (assoc (fixture) :nodes [(timeline-node)]))]
              row
              (lv/plan pane w (+ t0 100000))]

        (expect (<= (long (p/display-width (str (:text row)))) w) (str w " " (:text row)))))
  (it "draws a settled timeline to its last end, not to the clock"
      (let [pane
            (lv/settled (lv/opened (assoc (fixture) :nodes [(timeline-node)])) {:reason :completed})

            bar
            (fn [later]
              (some #(when (= "test" (:item-id %)) (:text %)) (lv/plan pane 80 later)))]

        (expect (= (bar (+ t0 100000)) (bar (+ t0 900000))))))
  (it "ticks only an open pane with a running lane"
      (let [open (lv/opened (assoc (fixture) :nodes [(timeline-node)]))]
        (expect (lv/ticking? [open]))
        (expect (not (lv/ticking? [(lv/settled open {:reason :completed})])))
        (expect (not (lv/ticking? [(lv/opened (assoc (fixture)
                                                :nodes [(update (timeline-node)
                                                                :lanes
                                                                #(vec (take 1 %)))]))]))))))

(defdescribe divider-orientation-test
             (it "stands a divider upright inside a wrap group and lays it flat elsewhere"
                 (let [view
                       (assoc (fixture)
                         :nodes [{:id "g"
                                  :type :group
                                  :direction :wrap
                                  :children [{:id "a" :type :paragraph :text "TEAM"}
                                             {:id "d" :type :divider}
                                             {:id "b" :type :paragraph :text "1m 20s"}]}
                                 {:id "flat" :type :divider}])

                       rows
                       (lv/plan (lv/opened view) 40)

                       cells
                       (tree-seq #(seq (:cells %)) :cells {:cells rows})]

                   (expect (= "│" (some #(when (= "d" (:node-id %)) (:text %)) cells)))
                   (expect (= 40 (count (some #(when (= "flat" (:node-id %)) (:text %)) rows)))))))
