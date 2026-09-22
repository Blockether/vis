(ns com.blockether.vis.tui.live-primitives-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.view :as spec]
            [com.blockether.vis.tui.capture :as capture]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.human-input :as hi]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.live-view :as lv]
            [com.blockether.vis.tui.view-materializer :as materializer]
            [com.blockether.vis.tui.theme :as theme]
            [com.blockether.vis.tui.shared-theme :as shared-theme]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.html HtmlTerminal]))

(defn fixture
  []
  (hi/live-view<-wire (json/read-json (slurp (io/resource
                                               "vis-contract/fixtures/live-primitives.json")))))

(defn- entries [pane] (tree-seq #(seq (:cells %)) :cells {:cells (lv/plan pane 80)}))

(deftest all-primitives-paint-plan-test
  (let [view
        (fixture)

        pane
        (lv/opened view)

        rows
        (entries pane)]

    (is (nil? (spec/live-view-error view)))
    (is (= 6 (count (set (map :node-id (filter #(= :heading (:kind %)) rows))))))
    (is (= 4 (count (filter #(= :spinner (:kind %)) rows))))
    (is (str/includes? (str/join "\n" (map :text rows)) "# <button> is literal text"))
    (is (lv/animating? [pane]))
    (is (= #{"refresh" "details" "table"} (set (map :node-id (lv/controls [pane])))))
    (is (not (lv/animating? [(lv/settled pane {:reason :completed})])))
    (is (not (lv/animating? [(lv/minimized pane)])))
    (is (str/includes? (materializer/->markdown view) "Heading level 6"))
    (let [markdown (materializer/->markdown view)]
      (is (= markdown (materializer/->markdown (:view (materializer/parse-markdown markdown))))))))

(deftest disclosures-retain-state-and-output-test
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

    (is (some #(= "A updated" (:text %)) (entries pane)))
    (is (not-any? #(= "B started" (:text %)) (entries pane)))
    (is (some #(= "A updated" (:text %))
              (entries (-> pane
                           (lv/expanded "details")
                           (lv/expanded "details")))))
    (let [closed
          (lv/expanded pane "a")

          updated
          (lv/patched
            closed
            {:view-id (:id view) :seq 2 :ops [{:op :append :node-id "a" :lines ["later"]}]})]

      (is (not-any? #(= :log (:kind %)) (entries updated)))
      (is (some #(= "later" (:text %)) (entries (lv/expanded updated "a")))))
    (let [receipt (lv/settled pane {:reason :completed})]
      (is (empty? (:disclosures receipt)))
      (is (not-any? #(= :log (:kind %)) (entries receipt)))
      (is (every? :is-disabled (filter #(= :button (:kind %)) (entries receipt)))))))

(deftest primitives-paint-real-terminal-test
  (doseq [cols [40 80 120]]
    (let [view (update (fixture)
                       :nodes
                       #(filterv (fn [n]
                                   (contains? #{"h1" "code" "spinners" "actions" "details" "links"}
                                              (:id n)))
                          %))
          pane (assoc (lv/opened view) :is-following false)
          captured (capture/capture!
                     {:cols cols
                      :rows 48
                      :paint!
                      (fn [{:keys [screen]}]
                        (.beginFrame interactions/hit-map)
                        (lv/paint! (.newTextGraphics ^TerminalScreen screen) cols 48 [pane] 1 3)
                        (.commitFrame interactions/hit-map)
                        (.refresh ^TerminalScreen screen)
                        (vec (.current interactions/hit-map)))})
          text (capture/frame-text (last (:frames captured)))]

      (is (str/includes? text "Heading level 1"))
      (is (str/includes? text "Refresh results"))
      (is (str/includes? text "Details"))
      (is (str/includes? text "F3"))
      (is (str/includes? text "Braille"))
      (is (str/includes? text "Documentation"))
      (is (some #(= :url (:kind %)) (:ret captured)))
      (is (some #(= :live-activate (:kind %)) (:ret captured)))
      (is (not-any? #(= "disabled" (:node-id %)) (:ret captured))))))

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

(deftest button-control-routes-through-the-shared-gateway-action-test
  (let [pane
        (lv/opened (fixture))

        control
        (first (filter #(= "refresh" (:node-id %)) (lv/controls [pane])))

        called
        (promise)]

    (with-redefs [client/gateway-view-action! (fn [& args]
                                                (deliver called args)
                                                {:is-accepted true})]
      (is (true? (#'screen/activate-live-region! {:live-views [pane]} control)))
      (is (= [(get-in pane [:view :session-id]) (lv/view-id pane)
              {:action :activate :node-id "refresh"}]
             (deref called 3000 ::timeout))))))

(deftest human-record-retains-independent-disclosures-test
  ;; Regression #189: restore the full human snapshot through the archive reader.
  (let [raw
        (json/read-json (slurp (io/resource "vis-contract/fixtures/live-primitives.json")))

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

    (is (= #{"details"} (set (map :node-id (lv/controls [pane])))))
    (is (not-any? #(= :log (:kind %)) (entries pane)))
    (is (some #(= "A started" (:text %)) (entries open)))
    (is (not-any? #(= "B started" (:text %)) (entries open)))
    (is (every? :is-disabled (filter #(= :button (:kind %)) (entries open))))
    (is (not (lv/animating? [open])))))

(deftest logs-offer-read-only-search-controls-test
  (doseq [pane [(lv/opened (fixture))
                (lv/reopened (lv/settled (lv/opened (fixture)) {:reason :completed}))]]
    (let [opened (-> pane
                     (lv/expanded "details")
                     (lv/expanded "a"))
          search (filter #(= :live-log-search (:kind %)) (lv/controls [opened]))]

      (is (= ["a"] (mapv :node-id search)))
      (is (= "Search Build A logs" (:label (first search)))))))

(deftest log-search-renders-real-terminal-and-receipt-controls-test
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

      (is (nil? (:error captured)))
      (is (some? search))
      ;; #250: Search is a filled header action, not a full-width text row.
      (is (= (get-in disclosure [:bounds :row]) row) "Search belongs to the log header")
      (is (= 8 width) "only the padded Search button is clickable")
      (is (not (str/includes? (capture/frame-text captured) "Search Build A logs")))
      (when search
        (is (= " Search " (subs (nth lines row) col (+ col width))))
        (is (= (get-in captured [:ret :button-bg])
               (:bg (get-in (last (:frames captured)) [row col]))))))))

(deftest inline-log-search-renders-results-empty-error-and-loading-test
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

      (is (nil? (:error captured)))
      (is (str/includes? text "Search Build A logs"))
      (doseq [copy expected]
        (is (str/includes? text copy)))
      (when (and (>= cols 80) (:page response))
        (is (str/includes? text (str (get (:page response) "matched") " matches / 503 lines"))))
      (when (and (>= cols 80) (:error response)) (is (str/includes? text "Enter to retry.")))
      (is (seq (:frames captured)))
      (is (every? #(and (= 24 (count %))
                        (every? (fn [row]
                                  (= cols (count row)))
                                %))
                  (:frames captured))))))

;; #250: the query must not touch the left gutter, even while horizontally scrolled.
(deftest log-search-input-aligns-with-content-test
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
          (some #(str/index-of % "Search Build") lines)

          ^TerminalPosition cursor
          (get-in captured [:ret :cursor])]

      (is (nil? (:error captured)))
      (is (some? cursor))
      (when cursor
        (let [line
              (nth lines (.getRow cursor))

              prompt-col
              (str/index-of line "› ")]

          (is (= heading-col prompt-col) "the prompt uses the same left padding as the heading")
          (is (= \space (get line (dec (long heading-col)))) "leave the gutter clear")
          (is (<= (+ (long heading-col) 2) (.getColumn cursor) (- cols 4)))
          (when (empty? query) (is (= (+ (long heading-col) 2) (.getColumn cursor)))))))))

(deftest log-search-client-encodes-literal-query-test
  ;; A Jenkins job is `folder/job`: the node id rides the query string, where an
  ;; encoded `/` is ordinary text instead of a path separator the gateway refuses.
  (let [called (atom nil)]
    (with-redefs-fn {#'client/send-json! (fn [& args]
                                           (reset! called args)
                                           {"matched" 0})}
      #(client/live-view-log "sid" "view" "jobs/glms#6064 · console" 200 200 "[disk]&Ł"))
    (is (= ["GET"
            (str "/v1/sessions/sid/views/live/view/log"
                 "?node=jobs%2Fglms%236064+%C2%B7+console"
                 "&from=200&limit=200&query=%5Bdisk%5D%26%C5%81")]
           @called))))

(deftest styled-log-wire-patches-and-receipts-test
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
        (lv/patched (lv/opened view)
                    (hi/live-patch<-wire
                      {"view_id" "styled"
                       "seq" 1
                       "ops"
                       [{"op" "append" "node_id" "log" "lines" ["ERROR compiler"] "tone" "error"}
                        {"op" "append" "node_id" "log" "lines" ["plain"]}]}))

        logs
        #(filterv (fn [row]
                    (= :log (:kind row)))
           (entries %))]

    (is (= :warn (get-in view [:nodes 0 :line-tones 0])))
    (is (= ["WARN before" "ERROR compiler" "plain"] (mapv :text (logs pane))))
    (is (= [:warn :error nil] (mapv :tone (logs pane))))
    (is (= (logs pane) (logs (lv/expanded (lv/settled pane {:reason :completed}) "log"))))))

(deftest log-keeps-every-line-past-its-window-test
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
                                                  "lines" ["line 2" "line 3" "line 4"]}]}))

        rows
        (entries pane)]

    (is (= ["line 1" "line 2" "line 3" "line 4"] (mapv :text (filterv #(= :log (:kind %)) rows))))
    (is (not-any? #(str/includes? (str (:text %)) "earlier lines") rows))))

(deftest styled-log-narrow-terminal-test
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
                                                (lv/paint! (.newTextGraphics ^TerminalScreen screen)
                                                           cols
                                                           16 [(lv/opened view)]
                                                           1 3))})]

      (is (nil? (:error captured)))
      (let [frame (last (:frames captured))
            ;; #219: the disclosure inset leaves less room; preserve the visible error tone.
            line (first (filter #(str/includes? (apply str (map :ch %)) "ERROR") frame))
            start (str/index-of (apply str (map :ch line)) "ERROR")
            color theme/footer-error-fg]

        (is (some? start))
        (is (= [(.getRed ^com.googlecode.lanterna.TextColor color)
                (.getGreen ^com.googlecode.lanterna.TextColor color)
                (.getBlue ^com.googlecode.lanterna.TextColor color)]
               (:fg (get line start))))))))

(deftest styled-log-theme-contrast-test
  ;; #209: a theme's accent is not necessarily readable as small log text.
  (let [original
        @theme/active-theme-id

        luminance
        (fn [rgb]
          (reduce +
                  0.0
                  (map (fn [c weight]
                         (let [v (/ (double c) 255.0)]
                           (*
                             (double weight)
                             (if (<= v 0.04045) (/ v 12.92) (Math/pow (/ (+ v 0.055) 1.055) 2.4)))))
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

          (is (some? cell))
          (is (>= (/ (+ (max foreground background) 0.05) (+ (min foreground background) 0.05)) 4.5)
              (str id " " tone))))
      (finally (theme/apply-theme! original)))))

(deftest divider-markdown-roundtrip-test
  (let [view
        (assoc (fixture) :nodes [{:id "section-break" :type :divider}])

        markdown
        (materializer/->markdown view)

        parsed
        (:view (materializer/parse-markdown markdown))]

    (is (str/includes? markdown "<!-- vis:divider 1 -->\n---"))
    (is (= [:divider] (mapv :type (:nodes parsed))))
    (is (= markdown (materializer/->markdown parsed)))
    (doseq [invalid ["<!-- vis:divider 1 -->\ntext" "<!-- vis:divider 2 -->\n---\n---"]]
      (is (try (materializer/parse-markdown
                 (str/replace markdown "<!-- vis:divider 1 -->\n---" invalid))
               false
               (catch clojure.lang.ExceptionInfo _ true))))))

(deftest scrollbar-belongs-to-the-open-log-test
  ;; The band's scrollbar starts on the log's own row — the one carrying Search —
  ;; and measures the log alone, never the prose and status rows above it.
  (let [rows [{:node-id "p"} {:node-id "q"} {:node-id "a" :search-label "Search A"} {:node-id "a"}
              {:node-id "a"} {:node-id "a"} {:node-id "b"}]]
    (is (nil? (lv/log-span [{:node-id "p"} {:node-id "q"}])))
    (is (= [2 6] (lv/log-span rows)))
    ;; Four body rows from the top, the body at row 10: the bar starts two rows
    ;; down and tracks the two log rows on screen out of four.
    (is (= {:row 12 :track 2 :total 4 :start 0} (#'lv/bar-shape rows 0 4 10)))
    ;; Scrolled one row into the log: the bar starts at the body top.
    (is (= {:row 10 :track 3 :total 4 :start 1} (#'lv/bar-shape rows 3 3 10)))
    ;; The whole log fits: no bar at all.
    (is (nil? (#'lv/bar-shape rows 0 7 10)))
    ;; Without an open log the body itself is the scrolled thing.
    (is (= {:row 10 :track 1 :total 2 :start 0}
           (#'lv/bar-shape [{:node-id "p"} {:node-id "q"}] 0 1 10)))))
