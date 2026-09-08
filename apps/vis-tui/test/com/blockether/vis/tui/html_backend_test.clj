(ns com.blockether.vis.tui.html-backend-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.frame :as frame]
            [com.blockether.vis.tui.header :as header]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.scroll :as scroll]
            [com.blockether.vis.tui.terminal-image :as timg]
            [com.blockether.vis.tui.theme :as theme]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.gui2 Button GridLayout Panel TextGraphicsComponent]
           [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.html HtmlMedia$Kind HtmlTerminal HtmlTerminalView]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(defn- cell-grid
  [^DefaultVirtualTerminal terminal cols rows]
  (mapv (fn [row]
          (mapv (fn [col]
                  (.getCharacter terminal (TerminalPosition. (int col) (int row))))
                (range cols)))
        (range rows)))

(defn activity-review-rows
  "Deterministic execution fixture for live HTML and native grid review."
  [state]
  (mapv (fn [i operation]
          (cond-> {:id (str "call-" i)
                   :sequence i
                   :operation operation
                   :presenter "generic"
                   :signal "generic"
                   :state "succeeded"
                   :summary (if (= operation "shell") "cmd: npm test" (str "src/file-" i ".clj"))
                   :resources
                   (if (= operation "shell") [] [{:type "file" :id (str "src/file-" i ".clj")}])
                   :evidence (if (= operation "patch")
                               [{:kind "diff"
                                 :text (str "src/file-" i ".clj")
                                 :additions 2
                                 :deletions 1
                                 :lines [{:kind "addition" :text "updated"}]}]
                               [])}
            (= i 6)
            (assoc :state state)

            (and (= i 6) (= state "failed"))
            (assoc :error-summary "Test suite failed")))
        (range 7)
        ["cat" "cat" "cat" "patch" "patch" "shell" "shell"]))

(defn activity-review-terminal
  "Configure review defaults to match the actual TUI theme, including empty cells."
  ^HtmlTerminal [cols rows]
  (doto (-> (HtmlTerminal/builder)
            (.initialSize (TerminalSize. cols rows))
            (.defaultForeground theme/text-fg)
            (.defaultBackground theme/terminal-bg)
            (.title "Activity review")
            (.build))
    (.setCursorVisible false)))

(defn paint-activity-review!
  "Paint the production joined execution surface; no copied browser layout."
  [^TerminalScreen screen rows expansions]
  (.doResizeIfNecessary screen)
  (.clear screen)
  (let [cols
        (.getColumns (.getTerminalSize screen))

        height
        (.getRows (.getTerminalSize screen))

        projection
        {:state "running"
         :rows rows
         :counts (merge {:running 0 :succeeded 0 :failed 0 :cancelled 0}
                        (frequencies (map (comp keyword :state) rows)))
         :omitted {:rows 0 :by-classification {}}}

        rendered
        (render/format-answer-with-thinking-data*
          ""
          [{:thinking "Inspect files, apply the patch, then run tests."
            :forms [{:code "inspect_files()\napply_patch()\nrun_checks()"
                     :stdout "Read 3 files.\nUpdated 2 files."
                     :duration-ms 1200
                     :activity projection}]}]
          (- cols 4)
          {:show-thinking true :show-iterations true}
          nil
          false
          {:session-id "activity-review" :detail-expansions expansions})]

    (doto (.newTextGraphics screen)
      (.setBackgroundColor theme/terminal-bg)
      (.setForegroundColor theme/text-fg)
      (.fill \space))
    (.beginFrame interactions/hit-map)
    (let [consumed (render/draw-chat-bubble! (.newTextGraphics screen)
                                             {:role :assistant
                                              :text ""
                                              :prewrapped-lines (:lines rendered)
                                              :line-meta (:line-meta rendered)}
                                             2
                                             1
                                             (- cols 4)
                                             {:viewport-h (- height 2)})]
      (.commitFrame interactions/hit-map)
      (.refresh screen)
      (assoc rendered :review-rows (min height (+ 2 consumed))))))

(defn toggle-review-region
  "Use the same pure detail event that mouse and keyboard dispatch in the TUI."
  [expansions region]
  (let [handler (:fn (get (deref (deref #'state/event-registry)) :toggle-detail))]
    (:detail-expansions (handler {:detail-expansions expansions :scroll scroll/follow}
                                 [:toggle-detail (:session-id region) (:node-id region)
                                  (:collapsed? region)]))))

(deftest nested-result-execution-disclosures-test
  ;; Result stays inside Code; Activity remains independent through every fold state.
  (doseq [cols
          [40 80 160]

          status
          ["running" "succeeded" "failed"]]

    (with-open [html
                (activity-review-terminal cols 50)

                hs
                (doto (TerminalScreen. html) (.startScreen))

                terminal
                (DefaultVirtualTerminal. (TerminalSize. cols 50))

                ts
                (doto (TerminalScreen. terminal) (.startScreen))]

      (let [rows
            (activity-review-rows status)

            _
            (paint-activity-review! hs rows {})

            code-region
            (first (filter #(str/ends-with? (str (:node-id %)) ":code")
                           (.current interactions/hit-map)))

            _
            (paint-activity-review! hs rows (toggle-review-region {} code-region))

            regions
            (into [code-region]
                  (map (fn [suffix]
                         (first (filter #(str/ends-with? (str (:node-id %)) suffix)
                                        (.current interactions/hit-map))))
                       [":result" ":#band"]))]

        (is (every? some? regions))
        (doseq [result-open?
                [false true]

                code-open?
                [false true]

                activity-open?
                [false true]]

          (let [open-states
                [code-open? result-open? activity-open?]

                expansions
                (reduce (fn [folds [region open?]]
                          (if open? (toggle-review-region folds region) folds))
                        {}
                        (map vector regions open-states))

                _
                (paint-activity-review! hs rows expansions)

                _
                (paint-activity-review! ts rows expansions)

                grid
                (cell-grid terminal cols 50)

                lines
                (mapv (fn [row]
                        (apply str
                          (map #(.getCharacterString ^com.googlecode.lanterna.TextCharacter %)
                               row)))
                      grid)

                row-of
                (fn [label]
                  (first (keep-indexed #(when (str/includes? %2 label) %1) lines)))

                labels
                (if code-open? ["CODE" "RESULT" "ACTIVITY"] ["CODE" "ACTIVITY"])

                positions
                (mapv row-of labels)

                text
                (str/join "\n" lines)]

            (is (= (cell-grid html cols 50) grid))
            (is (every? some? positions))
            (is (apply < positions))
            (is (not-any? #(str/starts-with? % " │") lines))
            (is (= (and code-open? result-open?) (str/includes? text "Read 3 files.")))
            (is (= code-open? (some? (row-of "RESULT"))))
            (is (= code-open? (str/includes? text "inspect_files()")))
            (is (= activity-open? (str/includes? text "Read ×3")))
            (doseq [[label row open?] (map vector
                                           labels
                                           positions
                                           (if code-open? open-states [code-open? activity-open?]))]
              (let [col (str/index-of (nth lines row) label)
                    hit (.lookup interactions/hit-map (TerminalPosition. (int col) (int row)))]

                (is (= :toggle-details (:kind hit)))
                (is (= (not open?) (:collapsed? hit)))))))))))

(deftest joined-activity-html-native-parity-test
  (doseq [cols
          [40 80 120]

          status
          ["running" "succeeded" "failed"]]

    (let [size
          (TerminalSize. cols 50)

          rows
          (activity-review-rows status)]

      (with-open [html
                  (activity-review-terminal cols 50)

                  terminal
                  (DefaultVirtualTerminal. size)

                  hs
                  (doto (TerminalScreen. html) (.startScreen))

                  ts
                  (doto (TerminalScreen. terminal) (.startScreen))]

        (paint-activity-review! hs rows {})
        (let [band
              (first (filter #(str/ends-with? (str (:node-id %)) ":#band")
                             (.current interactions/hit-map)))

              opened-band
              (toggle-review-region {} band)

              _
              (paint-activity-review! hs rows opened-band)

              read-group
              (first (filter #(str/ends-with? (str (:node-id %)) "call-0#group")
                             (.current interactions/hit-map)))

              expanded
              (toggle-review-region opened-band read-group)

              collapsed
              {}]

          (is (some? band))
          (is (some? read-group))
          (doseq [expansions [{} expanded collapsed]]
            (paint-activity-review! hs rows expansions)
            (paint-activity-review! ts rows expansions)
            (is (= (cell-grid html cols 50) (cell-grid terminal cols 50)))
            (is (str/includes? (.renderHtml html) "ACTIVITY")))
          (paint-activity-review! hs rows expanded)
          (let [rendered (paint-activity-review! ts rows expanded)]
            (is (some #(str/includes? % "src/file-0.clj") (:lines rendered)))
            (when (= status "failed")
              (is (some #(str/includes? % "Test suite failed") (:lines rendered)))))
          ;; A streamed replacement keeps the group's manual expansion.
          (let [replacement (paint-activity-review! ts (activity-review-rows "succeeded") expanded)]
            (is (some #(str/includes? % "src/file-0.clj") (:lines replacement)))))))))

(deftest joined-execution-text-left-edge-test
  ;; Code, Result and Activity share the transcript edge without a rail.
  (doseq [cols [40 80 120]]
    (with-open [terminal (DefaultVirtualTerminal. (TerminalSize. cols 50))
                screen (doto (TerminalScreen. terminal) (.startScreen))]

      (let [rows (activity-review-rows "failed")
            _ (paint-activity-review! screen rows {})
            code (first (filter #(str/ends-with? (str (:node-id %)) ":code")
                                (.current interactions/hit-map)))
            expanded (toggle-review-region {} code)]

        (is (some? code))
        (doseq [expansions [{} expanded]]
          (paint-activity-review! screen rows expansions)
          (let [lines (mapv (fn [row]
                              (apply str
                                (map #(.getCharacterString ^com.googlecode.lanterna.TextCharacter %)
                                     row)))
                            (cell-grid terminal cols 50))
                column (fn [text]
                         (some #(when (str/includes? % text) (str/index-of % text)) lines))]

            (is (= (+ 2 (column "Vis")) (column "Inspect files")))
            (doseq [label (if (seq expansions) ["CODE" "RESULT" "ACTIVITY"] ["CODE" "ACTIVITY"])]
              (is (= (+ 2 (column "Vis")) (column label))))
            (is (nil? (column "Read ×3")))
            (is (nil? (column "Command failed")))
            (is (not-any? #(str/includes? % "│") lines))
            (if (seq expansions)
              (do (is (= (column "CODE") (column "inspect_files()")))
                  (is (some #(re-find #"RESULT  \+2 more ▸" %) lines)))
              (do (is (nil? (column "RESULT")))
                  (is (some #(re-find #"CODE  \+3 more ▸" %) lines))))))))))

(deftest joined-execution-background-and-disclosure-test
  ;; Removing the rail preserves filled execution bands and their disclosures.
  (doseq [cols
          [40 80 160]

          expanded?
          [false true]]

    (with-open [terminal
                (DefaultVirtualTerminal. (TerminalSize. cols 50))

                screen
                (doto (TerminalScreen. terminal) (.startScreen))]

      (paint-activity-review! screen
                              (activity-review-rows "succeeded")
                              {:vis.channel-tui/expand-all-details? expanded?})
      (let [grid
            (cell-grid terminal cols 50)

            lines
            (mapv (fn [row]
                    (apply str
                      (map #(.getCharacterString ^com.googlecode.lanterna.TextCharacter %) row)))
                  grid)

            band-rows
            (keep-indexed (fn [row cells]
                            (when (= theme/code-block-bg
                                     (.getBackgroundColor ^com.googlecode.lanterna.TextCharacter
                                                          (nth cells 1)))
                              row))
                          grid)

            bottom
            (last band-rows)

            chevron
            (if expanded? "▾" "▸")]

        (is (seq band-rows))
        (is (= #{theme/code-block-bg}
               (set (for [row
                          band-rows

                          col
                          (range 1 4)]

                      (.getBackgroundColor ^com.googlecode.lanterna.TextCharacter
                                           (get-in grid [row col])))))
            "Execution fills reach the left text edge")
        (doseq [label ["CODE" "ACTIVITY"]]
          (let [row (first (keep-indexed #(when (str/includes? %2 label) %1) lines))
                line (nth lines row)
                col (+ (str/index-of line label) (count label) 1)
                hit (.lookup interactions/hit-map (TerminalPosition. (int col) (int row)))]

            (is (re-find (re-pattern (str
                                       label
                                       (if (and (= label "CODE") (not expanded?)) "  \\+3 more" "")
                                       " "
                                       chevron))
                         line))
            (is (= :toggle-details (:kind hit)))
            (is (= (not expanded?) (:collapsed? hit)))))
        (is (str/blank? (subs (nth lines bottom) 2))
            "One empty execution row remains below the last content row")
        (is (= #{theme/code-block-bg}
               (set (map #(.getBackgroundColor ^com.googlecode.lanterna.TextCharacter %)
                         (subvec (nth grid bottom) 1 (- cols 3))))))))))

(deftest live-execution-bottom-padding-test
  ;; Live margin trimming must not remove the filled Activity bottom edge.
  (doseq [status
          ["running" "succeeded" "failed"]

          expanded?
          [false true]]

    (let [payload
          (render/progress->lines-data
            {:iterations [{:forms [{:code "inspect_files()"
                                    :activity {:state status
                                               :rows (activity-review-rows status)}}]}]}
            80
            {:show-iterations true}
            {:session-id "live-padding"
             :now-ms 1000
             :detail-expansions {:vis.channel-tui/expand-all-details? expanded?}})

          lines
          (:lines payload)

          last-band-row
          (last (keep-indexed #(when (str/starts-with? %2 @#'render/activity-marker) %1) lines))]

      (is (= @#'render/activity-marker (nth lines last-band-row)))
      (is (= "" (nth lines (inc last-band-row))))
      (is (str/includes? (nth lines (+ last-band-row 2)) "Esc to cancel")))))

(deftest screen-accepts-a-transport-neutral-html-terminal-test
  (with-open [terminal (-> (HtmlTerminal/builder)
                           (.build))]
    (is (identical? terminal (#'screen/create-terminal! {:html-terminal terminal})))
    (is (.contains (.renderLiveHtml terminal "/tui") "data-endpoint-prefix=\"/tui\""))
    (let [method-names (set (map #(.getName ^java.lang.reflect.Method %)
                                 (.getMethods HtmlTerminal)))]
      (is (not-any? method-names ["getUrl" "getUri" "getPort" "hasEmbeddedServer"])))))

(deftest html-media-uses-the-resolved-image-cell-box-test
  (let [terminal
        (-> (HtmlTerminal/builder)
            (.initialSize (TerminalSize. 40 12))
            (.build))

        active
        (deref #'screen/active-html-terminal)

        region
        {:row 3
         :col 4
         :img {:id "turn-1-image-1" :path "/tmp/preview.png" :mime "image/png" :cols 8 :rows 5}}]

    (try (reset! active terminal)
         (timg/set-backend! :html)
         (with-redefs [timg/video-source?
                       (constantly false)

                       timg/html-png-data
                       (fn [_ _]
                         (byte-array [1 2 3]))]

           (#'screen/paint-terminal-images! [region])
           (let [media (first (.getMedia terminal))]
             (is (= 1 (count (.getMedia terminal))))
             (is (= "turn-1-image-1" (.getId media)))
             (is (= HtmlMedia$Kind/IMAGE (.getKind media)))
             (is (= (TerminalPosition. 4 3) (.getPosition media)))
             (is (= (TerminalSize. 8 5) (.getSize media))))
           (#'screen/drop-terminal-images!)
           (is (empty? (.getMedia terminal))))
         (finally (reset! active nil) (timg/set-backend! :native) (.close terminal)))))

(deftest html-backend-reserves-inline-image-layout-test
  (try (timg/set-backend! :html)
       (is (= :html (timg/images-protocol)))
       (is (true? (timg/graphical-terminal?)))
       (finally (timg/set-backend! :native))))

(deftest header-actions-use-lanterna-grid-and-render-alone-test
  (let [panel
        (header/header-actions-component)

        preferred
        (.getPreferredSize ^Panel panel)

        html
        (HtmlTerminalView/render panel preferred "Vis header actions")]

    (is (instance? GridLayout (.getLayoutManager ^Panel panel)))
    (is (every? #(instance? Button %) (.getChildrenList ^Panel panel)))
    (is (= 1 (.getRows preferred)))
    (is (.contains html "Vis header actions"))
    (is (.contains html "help ("))
    (is (.contains html "data-live=\"false\""))))

(deftest header-actions-run-as-an-interactive-html-component-test
  (let [triggered
        (promise)

        panel
        (header/header-actions-component #(deliver triggered %))

        view
        (HtmlTerminalView/start panel (.getPreferredSize ^Panel panel) "Interactive Vis header")]

    (try (.addInput (.getTerminal view) (KeyStroke. KeyType/Enter))
         (is (= :header-help (deref triggered 3000 ::timeout)))
         (is (.contains (.renderHtml view) "help ("))
         (finally (.close view)))))

(deftest all-terminal-surfaces-enter-through-the-grid-test
  (let [source-root
        (-> (io/resource "com/blockether/vis/tui/frame.clj")
            .toURI
            io/file
            .getParentFile)

        calls-by-file
        (into (sorted-map)
              (keep (fn [file]
                      (when (and (.isFile file) (.endsWith (.getName file) ".clj"))
                        (let [calls (count (re-seq #"\.newTextGraphics" (slurp file)))]
                          (when (pos? calls) [(.getName file) calls])))))
              (file-seq source-root))]

    ;; `render` and `screen` only create child clips from grid-owned graphics.
    ;; Every terminal-screen root and section clip is centralized in `frame`.
    (is (= {"frame.clj" 4 "render.clj" 1 "screen.clj" 1} calls-by-file))))

(deftest full-screen-surfaces-use-one-lanterna-grid-test
  (let [{:keys [panel sections]}
        (frame/layout 80 30 {:header 3 :attachments 2 :composer 3 :footer 2})

        bounds
        (update-vals sections frame/bounds)]

    (is (instance? GridLayout (.getLayoutManager ^Panel panel)))
    (is (= [:header :header-gap :transcript :echo :attachments :composer :footer]
           (vec (keys sections))))
    (is (every? #(instance? TextGraphicsComponent %) (vals sections)))
    (is (= {:col 0 :row 0 :cols 80 :rows 3} (:header bounds)))
    (is (= {:col 0 :row 4 :cols 80 :rows 18} (:transcript bounds)))
    (is (= {:col 0 :row 22 :cols 80 :rows 1} (:echo bounds)))
    (is (= {:col 0 :row 23 :cols 80 :rows 2} (:attachments bounds)))
    (is (= {:col 0 :row 25 :cols 80 :rows 3} (:composer bounds)))
    (is (= {:col 0 :row 28 :cols 80 :rows 2} (:footer bounds)))))

(deftest grid-sections-fill-every-supported-column-count-test
  (doseq [cols (range 20 401)]
    (let [sections (:sections
                     (frame/layout cols 40 {:header 3 :attachments 2 :composer 4 :footer 2}))
          bounds (mapv (comp frame/bounds sections) frame/section-order)]

      (is (every? #(= {:col 0 :cols cols} (select-keys % [:col :cols])) bounds)
          (str "every section fills " cols " columns"))
      (is (= (mapv :row bounds) (vec (butlast (reductions + 0 (map :rows bounds)))))
          (str "sections are contiguous at " cols " columns"))
      (is (= 40 (reduce + (map :rows bounds)))
          (str "sections fill every row at " cols " columns")))))

(deftest laid-out-section-painters-stay-inside-their-grid-cell-test
  (let [terminal
        (DefaultVirtualTerminal. (TerminalSize. 12 5))

        screen
        (doto (TerminalScreen. terminal) (.startScreen))

        root
        (frame/layout 12
                      5
                      {:header 1 :attachments 0 :composer 0 :footer 0}
                      {:transcript (fn [graphics _]
                                     (.putString graphics 0 1 "above")
                                     (.putString graphics 0 2 "inside")
                                     (.putString graphics 0 4 "below"))})]

    (try (frame/paint! (.newTextGraphics screen) root :transcript)
         (.refresh screen)
         (is (= \space (.getCharacter (.getCharacter terminal (TerminalPosition. 0 1)))))
         (is (= \i (.getCharacter (.getCharacter terminal (TerminalPosition. 0 2)))))
         (is (= \space (.getCharacter (.getCharacter terminal (TerminalPosition. 0 4)))))
         (finally (.stopScreen screen)))))

(deftest complete-frame-cells-match-html-and-terminal-at-every-width-test
  (let [rows
        32

        initial-size
        (TerminalSize. 20 rows)

        terminal
        (DefaultVirtualTerminal. initial-size)

        html
        (-> (HtmlTerminal/builder)
            (.initialSize initial-size)
            (.columnRange 20 400)
            (.rowRange rows rows)
            (.browserResize false)
            (.build))

        terminal-screen
        (doto (TerminalScreen. terminal) (.startScreen))

        html-screen
        (doto (TerminalScreen. html) (.startScreen))

        db
        {:config nil
         :session nil
         :title "Grid parity"
         :messages [{:id "user" :role :user :text "Zażółć gęślą — grid 界"}
                    {:id "assistant"
                     :role :assistant
                     :text "The same cells reach both backends."
                     :traces [{:forms [{:code "first_call()\nfirst_detail()" :success? true}
                                       {:code "second_call()" :success? true}]}]}]
         :scroll scroll/follow
         :input (input/paste-text (input/empty-input) "interactive draft")
         :settings {}
         :pending-sends []
         :detail-expansions {}
         :live-views []
         :loading? false
         :cancelling? false
         :progress nil
         :channel-status {}
         :tabs []
         :tab-locals {}
         :slash-command-index 0
         :render-version 0}]

    (try (doseq [cols (range 20 401)]
           (let [size (TerminalSize. cols rows)]
             (.setTerminalSize terminal size)
             (.setTerminalSize html size)
             (.doResizeIfNecessary terminal-screen)
             (.doResizeIfNecessary html-screen)
             ;; Compare one metadata snapshot, not two sides of an async footer refresh.
             (with-redefs [timg/images-protocol (constantly nil)
                           client/get-router (constantly nil)]

               (#'screen/render-frame! terminal-screen cols rows db 1000)
               (#'screen/render-frame! html-screen cols rows db 1000))
             (is (= (cell-grid terminal cols rows) (cell-grid html cols rows))
                 (str "HTML and terminal cells differ at " cols " columns"))))
         (finally (.stopScreen terminal-screen)
                  (.stopScreen html-screen)
                  (.close html)
                  (.close terminal)))))

(deftest any-grid-view-renders-as-standalone-html-test
  (let [view
        (frame/view 18
                    2
                    (fn [graphics _]
                      (.putString graphics 0 0 "Standalone view")))

        html
        (HtmlTerminalView/render view (TerminalSize. 18 2) "Standalone Vis view")]

    (is (instance? GridLayout (.getLayoutManager ^Panel view)))
    (is (= 1 (count (.getChildrenList ^Panel view))))
    (is (.contains html ">Standalone</span>"))
    (is (.contains html ">view</span>"))
    (is (.contains html "Standalone Vis view"))))

(deftest grid-surface-owns-size-and-clipping-test
  (let [terminal
        (DefaultVirtualTerminal. (TerminalSize. 12 5))

        screen
        (doto (TerminalScreen. terminal) (.startScreen))]

    (try (let [base (.newTextGraphics screen)]
           (.putString base 0 0 "K")
           (let [graphics (frame/surface-graphics screen 8 3)]
             (is (= (TerminalSize. 8 3) (.getSize graphics)))
             (.putString graphics 7 2 "XY")
             (.refresh screen)
             (is (= \K (.getCharacter (.getCharacter terminal (TerminalPosition. 0 0)))))
             (is (= \X (.getCharacter (.getCharacter terminal (TerminalPosition. 7 2)))))
             (is (= \space (.getCharacter (.getCharacter terminal (TerminalPosition. 8 2)))))))
         (finally (.stopScreen screen)))))
