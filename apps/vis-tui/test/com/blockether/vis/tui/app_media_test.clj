;; Decisions of app pictures and the app debug log (Council #9354).
(ns com.blockether.vis.tui.app-media-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.imaging :as imaging]
            [com.blockether.vis.tui.app-log :as app-log]
            [com.blockether.vis.tui.app-model :as model]
            [com.blockether.vis.tui.capture :as capture]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as theme]
            [com.blockether.vis.tui.app-surface :as surface]
            [com.blockether.vis.tui.human-input :as hi]
            [com.blockether.vis.tui.live-view :as lv]
            [com.blockether.vis.tui.picture :as picture]
            [com.blockether.vis.tui.view-materializer :as materializer]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import [com.googlecode.lanterna TerminalPosition TextColor$RGB]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]))

(defn- image-node
  []
  (let [view (hi/live-view<-wire
               (json/read-json (slurp (io/resource "vis-contract/fixtures/live-primitives.json"))))]
    (some #(when (= :image (:type %)) %)
          (tree-seq #(seq (:children %)) :children {:children (:nodes view)}))))

(defn- image-rows [node text-w] (#'lv/node-rows node {:text-w text-w}))

(defn- part-right
  "The last column after the widest part of `rows`."
  [rows]
  (reduce max
          0
          (for [{:keys [parts]}
                rows

                {:keys [col frame pad text]}
                parts]

            (+ (long col)
               (long (if frame (+ (long pad) (long (:width frame))) (count (str text))))))))

(def ^:private entries
  [{:seq 1 :at 0 :level "debug" :source "app" :message "start"}
   {:seq 2
    :at 0
    :level "error"
    :source "host"
    :message "callback failed"
    :detail "trace 1\ntrace 2"} {:seq 3 :at 0 :level "warn" :source "app" :message "slow"}])

(defdescribe
  picture-decisions
  (describe "layout"
            (it "compares side by side only when both pictures fit, and the toggle stacks them"
                (expect (= :single (picture/layout 120 false false)))
                (expect (= :side (picture/layout 120 true false)))
                (expect (= :stacked (picture/layout 120 true true)))
                (expect (= :stacked (picture/layout 40 true false)))
                (expect (= :stacked (picture/layout 40 true true)))))
  (describe "fit"
            (it "keeps the aspect, never grows and gives an even pixel height"
                (expect (= [4 4] (picture/fit 4 4 80 12)))
                (expect (= [40 20] (picture/fit 800 400 40 12)))
                (expect (= [24 24] (picture/fit 1000 1000 80 12)))))
  (describe "rows"
            (it "draws the fixture picture as pixels, Before and After, inside the band"
                (doseq [w [40 120]]
                  (let [rows (image-rows (image-node) w)
                        pics (filter #(= :picture (:kind %)) rows)]

                    (expect (some (fn [r]
                                    (some :frame (:parts r)))
                                  pics))
                    (expect (some (fn [r]
                                    (some #(= "Before" (:text %)) (:parts r)))
                                  pics))
                    (expect (<= (long (part-right pics)) (long w))))))
            (it "shows a box with the facts when the picture has no data"
                (let [rows
                      (image-rows (dissoc (image-node) :data :before) 60)

                      texts
                      (keep :text (mapcat :parts (filter #(= :picture (:kind %)) rows)))]

                  (expect (not-any? :frame (mapcat :parts rows)))
                  (expect (some #(re-find #"PNG" %) texts))
                  (expect (<= (long (part-right (filter #(= :picture (:kind %)) rows))) 60))))))

(defdescribe
  app-log-decisions
  (it "names the errors in the chip, and an empty log has no chip"
      (expect (nil? (app-log/chip-label [])))
      (expect (= " Log (1 error) " (app-log/chip-label entries)))
      (expect (= " Log (1) " (app-log/chip-label [(first entries)]))))
  (it "the level filter shows that level and above, oldest first"
      (expect (= [1 2 3] (map :seq (app-log/visible entries nil))))
      (expect (= [2 3] (map :seq (app-log/visible entries "warn"))))
      (expect (= [2] (map :seq (app-log/visible entries "error"))))
      (expect (= "info" (app-log/next-filter nil)))
      (expect (nil? (app-log/next-filter "error"))))
  (it "Enter expands the detail of the selected entry and closes it again"
      (let [state
            {:selected 1 :scroll 0 :expanded #{} :filter nil}

            enter
            (KeyStroke. KeyType/Enter)

            opened
            (app-log/handle-key state enter (app-log/rows entries state))]

        (expect (= #{2} (:expanded opened)))
        (expect (= 5 (count (app-log/rows entries opened))))
        (expect (= #{}
                   (:expanded (app-log/handle-key opened enter (app-log/rows entries opened)))))))
  (it "a click on the chip opens the log, a click beside it does not"
      (let [view
            {:title "Review" :log entries}

            chip
            (surface/log-chip {:left 0 :top 3 :right 79} view)

            click
            (fn [x]
              (MouseAction. MouseActionType/CLICK_RELEASE 1 (TerminalPosition. (int x) 3)))]

        (expect (surface/log-chip-click? (click (:col chip)) chip))
        (expect (not (surface/log-chip-click? (click (dec (long (:col chip)))) chip)))
        (expect (nil? (surface/log-chip {:left 0 :top 3 :right 79} {:log []}))))))

(defdescribe log-op
             (it "a log op adds the stamped entry to the app"
                 (let [view
                       {:id "v" :seq 1 :placement :attachment :nodes []}

                       out
                       (materializer/apply-patch
                         view
                         {:view-id "v"
                          :seq 2
                          :ops [{:op :log :level "error" :message "boom" :at 5 :source "host"}]})]

                   (expect (= [{:seq 1 :at 5 :level "error" :source "host" :message "boom"}]
                              (:log out))))))

(defn- shared-picture
  "The picture named `pname` in the shared `app-pictures.json` fixture, as a node."
  [pname]
  (let [{:strs [accepted refused]}
        (json/read-json (slurp (io/resource "vis-contract/fixtures/app-pictures.json")))

        {{:strs [media_type data]} "picture"}
        (some #(when (= pname (get % "name")) %) (concat accepted refused))]

    {:type :image :id "p" :alt "p" :media-type media_type :data data}))

;; Audit M4 (Council #9376): a small PNG can declare a huge size, and its decode can stop the JVM.
(defdescribe
  picture-size-bound
  (it "reads the size from the header before a decode"
      (let [bytes (.decode (java.util.Base64/getDecoder) ^String (:data (shared-picture "png")))]
        (expect (= [3 2] (picture/decodable-size bytes "image/png")))))
  (it "never decodes a picture over the pixel bound and shows the placeholder"
      (let [decoded
            (atom 0)

            node
            (shared-picture "too many pixels")

            rows
            (with-redefs [imaging/decode (fn [& _]
                                           (swap! decoded inc)
                                           (throw (ex-info "decoded" {})))]
              (image-rows node 60))]

        (expect (zero? (long @decoded)))
        (expect (nil? (picture/raster node 40 12)))
        (expect (not-any? :frame (mapcat :parts rows)))))
  (it "decodes an accepted picture" (expect (some? (picture/raster (shared-picture "png") 40 12)))))

;; Audit L1 (Council #9376): the TUI document held megabytes of base64 picture data.
(defdescribe picture-markdown
             (it "writes picture facts without the data, and reads them back the same"
                 (let [node
                       (image-node)

                       view
                       {:id "v" :seq 1 :title "Pictures" :placement :attachment :nodes [node]}

                       markdown
                       (materializer/->markdown view)

                       parsed
                       (first (:nodes (:view (materializer/parse-markdown markdown))))]

                   (expect (not (str/includes? markdown (subs (:data node) 0 40))))
                   (expect (= [(:media-type node) (:width node) (:height node)]
                              ((juxt :media-type :width :height) parsed)))
                   (expect (pos? (long (:bytes parsed))))
                   (expect (= markdown (materializer/->markdown (assoc view :nodes [parsed])))))))

(defdescribe
  log-layout
  (it "the time shows milliseconds"
      (expect (str/starts-with? (subs (app-log/entry-text {:at 1042 :message "m"}) 8) ".042")))
  (it "a narrow row drops the source first, then shortens the level, so the message keeps room"
      (expect (= {:source? true :level 5} (app-log/entry-columns 120)))
      (expect (= {:source? false :level 5} (app-log/entry-columns 36)))
      (expect (= {:source? false :level 1} (app-log/entry-columns 33)))
      (doseq [w [33 36 120]]
        (let [text (app-log/entry-text
                     {:at 0 :level "error" :source "host" :message "callback failed"}
                     w)]
          (expect (<= (count text) (max (long w) 33)))
          (expect (str/includes? text "callback")))))
  (it "a narrow hint bar keeps Esc close"
      (doseq [w [20 30 40 120]]
        (expect (= ["Esc" "close"] (peek (app-log/hint-pairs w))))))
  (it "reconcile keeps the selection on an entry and inside the visible rows"
      (let [rows (app-log/rows entries {:expanded #{2}})]
        (expect (= {:selected 4 :scroll 3}
                   (select-keys (app-log/reconcile-state {:selected 9 :scroll 0} rows 2)
                                [:selected :scroll])))
        (expect (= {:selected 0 :scroll 0}
                   (select-keys (app-log/reconcile-state {:selected 0 :scroll 3} rows 2)
                                [:selected :scroll])))))
  (it "each key gives its state"
      (let [rows
            (app-log/rows entries {})

            state
            {:selected 1 :scroll 0 :expanded #{} :filter nil}]

        (doseq [[key want] [[(KeyStroke. KeyType/Escape) {::app-log/closed true}]
                            [(KeyStroke. KeyType/ArrowUp) {:selected 0}]
                            [(KeyStroke. KeyType/ArrowDown) {:selected 2}]
                            [(KeyStroke. KeyType/Home) {:selected 0}]
                            [(KeyStroke. KeyType/End) {:selected 2}]
                            [(KeyStroke. KeyType/Enter) {:expanded #{2}}]
                            [(KeyStroke. \f false false) {:filter "info" :selected 0}]
                            [(KeyStroke. \x false false) {:selected 1 :expanded #{}}]]]
          (let [out (app-log/handle-key state key rows)]
            (if (::app-log/closed want)
              (expect (contains? out :com.blockether.vis.tui.dialogs/done))
              (expect (= want (select-keys out (keys want))))))))))

(defdescribe
  picture-aspect
  (it "keeps the source aspect within one cell, side by side and stacked"
      (doseq [[cols rows]
              [[59 12] [116 12] [38 12]]

              aspect
              [2.0 2.2]]

        (let [[w h] (picture/fit-cells 64 40 cols rows aspect)]
          (expect (<= w cols))
          (expect (<= h rows))
          ;; the painted height in cell widths, against the source height
          (expect (< (Math/abs (- (* (double h) aspect) (/ (* 40.0 w) 64))) (+ aspect 0.01))))))
  (it "a picture never grows past its pixels, and the frame height is two pixels a row"
      (expect (= [4 4] (picture/fit 4 4 80 12 2.0)))
      (expect (= [64 40] (picture/fit 64 40 120 30 2.0))))
  (it "the cells of a row stay inside the limit"
      (let [frame
            {:width 10 :height 2 :pixels (int-array 20)}

            cells
            (picture/cells [{:col 0 :frame frame :pixel-row 0 :pad 0} {:col 6 :text "After"}] 8)]

        (expect (every? #(< (long (:col %)) 8) cells))
        (expect (= 8 (count (filter :top cells))))
        (expect (= [{:col 6 :text "After" :width 2 :caption? true}] (vec (filter :text cells))))))
  (it "the viewer gets the old picture first, and a picture without data is left out"
      (let [node (image-node)]
        (expect (= ["Before" "After"]
                   (map #(last (str/split (:name %) #" · ")) (picture/viewer-entries node))))
        (expect (= 1 (count (picture/viewer-entries (dissoc node :before)))))
        (expect (empty? (picture/viewer-entries (dissoc node :before :data)))))))

;; Council #9424: wrap kept a previous horizontal offset and hid wrapped code.
(defdescribe
  review-surface-controls
  (it "wrap resets horizontal scrolling and keeps wrapped lines unshifted"
      (let [state
            (assoc (surface/initial-state)
              :hscroll 8
              :wrap? false)

            geom
            {:view {:nodes []} :rows [{:kind :add :text "code"}] :list-h 10}

            wrapped
            (surface/handle-key state (KeyStroke. \w false false) geom {})]

        (expect (:wrap? wrapped))
        (expect (zero? (long (:hscroll wrapped))))
        (expect (= wrapped (surface/handle-key wrapped (KeyStroke. \> false false) geom {})))))
  (it "the log key opens the log and the picture key toggles only its compare layout"
      (let [node
            (image-node)

            view
            {:nodes [node] :log entries}

            state
            (surface/initial-state)

            geom
            {:view view :rows [{:kind :plan :node-id (:id node)}] :list-h 10}

            opened
            (atom false)]

        (expect (= state
                   (surface/handle-key state
                                       (KeyStroke. \l false false)
                                       geom
                                       {:log! #(reset! opened true)})))
        (expect @opened)
        (let [changed (surface/handle-key state (KeyStroke. \v false false) geom {})]
          (expect (= #{[:picture (:id node)]} (:folded changed)))
          (expect (= state (surface/handle-key changed (KeyStroke. \v false false) geom {}))))))
  (it "compare controls apply only to images with an old picture"
      (let [node
            (image-node)

            row
            {:node-id (:id node)}]

        (expect (= node (surface/compare-image {:nodes [node]} row)))
        (expect (nil? (surface/compare-image {:nodes [(dissoc node :before)]} row)))
        (expect (nil? (surface/compare-image {:nodes [(assoc node :type :diff)]} row)))
        (expect (nil? (surface/compare-image {:nodes []} row)))))
  (it "the footer offers only the log and comparison controls that apply"
      (let [node
            (image-node)

            state
            (surface/initial-state)

            row
            {:kind :plan :node-id (:id node)}

            verbs
            (set (#'surface/footer state {:nodes [node] :log entries} [row]))]

        (expect (contains? verbs ["l" "log"]))
        (expect (contains? verbs ["v" "Before/After"]))
        (expect (= ["Esc" "close"] (peek (#'surface/footer state {} []))))
        (expect (not (contains? (set (#'surface/footer state {} [])) ["l" "log"])))
        (expect (= [["Ctrl+Enter" "save"] ["Esc" "cancel"]]
                   (#'surface/footer (assoc state :editor {}) {} []))))))

(defdescribe
  code-paint-decisions
  (it "keeps code and line numbers at a contrast of at least 4.5 on each diff paper"
      (doseq [bg
              [(TextColor$RGB. 250 250 250) (TextColor$RGB. 10 10 10)]

              kind
              [:add :del :ctx]

              selected?
              [false true]]

        (binding [theme/dialog-bg bg]
          (let [{:keys [paper body gutter]} (surface/row-inks {:kind kind :selected? selected?})]
            (expect (>= (theme/contrast-ratio body paper) 4.5))
            (expect (>= (theme/contrast-ratio gutter paper) 4.5))))))
  (it "a word mark is clipped against the visible text after scrolling"
      (doseq [[mark scroll n want] [[nil 0 10 nil] [[2 6] 0 10 [2 6]] [[2 6] 4 10 [0 2]]
                                    [[2 6] 0 4 [2 4]] [[2 6] 6 10 nil] [[12 16] 0 10 nil]]]
        (expect (= want (surface/mark-span mark scroll n))))))

(defdescribe
  diff-layout-controls
  (it "the split toggle uses two sides only when they fit"
      (let [node {:id "d"
                  :type :diff
                  :new-revision "new"
                  :patch "--- a/a.py\n+++ b/a.py\n@@ -1 +1 @@\n-old\n+new\n"}]
        (expect (some #(= :split (:kind %)) (model/diff-rows node 120 #{})))
        (expect (not-any? #(= :split (:kind %)) (model/diff-rows node 40 #{})))
        (expect (not-any? #(= :split (:kind %)) (model/diff-rows node 120 #{} {:unified? true})))
        (expect (= "Changes  new" (:text (first (model/diff-rows node 40 #{})))))
        (expect (not (str/includes? (:text (first
                                             (model/diff-rows (dissoc node :new-revision) 40 #{})))
                                    "?")))))
  (it "file list entries keep per-file counts and long lines wrap without loss"
      (let [line
            (apply str (repeat 160 "x"))

            node
            {:id "d"
             :type :diff
             :patch (str "--- a/a.py\n+++ b/a.py\n@@ -1 +1 @@\n-old\n+"
                         line
                         "\n--- a/b.py\n+++ b/b.py\n@@ -0,0 +1 @@\n+second\n")}]

        (doseq [width [40 120]]
          (let [rows (model/diff-rows node width #{} {:wrap? true})
                files (filter #(= :file-item (:kind %)) rows)]

            (expect (= [["a.py" 1 1] ["b.py" 1 0]] (mapv (juxt :path :added :removed) files)))
            (expect (every? #(<= (long (p/display-width (surface/row-text % 0 width))) width) rows))
            (expect (< 1 (count (filter :wrapped rows)))))))))

;; Council #9424: character-based chunks clipped wide source characters at the pane edge.
(defdescribe unicode-code-wrap
             (it "wraps wide glyphs by terminal columns without dropping source text"
                 (let [text
                       (apply str (repeat 30 "界a"))

                       rows
                       (model/wrap-rows [{:kind :add :text text :new 1 :num-w 2}] 20)]

                   (expect (= text (apply str (map :text rows))))
                   (expect (< 1 (count rows)))
                   (expect (every? #(<= (long (p/display-width (let [{:keys [gutter body]}
                                                                     (model/row-parts %)]
                                                                 (str gutter body))))
                                        20)
                                   rows)))))

;; Council #9396: selection moved the log timestamp one cell to the left.
(defdescribe
  log-selection-alignment
  (it "keeps selected and unselected timestamps aligned inside narrow and wide dialogs"
      (doseq [width [40 120]]
        (let [component (app-log/component (constantly {:title "Review" :log entries}))
              state {:selected 1 :scroll 0 :expanded #{} :filter nil}
              result (capture/capture! {:cols width
                                        :rows 24
                                        :paint! (fn [{:keys [g]}]
                                                  (let [geom ((:measure component) state width 24)]
                                                    ((:paint component) g state geom)))})
              times (filter #(re-find #"\d{2}:\d{2}:\d{2}\.\d{3}" %)
                            (str/split-lines (capture/frame-text result)))
              columns (map #(count (first (str/split % #"\d{2}:\d{2}:\d{2}\.\d{3}"))) times)]

          (expect (nil? (:error result)))
          (expect (= 3 (count times)))
          (expect (= 1 (count (distinct columns))))))))

(defdescribe
  review-footer-modes
  (it "shows wrap and layout choices for the current diff mode"
      (doseq [[state rows layout wrap]
              [[{:unified? false :wrap? false} [{:kind :split}] ["u" "unified"] ["w" "wrap"]]
               [{:unified? true :wrap? true} [{:kind :add}] ["u" "split"] ["w" "cut"]]]]
        (let [verbs (set (#'surface/footer (merge (surface/initial-state) state) {} rows))]
          (expect (contains? verbs layout))
          (expect (contains? verbs wrap))
          (expect (= (not (:wrap? state)) (contains? verbs ["</>" "scroll"])))))))

;; Council #9442: comparison labels started at the pane edge, not the centered image.
(defdescribe picture-caption-alignment
             (it "aligns Before and After with their picture edges in both compare layouts"
                 (doseq [width [40 120]]
                   (let [node (assoc (image-node)
                                :width 64
                                :height 40)
                         rows (with-redefs [picture/raster
                                            (fn [_ cols max-rows]
                                              (let [[w h] (picture/fit 64 40 cols max-rows 2.0)]
                                                {:width w :height h :pixels (int-array (* w h))}))]
                                (image-rows node width))
                         parts (mapcat :parts rows)
                         captions (filter #(contains? #{"Before" "After"} (:text %)) parts)
                         edges (set (map #(+ (long (:col %)) (long (:pad %)))
                                         (filter :frame parts)))]

                     (expect (= 2 (count captions)))
                     (expect (every? #(contains? edges (:col %)) captions))))))

;; Council #9424: selected split rows ignored horizontal scroll while other rows scrolled.
(defdescribe split-selection-scroll
             (it "scrolls both sides of a selected row without moving its gutter"
                 (let [side
                       {:kind :add :new 1 :num-w 1 :text "abcdefgh0123456789"}

                       row
                       {:kind :split :half 17 :left side :right side}

                       start
                       (surface/row-text row 0 40)

                       shifted
                       (surface/row-text row 8 40)]

                   (expect (not= start shifted))
                   (expect (str/includes? shifted "0123456789"))
                   (expect (<= (long (p/display-width shifted)) 40)))))

;; Council #9424: a character index placed word tint inside a preceding wide glyph.
(defdescribe unicode-word-highlight
             (it "starts a word highlight at its painted cell after a wide source character"
                 (let [result
                       (capture/capture! {:cols 40
                                          :rows 4
                                          :paint!
                                          (fn [{:keys [g]}]
                                            (#'surface/paint-code-row!
                                             g
                                             0
                                             1
                                             38
                                             {:kind :add :new 1 :num-w 1 :text "界x" :mark [1 2]}
                                             0))})

                       row
                       (get (last (:frames result)) 1)

                       x
                       (first (keep-indexed #(when (= "x" (:ch %2)) %1) row))]

                   (expect (nil? (:error result)))
                   (expect (some? x))
                   (expect (not= (:bg (nth row x)) (:bg (nth row (dec (long x)))))))))
