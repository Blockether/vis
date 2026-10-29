(ns com.blockether.vis.tui.artifact-inspector-test
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.app-model :as app-model]
            [com.blockether.vis.tui.artifact-inspector :as inspector]
            [com.blockether.vis.tui.capture :as cap]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna TerminalPosition]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]
           [java.nio.file Files]))

(def staged
  [{:id "draft-1"
    :filename "diagram.png"
    :media-type "image/png"
    :size 2048
    :path "/tmp/diagram.png"}])

(def produced
  [{"filename" "decision.html"
    "media_type" "text/html"
    "size" 100
    "version" 1
    "iteration_id" "iteration-old"
    "index" 0}
   {"filename" "notes.pdf"
    "media_type" "application/pdf"
    "size" 200
    "version" 1
    "iteration_id" "iteration-pdf"
    "index" 1}
   {"filename" "decision.html"
    "media_type" "text/html"
    "size" 300
    "version" 2
    "iteration_id" "iteration-new"
    "index" 2}])

(defn- paint-component
  ([component] (paint-component component (:init component)))
  ([component state]
   (cap/capture! {:cols 96
                  :rows 40
                  :paint! (fn [{:keys [g]}]
                            (let [geom
                                  ((:measure component) state 96 40)

                                  state
                                  (if-let [reconcile (:reconcile component)]
                                    (reconcile state geom)
                                    state)]

                              ((:paint component) g state geom)))})))

;; Regression, td-848743: C-x i had no surface when only model output existed.
(defdescribe
  artifact-inspector-layout
  (it "keeps composer input and session output in distinct visible sections"
      (let [capture
            (paint-component (inspector/inspector-modal-component staged [] produced nil))

            text
            (cap/frame-text capture)]

        (expect (str/includes? text "diagram.png"))
        (expect (str/includes? text "decision.html"))
        (expect (str/includes? text "notes.pdf"))))
  (it "lists a PLAN file from an old session with the other artifacts"
      (let [artifacts
            (conj produced {"filename" "PLAN-search.md" "media_type" "text/markdown" "version" 1})

            text
            (cap/frame-text (paint-component
                              (inspector/inspector-modal-component [] [] artifacts nil)))]

        (expect (str/includes? text "PLAN-search.md"))
        (expect (str/includes? text "notes.pdf"))))
  (it "shows index failures as an explicit state"
      (let [text (cap/frame-text
                   (paint-component
                     (inspector/inspector-modal-component [] [] [] "Artifact index unavailable")))]
        (expect (str/includes? text "Artifact index unavailable")))))

(defdescribe artifact-inspector-selection
             (it "selects staged rows first and collapses produced filename versions newest-first"
                 (let [rows (inspector/inspector-rows staged [] produced)]
                   (expect (= [:staged :produced :produced] (mapv :source rows)))
                   (expect (= ["diagram.png" "decision.html" "notes.pdf"] (mapv :filename rows)))
                   (expect (= [nil 2 1] (mapv :version-count rows)))
                   (expect (= "iteration-new" (:iteration-id (second rows))))))
             (it "returns open and staged-only remove actions from the pure key handler"
                 (let [component
                       (inspector/inspector-modal-component staged [] produced nil)

                       on-key
                       (:on-key component)

                       geom
                       ((:measure component) (:init component) 96 24)

                       open-result
                       (on-key (:init component) (KeyStroke. KeyType/Enter) geom)

                       remove-result
                       (on-key (:init component) (KeyStroke. KeyType/Delete) geom)

                       produced-state
                       (assoc (:init component) :selected 1)

                       protected-result
                       (on-key produced-state (KeyStroke. KeyType/Delete) geom)]

                   (expect (= :open (:action (:com.blockether.vis.tui.dialogs/done open-result))))
                   (expect (= :remove
                              (:action (:com.blockether.vis.tui.dialogs/done remove-result))))
                   (expect (nil? (:com.blockether.vis.tui.dialogs/done protected-result))))))

;; Regression: C-x i said "No attachments in this session" while the transcript showed a
;; picture that the person had pasted into a message.
(defdescribe
  artifact-inspector-sent-pictures
  (let [sent [{:path "/tmp/clipboard.png"
               :mime "image/png"
               :name "clipboard.png"
               :width 940
               :height 1064
               :label "PNG · 940x1064"}]]
    (it "lists the pictures sent in the session between the staged and the produced rows"
        (let [rows (inspector/inspector-rows staged sent produced)
              text (cap/frame-text (paint-component
                                     (inspector/inspector-modal-component [] sent [] nil)))]

          (expect (= [:staged :sent :produced :produced] (mapv :source rows)))
          (expect (= "/tmp/clipboard.png" (get-in (second rows) [:image :path])))
          (expect (str/includes? text "clipboard.png"))))
    (it "opens a sent picture in the image viewer on Enter and externally on o"
        (let [component (inspector/inspector-modal-component [] sent [] nil)
              geom ((:measure component) (:init component) 96 24)
              done #(:com.blockether.vis.tui.dialogs/done
                      ((:on-key component) (:init component) % geom))]

          (expect (= {:action :view :index 0}
                     (-> (done (KeyStroke. KeyType/Enter))
                         (update :row :index)
                         (set/rename-keys {:row :index}))))
          (expect (= :open (:action (done (KeyStroke. \o false false)))))
          (expect (nil? (done (KeyStroke. KeyType/Delete))) "a sent picture cannot be removed")))))

(defdescribe
  artifact-inspector-paging
  (it
    "pages by whole table rows and keeps the selected row on screen"
    (let [attachments
          (mapv (fn [idx]
                  {"filename" (str "note-" idx ".txt") "version" 1})
                (range 50))

          component
          (inspector/inspector-modal-component [] [] attachments nil)

          measure
          (:measure component)

          reconcile
          (:reconcile component)

          on-key
          (:on-key component)

          geom
          (measure (:init component) 96 24)

          step
          (fn [state key]
            (reconcile (on-key state (KeyStroke. key) geom) geom))

          start
          (reconcile (:init component) geom)

          down
          (step start KeyType/PageDown)

          up
          (step down KeyType/PageUp)]

      (expect (< 0 (:selected down) (:list-h geom)))
      (expect (str/includes? (cap/frame-text (paint-component component down))
                             (str "note-" (- 49 (long (:selected down))) ".txt")))
      (expect (= 0 (:selected up))))))

(defn- png-head
  "The first bytes of a PNG file of `w`x`h` pixels: enough to read its size."
  [w h]
  (let [buf (java.nio.ByteBuffer/allocate 33)]
    (.put buf (byte-array (map unchecked-byte [0x89 0x50 0x4E 0x47 0x0D 0x0A 0x1A 0x0A])))
    (.putInt buf 13)
    (.put buf (.getBytes "IHDR" "US-ASCII"))
    (.putInt buf (int w))
    (.putInt buf (int h))
    (.put buf (byte-array [8 6 0 0 0]))
    (.array buf)))

(defdescribe
  artifact-inspector-table
  (let [sent
        [{:path "/tmp/table-shot.png"
          :mime "image/png"
          :name "table-shot.png"
          :width 800
          :height 400
          :label "PNG · 800x400"}]

        thumbnail
        (fn [row]
          (when (= :sent (:source row))
            {:path "/tmp/table-shot.png" :mime "image/png" :width 800 :height 400}))]

    (it "places each visible thumbnail inside its cell"
        (let [painted
              (atom nil)

              component
              (inspector/inspector-modal-component []
                                                   sent
                                                   produced
                                                   nil
                                                   {:thumbnail thumbnail
                                                    :paint-images! #(reset! painted %)})

              geom
              ((:measure component) (:init component) 96 24)

              state
              ((:reconcile component) (:init component) geom)

              _
              ((:after-refresh component) state geom)

              [{:keys [row col img]}]
              @painted]

          (expect (= 1 (count @painted)))
          (expect (= "/tmp/table-shot.png" (:path img)))
          (expect (<= 1 (:cols img) 12))
          (expect (<= 1 (:rows img) 3))
          (expect (< (long (get-in geom [:bounds :left])) (long col)))
          (expect (<= (long (:content-top geom)) (long row)))))
    (it "paints no thumbnails without a picture source"
        (expect (nil? (:after-refresh (inspector/inspector-modal-component [] sent [] nil)))))
    (it
      "loads a produced picture once in the background and keeps local pictures as they are"
      (let [calls
            (atom 0)

            gate
            (promise)

            row
            {:source :produced
             :filename "chart.png"
             :media-type "image/png"
             :iteration-id (str (random-uuid))
             :index 0}]

        (with-redefs [vis/gateway-iteration-attachment-bytes (fn [& _]
                                                               (swap! calls inc)
                                                               @gate
                                                               (png-head 640 480))]
          (let [load (inspector/thumbnail-loader "session-1")
                first-answer (load row)
                _ (deliver gate true)
                done
                (loop [n 0]
                  (let [v (load row)]
                    (if (and (= :loading v) (< n 200)) (do (Thread/sleep 10) (recur (inc n))) v)))]

            (expect (= :loading first-answer))
            (expect (= {:mime "image/png" :width 640 :height 480}
                       (select-keys done [:mime :width :height])))
            (expect (= 1 @calls))
            (expect
              (nil? (load {:source :produced :filename "notes.pdf" :media-type "application/pdf"})))
            (expect (= {:path "/tmp/table-shot.png" :mime "image/png" :width 800 :height 400}
                       (load (first (inspector/inspector-rows [] sent [])))))))))))

(defdescribe
  artifact-inspector-gateway
  (it "loads the whole-session index through the facade"
      (let [asked (atom nil)]
        (with-redefs [vis/gateway-session-artifacts (fn [session-id]
                                                      (reset! asked session-id)
                                                      [{"filename" "decision.html"}])
                      vis/gateway-app-views (constantly [])]

          (expect (= "decision.html"
                     (get (first (:artifacts (inspector/fetch-session-artifacts! "session-1")))
                          "filename")))
          (expect (= "session-1" @asked)))))
  (it "lists every app of the session, also an app that no block filed"
      (with-redefs [vis/gateway-session-artifacts
                    (constantly [{"media_type" app-model/media-type
                                  "view_id" "app-1"
                                  "filename" "counter.app.json"}])

                    vis/gateway-app-views
                    (fn [_ archived?]
                      (if archived?
                        [{"id" "app-3" "title" "Old" "key" "old" "is_archived" true}]
                        [{"id" "app-1" "title" "Counter" "key" "counter"}
                         {"id" "app-2" "title" "Todo" "key" "todo" "is_stale" true}]))]

        (let [rows (inspector/inspector-rows []
                                             []
                                             (:artifacts (inspector/fetch-session-artifacts!
                                                           "session-1")))]
          (expect (= #{"app-1" "app-2" "app-3"} (set (map :view-id rows))))
          (expect (= 3 (count rows)) "a filed app keeps one row")
          (expect (= :archived (:app-status (first (filter #(= "app-3" (:view-id %)) rows)))))
          (expect (every? app-model/app-row? rows))
          (expect (= :stale (:app-status (first (filter #(= "app-2" (:view-id %)) rows))))))))
  (it "keeps the artifacts when the app list cannot be read"
      (with-redefs [vis/gateway-session-artifacts
                    (constantly [{"filename" "a.txt"}])

                    vis/gateway-app-views
                    (fn [_ _]
                      (throw (ex-info "down" {})))]

        (expect (= [{"filename" "a.txt"}]
                   (:artifacts (inspector/fetch-session-artifacts! "session-1"))))))
  (it "paints an index it could not read as unavailable, never as empty"
      (with-redefs [vis/gateway-session-artifacts (constantly nil)]
        (expect (= {:artifacts [] :error "Artifact index unavailable"}
                   (inspector/fetch-session-artifacts! "session-1")))))
  (it "materializes durable bytes under the artifact basename"
      (with-redefs [vis/gateway-iteration-attachment-bytes
                    (fn [sid iid idx]
                      (expect (= ["session-1" "iteration-new" 2] [sid iid idx]))
                      (.getBytes "<html>decision</html>" "UTF-8"))]
        (let [file (inspector/materialize-artifact!
                     "session-1"
                     {:filename "decision.html" :iteration-id "iteration-new" :index 2})]
          (try (expect (= "decision.html" (.getName file)))
               (expect (= "<html>decision</html>"
                          (String. (Files/readAllBytes (.toPath file)) "UTF-8")))
               (finally (.delete file) (.delete (.getParentFile file))))))))

(def ^:private app-artifacts
  [{"filename" "review.app.json"
    "media_type" "application/vnd.vis.app+json"
    "view_id" "view-1"
    "title" "Review"
    "version" 1}
   {"filename" "review.app.json"
    "media_type" "application/vnd.vis.app+json"
    "view_id" "view-2"
    "title" "Second review"
    "app_status" "stale"
    "version" 1}])

(defdescribe
  app-attachment-rows
  (it "keeps one row for each app, also when two apps share a file name"
      (let [rows (inspector/inspector-rows [] [] app-artifacts)]
        (expect (= #{"view-1" "view-2"} (set (map :view-id rows))))
        (expect (= :stale (:app-status (first (filter #(= "view-2" (:view-id %)) rows)))))))
  (it "shows the app title and its lifecycle state"
      (let [text (cap/frame-text (paint-component
                                   (inspector/inspector-modal-component [] [] app-artifacts nil)))]
        (expect (str/includes? text "Second review"))))
  (it "opens an app on the app surface and other text in its viewer"
      (let [enter (fn [artifacts]
                    (let [component (inspector/inspector-modal-component [] [] artifacts nil)]
                      (get-in ((:on-key component) (:init component) (KeyStroke. KeyType/Enter) {})
                              [:com.blockether.vis.tui.dialogs/done :action])))]
        (expect (= :app (enter [(first app-artifacts)])))
        (expect (= :open (enter [{"filename" "PLAN.md" "media_type" "text/markdown" "version" 1}])))
        (expect (= :open
                   (enter [{"filename" "DIFF.json"
                            "media_type" "application/vnd.vis.diff+json"
                            "version" 1}]))))))

(def ^:private pictures
  [{:path "/tmp/a.png"
    :mime "image/png"
    :name "a.png"
    :width 1600
    :height 900
    :label "PNG · 1600x900"}
   {:path "/tmp/b.png"
    :mime "image/png"
    :name "b.png"
    :width 900
    :height 1600
    :label "PNG · 900x1600"}
   {:path "/tmp/c.png"
    :mime "image/png"
    :name "c.png"
    :width 800
    :height 800
    :label "PNG · 800x800"}])

(defdescribe
  image-viewer-test
  (it "arrows and digits select a picture; Enter and o open it; Esc closes"
      (let [step
            #(inspector/image-viewer-key {:index %1} %2 pictures)

            done
            :com.blockether.vis.tui.dialogs/done]

        (expect (= 1 (:index (step 0 (KeyStroke. KeyType/ArrowRight)))))
        (expect (= 2 (:index (step 2 (KeyStroke. KeyType/ArrowDown)))))
        (expect (= 0 (:index (step 1 (KeyStroke. KeyType/ArrowLeft)))))
        (expect (= 2 (:index (step 0 (KeyStroke. \3 false false)))))
        (expect (= 0 (:index (step 0 (KeyStroke. \9 false false)))) "a digit past the end stays")
        (expect (= "/tmp/b.png" (get-in (step 1 (KeyStroke. KeyType/Enter)) [done :image :path])))
        (expect (= "/tmp/c.png" (get-in (step 2 (KeyStroke. \o false false)) [done :image :path])))
        (expect (= {done nil} (step 1 (KeyStroke. KeyType/Escape))))))
  (it "a graphical terminal shows the picture enlarged inside the dialog"
      (let [painted
            (atom nil)

            component
            (inspector/image-viewer-component pictures 1 {:paint-images! #(reset! painted %)})

            state
            (:init component)

            geom
            ((:measure component) state 120 40)

            text
            (cap/frame-text (paint-component component))]

        ((:after-refresh component) state geom)
        (expect (str/includes? text "Image 2 of 3"))
        (expect (str/includes? text "b.png"))
        (let [[{:keys [row col img]}]
              @painted

              {:keys [left right top bottom]}
              (:bounds geom)]

          (expect (= "/tmp/b.png" (:path img)))
          ;; The picture fills the dialog height and stays inside its frame.
          (expect (< 10 (long (:rows img))))
          (expect (< (long left) (long col) (+ (long col) (long (:cols img))) (long right)))
          (expect (< (long top) (long row) (+ (long row) (long (:rows img))) (long bottom)))))))

(defdescribe
  image-viewer-zoom-test
  (let [component
        (inspector/image-viewer-component pictures 0 {})

        geom
        ((:measure component) (:init component) 120 40)

        start
        (:init component)

        on-key
        (fn [state key]
          ((:on-key component) state key geom))

        typed
        (fn [c]
          (KeyStroke. (char c) false false))

        mouse
        (fn [action col row]
          (MouseAction. action 1 (TerminalPosition. (int col) (int row))))

        frame
        (fn [state]
          (inspector/image-view-frame (nth pictures (:index state)) state geom))

        zoomed
        (on-key start (typed \+))]

    (it "+ and - zoom the picture, 0 fits it again"
        (expect (= 1.0 (:zoom (frame start))))
        (expect (= {:x 0 :y 0 :w 1600 :h 900} (:view (frame start))))
        (expect (< 1.0 (double (:zoom zoomed))))
        ;; A zoomed picture shows a part of the source.
        (expect (< (long (get-in (frame zoomed) [:view :w])) 1600))
        (expect (= 1.0 (:zoom (on-key zoomed (typed \-)))))
        (expect (= 1.0 (:zoom (on-key zoomed (typed \0)))))
        (expect (= 8.0
                   (:zoom (reduce (fn [s _]
                                    (on-key s (typed \+)))
                                  start
                                  (range 20))))))
    (it "arrows move a zoomed picture and stop at its edge, else select a picture"
        (expect (= 1 (:index (on-key start (KeyStroke. KeyType/ArrowRight)))))
        (let [moved
              (reduce (fn [s _]
                        (on-key s (KeyStroke. KeyType/ArrowRight)))
                      zoomed
                      (range 30))

              {:keys [x w]}
              (:view (frame moved))]

          (expect (= 0 (:index moved)))
          (expect (= 1600 (+ (long x) (long w)))))
        (expect (= {:index 1 :zoom 1.0}
                   (select-keys (on-key zoomed (KeyStroke. KeyType/PageDown)) [:index :zoom]))))
    (it "the wheel zooms around the pointer and a drag moves the picture"
        (let [{:keys [col row cols rows] :as before}
              (frame (assoc start :zoom 2.0))

              pc
              (+ (long col) (quot (long cols) 3))

              pr
              (+ (long row) (quot (long rows) 2))

              source-x
              (fn [f]
                (+ (long (get-in f [:view :x]))
                   (* (+ 0.5 (- pc (long (:col f))))
                      (long (first (:cell-px geom)))
                      (double (:scale f)))))

              wheeled
              (on-key (assoc start :zoom 2.0) (mouse MouseActionType/SCROLL_UP pc pr))

              dragged
              (-> zoomed
                  (on-key (mouse MouseActionType/CLICK_DOWN 60 20))
                  (on-key (mouse MouseActionType/DRAG 50 20)))]

          (expect (< 2.0 (double (:zoom wheeled))))
          (expect (< (abs (- (source-x before) (source-x (frame wheeled)))) 2.0))
          ;; Dragging the picture left shows more of its right side.
          (expect (< (long (get-in (frame zoomed) [:view :x]))
                     (long (get-in (frame dragged) [:view :x]))))
          (expect (nil? (:drag (on-key dragged (mouse MouseActionType/CLICK_RELEASE 50 20)))))))
    (it "paints only the shown part of a zoomed picture and names its keys"
        (let [painted
              (atom nil)

              component
              (inspector/image-viewer-component pictures 0 {:paint-images! #(reset! painted %)})

              text
              (cap/frame-text (cap/capture! {:cols 120
                                             :rows 40
                                             :paint! (fn [{:keys [g]}]
                                                       ((:paint component) g zoomed geom))}))]

          ((:after-refresh component) zoomed geom)
          (expect (= (:view (frame zoomed)) (:view (:img (first @painted)))))
          (expect (str/includes? text "150%"))))))
