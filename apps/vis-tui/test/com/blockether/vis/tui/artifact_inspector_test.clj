(ns com.blockether.vis.tui.artifact-inspector-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.artifact-inspector :as inspector]
            [com.blockether.vis.tui.capture :as cap]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna.input KeyStroke KeyType]
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
  [component]
  (cap/capture! {:cols 96
                 :rows 24
                 :paint! (fn [{:keys [g]}]
                           (let [state
                                 (:init component)

                                 geom
                                 ((:measure component) state 96 24)

                                 state
                                 ((:reconcile component) state geom)]

                             ((:paint component) g state geom)))}))

;; Regression, td-848743: C-x i had no surface when only model output existed.
(defdescribe
  artifact-inspector-layout
  (it "keeps composer input and session output in distinct visible sections"
      (let [capture
            (paint-component (inspector/inspector-modal-component staged produced nil))

            text
            (cap/frame-text capture)]

        (expect (str/includes? text "Ready to send"))
        (expect (str/includes? text "diagram.png"))
        (expect (str/includes? text "Produced in this session"))
        (expect (str/includes? text "decision.html  v2  ·  2 versions"))
        (expect (str/includes? text "notes.pdf"))))
  (it "lists a PLAN file from an old session with the other artifacts"
      (let [artifacts
            (conj produced {"filename" "PLAN-search.md" "media_type" "text/markdown" "version" 1})

            text
            (cap/frame-text (paint-component
                              (inspector/inspector-modal-component [] artifacts nil)))]

        (expect (str/includes? text "PLAN-search.md"))
        (expect (str/includes? text "Produced in this session"))
        (expect (str/includes? text "notes.pdf"))
        (expect (not (str/includes? text "Specifications")))))
  (it "shows an explicit empty state instead of silently doing nothing"
      (let [text (cap/frame-text (paint-component (inspector/inspector-modal-component [] [] nil)))]
        (expect (str/includes? text "No attachments in this session"))))
  (it "shows index failures as an explicit state"
      (let [text (cap/frame-text
                   (paint-component
                     (inspector/inspector-modal-component [] [] "Artifact index unavailable")))]
        (expect (str/includes? text "Artifact index unavailable")))))

(defdescribe artifact-inspector-selection
             (it "selects staged rows first and collapses produced filename versions newest-first"
                 (let [rows (inspector/inspector-rows staged produced)]
                   (expect (= [:staged :produced :produced] (mapv :source rows)))
                   (expect (= ["diagram.png" "decision.html" "notes.pdf"] (mapv :filename rows)))
                   (expect (= [nil 2 1] (mapv :version-count rows)))
                   (expect (= "iteration-new" (:iteration-id (second rows))))))
             (it "returns open and staged-only remove actions from the pure key handler"
                 (let [component
                       (inspector/inspector-modal-component staged produced nil)

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

(defdescribe
  artifact-inspector-paging
  (it
    "pages across attachment headings while keeping a selectable row focused"
    (let [attachments
          (mapv (fn [idx]
                  {"filename" (str "note-" idx ".txt") "version" 1})
                (range 50))

          component
          (inspector/inspector-modal-component [] attachments nil)

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

      (expect (<= (dec (:list-h geom)) (:selected down)))
      (expect (= 0 (:selected up))))))

(defdescribe artifact-inspector-gateway
             (it "loads the whole-session index through the facade"
                 (let [asked (atom nil)]
                   (with-redefs [vis/gateway-session-artifacts (fn [session-id]
                                                                 (reset! asked session-id)
                                                                 [{"filename" "decision.html"}])]
                     (expect (= "decision.html"
                                (get (first (:artifacts (inspector/fetch-session-artifacts!
                                                          "session-1")))
                                     "filename")))
                     (expect (= "session-1" @asked)))))
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

(defdescribe
  explicit-comment-capability
  (it "retains only the latest version's explicit boolean"
      (let [versions
            [{"filename" "PLAN-search.md" "version" 1 "commentable" true}
             {"filename" "PLAN-search.md" "version" 2 "commentable" false}]

            rows
            (inspector/inspector-rows [] versions)]

        (expect (= 2 (:version (first rows))))
        (expect (false? (:commentable (first rows)))))
      (doseq [flag [nil false "true"]]
        (expect (false? (:commentable (first (inspector/inspector-rows []
                                                                       [{"filename" "PLAN-search.md"
                                                                         "commentable" flag}])))))))
  (it "opens specialized diffs in the terminal for both capabilities"
      (doseq [flag [true false]]
        (let [component (inspector/inspector-modal-component []
                                                             [{"filename" "DIFF-search.json"
                                                               "media_type"
                                                               "application/vnd.vis.diff+json"
                                                               "version" 1
                                                               "commentable" flag}]
                                                             nil)
              action ((:on-key component) (:init component) (KeyStroke. KeyType/Enter) {})]

          (expect (= :annotate (get-in action [:com.blockether.vis.tui.dialogs/done :action])))
          (expect (= flag
                     (get-in action [:com.blockether.vis.tui.dialogs/done :row :commentable])))))))

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
  (it
    "a graphical terminal shows the picture enlarged inside the dialog"
    (let [painted
          (atom nil)

          component
          (inspector/image-viewer-component pictures
                                            1
                                            {:graphical? true :paint-images! #(reset! painted %)})

          state
          ((:reconcile component) (:init component) ((:measure component) (:init component) 120 40))

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
        (expect (< (long top) (long row) (+ (long row) (long (:rows img))) (long bottom))))))
  (it "any other terminal gets a numbered table of the pictures"
      (let [text (cap/frame-text (paint-component
                                   (inspector/image-viewer-component pictures 0 {})))]
        (expect (str/includes? text "1  a.png  ·  PNG · 1600x900"))
        (expect (str/includes? text "3  c.png  ·  PNG · 800x800"))
        (expect (str/includes? text "Enter open")))))
