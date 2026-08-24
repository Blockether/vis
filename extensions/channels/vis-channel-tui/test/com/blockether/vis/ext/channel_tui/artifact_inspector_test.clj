(ns com.blockether.vis.ext.channel-tui.artifact-inspector-test
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.artifact-inspector :as inspector]
            [com.blockether.vis.ext.channel-tui.capture :as cap]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
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
  (it "shows an explicit empty state instead of silently doing nothing"
      (let [text (cap/frame-text (paint-component (inspector/inspector-modal-component [] [] nil)))]
        (expect (str/includes? text "No attachments in this session"))))
  (it "shows index failures as an explicit state"
      (let [text (cap/frame-text
                   (paint-component
                     (inspector/inspector-modal-component [] [] "Artifact index unavailable")))]
        (expect (str/includes? text "Artifact index unavailable")))))

(defdescribe
  artifact-inspector-selection
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

        (expect (= :open (:action (:com.blockether.vis.ext.channel-tui.dialogs/done open-result))))
        (expect (= :remove
                   (:action (:com.blockether.vis.ext.channel-tui.dialogs/done remove-result))))
        (expect (nil? (:com.blockether.vis.ext.channel-tui.dialogs/done protected-result))))))

(defdescribe
  artifact-inspector-gateway
  (it "loads the whole-session index through the canonical gateway client"
      (let [request (atom nil)]
        (with-redefs [gateway-client/request!
                      (fn [method path opts]
                        (reset! request [method path opts])
                        {:status 200 :body "{\"artifacts\":[{\"filename\":\"decision.html\"}]}"})]
          (expect (= "decision.html"
                     (get (first (:artifacts (inspector/fetch-session-artifacts! "session-1")))
                          "filename")))
          (expect (= [:get "/v1/sessions/session-1/artifacts" {:timeout-ms 5000}] @request)))))
  (it "materializes durable bytes under the artifact basename"
      (with-redefs [gateway-client/iteration-attachment-bytes
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
