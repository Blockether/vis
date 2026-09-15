(ns com.blockether.vis.tui.composer-image-references-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.composer-attachment-rail :as rail]
            [com.blockether.vis.tui.composer-attachments :as attachments]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.state :as state]
            [lazytest.core :refer [defdescribe expect it]]))

(def review-draft
  {:input
   {:lines ["Compare [IMAGE #1] with [IMAGE #2]" "[Pasted #1: 2 lines, 22 B]"] :crow 0 :ccol 32}
   :attachments [{:id "diagram"
                  :image-number 1
                  :filename "diagram.png"
                  :media-type "image/png"
                  :size 153600
                  :width 1280
                  :height 720}
                 {:id "screen"
                  :image-number 2
                  :filename "screen.png"
                  :media-type "image/png"
                  :size 90112
                  :width 1280
                  :height 720}]
   :pastes {1 {:id 1 :content "Expected result\nActual"}}
   :paste-counter 1
   :image-counter 2
   :attachment-focus? true
   :attachment-index 0
   :render-version 0})

(defn paint-composer!
  "Deterministic production composer fixture for terminal and HTML review."
  [g draft cols]
  (.beginFrame interactions/hit-map)
  (render/fill-background! g cols 14)
  (rail/draw! g
              (:attachments draft)
              0
              cols
              {:focused? (:attachment-focus? draft) :focused-index (:attachment-index draft)})
  (let [cursor (render/draw-input-box! g
                                       (:input draft)
                                       (rail/rail-height (:attachments draft))
                                       4
                                       cols
                                       nil)]
    (.commitFrame interactions/hit-map)
    cursor))

(defdescribe
  composer-image-reference-contract
  (it "keeps image labels out of text-paste expansion and deletion"
      (let [pastes
            {1 {:id 1 :content "paragraph"}}

            text
            "[Pasted #1: 1 line, 9 B] [IMAGE #1]"]

        (expect (= "paragraph [IMAGE #1]" (input/expand-paste-placeholders text pastes)))
        (reset! state/app-db {:input {:lines ["[Pasted #1: 1 line, 9 B]"] :crow 0 :ccol 0}
                              :pastes pastes
                              :render-version 0})
        (state/dispatch [:remove-paste 1])
        (expect (= pastes (:pastes @state/app-db)))))
  (it "atomically deletes the last of several different image tokens"
      (let [text
            "[IMAGE #1] [IMAGE #2]"

            editor
            {:lines [text] :crow 0 :ccol (count text)}]

        (expect (= 2 (input/placeholder-id-before-cursor editor)))
        (expect (= "[IMAGE #1] " (input/input->text (input/delete-placeholder-backward editor))))))
  (it "keeps the caret before, inside and after a removed token within its surviving line"
      (doseq [[caret expected] [[0 0] [2 2] [6 2] [12 2] [14 4]]]
        (let [editor {:lines ["xx[IMAGE #1]yy"] :crow 0 :ccol caret}
              removed (input/remove-input-token editor "[IMAGE #1]")]

          (expect (= ["xxyy"] (:lines removed)))
          (expect (= expected (:ccol removed))))))
  (it "preserves references and the allocation counter through retry and tab snapshots"
      (let [submission
            {:text "[IMAGE #2]"
             :pastes {}
             :paste-counter 1
             :image-counter 4
             :attachments [{:id "b" :image-number 2 :media-type "image/png"}]}

            pristine
            {:input (input/empty-input) :attachments []}

            restored
            (#'state/restore-editor-only pristine submission)]

        (expect (= 4 (:image-counter restored)))
        (expect (= "[IMAGE #2]" (input/input->text (:input restored))))
        (expect (= (:attachments submission) (:attachments restored)))
        (expect (= 4 (:image-counter (#'state/tab-snapshot restored))))
        (reset! state/app-db (assoc restored :render-version 0))
        (state/dispatch [:reset-input])
        (expect (= 0 (:image-counter @state/app-db)))
        (expect (empty? (:attachments @state/app-db)))))
  (it "keeps all queued images unambiguous when separate messages reuse image numbers"
      (let [entry
            {:text "other [IMAGE #1]"
             :image-counter 1
             :attachments [{:id "other" :image-number 1 :media-type "image/png"}]}

            draft
            {:input {:lines ["first [IMAGE #1]"] :crow 0 :ccol 16}
             :image-counter 1
             :attachments [{:id "first" :image-number 1 :media-type "image/png"}]}

            merged
            (#'state/restore-entries-to-input draft [entry])]

        (expect (= [1 2] (mapv :image-number (:attachments merged))))
        (expect (= "first [IMAGE #1]\n\nother [IMAGE #2]" (input/input->text (:input merged))))))
  (it "reserves authored unowned references before image intake"
      (reset! state/app-db {:input {:lines ["literal [IMAGE #1] "] :crow 0 :ccol 19}
                            :attachments []
                            :render-version 0})
      (state/dispatch [:apply-attachment-intake
                       {:attachments [{:id "new" :media-type "image/png"}]}])
      (expect (= 2 (:image-number (first (:attachments @state/app-db)))))
      (expect (= "literal [IMAGE #1] [IMAGE #2]" (input/input->text (:input @state/app-db)))))
  (it "reserves unowned literals when queued image references return to the editor"
      (let [draft
            {:input {:lines ["literal [IMAGE #1]"] :crow 0 :ccol 17} :attachments []}

            entry
            {:text "queued [IMAGE #1]"
             :image-counter 1
             :attachments [{:id "queued" :image-number 1 :media-type "image/png"}]}

            merged
            (#'state/restore-entries-to-input draft [entry])]

        (expect (= 2 (:image-number (first (:attachments merged)))))
        (expect (= "literal [IMAGE #1]\n\nqueued [IMAGE #2]" (input/input->text (:input merged))))))
  (it "paints both wrapped token fragments as chips without painting surrounding prose"
      (let [capture
            (cap/capture!
              {:cols 20
               :rows 6
               :paint! (fn [{:keys [g]}]
                         (render/draw-input-box! g
                                                 {:lines ["abcdefghij[IMAGE #1]z"] :crow 0 :ccol 0}
                                                 0 4
                                                 20 nil))})

            grid
            (last (:frames capture))]

        (expect (nil? (:error capture)))
        (expect (false? (:bold (get-in grid [1 2]))))
        (expect (true? (:bold (get-in grid [1 12]))))
        (expect (true? (:bold (get-in grid [2 2]))))
        (expect (false? (:bold (get-in grid [2 6]))))))
  (it "renders a bounded staging surface, consistent labels, focus and a shadow"
      (doseq [cols [8 20 40 80]]
        (let [captured (cap/capture! {:cols cols
                                      :rows 14
                                      :paint! (fn [{:keys [g]}]
                                                (paint-composer! g review-draft cols))})
              grid (last (:frames captured))
              regions (.current interactions/hit-map)]

          (expect (nil? (:error captured)))
          (expect (not= (:bg (get-in grid [4 3])) (:bg (get-in grid [5 3]))))
          (doseq [{:keys [bounds]} regions]
            (expect (<= 0 (:col bounds) (+ (:col bounds) (:width bounds)) cols)))
          (when (>= cols 40)
            (expect (str/includes? (cap/frame-text captured) "IMAGE #1"))
            (expect (str/includes? (cap/frame-text captured) "IMAGE #2"))))))
  (it
    "sends the stable reference beside unchanged filename and bytes"
    (let
      [file
       (java.io.File/createTempFile "vis-image-reference-" ".png")

       encoded
       "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII="]

      (try (java.nio.file.Files/write (.toPath file)
                                      (.decode (java.util.Base64/getDecoder) encoded)
                                      (make-array java.nio.file.OpenOption 0))
           (expect (= {:filename "original.png"
                       :media-type "image/png"
                       :base64 encoded
                       :reference "[IMAGE #7]"}
                      (first (attachments/inline-payloads [{:path (.getPath file)
                                                            :filename "original.png"
                                                            :media-type "image/png"
                                                            :image-number 7}]))))
           (finally (.delete file))))))
