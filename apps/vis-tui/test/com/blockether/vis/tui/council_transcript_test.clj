(ns com.blockether.vis.tui.council-transcript-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.presentation :as presentation]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.virtual :as virtual]
            [lazytest.core :refer [defdescribe expect it]]))

(def request-text
  "Actual peer request.\nSecond line.\nThird line.\nFourth line.\nLast line remains available.")

(def council-entry {"entry_id" 42 "thread_id" 42 "content" request-text})

(defn- visible
  "The words of a projected row, without paint markers and inline-style sentinels."
  [row]
  (str/replace (str row) #"[\u200B-\u200D\u2060-\u206F\uE000-\uF8FF]" ""))

(defdescribe council-wire-history-and-live-events-retain-provenance
             (it "council wire history and live events retain provenance"
                 (let [turn
                       {"turn_id" "council-turn"
                        "request" request-text
                        "request_kind" "council"
                        "council" council-entry
                        "status" "done"}

                       message
                       (first (#'chat/turns->messages [turn]))

                       event
                       (#'chat/gateway-event->chunk (assoc turn "type" "turn.started"))]

                   (expect (= :council (:request-kind message)))
                   (expect (= council-entry (:council message)))
                   (expect (= request-text (:text message)))
                   (expect (= :council (:request-kind event)))
                   (expect (= council-entry (:council event))))))

(defdescribe
  council-preview-counts-rendered-rows-and-keeps-a-scoped-disclosure
  (it
    "council preview counts rendered rows and keeps a scoped disclosure"
    (doseq [width
            [40 100]

            text
            [request-text (str/join " " (repeat 60 "Long request"))]]

      (let [message
            {:role :user
             :request-kind :council
             :council council-entry
             :text text
             :session-turn-id "council-turn"}

            opts
            {:session-id "session" :detail-expansions {}}

            collapsed
            (virtual/project-message message width {} opts)

            control
            (last (:line-meta collapsed))

            expanded
            (virtual/project-message message
                                     width
                                     {}
                                     (assoc opts
                                       :detail-expansions {["session" (:node-id control)] true}))]

        (expect (= 5 (count (:prewrapped-lines collapsed)))
                "Four visible body rows and one control")
        (expect (= :toggle-details (:kind control)))
        (expect (true? (:collapsed? control)))
        (expect (> (count (:prewrapped-lines expanded)) 5))
        (expect (false? (:collapsed? (last (:line-meta expanded)))))
        (.reset interactions/hit-map)
        (.beginFrame interactions/hit-map)
        (let [frame (cap/capture! {:cols width
                                   :rows 12
                                   :paint! (fn [{:keys [g]}]
                                             (render/draw-chat-bubble! g
                                                                       collapsed
                                                                       0
                                                                       0
                                                                       width
                                                                       {:viewport-top 0
                                                                        :viewport-h 12}))})]
          (.commitFrame interactions/hit-map)
          (expect (nil? (:error frame)))
          (expect (some (fn [row]
                          (= (:node-id control) (:node-id (.lookup interactions/hit-map 3 row))))
                        (range 12))
                  "The existing mouse and keyboard disclosure map can open the request"))))))

(defdescribe
  council-prose-uses-justice-layout
  (it
    "council prose uses justice layout"
    (let [prose
          (str "A quiet paragraph can become much more comfortable when its lines share "
               "a reasonably even rhythm of spaces instead of alternating between very tight "
               "and very loose arrangements.")

          message
          {:role :user :request-kind :council :text prose :session-turn-id "council-turn"}

          opts
          {:session-id "session" :detail-expansions {}}

          collapsed
          (virtual/project-message message 28 {} opts)

          control
          (last (:line-meta collapsed))

          expanded
          (virtual/project-message message
                                   28
                                   {}
                                   (assoc opts
                                     :detail-expansions {["session" (:node-id control)] true}))

          lines
          (mapv visible (butlast (:prewrapped-lines expanded)))]

      (expect (= ["A quiet paragraph" "can become much more" "comfortable when its"
                  "lines share a reasonably" "even rhythm of spaces" "instead of alternating"
                  "between very tight and" "very loose arrangements."]
                 (mapv #(str/replace % #" +" " ") lines)))
      (expect (every? #(= 24 (p/display-width %)) (butlast lines)))
      (expect (= "very loose arrangements." (last lines)))
      (expect (= (subvec lines 0 4) (mapv visible (subvec (:prewrapped-lines collapsed) 0 4))))
      (expect (= :toggle-details (:kind control))))))

(defdescribe
  council-markdown-keeps-authored-lines-apart
  ;; User report 2026-10-31, Council thread #8805: the request showed raw backticks
  ;; and ran its authored lines together, with no gap between them.
  (it "renders Markdown and keeps each authored line as its own block"
      (let [text
            "Context: run `npm test`.\nGoals:\n1. First goal\n2. Second goal\nDone: tests pass."

            message
            {:role :user :request-kind :council :text text :session-turn-id "council-turn"}

            opts
            {:session-id "session" :detail-expansions {}}

            control
            (last (:line-meta (virtual/project-message message 60 {} opts)))

            expanded
            (virtual/project-message message
                                     60
                                     {}
                                     (assoc opts
                                       :detail-expansions {["session" (:node-id control)] true}))

            rows
            (vec (butlast (:prewrapped-lines expanded)))]

        (expect (= ["Context: run npm test." "" "Goals:" "" "1. First goal" "2. Second goal" ""
                    "Done: tests pass."]
                   (mapv (comp str/trimr visible) rows)))
        (expect (str/includes? (first rows) (str p/INLINE_CODE_ON "npm test" p/INLINE_CODE_OFF))))))

(defdescribe council-markdown-splits-only-lines-that-leave-a-block
             ;; User report 2026-10-31, Council thread #8805: authored lines must read as separate blocks.
             (it "splits plain lines and keeps lines that belong to one block joined"
                 (doseq [[input expected]
                         [["One.\nTwo." "One.\n\nTwo."] ["- a\n- b\nAfter." "- a\n- b\n\nAfter."]
                          ["1. a\n   more\n2. b" "1. a\n   more\n2. b"]
                          ["Intro:\n```\nx\ny\n```\nAfter." "Intro:\n\n```\nx\ny\n```\n\nAfter."]
                          ["| a | b |\n| - | - |\n| 1 | 2 |" "| a | b |\n| - | - |\n| 1 | 2 |"]
                          ["> a\n> b" "> a\n> b"] ["Title\n---" "Title\n---"]
                          ["One.\r\nTwo.\n\nThree." "One.\n\nTwo.\n\nThree."]]]
                   (expect (= expected (presentation/council-markdown input)) input))))

(defdescribe short-council-requests-do-not-grow-a-control
             (it "short council requests do not grow a control"
                 (let [projected (virtual/project-message
                                   {:role :user :request-kind :council :text "Short request"}
                                   40
                                   {}
                                   {:session-id "session"})]
                   (expect (= ["Short request"] (mapv visible (:prewrapped-lines projected))))
                   (expect (every? nil? (:line-meta projected))))))
