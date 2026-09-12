(ns com.blockether.vis.tui.council-transcript-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.virtual :as virtual]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(def request-text
  "Actual peer request.\nSecond line.\nThird line.\nFourth line.\nLast line remains available.")

(def council-entry {"entry_id" 42 "thread_id" 42 "kind" "coordination" "content" request-text})

(deftest council-wire-history-and-live-events-retain-provenance
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

    (is (= :council (:request-kind message)))
    (is (= council-entry (:council message)))
    (is (= request-text (:text message)))
    (is (= :council (:request-kind event)))
    (is (= council-entry (:council event)))))

(deftest council-preview-counts-rendered-rows-and-keeps-a-scoped-disclosure
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

      (is (= 5 (count (:prewrapped-lines collapsed))) "Four visible body rows and one control")
      (is (str/includes? (:text collapsed) "Show full message"))
      (is (= :toggle-details (:kind control)))
      (is (true? (:collapsed? control)))
      (is (> (count (:prewrapped-lines expanded)) 5))
      (is (str/includes? (:text expanded) "Show less"))
      (is (false? (:collapsed? (last (:line-meta expanded)))))
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
        (is (nil? (:error frame)))
        (is (str/includes? (cap/frame-text frame) "Council"))
        (is (some (fn [row]
                    (= (:node-id control) (:node-id (.lookup interactions/hit-map 3 row))))
                  (range 12))
            "The existing mouse and keyboard disclosure map can open the request")))))

(deftest short-council-requests-do-not-grow-a-control
  (let [projected (virtual/project-message
                    {:role :user :request-kind :council :text "Short request"}
                    40
                    {}
                    {:session-id "session"})]
    (is (= ["Short request"] (:prewrapped-lines projected)))
    (is (every? nil? (:line-meta projected)))))
