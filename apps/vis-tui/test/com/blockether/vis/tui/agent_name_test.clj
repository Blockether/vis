(ns com.blockether.vis.tui.agent-name-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.render :as render]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(deftest gateway-name-paints-the-transcript
  (doseq [agent-name ["Ada" "助手" "Vis"]]
    (let [workspace {"root" "/remote/project" "agent_name" agent-name}
          captured (cap/capture! {:cols 40
                                  :rows 12
                                  :paint! (fn [{:keys [g]}]
                                            (render/draw-messages-area!
                                              g
                                              {:visible [{:top 0
                                                          :projected {:role :assistant
                                                                      :text "Done."}}]
                                               :total-h 4
                                               :eff-scroll 0}
                                              0 12
                                              40 (get workspace "agent_name")))})]

      (is (nil? (:error captured)))
      (is (str/includes? (cap/frame-text captured) agent-name))
      (is (str/includes? (cap/frame-text captured) "Done.")))))

(deftest names-are-clipped-and-user-labels-stay-unchanged
  (let [captured
        (cap/capture!
          {:cols 20
           :rows 10
           :paint!
           (fn [{:keys [g]}]
             (render/draw-chat-bubble! g
                                       {:role :assistant :text "Done."}
                                       0 0
                                       12 {:agent-name "A very long agent name"})
             (render/draw-chat-bubble! g {:role :user :text "Hello"} 5 0 12 {:agent-name "Ada"}))})

        lines
        (str/split-lines (cap/frame-text captured))]

    (is (nil? (:error captured)))
    (is (str/includes? (first lines) "…"))
    (is (str/blank? (subs (first lines) 12)))
    (is (str/includes? (cap/frame-text captured) "You"))
    (is (not (str/includes? (cap/frame-text captured) "Ada")))))

(deftest progress-uses-the-same-gateway-name
  (doseq [progress [{:iterations []} {:iterations [{:thinking "Working"}]}]]
    (let [text (:text (render/progress->lines-data
                        progress
                        80
                        {}
                        {:agent-name "Ada" :now-ms 1000 :turn-start-ms 0}))]
      (is (str/includes? text "Ada is"))
      (is (not (str/includes? text "Vis is"))))))
