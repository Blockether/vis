(ns com.blockether.vis.tui.goal-usage-footer-test
  "Goal usage footers through live completion, persisted history and terminal layout."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.state :as state]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private goal-turn
  {"turn_id" "goal-usage"
   "position" 1
   "request" "/goal Verify the usage footer"
   "status" "completed"
   "content" [{"id" "goal-answer" "type" "prose" "markdown" "Goal verified."}]
   "provider" "openai"
   "model" "gpt-test"
   "input_tokens" 100
   "output_tokens" 20
   "input_cache_read_tokens" 70
   "total_cost" 0.0123
   "duration_ms" 1200
   "iterations" [{"position" 1 "llm_actual" {"provider" "openai" "model" "gpt-test"} "forms" []}]})

(defn- live-goal-message
  "Settle the real optimistic slash placeholder with recorded model usage."
  [status]
  (with-redefs [client/registered-slashes (constantly [{:slash/name :goal :slash/parent []}])]
    (let [pending (#'state/pending-assistant-for (get goal-turn "request"))
          response (#'state/completion-response
                    (get goal-turn "content")
                    []
                    1200
                    {:status status
                     :provider "openai"
                     :model "gpt-test"
                     :llm-actual {"provider" "openai" "model" "gpt-test"}
                     :tokens {"input" 100 "output" 20 "cached" 70}
                     :cost {"total_cost" 0.0123}})]

      (peek (#'state/replace-pending-assistant [pending] response)))))

(defn- capture-bubble
  "Draw a production bubble and retain its pixels and both height calculations."
  [message]
  (let [message
        (assoc message :message-meta-mode :full)

        captured
        (cap/capture! {:cols 100
                       :rows 20
                       :paint! (fn [{:keys [g]}]
                                 (render/draw-chat-bubble! g message 0 0 100 {:viewport-h 20}))})]

    {:text (cap/frame-text (first (:frames captured)))
     :draw-height (:ret captured)
     :layout-height (render/bubble-height message 100)
     :error (:error captured)}))

(defdescribe
  goal-usage-footer-test
  ;; Regression: /goal runs the model loop, but inherits the optimistic slash marker.
  (describe "live goal completion"
            (it "shows the actual provider, model, tokens and price even after cancellation"
                (doseq [status [:completed :cancelled :error]]
                  (let [message (live-goal-message status)
                        {:keys [text draw-height layout-height error]} (capture-bubble message)]

                    (expect (:slash? message))
                    (expect (nil? error))
                    (expect (str/includes? text "Goal verified."))
                    (expect (str/includes? text "openai/gpt-test"))
                    (expect (str/includes? text "100→20 (cached 70)"))
                    (expect (str/includes? text "~$0.0123"))
                    (expect (= draw-height layout-height))
                    (expect (= draw-height
                               (:draw-height (capture-bubble (dissoc message :slash?)))))))))
  (describe "persisted goal history"
            (it "keeps usage after the transcript is restored"
                (doseq [outcome ["complete" "cancelled" "error"]]
                  (let [message (peek (#'chat/turns->messages
                                       [(assoc goal-turn "prior_outcome" outcome)]))
                        {:keys [text draw-height layout-height error]} (capture-bubble message)]

                    (expect (nil? error))
                    (expect (str/includes? text "Goal verified."))
                    (expect (str/includes? text "openai/gpt-test"))
                    (expect (str/includes? text "100→20 (cached 70)"))
                    (expect (str/includes? text "~$0.0123"))
                    (expect (= draw-height layout-height))))))
  (describe "provider usage determines command metadata"
            (it "shows tokens-only and cost-only goal usage"
                (doseq [usage [{:tokens {"input" 100 "output" 0} :cost 0}
                               {:tokens {"input" 0 "output" 0} :cost 0.0123}]]
                  (let [message (merge (live-goal-message :completed) usage)
                        {:keys [text draw-height layout-height]} (capture-bubble message)]

                    (expect (str/includes? text "openai/gpt-test"))
                    (expect (= draw-height layout-height)))))
            (it "omits local-command footers and keys cached layout by the command marker"
                (let [command
                      (assoc (live-goal-message :completed)
                        :tokens {"input" 0 "output" 0}
                        :cost 0)

                      model-turn
                      (dissoc command :slash?)

                      model-height
                      (render/bubble-height model-turn 100)

                      {:keys [text draw-height layout-height]}
                      (capture-bubble command)]

                  (expect (not (str/includes? text "openai/gpt-test")))
                  (expect (= draw-height layout-height))
                  (expect (= 2 (- model-height layout-height)))))
            (it "omits empty cancellations even when routing metadata is present"
                (let [message
                      (assoc (live-goal-message :cancelled)
                        :tokens {"input" 0 "output" 0}
                        :cost 0)

                      {:keys [text draw-height layout-height]}
                      (capture-bubble message)]

                  (expect (not (str/includes? text "openai/gpt-test")))
                  (expect (= draw-height layout-height))))))

(defdescribe output-rate-footer-test
             (it "shows the output rate in live and restored footers"
                 (let [live
                       (#'state/completion-response
                        (get goal-turn "content")
                        []
                        1200
                        {:provider "openai"
                         :model "gpt-test"
                         :llm-actual {"provider" "openai" "model" "gpt-test"}
                         :tokens {"input" 100 "output" 20 "cached" 70}
                         :tokens-per-second 16.666
                         :cost {"total_cost" 0.0123}})

                       restored
                       (peek (#'chat/turns->messages
                              [(assoc goal-turn "tokens_per_second" 16.666)]))]

                   (doseq [message [live restored]]
                     (let [{:keys [text draw-height layout-height error]} (capture-bubble message)]
                       (expect (nil? error))
                       (expect (str/includes? text "100→20 (cached 70)"))
                       (expect (str/includes? text "16.7 tok/s"))
                       (expect (= draw-height layout-height)))))))
