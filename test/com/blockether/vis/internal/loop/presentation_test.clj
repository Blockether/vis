(ns com.blockether.vis.internal.loop.presentation-test
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.iteration :as iteration]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [lazytest.core :refer [defdescribe it expect]]))

(def ^:private environment {:turn/features {"voice_projection" true "voice_segments" true}})

(def ^:private model {:provider :zai-coding-plan :name "glm-5.1"})

(def ^:private answer
  "{\"type\":\"conversation\",\"text\":\"The check passed.\",\"phase\":\"answer\"}\n")

(def ^:private progress
  "{\"type\":\"conversation\",\"text\":\"I will check.\",\"phase\":\"progress\"}\n")

(def ^:private visual "{\"type\":\"visual\",\"markdown\":\"```mermaid\\ngraph TD; A-->B\\n```\"}\n")

(defn- response
  [text]
  {:stop-reason :end
   :tool-calls []
   :content text
   :api-usage {:input-tokens 10 :output-tokens 5}
   :duration-ms 2})

(defn- present
  [raw repair]
  (let [calls
        (atom [])

        result
        (with-redefs [transcript/ask-code-with-session!
                      (fn [env resolved opts]
                        (swap! calls conj {:environment env :model resolved :opts opts})
                        (if (instance? Exception repair) (throw repair) repair))]
          (#'iteration/final-presentation
           environment
           model
           {:messages [{:role "user" :content "Check it"}]
            :tools [{:name "python_execution"}]
            :on-chunk identity
            :cancel-fn (constantly false)}
           (response raw)))]

    {:result result :calls @calls}))

(defdescribe
  presentation-test
  ;; Regression: session a853a9c9-6296-42f9-ab32-67ee83447023 had prose without a spoken outcome.
  (it "accepts conversation and rich visuals without repair"
      (let [{:keys [result calls]} (present (str answer visual) nil)]
        (expect (empty? calls))
        (expect (= ["conversation" "visual"] (mapv #(get % "type") (:presentation-content result))))
        (expect (str/includes? (get (second (:presentation-content result)) "markdown")
                               "mermaid"))))
  (it "repairs raw prose once without tools or live chunks"
      (let [{:keys [result calls]}
            (present "The check passed." (response answer))

            opts
            (:opts (first calls))]

        (expect (= 1 (count calls)))
        (expect (= [] (:tools opts)))
        (expect (= :none (:tool-choice opts)))
        (expect (nil? (:on-chunk opts)))
        (expect (some? (:cancel-fn opts)))
        (expect (= {:provider :zai-coding-plan :model "glm-5.1"} (:routing opts)))
        (expect (= :repaired (:presentation-status result)))
        (expect (= "The check passed." (:content result)))
        (expect (= 20 (get-in result [:api-usage :input-tokens])))))
  ;; Regression: the iPhone reply repair used an invalid keyword effort before inference.
  (it
    "repairs with a supported effort without inheriting the task effort"
    (doseq [supported [["low" "medium" "high"] ["medium" "high"]]]
      (let [repair-model {:name "gpt-6.1-sol"
                          :reasoning? true
                          :reasoning-options [{:type "effort" :values supported}]}
            calls (atom [])
            result (with-redefs [transcript/ask-code-with-session!
                                 (fn [_ _ opts]
                                   (swap! calls conj opts)
                                   (let [effort (or (:reasoning-effort opts)
                                                    (svar/nearest-reasoning-effort
                                                      :openai-compatible-responses
                                                      repair-model
                                                      (:preferred-reasoning-effort opts)))
                                         resolution (svar/resolve-reasoning-effort
                                                      :openai-compatible-responses
                                                      repair-model
                                                      effort)]

                                     (when-not (:effective resolution)
                                       (throw (ex-info "Unsupported repair effort" resolution)))
                                     (response answer)))]
                     (#'iteration/final-presentation
                      environment
                      {:provider :openai-codex :name (:name repair-model)}
                      {:messages [{:role "user" :content "Check it"}]
                       :reasoning-effort "high"
                       :preferred-reasoning-effort "high"}
                      (response "The check passed.")))]

        (expect (= 1 (count @calls)))
        (expect (nil? (:reasoning-effort (first @calls))))
        (expect (= "low" (:preferred-reasoning-effort (first @calls))))
        (expect (= :repaired (:presentation-status result)))
        (expect (= "The check passed." (get (first (:presentation-content result)) "text"))))))
  (it "keeps committed progress and visuals with their original IDs"
      (let [prefix
            (str progress visual)

            {:keys [result calls]}
            (present prefix (response answer))]

        (expect (= 1 (count calls)))
        (expect (= (mapv :block (content/presentation-records prefix false))
                   (subvec (:presentation-content result) 0 2)))
        (expect (= "answer" (get (last (:presentation-content result)) "phase")))))
  (it "rejects a progress-only repair without starting another request"
      (let [{:keys [result calls]} (present progress (response progress))]
        (expect (= 1 (count calls)))
        (expect (= :failed (:presentation-status result)))
        (expect (= "presentation_unavailable" (get (last (:presentation-content result)) "code")))
        (expect (not-any? #(= "error" (get % "type")) (:presentation-content result)))))
  (it "does not execute tool calls returned by a repair"
      (let [{:keys [result calls]} (present "done"
                                            {:stop-reason :tool-calls
                                             :tool-calls [{:name "python_execution"
                                                           :input {:code "print(42)"}}]})]
        (expect (= 1 (count calls)))
        (expect (= :failed (:presentation-status result)))
        (expect (empty? (:tool-calls result)))))
  (it "preserves safe visual content after failed repair"
      (let [{:keys [result]} (present (str visual "invalid") (response "invalid"))]
        (expect (= "visual" (get (first (:presentation-content result)) "type")))
        (expect (= "notice" (get (last (:presentation-content result)) "type")))))
  (it "turns a repair transport failure into a presentation notice"
      (let [{:keys [result calls]} (present "done" (ex-info "offline" {}))]
        (expect (= 1 (count calls)))
        (expect (= :failed (:presentation-status result)))))
  (it "propagates interruption rather than hiding cancellation"
      (expect
        (try (present "done" (InterruptedException.)) false (catch InterruptedException _ true))))
  (it "accepts explicit silence without repair"
      (let [{:keys [result calls]} (present "{\"type\":\"silence\",\"reason\":\"user_requested\"}"
                                            nil)]
        (expect (empty? calls))
        (expect (= "silence" (get (first (:presentation-content result)) "type")))))
  (it "leaves non-live speech projections unchanged"
      (let [result (response "Done.\n```vis-speech\nDone.\n```\n")]
        (with-redefs [transcript/ask-code-with-session! (fn [& _]
                                                          (throw (ex-info "unexpected repair" {})))]
          (expect (= result
                     (#'iteration/final-presentation
                      {:turn/features {"voice_projection" true}}
                      model
                      {}
                      result))))))
  (it "never repairs a tool iteration"
      (let [result
            {:stop-reason :tool-calls :tool-calls [{:name "python_execution"}] :content progress}

            presented
            (#'iteration/final-presentation environment model {} result)]

        (expect (= result (dissoc presented :presentation-content)))
        (expect (= "progress" (get (first (:presentation-content presented)) "phase")))))
  (it "rejects replay of an already committed conversation"
      (let [{:keys [result calls]} (present (str answer "invalid") (response answer))]
        (expect (= 1 (count calls)))
        (expect (= :failed (:presentation-status result)))
        (expect (= 1
                   (count (filter #(= "conversation" (get % "type"))
                                  (:presentation-content result)))))))
  (it "propagates wrapped cancellation"
      (expect (try (present "done" (ex-info "cancelled" {} (InterruptedException.)))
                   false
                   (catch clojure.lang.ExceptionInfo e
                     (instance? InterruptedException (ex-cause e))))))
  (it "finalizes presentation failure without retrying the task"
      (doseq [[repair expected-status expected-type] [[answer :repaired "conversation"]
                                                      ["invalid" :failed "notice"]]]
        (let [env (assoc (loop-env/create-environment ::router {:db :memory})
                    :turn/features (:turn/features environment))
              calls (atom 0)]

          (try (with-redefs [transcript/ask-code-with-session!
                             (fn [_ _ opts]
                               (swap! calls inc)
                               (if (= 1 @calls)
                                 (response "done")
                                 (do (expect (= :none (:tool-choice opts))) (response repair))))]
                 (let [result (iteration/run-iteration env [] {:iteration 0 :resolved-model model})]
                   (expect (= 2 @calls))
                   (expect (= expected-status (:presentation-status result)))
                   (expect (empty? (:blocks result)))
                   (expect (= expected-type
                              (get (first (get-in result [:final-result :answer])) "type")))))
               (finally (loop-env/dispose-environment! env)))))))
