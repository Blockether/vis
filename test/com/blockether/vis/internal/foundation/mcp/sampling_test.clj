(ns com.blockether.vis.internal.foundation.mcp.sampling-test
  (:require [com.blockether.vis.internal.foundation.mcp.sampling :as sampling]
            [com.blockether.vis.internal.gateway.wiring :as wiring]
            [com.blockether.vis.internal.loop.router :as router]
            [com.blockether.vis.internal.view.core :as view]
            [lazytest.core :refer [defdescribe expect it]]))

(wiring/install!)

(defn- ask
  "The answer to a sampling request when the person allows it (`allowed?`) or
   refuses it. `prompts` collects what goes to the model."
  [allowed? prompts]
  (with-redefs [view/request!
                (fn [request]
                  ;; The person sees a real View.
                  (view/normalize-request request)
                  {:is-submitted allowed?})

                router/llm-text!
                (fn [opts]
                  (swap! prompts conj opts)
                  {:text "Paris"})]

    (sampling/ask! "session-1"
                   "atlas"
                   {"systemPrompt" "Be short."
                    "messages" [{"role" "user"
                                 "content" {"type" "text" "text" "Capital of France?"}}]
                    "maxTokens" 20})))

(defdescribe sampling-test
             (it "asks the model only after the person allows the request"
                 (let [prompts (atom [])]
                   (expect (= {"role" "assistant"
                               "content" {"type" "text" "text" "Paris"}
                               "model" "vis"
                               "stopReason" "endTurn"}
                              (ask true prompts)))
                   (expect (= [{:prompt "Capital of France?" :system "Be short."}] @prompts))))
             (it "sends nothing to a model when the person refuses"
                 (let [prompts (atom [])]
                   (expect (nil? (ask false prompts)))
                   (expect (empty? @prompts)))))
