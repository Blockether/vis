(ns com.blockether.vis.tui.model-footer-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.model-footer :as model-footer]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe model-footer-test
             (it "renders the model picker shortcut"
                 (let [segments
                       (model-footer/segments {:session-model-pref {:provider "openai-codex"
                                                                    :model "gpt-5.5"}}
                                              0)

                       label
                       (get-in (first segments) [:ast 2 2 2])]

                   (expect (str/includes? label "(C-x c)")))))

(defdescribe
  model-footer-session-default-test
  ;; Regression, issue #311: a project `.vis/config.yml` overlay sets the session's
  ;; default. The footer named the GLOBAL router default.
  (it "names the session's own default when the session pins no model"
      (with-redefs [vis/get-router
                    (constantly {:providers [{:id :anthropic-coding-plan
                                              :is-default true
                                              :default-model "claude-opus-5-5"
                                              :models [{:name "claude-opus-5-5"}]}
                                             {:id :openai-codex
                                              :default-model "gpt-6-astra"
                                              :models [{:name "gpt-6-astra"}
                                                       {:name "gpt-6-luna"}]}]})

                    vis/gateway-session-model-cached
                    (constantly nil)

                    vis/gateway-session-default-model-cached
                    (fn [sid]
                      (when (= "s1" sid) {:provider "openai-codex" :model "gpt-6-luna"}))]

        (let [label (get-in (first (model-footer/segments {:session {:id "s1"}} 0)) [:ast 2 2 2])]
          (expect (str/includes? label "openai-codex/gpt-6-luna")))))
  ;; A project can also define a provider or a model that the global fleet does not
  ;; list. The footer showed the global default model in place of that pair.
  (it "names a default pair that the global fleet does not list"
      (with-redefs [vis/get-router
                    (constantly {:providers [{:id :anthropic-coding-plan
                                              :is-default true
                                              :default-model "claude-opus-5-5"
                                              :models [{:name "claude-opus-5-5"}]}
                                             {:id :openai-codex
                                              :default-model "gpt-6-astra"
                                              :models [{:name "gpt-6-astra"}]}]})

                    vis/gateway-session-model-cached
                    (constantly nil)

                    vis/gateway-session-default-model-cached
                    (fn [sid]
                      (get {"s1" {:provider "openai-codex" :model "gpt-6-luna"}
                            "s2" {:provider "project-llm" :model "local-1"}}
                           sid))]

        (let [label (fn [sid]
                      (get-in (first (model-footer/segments {:session {:id sid}} 0)) [:ast 2 2 2]))]
          (expect (str/includes? (label "s1") "openai-codex/gpt-6-luna"))
          (expect (str/includes? (label "s2") "project-llm/local-1"))))))
