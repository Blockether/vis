(ns com.blockether.vis.ext.provider-alibaba-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-alibaba :as alibaba]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  provider-registration-test
  (it "registers both Alibaba plans as separate provider extension entries"
      (require 'com.blockether.vis.ext.provider-alibaba :reload)
      (let [coding
            (vis/provider-by-id :alibaba-coding-plan)

            token
            (vis/provider-by-id :alibaba-token-plan)

            ext-names
            (set (map :ext/name (vis/registered-extensions)))]

        (expect (= :alibaba-coding-plan (:provider/id coding)))
        (expect (= :alibaba-token-plan (:provider/id token)))
        (expect (contains? ext-names "provider-alibaba"))
        ;; The ids ARE the models.dev slugs and the base URLs are the ones the
        ;; catalog publishes for them - that pairing is what resolves pricing
        ;; and context windows for the preset models.
        (expect (= "https://coding-intl.dashscope.aliyuncs.com/v1"
                   (get-in coding [:provider/preset :base-url])))
        (expect (= "https://token-plan.ap-southeast-1.maas.aliyuncs.com/compatible-mode/v1"
                   (get-in token [:provider/preset :base-url])))
        (expect (ifn? (:provider/limits-fn coding)))
        (expect (ifn? (:provider/limits-fn token)))
        (expect (ifn? (:provider/auth-prompt-fn coding)))
        (expect (ifn? (:provider/auth-prompt-fn token)))))
  (it "presets the models each endpoint actually serves, with a window for the uncatalogued one"
      (require 'com.blockether.vis.ext.provider-alibaba :reload)
      (let [coding-models
            (get-in (vis/provider-by-id :alibaba-coding-plan) [:provider/preset :default-models])

            token-models
            (get-in (vis/provider-by-id :alibaba-token-plan) [:provider/preset :default-models])]

        (expect (= "qwen3-coder-plus" (first coding-models)))
        (expect (contains? (set coding-models) "glm-5"))
        ;; `qwen3.8-max` is served live but ships as `qwen3.8-max-preview` in
        ;; the models.dev catalog, so its window is declared inline.
        (expect (= {:name "qwen3.8-max" :context 1000000 :output-limit 131072 :tool-call? true}
                   (first token-models)))
        (expect (contains? (set (rest token-models)) "deepseek-v4-pro")))))

(defdescribe
  auth-prompt-test
  (it "exposes static API-key guidance per plan"
      (require 'com.blockether.vis.ext.provider-alibaba :reload)
      (let [coding-lines
            ((:provider/auth-prompt-fn (vis/provider-by-id :alibaba-coding-plan)))

            token-lines
            ((:provider/auth-prompt-fn (vis/provider-by-id :alibaba-token-plan)))]

        (expect (some #(= "  Alibaba (Coding Plan) requires a static API key." %) coding-lines))
        (expect (some #(= "         export ALIBABA_CODING_PLAN_API_KEY=<your-alibaba-api-key>" %)
                      coding-lines))
        (expect (some #(= "  Endpoint: https://coding-intl.dashscope.aliyuncs.com/v1" %)
                      coding-lines))
        (expect (some #(= "         export ALIBABA_TOKEN_PLAN_API_KEY=<your-alibaba-api-key>" %)
                      token-lines)))))

(defdescribe
  auth-detection-test
  (it "detects the TUI/config API key used by runtime model calls"
      (require 'com.blockether.vis.ext.provider-alibaba :reload)
      (with-redefs-fn {#'alibaba/load-auth-file (constantly nil)
                       #'alibaba/env-key-for-plan (constantly nil)
                       #'vis/current-config (constantly {:providers [{:id :alibaba-token-plan
                                                                      :api-key "config-key"}]})}
        (fn []
          (expect (= {:api-key "config-key" :source :config} (#'alibaba/detect-key :token)))
          ;; Plan-scoped: the Token Plan key must never authenticate the
          ;; Coding Plan, which the live endpoints reject with 401.
          (expect (nil? (#'alibaba/detect-key :coding))))))
  (it "reads each plan's own env var and never the sibling's"
      (require 'com.blockether.vis.ext.provider-alibaba :reload)
      (with-redefs-fn {#'alibaba/load-auth-file (constantly nil)
                       #'alibaba/configured-key-for-plan (constantly nil)
                       #'vis/current-config (constantly {:providers []})}
        (fn []
          (with-redefs-fn {#'alibaba/env-key-for-plan (fn [plan-tag]
                                                        (when (= :coding plan-tag) "env-key"))}
            (fn []
              (expect (= {:api-key "env-key" :source :env-var} (#'alibaba/detect-key :coding)))
              (expect (nil? (#'alibaba/detect-key :token)))))))))

(defdescribe
  get-token-test
  (it "answers the plan's own endpoint in the token envelope"
      (require 'com.blockether.vis.ext.provider-alibaba :reload)
      (with-redefs-fn {#'alibaba/detect-key (constantly {:api-key "k" :source :auth-file})}
        (fn []
          (expect (= {:token "k"
                      :api-url
                      "https://token-plan.ap-southeast-1.maas.aliyuncs.com/compatible-mode/v1"}
                     ((:provider/get-token-fn (vis/provider-by-id :alibaba-token-plan))))))))
  (it "fails fast with the plan's auth command when no key exists anywhere"
      (require 'com.blockether.vis.ext.provider-alibaba :reload)
      (with-redefs-fn {#'alibaba/detect-key (constantly nil)}
        (fn []
          (let [thrown (try ((:provider/get-token-fn (vis/provider-by-id :alibaba-coding-plan)))
                            nil
                            (catch clojure.lang.ExceptionInfo e e))]
            (expect (some? thrown))
            (expect (= :vis/alibaba-not-authenticated (:type (ex-data thrown))))
            (expect (= :alibaba-coding-plan (:provider-id (ex-data thrown))))
            (expect (re-find #"ALIBABA_CODING_PLAN_API_KEY" (ex-message thrown))))))))

(defdescribe limits-test
             (it
               "reports :ok with a console note - neither plan exposes a quota endpoint"
               (require 'com.blockether.vis.ext.provider-alibaba :reload)
               (with-redefs-fn {#'alibaba/detect-key (constantly {:api-key "k" :source :auth-file})}
                 (fn []
                   (let [report (vis/provider-limits :alibaba-token-plan)]
                     (expect (= :alibaba-token-plan (:provider-id report)))
                     (expect (= :ok (:status report)))
                     (expect (= [] (get-in report [:dynamic :limits])))
                     (expect (re-find #"Model Studio console" (get-in report [:dynamic :note])))))))
             (it "reports :unauthenticated when the plan key is absent"
                 (require 'com.blockether.vis.ext.provider-alibaba :reload)
                 (with-redefs-fn {#'alibaba/detect-key (constantly nil)}
                   (fn []
                     (let [report ((:provider/limits-fn (vis/provider-by-id :alibaba-coding-plan)))]
                       (expect (= :alibaba-coding-plan (:provider-id report)))
                       (expect (= :unauthenticated (:status report)))
                       (expect (= [] (get-in report [:dynamic :limits]))))))))

(defdescribe status-test
             (it "previews the key without exposing it"
                 (require 'com.blockether.vis.ext.provider-alibaba :reload)
                 (with-redefs-fn {#'alibaba/detect-key
                                  (constantly {:api-key "sk-sp-0123456789abcdef" :source :env-var})}
                   (fn []
                     (let [report ((:provider/status-fn (vis/provider-by-id :alibaba-token-plan)))]
                       (expect (true? (:is-authenticated report)))
                       (expect (= :env-var (:source report)))
                       ;; 8-char prefix only - the rest of the key never reaches the UI.
                       (expect (= "sk-sp-01..." (:api-key-preview report))))))))
