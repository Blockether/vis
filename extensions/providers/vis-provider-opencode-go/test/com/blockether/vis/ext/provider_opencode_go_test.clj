(ns com.blockether.vis.ext.provider-opencode-go-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-opencode-go :as opencode-go]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- reload! [] (require 'com.blockether.vis.ext.provider-opencode-go :reload))

(defdescribe provider-registration-test
             (it "registers ONE OpenCode Go provider that serves both wire dialects"
                 (reload!)
                 (let [ext-nses (set (map :ext/name (vis/registered-extensions)))]
                   (expect (contains? ext-nses "provider-opencode-go")))
                 ;; ONE provider — no more :opencode-go-anthropic split.
                 (let [provider (vis/provider-by-id :opencode-go)]
                   (expect (some? provider))
                   (expect (= :opencode-go (:provider/id provider)))
                   (expect (string? (:provider/label provider)))
                   (expect (= "https://opencode.ai/zen/go/v1"
                              (get-in provider [:provider/preset :base-url])))
                   ;; Provider-level :api-style is nil — svar defaults to OpenAI chat.
                   (expect (nil? (get-in provider [:provider/preset :api-style])))
                   (expect (seq (get-in provider [:provider/preset :default-models])))
                   (expect (every? #(or (string? %) (map? %))
                                   (get-in provider [:provider/preset :default-models])))
                   (expect (ifn? (:provider/get-token-fn provider)))
                   (expect (ifn? (:provider/auth-prompt-fn provider)))
                   (expect (ifn? (:provider/limits-fn provider))))))

(defdescribe per-model-wire-routing-test
             (it "declares OpenAI-wire models as bare strings (svar default chat wire)"
                 (reload!)
                 (let
                   [models
                    (get-in (vis/provider-by-id :opencode-go) [:provider/preset :default-models])

                    bare
                    (filter string? models)]

                   ;; GLM / Kimi / DeepSeek / MiMo / Hy3 ride the OpenAI chat wire.
                   (expect (some #(= "glm-5.2" %) bare))
                   (expect (some #(= "kimi-k2.7-code" %) bare))
                   (expect (some #(= "deepseek-v4-flash" %) bare))
                   (expect (some #(= "hy3" %) bare))))
             (it "declares Anthropic-wire models as maps with :api-style :anthropic"
                 (reload!)
                 (let
                   [models
                    (get-in (vis/provider-by-id :opencode-go) [:provider/preset :default-models])

                    styled
                    (filter map? models)]

                   ;; MiniMax / Qwen ride the Anthropic Messages wire via per-model override.
                   (expect (seq styled))
                   (expect (every? #(= :anthropic (:api-style %)) styled))
                   (expect (some #(re-find #"minimax" (:name %)) styled))
                   (expect (some #(re-find #"qwen" (:name %)) styled)))))

(defdescribe
  shared-key-auth-test
  (it "resolves the key + endpoint for the single provider"
      (reload!)
      (with-redefs-fn {#'opencode-go/detect-key (constantly {:api-key "k" :source :env-var})}
        (fn []
          (let [token ((:provider/get-token-fn (vis/provider-by-id :opencode-go)))]
            (expect (= {:token "k" :api-url "https://opencode.ai/zen/go/v1"} token))))))
  (it "detects the key from the OPENCODE_API_KEY env var"
      (reload!)
      (with-redefs-fn {#'opencode-go/configured-key (constantly nil)
                       #'opencode-go/load-auth-file (constantly nil)
                       #'opencode-go/env-key (constantly "env-key")}
        (fn []
          (expect (= {:api-key "env-key" :source :env-var} (#'opencode-go/detect-key))))))
  (it "throws a pointer at the auth command when unauthenticated"
      (reload!)
      (with-redefs-fn {#'opencode-go/detect-key (constantly nil)}
        (fn []
          (expect (= :vis/opencode-go-not-authenticated
                     (try ((:provider/get-token-fn (vis/provider-by-id :opencode-go)))
                          nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))))

(defdescribe auth-prompt-test
             (it "exposes static API-key guidance"
                 (reload!)
                 (let [lines ((:provider/auth-prompt-fn (vis/provider-by-id :opencode-go)))]
                   (expect (some #(= "  OpenCode Go requires a static API key." %) lines))
                   (expect (some #(= "         export OPENCODE_API_KEY=<your-opencode-go-api-key>" %)
                                 lines))
                   (expect (some #(= "  Endpoint: https://opencode.ai/zen/go/v1" %) lines)))))

(defdescribe limits-test
             (it "reports :ok and a flat-rate note when authenticated"
                 (reload!)
                 (with-redefs-fn {#'opencode-go/detect-key (constantly {:api-key "k"
                                                                        :source :env-var})}
                   (fn []
                     (let [report ((:provider/limits-fn (vis/provider-by-id :opencode-go)))]
                       (expect (= :opencode-go (:provider-id report)))
                       (expect (= :ok (:status report)))
                       (expect (= [] (get-in report [:dynamic :limits])))
                       (expect (some? (get-in report [:dynamic :note])))))))
             (it "reports :unauthenticated when no key is available"
                 (reload!)
                 (with-redefs-fn {#'opencode-go/detect-key (constantly nil)}
                   (fn []
                     (let [report ((:provider/limits-fn (vis/provider-by-id :opencode-go)))]
                       (expect (= :opencode-go (:provider-id report)))
                       (expect (= :unauthenticated (:status report)))
                       (expect (= [] (get-in report [:dynamic :limits]))))))))
