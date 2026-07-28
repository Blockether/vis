(ns com.blockether.vis.ext.provider-openrouter-test
  (:require [com.blockether.svar.core :as svar]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-openrouter :as openrouter]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-registration-test
             (it "registers the OpenRouter provider extension entry"
                 (require 'com.blockether.vis.ext.provider-openrouter :reload)
                 (let
                   [provider
                    (vis/provider-by-id :openrouter)

                    ext-nses
                    (set (map :ext/name (vis/registered-extensions)))]

                   (expect (= :openrouter (:provider/id provider)))
                   (expect (= "OpenRouter" (:provider/label provider)))
                   (expect (contains? ext-nses "provider-openrouter"))
                   (expect (= (svar/provider-base-url :openrouter)
                              (get-in provider [:provider/preset :base-url])))
                   ;; Gateway slugs are `vendor/model`; the preset seeds a usable set.
                   (expect (seq (get-in provider [:provider/preset :default-models])))
                   (expect (every? string? (get-in provider [:provider/preset :default-models])))
                   (expect (ifn? (:provider/limits-fn provider)))
                   (expect (ifn? (:provider/auth-prompt-fn provider)))
                   (expect (ifn? (:provider/get-token-fn provider))))))

(defdescribe
  auth-prompt-test
  (it "exposes static API-key guidance"
      (require 'com.blockether.vis.ext.provider-openrouter :reload)
      (let [lines ((:provider/auth-prompt-fn (vis/provider-by-id :openrouter)))]
        (expect (some #(= "  OpenRouter requires a static API key." %) lines))
        (expect (some #(= "         export OPENROUTER_API_KEY=<your-openrouter-api-key>" %) lines))
        (expect (some #(= (str "  Endpoint: " (svar/provider-base-url :openrouter)) %) lines)))))

(defdescribe
  auth-detection-test
  (it "prefers the TUI/config API key used by runtime model calls"
      (require 'com.blockether.vis.ext.provider-openrouter :reload)
      (with-redefs-fn {#'openrouter/load-auth-file (constantly nil)
                       #'openrouter/env-key (constantly nil)
                       #'vis/current-config (constantly {:providers [{:id :openrouter
                                                                      :api-key "config-key"}]})}
        (fn []
          (expect (= {:api-key "config-key" :source :config} (#'openrouter/detect-key))))))
  (it "falls back to the env var, then the persisted auth file"
      (require 'com.blockether.vis.ext.provider-openrouter :reload)
      (with-redefs-fn {#'openrouter/configured-key (constantly nil)
                       #'openrouter/load-auth-file (constantly {:api-key "file-key"})
                       #'openrouter/env-key (constantly "env-key")}
        (fn []
          (expect (= {:api-key "env-key" :source :env-var} (#'openrouter/detect-key)))))
      (with-redefs-fn {#'openrouter/configured-key (constantly nil)
                       #'openrouter/env-key (constantly nil)
                       #'openrouter/load-auth-file (constantly {:api-key "file-key"})}
        (fn []
          (expect (= {:api-key "file-key" :source :auth-file} (#'openrouter/detect-key)))))))

(defdescribe get-token-test
             (it "returns the router token envelope when a key exists"
                 (require 'com.blockether.vis.ext.provider-openrouter :reload)
                 (with-redefs-fn {#'openrouter/detect-key (constantly {:api-key "k"
                                                                       :source :env-var})}
                   (fn []
                     (expect (= {:token "k" :api-url (svar/provider-base-url :openrouter)}
                                ((:provider/get-token-fn (vis/provider-by-id :openrouter))))))))
             (it "throws a pointer at `vis providers auth openrouter` when unauthenticated"
                 (require 'com.blockether.vis.ext.provider-openrouter :reload)
                 (with-redefs-fn {#'openrouter/detect-key (constantly nil)}
                   (fn []
                     (expect (= :vis/openrouter-not-authenticated
                                (try ((:provider/get-token-fn (vis/provider-by-id :openrouter)))
                                     nil
                                     (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))))

(defdescribe
  limits-test
  (it "reports capped-key credit usage from /api/v1/key"
      (require 'com.blockether.vis.ext.provider-openrouter :reload)
      (with-redefs-fn {#'openrouter/detect-key (constantly {:api-key "k" :source :auth-file})
                       #'openrouter/fetch-key-info!
                       (fn [api-key]
                         (expect (= "k" api-key))
                         {:data {:label "vis" :usage 2.5 :limit 10.0 :is_free_tier false}})}
        (fn []
          (let [report (vis/provider-limits :openrouter)]
            (expect (= :openrouter (:provider-id report)))
            (expect (= :ok (:status report)))
            (expect (= [:openrouter-credits] (mapv :id (get-in report [:dynamic :limits]))))
            (expect (= 2.5 (get-in report [:dynamic :limits 0 :used])))
            (expect (= 10.0 (get-in report [:dynamic :limits 0 :limit])))
            (expect (= 7.5 (get-in report [:dynamic :limits 0 :remaining])))
            (expect (false? (get-in report [:dynamic :limits 0 :is-unlimited])))))))
  ;; Pure mapping assertion - `with-redefs` mutates a global Var and lazytest
  ;; runs `it` blocks in parallel, so the uncapped case must NOT race the
  ;; capped case above over `fetch-key-info!`.
  (it "marks an uncapped key unlimited and still reports usage"
      (require 'com.blockether.vis.ext.provider-openrouter :reload)
      (let
        [dynamic
         (#'openrouter/key-info->dynamic-limits {:data {:usage 1.25 :limit nil}})

         row
         (get-in dynamic [:limits 0])]

        (expect (true? (:is-unlimited row)))
        (expect (= 1.25 (:used row)))
        (expect (nil? (:limit row)))
        (expect (= :account (:scope row)))
        (expect (= :credits (:kind row)))))
  (it "reports :unauthenticated when no key is available"
      (require 'com.blockether.vis.ext.provider-openrouter :reload)
      (with-redefs-fn {#'openrouter/detect-key (constantly nil)}
        (fn []
          (let [report ((:provider/limits-fn (vis/provider-by-id :openrouter)))]
            (expect (= :openrouter (:provider-id report)))
            (expect (= :unauthenticated (:status report)))
            (expect (= [] (get-in report [:dynamic :limits]))))))))
