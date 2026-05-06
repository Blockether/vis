(ns com.blockether.vis.ext.channel-telegram.bot-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-telegram.api :as tg]
            [com.blockether.vis.ext.channel-telegram.bot]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- private-var [sym]
  (var-get (ns-resolve 'com.blockether.vis.ext.channel-telegram.bot sym)))

(defn- reset-chat-state! []
  (reset! (private-var 'chat-state) {}))

(defn- telegram-update [chat-id text]
  {:message {:chat {:id chat-id}
             :from {:username "alice"}
             :text text}})

(defdescribe command-test
  (it "sets reasoning and verbosity through Telegram slash commands"
    (reset-chat-state!)
    (let [sent (atom [])
          handle-command! (private-var 'handle-command!)]
      (with-redefs [tg/send-message! (fn [_token chat-id text & _opts]
                                       (swap! sent conj [chat-id text]))]
        (expect (true? (handle-command! "token" 42 "/reasoning deep")))
        (expect (true? (handle-command! "token" 42 "/verbosity high")))
        (expect (= [[42 "Reasoning: deep"]
                    [42 "Codex verbosity: high"]]
                  @sent))
        (expect (= {:reasoning-level :deep
                    :openai-codex-verbosity :high}
                  (get-in @(private-var 'chat-state) [42 :settings]))))))

  (it "cancels the active Telegram turn"
    (reset-chat-state!)
    (let [cancelled (atom nil)
          handle-command! (private-var 'handle-command!)]
      (swap! (private-var 'chat-state) assoc-in [42 :in-flight] :token)
      (with-redefs [vis/cancel! (fn [token] (reset! cancelled token))
                    tg/send-message! (fn [_token _chat-id _text & _opts] nil)]
        (expect (true? (handle-command! "token" 42 "/cancel")))
        (expect (= :token @cancelled))))))

(defdescribe model-command-test
  (it "/model only shows the current model"
    (reset-chat-state!)
    (let [saved           (atom ::not-called)
          handle-command! (private-var 'handle-command!)]
      (with-redefs [vis/load-config (fn [] {:providers [{:id :openai
                                                         :models [{:name "gpt-5"}
                                                                  {:name "gpt-5-mini"}]}]})
                    vis/save-config! (fn [config] (reset! saved config))
                    tg/send-message! (fn [_token _chat-id text & _opts]
                                       (expect (= "Current model: openai/gpt-5\n\nUse /models to list and choose." text)))]
        (expect (true? (handle-command! "token" 42 "/model")))
        (expect (= ::not-called @saved)))))

  (it "/models selects a model and preserves non-provider config"
    (reset-chat-state!)
    (let [saved           (atom nil)
          rebuilt         (atom nil)
          refreshed       (atom nil)
          handle-command! (private-var 'handle-command!)]
      (with-redefs [vis/load-config (fn [] {:providers [{:id :openai
                                                         :models [{:name "gpt-5"}
                                                                  {:name "gpt-5-mini"}]}]})
                    vis/load-config-raw (fn [] {:tui-settings {:show-thinking false}
                                                :db-spec {:backend :sqlite}})
                    vis/save-config! (fn [config] (reset! saved config))
                    vis/reload-config! (fn [] @saved)
                    vis/rebuild-router! (fn [config]
                                          (reset! rebuilt config)
                                          :router)
                    vis/refresh-cached-routers! (fn [router]
                                                  (reset! refreshed router))
                    tg/send-message! (fn [_token _chat-id text & _opts]
                                       (expect (= "Model set: openai/gpt-5-mini" text)))]
        (expect (true? (handle-command! "token" 42 "/models 2")))
        (expect (= {:tui-settings {:show-thinking false}
                    :db-spec {:backend :sqlite}
                    :providers [{:id :openai
                                 :models [{:name "gpt-5-mini"}
                                          {:name "gpt-5"}]}]}
                  @saved))
        (expect (= @saved @rebuilt))
        (expect (= :router @refreshed)))))

  (it "/models lists choices with an inline keyboard"
    (reset-chat-state!)
    (let [sent            (atom nil)
          handle-command! (private-var 'handle-command!)]
      (with-redefs [vis/load-config (fn [] {:providers [{:id :openai
                                                         :models [{:name "gpt-5"}
                                                                  {:name "gpt-5-mini"}]}]})
                    tg/send-message! (fn [_token _chat-id text & [{:keys [reply-markup]}]]
                                       (reset! sent {:text text :reply-markup reply-markup}))]
        (expect (true? (handle-command! "token" 42 "/models")))
        (expect (= "Models\nCurrent: openai/gpt-5\n\n1. ✅ openai/gpt-5\n2. openai/gpt-5-mini\n\nTap a button, or send /models 2, or /models provider/model."
                  (:text @sent)))
        (expect (= {"inline_keyboard" [[{"text" "✅ openai/gpt-5"
                                         "callback_data" "model:0"}]
                                       [{"text" "openai/gpt-5-mini"
                                         "callback_data" "model:1"}]]}
                  (:reply-markup @sent)))))))

(defdescribe turn-parity-test
  (it "forwards TUI-equivalent reasoning, Codex verbosity, and cancellation opts"
    (reset-chat-state!)
    (swap! (private-var 'chat-state) assoc-in [42 :settings]
      {:reasoning-level :deep
       :openai-codex-verbosity :high})
    (let [seen-send (promise)
          sent      (promise)
          token     {:cancel (atom false) :future (atom nil)}
          handle-update! (private-var 'handle-update!)]
      (with-redefs [vis/cancellation-token (fn [] token)
                    vis/cancellation-atom (fn [t]
                                            (expect (= token t))
                                            (:cancel t))
                    vis/cancellation-set-future! (fn [t fut]
                                                   (expect (= token t))
                                                   (reset! (:future t) fut))
                    vis/get-router (fn [] :router)
                    vis/resolve-effective-model (fn [_]
                                                  {:provider :openai-codex
                                                   :name "gpt-5.5"
                                                   :reasoning? true})
                    vis/for-telegram-chat! (fn [chat-id]
                                             (expect (= 42 chat-id))
                                             {:id "c1"})
                    vis/send! (fn [id text opts]
                                (deliver seen-send [id text opts])
                                {:answer "ok" :cost {:provider :openai-codex
                                                     :model "gpt-5.5"}})
                    vis/format-meta-line (fn [_] "openai-codex/gpt-5.5")
                    tg/send-chat-action! (fn [_token _chat-id _action])
                    tg/send-message! (fn [_token _chat-id text & _opts]
                                       (deliver sent text))]
        (handle-update! "token" (telegram-update 42 "hello"))
        (expect (= "ok\n\n_🤖 openai-codex/gpt-5.5_"
                  (deref sent 1000 :timeout)))
        (let [[id text opts] (deref seen-send 1000 :timeout)]
          (expect (= "c1" id))
          (expect (= "hello" text))
          (expect (= :deep (:reasoning-default opts)))
          (expect (= {:text {:verbosity "high"}} (:extra-body opts)))
          (expect (= (:cancel token) (:cancel-atom opts)))
          (expect (not (contains? opts :max-context-tokens))))))))
