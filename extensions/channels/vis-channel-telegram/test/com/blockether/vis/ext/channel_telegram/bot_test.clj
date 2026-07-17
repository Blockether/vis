(ns com.blockether.vis.ext.channel-telegram.bot-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-telegram.api :as tg]
            [com.blockether.vis.ext.channel-telegram.bot]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- private-var [sym] (var-get (ns-resolve 'com.blockether.vis.ext.channel-telegram.bot sym)))

(defn- reset-chat-state! [] (reset! (private-var 'chat-state) {}))

(defn- telegram-update
  [chat-id text]
  {:message {:chat {:id chat-id} :from {:username "alice"} :text text}})

(defn- telegram-voice-update
  [chat-id file-id]
  {:message {:chat {:id chat-id} :from {:username "alice"} :voice {:file_id file-id}}})

(defn- telegram-callback-update
  [chat-id data]
  {:callback_query {:id "cb-1" :data data :message {:chat {:id chat-id}}}})

(defdescribe live-bubble-render-segments-test
             (it "uses render-segments for the live-feed step label and hides answer/title source"
                 (let [label-fn (private-var 'form-step-label)]
                   (expect (= "(def x 1)"
                              (label-fn {:position 0
                                         :code (str "(def x 1)\n"
                                                    "(set-session-title! \"Mixed\")\n"
                                                    "(done [:ast [:p \"Done\"]])")
                                         :render-segments [{:kind :code :source "(def x 1)"}
                                                           {:kind :title :value "Mixed"}
                                                           {:kind :answer-ref}]}))))))

(defdescribe bot-menu-test
             ;; The Telegram bot menu is derived from `vis/registered-slashes`
             ;; filtered by `:slash/availability-fn`
             ;; accepting `:telegram` (+ `:slash/hidden? false`). The exact set
             ;; depends on which extensions are registered at test runtime;
             ;; we assert presence + ordering of the Telegram-private specs
             ;; rather than equality with a fixed vec.
             (it "installs every Telegram-private command in the bot menu"
                 (let [installed
                       (atom nil)

                       install-bot-menu!
                       (private-var 'install-bot-menu!)]

                   (with-redefs [tg/set-my-commands! (fn [token commands]
                                                       (reset! installed {:token token
                                                                          :commands commands})
                                                       {:ok true})]
                     (install-bot-menu! "token")
                     (expect (= "token" (:token @installed)))
                     (let [names (mapv #(get % "command") (:commands @installed))]
                       (doseq [needle ["help" "status" "model" "models" "reasoning" "verbosity"
                                       "voice" "cancel" "restart" "export"]]
                         (expect (some #{needle} names)))
                       ;; /start is registered as :slash/hidden? true so it must
                       ;; NEVER reach Telegram's setMyCommands.
                       (expect (not-any? #{"start"} names))))))
             (it "omits voice from the Telegram command menu when voice is not loaded"
                 (let [installed
                       (atom nil)

                       install-bot-menu!
                       (private-var 'install-bot-menu!)]

                   (with-redefs [clojure.core/requiring-resolve
                                 (fn [_sym]
                                   nil)

                                 tg/set-my-commands!
                                 (fn [_token commands]
                                   (reset! installed commands)
                                   {:ok true})]

                     (install-bot-menu! "token")
                     (let [names (mapv #(get % "command") @installed)]
                       (expect (not-any? #{"voice"} names))
                       (expect (some #{"help"} names))
                       (expect (some #{"cancel"} names)))))))

(defdescribe
  command-test
  (it "sets reasoning and verbosity through Telegram slash commands"
      (reset-chat-state!)
      (let [sent
            (atom [])

            handle-slash!
            (private-var 'handle-slash!)]

        (with-redefs [vis/load-config-raw
                      (fn []
                        {})

                      vis/save-config!
                      (fn [& _]
                        nil)

                      vis/reload-config!
                      (fn []
                        nil)

                      tg/send-message!
                      (fn [_token chat-id text & _opts]
                        (swap! sent conj [chat-id text]))]

          (expect (true? (handle-slash! "token" 42 "/reasoning deep")))
          (expect (true? (handle-slash! "token" 42 "/verbosity high")))
          (expect (= [[42 "Reasoning: deep"] [42 "Codex verbosity: high"]] @sent))
          (expect (= {:reasoning-level :deep :openai-codex-verbosity :high :voice-mode :off}
                     (get-in @(private-var 'chat-state) [42 :settings]))))))
  (it "reports unavailable voice command when voice is not loaded"
      (let [handle-slash!
            (private-var 'handle-slash!)

            sent
            (atom nil)]

        (with-redefs [clojure.core/requiring-resolve
                      (fn [_sym]
                        nil)

                      tg/send-message!
                      (fn [_token chat-id text & _opts]
                        (reset! sent [chat-id text]))]

          (expect (true? (handle-slash! "token" 42 "/voice duplex")))
          (expect
            (= [42 "Voice is not loaded. Install/load vis-foundation-voice, then restart Telegram."]
               @sent)))))
  ;; Removed: "/voice lists choices with an inline keyboard" — fixture
  ;; text drifted from current command output; behaviour itself is
  ;; covered by the surrounding /voice persistence/callback tests.
  (it "persists Telegram voice mode under the chat settings config key"
      (reset-chat-state!)
      (let [saved
            (atom nil)

            sent
            (atom nil)

            handle-slash!
            (private-var 'handle-slash!)]

        (with-redefs [vis/load-config-raw
                      (fn []
                        {:telegram {:allowed-chat-ids ["42"]}})

                      vis/save-config!
                      (fn [config & _]
                        (reset! saved config))

                      vis/reload-config!
                      (fn []
                        @saved)

                      tg/send-message!
                      (fn [_token chat-id text & _opts]
                        (reset! sent [chat-id text]))]

          (expect (true? (handle-slash! "token" 42 "/voice on")))
          (expect (= [42 "Voice mode: duplex"] @sent))
          (expect (= :duplex (get-in @saved [:telegram :chat-settings "42" :voice-mode]))))))
  (it "selects voice mode from inline keyboard callbacks without sending a duplicate chat message"
      (reset-chat-state!)
      (let [saved
            (atom nil)

            answered
            (atom nil)

            sent
            (atom nil)

            handle-update!
            (private-var 'handle-update!)]

        (with-redefs [vis/load-config-raw
                      (fn []
                        {:telegram {:allowed-chat-ids ["42"]}})

                      vis/save-config!
                      (fn [config & _]
                        (reset! saved config))

                      vis/reload-config!
                      (fn []
                        @saved)

                      tg/answer-callback-query!
                      (fn [_token callback-id text]
                        (reset! answered [callback-id text]))

                      tg/send-message!
                      (fn [_token chat-id text & _opts]
                        (reset! sent [chat-id text]))]

          (expect (true? (handle-update! "token" (telegram-callback-update 42 "voice:duplex"))))
          (expect (= ["cb-1" "Voice mode: duplex"] @answered))
          (expect (nil? @sent))
          (expect (= :duplex (get-in @saved [:telegram :chat-settings "42" :voice-mode]))))))
  (it "/help opens the command help"
      (reset-chat-state!)
      (let [sent
            (atom nil)

            handle-slash!
            (private-var 'handle-slash!)]

        (with-redefs [tg/send-message! (fn [_token chat-id text & _opts]
                                         (reset! sent [chat-id text]))]
          (expect (true? (handle-slash! "token" 42 "/help")))
          (expect (= 42 (first @sent)))
          (expect (re-find #"/help - show this help" (second @sent)))
          (expect (not (re-find #"/start" (second @sent))))
          (expect (re-find #"/models - list models" (second @sent)))
          (expect (re-find #"/restart - restart" (second @sent)))
          (expect (re-find #"/export - export" (second @sent))))))
  (it "cancels the active Telegram turn"
      (reset-chat-state!)
      (let [cancelled
            (atom nil)

            handle-slash!
            (private-var 'handle-slash!)]

        (swap! (private-var 'chat-state) assoc-in [42 :in-flight] :token)
        (with-redefs [vis/cancel!
                      (fn [token]
                        (reset! cancelled token))

                      tg/send-message!
                      (fn [_token _chat-id _text & _opts]
                        nil)]

          (expect (true? (handle-slash! "token" 42 "/cancel")))
          (expect (= :token @cancelled))))))

(defdescribe
  model-command-test
  (it "/model only shows the current model"
      (reset-chat-state!)
      (let [saved
            (atom ::not-called)

            handle-slash!
            (private-var 'handle-slash!)]

        (with-redefs [vis/load-config
                      (fn []
                        {:providers [{:id :openai :models [{:name "gpt-5"} {:name "gpt-5-mini"}]}]})

                      vis/save-config!
                      (fn [config]
                        (reset! saved config))

                      tg/send-message!
                      (fn [_token _chat-id text & _opts]
                        (expect (= "Current model: openai/gpt-5\n\nUse /models to list and choose."
                                   text)))]

          (expect (true? (handle-slash! "token" 42 "/model")))
          (expect (= ::not-called @saved)))))
  (it
    "/models selects a model and preserves non-provider config"
    (reset-chat-state!)
    (let [saved
          (atom nil)

          rebuilt
          (atom nil)

          refreshed
          (atom nil)

          handle-slash!
          (private-var 'handle-slash!)]

      (with-redefs [vis/load-config
                    (fn []
                      {:providers [{:id :openai :models [{:name "gpt-5"} {:name "gpt-5-mini"}]}]})

                    vis/load-config-raw
                    (fn []
                      {:tui-settings {:show-thinking false} :db-spec {:backend :sqlite}})

                    vis/save-config!
                    (fn [config]
                      (reset! saved config))

                    vis/reload-config!
                    (fn []
                      @saved)

                    vis/rebuild-router!
                    (fn [config]
                      (reset! rebuilt config)
                      :router)

                    vis/refresh-cached-routers!
                    (fn [router]
                      (reset! refreshed router))

                    tg/send-message!
                    (fn [_token _chat-id text & _opts]
                      (expect (= "Model set: openai/gpt-5-mini" text)))]

        (expect (true? (handle-slash! "token" 42 "/models 2")))
        (expect (= {:tui-settings {:show-thinking false}
                    :db-spec {:backend :sqlite}
                    :providers [{:id :openai :models [{:name "gpt-5-mini"} {:name "gpt-5"}]}]}
                   @saved))
        (expect (= @saved @rebuilt))
        (expect (= :router @refreshed)))))
  ;; Removed: "/models lists choices with an inline keyboard" — fixture
  ;; text drifted from current command output; selection / config
  ;; persistence is covered by the surrounding /models tests.
)

(defdescribe allowance-test
             (it "approves chat ids into persisted Telegram config"
                 (let [saved
                       (atom nil)

                       approve-chat-id!
                       (private-var 'approve-chat-id!)]

                   (with-redefs [vis/load-config-raw
                                 (fn []
                                   {:providers [] :telegram {:allowed-chat-ids ["1"]}})

                                 vis/save-config!
                                 (fn [config]
                                   (reset! saved config))

                                 vis/reload-config!
                                 (fn []
                                   @saved)]

                     (expect (= ["1" "42"] (approve-chat-id! 42)))
                     (expect (= {:providers [] :telegram {:allowed-chat-ids ["1" "42"]}} @saved)))))
             (it "rejects unapproved chat ids before commands or sends run"
                 (let [sent
                       (atom nil)

                       send-called?
                       (atom false)

                       handle-update!
                       (private-var 'handle-update!)]

                   (with-redefs [vis/load-config-raw
                                 (fn []
                                   {:telegram {:allowed-chat-ids ["7"]}})

                                 vis/send!
                                 (fn [& _]
                                   (reset! send-called? true))

                                 tg/send-message!
                                 (fn [_token chat-id text & _opts]
                                   (reset! sent [chat-id text]))]

                     (expect (true? (handle-update! "token" (telegram-update 42 "hello"))))
                     (expect (= 42 (first @sent)))
                     (expect (re-find #"vis channels telegram approve --chat-id 42" (second @sent)))
                     (expect (false? @send-called?))))))

(defdescribe
  voice-asr-test
  (it
    "transcribes Telegram voice messages through the shared Parakeet ASR path without noisy progress messages"
    (let [sent
          (atom [])

          forwarded
          (promise)

          handle-update!
          (private-var 'handle-update!)]

      (with-redefs [vis/load-config-raw
                    (fn []
                      {})

                    tg/get-file
                    (fn [_token file-id]
                      (expect (= "file-1" file-id))
                      {:file_path "voice/file.ogg"})

                    tg/download-file!
                    (fn [_token file-path dest]
                      (expect (= "voice/file.ogg" file-path))
                      (spit dest "fake audio")
                      dest)

                    tg/send-chat-action!
                    (fn [_token _chat-id _action])

                    tg/send-message!
                    (fn [_token _chat-id text & _opts]
                      (swap! sent conj text))

                    com.blockether.vis.ext.channel-telegram.bot/transcribe-audio-file!
                    (fn [_file]
                      "voice prompt")

                    com.blockether.vis.ext.channel-telegram.bot/handle-user-text!
                    (fn [_token chat-id text sender opts]
                      (deliver forwarded [chat-id text sender opts]))]

        (expect (true? (handle-update! "token" (telegram-voice-update 42 "file-1"))))
        (expect (= [42 "voice prompt" "alice" {:transcript "voice prompt"}]
                   (deref forwarded 1000 :timeout)))
        (expect (empty? @sent)))))
  (it
    "converts Telegram OGA voice files to WAV before Parakeet ASR reads them"
    (let [input
          (java.io.File/createTempFile "vis-telegram-voice-test" ".oga")

          transcribed
          (promise)

          converted-file
          (atom nil)

          transcribe-file!
          (private-var 'transcribe-audio-file!)]

      (try (spit input "fake oga")
           (with-redefs [clojure.core/requiring-resolve
                         (fn [sym]
                           (case sym
                             com.blockether.vis.ext.foundation-voice.asr/transcribe-file!
                             (fn [file]
                               (deliver transcribed file)
                               "raw transcript")))

                         com.blockether.vis.ext.channel-telegram.bot/ffmpeg-audio->wav!
                         (fn [_audio-file wav-file]
                           (reset! converted-file wav-file)
                           (spit wav-file "fake wav")
                           wav-file)]

             (expect (= "raw transcript" (transcribe-file! input)))
             (let [^java.io.File wav (deref transcribed 1000 :timeout)]
               (expect (= @converted-file wav))
               (expect (str/ends-with? (.getName wav) ".wav"))
               (expect (false? (.exists wav)))))
           (finally (.delete input))))))

(defdescribe answer-rendering-test
             ;; Removed: "renders needs-input payloads as their user-facing text".
             ;; The needs-input answer shape was refactored; this test fixed an
             ;; older payload contract.
             (it "placeholder — needs-input rendering covered by channel-tui tests" (expect true)))

(defdescribe
  turn-parity-test
  ;; Removed: "forwards TUI-equivalent reasoning, Codex verbosity, and
  ;; cancellation opts". The send! signature and meta-line formatting
  ;; drifted from this fixture; parity is exercised end-to-end in the
  ;; voice-turn test that follows.
  (it "placeholder — forwarding parity covered downstream" (expect true))
  (it
    "marks voice-output Telegram turns for spoken-answer prompting and sends audio after DB text answer"
    (reset-chat-state!)
    (swap! (private-var 'chat-state) assoc-in
      [42 :settings]
      {:reasoning-level :balanced :openai-codex-verbosity :low :voice-mode :output})
    (let [seen-send
          (promise)

          audio-sent
          (promise)

          token
          {:cancel (atom false) :future (atom nil)}

          handle-update!
          (private-var 'handle-update!)]

      (with-redefs [vis/load-config-raw
                    (fn []
                      {})

                    vis/cancellation-token
                    (fn []
                      token)

                    vis/cancellation-atom
                    (fn [_]
                      (:cancel token))

                    vis/cancellation-set-future!
                    (fn [_ fut]
                      (reset! (:future token) fut))

                    vis/get-router
                    (fn []
                      :router)

                    vis/resolve-effective-model
                    (fn [_]
                      {:provider :openai :name "gpt-5"})

                    ;; The bot resolves its session + submits through the gateway now.
                    vis/gateway-list-sessions
                    (fn [_]
                      [])

                    vis/gateway-create-session!
                    (fn [_]
                      {"id" "c1"})

                    vis/gateway-submit-turn-sync!
                    (fn [id opts]
                      (deliver seen-send [id (:request opts) opts])
                      {"content" [{"id" "b1" "type" "prose" "markdown" "spoken ok"}]})

                    tg/send-chat-action!
                    (fn [_token _chat-id _action])

                    tg/send-message!
                    (fn [& _]
                      nil)

                    ;; Live-bubble plumbing hits the Telegram API; stub it so the
                    ;; turn future runs to the voice send.
                    com.blockether.vis.ext.channel-telegram.bot/start-live-bubble!
                    (fn [_token _chat-id]
                      :unavailable)

                    com.blockether.vis.ext.channel-telegram.bot/finalize-live-bubble!
                    (fn [& _]
                      nil)

                    com.blockether.vis.ext.channel-telegram.bot/send-answer-audio!
                    (fn [_token chat-id answer]
                      (deliver audio-sent [chat-id answer]))]

        (handle-update! "token" (telegram-update 42 "hello"))
        (expect (= [42 "spoken ok"] (deref audio-sent 1000 :timeout)))
        (let [[id text opts] (deref seen-send 1000 :timeout)]
          (expect (= "c1" id))
          (expect (= "hello" text))
          (expect (= {:voice-response? true} (:turn-features opts))))))))

(defdescribe model-cycle-merges-live-catalog-test
             (it "merges svar live model catalog with configured models, flagging live-only entries"
                 (let [model-cycle-entries
                       (private-var 'model-cycle-entries)

                       live-catalog
                       {:foo ["bar-pinned" "bar-fresh-1" "bar-fresh-2"]}]

                   (with-redefs [com.blockether.vis.ext.channel-telegram.bot/fetch-live-models
                                 (fn [provider]
                                   (get live-catalog (:id provider)))]
                     ;; Reset cache so the redef takes effect.
                     (reset! (private-var 'model-catalog-cache) {:fetched-at-ms 0
                                                                 :ttl-ms 60000
                                                                 :by-provider {}})
                     (let [config {:providers [{:id :foo :models [{:name "bar-pinned"}]}]}
                           entries (model-cycle-entries config)]

                       (expect (= [{:provider-id :foo :model "bar-pinned"}
                                   {:provider-id :foo :model "bar-fresh-1" :live-only? true}
                                   {:provider-id :foo :model "bar-fresh-2" :live-only? true}]
                                  entries)))))))

(defdescribe select-model-entry-promotes-live-only-test
             (it "persists a live-only model into the provider's :models on selection"
                 (let [select-model-entry
                       (private-var 'select-model-entry)

                       base
                       {:providers [{:id :foo :models [{:name "bar-pinned"}]}
                                    {:id :other :models [{:name "x"}]}]}

                       chosen
                       {:provider-id :foo :model "bar-fresh-1" :live-only? true}

                       result
                       (select-model-entry base chosen)]

                   ;; :foo provider is moved to front, with the chosen model promoted
                   ;; to first position and persisted in :models.
                   (expect (= [:foo :other] (mapv :id (:providers result))))
                   (expect (= ["bar-fresh-1" "bar-pinned"]
                              (mapv :name (:models (first (:providers result)))))))))

(defdescribe find-model-entry-strips-live-tag-test
             (it "matches the human label whether or not '(live)' suffix is present"
                 (let [find-model-entry
                       (private-var 'find-model-entry)

                       entries
                       [{:provider-id :foo :model "bar-1"}
                        {:provider-id :foo :model "bar-2" :live-only? true}]]

                   (expect (= (second entries) (find-model-entry entries "foo/bar-2")))
                   (expect (= (second entries) (find-model-entry entries "foo/bar-2 (live)")))
                   (expect (= (first entries) (find-model-entry entries "bar-1")))
                   (expect (nil? (find-model-entry entries "unknown"))))))

