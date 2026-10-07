(ns com.blockether.vis.tui.agent-name-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.dialogs :as dialogs]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.scroll :as scroll]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.terminal-image :as timg]
            [com.blockether.vis.tui.terminals :as term]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna.screen TerminalScreen]))

(defdescribe gateway-name-paints-the-transcript
             (it "gateway name paints the transcript"
                 (doseq [agent-name ["Ada" "助手" "Vis"]]
                   (let [workspace {"root" "/remote/project" "agent_name" agent-name}
                         captured (cap/capture! {:cols 40
                                                 :rows 12
                                                 :paint! (fn [{:keys [g]}]
                                                           (render/draw-messages-area!
                                                             g
                                                             {:visible [{:top 0
                                                                         :projected
                                                                         {:role :assistant
                                                                          :text "Done."}}]
                                                              :total-h 4
                                                              :eff-scroll 0}
                                                             0 12
                                                             40 (get workspace "agent_name")))})]

                     (expect (nil? (:error captured)))
                     (expect (str/includes? (cap/frame-text captured) agent-name))
                     (expect (str/includes? (cap/frame-text captured) "Done."))))))

(defdescribe names-are-clipped-and-user-labels-stay-unchanged
             (it "names are clipped and user labels stay unchanged"
                 (let [captured
                       (cap/capture! {:cols 20
                                      :rows 10
                                      :paint! (fn [{:keys [g]}]
                                                (render/draw-chat-bubble!
                                                  g
                                                  {:role :assistant :text "Done."}
                                                  0 0
                                                  12 {:agent-name "A very long agent name"})
                                                (render/draw-chat-bubble! g
                                                                          {:role :user
                                                                           :text "Hello"}
                                                                          5 0
                                                                          12 {:agent-name "Ada"}))})

                       lines
                       (str/split-lines (cap/frame-text captured))]

                   (expect (nil? (:error captured)))
                   (expect (str/includes? (first lines) "…"))
                   (expect (str/blank? (subs (first lines) 12)))
                   (expect (str/includes? (cap/frame-text captured) "You"))
                   (expect (not (str/includes? (cap/frame-text captured) "Ada"))))))

(defdescribe
  council-wakes-have-their-own-speaker-label
  (it "council wakes have their own speaker label"
      ;; Provenance is persisted metadata, never a request-text convention.
      (doseq [cols
              [40 100]

              [message label]
              [[{:role :user :request-kind :council :text "Actual peer request"} "Council"]
               ;; Match Companion, including separators and the thread rather than entry id.
               [{:role :user
                 :request-kind :council
                 :text "Actual peer request"
                 :council {"entry_id" 84 "thread_id" 42 "kind" "coordination"}}
                "Council · Coordination · Thread #42"]
               [{:role :user
                 :request-kind :council
                 :text "Actual peer request"
                 :council {"entry_id" 84 "thread_id" 42 "kind" "informational"}}
                "Council · Information · Thread #42"]
               [{:role :user
                 :request-kind :council
                 :text "Actual peer request"
                 :council {"entry_id" 84 "thread_id" 42 "kind" "complain"}}
                "Council · Complaint · Thread #42"]
               [{:role :user :text "Council wake — literal user text"} "You"]
               [{:role :user :text "Hello"} "You"]
               [{:role :user :text "Explain Council wake — messages."} "You"]
               [{:role :user :text ""} "You"] [{:role :user :text "Hello" :status :queued} "Queued"]
               [{:role :assistant :text "Council wake — quoted in an answer."} "Ada"]]]

        (let [captured
              (cap/capture! {:cols cols
                             :rows 12
                             :paint!
                             (fn [{:keys [g]}]
                               (render/draw-chat-bubble! g message 0 0 cols {:agent-name "Ada"}))})

              heading
              (str/trim (first (str/split-lines (cap/frame-text captured))))]

          (expect (nil? (:error captured)))
          (expect (= label heading) (pr-str [cols message]))))))

(defdescribe progress-uses-the-same-gateway-name
             (it "progress uses the same gateway name"
                 (doseq [progress [{:iterations []} {:iterations [{:thinking "Working"}]}]]
                   (let [text (:text (render/progress->lines-data
                                       progress
                                       80
                                       {}
                                       {:agent-name "Ada" :now-ms 1000 :turn-start-ms 0}))]
                     (expect (str/includes? text "Ada is"))
                     (expect (not (str/includes? text "Vis is")))))))

(defdescribe
  live-ticks-keep-the-gateway-name
  ;; #325: the cheap live-bubble tick painted the default name between full frames.
  (it
    "live ticks keep the gateway name"
    (let [db
          {:messages [{:role :user :text "Hello"} {:role :assistant :text ""}]
           :session {:id "s1"}
           :workspace {"root" "/remote/project" "agent_name" "Ada"}
           :input {:lines [""] :crow 0 :ccol 0}
           :scroll scroll/follow
           :settings {}
           :loading? true
           :progress {:iterations [{:thinking "Checking"}]}}

          transcript
          (fn [terminal]
            (subvec (term/grid terminal) 0 20))

          capture
          (binding [interactions/hit-map (interactions/create-hit-map)]
            (with-redefs [state/app-db (atom db)
                          timg/images-protocol (constantly nil)]

              (cap/capture! {:cols 80
                             :rows 30
                             :paint! (fn [{:keys [screen terminal]}]
                                       (let [layout (#'screen/render-frame! screen 80 30 db 1000)
                                             full (transcript terminal)]

                                         (#'screen/render-live-bubble-frame!
                                          screen
                                          80
                                          30
                                          (assoc db :layout layout)
                                          1080
                                          layout)
                                         (.refresh ^TerminalScreen screen)
                                         [full (transcript terminal)]))})))

          heading
          (fn [rows]
            (some #(let [line (str/trim %)] (when (re-find #"^(Ada|Vis)\b" line) line)) rows))]

      (expect (nil? (:error capture)) (str (:error capture)))
      (doseq [rows (:ret capture)]
        (expect (str/starts-with? (str (heading rows)) "Ada") (pr-str (heading rows)))))))

(defn settings-fixture
  "Production settings dialog; only the gateway boundary is an in-memory fixture."
  [cols keys save-error]
  (let [saved
        (atom "Vis")

        requests
        (atom [])

        errors
        (atom [])]

    (with-redefs [client/setting
                  (fn [id]
                    (expect (= "agent_name" id))
                    {"id" id "type" "string" "value" @saved})

                  client/set-setting-value!
                  (fn [id value]
                    (swap! requests conj [id value])
                    (when save-error (throw (ex-info save-error {})))
                    (reset! saved (str/trim value))
                    {"id" id "value" @saved})]

      (with-redefs-fn {#'dialogs/agent-name-setting (atom nil)
                       #'dialogs/provider-inventory (atom {:status :unloaded})
                       #'dialogs/mcp-inventory (atom {:status :unloaded})
                       #'dialogs/mark-inventories-loading! (constantly nil)
                       #'dialogs/load-inventories! #'dialogs/load-agent-name!
                       #'dialogs/mini-note! (fn [_ _ _ _ text]
                                              (swap! errors conj text))}
        (fn []
          {:capture (cap/capture!
                      {:cols cols
                       :rows 20
                       :keys keys
                       :paint! (fn [{:keys [screen]}]
                                 (dialogs/settings-dialog! screen {} {:focus-section "General"}))})
           :saved @saved
           :requests @requests
           :errors @errors})))))

(defdescribe settings-edit-saves-through-the-gateway
             (it "settings edit saves through the gateway"
                 (doseq [cols [40 100]]
                   (let [{:keys [capture saved requests]} (settings-fixture cols
                                                                            [:enter :backspace
                                                                             :backspace :backspace
                                                                             \A \d \a :enter :esc]
                                                                            nil)]
                     (expect (nil? (:error capture)))
                     (expect (= "Ada" saved))
                     (expect (= [["agent_name" "Ada"]] requests))
                     (expect (re-find #"Agent name\s+Ada" (cap/frame-text capture)))))))

(defdescribe settings-cancel-and-save-failure-preserve-the-name
             (it "settings cancel and save failure preserve the name"
                 (let [{:keys [capture saved requests]}
                       (settings-fixture 40 [:enter \x :esc :esc] nil)]
                   (expect (nil? (:error capture)))
                   (expect (= "Vis" saved))
                   (expect (empty? requests)))
                 (let [{:keys [capture saved errors]}
                       (settings-fixture 40 [:enter \x :enter :esc] "Write failed")]
                   (expect (nil? (:error capture)))
                   (expect (= "Vis" saved))
                   (expect (= ["Write failed"] errors)))))

(defdescribe
  gateway-rename-and-reconnect-update-the-owning-tab
  (it "gateway rename and reconnect update the owning tab"
      (let [before @state/app-db]
        (try (reset! state/app-db {:active-tab-id "a"
                                   :tabs [{:id "a"} {:id "b"}]
                                   :render-version 0
                                   :workspace {"agent_name" "Vis"}
                                   :tab-locals {"b" {:workspace {"agent_name" "Vis"}}}})
             (doseq [event [{"type" "session.agent_name_updated" "agent_name" "Ada"}
                            {"type" "subscription.ready" "is_live" false "agent_name" "助手"}]]
               (let [chunk (#'chat/gateway-event->chunk event)]
                 (expect (= (get event "agent_name") (:agent-name chunk)))
                 (state/dispatch [:sync-agent-name "b" chunk])
                 (expect (= (get event "agent_name")
                            (get-in @state/app-db [:tab-locals "b" :workspace "agent_name"])))
                 (expect (= "Vis" (get-in @state/app-db [:workspace "agent_name"])))
                 (state/dispatch [:sync-agent-name "a" chunk])
                 (expect (= (get event "agent_name")
                            (get-in @state/app-db [:workspace "agent_name"])))
                 (state/dispatch [:sync-agent-name "a" {:agent-name "Vis"}])))
             (finally (reset! state/app-db before))))))

(defdescribe
  setting-client-uses-shared-endpoints
  (it "setting client uses shared endpoints"
      (let [requests (atom [])]
        (with-redefs-fn {#'client/send-json! (fn [& args]
                                               (swap! requests conj args)
                                               {})}
          #(do (client/setting "agent_name") (client/set-setting-value! "agent_name" "Ada")))
        (expect (= [(list "GET" "/v1/settings/agent_name")
                    (list "POST" "/v1/settings" {:id "agent_name" :action "value" :value "Ada"})]
                   @requests)))))
