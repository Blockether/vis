(ns com.blockether.vis.tui.startup-no-provider-test
  "The gateway can answer session startup with 503 no-provider: the terminal app
   has to stay open and offer the Providers dialog instead of exiting."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.terminal-image :as timg]
            [com.blockether.vis.tui.terminals :as term]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [com.googlecode.lanterna TerminalSize]
           [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(def no-provider-error
  "The exception the TUI gateway client raises for the startup 503: the parsed
   error body of the response, plus the transport keys the client adds."
  (ex-info "make-router requires at least one provider"
           {"error" {"type" "no-provider" "message" "make-router requires at least one provider"}
            :http-status 503
            :vis/user-error true}))

(defn- await-value
  "Wait for a production render or input transition, returning its observed value."
  [f]
  (let [deadline (+ (System/nanoTime) 5000000000)]
    (loop []

      (if-let [value (f)]
        value
        (when (< (System/nanoTime) deadline) (Thread/sleep 10) (recur))))))

(defn- start-screen!
  "Run the real TUI against a gateway that refuses to start a session while
   `fail?` is set. `heal-in-dialog?` models the user adding a provider in the
   dialog the screen opens. `close!` shuts the application down and restores
   the application state, recording whether the run really ended."
  [terminal fail? heal-in-dialog?]
  (let [before
        @state/app-db

        session-id
        "44444444-4444-4444-8444-444444444444"

        dialogs
        (atom [])

        notices
        (atom [])

        error
        (atom nil)

        closed
        (atom nil)

        config
        {:providers [{:id :openai-codex :models [{:name "gpt-5.5"}]}]}

        no-op
        (fn [& _]
          nil)

        bindings
        (merge
          (zipmap [#'screen/configure-terminal-input! #'screen/probe-terminal-cell-size!
                   #'screen/register-terminal-interrupt-handlers! #'screen/enable-terminal-state!
                   #'screen/disable-terminal-state! #'screen/install-ssh-passphrase-prompt!
                   #'screen/start-provider-limits-thread! #'screen/start-workspace-refresh-thread!
                   #'screen/ensure-launch-project-id! #'screen/latest-project-session-id
                   #'screen/session-db-title #'screen/persist-tabs!
                   #'screen/release-workspace-sessions! #'screen/start-deferred-gateway-slash-load!
                   #'screen/start-deferred-improve-settings-load!
                   #'client/gateway-reconcile-running-turns! #'client/reload-config!
                   #'client/toggles-hydrate-from-config! #'client/save-toggles!
                   #'client/watch-notifications! #'client/unwatch-notifications!
                   #'client/add-channel-event-listener! #'client/remove-channel-event-listener!
                   #'client/gateway-release-session! #'timg/images-protocol]
                  (repeat no-op))
          {#'screen/create-terminal! (constantly terminal)
           #'screen/session-workspace (constantly {:root "/tmp"})
           #'screen/subscribe-session-live! (constantly no-op)
           #'screen/open-settings-modal! (fn [_screen & [section]]
                                           (swap! dialogs conj (or section "Settings"))
                                           (when heal-in-dialog? (reset! fail? false)))
           #'client/toggle-add-listener! (constantly no-op)
           #'client/load-config (constantly config)
           #'client/load-config-raw (constantly {})
           #'client/notify! (fn [message & _]
                              (swap! notices conj message))
           #'chat/resume-session (fn [sid]
                                   (if @fail? (throw no-provider-error) {:id sid :history []}))})

        runner
        (future (try (with-redefs-fn bindings #(screen/run-chat! {:session-id session-id}))
                     (catch Throwable t (reset! error t))))]

    {:session-id session-id
     :dialogs dialogs
     :notices notices
     :error error
     :closed closed
     :runner runner
     :close! (fn []
               (state/dispatch [:shutdown])
               (.addInput ^DefaultVirtualTerminal terminal (KeyStroke. KeyType/Escape))
               ;; The run has to end here: a surviving input loop keeps writing to
               ;; the shared application state every later test reads.
               (reset! closed (deref runner 5000 ::running))
               (reset! state/app-db before))}))

(deftest startup-without-a-provider-opens-providers-and-binds-after-saving
  (let [terminal
        (DefaultVirtualTerminal. (TerminalSize. 100 30))

        fail?
        (atom true)

        {:keys [session-id dialogs error closed close!]}
        (start-screen! terminal fail? true)]

    (try (is (= ["Providers"] (await-value #(seq @dialogs))))
         ;; The dialog saved a provider, so the same deferred startup is re-armed
         ;; and the session it builds lands in the waiting tab.
         (is (= session-id (await-value #(get-in @state/app-db [:session :id]))))
         (finally (close!)))
    (is (not= ::running @closed))
    (is (nil? @error))))

(deftest a-provider-that-never-answers-leaves-the-tui-open
  ;; Regression: startup used to throw here, which printed "make-router requires
  ;; at least one provider" and exited the application with status 2, so the user
  ;; could never reach the dialog that fixes it.
  (let [terminal
        (DefaultVirtualTerminal. (TerminalSize. 100 30))

        fail?
        (atom true)

        {:keys [session-id dialogs notices error closed runner close!]}
        (start-screen! terminal fail? false)]

    (try (is (some? (await-value #(seq @notices))))
         (is (some #(str/includes? % "provider") @notices))
         (is (= ["Providers"] @dialogs))
         (is (nil? (get-in @state/app-db [:session :id])))
         (is (false? (future-done? runner)))
         (is (nil? @error))
         ;; The user adds a provider and reopens Providers with C-x o: startup is
         ;; retried from the live screen instead of a restart.
         (reset! fail? false)
         (.addInput terminal (KeyStroke. (Character/valueOf \x) true false false))
         (.addInput terminal (term/keystroke \o))
         (is (= session-id (await-value #(get-in @state/app-db [:session :id]))))
         (finally (close!)))
    (is (not= ::running @closed))
    (is (nil? @error))))
