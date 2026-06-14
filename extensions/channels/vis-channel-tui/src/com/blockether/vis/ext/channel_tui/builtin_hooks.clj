(ns com.blockether.vis.ext.channel-tui.builtin-hooks
  "Built-in TUI channel contributions mounted on the channel-tui extension.

   Channel-tui is itself an extension (`:ext/name`
   `com.blockether.vis.ext.channel-tui`), so it can declare its own
   `:ext/channel-contributions` like any external extension. This gives every
   first-party TUI surface (model display, reasoning level, codex
   verbosity) the same contribution path third-party extensions use.

   Why register here instead of inline in footer.clj:

     1. The data flow is uniform - footer's `extension-footer-segments`
        treats first-party + third-party contributions identically. Settings
        UI's contributor toggle (in dialogs.clj) sees these as
        regular contributors that the user can hide.

     2. Other channels (Telegram, web, ...) reading
        `(channel-contributions-for :tui)` get the same model/provider data
        without channel-tui-specific calls.

     3. Provider extensions can later override / supplement these
        contributions with provider-specific contributions (e.g. anthropic could
        register a `:anthropic/model-footer` showing rate-limit
        headroom) without touching channel-tui core.

   The contribution fns return CANONICAL IR (channel-agnostic). Channels
   translate IR to their surface."
  (:require
   [com.blockether.vis.core :as vis]))

;; -----------------------------------------------------------------------------
;; Model / provider display
;; -----------------------------------------------------------------------------

(defn- chosen-model-info
  []
  (when-let [r (try (vis/get-router) (catch Throwable _ nil))]
    (try (vis/resolve-effective-model r) (catch Throwable _ nil))))

(defn- model-footer-render
  "Footer-segment contribution returning a VECTOR of segments:
     1. `provider/model` display (priority 2, bold)
     2. `(Ctrl+T)` keybinding hint joined to it (priority 5, muted)

   Returns nil when no model is configured (no router / no resolver)."
  [_db _now-ms]
  (let [;; Per-session model preference (the unified, channel-neutral choice
        ;; — the SAME one the web rail shows and the engine routes). For the
        ;; CURRENT tab `db` already reflects the active session, so its
        ;; `:session` is the active one. Falls back to the resolved router
        ;; model when the session has no explicit pick.
        pref     (when-let [sid (get-in _db [:session :id])] (vis/session-model-of sid))
        info     (chosen-model-info)
        model    (or pref (:name info))
        provider (if pref
                   (some (fn [p] (when (some #(= pref (vis/model-name %)) (:models p)) (:id p)))
                     (vis/configured-providers))
                   (:provider info))
        display  (cond
                   (and provider model) (str (name provider) "/" model)
                   model                model
                   :else                nil)]
    (when (or info pref)
      (when display
        [{:ir       [:ir {} [:p {} [:span {} display]]]
          :region   :left
          :priority 2
          :row      0
          :fg-role  :success
          :bold?    true}
         {:ir         [:ir {} [:p {} [:span {} "(Ctrl+T)"]]]
          :region     :left
          :priority   5
          :row        0
          :fg-role    :muted
          :join-left? true}]))))

;; -----------------------------------------------------------------------------
;; Contribution map
;; -----------------------------------------------------------------------------

(def channel-contributions
  {:tui.slot/footer-segment
   [{:id :tui.builtin.model/footer
     :fn #'model-footer-render}]})
