(ns com.blockether.vis.internal.view.app-surfaces
  "Where a live app shows. An app declares its surfaces; the person's settings
   decide. Every picture of an app that leaves the engine goes through [[shown]],
   so the TUI and the Companion read the same answer and resolve nothing."
  (:require [com.blockether.vis.contract.app :as app]
            [com.blockether.vis.internal.config.toggles :as toggles]))

(defn- setting
  "The live value of setting `id`, or nil when no such setting is registered."
  [id]
  (when (toggles/toggle-spec id) (toggles/value-of id)))

(defn- register-settings!
  "Register the two settings of the app with prefix `k`. Their defaults are what
   the app declares, so a stored choice of the person wins and a new declaration
   moves only the default. Registering again only merges."
  [k {:keys [title surfaces]}]
  (let [declared (merge app/surface-defaults surfaces)]
    (toggles/register-toggle! {:id (str k "_inline")
                               :label (str title ": in the chat")
                               :description "Show this app in the chat turn that opened it."
                               :type :boolean
                               :default (:inline declared)
                               :scopes ["global"]
                               :owner :vis
                               :group :apps
                               :parent "apps_inline"
                               :persist? true})
    (toggles/register-toggle! {:id (str k "_session")
                               :label (str title ": session apps")
                               :description "Where the session apps button opens this app."
                               :type :enum
                               :choices app/session-surfaces
                               :default (:session declared)
                               :scopes ["global"]
                               :owner :vis
                               :group :apps
                               :parent "apps_session"
                               :persist? true})))

(defn shown
  "`view` as a reader gets it. An app gains its effective `:surfaces` and, when it
   has a key, its `:settings-key`; its own settings exist from then on. Any other
   view passes through."
  [view]
  (if-not (and (map? view) (app/app? view))
    view
    (let [k (app/settings-key view)]
      (when k (register-settings! k view))
      (cond-> (assoc view :surfaces (app/effective-surfaces view setting))
        k
        (assoc :settings-key k)))))
