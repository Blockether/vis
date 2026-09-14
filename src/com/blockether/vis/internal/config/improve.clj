(ns com.blockether.vis.internal.config.improve
  "Improve settings. Automatic permits bounded model analysis, not command replay or deployment."
  (:require [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.util :as util]))

(defonce ^:private generation (atom 0))

(defonce ^:private settings-lock (Object.))

#_{:clj-kondo/ignore [:unused-private-var]}

(defonce ^:private mode-listener
  (toggles/add-listener! (fn [{:keys [id]}]
                           (when (= id "improve_mode") (swap! generation inc)))))

(defn settings
  "Current live mode and merged, persisted review route. No implicit provider or model."
  []
  (let [raw (get (config/load-config-raw) "improve")]
    {:mode (toggles/value-of "improve_mode")
     :provider (get raw "provider")
     :model (get raw "model")
     :interval_minutes (get raw "interval_minutes" 60)}))

(defn- invalid! [message] (throw (ex-info message {:type :improve/invalid :status 400})))

(defn update-settings!
  "Persist a partial snake_case settings map without replacing other config owners.
   Provider/model fields are accepted only when the resulting mode is Automatic.
   An incomplete route may be saved, but cannot run a review."
  [attrs]
  (locking settings-lock
    (when-not (and (map? attrs) (every? #{:mode :provider :model :interval_minutes} (keys attrs)))
      (invalid! "Unknown Improve settings fields"))
    (let [next-settings
          (merge (settings) attrs)

          {:keys [mode provider model interval_minutes]}
          next-settings]

      (when-not (#{"off" "human" "automatic"} mode)
        (invalid! "mode must be off, human or automatic"))
      (when (and (not= "automatic" mode) (some #(contains? attrs %) [:provider :model]))
        (invalid! "Provider and model are editable only in Automatic mode"))
      (doseq [value [provider model]]
        (when-not (or (nil? value) (and (util/non-blank-string? value) (<= (count value) 200)))
          (invalid! "Provider and model must be nonblank strings or null")))
      (when-not (and (integer? interval_minutes) (<= 1 interval_minutes 1440))
        (invalid! "interval_minutes must be an integer between 1 and 1440"))
      ;; Invalidate before IO too: a failed save must never authorize an old result.
      (swap! generation inc)
      (config/update-machine-config! (fn [raw]
                                       (-> raw
                                           (assoc-in ["toggles" "improve_mode"] mode)
                                           (assoc "improve" {"provider" provider
                                                             "model" model
                                                             "interval_minutes" interval_minutes})))
                                     :improve)
      (toggles/set-value! "improve_mode" mode)
      (settings))))

(defn snapshot
  "Capture settings and a generation. A mode round trip invalidates old reviews."
  []
  (locking settings-lock {:settings (settings) :generation @generation}))

(defn current?
  "Quick read-only commit gate; detects route edits and intervening mode changes."
  [snapshot]
  (and (= (:generation snapshot) @generation)
       (= "automatic" (toggles/value-of "improve_mode"))
       (= (:settings snapshot) (settings))))
