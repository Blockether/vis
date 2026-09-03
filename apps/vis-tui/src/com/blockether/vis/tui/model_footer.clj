(ns com.blockether.vis.tui.model-footer
  "The model identity segment painted at the left edge of the TUI footer."
  (:require [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.keymap :as keymap]
            [com.blockether.vis.tui.state :as state]))

;; Model / provider display

(defn- chosen-model-info
  []
  (when-let [r (try (vis/get-router) (catch Throwable _ nil))]
    (try (vis/resolve-effective-model r) (catch Throwable _ nil))))

(defn segments
  "Footer-segment contribution returning a VECTOR of segments:
     1. `provider/model (C-x c) (cycle n/N C-x m)` clickable picker button
        (priority 2, bold) — BOTH keybinding hints live INSIDE the one
        clickable chip that opens the fuzzy model picker. The `n/N` shows the
        current model's position in the C-x m cycle (omitted when the model
        isn't a cycle entry).
     2. optional `⚠ … overloaded` breaker notice (priority 3, warn).

   Returns nil when no model is configured (no router / no resolver)."
  [_db _now-ms]
  (let [;; Per-session model preference (the unified, channel-neutral choice
        ;; — the SAME one the web rail shows and the engine routes). For the
        ;; CURRENT tab `db` already reflects the active session, so its
        ;; `:session` is the active one. Falls back to the resolved router
        ;; model when the session has no explicit pick.
        ;; cached read — the footer renders per frame; no per-paint DB hit.
        ;; pref is {:provider :model} (provider + model both come from it).
        pref
        (or (:session-model-pref _db)
            (when-let [sid (get-in _db [:session :id])]
              (vis/gateway-session-model-cached sid)))

        info
        (chosen-model-info)

        model
        (or (:model pref) (:name info))

        provider
        (or (:provider pref)
            (some-> (:provider info)
                    name))

        ;; DISPLAY-only model id: path-style names (google/gemma-4-12b-qat)
        ;; flatten their slashes so the chip never reads as three segments.
        ;; `model` (the raw id) still feeds routing-status / cycle lookups.
        display
        (let [m (vis/display-model-name model)]
          (cond (and provider m) (str provider "/" m)
                m m
                :else nil))

        ;; When the DISPLAYED provider's circuit breaker is open (provider
        ;; overloaded — repeated 5xx/529/stream failures), svar fails turns over
        ;; to the next available provider. Surface it so the footer doesn't claim
        ;; `opus` while turns actually run on `zai`.
        overload
        (when provider (try (vis/model-routing-status provider model) (catch Throwable _ nil)))

        ;; Live `n/N` for the (cycle …) hint: where the CURRENT model sits in
        ;; the C-x m cycle list. Reads the same entries the handler steps, so
        ;; the count matches what cycling actually walks. Blank when the model
        ;; isn't a cycle entry (e.g. an ad-hoc override), leaving `(cycle C-x m)`.
        cyclepos
        (when (and provider model)
          (try (state/model-cycle-position provider model) (catch Throwable _ nil)))

        pos
        (if cyclepos (str (first cyclepos) "/" (second cyclepos) " ") "")]

    (when (or info pref)
      (when display
        (cond-> [{;; ONE clickable chip carrying BOTH hints: `(C-x c)` opens the
                  ;; fuzzy per-session picker; `(cycle C-x m)`
                  ;; is the quick keyboard cycle. Folding the cycle hint INTO the
                  ;; button (was a separate muted segment sitting outside it) keeps
                  ;; every keybinding for the model control in one place. The whole
                  ;; segment renders one `:fg-role`, so the cycle hint shares the
                  ;; button's strong color. This is the DEFAULT, channel-level model
                  ;; decoration every provider reuses — no per-provider footer needed.
                  :ast [:ast {}
                        [:p {}
                         [:span {}
                          (str display
                               " ("
                               (keymap/label-for :pick-model)
                               ")"
                               " (cycle "
                               pos
                               (keymap/label-for :cycle-model)
                               ")")]]]
                  :region :left
                  :priority 2
                  :row 0
                  :fg-role :success
                  :kind :footer-model
                  :bold? true}]
          overload
          (conj {:ast [:ast {}
                       [:p {}
                        [:span {}
                         (str "⚠ " (:overloaded-model overload)
                              " overloaded → " (or (:serving-model overload)
                                                   "no provider available"))]]]
                 :region :left
                 :priority 3
                 :row 0
                 :fg-role :warn
                 :join-left? true}))))))
