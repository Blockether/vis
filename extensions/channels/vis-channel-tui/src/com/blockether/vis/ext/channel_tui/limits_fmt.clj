(ns com.blockether.vis.ext.channel-tui.limits-fmt
  "TUI aliases for the channel-neutral limits row formatters.

   The real implementation lives in
   `com.blockether.vis.internal.limits-format` (hoisted from here so
   other channels render the SAME compact quota summaries on their
   provider cards). This namespace keeps the TUI-local require sites
   (`footer.clj`, `provider.clj`) and the existing test suite stable."
  (:require [com.blockether.vis.internal.limits-format :as limits-format]))

(def format-limit-number limits-format/format-limit-number)

(def generic-limit-label limits-format/generic-limit-label)

(def percentage-limit-row? limits-format/percentage-limit-row?)

(def account-plan-window-row? limits-format/account-plan-window-row?)

(def format-limit-usage limits-format/format-limit-usage)

(def short-limit-label limits-format/short-limit-label)

(def compact-limit-usage limits-format/compact-limit-usage)

(def limit-label-parts limits-format/limit-label-parts)

(def compact-limit-cells limits-format/compact-limit-cells)

(def generic-limit-has-signal? limits-format/generic-limit-has-signal?)

(def label+usage limits-format/label+usage)

(def dynamic-summary limits-format/dynamic-summary)

(def limit-row-exhausted? limits-format/limit-row-exhausted?)

(def limit-row-pressure limits-format/limit-row-pressure)

(def prioritize-limit-rows limits-format/prioritize-limit-rows)

(def limit-window-ms limits-format/limit-window-ms)

(def limit-window-order limits-format/limit-window-order)
