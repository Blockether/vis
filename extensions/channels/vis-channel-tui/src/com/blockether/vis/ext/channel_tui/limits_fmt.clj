(ns com.blockether.vis.ext.channel-tui.limits-fmt
  "TUI aliases for the channel-neutral limits row formatters.

   The real implementation lives in
   `com.blockether.vis.internal.limits-format` (hoisted from here so
   the web channel renders the SAME compact quota summaries on its
   provider cards). This namespace keeps the TUI-local require sites
   (`footer.clj`, `provider.clj`) and the existing test suite stable."
  (:require [com.blockether.vis.internal.limits-format :as limits-format]))

(def format-limit-number limits-format/format-limit-number)
(def generic-limit-label limits-format/generic-limit-label)
(def percentage-limit-row? limits-format/percentage-limit-row?)
(def format-limit-usage limits-format/format-limit-usage)
(def generic-limit-has-signal? limits-format/generic-limit-has-signal?)
(def label+usage limits-format/label+usage)
(def dynamic-summary limits-format/dynamic-summary)
