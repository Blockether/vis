(ns com.blockether.vis.internal.pyfmt
  "Beautify model-emitted Python with ruff (com.blockether/ruff — in-process via
   the JDK FFM API, black-compatible) before it is shown. Used by the gateway's
   code renderer so the trace shows tidy, consistently-wrapped Python instead of
   the model's raw one-liners.

   CACHED: ruff output is deterministic for a given input, and the same code
   block is rendered many times (pinned trace + live SSE re-emits + reconnect
   replay), so an LRU memo means each distinct block formats exactly once.

   SAFE: `ruff/format-or` returns the source verbatim if ruff is unavailable
   (e.g. the native lib isn't bundled in a particular build) or the code doesn't
   parse — the original is never lost."
  (:require [clojure.core.memoize :as memo]
            [clojure.string :as str]
            [com.blockether.ruff :as ruff]))

(def ^:private format*
  ;; LRU-bounded so a long session's distinct code blocks can't grow unbounded.
  (memo/lru (fn [^String code]
              (ruff/format-or code))
            :lru/threshold
            1024))

(defn beautify-python
  "ruff-format `code` (cached). nil/blank -> \"\". Never throws — falls back to
   the verbatim source when ruff can't format or isn't available."
  ^String [code]
  (let [s (str code)]
    (if (str/blank? s) "" (format* s))))
