(ns com.blockether.vis.internal.python.format
  "Beautify model-emitted Python before it is shown, with the plain-Java port of
   `ruff format` in the vis-python-presentation package. Used by the gateway's
   code renderer so the trace shows tidy, consistently-wrapped Python instead of
   the model's raw one-liners.

   CACHED: formatting is deterministic for a given input, and the same code
   block is rendered many times (pinned trace + live SSE re-emits + reconnect
   replay), so an LRU memo means each distinct block formats exactly once.

   SAFE: `PythonFormatter/format` returns the source verbatim when the code
   doesn't parse or opts out with a suppression comment — the original is never
   lost."
  (:require [clojure.core.memoize :as memo]
            [clojure.string :as str])
  (:import (com.blockether.vis.python PythonFormatter)))

(def ^:private format*
  ;; LRU-bounded so a long session's distinct code blocks can't grow unbounded.
  (memo/lru (fn [^String code]
              (PythonFormatter/format code))
            :lru/threshold
            1024))

(defn beautify-python
  "Format `code` the way `ruff format` does (cached). nil/blank -> \"\". Never
   throws — falls back to the verbatim source when the code can't be formatted."
  ^String [code]
  (let [s (str code)]
    (if (str/blank? s) "" (format* s))))
