(ns com.blockether.vis.shared
  "Shared utilities for vis.")

(defn truncate
  "Return `s` capped at `n` chars, preserving nil."
  [s n]
  (when s
    (if (> (count s) n)
      (subs s 0 n)
      s)))

(defn add-shutdown-hook!
  "Register a zero-arg function to run on JVM shutdown."
  [^Runnable f]
  (.addShutdownHook (Runtime/getRuntime)
    (Thread. f "vis-shutdown-hook")))
