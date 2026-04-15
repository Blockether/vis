(ns com.blockether.vis.utils
  "Shared utilities for vis.")

(defn add-shutdown-hook!
  "Register a zero-arg function to run on JVM shutdown."
  [^Runnable f]
  (.addShutdownHook (Runtime/getRuntime)
    (Thread. f "vis-shutdown-hook")))
