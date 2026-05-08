(ns com.blockether.vis.internal.workspace-context
  "Per-turn workspace cwd binding.

   Vis never mutates JVM user.dir. Channels attach :workspace/root to the
   active conversation/turn; extension wrappers bind that root dynamically so
   tools can resolve cwd-relative paths against the workspace explicitly."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]))

(set! *warn-on-reflection* true)

(def ^:dynamic *workspace-root*
  "Canonical workspace root for the current tool call, or nil for process cwd."
  nil)

(defn normalize-root
  "Canonicalize a workspace root string/File. Blank/nil means no workspace."
  [root]
  (let [s (some-> root str str/trim)]
    (when (seq s)
      (.getCanonicalPath (io/file s)))))

(defn workspace-root
  "Return the canonical :workspace/root from an env/root value."
  [env-or-root]
  (normalize-root (if (map? env-or-root)
                    (:workspace/root env-or-root)
                    env-or-root)))

(defn cwd
  "Explicit cwd for path/tool resolution: active workspace root or process cwd."
  ^File []
  (io/file (or *workspace-root* (System/getProperty "user.dir"))))
