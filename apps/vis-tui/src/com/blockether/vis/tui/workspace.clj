(ns com.blockether.vis.tui.workspace
  "Client-side path context for rendering and local file affordances."
  (:require [clojure.java.io :as io]))

(def ^:dynamic *workspace-root* nil)

(defn cwd [] (io/file (or *workspace-root* (System/getProperty "user.dir"))))

(defn workspace-root
  [workspace]
  (cond (map? workspace) (or (:root workspace) (get workspace "root") (:workspace/root workspace))
        (some? workspace) (str workspace)
        :else (str (cwd))))

(defn root [] (workspace-root *workspace-root*))
(defn session [] nil)

(defn ancestor-roots
  [file]
  (loop [f
         (.getCanonicalFile (io/file file))

         acc
         []]

    (if f (recur (.getParentFile f) (conj acc f)) acc)))
