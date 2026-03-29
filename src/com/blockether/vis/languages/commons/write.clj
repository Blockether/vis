(ns com.blockether.vis.languages.commons.write
  "Base WRITE tool for RLM agents.
   Full file overwrite. For surgical edits, use edit.clj."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;;; ── Safety ─────────────────────────────────────────────────────────────

(def ^:private max-write-size
  "Max content size to write (10 MB)."
  (* 10 1024 1024))

(defn- validate-write-path!
  "Validate write path: not a directory, file itself not a symlink."
  [path]
  (let [f (io/file path)]
    (when (.isDirectory f)
      (throw (ex-info (str "Path is a directory: " path) {:path path})))
    (when (and (.exists f) (java.nio.file.Files/isSymbolicLink (.toPath f)))
      (throw (ex-info (str "Refusing to write through symlink: " path) {:path path})))
    f))

;;; ── Core ───────────────────────────────────────────────────────────────

(defn write-file
  "Write content to a file (full overwrite).

   Creates parent directories if needed.
   Returns map: {:path str :lines int}"
  [path content]
  (when-not (string? content)
    (throw (ex-info (str "content must be a string, got: " (type content)) {:type (type content)})))
  (when (> (count content) max-write-size)
    (throw (ex-info (str "Content too large: " (quot (count content) 1024) "KB (max " (quot max-write-size 1024) "KB)")
                    {:size (count content) :max max-write-size})))
  (let [f   (validate-write-path! path)
        dir (.getParentFile f)]
    (when (and dir (not (.exists dir)))
      (.mkdirs dir))
    (spit f content :encoding "UTF-8")
    {:path  (.getCanonicalPath f)
     :lines (if (str/blank? content) 0 (count (str/split-lines content)))}))

;;; ── Tool definition ────────────────────────────────────────────────────

(def tool-def
  {:sym 'write-file
   :fn  write-file
   :doc "Write content to a file (full overwrite). Creates parent dirs if needed."
   :params [{:name "path" :type :string :required true
             :description "File path to write"}
            {:name "content" :type :string :required true
             :description "Content to write"}]
   :returns {:type :map
             :description "{:path str :lines int}"}})
