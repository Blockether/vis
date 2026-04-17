(ns com.blockether.vis.languages.commons.write
  "Base WRITE tool for RLM agents.
   Full file overwrite. For surgical edits, use edit.clj."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.loop.sci.tool :as sci-tool])
  (:import [com.github.difflib DiffUtils UnifiedDiffUtils]))

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
  (let [f          (validate-write-path! path)
        dir        (.getParentFile f)
        existed?   (.exists f)
        old-content (when existed?
                      (try (slurp f :encoding "UTF-8")
                        (catch Exception _ nil)))
        _          (when (and dir (not (.exists dir)))
                     (.mkdirs dir))
        _          (spit f content :encoding "UTF-8")
        new-lines  (if (str/blank? content) 0 (count (str/split-lines content)))
        fname      (.getName f)]
    (cond-> {:path     (.getCanonicalPath f)
             :lines    new-lines
             :created? (not existed?)}
      ;; For overwrites, generate unified diff
      old-content (assoc :old-lines (if (str/blank? old-content) 0 (count (str/split-lines old-content)))
                    :diff (let [old-lines (vec (str/split-lines (or old-content "")))
                                new-lines-v (vec (str/split-lines content))
                                patch (DiffUtils/diff old-lines new-lines-v)]
                            (vec (UnifiedDiffUtils/generateUnifiedDiff
                                   (str "a/" fname) (str "b/" fname)
                                   old-lines patch 3))))
      ;; For new files, include a preview
      (not existed?) (assoc :preview (vec (take 20 (if (str/blank? content) [] (str/split-lines content))))))))

(defn- validate-write-input
  [{:keys [args]}]
  (let [[path content & extra] args]
    (when (seq extra)
      (throw (ex-info "write-file expects exactly 2 positional args: (write-file path content)"
               {:type :tool/invalid-input :tool 'write-file :args args})))
    (when-not (string? path)
      (throw (ex-info "write-file path must be a string"
               {:type :tool/invalid-input :tool 'write-file :got path :got-type (type path)})))
    (when-not (string? content)
      (throw (ex-info "write-file content must be a string"
               {:type :tool/invalid-input :tool 'write-file :got-type (type content)})))
    {:args [path content]}))

(defn- validate-write-output
  [{:keys [result]}]
  (when-not (map? result)
    (throw (ex-info "write-file must return a map"
             {:type :tool/invalid-output :tool 'write-file :got-type (type result)})))
  (when-not (string? (:path result))
    (throw (ex-info "write-file output :path must be a string"
             {:type :tool/invalid-output :tool 'write-file :result result})))
  (when-not (integer? (:lines result))
    (throw (ex-info "write-file output :lines must be an int"
             {:type :tool/invalid-output :tool 'write-file :result result})))
  {:result result})

;;; ── Tool definition ────────────────────────────────────────────────────

(def tool-def
  (sci-tool/make-tool-def
    'write-file
    write-file
    {:doc (:doc (meta #'write-file))
     :arglists (:arglists (meta #'write-file))
     :validate-input validate-write-input
     :validate-output validate-write-output
     :examples ["(write-file \"/tmp/notes.txt\" \"hello\\nworld\")"
                "(write-file \"src/foo.clj\" \"(ns foo)\\n(def x 1)\")"]}))
