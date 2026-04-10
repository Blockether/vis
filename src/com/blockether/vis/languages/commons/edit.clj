(ns com.blockether.vis.languages.commons.edit
  "Base EDIT tool for RLM agents.
   Surgical text replacement: old_string → new_string.
   Matches exact content, not line numbers — robust against file changes."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [com.github.difflib DiffUtils UnifiedDiffUtils]))

;;; ── Safety ─────────────────────────────────────────────────────────────

(def ^:private max-file-size
  "Max file size to edit (10 MB)."
  (* 10 1024 1024))

(defn- validate-edit-path!
  "Validate file path for editing: must exist, not a directory, file itself not a symlink, within size limit."
  [path]
  (let [f (io/file path)]
    (when-not (.exists f)
      (throw (ex-info (str "File not found: " path) {:path path})))
    (when (.isDirectory f)
      (throw (ex-info (str "Path is a directory: " path) {:path path})))
    (when (java.nio.file.Files/isSymbolicLink (.toPath f))
      (throw (ex-info (str "Refusing to edit through symlink: " path) {:path path})))
    (let [size (.length f)]
      (when (> size max-file-size)
        (throw (ex-info (str "File too large to edit: " (quot size 1024) "KB (max " (quot max-file-size 1024) "KB)")
                        {:path path :size size :max max-file-size}))))
    f))

;;; ── Core ───────────────────────────────────────────────────────────────

(defn edit-file
  "Replace exact text in a file.

   Params:
   - path        — File path (string, required)
   - old-string  — Exact text to find and replace (string, required, must be non-empty)
   - new-string  — Replacement text (string, required)
   - replace-all — Replace all occurrences (boolean, optional, default false)

   When replace-all is false (default), old-string must appear exactly once.
   Throws if file not found, old-string empty, not found, or ambiguous.

   Returns map: {:path str :replacements int}"
  ([path old-string new-string] (edit-file path old-string new-string false))
  ([path old-string new-string replace-all]
   (when (str/blank? old-string)
     (throw (ex-info "old-string must not be empty" {})))
   (when (= old-string new-string)
     (throw (ex-info "old-string and new-string are identical — nothing to change" {})))
   (let [f           (validate-edit-path! path)
         content     (slurp f :encoding "UTF-8")
         ;; Count occurrences using String.indexOf (no regex, no backreference issues)
         occurrences (loop [idx 0 cnt 0]
                       (let [found (.indexOf ^String content ^String old-string (int idx))]
                         (if (neg? found)
                           cnt
                           (recur (+ found (.length old-string)) (inc cnt)))))]
     (when (zero? occurrences)
       (throw (ex-info (str "old-string not found in " path
                            ". Read the file first to get the exact text.")
                       {:path path
                        :old-string (subs old-string 0 (min 100 (count old-string)))})))
     (when (and (not replace-all) (> occurrences 1))
       (throw (ex-info (str "old-string matches " occurrences " locations in " path
                            ". Provide more context to make it unique, or pass replace-all=true.")
                       {:path path :matches occurrences})))
     ;; Use String.replace (literal, no regex backreferences) for safety
     (let [first-idx    (.indexOf ^String content ^String old-string)
           new-content  (if replace-all
                          (.replace ^String content ^String old-string ^String new-string)
                          (str (subs content 0 first-idx) new-string (subs content (+ first-idx (.length old-string)))))
           replacements (if replace-all occurrences 1)
           ;; Generate unified diff via java-diff-utils
           old-lines    (vec (str/split-lines content))
           new-lines    (vec (str/split-lines new-content))
           patch        (DiffUtils/diff old-lines new-lines)
           diff-lines   (vec (UnifiedDiffUtils/generateUnifiedDiff
                               (str "a/" (.getName f)) (str "b/" (.getName f))
                               old-lines patch 3))]
       (spit f new-content :encoding "UTF-8")
       {:path         (.getCanonicalPath f)
        :replacements replacements
        :diff         diff-lines}))))

;;; ── Tool definition ────────────────────────────────────────────────────

(def tool-def
  {:sym 'edit-file
   :fn  edit-file
   :doc "Replace exact text in a file. old-string must match exactly once (or use replace-all). Read the file first to get exact text."
   :params [{:name "path" :type :string :required true
             :description "File path to edit"}
            {:name "old-string" :type :string :required true
             :description "Exact text to find (must be unique in file unless replace-all)"}
            {:name "new-string" :type :string :required true
             :description "Replacement text"}
            {:name "replace-all" :type :boolean :required false
             :description "Replace all occurrences (default false)"}]
   :returns {:type :map
             :description "{:path str :replacements int}"}})
