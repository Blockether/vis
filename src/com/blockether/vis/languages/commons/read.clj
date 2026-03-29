(ns com.blockether.vis.languages.commons.read
  "Base READ tool for RLM agents.
   Reads files with optional offset (line) and limit (line count).
   Returns content with line numbers for precise referencing."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;;; ── Safety ─────────────────────────────────────────────────────────────

(def ^:private max-file-size
  "Max file size to read without offset+limit (10 MB)."
  (* 10 1024 1024))

(defn- validate-path!
  "Validate file path: must exist, not a directory, file itself not a symlink."
  [path]
  (let [f (io/file path)]
    (when-not (.exists f)
      (throw (ex-info (str "File not found: " path) {:path path})))
    (when (.isDirectory f)
      (throw (ex-info (str "Path is a directory: " path) {:path path})))
    (when (java.nio.file.Files/isSymbolicLink (.toPath f))
      (throw (ex-info (str "Refusing to follow symlink: " path) {:path path})))
    f))

;;; ── Core ───────────────────────────────────────────────────────────────

(defn read-file
  "Read a file with optional offset and limit.

   Params:
   - path   — File path (string, required)
   - offset — Starting line number, 1-based (int, optional, default 1)
   - limit  — Max lines to return (int, optional, default all)

   Uses buffered reading when offset/limit specified — safe for large files.
   Returns string with numbered lines: \"1\\t(ns foo)\\n2\\t(:require ...)\""
  ([path] (read-file path nil nil))
  ([path offset] (read-file path offset nil))
  ([path offset limit]
   (let [f   (validate-path! path)
         off (max 0 (dec (or offset 1)))
         use-range? (or (and offset (> offset 1)) limit)]

     ;; Guard: full reads on huge files require offset+limit
     (when (and (not use-range?) (> (.length f) max-file-size))
       (throw (ex-info (str "File too large: " (quot (.length f) 1024) "KB. Use offset+limit to read a portion.")
                       {:path path :size (.length f) :max max-file-size})))

     (if use-range?
       ;; Buffered line-by-line — only reads what's needed
       (with-open [rdr (io/reader f :encoding "UTF-8")]
         (let [lines   (line-seq rdr)
               taken   (vec (cond->> (drop off lines) limit (take limit)))
               total   (+ off (count taken)
                          (count (drop (+ off (count taken)) lines)))
               numbered (map-indexed
                         (fn [i line] (str (+ off i 1) "\t" line))
                         taken)]
           (str (str/join "\n" numbered)
                "\n[lines " (+ off 1) "-" (+ off (count taken)) " of " total "]")))

       ;; Full read — guarded by size check above
       (let [content  (slurp f :encoding "UTF-8")
             lines    (if (str/blank? content) [] (str/split-lines content))
             numbered (map-indexed (fn [i line] (str (inc i) "\t" line)) lines)]
         (str/join "\n" numbered))))))

;;; ── Tool definition ────────────────────────────────────────────────────

(def tool-def
  {:sym 'read-file
   :fn  read-file
   :doc "Read a file with optional line offset and limit. Returns numbered lines."
   :params [{:name "path" :type :string :required true
             :description "File path to read"}
            {:name "offset" :type :int :required false
             :description "Starting line number (1-based, default 1)"}
            {:name "limit" :type :int :required false
             :description "Max number of lines to return"}]
   :returns {:type :string
             :description "File content with line numbers (N\\tcontent)"}})
