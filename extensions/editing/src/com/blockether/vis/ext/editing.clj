(ns com.blockether.vis.ext.editing
  "Editing extension — read, write, grep, list, patch.

   Self-registers via `register-global!` at namespace load time.
   Drop on the classpath and every new environment gets it."
  (:refer-clojure :exclude [read list])
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.loop.runtime.conversation.environment.extension :as ext]))

;; =============================================================================
;; Implementation fns (not exposed directly — wrapped via ext/symbol)
;; =============================================================================

(defn- safe-path
  "Resolve path relative to CWD. Rejects traversal outside CWD."
  ^java.io.File [^String path]
  (let [cwd  (System/getProperty "user.dir")
        f    (io/file cwd path)
        abs  (.getCanonicalPath f)]
    (when-not (str/starts-with? abs cwd)
      (throw (ex-info (str "Path escapes working directory: " path)
               {:type :ext.editing/path-traversal :path path :resolved abs})))
    f))

(defn- read-file
  "Read file contents with optional offset/limit (1-indexed lines)."
  ([path] (read-file path nil nil))
  ([path offset] (read-file path offset nil))
  ([path offset limit]
   (let [f (safe-path path)]
     (when-not (.exists f)
       (throw (ex-info (str "File not found: " path) {:type :ext.editing/not-found :path path})))
     (when (.isDirectory f)
       (throw (ex-info (str "Path is a directory: " path) {:type :ext.editing/is-directory :path path})))
     (let [lines    (str/split-lines (slurp f))
           total    (count lines)
           off      (max 0 (dec (or offset 1)))
           lim      (or limit total)
           selected (take lim (drop off lines))
           numbered (map-indexed (fn [i line]
                                   (str (format "%4d" (+ off i 1)) "  " line))
                      selected)
           content  (str/join "\n" numbered)
           showing  (count selected)]
       (str content
         (when (< (+ off showing) total)
           (str "\n\n[" (- total off showing) " more lines. Use offset=" (+ off showing 1) " to continue.]")))))))

(defn- write-file
  "Write content to a file. Creates parent directories."
  [path content]
  (let [f (safe-path path)]
    (.mkdirs (.getParentFile f))
    (spit f content)
    (str "Wrote " (count content) " bytes to " path)))

(defn- list-files
  "List files/dirs at path. Returns a vector of names."
  ([] (list-files "."))
  ([path]
   (let [f (safe-path path)]
     (when-not (.exists f)
       (throw (ex-info (str "Path not found: " path) {:type :ext.editing/not-found :path path})))
     (if (.isDirectory f)
       (vec (sort (map #(.getName ^java.io.File %) (.listFiles f))))
       (throw (ex-info (str "Not a directory: " path) {:type :ext.editing/not-directory :path path}))))))

(defn- grep-files
  "Search for pattern in files. Returns matches with file:line:content."
  ([pattern] (grep-files pattern "."))
  ([pattern path]
   (let [f   (safe-path path)
         pat (re-pattern (str pattern))]
     (when-not (.exists f)
       (throw (ex-info (str "Path not found: " path) {:type :ext.editing/not-found :path path})))
     (let [files (if (.isDirectory f)
                   (filter #(and (.isFile ^java.io.File %) (not (.isHidden ^java.io.File %)))
                     (file-seq f))
                   [f])
           matches (for [^java.io.File file files
                         :let [rel (.getPath (.toPath (.getCanonicalFile (io/file (System/getProperty "user.dir"))))
                                     (.relativize (.toPath (.getCanonicalFile file))))]
                         :when (try (.isFile file) (catch Exception _ false))
                         :let [lines (try (str/split-lines (slurp file)) (catch Exception _ nil))]
                         :when lines
                         [idx line] (map-indexed vector lines)
                         :when (re-find pat line)]
                     (str rel ":" (inc idx) ":" (str/trim line)))]
       (vec (take 200 matches))))))

(defn- patch-file
  "Apply a search-and-replace patch to a file.
   old-text must match exactly once in the file."
  [path old-text new-text]
  (let [f       (safe-path path)
        content (slurp f)
        count   (count (re-seq (re-pattern (java.util.regex.Pattern/quote old-text)) content))]
    (cond
      (zero? count)
      (throw (ex-info (str "old-text not found in " path)
               {:type :ext.editing/patch-no-match :path path}))
      (> count 1)
      (throw (ex-info (str "old-text matches " count " times in " path ". Must be unique.")
               {:type :ext.editing/patch-ambiguous :path path :matches count}))
      :else
      (let [patched (str/replace-first content old-text new-text)]
        (spit f patched)
        (str "Patched " path)))))

;; =============================================================================
;; Extension definition
;; =============================================================================

(def editing-extension
  (ext/extension
    {:ext/namespace 'com.blockether.vis.ext.editing
     :ext/doc       "Filesystem tools: read, write, grep, list, patch."
     :ext/version   "0.1.0"
     :ext/author    "Blockether"
     :ext/license   "Apache-2.0"
     :ext/group     "filesystem"
     :ext/prompt    "Filesystem tools available in the sandbox:
- (read-file path) or (read-file path offset limit) — read file with line numbers (1-indexed)
- (write-file path content) — write/overwrite file, creates parent dirs
- (list-files) or (list-files path) — list directory contents
- (grep-files pattern) or (grep-files pattern path) — search for regex, returns file:line:match
- (patch-file path old-text new-text) — exact search-and-replace (old-text must be unique)"
     :ext/symbols
     [(ext/symbol 'read-file read-file
        {:doc      "Read file contents with optional line offset/limit."
         :arglists '([path] [path offset] [path offset limit])
         :examples ["(read-file \"src/core.clj\")"
                    "(read-file \"big.log\" 100 50)"]})
      (ext/symbol 'write-file write-file
        {:doc      "Write content to file. Creates parent dirs."
         :arglists '([path content])
         :examples ["(write-file \"out.txt\" \"hello\")"]})
      (ext/symbol 'list-files list-files
        {:doc      "List files/dirs at path."
         :arglists '([] [path])
         :examples ["(list-files)" "(list-files \"src\")"]})
      (ext/symbol 'grep-files grep-files
        {:doc      "Search for regex pattern in files."
         :arglists '([pattern] [pattern path])
         :examples ["(grep-files \"TODO\")" "(grep-files \"defn\" \"src\")"]})
      (ext/symbol 'patch-file patch-file
        {:doc      "Exact search-and-replace in file. old-text must match once."
         :arglists '([path old-text new-text])
         :examples ["(patch-file \"src/core.clj\" \"old code\" \"new code\")"]})]}))

;; Self-register at load time
(ext/register-global! editing-extension)
