(ns com.blockether.vis.ext.lang-clojure.path
  "Shared path guards for Clojure language zipper tools."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [com.blockether.vis.internal.workspace :as workspace])
  (:import
   (java.io File)))

(def clojure-file-extensions
  "File extensions whose contents `z/` zipper tools can safely parse via rewrite-clj."
  #{"clj" "cljc" "cljs" "edn"})

(defn safe-path
  "Resolve `p` against the Vis workspace cwd and reject traversal outside it."
  ^File [p]
  (let [cwd        (workspace/cwd)
        resolved   (.toAbsolutePath (fs/path cwd (str p)))
        normalized (.normalize resolved)
        cwd-norm   (.normalize (.toAbsolutePath (fs/path cwd)))]
    (when-not (.startsWith normalized cwd-norm)
      (throw (ex-info (str "Path '" p "' escapes the working directory")
               {:type :ext.lang-clojure/path-escape :path (str p)})))
    (.toFile normalized)))

(defn file-extension
  "Lowercased extension of the path's basename, without the dot. Empty string when absent."
  [path]
  (let [s    (str path)
        base (.getName (File. s))
        idx  (.lastIndexOf base ".")]
    (if (pos? idx)
      (str/lower-case (subs base (inc idx)))
      "")))

(defn ensure-clojure-file-ext!
  "Reject non-Clojure/EDN paths before rewrite-clj sees arbitrary text bytes."
  [path]
  (let [ext (file-extension path)]
    (when-not (contains? clojure-file-extensions ext)
      (throw (ex-info
               (str "z/ tools only operate on Clojure/EDN files ("
                 (str/join ", " (sort (map #(str "." %) clojure-file-extensions)))
                 "). Got '" path "' (extension " (pr-str ext)
                 "). Use v/patch / v/write for plain-text files.")
               {:type      :ext.lang-clojure/non-clojure-file
                :path      (str path)
                :extension ext
                :allowed   clojure-file-extensions})))))

(defn ensure-existing-file!
  "Return file `f`, or throw if missing/a directory."
  [^File f]
  (when-not (.exists f)
    (throw (ex-info (str "File not found: " (.getPath f))
             {:type :ext.lang-clojure/file-not-found :path (.getPath f)})))
  (when (.isDirectory f)
    (throw (ex-info (str "Path is a directory, not a file: " (.getPath f))
             {:type :ext.lang-clojure/path-is-dir :path (.getPath f)})))
  f)

(defn rel-path
  "Workspace-relative path string for file `f`."
  [^File f]
  (let [cwd (.toAbsolutePath (fs/path (workspace/cwd)))
        p   (.toAbsolutePath (.toPath f))]
    (str (.relativize cwd p))))
