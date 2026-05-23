(ns com.blockether.vis.ext.language-clojure.find
  "Find Clojure defs across the workspace by name regex.

   Surface: `(find-defs root opts)` returns
     {:matches [{:path :line :kind :name [:dispatch] [:doc]} ...]
      :scanned N
      :truncated? bool
      :elapsed-ms N}

   Implementation: directory walk that respects the same skip list
   as the foundation-core language scan (avoids `target`, `node_modules`,
   etc.), then `outline-string` per file and filter by name regex.

   This is intentionally regex-on-name, NOT full-text search — Vis
   already ships `v/rg` for content search. The value-add here is
   the structured, def-aware view: we tell the model `name`,
   `kind`, `line`, optional `dispatch` and `doc` so it can jump
   straight into a focused read."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.ext.language-clojure.outline :as outline])
  (:import
   (java.nio.file FileVisitResult Files Path SimpleFileVisitor)
   (java.nio.file.attribute BasicFileAttributes)))

(def ^:private clj-extensions #{"clj" "cljs" "cljc" "edn"})

(def ^:private skip-dirs
  ;; Same intent as foundation-core/languages.clj. Kept local to
  ;; avoid a cross-extension require — both lists evolve slowly.
  #{".git" ".hg" ".svn" "node_modules" "target" "dist" "build"
    ".venv" "venv" "__pycache__" ".cpcache" ".cljs-cache"
    ".shadow-cljs" ".clj-kondo" ".lsp" ".idea" ".gradle"
    ".next" ".nuxt" "vendor" ".cache" "out" ".out"})

(defn- clj-file? [^java.io.File f]
  (let [name (.getName f)
        dot  (.lastIndexOf name ".")]
    (and (.isFile f)
      (pos? dot)
      (contains? clj-extensions (.toLowerCase (subs name (inc dot)))))))

(defn- collect-files
  [^java.io.File root max-files deadline-ms]
  (let [^Path start    (.toPath root)
        out            (java.util.ArrayList.)
        visited        (long-array 1 0)
        deadline       (+ (System/currentTimeMillis) (long deadline-ms))
        truncated      (boolean-array 1 false)
        visitor        (proxy [SimpleFileVisitor] []
                         (preVisitDirectory [^Path dir ^BasicFileAttributes _]
                           (cond
                             (> (System/currentTimeMillis) deadline)
                             (do (aset truncated 0 true) FileVisitResult/TERMINATE)
                             (= dir start) FileVisitResult/CONTINUE
                             (contains? skip-dirs (str (.getFileName dir)))
                             FileVisitResult/SKIP_SUBTREE
                             :else FileVisitResult/CONTINUE))
                         (visitFile [^Path file ^BasicFileAttributes attrs]
                           (let [count* (aget visited 0)]
                             (cond
                               (or (>= count* (long max-files))
                                 (> (System/currentTimeMillis) deadline))
                               (do (aset truncated 0 true) FileVisitResult/TERMINATE)

                               (.isRegularFile attrs)
                               (let [f (.toFile file)]
                                 (aset visited 0 (inc count*))
                                 (when (clj-file? f) (.add out f))
                                 FileVisitResult/CONTINUE)
                               :else FileVisitResult/CONTINUE)))
                         (visitFileFailed [^Path _ _] FileVisitResult/CONTINUE))]
    (try (Files/walkFileTree start visitor) (catch Throwable _ nil))
    {:files (vec out)
     :truncated? (aget truncated 0)}))

(defn- compile-pattern
  "Coerce `name-pattern` into a `java.util.regex.Pattern`. nil means
   match everything."
  [name-pattern]
  (cond
    (nil? name-pattern) nil
    (instance? java.util.regex.Pattern name-pattern) name-pattern
    (string? name-pattern) (re-pattern name-pattern)
    :else (throw (ex-info "name-pattern must be a string regex or nil"
                   {:type :clj/find-bad-args :got name-pattern}))))

(defn find-defs
  "Walk `root` and return def entries whose name matches the regex.

   Opts:
     :name           regex string, defaults to nil (return all defs)
     :kind           keyword or set; e.g. :defn / #{:defn :defmacro}.
                     nil means any def-family form.
     :max-files      hard cap on visited files (default 5000)
     :deadline-ms    wall-time budget (default 2000)
     :limit          cap on returned matches (default 200)"
  [workspace-root {:keys [name kind max-files deadline-ms limit]
                   :or   {max-files 5000 deadline-ms 2000 limit 200}}]
  (let [start    (System/currentTimeMillis)
        root     (io/file workspace-root)
        _        (when-not (and (.exists root) (.isDirectory root))
                   (throw (ex-info "workspace-root must be a readable directory"
                            {:type :clj/find-bad-args :root (str root)})))
        pat      (compile-pattern name)
        kinds    (cond
                   (nil? kind)       nil
                   (keyword? kind)   #{kind}
                   (set? kind)       kind
                   (sequential? kind) (set kind)
                   :else
                   (throw (ex-info ":kind must be a keyword, set or sequence of keywords"
                            {:type :clj/find-bad-args :kind kind})))
        {:keys [files truncated?]} (collect-files root max-files deadline-ms)
        rel      (fn [^java.io.File f]
                   (let [base (.getAbsolutePath root)
                         p    (.getAbsolutePath f)]
                     (if (str/starts-with? p (str base "/"))
                       (subs p (inc (count base)))
                       p)))
        matches  (volatile! [])]
    (loop [fs files]
      (cond
        (empty? fs) nil
        (>= (count @matches) (long limit)) nil
        :else
        (let [^java.io.File f (first fs)
              o (try (outline/outline-string (slurp f))
                  (catch Throwable _ nil))]
          (when o
            (doseq [entry (:forms o)
                    :let [nm (:name entry)]
                    :when (and nm
                            (or (nil? pat) (re-find pat nm))
                            (or (nil? kinds) (contains? kinds (:kind entry)))
                            (< (count @matches) (long limit)))]
              (vswap! matches conj
                (cond-> {:path (rel f)
                         :line (:line entry)
                         :kind (:kind entry)
                         :name nm}
                  (:dispatch entry) (assoc :dispatch (:dispatch entry))
                  (:doc entry)      (assoc :doc (:doc entry))))))
          (recur (next fs)))))
    {:matches    @matches
     :scanned    (count files)
     :truncated? (or truncated? (>= (count @matches) (long limit)))
     :elapsed-ms (- (System/currentTimeMillis) start)}))
