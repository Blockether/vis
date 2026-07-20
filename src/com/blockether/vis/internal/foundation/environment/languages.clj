(ns com.blockether.vis.internal.foundation.environment.languages
  "Bounded language scan over a directory tree.

   Walks the tree with `Files/walkFileTree`, skipping common
   non-source subdirectories (`.git`, `node_modules`, `target`, ...)
   via `FileVisitResult/SKIP_SUBTREE`. Counts files and bytes per
   language using a small extension-to-language map.

   The walk has TWO hard guards: a max-file count and a wall-time
   deadline. Either one stops the walk via `TERMINATE`. Callers get
   a possibly-partial result; on a small repo the result is exact.

   No third-party deps. Reflection-clean."
  (:import (java.io File)
           (java.nio.file FileVisitResult Files Path SimpleFileVisitor)
           (java.nio.file.attribute BasicFileAttributes)))

(def ^:const default-max-files 10000)

(def ^:const default-deadline-ms 1000)

(def ^:private skip-directories
  "Subdirectories that the language scanner skips wholesale via
   `FileVisitResult/SKIP_SUBTREE`. Covers VCS metadata, dependency
   caches, generic build outputs, and the conventional output
   directories of common static-site generators (mdBook -> `book`,
   Jekyll -> `_site`, Hugo -> `public`). Build outputs would
   otherwise dominate `:primary` with bundled minified assets."
  #{".git" ".hg" ".svn" "node_modules" "target" "dist" "build" ".venv" "venv" "__pycache__"
    ".cpcache" ".cljs-cache" ".shadow-cljs" ".clj-kondo" ".lsp" ".idea" ".gradle" ".next" ".nuxt"
    "vendor" ".cache" "out" ".out" ".verification" ".verification-baseline" "book" "_site" "public"
    "site" "_book"})

(def ^:private extension-to-language
  {"clj" "clojure"
   "cljs" "clojure"
   "cljc" "clojure"
   "edn" "edn"
   "java" "java"
   "kt" "kotlin"
   "scala" "scala"
   "groovy" "groovy"
   "py" "python"
   "pyi" "python"
   "rb" "ruby"
   "go" "go"
   "rs" "rust"
   "ts" "typescript"
   "tsx" "typescript"
   "js" "javascript"
   "jsx" "javascript"
   "mjs" "javascript"
   "cjs" "javascript"
   "swift" "swift"
   "c" "c"
   "h" "c"
   "cpp" "cpp"
   "cc" "cpp"
   "hpp" "cpp"
   "hh" "cpp"
   "cs" "csharp"
   "php" "php"
   "ex" "elixir"
   "exs" "elixir"
   "erl" "erlang"
   "hrl" "erlang"
   "hs" "haskell"
   "lua" "lua"
   "ml" "ocaml"
   "mli" "ocaml"
   "fs" "fsharp"
   "dart" "dart"
   "zig" "zig"
   "nim" "nim"
   "r" "r"
   "md" "markdown"
   "mdx" "markdown"
   "rst" "rst"
   "txt" "text"
   "sh" "shell"
   "bash" "shell"
   "zsh" "shell"
   "fish" "shell"
   "yml" "yaml"
   "yaml" "yaml"
   "toml" "toml"
   "json" "json"
   "jsonc" "json"
   "xml" "xml"
   "html" "html"
   "htm" "html"
   "css" "css"
   "scss" "scss"
   "sass" "sass"
   "less" "less"
   "sql" "sql"
   "Dockerfile" "dockerfile"})

(def ^:private generated-file-suffixes
  "Filename suffixes that indicate a generated / minified asset. The
   matching files are still visited but excluded from the language
   roll-up so a single 3 MB bundled `app.min.js` cannot pretend
   to be the project's primary language."
  [".min.js" ".min.css" ".min.mjs" ".bundle.js" ".bundle.css" ".chunk.js" ".map"])

(def ^:private generated-file-names
  "Exact filenames that indicate generated content (lockfiles)."
  #{"package-lock.json" "pnpm-lock.yaml" "yarn.lock" "Cargo.lock" "poetry.lock" "Gemfile.lock"
    "composer.lock" "go.sum" "deno.lock" "shrinkwrap.json"})

(defn- generated?
  [^String filename]
  (or (contains? generated-file-names filename)
      (let [lower (.toLowerCase filename)]
        (some (fn [^String suffix]
                (.endsWith lower suffix))
              generated-file-suffixes))))

(defn- file-extension-of
  ^String [^String filename]
  (let [dot (.lastIndexOf filename ".")]
    (cond (= filename "Dockerfile") "Dockerfile"
          (and (pos? dot) (< dot (dec (count filename)))) (.toLowerCase (subs filename (inc dot)))
          :else nil)))

(defn- dir-decision
  ^FileVisitResult [^Path dir]
  (let [name (str (.getFileName dir))]
    (if (contains? skip-directories name) FileVisitResult/SKIP_SUBTREE FileVisitResult/CONTINUE)))

(defn- bucket-rollup
  "Convert a `{lang {:files n :bytes b}}` map into a sorted vec with
   percentages by both file count and total bytes.

   Sort key is **file count first, bytes as tiebreaker**. File count
   is a more robust primary-language signal than total bytes: a
   single 3 MB bundled `app.min.js` cannot drown out 50 source
   files. Generated assets are filtered upstream in the visitor; the
   sort here defends against the residual edge cases."
  [buckets]
  (let
    [entries
     (vec buckets)

     total-files
     (long (reduce + 0 (map (comp :files val) entries)))

     total-bytes
     (long (reduce + 0 (map (comp :bytes val) entries)))

     scored
     (map (fn [[lang {:keys [files bytes]}]]
            (let
              [files*
               (long files)

               bytes*
               (long bytes)]

              {:language lang
               :files files*
               :bytes bytes*
               :files-pct (if (zero? total-files) 0.0 (/ (double files*) (double total-files)))
               :bytes-pct (if (zero? total-bytes) 0.0 (/ (double bytes*) (double total-bytes)))}))
          entries)]

    (vec (sort-by (fn [m]
                    [(- (long (:files m))) (- (long (:bytes m)))])
                  scored))))

(defn scan
  "Walk `root` (a `java.io.File` or `java.nio.file.Path`), counting
   files and bytes per language. Returns a map:

     {:total-files   N
      :total-bytes   N
      :truncated?    bool   ;; true when max-files or deadline tripped
      :elapsed-ms    N
      :primary       \"clojure\"   ;; nil when no source detected
      :languages     [{:language :files :bytes :files-pct :bytes-pct} ...]}

   Languages are sorted by total bytes descending. `:primary` is the
   first entry's `:language` (max-bytes wins).

   Options:
     :max-files    soft cap on visited files (default 10000).
     :deadline-ms  wall-time cap in ms (default 1000)."
  ([root] (scan root nil))
  ([root
    {:keys [max-files deadline-ms]
     :or {max-files default-max-files deadline-ms default-deadline-ms}}]
   (let
     [^Path start
      (cond (instance? Path root) root
            (instance? File root) (.toPath ^File root)
            :else (.toPath (java.io.File. (str root))))

      buckets
      (java.util.HashMap.)

      visited
      (long-array 1 0)

      deadline
      (+ (System/currentTimeMillis) (long deadline-ms))

      truncated
      (boolean-array 1 false)

      start-ms
      (System/currentTimeMillis)

      visitor
      (proxy [SimpleFileVisitor] []
        (preVisitDirectory [^Path dir ^BasicFileAttributes _attrs]
          (cond (> (System/currentTimeMillis) deadline) (do (aset truncated 0 true)
                                                            FileVisitResult/TERMINATE)
                ;; The root itself is never skipped even
                ;; if its name matches skip-directories
                ;; (e.g. running inside `target/`).
                (= dir start) FileVisitResult/CONTINUE
                :else (dir-decision dir)))
        (visitFile [^Path file ^BasicFileAttributes attrs]
          (let [count* (aget visited 0)]
            (cond (or (>= count* (long max-files)) (> (System/currentTimeMillis) deadline))
                  (do (aset truncated 0 true) FileVisitResult/TERMINATE)
                  (.isRegularFile attrs)
                  (do (aset visited 0 (inc count*))
                      (let [filename (str (.getFileName file))]
                        (when (and (not (generated? filename)) (some? (file-extension-of filename)))
                          (when-let [lang (extension-to-language (file-extension-of filename))]
                            (let
                              [bucket (or (.get buckets lang) {:files 0 :bytes 0})
                               size (try (.size attrs) (catch Throwable _ 0))]

                              (.put buckets
                                    lang
                                    {:files (inc (long (:files bucket)))
                                     :bytes (+ (long (:bytes bucket)) (long size))})))))
                      FileVisitResult/CONTINUE)
                  :else FileVisitResult/CONTINUE)))
        (visitFileFailed [^Path _file _exception] FileVisitResult/CONTINUE))]

     (try (Files/walkFileTree start visitor) (catch Throwable _ nil))
     (let
       [bucket-map
        (into {} buckets)

        rolled
        (bucket-rollup bucket-map)

        total-files
        (long (reduce + 0 (map :files rolled)))

        total-bytes
        (long (reduce + 0 (map :bytes rolled)))

        elapsed
        (- (System/currentTimeMillis) start-ms)]

       {:total-files total-files
        :total-bytes total-bytes
        :truncated? (aget truncated 0)
        :elapsed-ms elapsed
        :primary (some-> rolled
                         first
                         :language)
        :languages rolled}))))