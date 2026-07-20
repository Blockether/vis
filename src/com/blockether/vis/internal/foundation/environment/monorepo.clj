(ns com.blockether.vis.internal.foundation.environment.monorepo
  "Monorepo / multi-package detection.

   Walks the tree once (bounded), counting per-language manifest
   files at any depth below the root. >=2 manifests of the same
   kind in distinct subdirectories signals a multi-package
   workspace; we report the kind, the count, and a best-guess
   shape label (e.g. \"polylith\", \"workspace\", \"submodules\").

   Reflection-clean. Honors the same skip-directory list as the
   language scanner."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.paths :as paths])
  (:import (java.io File)
           (java.nio.file FileVisitResult Files Path SimpleFileVisitor)
           (java.nio.file.attribute BasicFileAttributes)))

(def ^:const default-max-files 20000)

(def ^:const default-deadline-ms 800)

(def ^:private skip-directories
  #{".git" ".hg" ".svn" "node_modules" "target" "dist" "build" ".venv" "venv" "__pycache__"
    ".cpcache" ".cljs-cache" ".shadow-cljs" ".clj-kondo" ".lsp" ".idea" ".gradle" ".next" ".nuxt"
    "vendor" ".cache" "out" ".out" ".verification" ".verification-baseline" "book" "_site" "public"
    "site" "_book"})

(def ^:private manifest-kinds
  "Filename -> ecosystem label."
  {"deps.edn" :clojure
   "project.clj" :clojure
   "package.json" :node
   "Cargo.toml" :rust
   "pyproject.toml" :python
   "setup.py" :python
   "go.mod" :go
   "pom.xml" :maven
   "build.gradle" :gradle
   "build.gradle.kts" :gradle
   "Gemfile" :ruby
   "mix.exs" :elixir})

(defn- shape-label
  "Heuristic label for the multi-package shape.

   - polylith - `extensions/` AND `packages/` AND `bb.edn|deps.edn`
     at root; or any `polylith` directory.
   - submodules - `.gitmodules` at root.
   - workspace - multiple manifests of one kind in distinct subdirs."
  [^File root manifests-by-kind]
  (let
    [ext-dir
     (.exists (io/file root "extensions"))

     pkg-dir
     (.exists (io/file root "packages"))

     gitmod
     (.exists (io/file root ".gitmodules"))

     any-multi
     (some #(>= (long %) 2) (map (comp count val) manifests-by-kind))]

    (cond gitmod "submodules"
          (and ext-dir pkg-dir) "polylith"
          (and ext-dir (>= (long (count (get manifests-by-kind :clojure))) 2))
          "polylith-extensions-only"
          any-multi "workspace"
          :else nil)))

(defn snapshot
  "Walk `root` looking for known package manifests. Returns:

     {:shape   \"polylith\" | \"workspace\" | \"submodules\" | nil
      :totals  {:clojure N :node N ...}        ;; only kinds present
      :files   {:clojure [\"path/deps.edn\" ...]
                ...}
      :truncated? bool}

   The `:files` paths are relative to `root` and OMIT the manifest
   in the root itself (we only care about descendants - a single
   root-level deps.edn is not a monorepo)."
  ([root] (snapshot root nil))
  ([root
    {:keys [max-files deadline-ms]
     :or {max-files default-max-files deadline-ms default-deadline-ms}}]
   (let
     [^File root-file
      (cond (instance? File root) root
            (instance? Path root) (.toFile ^Path root)
            :else (java.io.File. (str root)))

      ^Path start
      (.toPath root-file)

      buckets
      (java.util.HashMap.)

      visited
      (long-array 1 0)

      deadline
      (+ (System/currentTimeMillis) (long deadline-ms))

      truncated
      (boolean-array 1 false)

      visitor
      (proxy [SimpleFileVisitor] []
        (preVisitDirectory [^Path dir ^BasicFileAttributes _attrs]
          (cond (> (System/currentTimeMillis) deadline) (do (aset truncated 0 true)
                                                            FileVisitResult/TERMINATE)
                (= dir start) FileVisitResult/CONTINUE
                :else (let [name (str (.getFileName dir))]
                        (if (contains? skip-directories name)
                          FileVisitResult/SKIP_SUBTREE
                          FileVisitResult/CONTINUE))))
        (visitFile [^Path file ^BasicFileAttributes _attrs]
          (let [count* (aget visited 0)]
            (cond (or (>= count* (long max-files)) (> (System/currentTimeMillis) deadline))
                  (do (aset truncated 0 true) FileVisitResult/TERMINATE)
                  :else (do (aset visited 0 (inc count*))
                            (let
                              [name (str (.getFileName file))
                               parent (.getParent file)]

                              ;; Ignore manifests sitting in the
                              ;; ROOT itself - only descendants
                              ;; signal a multi-package shape.
                              (when (and parent (not= parent start))
                                (when-let [kind (get manifest-kinds name)]
                                  (let
                                    [rel (paths/unixify (.relativize start file))
                                     cur (or (.get buckets kind) [])]

                                    (.put buckets kind (conj cur rel))))))
                            FileVisitResult/CONTINUE))))
        (visitFileFailed [^Path _file _exception] FileVisitResult/CONTINUE))]

     (try (Files/walkFileTree start visitor) (catch Throwable _ nil))
     (let
       [files-by-kind
        (into {} buckets)

        totals
        (into {}
              (map (fn [[k v]]
                     [k (count v)])
                   files-by-kind))

        shape
        (shape-label root-file files-by-kind)]

       {:shape shape :totals totals :files files-by-kind :truncated? (aget truncated 0)}))))
