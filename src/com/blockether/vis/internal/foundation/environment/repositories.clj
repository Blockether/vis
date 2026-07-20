(ns com.blockether.vis.internal.foundation.environment.repositories
  "Bounded discovery of multiple Git repositories below the current
   project root. This catches multirepo workspaces where the user's cwd
   is a parent directory or a primary repo that vendors sibling/nested
   repos outside `.gitmodules`.

   Returns compact per-repo Git summaries for the system prompt. Full
   status walks are bounded per repo by `git/snapshot`; the repository
   scan itself is bounded by max files, max repos, and a wall-clock
   deadline. Never throws."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.foundation.environment.git :as git]
            [com.blockether.vis.internal.paths :as paths])
  (:import (java.io File)
           (java.nio.file FileVisitResult Files Path SimpleFileVisitor)
           (java.nio.file.attribute BasicFileAttributes)
           (java.util LinkedHashSet)))

(def ^:const default-max-files 20000)

(def ^:const default-max-repos 12)

(def ^:const default-deadline-ms 1000)

(def ^:const default-status-timeout-ms 500)

(def ^:private skip-directories
  #{".git" ".hg" ".svn" "node_modules" "target" "dist" "build" ".venv" "venv" "__pycache__"
    ".cpcache" ".cljs-cache" ".shadow-cljs" ".clj-kondo" ".lsp" ".idea" ".gradle" ".next" ".nuxt"
    "vendor" ".cache" "out" ".out" ".verification" ".verification-baseline" "book" "_site" "public"
    "site" "_book"})

(defn- file-of
  ^File [root]
  (cond (instance? File root) root
        (instance? Path root) (.toFile ^Path root)
        :else (io/file (str root))))

(defn- repo-root?
  [^Path dir]
  (Files/exists (.resolve dir ".git") (make-array java.nio.file.LinkOption 0)))

(defn- add-root!
  [^LinkedHashSet roots ^Path dir ^long max-repos]
  (when (< (.size roots) max-repos) (.add roots dir)))

(defn- canonical-path
  ^String [^File f]
  (try (.getCanonicalPath f) (catch Throwable _ (.getAbsolutePath f))))

(defn- rel-path
  ^String [^Path start ^Path repo-root]
  ;; Display/relative paths are ALWAYS `/`-separated, on every OS.
  (let [rel (paths/unixify (.relativize start repo-root))]
    (if (empty? rel) "." rel)))

(defn- select-repo-summary
  [repo-map]
  (select-keys repo-map
               [:root :git-dir :branch :detached? :detached-sha :worktree? :submodules? :clean?
                :dirty? :changes? :status-unavailable? :modified :added :changed :removed :missing
                :untracked :conflicting :stash-count :upstream :ahead :behind :stale?]))

(defn- discover-roots
  [^File root-file
   {:keys [max-files max-repos deadline-ms]
    :or {max-files default-max-files max-repos default-max-repos deadline-ms default-deadline-ms}}]
  (let
    [^Path start
     (.toPath root-file)

     roots
     (LinkedHashSet.)

     visited
     (long-array 1 0)

     truncated
     (boolean-array 1 false)

     deadline
     (+ (System/currentTimeMillis) (long deadline-ms))

     stop?
     (fn []
       (or (> (System/currentTimeMillis) deadline)
           (>= (.size roots) (long max-repos))
           (>= (aget visited 0) (long max-files))))

     visitor
     (proxy [SimpleFileVisitor] []
       (preVisitDirectory [^Path dir ^BasicFileAttributes _attrs]
         (cond (stop?) (do (aset truncated 0 true) FileVisitResult/TERMINATE)
               (= dir start) (do (when (repo-root? dir) (add-root! roots dir (long max-repos)))
                                 FileVisitResult/CONTINUE)
               :else (let [name (str (.getFileName dir))]
                       (cond (contains? skip-directories name) FileVisitResult/SKIP_SUBTREE
                             (repo-root? dir) (do (add-root! roots dir (long max-repos))
                                                  (if (>= (.size roots) (long max-repos))
                                                    (do (aset truncated 0 true)
                                                        FileVisitResult/TERMINATE)
                                                    FileVisitResult/SKIP_SUBTREE))
                             :else FileVisitResult/CONTINUE))))
       (visitFile [^Path _file ^BasicFileAttributes _attrs]
         (let [n (inc (aget visited 0))]
           (aset visited 0 n)
           (if (stop?)
             (do (aset truncated 0 true) FileVisitResult/TERMINATE)
             FileVisitResult/CONTINUE)))
       (visitFileFailed [^Path _file ^java.io.IOException _exception] FileVisitResult/CONTINUE))]

    (try (Files/walkFileTree start visitor) (catch Throwable _ nil))
    [(vec roots) (aget truncated 0)]))

(defn snapshot
  "Discover Git repositories below `root` and return compact summaries.

   Shape:
     {:root <abs-root>
      :count 2
      :repositories [{:path <relative-path> :root <abs-root> :branch <branch> ...}]
      :truncated? false}"
  ([root] (snapshot root nil))
  ([root opts]
   (let
     [root-file
      (file-of root)

      root-path
      (canonical-path root-file)

      [roots truncated?]
      (discover-roots root-file opts)

      status-timeout-ms
      (long (or (:status-timeout-ms opts) default-status-timeout-ms))

      repos
      (->> roots
           (mapv (fn [^Path repo-path]
                   (let
                     [repo-file
                      (.toFile repo-path)

                      summary
                      (try (git/snapshot repo-file {:status-timeout-ms status-timeout-ms})
                           (catch Throwable _ nil))]

                     (cond->
                       {:path (rel-path (.toPath root-file) repo-path)
                        :root (canonical-path repo-file)}
                       summary
                       (merge (select-repo-summary summary)))))))]

     {:root root-path :count (count repos) :repositories repos :truncated? truncated?})))
