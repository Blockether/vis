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

(def ^:const default-inventory-max-repos 64)

(def ^:const default-deadline-ms 1000)

(def ^:const default-status-timeout-ms 500)

(def ^:private skip-directories
  #{".git" ".hg" ".svn" "node_modules" "target" "dist" "build" ".venv" "venv" "__pycache__"
    ".cpcache" ".cljs-cache" ".shadow-cljs" ".clj-kondo" ".lsp" ".idea" ".gradle" ".next" ".nuxt"
    "vendor" ".cache" "out" ".out" ".verification" ".verification-baseline" "book" "_site" "public"
    "site" "_book"})

(defonce ^:private inventory-cache (atom {}))

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

(defn- canonical-file
  ^File [^File f]
  (try (.getCanonicalFile f) (catch Throwable _ (.getAbsoluteFile f))))

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

(defn refresh-inventory!
  "Forget all cached repository-root inventories. Environment `refresh()` calls
   this after tree changes such as cloning or removing a repository."
  []
  (reset! inventory-cache {})
  nil)

(defn inventory
  "Return a lightweight, cached inventory of Git roots below `root`.

   Unlike `snapshot`, this performs no per-repository Git status work and is
   suitable for extension discovery. Known VCS metadata, cache, vendor, and
   build directories are skipped. The default scan is bounded at 64
   repositories.

   Shape:
     {:root <abs-root>
      :count 2
      :repositories [{:path <relative-path> :root <abs-root>} ...]
      :truncated? false}"
  ([root] (inventory root nil))
  ([root opts]
   (let
     [root-file
      (canonical-file (file-of root))

      root-path
      (canonical-path root-file)

      scan-opts
      {:max-files (long (or (:max-files opts) default-max-files))
       :max-repos (long (or (:max-repos opts) default-inventory-max-repos))
       :deadline-ms (long (or (:deadline-ms opts) default-deadline-ms))}

      cache-key
      [root-path scan-opts]]

     (or (get @inventory-cache cache-key)
         (let
           [[roots truncated?]
            (discover-roots root-file scan-opts)

            repositories
            (->> roots
                 (map (fn [^Path repo-path]
                        (let [repo-file (canonical-file (.toFile repo-path))]
                          {:path (rel-path (.toPath root-file) (.toPath repo-file))
                           :root (canonical-path repo-file)})))
                 (sort-by :path)
                 vec)

            result
            {:root root-path
             :count (count repositories)
             :repositories repositories
             :truncated? truncated?}]

           (swap! inventory-cache assoc cache-key result)
           result)))))

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
      (canonical-file (file-of root))

      root-path
      (canonical-path root-file)

      max-repos
      (long (or (:max-repos opts) default-max-repos))

      root-inventory
      (inventory root-file
                 (cond-> {:max-repos (max max-repos default-inventory-max-repos)}
                   (:max-files opts)
                   (assoc :max-files (:max-files opts))

                   (:deadline-ms opts)
                   (assoc :deadline-ms (:deadline-ms opts))))

      repository-rows
      (vec (take max-repos (:repositories root-inventory)))

      truncated?
      (or (:truncated? root-inventory) (> (:count root-inventory) max-repos))

      status-timeout-ms
      (long (or (:status-timeout-ms opts) default-status-timeout-ms))

      repos
      (->> repository-rows
           (mapv (fn [{:keys [path root]}]
                   (let
                     [repo-file
                      (file-of root)

                      summary
                      (try (git/snapshot repo-file {:status-timeout-ms status-timeout-ms})
                           (catch Throwable _ nil))]

                     (cond-> {:path path :root root}
                       summary
                       (merge (select-repo-summary summary)))))))]

     {:root root-path :count (count repos) :repositories repos :truncated? truncated?})))
