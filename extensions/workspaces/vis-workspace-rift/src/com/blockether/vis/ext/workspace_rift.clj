(ns com.blockether.vis.ext.workspace-rift
  "Rift copy-on-write workspace backend."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.rift :as rift]
            [com.blockether.vis.core :as vis]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [java.nio.file FileVisitResult Files LinkOption Path SimpleFileVisitor]
           [java.nio.file.attribute FileAttribute PosixFilePermission]))

(defn- file-path ^String [path] (.getCanonicalPath (io/file path)))

(defn- delete-path! [^Path path] (Files/deleteIfExists path))

(defn- delete-tree!
  "Delete a temporary Rift probe tree.

   This is intentionally fail-loud. Recursive deletion cannot be atomic, but it
   must never silently leave a partial tree behind. We try every child first,
   then throw with both failures and remaining paths so callers get actionable
   cleanup diagnostics."
  [dir]
  (let
    [root (some-> dir
                  io/file
                  .toPath)]
    (when (and root (Files/exists root (make-array LinkOption 0)))
      (let [failures (atom [])]
        (Files/walkFileTree
          root
          (proxy [SimpleFileVisitor] []
            (visitFile [path _attrs]
              (try (delete-path! path)
                   (catch Throwable t
                     (swap! failures conj {:path (str path) :error (or (ex-message t) (str t))})))
              FileVisitResult/CONTINUE)
            (postVisitDirectory [path exc]
              (when exc
                (swap! failures conj {:path (str path) :error (or (ex-message exc) (str exc))}))
              (try (delete-path! path)
                   (catch Throwable t
                     (swap! failures conj {:path (str path) :error (or (ex-message t) (str t))})))
              FileVisitResult/CONTINUE)))
        (when (seq @failures)
          (let
            [remaining
             (try (with-open [stream (Files/walk root (make-array java.nio.file.FileVisitOption 0))]
                    (vec (map str (iterator-seq (.iterator stream)))))
                  (catch Throwable t
                    [(str "<remaining-path-scan-failed: " (or (ex-message t) (str t)) ">")]))]
            (throw (ex-info "Failed to fully delete Rift temporary tree"
                            {:type :workspace-rift/delete-tree-failed
                             :root (str root)
                             :failures @failures
                             :remaining remaining}))))))))

(defn- linked-git-worktree-source?
  [root]
  (try (let [dot-git (io/file root ".git")]
         (and (.isFile dot-git)
              (some-> (slurp dot-git)
                      str/trim
                      (str/starts-with? "gitdir:"))))
       (catch Throwable _ false)))

(defn- writable-dir?
  [path]
  (try (Files/isWritable (.toPath (io/file path))) (catch Throwable _ false)))

(defn- failure-data
  [{:keys [source-root store-root name]} t]
  (let
    [into-file
     (io/file store-root)

     parent-file
     (.getParentFile into-file)

     data
     (ex-data t)]

    {:source-root (file-path source-root)
     :source-linked-worktree? (linked-git-worktree-source? source-root)
     :clone-name (str name)
     :store-root (file-path store-root)
     :store-exists? (.exists into-file)
     :store-writable? (writable-dir? store-root)
     :store-parent (some-> parent-file
                           .getCanonicalPath)
     :store-parent-writable? (boolean (and parent-file (writable-dir? parent-file)))
     :rift-error-type (:type data)
     :rift-error-code (:code data)
     :rift-error-command (:command data)
     :rift-error-path (:path data)
     :error (or (ex-message t) (str t))}))

(defn- read-only-perms
  [src]
  (try (let [no-link (make-array LinkOption 0)]
         (into {}
               (comp (filter #(.isFile ^File %))
                     (map (fn [^File f]
                            (let [p (.toPath f)]
                              [p (Files/getPosixFilePermissions p no-link)])))
                     (filter (fn [[_ ^java.util.Set perms]]
                               (not (.contains perms PosixFilePermission/OWNER_WRITE)))))
               (file-seq (io/file src))))
       (catch Exception _ {})))

(defn- with-source-writable
  [src thunk]
  (let [orig (read-only-perms src)]
    (doseq [[^Path p ^java.util.Set perms] orig]
      (let [w (java.util.HashSet. perms)]
        (.add w PosixFilePermission/OWNER_WRITE)
        (Files/setPosixFilePermissions p w)))
    (try (thunk)
         (finally (doseq [[^Path p perms] orig]
                    (try (Files/setPosixFilePermissions p perms) (catch Exception _ nil)))))))

(defonce ^:private availability-cache (atom {}))

(defn- probe-key [source-root store-root] [(file-path source-root) (file-path store-root)])

(defn- probe!
  [{:keys [source-root store-root]}]
  (try (when (linked-git-worktree-source? source-root)
         (throw (ex-info "Linked Git worktrees are not supported as Rift sources"
                         {:type :workspace/unsupported-rift-source :reason :linked-git-worktree})))
       (Files/createDirectories (.toPath (io/file store-root)) (make-array FileAttribute 0))
       (let
         [src
          (str (Files/createTempDirectory (.toPath (io/file source-root))
                                          ".vis-rift-probe-"
                                          (make-array FileAttribute 0)))

          dst
          (str (Files/createTempDirectory (.toPath (io/file store-root))
                                          ".vis-rift-probe-"
                                          (make-array FileAttribute 0)))]

         (try (spit (io/file src "probe") "ok")
              (rift/init {:at src})
              (if (rift/create {:from src :name "clone" :into dst})
                {:available? true}
                {:available? false :reason :probe-returned-no-workspace})
              (finally (try (rift/remove! {:at (str (io/file dst "clone"))})
                            (catch Throwable _ nil))
                       (try (rift/gc) (catch Throwable _ nil))
                       (delete-tree! src)
                       (delete-tree! dst))))
       (catch Throwable t
         {:available? false
          :reason (or (:reason (ex-data t)) :probe-failed)
          :details (failure-data
                     {:source-root source-root :store-root store-root :name "availability-probe"}
                     t)})))

(defn- available?
  [{:keys [source-root store-root] :as opts}]
  (let
    [store-root
     (or store-root source-root)

     opts
     (assoc opts :store-root store-root)]

    (try (if (linked-git-worktree-source? source-root)
           {:available? false :reason :linked-git-worktree}
           (do (Files/createDirectories (.toPath (io/file store-root)) (make-array FileAttribute 0))
               (let [key (probe-key source-root store-root)]
                 (if (contains? @availability-cache key)
                   (get @availability-cache key)
                   (let [result (probe! opts)]
                     (swap! availability-cache assoc key result)
                     result)))))
         (catch Throwable t
           {:available? false
            :reason :probe-setup-failed
            :details {:error (or (ex-message t) (str t))}}))))

(defn- fork!
  [{:keys [source-root store-root name] :as opts}]
  (try (when (linked-git-worktree-source? source-root)
         (throw (ex-info "Linked Git worktrees are not supported as Rift sources"
                         {:type :workspace/unsupported-rift-source
                          :reason :linked-git-worktree
                          :source (file-path source-root)})))
       (rift/init {:at source-root})
       (Files/createDirectories (.toPath (io/file store-root)) (make-array FileAttribute 0))
       (with-source-writable source-root
                             #(rift/create {:from source-root :name name :into store-root}))
       (catch Throwable t
         (tel/log! {:level :warn :id ::fork-failed :data (failure-data opts t)}
                   (str "Rift workspace fork failed: " (or (ex-message t) (str t))))
         (throw t))))

(defn- discard! [{:keys [root]}] (rift/remove! {:at root}) (rift/gc))

(vis/register-extension! {:ext/name "workspace-rift"
                          :ext/description "Rift copy-on-write isolated workspace backend."
                          :ext/kind "workspace"
                          :ext/version "0.0.10"
                          :ext/owner "vis"
                          :ext/workspace-backends [(vis/workspace-backend
                                                     {:workspace.backend/id :rift
                                                      :workspace.backend/priority 100
                                                      :workspace.backend/capabilities
                                                      #{:isolated-fork :merge-back :rollback
                                                        :retained-revisions :parallel-safe}
                                                      :workspace.backend/available-fn available?
                                                      :workspace.backend/fork-fn fork!
                                                      :workspace.backend/discard-fn discard!})]})
