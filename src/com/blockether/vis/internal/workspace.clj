(ns com.blockether.vis.internal.workspace
  "Basic Git worktree-backed workspace manager.

   Runtime state lives outside the checkout in ~/.vis/workspaces.edn and
   materialized worktrees live under ~/.vis/workspaces/<repo-id>/<workspace-id>/.
   This namespace owns the real `git worktree add` call; callers get plain
   EDN records with stable paths."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str])
  (:import [java.io File]
           [java.time Instant]
           [java.util UUID]))

(set! *warn-on-reflection* true)

(def ^:dynamic *vis-home*
  "Override Vis runtime home for tests. Defaults to ~/.vis."
  nil)

(defn- now []
  (str (Instant/now)))

(defn- file-path [^File f]
  (.getCanonicalPath f))

(defn- vis-home
  [opts]
  (io/file (or (:vis-home opts)
             *vis-home*
             (io/file (System/getProperty "user.home") ".vis"))))

(defn- state-file
  [opts]
  (io/file (vis-home opts) "workspaces.edn"))

(defn- workspaces-dir
  [opts]
  (io/file (vis-home opts) "workspaces"))

(defn- ensure-parent!
  [^File file]
  (when-let [parent (.getParentFile file)]
    (.mkdirs parent)))

(defn- read-state
  [opts]
  (let [^File file (state-file opts)]
    (if (.exists file)
      (let [data (edn/read-string {:readers {} :default (fn [_ form] form)}
                   (slurp file))]
        (merge {:version 1 :workspaces {}} data))
      {:version 1 :workspaces {}})))

(defn- write-state!
  [opts state]
  (let [file (state-file opts)]
    (ensure-parent! file)
    (spit file (pr-str state))
    state))

(defn- update-state!
  [opts f & args]
  (write-state! opts (apply f (read-state opts) args)))

(defn- git-result
  [dir args]
  (let [argv (cond-> ["git"]
               dir (into ["-C" (file-path (io/file dir))])
               true (into args))]
    (assoc (apply sh/sh argv) :argv argv)))

(defn- git!
  [dir args]
  (let [result (git-result dir args)]
    (when-not (zero? (:exit result))
      (throw (ex-info (str "git failed: " (str/join " " (:argv result)))
               {:argv (:argv result)
                :exit (:exit result)
                :out  (:out result)
                :err  (:err result)})))
    (str/trim (or (:out result) ""))))

(defn- local-branch-exists?
  [repo-root branch]
  (zero? (:exit (git-result repo-root ["show-ref" "--verify" "--quiet"
                                       (str "refs/heads/" branch)]))))

(defn- discover-repo-root
  []
  (git! nil ["rev-parse" "--show-toplevel"]))

(defn- sanitize-id
  [s]
  (let [s (-> (str (or s "workspace"))
            str/lower-case
            (str/replace #"[^a-z0-9._-]+" "-")
            (str/replace #"(^-+|-+$)" ""))]
    (if (str/blank? s) "workspace" s)))

(defn- repo-id
  [repo-root]
  (let [root (file-path (io/file repo-root))
        name (sanitize-id (.getName (io/file root)))
        hash (Long/toUnsignedString (Integer/toUnsignedLong (hash root)) 36)]
    (str name "-" hash)))

(defn- workspace-id
  [branch]
  (str (sanitize-id (or branch "worktree")) "-" (subs (str (UUID/randomUUID)) 0 8)))

(defn- generated-branch
  [workspace-id]
  (str "vis/" workspace-id))

(defn- normalize-create-opts
  [opts]
  (let [opts         (or opts {})
        repo-root    (file-path (io/file (or (:repo-root opts) (discover-repo-root))))
        repo-id      (or (:repo-id opts) (repo-id repo-root))
        branch       (not-empty (str/trim (str (or (:branch opts) (:branch-name opts) ""))))
        workspace-id (or (:workspace-id opts) (workspace-id branch))
        branch       (or branch (generated-branch workspace-id))
        root         (file-path (io/file (workspaces-dir opts) repo-id workspace-id))]
    (assoc opts
      :repo-root repo-root
      :repo-id repo-id
      :workspace-id workspace-id
      :branch branch
      :root root)))

(defn create-worktree!
  "Create a Git worktree and record minimal runtime state.

   Options:
   - :repo-root      repository root; defaults to current git repository
   - :branch or :branch-name  branch to create from HEAD; generated when absent
   - :workspace-id  optional id, mostly for tests
   - :repo-id       optional stable repo bucket id
   - :vis-home      optional runtime home override; defaults to ~/.vis

   Returns the persisted workspace record."
  ([] (create-worktree! {}))
  ([opts]
   (let [{:keys [repo-root repo-id workspace-id branch root] :as opts}
         (normalize-create-opts opts)
         root-file (io/file root)]
     (ensure-parent! root-file)
     (if (local-branch-exists? repo-root branch)
       (git! repo-root ["worktree" "add" root branch])
       (git! repo-root ["worktree" "add" "-b" branch root "HEAD"]))
     (let [head   (git! root ["rev-parse" "HEAD"])
           record {:workspace/id workspace-id
                   :workspace/repo-id repo-id
                   :workspace/root root
                   :workspace/repo-root repo-root
                   :workspace/state :ready
                   :workspace/created-at (now)
                   :main {:branch branch
                          :head head}}]
       (update-state! opts assoc-in [:workspaces workspace-id] record)
       record))))

(defn- enrich-status
  [workspace]
  (let [root (:workspace/root workspace)]
    (try
      (let [branch (git! root ["rev-parse" "--abbrev-ref" "HEAD"])
            head   (git! root ["rev-parse" "HEAD"])
            dirty? (not (str/blank? (git! root ["status" "--porcelain"])))
            exists? (.exists (io/file root))]
        (assoc workspace
          :workspace/exists? exists?
          :git/branch branch
          :git/head head
          :git/dirty? dirty?))
      (catch Throwable t
        (assoc workspace
          :workspace/exists? (.exists (io/file root))
          :workspace/state :error
          :workspace/error (or (ex-message t) (str t)))))))

(defn workspace-status
  "Return persisted workspace status.

   No args returns a map of workspace id to status. One arg returns one
   workspace status. Two args allow tests to pass opts then id."
  ([]
   (workspace-status {}))
  ([opts-or-id]
   (if (map? opts-or-id)
     (into {}
       (map (fn [[id workspace]] [id (enrich-status workspace)]))
       (:workspaces (read-state opts-or-id)))
     (workspace-status {} opts-or-id)))
  ([opts workspace-id]
   (when-let [workspace (get-in (read-state opts) [:workspaces workspace-id])]
     (enrich-status workspace))))

(defn- roots
  [workspace]
  {:workspace/id (:workspace/id workspace)
   :workspace/root (:workspace/root workspace)
   :main/root (:workspace/root workspace)
   :repo/root (:workspace/repo-root workspace)})

(defn workspace-roots
  "Return workspace root paths.

   No args returns a map of workspace id to root maps. One id returns that
   workspace's roots. Two args allow tests to pass opts then id."
  ([]
   (workspace-roots {}))
  ([opts-or-id]
   (if (map? opts-or-id)
     (into {}
       (map (fn [[id workspace]] [id (roots workspace)]))
       (:workspaces (read-state opts-or-id)))
     (workspace-roots {} opts-or-id)))
  ([opts workspace-id]
   (some-> (get-in (read-state opts) [:workspaces workspace-id]) roots)))
