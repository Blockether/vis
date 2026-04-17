(ns com.blockether.vis.loop.runtime.tools.git
  "Git tool bindings for the SCI sandbox. Kept separate from shared.clj
   to avoid a cyclic dependency (shared → knowledge.git → storage.db → shared)."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.loop.knowledge.git :as rlm-git]
   [com.blockether.vis.loop.storage.db :as db]))

(defn- strip-worktree-prefix
  [^String abs-path ^String worktree-abs]
  (if (str/starts-with? abs-path worktree-abs)
    (let [tail (subs abs-path (count worktree-abs))]
      (cond-> tail
        (and (seq tail) (= (first tail) java.io.File/separatorChar))
        (subs 1)))
    abs-path))

(defn- with-repo-for-path
  "Resolve the attached :repo entity that owns `path`, open its Repository,
   run `(f repo relative-path)`, close in finally."
  [db-info ^String path f]
  (let [repos (when db-info (db/db-list-repos db-info))]
    (cond
      (empty? repos)
      (throw (ex-info "No git repositories attached. Call rlm/ingest-git! first."
               {:type :rlm/no-git-repos}))
      (= 1 (count repos))
      (let [repo-meta (first repos)
            ^org.eclipse.jgit.lib.Repository repo (rlm-git/open-repo (:path repo-meta))]
        (when-not repo
          (throw (ex-info (str "Attached repo " (pr-str (:name repo-meta))
                            " no longer opens at " (pr-str (:path repo-meta)))
                   {:type :rlm/repo-open-failed
                    :repo-name (:name repo-meta) :repo-path (:path repo-meta)})))
        (try
          (let [wt (.getAbsolutePath (.getWorkTree repo))
                rel (strip-worktree-prefix (str path) wt)]
            (f repo rel))
          (finally (.close repo))))
      :else
      (let [pf (java.io.File. path)]
        (when-not (.isAbsolute pf)
          (throw (ex-info (str "Multi-repo mode requires an absolute path. Got "
                            (pr-str path) ". Attached repos: " (pr-str (mapv :name repos)))
                   {:type :rlm/no-repo-for-path :path path :reason :relative-path
                    :attached (mapv :name repos)})))
        (let [abs (.getAbsolutePath pf)
              hit (->> repos
                    (filter (fn [rm]
                              (let [rp (:path rm)]
                                (or (= abs rp)
                                  (str/starts-with? abs (str rp java.io.File/separator))))))
                    first)]
          (when-not hit
            (throw (ex-info (str "No attached git repo owns path " (pr-str path)
                              ". Pass an absolute path inside one of: " (pr-str (mapv :name repos)))
                     {:type :rlm/no-repo-for-path :path path :attached (mapv :name repos)})))
          (let [^org.eclipse.jgit.lib.Repository repo (rlm-git/open-repo (:path hit))]
            (try
              (let [wt (.getAbsolutePath (.getWorkTree repo))
                    rel (strip-worktree-prefix abs wt)]
                (f repo rel))
              (finally (.close repo)))))))))

(defn- repo-has-object?
  [^org.eclipse.jgit.lib.Repository repo ^String sha]
  (try
    (when-let [oid (.resolve repo sha)]
      (with-open [reader (.newObjectReader repo)] (.has reader oid)))
    (catch Exception _ false)))

(defn- with-repo-for-sha
  "Resolve the attached :repo entity whose object database contains `sha`."
  [db-info ^String sha f]
  (let [repos (when db-info (db/db-list-repos db-info))]
    (cond
      (empty? repos)
      (throw (ex-info "No git repositories attached. Call rlm/ingest-git! first."
               {:type :rlm/no-git-repos}))
      (= 1 (count repos))
      (let [repo-meta (first repos)
            ^org.eclipse.jgit.lib.Repository repo (rlm-git/open-repo (:path repo-meta))]
        (when-not repo
          (throw (ex-info (str "Attached repo " (pr-str (:name repo-meta))
                            " no longer opens at " (pr-str (:path repo-meta)))
                   {:type :rlm/repo-open-failed})))
        (try (f repo) (finally (.close repo))))
      :else
      (let [ref-like? (contains? #{"HEAD" "FETCH_HEAD" "ORIG_HEAD" "main" "master"} sha)]
        (when ref-like?
          (throw (ex-info (str "Ref " (pr-str sha)
                            " is ambiguous across multiple attached repos "
                            (pr-str (mapv :name repos)) ". Pass an actual SHA instead.")
                   {:type :rlm/ambiguous-ref :ref sha :attached (mapv :name repos)})))
        (loop [remaining repos]
          (if-let [rm (first remaining)]
            (let [^org.eclipse.jgit.lib.Repository repo (rlm-git/open-repo (:path rm))]
              (if (and repo (repo-has-object? repo sha))
                (try (f repo) (finally (.close repo)))
                (do (when repo (try (.close repo) (catch Exception _ nil)))
                  (recur (rest remaining)))))
            (throw (ex-info (str "SHA " (pr-str sha) " not found in any attached repo. Attached: "
                              (pr-str (mapv :name repos)))
                     {:type :rlm/no-repo-for-sha :sha sha :attached (mapv :name repos)}))))))))

(defn make-git-sci-bindings
  "Return the map of git-* SCI symbol → fn for a given `db-info`.
   Always bound when a DB is present; errors cleanly when no :repo entity exists."
  [db-info]
  {'git-search-commits
   (fn git-search-commits
     ([] (git-search-commits {}))
     ([opts] (when db-info (db/db-search-commits db-info opts))))
   'git-commit-history
   (fn git-commit-history
     ([] (git-commit-history {}))
     ([opts] (when db-info (db/db-search-commits db-info opts))))
   'git-commits-by-ticket
   (fn git-commits-by-ticket [ticket-ref]
     (when db-info (db/db-search-commits db-info {:ticket ticket-ref})))
   'git-commit-parents
   (fn git-commit-parents [sha]
     (when db-info (vec (:parents (db/db-commit-by-sha db-info sha)))))
   'git-file-history
   (fn git-file-history
     ([path] (git-file-history path {}))
     ([path opts] (with-repo-for-path db-info path (fn [repo rel] (rlm-git/file-history repo rel opts)))))
   'git-blame
   (fn git-blame [path from to]
     (with-repo-for-path db-info path (fn [repo rel] (rlm-git/blame repo rel from to))))
   'git-commit-diff
   (fn git-commit-diff [sha]
     (with-repo-for-sha db-info sha (fn [repo] (rlm-git/commit-diff repo sha))))})
