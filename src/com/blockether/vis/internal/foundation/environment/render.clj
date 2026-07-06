(ns com.blockether.vis.internal.foundation.environment.render
  "Build compact foundation environment data for `ctx`. No prompt labels.")

(defn- compact-runtime
  [{:keys [host git languages monorepo repositories]}]
  (cond-> {:host (select-keys host
                              [:cwd :user :home :shell :os-name :os-arch :os-version :locale :jvm
                               :time :timezone])}
    git
    (assoc :git
      (select-keys git
                   [:root :branch :detached? :detached-sha :clean? :dirty? :changes? :stale?
                    :upstream :ahead :behind :stash-count :submodules? :worktree? :modified :added
                    :changed :removed :missing :untracked :conflicting]))

    languages
    (assoc :languages
      (-> languages
          (select-keys [:total-files :total-bytes :primary :truncated? :elapsed-ms])
          (assoc :top (mapv #(select-keys % [:language :files :bytes-pct])
                            (take 5 (:languages languages))))))

    monorepo
    (assoc :monorepo (select-keys monorepo [:shape :totals :truncated?]))

    repositories
    (assoc :repositories
      (-> repositories
          (select-keys [:root :count :truncated?])
          (assoc :repositories (mapv #(select-keys %
                                                   [:path :branch :detached? :clean? :dirty?
                                                    :changes? :stale? :upstream :ahead :behind
                                                    :stash-count])
                                     (take 10 (:repositories repositories))))))))

(defn project-context
  "Build foundation-owned `(:project ctx)` data from runtime snapshot, project
   guidance, and scan warnings."
  [snapshot guidance warnings]
  (let [runtime
        (compact-runtime snapshot)

        root
        (or (get-in runtime [:git :root]) (get-in runtime [:host :cwd]))

        guidance*
        (when (:found? guidance) (select-keys guidance [:source :path :content]))

        warnings*
        (vec (or warnings []))]

    {:project (cond-> (assoc runtime :root root)
                guidance*
                (assoc :guidance guidance*)

                (seq warnings*)
                (assoc :warnings warnings*))}))
