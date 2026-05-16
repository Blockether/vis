(ns com.blockether.vis.ext.foundation.environment.render
  "Render foundation environment prompt fragments as Clojure data comments. No XML.")

(set! *warn-on-reflection* true)

(defn- compact-runtime
  [{:keys [host git languages monorepo repositories]}]
  (cond-> {:host (select-keys host
                   [:cwd :user :home :shell :os-name :os-arch :os-version
                    :locale :jvm :time :timezone])}
    git
    (assoc :git (select-keys git
                  [:root :branch :detached? :detached-sha :clean? :dirty?
                   :changes? :stale? :upstream :ahead :behind :stash-count
                   :submodules? :worktree? :modified :added :changed :removed
                   :missing :untracked :conflicting]))
    languages
    (assoc :languages (-> languages
                        (select-keys [:total-files :total-bytes :primary
                                      :truncated? :elapsed-ms])
                        (assoc :top (mapv #(select-keys % [:language :files :bytes-pct])
                                      (take 5 (:languages languages))))))
    monorepo
    (assoc :monorepo (select-keys monorepo [:shape :totals :truncated?]))
    repositories
    (assoc :repositories (-> repositories
                           (select-keys [:root :count :truncated?])
                           (assoc :repositories
                             (mapv #(select-keys % [:path :branch :detached?
                                                    :clean? :dirty? :changes?
                                                    :stale? :upstream :ahead
                                                    :behind :stash-count])
                               (take 10 (:repositories repositories))))))))

(defn render
  "Build a model-facing Clojure data snapshot of runtime environment."
  [snapshot]
  (str ";; ctx.runtime =\n"
    (pr-str (compact-runtime snapshot))
    "\n"))

(defn format-project-guidance-block
  "Render project guidance as comments."
  [{:keys [found? source path content]}]
  (when found?
    (str ";; ctx.project-guidance = "
      (pr-str {:source source :path path})
      "\n"
      content
      (when-not (.endsWith ^String content "\n") "\n"))))

(defn format-scan-warnings-block
  "Render scan warnings as comments."
  [warnings]
  (when (seq warnings)
    (str ";; ctx.scan-warnings = " (pr-str (vec warnings)))))
