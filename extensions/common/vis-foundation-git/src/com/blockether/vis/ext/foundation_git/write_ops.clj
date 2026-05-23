(ns com.blockether.vis.ext.foundation-git.write-ops
  "Write-side SCI op family under the `git/` alias: add, commit!, amend!,
   push!, fetch!. Pure JGit, no shell. Mirrors merge_ops.clj shape.

   Auth: SSH remotes use whatever ~/.ssh keys JGit's MINA backend
   discovers. HTTPS remotes need :credentials {:username :password} or
   {:token \"...\"}."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.workspace :as workspace])
  (:import
   [java.io File]
   [org.eclipse.jgit.api Git]
   [org.eclipse.jgit.transport RefSpec UsernamePasswordCredentialsProvider]))

(defn- open-git ^Git [] (Git/open ^File (workspace/cwd)))

(defn- coerce-paths [arg]
  (cond
    (= :all arg)                                  ["."]
    (string? arg)                                 [arg]
    (and (sequential? arg) (every? string? arg))  (vec arg)
    :else (throw (ex-info (str "git/add expects :all, a path string, or vec of strings, got " (pr-str arg))
                   {:type :foundation-git/invalid-opts :arg arg}))))

(defn add
  "Stage `arg`: :all, a path, or a vec of paths."
  [arg]
  (let [paths (coerce-paths arg)]
    (with-open [git (open-git)]
      (let [cmd (.add git)]
        (doseq [p paths] (.addFilepattern cmd p))
        (.call cmd)))
    {:op :add :paths paths}))

(defn- head-message [^Git git]
  (some-> (.. git log (setMaxCount 1) call) first .getFullMessage))

(defn commit!
  "Create a commit. Opts: {:message :all? :allow-empty? :amend? :no-edit?}.
   :all?    stages every modified TRACKED file first (git commit -a).
   :amend?  rewrites HEAD; :no-edit? keeps its message verbatim."
  [{:keys [message all? allow-empty? amend? no-edit?]}]
  (when (and no-edit? (not amend?))
    (throw (ex-info "git/commit! :no-edit? requires :amend? true"
             {:type :foundation-git/invalid-opts})))
  (with-open [git (open-git)]
    (when all?
      (.. git add (addFilepattern ".") (setUpdate true) call))
    (let [msg    (cond
                   (and no-edit? amend?)             (head-message git)
                   (some-> message str str/trim seq) (str/trim message)
                   :else
                   (throw (ex-info "git/commit! requires :message (or :amend? + :no-edit?)"
                            {:type :foundation-git/invalid-opts})))
          commit (.. git commit
                   (setMessage msg)
                   (setAmend (boolean amend?))
                   (setAllowEmpty (boolean allow-empty?))
                   call)
          sha    (.getName commit)]
      {:op        (if amend? :amend :commit)
       :sha       sha
       :short-sha (subs sha 0 7)
       :message   msg
       :amend?    (boolean amend?)})))

(defn amend!
  "Amend HEAD. () keeps the existing message; {:message \"...\"} rewrites it.
   Pass {:all? true} to restage every tracked modification before amending."
  ([] (amend! {}))
  ([opts]
   (let [opts (assoc opts :amend? true)
         opts (if (some-> (:message opts) str str/trim seq)
                opts
                (assoc opts :no-edit? true))]
     (commit! opts))))

(defn- ->credentials [{:keys [username password token]}]
  (when (or username password token)
    (UsernamePasswordCredentialsProvider.
      ^String (or username (when token "x-access-token") "")
      ^String (or token password ""))))

(defn- refspec-for [branch delete?]
  (cond
    (= :all branch) nil
    delete?         (RefSpec. (str ":refs/heads/" branch))
    :else           (RefSpec. (str "refs/heads/" branch ":refs/heads/" branch))))

(defn push!
  "Push to a remote. Opts: {:remote :branch :force? :tags? :delete? :credentials}."
  [{:keys [remote branch force? tags? delete? credentials]}]
  (with-open [git (open-git)]
    (let [remote (or remote "origin")
          repo   (.getRepository git)
          branch (or branch (.getBranch repo))
          spec   (refspec-for branch delete?)
          cmd    (.. git push (setRemote remote) (setForce (boolean force?)))]
      (when spec   (.setRefSpecs cmd (into-array RefSpec [spec])))
      (when tags?  (.setPushTags cmd))
      (when-let [cp (->credentials credentials)]
        (.setCredentialsProvider cmd cp))
      (let [results (.call cmd)
            updates (vec (mapcat (fn [r]
                                   (map (fn [u]
                                          {:remote-name (.getRemoteName u)
                                           :status      (str (.getStatus u))
                                           :message     (.getMessage u)})
                                     (.getRemoteUpdates r)))
                           results))]
        {:op      :push
         :remote  remote
         :branch  branch
         :force?  (boolean force?)
         :tags?   (boolean tags?)
         :delete? (boolean delete?)
         :updates updates}))))

(defn fetch!
  "Fetch from a remote. Opts: {:remote :credentials}."
  [{:keys [remote credentials]}]
  (with-open [git (open-git)]
    (let [cmd (.. git fetch (setRemote (or remote "origin")))]
      (when-let [cp (->credentials credentials)]
        (.setCredentialsProvider cmd cp))
      (let [res (.call cmd)]
        {:op       :fetch
         :remote   (or remote "origin")
         :messages (.getMessages res)
         :tracking-updates
         (mapv (fn [u] {:local-name  (.getLocalName u)
                        :remote-name (.getRemoteName u)
                        :result      (str (.getResult u))})
           (.getTrackingRefUpdates res))}))))

;; ============================================================================
;; SCI tool wrappers
;; ============================================================================

(defn- ok [v] (extension/success {:result v}))

(defn- render-edn
  "Tiny channel renderer: round-trip the result map as a pretty-printed EDN
   block. The model gets the raw map via SCI; this only shapes the TUI/channel
   preview."
  [result]
  {:vis.ir/kind :ir/code-block
   :language    "edn"
   :text        (pr-str result)})

(defn add-tool
  "Stage paths. (git/add \"file\"), (git/add [\"a\" \"b\"]), or (git/add :all)."
  [arg] (ok (add arg)))

(defn commit!-tool
  "Create a commit. Opts: {:message :all? :allow-empty? :amend? :no-edit?}."
  [opts] (ok (commit! opts)))

(defn amend!-tool
  "Amend HEAD. () keeps the existing message; {:message \"...\"} rewrites it."
  ([] (ok (amend!)))
  ([opts] (ok (amend! opts))))

(defn push!-tool
  "Push to remote. Opts: {:remote :branch :force? :tags? :delete? :credentials}."
  ([] (ok (push! {})))
  ([opts] (ok (push! opts))))

(defn fetch!-tool
  "Fetch from remote. Opts: {:remote :credentials}."
  ([] (ok (fetch! {})))
  ([opts] (ok (fetch! opts))))

(def add-symbol     (extension/symbol #'add-tool     {:symbol 'add     :tag :mutation :render-fn render-edn}))
(def commit!-symbol (extension/symbol #'commit!-tool {:symbol 'commit! :tag :mutation :render-fn render-edn}))
(def amend!-symbol  (extension/symbol #'amend!-tool  {:symbol 'amend!  :tag :mutation :render-fn render-edn}))
(def push!-symbol   (extension/symbol #'push!-tool   {:symbol 'push!   :tag :mutation :render-fn render-edn}))
(def fetch!-symbol  (extension/symbol #'fetch!-tool  {:symbol 'fetch!  :tag :mutation :render-fn render-edn}))

(def write-ops-symbols
  [add-symbol commit!-symbol amend!-symbol push!-symbol fetch!-symbol])
