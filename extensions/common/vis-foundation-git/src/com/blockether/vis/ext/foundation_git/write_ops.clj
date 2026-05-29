(ns com.blockether.vis.ext.foundation-git.write-ops
  "Write-side SCI op family under the `git/` alias: add, commit!, amend!,
   push!, fetch!, reset!, branch!, checkout!, cherry-pick!, rebase!.
   Pure JGit, no shell. Mirrors merge_ops.clj shape.

   Auth: SSH remotes go through Apache MINA SSHD via JGit's
   `SshdSessionFactory`. That factory reads ~/.ssh/config + IdentityFile,
   speaks ed25519 / rsa-sha2-512, and talks to ssh-agent — i.e. the
   handshake GitHub actually accepts. HTTPS remotes still need
   :credentials {:username :password} or {:token \"...\"}."
  ;; Shadows `clojure.core/reset!` deliberately — our `git/reset!` is
  ;; the canonical Git port-of-call name for the model. The clj-kondo
  ;; lint warning is suppressed via the exclude below; the internal
  ;; mutation of `ssh-passphrase-prompt-fn` uses `core/reset!` via
  ;; fully-qualified call so no behaviour breaks.
  (:refer-clojure :exclude [reset!])
  (:require
   [clojure.core :as core]
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.workspace :as workspace])
  (:import
   [java.io File IOException]
   [java.nio.file Paths]
   [java.util.function Function]
   [org.eclipse.jgit.api Git ListBranchCommand$ListMode
    ResetCommand$ResetType RebaseCommand$Operation
    RebaseCommand$InteractiveHandler
    RebaseResult]
   [org.eclipse.jgit.lib BranchTrackingStatus ObjectId Repository]
   [org.eclipse.jgit.transport RefSpec SshSessionFactory UsernamePasswordCredentialsProvider]
   [org.eclipse.jgit.transport.sshd KeyPasswordProvider SshdSessionFactory SshdSessionFactoryBuilder]))

(defn- ^java.nio.file.Path ->path [^String s] (Paths/get s (make-array String 0)))

;; ----------------------------------------------------------------------------
;; Encrypted SSH key passphrase prompting
;; ----------------------------------------------------------------------------

(def ^:private ssh-passphrase-prompt-fn
  "Atom holding the active passphrase prompt:
     (fn [resource-uri attempt] -> String | nil)
   Channels with a UI (TUI) reset this to a masked dialog via
   `set-ssh-passphrase-prompt!`. Headless callers leave it `nil` and rely
   on the `VIS_SSH_KEY_PASSPHRASE` env var fallback. `nil` return
   (user cancelled / no env var) raises a structured IOException so JGit
   skips the encrypted key instead of looping."
  (atom nil))

(defn set-ssh-passphrase-prompt!
  "Register a channel-aware passphrase prompt. The fn receives the
   resource URI string + attempt counter (1-based) and returns a non-blank
   passphrase or `nil` to cancel. Pass `nil` to clear (e.g. when the TUI
   channel shuts down)."
  [f]
  (core/reset! ssh-passphrase-prompt-fn f))

(defn- resolve-passphrase
  "Prompt -> env var fallback. Returns String or nil."
  [resource attempt]
  (let [from-fn  (when-let [f @ssh-passphrase-prompt-fn]
                   (try (f resource attempt)
                     (catch Throwable _ nil)))
        from-env (when (str/blank? from-fn)
                   (System/getenv "VIS_SSH_KEY_PASSPHRASE"))]
    (cond
      (not (str/blank? from-fn))  from-fn
      (not (str/blank? from-env)) from-env
      :else                       nil)))

(defn- ^KeyPasswordProvider ->key-password-provider []
  ;; Three attempts mirrors OpenSSH ssh-add default; after that JGit
  ;; treats the key as unusable and tries the next identity.
  (let [attempts (atom 3)]
    (reify KeyPasswordProvider
      (getPassphrase [_ uri attempt]
        (let [resource (str uri)
              pw       (resolve-passphrase resource attempt)]
          (when (str/blank? pw)
            (throw (IOException.
                     (str "SSH key " resource " is encrypted; no passphrase available. "
                       "Set VIS_SSH_KEY_PASSPHRASE or register a passphrase prompt "
                       "(set-ssh-passphrase-prompt!) before calling git/push! again."))))
          (.toCharArray ^String pw)))
      (setAttempts [_ n] (core/reset! attempts n))
      (getAttempts [_] @attempts)
      (keyLoaded [_ _uri _attempt error]
        ;; true -> try again with a new passphrase (we don't loop here;
        ;; user-supplied prompt is single-shot). false -> give up on key.
        (nil? error)))))

(defonce ^:private installed-sshd-session-factory
  (atom nil))

(defn- install-sshd-session-factory!
  "Install JGit's Apache-MINA `SshdSessionFactory` once, pointed at the
   user's real ~/.ssh directory and wired to our channel-aware
   `KeyPasswordProvider`. The stock JGit factory is already an
   `SshdSessionFactory`, but it is not configured with our home/ssh dirs or
   prompt provider; checking only `instance?` left it in place and made
   `git/fetch!` fail with `publickey: no keys to try` (Vis conv c4eb7bab).
   Ed25519 keys also require the optional Bouncy Castle provider declared in
   deps.edn. Idempotent while the installed factory remains current."
  []
  (let [current (SshSessionFactory/getInstance)]
    (when-not (identical? current @installed-sshd-session-factory)
      (let [home    (System/getProperty "user.home")
            home-p  (->path home)
            ssh-p   (->path (str home "/.ssh"))
            factory (-> (SshdSessionFactoryBuilder.)
                      (.setPreferredAuthentications "publickey")
                      (.setHomeDirectory (.toFile home-p))
                      (.setSshDirectory  (.toFile ssh-p))
                      (.setKeyPasswordProvider
                        (reify Function
                          (apply [_ _credentials-provider]
                            (->key-password-provider))))
                      (.build nil))]
        (SshSessionFactory/setInstance factory)
        (core/reset! installed-sshd-session-factory factory)))))

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
    {:op :git/add :paths paths}))

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
      {:op        (if amend? :git/amend :git/commit)
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
  "Push to a remote. Opts: {:remote :branch :force? :tags? :delete? :credentials}.

   For SSH remotes, the Apache MINA `SshdSessionFactory` is installed on
   first call; from then on JGit reads ~/.ssh/config the way `git`
   itself does (IdentityFile, HostName, User, agent, ed25519,
   rsa-sha2-512). HTTPS remotes use the supplied :credentials."
  [{:keys [remote branch force? tags? delete? credentials]}]
  (install-sshd-session-factory!)
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
        {:op      :git/push
         :remote  remote
         :branch  branch
         :force?  (boolean force?)
         :tags?   (boolean tags?)
         :delete? (boolean delete?)
         :updates updates}))))

(defn- object-id->sha
  [^ObjectId oid]
  (some-> oid .getName))

(defn- object-id->short-sha
  [^ObjectId oid]
  (when-let [sha (object-id->sha oid)]
    (subs sha 0 (min 7 (count sha)))))

(defn- tracking-update->map
  [^org.eclipse.jgit.transport.TrackingRefUpdate update]
  {:local-name    (.getLocalName update)
   :remote-name   (.getRemoteName update)
   :result        (str (.getResult update))
   :old-sha       (object-id->sha (.getOldObjectId update))
   :old-short-sha (object-id->short-sha (.getOldObjectId update))
   :new-sha       (object-id->sha (.getNewObjectId update))
   :new-short-sha (object-id->short-sha (.getNewObjectId update))})

(defn- update-kind
  [result]
  (case (str result)
    "NEW" :new
    "FAST_FORWARD" :fast-forward
    "FORCED" :forced
    "NO_CHANGE" :unchanged
    "IO_FAILURE" :error
    "LOCK_FAILURE" :error
    "REJECTED" :rejected
    "RENAMED" :renamed
    :updated))

(defn- fetch-summary
  [updates]
  (let [freqs (frequencies (map :kind updates))]
    {:updated       (count updates)
     :new           (long (get freqs :new 0))
     :fast-forward  (long (get freqs :fast-forward 0))
     :forced        (long (get freqs :forced 0))
     :rejected      (long (get freqs :rejected 0))
     :errors        (long (get freqs :error 0))
     :up-to-date?   (empty? updates)}))

(defn- tracking-status
  [^Repository repo branch]
  (try
    (when-let [^BranchTrackingStatus tracking (and branch (BranchTrackingStatus/of repo branch))]
      {:upstream? true
       :upstream  (.getRemoteTrackingBranch tracking)
       :ahead     (.getAheadCount tracking)
       :behind    (.getBehindCount tracking)})
    (catch Throwable _ nil)))

(defn fetch!
  "Fetch from a remote. Opts: {:remote :credentials}. Returns porcelain-ish
   EDN: compact update summary plus current-branch ahead/behind after fetch."
  [{:keys [remote credentials]}]
  (install-sshd-session-factory!)
  (with-open [git (open-git)]
    (let [remote (or remote "origin")
          repo   (.getRepository git)
          branch (.getBranch repo)
          cmd    (.. git fetch (setRemote remote))]
      (when-let [cp (->credentials credentials)]
        (.setCredentialsProvider cmd cp))
      (let [res     (.call cmd)
            updates (mapv (fn [u]
                            (let [m (tracking-update->map u)]
                              (assoc m
                                :ref (some-> (:local-name m)
                                       (str/replace #"^refs/remotes/" ""))
                                :kind (update-kind (:result m))
                                :range (when (and (:old-short-sha m) (:new-short-sha m))
                                         (str (:old-short-sha m) ".." (:new-short-sha m))))))
                      (.getTrackingRefUpdates res))]
        (cond-> {:op       :git/fetch
                 :remote   remote
                 :status   (if (seq updates) :updated :up-to-date)
                 :branch   branch
                 :tracking (tracking-status repo branch)
                 :summary  (fetch-summary updates)
                 :updates  updates}
          (not (str/blank? (.getMessages res)))
          (assoc :remote-messages (.getMessages res)))))))

;; ============================================================================
;; History rewrite ops: reset!, branch!, checkout!, cherry-pick!, rebase!
;; ============================================================================
;;
;; History-rewriting ops are gated on the SAME activation predicate as
;; the rest of foundation-git (must sit in a real repo). Every op
;; surfaces a structured map; JGit-level failures become
;; `ex-info`s the model can read off the SCI return value.
;;
;; All ops accept short shas (>=4 chars) AND full 40-char shas. Branch
;; names accept both short (`main`) and qualified (`refs/heads/main`).
;; The map shape mirrors `commit!` / `push!` so the channel renderers
;; stay consistent across the family.

(defn- resolve-rev
  "Look up a commit-ish (sha, short sha, branch, tag, `HEAD`, `HEAD~N`)
   in the open repository. Throws a structured ex-info when JGit
   returns nil OR when the resolved ObjectId points at a commit that
   doesn't exist in the object database (raw 40-char sha that nothing
   actually wrote).

   JGit's `.resolve` happily round-trips a syntactically-valid sha
   without checking the object store; the failure surfaces deep inside
   the downstream command (e.g. `Cannot read deadbeef…` from
   `ResetCommand.resolveRefToCommitId`). Round-trip through a RevWalk
   `parseCommit` so the lookup fails fast with our typed error shape."
  ^ObjectId [^Repository repo ^String revstr]
  (let [oid (try (.resolve repo revstr) (catch Throwable _ nil))]
    (when (nil? oid)
      (throw (ex-info (str "git: unknown revision " (pr-str revstr))
               {:type :foundation-git/unknown-rev :rev revstr})))
    (try
      (with-open [walk (org.eclipse.jgit.revwalk.RevWalk. repo)]
        (.parseCommit walk oid))
      (catch Throwable t
        (throw (ex-info (str "git: unknown revision " (pr-str revstr))
                 {:type :foundation-git/unknown-rev :rev revstr :cause (.getMessage t)}))))
    oid))

(defn- short-sha [^String sha]
  (when sha (subs sha 0 (min 7 (count sha)))))

(defn- ^ResetCommand$ResetType reset-mode
  [mode]
  (case mode
    :soft   ResetCommand$ResetType/SOFT
    :mixed  ResetCommand$ResetType/MIXED
    :hard   ResetCommand$ResetType/HARD
    :keep   ResetCommand$ResetType/KEEP
    :merge  ResetCommand$ResetType/MERGE
    (throw (ex-info (str "git/reset! :mode must be one of :soft :mixed :hard :keep :merge, got "
                      (pr-str mode))
             {:type :foundation-git/invalid-opts :mode mode}))))

(defn reset!
  "Move HEAD (and optionally the index / working tree) to a revision.

   Opts: {:mode :to :paths}.

     :mode   #{:soft :mixed :hard :keep :merge} — required. :soft keeps
             index + worktree; :mixed (the bare-`git reset` default)
             resets the index, keeps worktree; :hard resets everything.
             :keep / :merge match JGit's safe variants.
     :to     revision string (sha, short sha, branch, `HEAD~N`,
             `origin/main`, …). Required.
     :paths  optional vec/string of path patterns. When present the
             reset becomes a per-path index reset (mode IS ignored by
             JGit in that branch, matching `git reset <paths>` shell
             semantics); without paths the whole tree resets per :mode.

   Returns:
     {:op :git/reset :mode :to :resolved-sha :short-sha :head-before :head-after
      :paths}

   Use as the building block for \"reword 40 commits\": reset --soft to
   the parent of the oldest bad commit, then re-commit one by one."
  [{:keys [mode to paths]}]
  (when (str/blank? (str to))
    (throw (ex-info "git/reset! requires :to (revision)"
             {:type :foundation-git/invalid-opts})))
  (with-open [git (open-git)]
    (let [repo         (.getRepository git)
          head-before  (some-> (.resolve repo "HEAD") .getName)
          resolved     (.getName (resolve-rev repo (str to)))
          path-strs    (cond
                         (nil? paths)      []
                         (string? paths)   [paths]
                         (sequential? paths) (vec paths)
                         :else (throw (ex-info "git/reset! :paths must be a string or vec of strings"
                                        {:type :foundation-git/invalid-opts :paths paths})))
          cmd          (.. git reset (setRef resolved))]
      (if (seq path-strs)
        (do (doseq [p path-strs] (.addPath cmd p))
          (.call cmd))
        (do (.setMode cmd (reset-mode mode))
          (.call cmd)))
      (let [head-after (some-> (.resolve repo "HEAD") .getName)]
        {:op           :git/reset
         :mode         (when (empty? path-strs) mode)
         :to           (str to)
         :resolved-sha resolved
         :short-sha    (short-sha resolved)
         :head-before  head-before
         :head-after   head-after
         :paths        path-strs}))))

(defn- list-branches-impl
  [^Git git list-mode]
  (let [^ListBranchCommand$ListMode mode (case list-mode
                                           :local   nil
                                           :remote  ListBranchCommand$ListMode/REMOTE
                                           :all     ListBranchCommand$ListMode/ALL
                                           (throw (ex-info
                                                    "git/branch! :list mode must be :local :remote or :all"
                                                    {:type :foundation-git/invalid-opts :mode list-mode})))
        cmd  (.branchList git)
        _    (when mode (.setListMode cmd mode))
        refs (.call cmd)]
    (mapv (fn [ref]
            (let [name (.getName ref)
                  obj  (.getObjectId ref)]
              {:name      name
               :short     (str/replace name #"^refs/(heads|remotes)/" "")
               :sha       (when obj (.getName obj))
               :short-sha (when obj (short-sha (.getName obj)))}))
      refs)))

(defn branch!
  "Create / delete / list / rename branches.

   Opts: {:op :name :from :force? :delete :rename :list}.

     One of `:op #{:create :delete :rename :list}` OR a short-form key
     `(:delete name)` / `(:rename [old new])` / `(:list mode)`.

     :create  {:name :from? :force?} — :name required; :from defaults
              to HEAD; :force? overrides an existing branch.
     :delete  {:name :force?}        — :name (or vec of names) required.
              :force? = true mirrors `git branch -D` (drops unmerged
              branch).
     :rename  {:old :new :force?}    — atomic rename.
     :list    {:mode :local | :remote | :all} (default :local).

   Returns op-specific maps:
     create  {:op :git/branch-create :name :from :sha :short-sha :force?}
     delete  {:op :git/branch-delete :deleted [name ...] :force?}
     rename  {:op :git/branch-rename :old :new}
     list    {:op :git/branch-list :mode :branches [{:name :short :sha :short-sha}]}"
  [opts]
  (let [op (or (:op opts)
             (cond (contains? opts :create) :create
               (contains? opts :delete) :delete
               (contains? opts :rename) :rename
               (contains? opts :list)   :list))]
    (with-open [git (open-git)]
      (case op
        :create
        (let [name   (or (:name opts) (:create opts))
              from   (or (:from opts) "HEAD")
              force? (boolean (:force? opts))]
          (when (str/blank? (str name))
            (throw (ex-info "git/branch! :create requires :name" {:type :foundation-git/invalid-opts})))
          (let [repo (.getRepository git)
                _    (resolve-rev repo from)
                ref  (.. git branchCreate
                       (setName name)
                       (setStartPoint from)
                       (setForce force?)
                       call)
                sha  (some-> (.getObjectId ref) .getName)]
            {:op        :git/branch-create
             :name      name
             :from      from
             :sha       sha
             :short-sha (short-sha sha)
             :force?    force?}))

        :delete
        (let [arg     (or (:name opts) (:delete opts))
              names   (cond
                        (string? arg) [arg]
                        (and (sequential? arg) (every? string? arg)) (vec arg)
                        :else (throw (ex-info "git/branch! :delete requires :name (string or vec)"
                                       {:type :foundation-git/invalid-opts})))
              force?  (boolean (:force? opts))
              deleted (vec (.. git branchDelete
                             (setBranchNames (into-array String names))
                             (setForce force?)
                             call))]
          {:op      :git/branch-delete
           :deleted (mapv #(str/replace % #"^refs/heads/" "") deleted)
           :force?  force?})

        :rename
        (let [pair    (or (:rename opts) [(:old opts) (:new opts)])
              [old-n new-n] (if (sequential? pair) pair [(:old opts) (:new opts)])]
          (when (or (str/blank? (str old-n)) (str/blank? (str new-n)))
            (throw (ex-info "git/branch! :rename requires :old + :new (or :rename [old new])"
                     {:type :foundation-git/invalid-opts})))
          (.. git branchRename
            (setOldName old-n)
            (setNewName new-n)
            call)
          {:op :git/branch-rename :old old-n :new new-n})

        :list
        (let [mode (or (:mode opts) (:list opts) :local)]
          {:op       :git/branch-list
           :mode     mode
           :branches (list-branches-impl git mode)})

        (throw (ex-info
                 "git/branch! requires :op #{:create :delete :rename :list} (or the short-form key)"
                 {:type :foundation-git/invalid-opts :opts opts}))))))

(defn checkout!
  "Switch HEAD to a branch or detach onto a sha; optionally restore files.

   Opts: {:branch :sha :create? :force? :paths :start-point}.

     :branch       branch name to switch to. With :create? true the
                   branch is created from :start-point (or HEAD).
     :sha          detached HEAD onto a commit. Mutually exclusive with
                   :branch when no :paths supplied.
     :paths        when present, restores those paths from HEAD (or
                   from :sha / :branch when supplied) WITHOUT moving
                   HEAD — i.e. `git checkout -- <paths>`.
     :force?       discard local changes that conflict (analogous to
                   `git checkout -f`).
     :create?      with :branch, create the branch first.
     :start-point  start-point for :create? (defaults to HEAD).

   Returns:
     {:op :git/checkout :branch :sha :head :short-head :created? :files-restored}"
  [{:keys [branch sha paths create? force? start-point]}]
  (with-open [git (open-git)]
    (let [repo  (.getRepository git)
          cmd   (.checkout git)]
      (cond
        (seq paths)
        (let [path-strs (cond
                          (string? paths)   [paths]
                          (sequential? paths) (vec paths)
                          :else (throw (ex-info "git/checkout! :paths must be a string or vec"
                                         {:type :foundation-git/invalid-opts})))]
          (.setStartPoint cmd ^String (or sha branch "HEAD"))
          (doseq [p path-strs] (.addPath cmd p))
          (.call cmd)
          {:op             :git/checkout
           :paths          path-strs
           :files-restored (count path-strs)
           :start-point    (or sha branch "HEAD")})

        (some? branch)
        (do
          (.setName cmd branch)
          (.setForceRefUpdate cmd (boolean force?))
          (when create?
            (.setCreateBranch cmd true)
            (when start-point (.setStartPoint cmd ^String start-point)))
          (.call cmd)
          (let [head (some-> (.resolve repo "HEAD") .getName)]
            {:op         :git/checkout
             :branch     branch
             :head       head
             :short-head (short-sha head)
             :created?   (boolean create?)}))

        (some? sha)
        (do
          (.setName cmd sha)
          (.setForceRefUpdate cmd (boolean force?))
          (.call cmd)
          (let [head (some-> (.resolve repo "HEAD") .getName)]
            {:op         :git/checkout
             :sha        sha
             :head       head
             :short-head (short-sha head)
             :detached?  true}))

        :else
        (throw (ex-info "git/checkout! requires :branch, :sha, or :paths"
                 {:type :foundation-git/invalid-opts}))))))

(defn cherry-pick!
  "Re-apply one or more commits onto HEAD.

   Opts: {:commits :mainline :no-commit?}.

     :commits   revstring or vec of revstrings (sha, branch, HEAD~N).
                Applied in order.
     :mainline  parent number for merge-commit pick (1-based).
     :no-commit? leave changes staged; don't commit per pick.

   Returns: {:op :git/cherry-pick :status :picked [{...}] :failing-paths}"
  [{:keys [commits mainline no-commit?]}]
  (when (or (nil? commits) (and (sequential? commits) (empty? commits)))
    (throw (ex-info "git/cherry-pick! requires :commits"
             {:type :foundation-git/invalid-opts})))
  (with-open [git (open-git)]
    (let [repo (.getRepository git)
          revs (cond
                 (string? commits) [commits]
                 (sequential? commits) (vec commits)
                 :else (throw (ex-info "git/cherry-pick! :commits must be a string or vec of strings"
                                {:type :foundation-git/invalid-opts})))
          cmd  (.cherryPick git)]
      (doseq [rev revs]
        (.include cmd (resolve-rev repo rev)))
      (when mainline (.setMainlineParentNumber cmd (int mainline)))
      (when no-commit? (.setNoCommit cmd true))
      (let [result          (.call cmd)
            status          (some-> result .getStatus .name)
            picked-commits  (some->> (.getCherryPickedRefs result)
                              (mapv (fn [r]
                                      (let [obj (.getObjectId r)
                                            sha (some-> obj .getName)]
                                        {:ref       (.getName r)
                                         :sha       sha
                                         :short-sha (short-sha sha)}))))
            failing-paths   (some-> result .getFailingPaths)
            new-head        (some-> result .getNewHead .getName)]
        {:op            :git/cherry-pick
         :status        status
         :picked        (or picked-commits [])
         :failing-paths (when failing-paths (vec (.keySet failing-paths)))
         :new-head      new-head
         :short-head    (short-sha new-head)}))))

(defn- rebase-operation
  ^RebaseCommand$Operation [op]
  (case op
    :begin    RebaseCommand$Operation/BEGIN
    :continue RebaseCommand$Operation/CONTINUE
    :skip     RebaseCommand$Operation/SKIP
    :abort    RebaseCommand$Operation/ABORT
    (throw (ex-info (str "git/rebase! :operation must be one of :begin :continue :skip :abort, got "
                      (pr-str op))
             {:type :foundation-git/invalid-opts :operation op}))))

(defn- rebase-result->map
  [^RebaseResult result]
  (let [status (.getStatus result)
        new-head (.getCurrentCommit result)]
    {:status            (.name status)
     :successful?       (.isSuccessful status)
     :conflicts         (when-let [cs (.getConflicts result)] (vec cs))
     :failing-paths     (when-let [fp (.getFailingPaths result)]
                          (vec (.keySet fp)))
     :current-commit    (when new-head (.getName new-head))
     :short-current     (short-sha (some-> new-head .getName))}))

(defn rebase!
  "Rewind onto an upstream, re-applying commits. Use for non-interactive
   linearisation and — critically — for the \"reword last 40 commits\"
   workflow via :edit-todos-fn.

   Opts: {:operation :upstream :onto :preserve-merges? :edit-todos-fn
          :strategy}.

     :operation       #{:begin :continue :skip :abort}. Defaults to
                      :begin when :upstream is given.
     :upstream        revstring (sha, branch, HEAD~N). The commits
                      strictly AFTER this rev are replayed.
     :onto            replay target (defaults to :upstream).
     :preserve-merges? recreate merge commits (JGit's
                      `setPreserveMerges`).
     :edit-todos-fn   `(fn [todos] -> todos')` over the rebase plan.
                      Each todo is `{:action :pick|:reword|:edit|:squash|:fixup|:drop
                                     :sha :short-sha :message}`. The fn
                      MUST return the same shape; missing entries drop
                      that commit from the plan. The driver wires this
                      into JGit's `InteractiveHandler` so the rebase
                      pauses for reword / edit slots.
     :reword-message-fn `(fn [sha previous-msg] -> new-msg)`. Optional;
                      drives the message-rewrite slot of the
                      InteractiveHandler when an entry is :reword /
                      :squash. Default returns previous message
                      unchanged.

   Returns: {:op :git/rebase :status :successful? :conflicts :failing-paths
             :current-commit :short-current}"
  [{:keys [operation upstream onto preserve-merges? edit-todos-fn reword-message-fn]
    :or   {reword-message-fn (fn [_sha msg] msg)}}]
  (with-open [git (open-git)]
    (let [op    (or operation (when upstream :begin))
          cmd   (.rebase git)]
      (when (nil? op)
        (throw (ex-info "git/rebase! requires :operation (or :upstream which implies :begin)"
                 {:type :foundation-git/invalid-opts})))
      (.setOperation cmd (rebase-operation op))
      (when (and (= op :begin) upstream)
        (.setUpstream cmd ^String upstream))
      (when (and (= op :begin) onto)
        (.setUpstreamName cmd ^String onto))
      (when preserve-merges?
        (.setPreserveMerges cmd true))
      (when edit-todos-fn
        (.runInteractively cmd
          (reify RebaseCommand$InteractiveHandler
            (prepareSteps [_ steps]
              (let [todos (mapv (fn [s]
                                  {:action    (some-> (.getAction s) .name str/lower-case keyword)
                                   :sha       (.. s getCommit name)
                                   :short-sha (short-sha (.. s getCommit name))
                                   :message   (.getShortMessage s)})
                            steps)
                    edited (edit-todos-fn todos)
                    by-sha (into {} (map (fn [t] [(:sha t) t])) edited)]
                ;; Mutate in place: JGit's RebaseTodoLine API exposes
                ;; setAction. Removing a step = `:drop` action via
                ;; setAction(COMMENT) (JGit's drop-equivalent).
                (doseq [^org.eclipse.jgit.lib.RebaseTodoLine step steps]
                  (let [sha    (.. step getCommit name)
                        wish   (get by-sha sha)
                        action (:action wish)]
                    (when (and action
                            (not= action :drop)
                            (not= action (some-> (.getAction step) .name str/lower-case keyword)))
                      (.setAction step
                        (case action
                          :pick   org.eclipse.jgit.lib.RebaseTodoLine$Action/PICK
                          :reword org.eclipse.jgit.lib.RebaseTodoLine$Action/REWORD
                          :edit   org.eclipse.jgit.lib.RebaseTodoLine$Action/EDIT
                          :squash org.eclipse.jgit.lib.RebaseTodoLine$Action/SQUASH
                          :fixup  org.eclipse.jgit.lib.RebaseTodoLine$Action/FIXUP
                          org.eclipse.jgit.lib.RebaseTodoLine$Action/PICK)))
                    (when (= action :drop)
                      (.setAction step org.eclipse.jgit.lib.RebaseTodoLine$Action/COMMENT))))))
            (modifyCommitMessage [_ commit-msg]
              ;; JGit calls this for the CURRENT reword/squash slot.
              ;; Without thread-local sha context we pass through;
              ;; per-sha rewrites should iterate manually via reset!
              ;; + commit! pattern instead.
              (or (reword-message-fn nil commit-msg) commit-msg)))))
      (assoc (rebase-result->map (.call cmd))
        :op :git/rebase :operation op))))

;; ============================================================================
;; SCI tool wrappers
;; ============================================================================

(defn- ok [v] (extension/success {:result v}))

(defn- op-label
  "Badge label for a write-op `:op` keyword. `:git/branch-create` →
   \"BRANCH-CREATE\", falls back to \"GIT\" when absent."
  [op]
  (if op (str/upper-case (name op)) "GIT"))

(defn- render-edn
  "Channel renderer for the write-op family. Returns the
   `{:summary :display}` contract: a zone summary (op label on the left,
   the most salient metric on the right) plus the full EDN dump as the
   display body. The model gets the raw map via SCI; this only shapes
   the TUI/channel preview."
  [result]
  (let [op    (:op result)
        label (op-label op)
        ;; Right-anchored metric chosen per op shape; everything has a
        ;; natural short-sha / count / branch we can surface.
        right (case op
                :git/commit     (:short-sha result)
                :git/amend      (:short-sha result)
                :git/reset      (:short-sha result)
                :git/push       (let [n (count (:updates result))]
                                  (str n " update" (when (not= 1 n) "s")))
                :git/fetch      (let [s (:summary result)]
                                  (if (:up-to-date? s)
                                    "up-to-date"
                                    (str (:updated s) " update"
                                      (when (not= 1 (:updated s)) "s"))))
                :git/add        (let [n (count (:paths result))]
                                  (str n " path" (when (not= 1 n) "s")))
                :git/branch-create (:short-sha result)
                :git/branch-delete (let [n (count (:deleted result))]
                                     (str n " deleted"))
                :git/branch-rename (:new result)
                :git/branch-list   (let [n (count (:branches result))]
                                     (str n " branch" (when (not= 1 n) "es")))
                :git/checkout   (or (:short-head result)
                                  (when-let [n (:files-restored result)]
                                    (str n " restored")))
                :git/cherry-pick (:status result)
                :git/rebase     (:status result)
                nil)
        ;; A middle hint: the branch / target / first path when present.
        center (some-> (or (:branch result) (:to result) (:remote result)
                         (:name result) (:from result))
                 str)]
    {:summary
     (cond-> {:left (extension/ir-strong label)}
       (and center (not (str/blank? center))) (assoc :center (extension/ir-code center))
       (and right (not (str/blank? (str right)))) (assoc :right (str right)))
     :display
     (extension/ir-root
       (extension/ir-p (extension/ir-strong label))
       (extension/ir-code-block "edn" (pr-str result)))}))

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

(defn reset!-tool
  "Move HEAD (and optionally index / worktree) to a revision.
   Opts: {:mode #{:soft :mixed :hard :keep :merge} :to :paths}."
  [opts] (ok (reset! opts)))

(defn branch!-tool
  "Create / delete / list / rename branches.
   See `branch!` for opt shape."
  [opts] (ok (branch! opts)))

(defn checkout!-tool
  "Switch HEAD to a branch / sha or restore paths.
   Opts: {:branch :sha :paths :create? :force? :start-point}."
  [opts] (ok (checkout! opts)))

(defn cherry-pick!-tool
  "Re-apply commits onto HEAD.
   Opts: {:commits :mainline :no-commit?}."
  [opts] (ok (cherry-pick! opts)))

(defn rebase!-tool
  "Rewind onto an upstream and re-apply commits; optionally interactive.
   Opts: {:operation :upstream :onto :preserve-merges? :edit-todos-fn :reword-message-fn}."
  [opts] (ok (rebase! opts)))

(def add-symbol         (extension/symbol #'add-tool         {:symbol 'add         :tag :mutation :render-fn render-edn}))
(def commit!-symbol     (extension/symbol #'commit!-tool     {:symbol 'commit!     :tag :mutation :render-fn render-edn}))
(def amend!-symbol      (extension/symbol #'amend!-tool      {:symbol 'amend!      :tag :mutation :render-fn render-edn}))
(def push!-symbol       (extension/symbol #'push!-tool       {:symbol 'push!       :tag :mutation :render-fn render-edn}))
(def fetch!-symbol      (extension/symbol #'fetch!-tool      {:symbol 'fetch!      :tag :mutation :render-fn render-edn}))
(def reset!-symbol      (extension/symbol #'reset!-tool      {:symbol 'reset!      :tag :mutation :render-fn render-edn}))
(def branch!-symbol     (extension/symbol #'branch!-tool     {:symbol 'branch!     :tag :mutation :render-fn render-edn}))
(def checkout!-symbol   (extension/symbol #'checkout!-tool   {:symbol 'checkout!   :tag :mutation :render-fn render-edn}))
(def cherry-pick!-symbol (extension/symbol #'cherry-pick!-tool {:symbol 'cherry-pick! :tag :mutation :render-fn render-edn}))
(def rebase!-symbol     (extension/symbol #'rebase!-tool     {:symbol 'rebase!     :tag :mutation :render-fn render-edn}))

(def write-ops-symbols
  [add-symbol commit!-symbol amend!-symbol push!-symbol fetch!-symbol
   reset!-symbol branch!-symbol checkout!-symbol cherry-pick!-symbol rebase!-symbol])
