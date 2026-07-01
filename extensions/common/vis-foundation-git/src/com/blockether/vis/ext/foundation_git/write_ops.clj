(ns com.blockether.vis.ext.foundation-git.write-ops
  "Write-side op family under the `git/` alias: add, commit!, amend!,
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
   [com.blockether.vis.ext.foundation-git.render :as render]
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
   [org.eclipse.jgit.transport.sshd KeyPasswordProvider SshdSessionFactoryBuilder]))

(defn- ->path ^java.nio.file.Path [^String s] (Paths/get s (make-array String 0)))

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

(defn- ->key-password-provider ^KeyPasswordProvider []
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

(defn- short-sha
  "7-char producer abbreviation (git's default abbrev) — the ONE
   short-sha every write-op result carries."
  [^String sha]
  (when sha (subs sha 0 (min 7 (count sha)))))

(defn- open-git ^Git [] (Git/open ^File (workspace/cwd)))

(defn- coerce-paths [arg]
  (cond
    (= :all arg)                                  ["."]
    ;; The tool bridge can't carry a Clojure keyword, so the string
    ;; ":all" / "all" crosses as a plain string — treat it as :all too,
    ;; otherwise it silently stages a literal (non-existent) pathspec.
    (and (string? arg) (#{":all" "all"} arg))     ["."]
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
  "Create a commit. Opts: {\"message\": ..., \"is_all\": bool, \"is_allow_empty\": bool, \"is_amend\": bool, \"is_no_edit\": bool}.
   is_all    stages every modified TRACKED file first (git commit -a).
   is_amend  rewrites HEAD; is_no_edit keeps its message verbatim."
  [{:keys [message is_all is_allow_empty is_amend is_no_edit]}]
  (when (and is_no_edit (not is_amend))
    (throw (ex-info "git_commit is_no_edit requires is_amend true"
                    {:type :foundation-git/invalid-opts})))
  (with-open [git (open-git)]
    (when is_all
      (.. git add (addFilepattern ".") (setUpdate true) call))
    (let [msg    (cond
                   (and is_no_edit is_amend)         (head-message git)
                   (some-> message str str/trim seq) (str/trim message)
                   :else
                   (throw (ex-info "git_commit requires message (or is_amend + is_no_edit)"
                                   {:type :foundation-git/invalid-opts})))
          commit (.. git commit
                     (setMessage msg)
                     (setAmend (boolean is_amend))
                     (setAllowEmpty (boolean is_allow_empty))
                     call)
          sha    (.getName commit)]
      {:op        (if is_amend :git/amend :git/commit)
       :sha       sha
       :short-sha (short-sha sha)
       :message   msg
       :amend?    (boolean is_amend)})))

(defn amend!
  "Amend HEAD. () keeps the existing message; {\"message\": \"...\"} rewrites it.
   Pass {\"is_all\": true} to restage every tracked modification before amending."
  ([] (amend! {}))
  ([opts]
   (let [opts (assoc opts :is_amend true)
         opts (if (some-> (:message opts) str str/trim seq)
                opts
                (assoc opts :is_no_edit true))]
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
  "Push to a remote. Opts: {\"remote\": ..., \"branch\": ..., \"is_force\": bool, \"is_tags\": bool, \"is_delete\": bool, \"credentials\": ...}.

   For SSH remotes, the Apache MINA `SshdSessionFactory` is installed on
   first call; from then on JGit reads ~/.ssh/config the way `git`
   itself does (IdentityFile, HostName, User, agent, ed25519,
   rsa-sha2-512). HTTPS remotes use the supplied credentials."
  [{:keys [remote branch is_force is_tags is_delete credentials]}]
  (install-sshd-session-factory!)
  (with-open [git (open-git)]
    (let [remote (or remote "origin")
          repo   (.getRepository git)
          branch (or branch (.getBranch repo))
          spec   (refspec-for branch is_delete)
          cmd    (.. git push (setRemote remote) (setForce (boolean is_force)))]
      (when spec    (.setRefSpecs cmd (into-array RefSpec [spec])))
      (when is_tags (.setPushTags cmd))
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
         :force?  (boolean is_force)
         :tags?   (boolean is_tags)
         :delete? (boolean is_delete)
         :updates updates}))))

(defn- object-id->sha
  [^ObjectId oid]
  (some-> oid .getName))

(defn- object-id->short-sha
  [^ObjectId oid]
  (short-sha (object-id->sha oid)))

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
;; `ex-info`s the model can read off the Python return value.
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

(defn- reset-mode ^ResetCommand$ResetType
  [mode]
  (case mode
    :soft   ResetCommand$ResetType/SOFT
    :mixed  ResetCommand$ResetType/MIXED
    :hard   ResetCommand$ResetType/HARD
    :keep   ResetCommand$ResetType/KEEP
    :merge  ResetCommand$ResetType/MERGE
    (throw (ex-info (str "git_reset mode must be one of \"soft\" \"mixed\" \"hard\" \"keep\" \"merge\", got "
                         (pr-str mode))
                    {:type :foundation-git/invalid-opts :mode mode}))))

(defn reset!
  "Move HEAD (and optionally the index / working tree) to a revision.

   Opts: {\"mode\": ..., \"to\": ..., \"paths\": ...}.

     mode   one of \"soft\" \"mixed\" \"hard\" \"keep\" \"merge\" — required. soft keeps
            index + worktree; mixed (the bare-`git reset` default)
            resets the index, keeps worktree; hard resets everything.
            keep / merge match JGit's safe variants.
     to     revision string (sha, short sha, branch, HEAD~N,
            origin/main, …). Required.
     paths  optional list/string of path patterns. When present the
            reset becomes a per-path index reset (mode IS ignored by
            JGit in that branch, matching `git reset <paths>` shell
            semantics); without paths the whole tree resets per mode.

   Returns:
     {:op :git/reset :mode :to :resolved-sha :short-sha :head-before :head-after
      :paths}

   Use as the building block for \"reword 40 commits\": reset --soft to
   the parent of the oldest bad commit, then re-commit one by one."
  [{:keys [mode to paths]}]
  (when (str/blank? (str to))
    (throw (ex-info "git_reset requires \"to\" (revision)"
                    {:type :foundation-git/invalid-opts})))
  (with-open [git (open-git)]
    (let [repo         (.getRepository git)
          head-before  (some-> (.resolve repo "HEAD") .getName)
          resolved     (.getName (resolve-rev repo (str to)))
          path-strs    (cond
                         (nil? paths)      []
                         (string? paths)   [paths]
                         (sequential? paths) (vec paths)
                         :else (throw (ex-info "git_reset paths must be a string or list of strings"
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
                                                   "git_branch list mode must be \"local\" \"remote\" or \"all\""
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

   Opts: {\"op\": ..., \"name\": ..., \"from\": ..., \"is_force\": bool, \"delete\": ..., \"rename\": ..., \"list\": ...}.

     One of op in #{\"create\" \"delete\" \"rename\" \"list\"} OR a short-form key
     (delete name) / (rename [old new]) / (list mode).

     create  {\"name\": ..., \"from\": ..., \"is_force\": bool} — name required; from defaults
             to HEAD; is_force overrides an existing branch.
     delete  {\"name\": ..., \"is_force\": bool} — name (or list of names) required.
             is_force mirrors `git branch -D` (drops unmerged branch).
     rename  {\"old\": ..., \"new\": ...}  — atomic rename.
     list    {\"mode\": \"local\" | \"remote\" | \"all\"} (default \"local\").

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
        (let [name     (or (:name opts) (:create opts))
              from     (or (:from opts) "HEAD")
              is_force (boolean (:is_force opts))]
          (when (str/blank? (str name))
            (throw (ex-info "git_branch create requires name" {:type :foundation-git/invalid-opts})))
          (let [repo (.getRepository git)
                _    (resolve-rev repo from)
                ref  (.. git branchCreate
                         (setName name)
                         (setStartPoint from)
                         (setForce is_force)
                         call)
                sha  (some-> (.getObjectId ref) .getName)]
            {:op        :git/branch-create
             :name      name
             :from      from
             :sha       sha
             :short-sha (short-sha sha)
             :force?    is_force}))

        :delete
        (let [arg      (or (:name opts) (:delete opts))
              names    (cond
                         (string? arg) [arg]
                         (and (sequential? arg) (every? string? arg)) (vec arg)
                         :else (throw (ex-info "git_branch delete requires name (string or list)"
                                               {:type :foundation-git/invalid-opts})))
              is_force (boolean (:is_force opts))
              deleted  (vec (.. git branchDelete
                                (setBranchNames (into-array String names))
                                (setForce is_force)
                                call))]
          {:op      :git/branch-delete
           :deleted (mapv #(str/replace % #"^refs/heads/" "") deleted)
           :force?  is_force})

        :rename
        (let [pair    (or (:rename opts) [(:old opts) (:new opts)])
              [old-n new-n] (if (sequential? pair) pair [(:old opts) (:new opts)])]
          (when (or (str/blank? (str old-n)) (str/blank? (str new-n)))
            (throw (ex-info "git_branch rename requires old + new (or rename [old new])"
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
                "git_branch requires op in #{\"create\" \"delete\" \"rename\" \"list\"} (or the short-form key)"
                {:type :foundation-git/invalid-opts :opts opts}))))))

(defn checkout!
  "Switch HEAD to a branch or detach onto a sha; optionally restore files.

   Opts: {\"branch\": ..., \"sha\": ..., \"is_create\": bool, \"is_force\": bool, \"paths\": ..., \"start_point\": ...}.

     branch       branch name to switch to. With is_create true the
                  branch is created from start_point (or HEAD).
     sha          detach HEAD onto a commit. Mutually exclusive with
                  branch when no paths supplied.
     paths        when present, restores those paths from HEAD (or
                  from sha / branch when supplied) WITHOUT moving
                  HEAD — i.e. `git checkout -- <paths>`.
     is_force     discard local changes that conflict (analogous to
                  `git checkout -f`).
     is_create    with branch, create the branch first.
     start_point  start-point for is_create (defaults to HEAD).

   Returns one of three shapes by mode (keys don't co-occur across modes):
     paths  → {:op :git/checkout :paths :files-restored :start-point}
     branch → {:op :git/checkout :branch :head :short-head :created?}
     sha    → {:op :git/checkout :sha :head :short-head :detached?}"
  [{:keys [branch sha paths is_create is_force start_point]}]
  (with-open [git (open-git)]
    (let [repo  (.getRepository git)
          cmd   (.checkout git)]
      (cond
        (seq paths)
        (let [path-strs (cond
                          (string? paths)   [paths]
                          (sequential? paths) (vec paths)
                          :else (throw (ex-info "git_checkout paths must be a string or list"
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
          (.setForceRefUpdate cmd (boolean is_force))
          (when is_create
            (.setCreateBranch cmd true)
            (when start_point (.setStartPoint cmd ^String start_point)))
          (.call cmd)
          (let [head (some-> (.resolve repo "HEAD") .getName)]
            {:op         :git/checkout
             :branch     branch
             :head       head
             :short-head (short-sha head)
             :created?   (boolean is_create)}))

        (some? sha)
        (do
          (.setName cmd sha)
          (.setForceRefUpdate cmd (boolean is_force))
          (.call cmd)
          (let [head (some-> (.resolve repo "HEAD") .getName)]
            {:op         :git/checkout
             :sha        sha
             :head       head
             :short-head (short-sha head)
             :detached?  true}))

        :else
        (throw (ex-info "git_checkout requires branch, sha, or paths"
                        {:type :foundation-git/invalid-opts}))))))

(defn cherry-pick!
  "Re-apply one or more commits onto HEAD.

   Opts: {\"commits\": ..., \"mainline\": ..., \"is_no_commit\": bool}.

     commits      revstring or list of revstrings (sha, branch, HEAD~N).
                  Applied in order.
     mainline     parent number for merge-commit pick (1-based).
     is_no_commit leave changes staged; don't commit per pick.

   Returns: {:op :git/cherry-pick :status :picked [{...}] :failing-paths}"
  [{:keys [commits mainline is_no_commit]}]
  (when (or (nil? commits) (and (sequential? commits) (empty? commits)))
    (throw (ex-info "git_cherry_pick requires commits"
                    {:type :foundation-git/invalid-opts})))
  (with-open [git (open-git)]
    (let [repo (.getRepository git)
          revs (cond
                 (string? commits) [commits]
                 (sequential? commits) (vec commits)
                 :else (throw (ex-info "git_cherry_pick commits must be a string or list of strings"
                                       {:type :foundation-git/invalid-opts})))
          cmd  (.cherryPick git)]
      (doseq [rev revs]
        (.include cmd (resolve-rev repo rev)))
      (when mainline
        (let [n (cond
                  (integer? mainline) (int mainline)
                  (string? mainline)  (or (some-> (str/trim mainline) parse-long int)
                                          (throw (ex-info (str "git_cherry_pick mainline must be a parent number (1-based), got " (pr-str mainline))
                                                          {:type :foundation-git/invalid-opts :key :mainline :value mainline})))
                  :else               (throw (ex-info (str "git_cherry_pick mainline must be a parent number (1-based), got " (pr-str mainline))
                                                      {:type :foundation-git/invalid-opts :key :mainline :value mainline})))]
          (.setMainlineParentNumber cmd n)))
      (when is_no_commit (.setNoCommit cmd true))
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
    (throw (ex-info (str "git_rebase operation must be one of \"begin\" \"continue\" \"skip\" \"abort\", got "
                         (pr-str op))
                    {:type :foundation-git/invalid-opts :operation op}))))

(defn- rebase-hint
  "Actionable next-step for a non-OK rebase `status` name, or nil. The
   model reads this straight off the result instead of burning a separate
   `git/status` / `(doc 'git/rebase!)` round-trip to figure out recovery."
  [status-name]
  (case status-name
    "UNCOMMITTED_CHANGES"
    "Working tree has uncommitted changes (see :uncommitted-changes). Commit or stash them, or just re-run with {\"is_autostash\": True} to stash+restore them around the rebase."
    "CONFLICTS"
    "Rebase hit conflicts (see :conflicts). Resolve them, git_add(...) the files, then git_rebase({\"operation\": \"continue\"}). Bail with git_rebase({\"operation\": \"abort\"})."
    "STOPPED"
    "Rebase paused on an edit/reword step (see :current-commit). Make the change, git_add(...), then git_rebase({\"operation\": \"continue\"}). Skip with {\"operation\": \"skip\"} or bail with {\"operation\": \"abort\"}."
    "STASH_APPLY_CONFLICTS"
    "Rebase finished but re-applying the auto-stash hit conflicts. Resolve them and git_add(...); the stashed changes are preserved (see :autostash-ref)."
    "FAILED"
    "Rebase failed and was rolled back (see :failing-paths). Inspect those paths, clean the tree, and retry."
    nil))

(defn- rebase-result->map
  [^RebaseResult result]
  (let [status (.getStatus result)
        sname  (.name status)
        new-head (.getCurrentCommit result)]
    {:status            sname
     :successful?       (.isSuccessful status)
     :conflicts         (when-let [cs (.getConflicts result)] (vec cs))
     :failing-paths     (when-let [fp (.getFailingPaths result)]
                          (vec (.keySet fp)))
     ;; JGit hands back the exact paths that block a :begin rebase; surfacing
     ;; them here is what lets the model skip the extra `git/status` call.
     :uncommitted-changes (when-let [u (.getUncommittedChanges result)]
                            (vec u))
     :current-commit    (when new-head (.getName new-head))
     :short-current     (short-sha (some-> new-head .getName))
     :hint              (rebase-hint sname)}))

(defn rebase!
  "Rewind onto an upstream, re-applying commits. Use for non-interactive
   linearisation and — critically — for the \"reword last 40 commits\"
   workflow via edit-todos-fn.

   Opts (Python tool surface): {\"operation\": ..., \"upstream\": ..., \"onto\": ...,
          \"is_preserve_merges\": bool, \"is_autostash\": bool}.

     operation          one of \"begin\" \"continue\" \"skip\" \"abort\". Defaults to
                        \"begin\" when upstream is given.
     upstream           revstring (sha, branch, HEAD~N). The commits
                        strictly AFTER this rev are replayed.
     onto               replay target (defaults to upstream).
     is_preserve_merges recreate merge commits (JGit's setPreserveMerges).

   Internal Clojure callbacks — NOT passable from the Python tool surface
   (the model cannot supply a function); used by programmatic callers/tests:
     edit-todos-fn      (fn [todos] -> todos') over the rebase plan. Each
                        todo is {:action :pick|:reword|:edit|:squash|:fixup|:drop
                        :sha :short-sha :message}; missing entries drop the commit.
     reword-message-fn  (fn [sha previous-msg] -> new-msg). Drives the
                        message-rewrite slot when an entry is :reword/:squash.

   Returns: {:op :git/rebase :status :successful? :conflicts :failing-paths
             :current-commit :short-current}"
  [{:keys [operation upstream onto is_preserve_merges edit-todos-fn reword-message-fn is_autostash]
    :or   {reword-message-fn (fn [_sha msg] msg)}}]
  (with-open [git (open-git)]
    (let [op    (or operation (when upstream :begin))
          cmd   (.rebase git)]
      (when (nil? op)
        (throw (ex-info "git_rebase requires operation (or upstream which implies begin)"
                        {:type :foundation-git/invalid-opts})))
      (.setOperation cmd (rebase-operation op))
      (when (and (= op :begin) upstream)
        (.setUpstream cmd ^String upstream))
      (when (and (= op :begin) onto)
        (.setUpstreamName cmd ^String onto))
      (when is_preserve_merges
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
      ;; Autostash: park tracked working-tree changes before a :begin rebase
      ;; and restore them after. JGit's RebaseCommand has no setStash in this
      ;; version, so we drive stashCreate/Apply/Drop here — the supported
      ;; replacement for the manual "WIP commit + mixed reset" dance. A clean
      ;; tree makes stashCreate return nil, so the gate is "got a stash back".
      (let [stash (when (and is_autostash (= op :begin))
                    (.call (.stashCreate git)))
            ref   (some-> ^org.eclipse.jgit.revwalk.RevCommit stash .getName)
            base  (assoc (rebase-result->map (.call cmd))
                         :op :git/rebase :operation op)]
        (cond
          (nil? stash)
          base

          (:successful? base)
          ;; Rebase landed; replay the parked changes on top.
          (try
            (.call (doto (.stashApply git) (.setStashRef ^String ref)))
            (.call (.stashDrop git))
            (assoc base :autostash-applied? true)
            (catch Exception _
              ;; Rebase OK but parked changes don't re-apply cleanly. Keep the
              ;; stash (don't drop) so the work stays recoverable.
              (assoc base :autostash-applied? false
                     :autostash-ref ref
                     :status "STASH_APPLY_CONFLICTS"
                     :hint (rebase-hint "STASH_APPLY_CONFLICTS"))))

          :else
          ;; Rebase couldn't complete (conflicts / stopped). Roll it back and
          ;; restore the tree so :autostash? is fully reversible — nothing is
          ;; left half-applied or stranded in a stash the model can't reach.
          (do
            (.call (.setOperation (.rebase git) RebaseCommand$Operation/ABORT))
            (.call (doto (.stashApply git) (.setStashRef ^String ref)))
            (.call (.stashDrop git))
            (assoc base :autostash-applied? true
                   :hint (str "Auto-stash restored and rebase aborted — it could not "
                              "complete cleanly (" (:status base) "). Commit your changes "
                              "first, then rebase, so conflicts surface against committed work."))))))))

;; ============================================================================
;; Tool wrappers
;; ============================================================================

(defn- ok [v] (extension/success {:result v}))

(defn add-tool
  "Stage paths. git_add(\"file\"), git_add([\"a\", \"b\"]), or git_add(\".\")."
  [arg] (ok (add arg)))

(defn commit!-tool
  "Create a commit. Opts: {\"message\": ..., \"is_all\": bool, \"is_allow_empty\": bool, \"is_amend\": bool, \"is_no_edit\": bool}."
  [opts] (ok (commit! opts)))

(defn amend!-tool
  "Amend HEAD. () keeps the existing message; {\"message\": \"...\"} rewrites it."
  ([] (ok (amend!)))
  ([opts] (ok (amend! opts))))

(defn push!-tool
  "Push to remote. Opts: {\"remote\": ..., \"branch\": ..., \"is_force\": bool, \"is_tags\": bool, \"is_delete\": bool, \"credentials\": ...}."
  ([] (ok (push! {})))
  ([opts] (ok (push! opts))))

(defn fetch!-tool
  "Fetch from remote. Opts: {\"remote\": ..., \"credentials\": ...}."
  ([] (ok (fetch! {})))
  ([opts] (ok (fetch! opts))))

(defn reset!-tool
  "Move HEAD (and optionally index / worktree) to a revision.
   Opts: {\"mode\": \"soft\"|\"mixed\"|\"hard\"|\"keep\"|\"merge\", \"to\": ..., \"paths\": ...}."
  [opts] (ok (reset! opts)))

(defn branch!-tool
  "Create / delete / list / rename branches.
   See branch! for opt shape."
  [opts] (ok (branch! opts)))

(defn checkout!-tool
  "Switch HEAD to a branch / sha or restore paths.
   Opts: {\"branch\": ..., \"sha\": ..., \"paths\": ..., \"is_create\": bool, \"is_force\": bool, \"start_point\": ...}."
  [opts] (ok (checkout! opts)))

(defn cherry-pick!-tool
  "Re-apply commits onto HEAD.
   Opts: {\"commits\": ..., \"mainline\": ..., \"is_no_commit\": bool}."
  [opts] (ok (cherry-pick! opts)))

(defn rebase!-tool
  "Rewind onto an upstream and re-apply commits; optionally interactive.

   Opts: {\"upstream\": ..., \"onto\": ..., \"operation\": ..., \"is_autostash\": bool, \"is_preserve_merges\": bool}.
     upstream         rev to replay onto (commits strictly after it move).
                      Implies operation \"begin\".
     operation        one of \"begin\" \"continue\" \"skip\" \"abort\" — drive a paused rebase.
     is_autostash     True → stash dirty tracked files before a begin rebase
                      and restore them after. Fully reversible: if the rebase
                      can't complete it is aborted and the tree restored. Use
                      this instead of a manual WIP-commit dance.

   Result :status (string) and what to do next (also in :hint):
     OK / FAST_FORWARD / UP_TO_DATE  → done, :successful? true.
     UNCOMMITTED_CHANGES → tree dirty; blocking paths in :uncommitted-changes.
                           Re-run with {\"is_autostash\": True}, or commit/stash first.
     CONFLICTS → resolve files in :conflicts, git_add(...), then
                 git_rebase({\"operation\": \"continue\"}); or {\"operation\": \"abort\"}.
     STOPPED   → paused on edit/reword; fix, git_add(...),
                 git_rebase({\"operation\": \"continue\"}).
     FAILED    → rolled back; see :failing-paths."
  [opts] (ok (rebase! opts)))

(def add-symbol (extension/symbol #'add-tool {:symbol 'add :tag :mutation :render render/render-add :color-role :tool-color/edit}))
(def commit!-symbol (extension/symbol #'commit!-tool {:symbol 'commit! :tag :mutation :render render/render-commit :color-role :tool-color/edit}))
(def amend!-symbol (extension/symbol #'amend!-tool {:symbol 'amend! :tag :mutation :render render/render-commit :color-role :tool-color/edit}))
(def push!-symbol (extension/symbol #'push!-tool {:symbol 'push! :tag :mutation :render render/render-push :color-role :tool-color/edit}))
(def fetch!-symbol (extension/symbol #'fetch!-tool {:symbol 'fetch! :tag :mutation :render render/render-fetch :color-role :tool-color/read}))
(def reset!-symbol (extension/symbol #'reset!-tool {:symbol 'reset! :tag :mutation :render render/render-reset :color-role :tool-color/edit}))
(def branch!-symbol (extension/symbol #'branch!-tool {:symbol 'branch! :tag :mutation :render render/render-branch :color-role :tool-color/edit}))
(def checkout!-symbol (extension/symbol #'checkout!-tool {:symbol 'checkout! :tag :mutation :render render/render-checkout :color-role :tool-color/edit}))
(def cherry-pick!-symbol (extension/symbol #'cherry-pick!-tool {:symbol 'cherry-pick! :tag :mutation :render render/render-cherry-pick :color-role :tool-color/edit}))
(def rebase!-symbol (extension/symbol #'rebase!-tool {:symbol 'rebase! :tag :mutation :render render/render-rebase :color-role :tool-color/edit}))

;; Hidden back-compat aliases: both spellings resolve to the SAME tool, but
;; only the canonical name (`add`, `commit!`) is advertised in the prompt
;; symbol catalog. `git/add` ↔ `git/add!` and `git/commit` ↔ `git/commit!`.
(def add!-symbol (extension/symbol #'add-tool {:symbol 'add! :tag :mutation :hidden? true :render render/render-add :color-role :tool-color/edit}))
(def commit-symbol (extension/symbol #'commit!-tool {:symbol 'commit :tag :mutation :hidden? true :render render/render-commit :color-role :tool-color/edit}))

(def write-ops-symbols
  [add-symbol add!-symbol commit!-symbol commit-symbol amend!-symbol push!-symbol fetch!-symbol
   reset!-symbol branch!-symbol checkout!-symbol cherry-pick!-symbol rebase!-symbol])
