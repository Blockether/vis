(ns com.blockether.vis.ext.foundation-git.write-ops
  "Write-side SCI op family under the `git/` alias: add, commit!, amend!,
   push!, fetch!. Pure JGit, no shell. Mirrors merge_ops.clj shape.

   Auth: SSH remotes go through Apache MINA SSHD via JGit's
   `SshdSessionFactory`. That factory reads ~/.ssh/config + IdentityFile,
   speaks ed25519 / rsa-sha2-512, and talks to ssh-agent — i.e. the
   handshake GitHub actually accepts. HTTPS remotes still need
   :credentials {:username :password} or {:token \"...\"}."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.workspace :as workspace])
  (:import
   [java.io File IOException]
   [java.nio.file Paths]
   [java.util.function Function]
   [org.eclipse.jgit.api Git]
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
  (reset! ssh-passphrase-prompt-fn f))

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
      (setAttempts [_ n] (reset! attempts n))
      (getAttempts [_] @attempts)
      (keyLoaded [_ _uri _attempt error]
        ;; true -> try again with a new passphrase (we don't loop here;
        ;; user-supplied prompt is single-shot). false -> give up on key.
        (nil? error)))))

(defn- install-sshd-session-factory!
  "Install JGit's Apache-MINA `SshdSessionFactory` once, pointed at the
   user's real ~/.ssh directory and wired to our channel-aware
   `KeyPasswordProvider`. The default factory cannot read ssh-config or
   modern key algorithms; without this swap, every push to GitHub fails
   with `remote hung up unexpectedly` mid-handshake (Vis conv 11d4f817 /
   t10/i11). Idempotent — only the first call builds and registers the
   factory."
  []
  (when-not (instance? SshdSessionFactory (SshSessionFactory/getInstance))
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
      (SshSessionFactory/setInstance factory))))

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
  (install-sshd-session-factory!)
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
  "Tiny channel renderer: surface the result map as a fenced EDN code block.
   The model gets the raw map via SCI; this only shapes the TUI/channel
   preview. Must return canonical IR ([:ir {} …]) per `render-value?`."
  [result]
  [:ir {} [:code {:lang "edn"} (pr-str result)]])

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
