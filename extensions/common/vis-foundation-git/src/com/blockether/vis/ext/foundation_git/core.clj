(ns com.blockether.vis.ext.foundation-git.core
  "Git observation tools for the LLM under the `git/` alias.

   Ships three observation tools: `git/diff`, `git/status`,
   `git/log`. All three are read-only, JGit-backed, and run inside the
   currently bound workspace root (channels rebind `*workspace-root*`
   per turn, PLAN.md §5).

   The extension activates only when the active workspace root is inside
   a git repo. No host `git` binary is required for observation tools."
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.git :as git-core]))

;; =============================================================================
;; Activation
;; =============================================================================

(defn- in-repo?
  "True when the active workspace root sits inside a git repo. Uses JGit
   only; returns false on any failure so symbols stay hidden when there
   is no repo to read."
  [env]
  (let [root (:workspace/root env)]
    (when (string? root)
      (try (git-core/in-repository? (io/file root))
        (catch Throwable _ false)))))

(defn- activation-fn
  "Per-turn activation. Returns true only when the workspace root is in
   a git repository; false silently hides all `git/*` symbols from the
   model for that turn."
  [env]
  (boolean (in-repo? env)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- env-root
  "Canonical workspace root from the env. Throws if missing — the
   activation-fn should have prevented the symbol from firing without
   one, so this is a defensive last line."
  [env]
  (or (:workspace/root env)
    (throw (ex-info "git/* tool fired without :workspace/root in env"
             {:type :foundation-git/no-workspace}))))

;; =============================================================================
;; Tools
;; =============================================================================

(defn git-diff-fn
  "Diff inside the active workspace's repository.

   No opts -> default workspace diff (branch workspaces diff against the
   commit they were spawned from; trunk workspaces diff working tree vs
   HEAD). With opts the model can ask for an arbitrary range or path.

   Opts map keys:
     :from   base ref (string; sha, branch name, HEAD~N, ...).
     :to     target ref (string). nil with :from means working tree.
     :path   repo-relative path to restrict the diff to (string).

   Returns:
     {:branch    \"vis/abc\" | nil
      :head      \"sha\"
      :kind      :trunk | :branch | nil | :range
      :from      <resolved base ref or nil for WT-only diffs>
      :to        <resolved target ref or nil for working-tree>
      :path      <:path filter, when present>
      :stat      {:files N :+ N :- N}
      :files     [{:file :+ :-} ...]
      :porcelain [{:status :file} ...]}

   `:porcelain` only carries entries when the diff includes the working
   tree (i.e. :to is nil). For ref-to-ref diffs it's an empty vec."
  ([env] (git-diff-fn env nil))
  ([env opts]
   (when (and (some? opts) (not (map? opts)))
     (throw (ex-info (str "git/diff expected optional opts map, got " (pr-str opts) ". "
                       "Call (git/diff), (git/diff {:from \"sha\"}), or (git/diff {:from \"a\" :to \"b\" :path \"src\"}).")
              {:type :foundation-git/invalid-opts
               :opts opts
               :expected "nil or map"
               :examples ["(git/diff)"
                          "(git/diff {:from \"HEAD~3\"})"
                          "(git/diff {:from \"main\" :to \"feature\"})"
                          "(git/diff {:path \"src/foo.clj\"})"]})))
   (let [{:keys [from to path]} (or opts {})
         _ (doseq [[k v] {:from from :to to :path path}]
             (when (and (some? v) (not (string? v)))
               (throw (ex-info (str "git/diff " k " must be a string, got " (pr-str v))
                        {:type :foundation-git/invalid-opts :opts opts :key k :value v}))))
         root        (env-root env)
         root-file   (io/file root)
         ws-id       (:workspace/id env)
         db-info     (:db-info env)
         ws          (when (and db-info ws-id) (vis/workspace-get db-info ws-id))
         ws-base     (when (and (= :branch (:kind ws)) (:commit-id ws))
                       (:commit-id ws))
         ;; explicit :from wins over the workspace default; explicit :to
         ;; switches us into ref-to-ref mode where porcelain is empty.
         base        (or from ws-base "HEAD")
         new-rev     (cond
                       (some? to)               to
                       (and from (not ws-base)) nil  ;; :from + no :to -> base..WT
                       ws-base                  "HEAD"  ;; default workspace shape
                       :else                    nil)
         status      (git-core/status-snapshot root-file)
         files       (vec (or (git-core/diff-numstat root-file base new-rev path) []))
         porcelain   (if new-rev [] (vec (or (:entries status) [])))
         head        (:head status)
         +sum        (reduce + 0 (map :+ files))
         -sum        (reduce + 0 (map :- files))
         kind        (cond from :range :else (:kind ws))]
     (extension/success
       {:result (cond-> {:branch    (:branch ws)
                         :head      head
                         :kind      kind
                         :from      base
                         :to        new-rev
                         :stat      {:files (count files) :+ +sum :- -sum}
                         :files     files
                         :porcelain porcelain}
                  path (assoc :path path))}))))

(defn git-status-fn
  "Working-tree status of the active workspace as parsed porcelain.

   Returns:
     {:branch \"main\"
      :head   \"sha\"
      :clean? bool
      :entries [{:status :file} ...]}"
  [env]
  (let [snapshot (or (git-core/status-snapshot (io/file (env-root env)))
                   {:branch nil :head nil :clean? true :entries []})]
    (extension/success {:result snapshot})))

(defn- coerce-log-limit
  "Normalize the (git/log ...) argument into a 1..200 integer.
   Accepts nil, a positive integer, or a map carrying `:limit` / `:n`.
   Throws a `:foundation-git/invalid-opts` ex-info with examples for
   anything else, so the model sees a clear usage error instead of a
   raw JVM ClassCastException from `(long ...)`."
  ^long [arg]
  (let [raw (cond
              (nil? arg)     20
              (integer? arg) arg
              (map? arg)     (or (:limit arg) (:n arg) 20)
              :else          ::bad)]
    (when (or (= raw ::bad) (not (integer? raw)) (neg? (long raw)))
      (throw (ex-info (str "git/log expected nil, a positive integer, or {:limit N}, got "
                        (pr-str arg) ". "
                        "Call (git/log), (git/log 50), or (git/log {:limit 50}).")
               {:type :foundation-git/invalid-opts
                :opts arg
                :expected "nil, positive integer, or {:limit N}"
                :examples ["(git/log)" "(git/log 50)" "(git/log {:limit 50})"]})))
    (max 1 (min 200 (long raw)))))

(defn- coerce-log-opts
  "Map form for git/log. Accepts `:limit/:n` for count, `:path/:ref` for
   commit selection, and `:since/:until/:author` for time + author filters.
   Returns a normalised opts map; anything not-a-map throws the same
   :foundation-git/invalid-opts shape as `coerce-log-limit`."
  [m]
  (when-not (map? m)
    (throw (ex-info (str "git/log expected a map, got " (pr-str m))
             {:type :foundation-git/invalid-opts :opts m})))
  {:limit  (coerce-log-limit (or (:limit m) (:n m)))
   :path   (when-let [p (:path m)] (when (string? p) p))
   :ref    (when-let [r (:ref m)]  (when (string? r) r))
   :since  (:since m)
   :until  (:until m)
   :author (when-let [a (:author m)] (when (string? a) a))})

(defn git-log-fn
  "Recent commits in the active workspace. Default limit is 20; max 200.

   Signatures:
     (git/log)                                 ; default 20
     (git/log 50)                              ; integer limit
     (git/log {:limit 50})                     ; map form, also accepts :n
     (git/log {:path \"src/foo.clj\" :limit 5})  ; commits touching that path
     (git/log {:ref \"main\" :limit 5})          ; log from a branch/sha

   Each commit map now carries: :sha :short-sha :author :email :at
   :committer :committer-email :committed-at :subject :body :parents."
  ([env] (git-log-fn env nil))
  ([env arg]
   (let [root    (io/file (env-root env))
         {:keys [limit path ref since until author]}
         (cond
           (nil? arg)     {:limit 20}
           (integer? arg) {:limit (coerce-log-limit arg)}
           (map? arg)     (coerce-log-opts arg)
           :else          (do (coerce-log-limit arg) nil))
         status  (git-core/status-snapshot root)
         commits (vec (or (git-core/recent-commits root limit
                            {:path   path  :ref   ref
                             :since  since :until until :author author}) []))]
     (extension/success
       {:result {:branch  (:branch status)
                 :commits commits}}))))

(defn git-show-fn
  "Detailed view of one commit.

   `(git/show \"sha\")` or `(git/show {:rev \"sha\"})`. Returns the canonical
   commit map (sha, author, email, committer, committed-at, parents,
   subject, body) plus :files (per-file +/- numstat against the first
   parent) and :stat totals."
  ([env arg]
   (let [rev (cond
               (string? arg) arg
               (map? arg)    (:rev arg)
               :else         nil)]
     (when-not (and (string? rev) (seq rev))
       (throw (ex-info (str "git/show expected a sha string or {:rev sha}, got " (pr-str arg))
                {:type :foundation-git/invalid-opts :opts arg
                 :examples ["(git/show \"HEAD\")"
                            "(git/show \"abc1234\")"
                            "(git/show {:rev \"HEAD~1\"})"]})))
     (let [root   (io/file (env-root env))
           result (git-core/show-commit root rev)]
       (when-not result
         (throw (ex-info (str "git/show could not resolve revision: " rev)
                  {:type :foundation-git/unknown-rev :rev rev})))
       (extension/success {:result result})))))

(defn git-blame-fn
  "Per-line blame for one tracked file.

   `(git/blame \"src/foo.clj\")`           — whole file
   `(git/blame {:path \"src/foo.clj\" :from 10 :to 40})` — line range

   Returns `{:path :head :total :lines [{:line :sha :short-sha :author
   :email :at :content :source-line} ...]}`. Outside the work tree or on
   untracked files returns `:foundation-git/not-tracked`."
  ([env arg]
   (let [{:keys [path from to]}
         (cond
           (string? arg) {:path arg}
           (map? arg)    arg
           :else         (throw (ex-info (str "git/blame expected a path string or opts map, got " (pr-str arg))
                                  {:type :foundation-git/invalid-opts :opts arg
                                   :examples ["(git/blame \"src/foo.clj\")"
                                              "(git/blame {:path \"src/foo.clj\" :from 10 :to 40})"]})))]
     (when-not (and (string? path) (seq path))
       (throw (ex-info (str "git/blame requires a non-blank :path, got " (pr-str arg))
                {:type :foundation-git/invalid-opts :opts arg})))
     (let [root   (io/file (env-root env))
           result (git-core/blame-file root path {:from from :to to})]
       (when-not result
         (throw (ex-info (str "git/blame failed: file is outside the repo or not tracked: " path)
                  {:type :foundation-git/not-tracked :path path})))
       (extension/success {:result result})))))

(def ^{:doc "Diff stat + porcelain. No opts = workspace default (branch workspaces diff against spawn commit; trunk diffs WT vs HEAD). Opts map: {:from ref :to ref :path P} for arbitrary range + path filter. :to nil means working tree. Returns {:branch :head :kind :from :to [:path] :stat {:files :+ :-} :files [...] :porcelain [...]}. JGit-backed; no host git binary needed."
       :arglists '([] [opts])} diff git-diff-fn)

(def ^{:doc "Working-tree status of the currently bound workspace. Returns {:branch :head :clean? :entries [{:status :file} ...]}. JGit-backed; no host git binary needed."
       :arglists '([])} status git-status-fn)

(def ^{:doc "Recent commits on the currently bound workspace's branch. Default 20 (max 200). Accepts a positive integer or a `{:limit N :path P :ref R :since D :until D :author S}` map. :since/:until accept ISO date strings, epoch ms/s, or java.util.Date. :author is a case-insensitive substring match on name OR email. Returns {:branch :commits [{:sha :short-sha :author :email :at :committer :committer-email :committed-at :subject :body :parents [...]} ...]}. JGit-backed; no host git binary needed."
       :arglists '([] [arg])} log git-log-fn)

(def ^{:doc "Detailed view of one commit. Accepts a sha string or {:rev sha}. Returns the canonical commit map plus :files (per-file numstat against the first parent) and :stat totals. JGit-backed; no host git binary needed."
       :arglists '([arg])} show git-show-fn)

(def ^{:doc "Per-line blame for one tracked file. Accepts a path string or {:path P :from L :to L}. Returns {:path :head :total :lines [{:line :sha :short-sha :author :email :at :content :source-line} ...]}. JGit-backed; no host git binary needed."
       :arglists '([arg])} blame git-blame-fn)

(defn- inject-env
  [env f args]
  {:env env :fn f :args (into [env] args)})

(def diff-symbol
  (vis/symbol #'diff
    {:before-fn inject-env
     :render-fn vis/render-string}))

(def status-symbol
  (vis/symbol #'status
    {:before-fn inject-env
     :render-fn vis/render-string}))

(def log-symbol
  (vis/symbol #'log
    {:before-fn inject-env
     :render-fn vis/render-string}))

(def show-symbol
  (vis/symbol #'show
    {:before-fn inject-env
     :render-fn vis/render-string}))

(def blame-symbol
  (vis/symbol #'blame
    {:before-fn inject-env
     :render-fn vis/render-string}))

(def git-symbols
  [diff-symbol status-symbol log-symbol show-symbol blame-symbol])

;; =============================================================================
;; Op registry
;; =============================================================================

(doseq [op [:git/diff :git/status :git/log :git/show :git/blame]]
  (vis/register-op! op {:tag :observation}))

;; =============================================================================
;; Extension manifest
;; =============================================================================

(def vis-extension
  (vis/extension
    {:ext/name           "foundation-git"
     :ext/description    "JGit-backed observation tools under git/: diff, status, log, show, blame. Activates only when the active workspace sits inside a repo."
     :ext/version        "0.1.0"
     :ext/author         "Blockether"
     :ext/owner          "vis"
     :ext/license        "Apache-2.0"
     :ext/activation-fn  activation-fn
     :ext/sci            {:ext.sci/alias 'git
                          :ext.sci/symbols git-symbols}
     :ext/kind           "git"}))

(vis/register-extension! vis-extension)
