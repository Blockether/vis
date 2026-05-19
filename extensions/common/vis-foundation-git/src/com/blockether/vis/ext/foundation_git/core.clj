(ns com.blockether.vis.ext.foundation-git.core
  "Git observation tools for the LLM under the `v/` alias.

   Ships three observation tools: `v/git-diff`, `v/git-status`,
   `v/git-log`. All three are read-only and run inside the currently
   bound workspace root (channels rebind `*workspace-root*` per turn,
   PLAN.md §5).

   The extension activates only when:
     1. `git` is available on the host PATH (cached one-shot at boot)
     2. the active workspace root is inside a git repo (per turn)"
  (:require
   [clojure.java.shell :as sh]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.workspace :as workspace]))

;; =============================================================================
;; Activation
;; =============================================================================

(defonce ^:private git-binary-available?
  (delay
    (try (zero? (:exit (sh/sh "git" "--version")))
      (catch Throwable _ false))))

(defn- in-repo?
  "True when the active workspace root sits inside a git repo. Cheap —
   one `git rev-parse` per turn at most. Returns false on any failure,
   so we never call into git from a tool that has no repo to read."
  [env]
  (let [root (:workspace/root env)]
    (when (string? root)
      (try (zero? (:exit (sh/sh "git" "-C" root "rev-parse" "--is-inside-work-tree")))
        (catch Throwable _ false)))))

(defn- activation-fn
  "Per-turn activation. Returns true only when both checks pass; false
   silently hides all `v/git-*` symbols from the model for that turn."
  [env]
  (and @git-binary-available? (in-repo? env)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- git-out
  "Run `git <args>` inside `dir`. Throws on non-zero exit. Returns
   right-trimmed stdout."
  [dir args]
  (let [argv (cond-> ["git"]
               dir (into ["-C" (str dir)])
               true (into args))
        {:keys [exit out err]} (apply sh/sh argv)]
    (when-not (zero? exit)
      (throw (ex-info (str "git failed: " (str/join " " argv))
               {:type :foundation-git/git-failed
                :argv argv :exit exit :err err})))
    (str/trimr (or out ""))))

(defn- env-root
  "Canonical workspace root from the env. Throws if missing — the
   activation-fn should have prevented the symbol from firing without
   one, so this is a defensive last line."
  [env]
  (or (:workspace/root env)
    (throw (ex-info "v/git-* tool fired without :workspace/root in env"
             {:type :foundation-git/no-workspace}))))

(defn- parse-numstat
  "Parse `git diff --numstat` lines into [{:file :+ :-}]."
  [out]
  (->> (str/split-lines (str out))
    (remove str/blank?)
    (mapv (fn [line]
            (let [[adds dels file] (str/split line #"\t" 3)
                  parse-n          (fn [s] (try (Long/parseLong (str s))
                                             (catch Exception _ 0)))]
              {:file file
               :+    (parse-n adds)
               :-    (parse-n dels)})))))

(defn- parse-porcelain
  "Parse `git status --porcelain` lines into [{:status :file}]."
  [out]
  (->> (str/split-lines (str out))
    (remove str/blank?)
    (mapv (fn [line]
            (let [len (count line)]
              {:status (str/trim (subs line 0 (min 2 len)))
               :file   (when (> len 3) (str/triml (subs line 3 len)))})))))

(defn- parse-log
  "Parse `git log --pretty=format:%H%x09%an%x09%at%x09%s` into
   [{:sha :author :at :subject}]."
  [out]
  (->> (str/split-lines (str out))
    (remove str/blank?)
    (mapv (fn [line]
            (let [[sha author at subject] (str/split line #"\t" 4)]
              {:sha     sha
               :author  author
               :at      (try (Long/parseLong (str at)) (catch Exception _ nil))
               :subject (or subject "")})))))

;; =============================================================================
;; Tools
;; =============================================================================

(defn git-diff-fn
  "Diff the active workspace. For a branch-kind workspace, diffs the
   workspace HEAD against the commit it was spawned from (recorded in
   `:workspace/id`'s row as `commit_id`). Otherwise diffs the working
   tree against HEAD.

   Returns:
     {:branch    \"vis/abc\" | nil
      :head      \"sha\"
      :kind      :trunk | :branch | nil
      :stat      {:files N :+ N :- N}
      :files     [{:file :+ :-} ...]
      :porcelain [{:status :file} ...]}"
  [env]
  (let [root      (env-root env)
        ws-id     (:workspace/id env)
        db-info   (:db-info env)
        ws        (when (and db-info ws-id) (vis/workspace-get db-info ws-id))
        base      (when (and (= :branch (:kind ws)) (:commit-id ws))
                    (:commit-id ws))
        diff-arg  (if base [base "HEAD"] ["HEAD"])
        numstat   (parse-numstat (git-out root (into ["diff" "--numstat"] diff-arg)))
        porc      (parse-porcelain (git-out root ["status" "--porcelain"]))
        head      (git-out root ["rev-parse" "HEAD"])
        +sum      (reduce + 0 (map :+ numstat))
        -sum      (reduce + 0 (map :- numstat))]
    (extension/success
      {:result {:branch    (:branch ws)
                :head      head
                :kind      (:kind ws)
                :stat      {:files (count numstat) :+ +sum :- -sum}
                :files     numstat
                :porcelain porc}})))

(defn git-status-fn
  "Working-tree status of the active workspace as parsed porcelain.

   Returns:
     {:branch \"main\"
      :head   \"sha\"
      :clean? bool
      :entries [{:status :file} ...]}"
  [env]
  (let [root    (env-root env)
        branch  (try (git-out root ["rev-parse" "--abbrev-ref" "HEAD"])
                  (catch Throwable _ nil))
        head    (try (git-out root ["rev-parse" "HEAD"])
                  (catch Throwable _ nil))
        entries (parse-porcelain (git-out root ["status" "--porcelain"]))]
    (extension/success
      {:result {:branch  branch
                :head    head
                :clean?  (empty? entries)
                :entries entries}})))

(defn git-log-fn
  "Recent commits in the active workspace. Default limit is 20; pass a
   positive int up to 200 to override.

   Returns:
     {:branch \"main\"
      :commits [{:sha :author :at :subject} ...]}"
  ([env] (git-log-fn env 20))
  ([env n]
   (let [root   (env-root env)
         limit  (max 1 (min 200 (long (or n 20))))
         branch (try (git-out root ["rev-parse" "--abbrev-ref" "HEAD"])
                  (catch Throwable _ nil))
         out    (git-out root
                  ["log" (str "-" limit)
                   "--pretty=format:%H%x09%an%x09%at%x09%s"])]
     (extension/success
       {:result {:branch  branch
                 :commits (parse-log out)}}))))

(def ^{:doc "Diff stat + porcelain for the currently bound workspace. Branch workspaces diff against their spawn commit; trunk workspaces diff against HEAD. Returns {:branch :head :kind :stat {:files :+ :-} :files [...] :porcelain [...]}."
       :arglists '([])} git-diff git-diff-fn)

(def ^{:doc "Working-tree status of the currently bound workspace. Returns {:branch :head :clean? :entries [{:status :file} ...]}."
       :arglists '([])} git-status git-status-fn)

(def ^{:doc "Recent commits on the currently bound workspace's branch. Default 20 (max 200). Returns {:branch :commits [{:sha :author :at :subject} ...]}."
       :arglists '([] [n])} git-log git-log-fn)

(defn- inject-env
  [env f args]
  {:env env :fn f :args (into [env] args)})

(def git-diff-symbol
  (vis/symbol #'git-diff
    {:before-fn inject-env
     :render-fn vis/render-string}))

(def git-status-symbol
  (vis/symbol #'git-status
    {:before-fn inject-env
     :render-fn vis/render-string}))

(def git-log-symbol
  (vis/symbol #'git-log
    {:before-fn inject-env
     :render-fn vis/render-string}))

(def git-symbols
  [git-diff-symbol git-status-symbol git-log-symbol])

;; =============================================================================
;; Op registry
;; =============================================================================

(doseq [op [:v/git-diff :v/git-status :v/git-log]]
  (vis/register-op! op {:tag :op.tag/observation}))

;; =============================================================================
;; Extension manifest
;; =============================================================================

(def vis-extension
  (vis/extension
    {:ext/name           "foundation-git"
     :ext/description    "Git observation tools under v/: git-diff, git-status, git-log. Activates only when git is on PATH and the active workspace sits inside a repo."
     :ext/version        "0.1.0"
     :ext/author         "Blockether"
     :ext/owner          "vis"
     :ext/license        "Apache-2.0"
     :ext/activation-fn  activation-fn
     :ext/sci            {:ext.sci/alias 'v
                          :ext.sci/symbols git-symbols}
     :ext/kind           "foundation"}))

(vis/register-extension! vis-extension)
