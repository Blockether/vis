(ns com.blockether.vis.internal.workspace.drafts
  "Draft lifecycle above the workspace primitives: create, approve, discard and
   status. Each mutation crosses the extension op-hook boundary (`:draft/create`,
   `:draft/approve`, `:draft/discard`), so an extension can veto it with a
   `:before` guard or observe it with an `:after` hook.

   Approval is how a draft lands: everything in the draft is staged and
   committed on its `vis/<label>` branch, which the trunk repository then holds
   — a linked worktree shares the refs already, a Rift clone is fetched from —
   and the user merges with their own tools. Nothing here touches the trunk
   working tree or its index; `git commit` itself still crosses the
   `:git/commit` boundary through `workspace.git/commit!`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.internal.workspace.git :as git])
  (:import [java.io File]))

(def ^:private git-timeout {:timeout-secs 120})

(defn- git-error
  [^File dir args {:keys [out err]}]
  (ex-info (str "git " (str/join " " args)
                " failed in " (.getPath dir)
                ": " (str/trim (str (if (str/blank? (str err)) out err))))
           {:type :draft/git-failed
            :dir (.getPath dir)
            :args (mapv str args)
            :details (str/trim (str err))}))

(defn- git!
  "`git <args>` in `dir`, throwing `:draft/git-failed` unless it exits 0. Returns
   the trimmed stdout."
  ^String [^File dir args]
  (let [{:keys [exit out] :as result} (git/run-git dir args git-timeout)]
    (when-not (= 0 exit) (throw (git-error dir args result)))
    (str/trim (str out))))

(defn- git-lines [^File dir args] (into [] (remove str/blank?) (str/split-lines (git! dir args))))

(defn- current-branch
  "The branch `dir` has checked out, nil when detached."
  [^File dir]
  (let [{:keys [exit out]}
        (git/run-git dir ["symbolic-ref" "--short" "--quiet" "HEAD"] git-timeout)]
    (when (= 0 exit) (not-empty (str/trim (str out))))))

(defn- branch-taken?
  [^File repo branch]
  (= 0
     (:exit (git/run-git repo
                         ["rev-parse" "--verify" "--quiet" (str "refs/heads/" branch)]
                         git-timeout))))

(defn- free-branch-name
  "`vis/<label>`, numerically suffixed while `repo` already has that branch."
  [^File repo label]
  (let [base (str workspace/draft-branch-prefix label)]
    (loop [branch base
           i 2]

      (if (branch-taken? repo branch) (recur (str base "-" i) (inc i)) branch))))

(defn- draft-branch
  "True when `branch` is one drafts land on."
  [branch]
  (and branch (str/starts-with? branch workspace/draft-branch-prefix) branch))

(defn- worktree? [ws] (= :worktree (workspace/backend-id (:workspace-backend ws))))

(defn- require-draft
  [db-info workspace-id]
  (let [ws (workspace/get db-info workspace-id)]
    (when-not ws
      (throw (ex-info "Unknown workspace" {:type :draft/unknown :workspace-id workspace-id})))
    (when-not (workspace/draft? ws)
      (throw (ex-info "Not a draft workspace"
                      {:type :draft/not-a-draft :workspace-id workspace-id})))
    (when-not (= :active (:state ws))
      (throw (ex-info (str "Draft is " (name (:state ws)))
                      {:type :draft/not-active :workspace-id workspace-id :state (:state ws)})))
    ws))

(defn- hook-ctx
  "Hook payload for `ws` — plain data, safe to stringify for Python."
  [ws extra]
  (merge {:workspace-id (:id ws)
          :label (:label ws)
          :root (:root ws)
          :repo-root (:repo-root ws)
          :backend (some-> (:workspace-backend ws)
                           workspace/backend-id
                           name)}
         extra))

(defn- through-hooks
  "Run `f` through the `op` extension boundary with `ctx` as the hook argument.
   A vetoed operation throws `:draft/blocked` with the extension's reason; the
   result of `f` otherwise flows back unchanged."
  [op env ctx f]
  (let [res (extension/invoke-operation op
                                        env
                                        (fn [_ctx]
                                          (extension/success {:result (f)}))
                                        [ctx])]
    (if (extension/envelope-success? res)
      (:result res)
      (throw (ex-info (or (some-> res
                                  :error
                                  :message
                                  not-empty)
                          (str "Refused: " (name op)))
                      {:type :draft/blocked :op op :ctx ctx :error (:error res)})))))

;; Lifecycle

(defn create!
  "Create a draft through the `:draft/create` boundary; `opts` are
   `workspace/create!`'s. `env` carries `:db-info` and `:session-id`."
  [env opts]
  (through-hooks :draft/create
                 env
                 {:label (:label opts)
                  :clean (boolean (:clean? opts))
                  :repo-root (some-> (or (:repo-root (:from opts)) (workspace/trunk-root))
                                     str)}
                 #(workspace/create! (:db-info env) opts)))

(defn discard!
  "Discard `workspace-id` through the `:draft/discard` boundary — `workspace/abandon!`
   plus the hook round-trip. The caller has already moved the session off it."
  [env {:keys [workspace-id reason]}]
  (let [ws (require-draft (:db-info env) workspace-id)]
    (through-hooks :draft/discard
                   env
                   (hook-ctx ws {:reason reason})
                   #(dissoc (workspace/abandon! (:db-info env)
                                                {:workspace-id workspace-id :reason reason})
                      :discard-future))))

(defn- ensure-draft-branch!
  "Put the draft at `root` on a `vis/…` branch: a worktree already is; a Rift clone
   still sits on the trunk's branch and gets a fresh one, named free in `trunk`
   so the fetch below lands it without a clash."
  [^File root ^File trunk label]
  (or (draft-branch (current-branch root))
      (let [branch (free-branch-name trunk label)]
        (git! root ["switch" "-c" branch])
        branch)))

(defn- stage-all!
  "Stage every change in the draft except backend bookkeeping; returns the
   staged paths."
  [^File root]
  (git! root ["add" "-A" "--" "."])
  (git! root ["rm" "-r" "-q" "--cached" "--ignore-unmatch" "--" ".rift" ".trash"])
  (git-lines root ["diff" "--cached" "--name-only"]))

(defn- commit-message
  [label message session-id]
  (str (if (str/blank? (str message)) (str "draft(" label "): approve") (str/trim (str message)))
       "\n\n"
       (when-not (str/blank? (str session-id)) (str "Vis-Session: " session-id "\n"))
       "Vis-Draft: "
       label
       "\n"))

(defn- commit!
  [^File root message]
  (let [{:keys [exit] :as result} (git/commit! root ["commit" "--quiet" "-m" message] git-timeout)]
    (when-not (= 0 exit) (throw (git-error root ["commit"] result)))
    (git! root ["rev-parse" "HEAD"])))

(defn approve!
  "Land the draft `workspace-id` as one commit on its `vis/<label>` branch, made
   visible in the trunk repository: a worktree shares the refs, a Rift clone is
   fetched from (fast-forward only, so repeated approvals stack). Crosses the
   `:draft/approve` boundary with the staged file list, then `:git/commit`
   through `workspace.git/commit!`. Returns `{:status :approved …}` with
   `:branch`, `:commit` and `:files`, or `{:status :nothing-to-approve}` when the
   draft holds no change. The draft stays active afterwards; discarding it keeps
   an approved branch.

   `opts`: `:workspace-id`, optional `:message` (subject line; the
   `Vis-Session`/`Vis-Draft` trailers are always appended)."
  [env {:keys [workspace-id message]}]
  (let [db-info
        (:db-info env)

        ws
        (require-draft db-info workspace-id)

        root
        (io/file (:root ws))

        trunk
        (io/file (:repo-root ws))

        _
        (when-not (workspace/git-managed? trunk)
          (throw (ex-info (str "Approval needs a Git-managed project: "
                               (:repo-root ws)
                               " is not a repository. Use apply to copy the draft's files instead.")
                          {:type :draft/not-git-managed :workspace-id workspace-id})))

        branch
        (ensure-draft-branch! root trunk (:label ws))

        files
        (stage-all! root)]

    (if (empty? files)
      {:status :nothing-to-approve :branch branch :files [] :workspace ws}
      (through-hooks
        :draft/approve
        env
        (hook-ctx ws {:branch branch :files files :message message})
        (fn []
          (let [sha (commit! root (commit-message (:label ws) message (:session-id env)))]
            (when-not (worktree? ws)
              (git! trunk
                    ["fetch" "--quiet" (.getPath root)
                     (str "refs/heads/" branch ":refs/heads/" branch)]))
            (let [result {:status :approved :branch branch :commit sha :files files :workspace ws}]
              (workspace/fire-hook! :on-approve ws (dissoc result :workspace))
              result)))))))

(defn status
  "Landing status of draft `ws`: its `:branch`, how many approved commits the
   trunk's HEAD lacks (`:ahead`) and how many paths in the draft still differ
   from that branch (`:pending`); the git facts are nil outside a repository."
  [ws]
  (let [root
        (io/file (:root ws))

        trunk
        (io/file (:repo-root ws))

        git?
        (and (.isDirectory root) (workspace/git-managed? trunk) (workspace/git-managed? root))

        branch
        (when git? (draft-branch (current-branch root)))]

    {:workspace-id (:id ws)
     :label (:label ws)
     :root (:root ws)
     :repo-root (:repo-root ws)
     :state (:state ws)
     :backend (some-> (:workspace-backend ws)
                      workspace/backend-id
                      name)
     :mechanism (some-> (:workspace-mechanism ws)
                        workspace/mechanism-id
                        name)
     :branch branch
     :ahead (when branch
              (let [{:keys [exit out]}
                    (git/run-git trunk ["rev-list" "--count" (str "HEAD.." branch)] git-timeout)]
                (when (= 0 exit) (parse-long (str/trim (str out))))))
     :pending
     (when git?
       (let [{:keys [exit out]}
             (git/run-git root ["status" "--porcelain" "--untracked-files=all"] git-timeout)]
         (when (= 0 exit) (count (remove str/blank? (str/split-lines (str out)))))))}))
